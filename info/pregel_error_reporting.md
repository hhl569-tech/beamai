# Pregel 顶点执行失败的错误上报机制

## 问题

当超级步 S 中 vertex 执行失败后，我们是否将此信息告知了上层应用，以便暂停超级步 S+1？

**答案：当前实现没有告知上层应用。**

## 当前错误处理分析

### 1. 节点执行失败时发生了什么？

代码位置：`graph_compute.erl:172-181`

```erlang
finish_node_execution(Ctx, VertexValue, Node, State, Edges) ->
    case graph_node:execute(Node, State) of
        {ok, NewState} ->
            %% 成功：发送消息给下一节点
            Ctx1 = send_to_next_nodes(Ctx, Edges, NewState),
            NewValue = VertexValue#{result => {ok, NewState}},
            pregel:vote_to_halt(pregel:set_value(Ctx1, NewValue));
        {error, Reason} ->
            %% 失败：只保存错误，不发送消息
            NewValue = VertexValue#{result => {error, Reason}},
            pregel:vote_to_halt(pregel:set_value(Ctx, NewValue))
            %% ⚠️ 没有通知任何人！
    end.
```

**失败时的行为：**
- ✅ 保存错误到 `vertex.value.result`
- ❌ 不发送消息给下一节点
- ❌ 不通知 Master 有错误发生

### 2. Worker 上报给 Master 的信息

代码位置：`pregel_worker.erl:331-337`

```erlang
notify_master_done(#state{worker_id = WorkerId, master = Master}, Vertices, Outbox) ->
    Result = #{
        worker_id => WorkerId,
        active_count => ...,    %% 活跃顶点数
        message_count => ...    %% 消息数
        %% ⚠️ 没有错误信息！
    },
    gen_server:cast(Master, {worker_done, self(), Result}).
```

**上报的信息不包含错误！**

## 当前错误处理流程

```
┌─────────────────────────────────────────────────────────────────┐
│                    当前错误处理流程                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  超级步 S:                                                       │
│  ┌─────────┐                                                    │
│  │ Node A  │ ─── 执行失败 ───→ result = {error, Reason}        │
│  └─────────┘                                                    │
│       │                                                         │
│       │ ❌ 不发送消息给下一节点                                   │
│       │ ❌ 不通知 Master 有错误                                   │
│       │ ✅ 只保存到 vertex.value.result                          │
│       │                                                         │
│       ▼                                                         │
│  Worker 上报: {active_count, message_count}                     │
│       │                                                         │
│       │ ⚠️ Master 不知道有节点失败！                              │
│       ▼                                                         │
│  Master: "所有 Worker 完成，继续超级步 S+1"                       │
│                                                                 │
│  超级步 S+1:                                                     │
│  后续节点收不到消息，vote_to_halt                                 │
│  执行看起来"正常"完成，但结果是错的                                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 问题总结

| 问题 | 当前实现 |
|------|---------|
| **Worker 是否上报错误？** | ❌ 否，只上报 active_count 和 message_count |
| **Master 是否知道有错误？** | ❌ 否 |
| **是否暂停超级步 S+1？** | ❌ 否，继续执行 |
| **错误存储在哪？** | vertex.value.result（只在本地） |
| **何时发现错误？** | 执行完成后，从图中提取结果时 |

## 错误发现的时机

代码位置：`graph_compute.erl:245-291`

```erlang
%% 执行完成后才检查错误
find_last_result(Graph) ->
    ...
    %% 检查是否有错误结果
    ErrorResults = [{Id, R} || {Id, {error, _} = R} <- Results],
    case ErrorResults of
        [{_NodeId, {error, Reason}} | _] ->
            {error, Reason};  %% 这时才发现错误
        ...
    end.
```

**错误只在整个图执行完成后才被发现！**

## 改进方案

### 方案 1：Worker 上报错误信息

```erlang
%% 修改 notify_master_done
notify_master_done(State, Vertices, Outbox) ->
    %% 检查是否有失败的顶点
    FailedVertices = find_failed_vertices(Vertices),

    Result = #{
        worker_id => WorkerId,
        active_count => ...,
        message_count => ...,
        %% 新增错误信息
        failed_count => length(FailedVertices),
        failed_vertices => FailedVertices
    },
    gen_server:cast(Master, {worker_done, self(), Result}).

find_failed_vertices(Vertices) ->
    maps:fold(
        fun(Id, V, Acc) ->
            case maps:get(result, pregel_vertex:value(V), undefined) of
                {error, Reason} -> [{Id, Reason} | Acc];
                _ -> Acc
            end
        end,
        [],
        Vertices
    ).
```

### 方案 2：Master 检查错误并决定是否继续

```erlang
%% 修改 complete_superstep
complete_superstep(State) ->
    Results = pregel_barrier:get_results(Barrier),

    %% 汇总错误
    TotalFailed = lists:sum([maps:get(failed_count, R, 0) || R <- Results]),

    case TotalFailed > 0 of
        true ->
            %% 有错误，决定如何处理
            handle_superstep_errors(Results, State);
        false ->
            %% 无错误，继续正常流程
            ...
    end.

handle_superstep_errors(Results, State) ->
    case ErrorStrategy of
        fail_fast ->
            %% 立即终止
            finish_execution({error, vertex_failures}, State);
        continue ->
            %% 忽略错误，继续执行
            start_next_superstep(State);
        retry ->
            %% 重试失败顶点
            retry_failed_vertices(Results, State);
        {callback, Fun} ->
            %% 通知上层，让上层决定
            Fun(Results, State)
    end.
```

### 方案 3：支持错误策略配置

```erlang
%% 在 opts 中配置错误处理策略
Opts = #{
    on_vertex_error => fail_fast    %% 立即终止
                     | continue     %% 忽略继续
                     | retry        %% 重试
                     | {callback, Fun}  %% 回调上层决定
}.
```

## 改进后的错误处理流程

```
┌─────────────────────────────────────────────────────────────────┐
│                    改进后的错误处理流程                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  超级步 S:                                                       │
│  ┌─────────┐                                                    │
│  │ Node A  │ ─── 执行失败 ───→ result = {error, Reason}        │
│  └─────────┘                                                    │
│       │                                                         │
│       ▼                                                         │
│  Worker 上报: {                                                  │
│    active_count,                                                │
│    message_count,                                               │
│    failed_count: 1,                    ← 新增                   │
│    failed_vertices: [{node_a, Reason}] ← 新增                   │
│  }                                                              │
│       │                                                         │
│       ▼                                                         │
│  Master 收到错误信息                                             │
│       │                                                         │
│       ├─── fail_fast ───→ 立即终止，返回错误                     │
│       │                                                         │
│       ├─── continue ────→ 记录错误，继续超级步 S+1               │
│       │                                                         │
│       ├─── retry ───────→ 重试失败顶点                          │
│       │                                                         │
│       └─── callback ────→ 通知上层应用，等待决策                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 错误策略详解

### fail_fast（快速失败）

```
适用场景: 任何错误都不可接受
行为: 立即终止，返回错误给调用者
结果: {error, {vertex_failures, FailedList}}
```

### continue（继续执行）

```
适用场景: 部分节点失败可接受
行为: 记录错误，继续执行后续超级步
结果: 最终结果中包含错误信息
```

### retry（重试）

```
适用场景: 临时性错误（如网络超时）
行为: 重试失败的顶点，最多 N 次
结果: 重试成功则继续，否则按其他策略处理
```

### callback（回调）

```
适用场景: 需要上层应用决策
行为: 调用回调函数，传入错误信息
结果: 由回调函数决定下一步行动
```

## 实现优先级建议

```
┌─────────────────────────────────────────────────────────────────┐
│                      实现优先级                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 【必须】Worker 上报错误信息                                   │
│     - 修改 notify_master_done                                   │
│     - 添加 failed_count 和 failed_vertices                      │
│                                                                 │
│  2. 【必须】Master 检查错误                                       │
│     - 修改 complete_superstep                                   │
│     - 汇总所有 Worker 的错误                                     │
│                                                                 │
│  3. 【建议】支持 fail_fast 策略                                   │
│     - 默认行为：有错误立即终止                                    │
│                                                                 │
│  4. 【可选】支持其他错误策略                                       │
│     - continue, retry, callback                                 │
│     - 根据实际需求添加                                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 总结

| 问题 | 当前状态 | 改进建议 |
|------|---------|---------|
| **错误上报** | ❌ 无 | Worker 上报 failed_count 和 failed_vertices |
| **Master 感知** | ❌ 无 | 在 complete_superstep 中检查错误 |
| **暂停下一超级步** | ❌ 无 | 根据错误策略决定是否继续 |
| **错误策略** | ❌ 无 | 支持 fail_fast/continue/retry/callback |
| **上层应用感知** | ❌ 延迟 | 立即通知或回调 |
