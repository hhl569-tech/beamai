# pregel_worker 错误处理设计

## 设计原则

**职责分离**：pregel_worker 不做异常处理和重试，只约定成功/失败的数据结构，根据结果决定下一步动作。

```
┌─────────────────────────────────────────────────────────────────┐
│                    职责分离                                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Pregel Worker 层:                                               │
│  ─────────────────                                              │
│  • 定义成功/失败的数据结构契约                                    │
│  • 根据结果决定下一步动作                                        │
│  • 不做异常处理                                                  │
│  • 不做重试                                                      │
│                                                                 │
│  Graph Compute 层:                                               │
│  ─────────────────                                              │
│  • 负责异常处理（try-catch）                                     │
│  • 负责重试逻辑                                                  │
│  • 返回符合契约的成功/失败结果                                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 数据结构契约

### 计算函数返回值约定

```erlang
%% pregel_worker 约定的计算结果结构

%% 成功时返回
#{
    vertex := vertex(),                    %% 新顶点状态
    outbox := [{vertex_id(), term()}],     %% 发出的消息
    status := ok                           %% 状态标记
}

%% 失败时返回
#{
    vertex := vertex(),                    %% 原顶点状态（不变）
    outbox := [],                          %% 无消息发出
    status := {error, Reason}              %% 错误状态
}
```

### 类型定义

```erlang
%% 计算结果状态
-type compute_status() :: ok | {error, term()}.

%% 计算结果
-type compute_result() :: #{
    vertex := vertex(),
    outbox := [{vertex_id(), term()}],
    status := compute_status()
}.
```

---

## pregel_worker 的处理逻辑

### 修改后的 compute_vertices

```erlang
%% compute_vertices 处理成功/失败
-spec compute_vertices(#{vertex_id() => vertex()},
                       #{vertex_id() => vertex()},
                       #{vertex_id() => [term()]},
                       fun((context()) -> compute_result()),
                       non_neg_integer(),
                       non_neg_integer()) ->
    {#{vertex_id() => vertex()}, [{vertex_id(), term()}], [{vertex_id(), term()}]}.

compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    maps:fold(
        fun(Id, Vertex, {VAcc, OAcc, FailedAcc}) ->
            Messages = maps:get(Id, Inbox, []),
            ActiveVertex = activate_if_has_messages(Vertex, Messages),
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),

            %% 调用计算函数，它负责异常处理和重试
            Result = ComputeFn(Context),

            %% 根据结果决定下一步
            case maps:get(status, Result, ok) of
                ok ->
                    #{vertex := NewVertex, outbox := Out} = Result,
                    {VAcc#{Id => NewVertex}, Out ++ OAcc, FailedAcc};
                {error, Reason} ->
                    %% 失败：记录失败顶点，不更新顶点，不发消息
                    {VAcc, OAcc, [{Id, Reason} | FailedAcc]}
            end
        end,
        {AllVertices, [], []},
        ActiveVertices
    ).
```

### 修改后的 execute_superstep

```erlang
execute_superstep(#state{
    vertices = Vertices,
    inbox = Inbox,
    compute_fn = ComputeFn,
    combiner = Combiner,
    superstep = Superstep,
    num_vertices = NumVertices
} = State) ->
    %% 1. 筛选需要计算的顶点
    ActiveVertices = filter_active_vertices(Vertices, Inbox),

    %% 2. 执行所有顶点计算（返回失败列表）
    {NewVertices, Outbox, FailedVertices} = compute_vertices(
        ActiveVertices, Vertices, Inbox, ComputeFn, Superstep, NumVertices
    ),

    %% 3. 应用合并器
    CombinedOutbox = apply_combiner(Outbox, Combiner),

    %% 4. 路由消息
    route_messages(CombinedOutbox, State),

    %% 5. 通知 Master（含失败信息）
    notify_master_done(State, NewVertices, CombinedOutbox, FailedVertices),

    %% 6. 返回更新后的状态
    State#state{vertices = NewVertices, inbox = #{}}.
```

### 修改后的 notify_master_done

```erlang
%% 当前（无错误信息）
notify_master_done(#state{worker_id = WorkerId, master = Master}, Vertices, Outbox) ->
    Result = #{
        worker_id => WorkerId,
        active_count => pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices),
        message_count => length(Outbox)
    },
    gen_server:cast(Master, {worker_done, self(), Result}).

%% 修改后（含错误信息）
notify_master_done(#state{worker_id = WorkerId, master = Master},
                   Vertices, Outbox, FailedVertices) ->
    Result = #{
        worker_id => WorkerId,
        active_count => pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices),
        message_count => length(Outbox),
        %% 新增
        failed_count => length(FailedVertices),
        failed_vertices => FailedVertices  %% [{vertex_id(), Reason}]
    },
    gen_server:cast(Master, {worker_done, self(), Result}).
```

---

## Graph Compute 层的职责

### 计算函数实现

```erlang
%% graph_compute.erl 中的计算函数

compute_fn() ->
    fun(Ctx) ->
        try
            %% 执行节点逻辑
            Result = do_compute(Ctx),

            %% 成功返回
            Result#{status => ok}
        catch
            Class:Reason:Stacktrace ->
                %% 可选：重试逻辑
                case should_retry(Reason) of
                    true ->
                        retry_compute(Ctx);
                    false ->
                        %% 失败返回
                        #{
                            vertex => maps:get(vertex, Ctx),
                            outbox => [],
                            status => {error, {Class, Reason}}
                        }
                end
        end
    end.

%% 判断是否应该重试
should_retry({timeout, _}) -> true;
should_retry({temporary_failure, _}) -> true;
should_retry(_) -> false.

%% 重试逻辑
retry_compute(Ctx) ->
    retry_compute(Ctx, 3).  %% 最多重试 3 次

retry_compute(Ctx, 0) ->
    #{
        vertex => maps:get(vertex, Ctx),
        outbox => [],
        status => {error, max_retries_exceeded}
    };
retry_compute(Ctx, Remaining) ->
    try
        Result = do_compute(Ctx),
        Result#{status => ok}
    catch
        _:Reason:_ ->
            case should_retry(Reason) of
                true ->
                    timer:sleep(100),  %% 可选：退避
                    retry_compute(Ctx, Remaining - 1);
                false ->
                    #{
                        vertex => maps:get(vertex, Ctx),
                        outbox => [],
                        status => {error, Reason}
                    }
            end
    end.
```

---

## 流程图

```
┌─────────────────────────────────────────────────────────────────┐
│                    错误处理流程                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  pregel_worker                       graph_compute              │
│  ─────────────                       ─────────────              │
│       │                                    │                    │
│       │  调用 ComputeFn(Context)           │                    │
│       │ ──────────────────────────────────→│                    │
│       │                                    │                    │
│       │                              ┌─────┴─────┐              │
│       │                              │ try-catch │              │
│       │                              │  执行节点  │              │
│       │                              └─────┬─────┘              │
│       │                                    │                    │
│       │                         ┌──────────┴──────────┐         │
│       │                         │                     │         │
│       │                      成功                   失败        │
│       │                         │                     │         │
│       │                         │              ┌──────┴──────┐  │
│       │                         │              │ should_retry│  │
│       │                         │              └──────┬──────┘  │
│       │                         │                     │         │
│       │                         │           ┌─────────┴────────┐│
│       │                         │           │                  ││
│       │                         │         重试              不重试│
│       │                         │           │                  ││
│       │                         │           │          返回失败 ││
│       │                         │           │                  ││
│       │                         ▼           ▼                  ▼│
│       │←─────────────────────────────────────────────────────────│
│       │  返回 #{vertex, outbox, status}                        │
│       │                                                         │
│  ┌────┴────┐                                                    │
│  │ 检查    │                                                    │
│  │ status  │                                                    │
│  └────┬────┘                                                    │
│       │                                                         │
│  ┌────┴────────────────┐                                        │
│  │                     │                                        │
│ ok               {error, Reason}                                │
│  │                     │                                        │
│  ▼                     ▼                                        │
│ 更新顶点            记录失败                                     │
│ 收集 outbox         不更新顶点                                   │
│                     不发消息                                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 任务清单

```
┌─────────────────────────────────────────────────────────────────┐
│                    pregel_worker 修改任务                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 约定计算结果数据结构                                         │
│     □ 定义 compute_status() 类型                               │
│     □ 定义 compute_result() 类型                               │
│     □ 成功: #{vertex, outbox, status => ok}                    │
│     □ 失败: #{vertex, outbox => [], status => {error, Reason}} │
│                                                                 │
│  2. 修改 compute_vertices                                        │
│     □ 返回值增加 FailedVertices 列表                            │
│     □ 检查返回结果的 status 字段                                │
│     □ 成功: 更新顶点，收集 outbox                               │
│     □ 失败: 记录到 FailedVertices，不更新，不发消息             │
│                                                                 │
│  3. 修改 execute_superstep                                       │
│     □ 接收 compute_vertices 返回的 FailedVertices              │
│     □ 传递给 notify_master_done                                │
│                                                                 │
│  4. 修改 notify_master_done                                      │
│     □ 添加 failed_count 字段                                   │
│     □ 添加 failed_vertices 字段                                │
│                                                                 │
│  不需要:                                                         │
│  ✗ superstep_state 字段（简化设计不需要）                       │
│  ✗ try-catch 异常处理（由 graph_compute 负责）                  │
│  ✗ 重试逻辑（由 graph_compute 负责）                            │
│  ✗ max_retries 配置（由 graph_compute 管理）                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    graph_compute 修改任务                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 修改 compute_fn                                              │
│     □ 添加 try-catch 包装                                      │
│     □ 成功时添加 status => ok                                  │
│     □ 失败时返回 status => {error, Reason}                     │
│                                                                 │
│  2. 实现重试逻辑（可选）                                         │
│     □ should_retry/1 - 判断是否应该重试                        │
│     □ retry_compute/2 - 带重试次数的执行                       │
│     □ 配置最大重试次数                                          │
│     □ 可选：退避策略                                            │
│                                                                 │
│  3. 错误分类                                                     │
│     □ 可重试错误：timeout, temporary_failure                   │
│     □ 不可重试错误：其他                                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 总结

| 层 | 职责 | 修改内容 |
|----|------|----------|
| **pregel_worker** | 定义契约，根据 status 决定动作，上报失败信息 | compute_vertices, notify_master_done |
| **graph_compute** | 异常处理，重试逻辑，返回符合契约的结果 | compute_fn |

### 设计优点

| 优点 | 说明 |
|------|------|
| **职责清晰** | Pregel 层不关心如何处理错误，只关心结果 |
| **灵活性** | 重试策略可在 Graph 层自定义 |
| **简洁** | pregel_worker 不需要 superstep_state |
| **可扩展** | Graph 层可以实现复杂的错误处理策略 |

---

## 相关文档

- `pregel_error_reporting.md` - 错误上报机制分析
- `pregel_superstep_state_structure.md` - superstep_state 数据结构（备选方案）
- `pregel_pending_writes_placement.md` - pending_writes 放置分析
