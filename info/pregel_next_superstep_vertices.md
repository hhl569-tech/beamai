# Pregel 下一超级步顶点确定与错误检测

## 概述

本文档说明：
1. 超级步结束后，如何确定下一个超级步需要哪些节点参与
2. 如何检测当前步骤是否出错
3. 活跃顶点筛选的调用位置

## 1. 确定下一超级步参与节点

### 1.1 活跃顶点筛选函数

代码位置：`pregel_worker.erl:200-207`

```erlang
filter_active_vertices(Vertices, Inbox) ->
    InboxKeys = maps:keys(Inbox),
    maps:filter(
        fun(Id, V) ->
            %% 条件1: 收件箱中有消息
            lists:member(Id, InboxKeys)
            orelse
            %% 条件2: 顶点未停止（halted = false）
            pregel_vertex:is_active(V)
        end,
        Vertices
    ).
```

### 1.2 判断条件

一个顶点在下一超级步被激活，需要满足以下**任一**条件：

| 条件 | 说明 | 代码 |
|------|------|------|
| **有消息** | 该顶点的 inbox 中有待处理消息 | `lists:member(Id, InboxKeys)` |
| **未停止** | 顶点的 `halted = false` | `pregel_vertex:is_active(V)` |

### 1.3 调用者是 Worker，不是 Master

```
Master                                    Worker
───────                                   ──────

pregel_worker:execute_superstep(Pid, N)
        │
        │  {execute_superstep, N}
        └────────────────────────────────→ handle_cast()
                                                │
                                                ▼
                                          execute_superstep(State)
                                                │
                                                ▼
                                          filter_active_vertices(Vertices, Inbox)
                                                │
                                                ▼
                                          compute_vertices(ActiveVertices, ...)
```

代码位置：`pregel_worker.erl:167-194`

```erlang
execute_superstep(#state{vertices = Vertices, inbox = Inbox, ...} = State) ->
    %% 1. 筛选需要计算的顶点（有消息或活跃）
    ActiveVertices = filter_active_vertices(Vertices, Inbox),  %% ← Worker 调用

    %% 2. 执行所有顶点计算
    {NewVertices, Outbox} = compute_vertices(ActiveVertices, ...),

    %% 3. 应用合并器
    CombinedOutbox = apply_combiner(Outbox, Combiner),

    %% 4. 路由消息到目标 Worker
    route_messages(CombinedOutbox, State),

    %% 5. 通知 Master 完成
    notify_master_done(State, NewVertices, CombinedOutbox),

    %% 6. 返回更新后的状态
    State#state{vertices = NewVertices, inbox = #{}}.
```

### 1.4 关键点

| 项目 | 说明 |
|------|------|
| **调用者** | Worker（不是 Master） |
| **筛选范围** | Worker 本地的顶点（不是全图） |
| **数据来源** | Worker 自己的 `vertices` 和 `inbox` |
| **Master 角色** | 只负责触发超级步，不参与顶点筛选 |

### 1.5 消息传递决定激活

```
超级步 N                              超级步 N+1
┌─────────────────────┐              ┌─────────────────────┐
│  Node A 执行        │              │                     │
│    │                │              │  Node B 被激活      │
│    │ send_message   │              │  (收到消息)         │
│    │ to Node B      │  ─────────→  │    │                │
│    ▼                │    消息      │    ▼                │
│  vote_to_halt()     │    投递      │  执行计算           │
└─────────────────────┘              └─────────────────────┘

消息在超级步 N 发送，在超级步 N+1 被目标顶点接收
```

### 1.6 完整流程图

```
超级步 N 结束后，确定超级步 N+1 参与节点:

┌─────────────────────────────────────────────────────────────────┐
│                    Worker 本地状态                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  vertices: #{                    inbox: #{                      │
│    node_a => #{halted => true},    node_b => [{state, S1}],    │
│    node_b => #{halted => true},    node_c => [{state, S2}]     │
│    node_c => #{halted => true},  }                              │
│    node_d => #{halted => false}                                 │
│  }                                                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                filter_active_vertices()                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  node_a: halted=true,  无消息  → ❌ 不参与                      │
│  node_b: halted=true,  有消息  → ✅ 参与（有消息）              │
│  node_c: halted=true,  有消息  → ✅ 参与（有消息）              │
│  node_d: halted=false, 无消息  → ✅ 参与（未停止）              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
          超级步 N+1 活跃顶点: [node_b, node_c, node_d]
```

## 2. 错误检测机制

### 2.1 当前实现：无错误处理

代码位置：`pregel_worker.erl:217-230`

```erlang
compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    maps:fold(
        fun(Id, Vertex, {VAcc, OAcc}) ->
            Messages = maps:get(Id, Inbox, []),
            ActiveVertex = activate_if_has_messages(Vertex, Messages),
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),
            %% ⚠️ 没有 try-catch，异常会导致整个 fold 失败
            #{vertex := NewVertex, outbox := Out} = ComputeFn(Context),
            {VAcc#{Id => NewVertex}, Out ++ OAcc}
        end,
        {AllVertices, []},
        ActiveVertices
    ).
```

### 2.2 错误传播路径

```
顶点计算函数抛出异常
        │
        ▼
maps:fold 失败
        │
        ▼
execute_superstep 函数崩溃
        │
        ▼
Worker 进程崩溃
        │
        ▼
Master 永远收不到该 Worker 的 {worker_done, Result}
        │
        ▼
Barrier 永远无法完成 (received < expected)
        │
        ▼
整个 Pregel 执行卡住或超时
```

### 2.3 问题总结

| 问题 | 说明 |
|------|------|
| **无单顶点错误隔离** | 一个顶点失败导致整个 Worker 崩溃 |
| **无错误状态记录** | 不知道哪个顶点失败、失败原因 |
| **无恢复机制** | 无法重试失败的顶点 |
| **无错误上报** | Master 无法得知具体错误信息 |

### 2.4 与上层 graph_compute 的错误处理

虽然 Pregel 层没有错误处理，但上层 `graph_compute.erl` 中的计算函数有 try-catch：

```erlang
%% graph_compute.erl 中的节点执行
execute_node(Node, State) ->
    try
        %% 执行节点逻辑
        Result = graph_node:execute(Node, State),
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.
```

但这只是防止计算函数本身抛出异常，错误结果仍然会被包装后继续传递，而不是在 Pregel 层被感知。

## 3. 总结对比

| 方面 | 下一超级步节点确定 | 错误检测 |
|------|-------------------|---------|
| **机制** | 消息 + halted 状态 | ❌ 无机制 |
| **实现位置** | `filter_active_vertices` (Worker) | 无 |
| **判断条件** | 有消息 OR 未停止 | - |
| **当前问题** | 正常工作 | 单点失败导致全局卡死 |

## 4. 改进建议

如需实现错误检测和恢复，参考 `info/pregel_single_vertex_restart.md` 中的设计方案。
