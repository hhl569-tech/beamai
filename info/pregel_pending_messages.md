# Pregel pending_messages 机制

## 概述

`pending_messages` 是 Master 进程中的**消息缓冲区**，用于缓存无法立即投递的跨 Worker 消息。

## 定义

代码位置：`pregel_master.erl:59`

```erlang
pending_messages :: #{non_neg_integer() => [{term(), term()}]}
%%                    ↑ WorkerId            ↑ 消息列表 [{VertexId, Value}]
```

## 作用

当 Worker 发送跨 Worker 消息时，如果目标 Worker 暂时不可用，消息会被缓存到 Master 的 `pending_messages` 中，等待超步完成时统一投递。

## 消息路由流程

代码位置：`pregel_master.erl:350-364`

```erlang
handle_route_messages(TargetWorkerId, Messages, State) ->
    case maps:get(TargetWorkerId, Workers, undefined) of
        undefined ->
            %% ❌ 目标 Worker 不存在，缓存消息
            Existing = maps:get(TargetWorkerId, Pending, []),
            State#state{pending_messages = Pending#{TargetWorkerId => Messages ++ Existing}};
        Pid ->
            %% ✅ 目标 Worker 存在，直接发送
            pregel_worker:receive_messages(Pid, Messages),
            State
    end.
```

## 生命周期

### 1. 初始化（空）

```erlang
%% pregel_master.erl:112
pending_messages = #{}
```

### 2. 超步执行期间（累积）

当消息无法直接投递时，缓存到 `pending_messages`。

### 3. 超步完成时（投递）

代码位置：`pregel_master.erl:264-265`

```erlang
complete_superstep(State) ->
    %% ...
    %% 2. 发送待处理消息
    deliver_pending_messages(PendingMessages, Workers),
    %% ...
```

投递函数：`pregel_master.erl:279-290`

```erlang
deliver_pending_messages(PendingMessages, Workers) ->
    maps:foreach(
        fun(TargetWorkerId, Messages) ->
            case maps:get(TargetWorkerId, Workers, undefined) of
                undefined -> ok;
                Pid -> pregel_worker:receive_messages(Pid, Messages)
            end
        end,
        PendingMessages
    ).
```

### 4. 下一超步开始（清空）

代码位置：`pregel_master.erl:298`

```erlang
start_next_superstep(State) ->
    NewState = State#state{
        superstep = Superstep + 1,
        barrier = pregel_barrier:new(maps:size(Workers)),
        pending_messages = #{}  %% 清空
    },
```

## 流程图

```
超步 N 执行中:

┌──────────┐                              ┌──────────┐
│ Worker 0 │ ──── 发送消息到 Worker 2 ───→│  Master  │
└──────────┘                              └──────────┘
                                               │
                          ┌────────────────────┴────────────────────┐
                          │                                         │
                          ▼                                         ▼
                   Worker 2 存在？                           Worker 2 不存在？
                          │                                         │
                          ▼                                         ▼
              直接发送给 Worker 2                      缓存到 pending_messages
                                                      #{2 => [{vertex_id, msg}]}


超步 N 完成时:

┌──────────┐
│  Master  │ ─── deliver_pending_messages() ──→ 发送给所有目标 Worker
└──────────┘


超步 N+1 开始:

pending_messages = #{}  %% 清空，重新开始累积
```

## 使用场景

`pending_messages` 主要处理以下情况：

1. **Worker 启动时序问题**：某些 Worker 可能还未完全初始化
2. **消息路由延迟**：通过 Master 转发的消息需要缓冲
3. **批量投递优化**：超步结束时统一投递，减少消息碎片

## 与 LangGraph pending_writes 的对比

| 特性 | 我们的 pending_messages | LangGraph pending_writes |
|------|------------------------|-------------------------|
| **用途** | 缓存跨 Worker 消息 | 记录并行节点的部分完成结果 |
| **触发场景** | Worker 暂时不可用 | 并行执行中某节点失败 |
| **持久化** | ❌ 不持久化 | ✅ 持久化到 checkpoint |
| **恢复能力** | 进程崩溃则丢失 | 可以从 checkpoint 恢复，避免重复执行 |
| **清空时机** | 每个超步开始时 | checkpoint 提交后 |

## LangGraph pending_writes 参考

LangGraph 的 `pending_writes` 用于错误恢复场景：

```python
# LangGraph checkpoint 结构
checkpoint = {
    "pending_writes": [
        # (node_id, channel_name, value)
        ("node_a", "messages", [...]),
        ("node_b", "score", 5),
    ]
}
```

当并行执行的多个节点中某个失败时：
- 已完成节点的结果保存在 `pending_writes`
- 恢复执行时，跳过已完成的节点
- 只重新执行失败的节点

## 当前实现的局限性

1. **不持久化**：`pending_messages` 存储在进程状态中，Master 崩溃则丢失
2. **无错误恢复**：无法利用它来实现 LangGraph 式的部分完成恢复
3. **仅限消息缓冲**：不记录节点执行结果，只是消息的临时缓冲

## 改进建议

如果需要实现类似 LangGraph 的错误恢复能力，可以：

1. 将 `pending_messages` 持久化到 checkpoint
2. 增加 `pending_writes` 概念，记录每个节点的执行结果
3. 支持从 checkpoint 恢复时，跳过已完成的节点

```erlang
%% 扩展的 checkpoint 结构
-record(superstep_checkpoint, {
    superstep :: non_neg_integer(),
    pending_messages :: #{worker_id() => [message()]},
    pending_writes :: [{node_id(), channel(), value()}],  %% 新增
    %% ...
}).
```
