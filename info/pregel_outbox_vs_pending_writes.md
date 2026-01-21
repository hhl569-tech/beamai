# Pregel outbox 与 LangGraph pending_writes 对比分析

## 核心差异：Channel 粒度

### LangGraph 的 Channel 机制

```
┌─────────────────────────────────────────────────────────────────┐
│              LangGraph Channel 机制（字段级）                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  GraphState 定义:                                                │
│  class State(TypedDict):                                        │
│      messages: Annotated[list, add_messages]  # Channel: append │
│      summary: str                              # Channel: last   │
│      score: Annotated[int, operator.add]      # Channel: sum    │
│                                                                 │
│  并行节点写入:                                                   │
│  ┌─────────────┐        ┌─────────────┐                        │
│  │   Node A    │        │   Node B    │                        │
│  │  writes:    │        │  writes:    │                        │
│  │  messages+  │        │  messages+  │                        │
│  │  score=3    │        │  score=5    │                        │
│  └──────┬──────┘        └──────┬──────┘                        │
│         │                      │                                │
│         └──────────┬───────────┘                                │
│                    ▼                                            │
│  pending_writes = [                                             │
│    (node_a, "messages", [msg1]),                               │
│    (node_a, "score", 3),                                        │
│    (node_b, "messages", [msg2]),                               │
│    (node_b, "score", 5)                                         │
│  ]                                                              │
│                    │                                            │
│                    ▼                                            │
│  合并结果（按 Channel 策略）:                                    │
│  {                                                              │
│    messages: [msg1, msg2],  # append                           │
│    score: 8                 # sum (3+5)                        │
│  }                                                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 我们的设计（整体 State 级）

```
┌─────────────────────────────────────────────────────────────────┐
│              我们的设计（整体 GraphState 级）                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  GraphState 定义:                                                │
│  -type graph_state() :: #{                                      │
│      messages := list(),                                        │
│      summary := binary(),                                       │
│      score := integer(),                                        │
│      ...                                                        │
│  }.                                                             │
│                                                                 │
│  消息传递（整体 State）:                                         │
│  ┌─────────────┐        ┌─────────────┐                        │
│  │   Node A    │        │   Node B    │                        │
│  │  sends:     │        │  sends:     │                        │
│  │  {state,    │        │  {state,    │                        │
│  │   StateA}   │        │   StateB}   │                        │
│  └──────┬──────┘        └──────┬──────┘                        │
│         │                      │                                │
│         └──────────┬───────────┘                                │
│                    ▼                                            │
│  outbox/inbox = [                                               │
│    {state, StateA},   # 整体 GraphState                        │
│    {state, StateB}    # 整体 GraphState                        │
│  ]                                                              │
│                    │                                            │
│                    ▼                                            │
│  合并（用户提供的 merge_fn）:                                    │
│  MergedState = merge_fn(StateA, StateB)                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 关键区别对比

| 特性 | LangGraph | 我们的设计 |
|------|-----------|-----------|
| **Channel 粒度** | 字段级（每个字段独立 Channel） | 整体级（整个 GraphState） |
| **合并策略** | 每字段独立策略（append/last/sum/custom） | 整体 merge_fn |
| **pending_writes 内容** | `[(node, channel, value)]` | `[{state, GraphState}]` |
| **需要 Channel 概念？** | ✅ 必须 | ❌ 不需要 |
| **并行写入处理** | 自动按 Channel 策略合并 | 调用用户 merge_fn |

---

## 为什么我们不需要 Channel？

### 原因 1：消息传递的是整体 State

```
┌─────────────────────────────────────────────────────────────────┐
│                    消息传递模式对比                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  LangGraph 模式:                                                 │
│  • 节点直接写入共享 State 的各字段                               │
│  • 需要 Channel 定义每字段的写入冲突解决策略                     │
│  • pending_writes 记录字段级写入                                │
│                                                                 │
│  我们的模式（Pregel 消息传递）:                                  │
│  • 节点发送完整的 {state, NewState} 消息                        │
│  • 接收节点收到多条消息时，调用 merge_fn 合并                   │
│  • outbox 记录完整的 State 消息                                 │
│  • 不需要字段级 Channel                                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 原因 2：合并责任在用户 merge_fn

```erlang
%% graph_compute.erl 中的状态合并
aggregate_state_messages(Messages) ->
    States = [S || {state, S} <- Messages],
    case States of
        [] -> {error, no_state_messages};
        [Single] -> {ok, Single};
        Multiple ->
            %% 用户提供的 merge_fn 负责合并
            MergedState = apply_merge_fn(Multiple, MergeFn),
            {ok, MergedState}
    end.

%% 用户定义合并策略（整体级）
MergeFn = fun(States) ->
    %% 用户决定如何合并多个 State
    %% 可以实现任意复杂的合并逻辑
    merge_states(States)
end.
```

### 原因 3：简化的数据流

```
LangGraph:
Node → 写入字段 → Channel 缓冲 → 按 Channel 合并 → 更新 State
                      ↑
               需要 pending_writes
               记录 (node, channel, value)

我们的设计:
Node → 发送 {state, S} → outbox 缓冲 → merge_fn 合并 → 传给下一节点
                              ↑
                    可扩展为 pending_writes
                    记录 {state, GraphState}
```

---

## outbox 扩展为 pending_writes

### 当前 outbox 结构

```erlang
%% 当前 compute_vertices 返回的 outbox
Outbox = [{TargetVertexId, Message}, ...]

%% 例如
Outbox = [
    {next_node, {state, #{messages => [...], score => 5}}},
    {another_node, {state, #{...}}}
]
```

### 扩展为 pending_writes

```erlang
%% 扩展后的 pending_writes 结构
-type pending_write() :: #{
    source_vertex := vertex_id(),       %% 发送方
    target_vertex := vertex_id(),       %% 接收方
    message := {state, graph_state()},  %% 完整的 State 消息
    timestamp := integer()              %% 时间戳（用于排序/去重）
}.

-type pending_writes() :: [pending_write()].

%% 例如
PendingWrites = [
    #{
        source_vertex => node_a,
        target_vertex => next_node,
        message => {state, #{messages => [msg1], score => 3}},
        timestamp => 1705123456789
    },
    #{
        source_vertex => node_b,
        target_vertex => next_node,
        message => {state, #{messages => [msg2], score => 5}},
        timestamp => 1705123456790
    }
]
```

### 不需要 Channel 的 pending_writes

```
┌─────────────────────────────────────────────────────────────────┐
│            我们的 pending_writes（无 Channel）                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  LangGraph pending_writes:                                      │
│  [(node_id, channel_name, value)]                              │
│       ↑         ↑          ↑                                   │
│    来源节点   字段名     字段值                                  │
│                                                                 │
│  我们的 pending_writes:                                         │
│  [(source_vertex, target_vertex, {state, GraphState})]         │
│       ↑              ↑                    ↑                    │
│    来源顶点       目标顶点           完整状态                    │
│                                                                 │
│  区别:                                                          │
│  • 没有 channel_name（我们传递整体 State）                      │
│  • 有 target_vertex（我们是消息传递模型）                       │
│  • value 是完整 GraphState（不是单个字段值）                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 总结

| 问题 | 回答 |
|------|------|
| **我们的 Channel 与 LangGraph 不同？** | ✅ 是的，我们是整体 State 级，LangGraph 是字段级 |
| **需要字段级 Channel？** | ❌ 不需要，我们用 merge_fn 处理整体 State |
| **outbox 可扩展为 pending_writes？** | ✅ 可以，记录 `(source, target, {state, S})` |
| **需要 channel() 字段？** | ❌ 不需要，我们传递完整 GraphState |

### 简化的 pending_writes 定义

```erlang
%% 我们的 pending_writes（简化版，无 Channel）
-type pending_writes() :: [#{
    source := vertex_id(),
    target := vertex_id(),
    state := graph_state(),
    timestamp := integer()
}].
```

这个设计保持了 Pregel 消息传递模型的简洁性，同时获得了 pending_writes 的错误恢复能力。

---

---

## 完整的 pending_writes 设计

虽然我们不需要 Channel，但仍然需要保存每个顶点的执行结果（成功/失败/中断）。

### pending_writes 的两部分

```
┌─────────────────────────────────────────────────────────────────┐
│              完整的 pending_writes 结构                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 执行结果追踪（superstep_state）                              │
│  ──────────────────────────────────                             │
│  记录每个顶点的执行状态：                                        │
│  #{                                                             │
│    vertex_a => #{status => completed, ...},                    │
│    vertex_b => #{status => failed, error => Reason},           │
│    vertex_c => #{status => interrupted, reason => need_input}  │
│  }                                                              │
│                                                                 │
│  用途：                                                          │
│  • 重试时跳过已完成的顶点                                        │
│  • 错误恢复时知道哪些顶点需要重新执行                            │
│  • human-in-the-loop 时知道哪些顶点等待输入                     │
│                                                                 │
│  2. 消息/输出追踪（outbox → pending_writes）                    │
│  ────────────────────────────────────────                       │
│  记录每个顶点发出的消息：                                        │
│  [                                                              │
│    #{source => vertex_a, target => vertex_b, state => S1},     │
│    #{source => vertex_a, target => vertex_c, state => S2}      │
│  ]                                                              │
│                                                                 │
│  用途：                                                          │
│  • 恢复时重新投递未送达的消息                                    │
│  • 确保消息不丢失                                                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 完整的数据结构

```erlang
%% 顶点执行状态
-type vertex_status() :: ok | {error, term()} | {interrupt, term()}.

%% 顶点执行结果（superstep_state 的内容）
-type vertex_result() :: #{
    status := vertex_status(),

    %% 成功时
    vertex => vertex(),                     %% 执行后的顶点状态
    outbox => [{vertex_id(), term()}],      %% 发出的消息

    %% 失败时
    error => term(),                        %% 错误原因

    %% 中断时
    interrupt_reason => term(),             %% 中断原因（如 need_input）

    %% 通用
    attempts => non_neg_integer(),          %% 尝试次数
    input_vertex => vertex(),               %% 执行前的顶点（用于重试）
    input_messages => [term()]              %% 收到的消息（用于重试）
}.

%% superstep_state：每个顶点的执行结果
-type superstep_state() :: #{vertex_id() => vertex_result()}.

%% pending_write：单条待处理的消息
-type pending_write() :: #{
    source := vertex_id(),
    target := vertex_id(),
    state := graph_state(),
    timestamp => integer()
}.

%% pending_writes：所有待处理的消息
-type pending_writes() :: [pending_write()].
```

### 两者的关系

```
┌─────────────────────────────────────────────────────────────────┐
│                superstep_state 与 pending_writes 的关系          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  顶点执行                                                        │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  superstep_state[vertex_id] = #{                        │   │
│  │    status => completed,                                 │   │
│  │    vertex => NewVertex,                                 │   │
│  │    outbox => [{target1, S1}, {target2, S2}]  ──────────┼───┤
│  │  }                                                      │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                          │      │
│                                                          ▼      │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  pending_writes = [                                     │   │
│  │    #{source => vertex_id, target => target1, state => S1},│  │
│  │    #{source => vertex_id, target => target2, state => S2} │  │
│  │  ]                                                      │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│  关系：                                                          │
│  • superstep_state 记录执行状态（成功/失败/中断）               │
│  • pending_writes 是从 superstep_state.outbox 提取的消息       │
│  • superstep_state 是源头，pending_writes 是衍生                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 恢复场景示例

```
场景：并行执行 3 个顶点，vertex_b 失败

超步 N 执行后的 superstep_state:
#{
    vertex_a => #{status => ok, vertex => V1, outbox => [...]},
    vertex_b => #{status => {error, timeout}, attempts => 1, ...},
    vertex_c => #{status => ok, vertex => V3, outbox => [...]}
}

重试时：
1. 检查 superstep_state
2. vertex_a, vertex_c 已完成 → 跳过，复用结果
3. vertex_b 失败 → 重新执行
4. 合并所有结果
```

---

## 结论：只需要 superstep_state

### 为什么不需要独立的 pending_writes？

```
┌─────────────────────────────────────────────────────────────────┐
│              superstep_state 已包含所有信息                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  superstep_state = #{                                           │
│    vertex_a => #{                                               │
│      status => ok,                                              │
│      vertex => NewVertex,                                       │
│      outbox => [{target1, S1}, {target2, S2}]  ← 消息已在这里   │
│    },                                                           │
│    vertex_b => #{                                               │
│      status => {error, Reason},                                │
│      ...                                                        │
│    }                                                            │
│  }                                                              │
│                                                                 │
│  pending_writes 的内容 = 所有成功顶点的 outbox 合并              │
│  → 这是冗余数据，可以随时从 superstep_state 提取                 │
│  → 无需单独维护                                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 按需提取 pending_writes

```erlang
%% 不需要独立维护 pending_writes
%% 需要时从 superstep_state 动态提取

-spec extract_pending_writes(superstep_state()) -> [{vertex_id(), term()}].
extract_pending_writes(SuperstepState) ->
    maps:fold(
        fun(_VertexId, #{status := ok, outbox := Outbox}, Acc) ->
                Outbox ++ Acc;
           (_, _, Acc) ->
                Acc
        end,
        [],
        SuperstepState
    ).
```

### 最终设计

```
┌─────────────────────────────────────────────────────────────────┐
│                    最终设计：只需 superstep_state                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  superstep_state = #{vertex_id() => vertex_result()}            │
│                                                                 │
│  vertex_result() = #{                                           │
│    status := ok | {error, term()} | {interrupt, term()},       │
│                                                                 │
│    %% 成功时                                                    │
│    vertex => vertex(),              %% 新顶点状态                │
│    outbox => [{vertex_id(), term()}], %% 发出的消息 ← 即 pending_writes │
│                                                                 │
│    %% 失败/中断时                                                │
│    error => term(),                 %% 错误原因                  │
│                                                                 │
│    %% 通用（用于重试）                                           │
│    attempts => non_neg_integer(),   %% 尝试次数                  │
│    input_vertex => vertex(),        %% 执行前的顶点              │
│    input_messages => [term()]       %% 收到的消息                │
│  }                                                              │
│                                                                 │
│  优点：                                                          │
│  ✅ 单一数据源，无冗余                                           │
│  ✅ outbox 已包含 pending_writes 的全部信息                      │
│  ✅ 需要时动态提取，减少内存占用                                 │
│  ✅ 简化数据结构，降低维护成本                                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 总结

| 问题 | 回答 |
|------|------|
| **需要独立的 pending_writes？** | ❌ 不需要 |
| **为什么？** | superstep_state.outbox 已包含消息，无需冗余存储 |
| **如何获取 pending_writes？** | 从 superstep_state 动态提取 |
| **最终设计** | **只需要 superstep_state** |

---

## 相关文档

- `pregel_message_reliability_analysis.md` - 消息可靠性分析
- `pregel_pending_writes_analysis.md` - pending_writes 需求分析
- `pregel_superstep_state_structure.md` - superstep_state 数据结构
- `pregel_channel_mechanism_analysis.md` - Channel 机制分析
