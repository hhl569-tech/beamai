# pending_writes 机制放置位置分析

## 问题

要为 Graph 添加 LangGraph 中的 pending_writes 机制，应该将它加到何处？

---

## LangGraph pending_writes 的作用

```
┌─────────────────────────────────────────────────────────────────┐
│              pending_writes 机制放置位置分析                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  LangGraph pending_writes 的两个作用:                            │
│                                                                 │
│  1. 【执行追踪】记录每个节点的写入操作                            │
│     → 用于单顶点重试时复用已完成结果                              │
│     → 类似我们的 superstep_state                                │
│                                                                 │
│  2. 【Channel 写入】记录对各 Channel 的写入                       │
│     → 用于状态合并时应用正确的合并策略                            │
│     → 类似我们的 Channel 机制                                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 分层建议

**结论：pending_writes 应该分两部分实现，分别放在不同层。**

```
┌─────────────────────────────────────────────────────────────────┐
│                    pending_writes 分层实现                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                     Graph 层                             │   │
│  │                                                          │   │
│  │  pending_writes 的 Channel 部分:                         │   │
│  │  • 记录对各字段的写入: {messages, [...]}                 │   │
│  │  • 定义合并策略: append, last_value, reducer            │   │
│  │  • 在 graph_compute.erl 或新模块 graph_channel.erl      │   │
│  │                                                          │   │
│  │  位置: graph_channel.erl (新模块)                        │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              │ 通过 compute_fn 调用             │
│                              ▼                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    Pregel Worker 层                      │   │
│  │                                                          │   │
│  │  pending_writes 的执行追踪部分:                           │   │
│  │  • 记录顶点执行状态: completed/failed/pending            │   │
│  │  • 保存已完成顶点的 outbox                               │   │
│  │  • 支持失败重试时复用已完成结果                           │   │
│  │                                                          │   │
│  │  位置: pregel_worker.erl (superstep_state)              │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 具体放置建议

| 功能 | 位置 | 模块 | 说明 |
|------|------|------|------|
| **执行状态追踪** | Pregel Worker | `pregel_worker.erl` | 新增 `superstep_state` 字段 |
| **Channel 写入记录** | Graph 层 | `graph_channel.erl` (新) | 新增模块 |
| **状态合并逻辑** | Graph 层 | `graph_compute.erl` | 修改 `aggregate_state_messages` |
| **持久化 Checkpoint** | Graph 层 | `graph_checkpoint.erl` (新) | 可选，用于持久化恢复 |

---

## 完整架构图

```
┌─────────────────────────────────────────────────────────────────┐
│                      完整架构                                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Graph 定义层                                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  graph_builder.erl                                      │   │
│  │  • 定义节点、边                                          │   │
│  │  • 可选：指定 Channel 配置                               │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  Graph 执行层                                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  graph_compute.erl                                      │   │
│  │  • compute_fn 计算函数                                   │   │
│  │  • 调用 graph_channel 进行状态合并                       │   │
│  │                                                          │   │
│  │  graph_channel.erl (新)                                  │   │
│  │  • Channel 定义和默认配置                                │   │
│  │  • merge_with_channels 合并逻辑                         │   │
│  │  • 记录 pending_writes（写入操作）                       │   │
│  │                                                          │   │
│  │  graph_checkpoint.erl (可选，新)                         │   │
│  │  • 持久化 pending_writes                                │   │
│  │  • 超步级 checkpoint                                    │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  Pregel 框架层                                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  pregel_worker.erl                                      │   │
│  │  • superstep_state: 追踪顶点执行状态                     │   │
│  │  • 支持单顶点重试                                        │   │
│  │                                                          │   │
│  │  pregel_master.erl                                      │   │
│  │  • 保持不变，不感知 pending_writes                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 各模块职责详解

### 1. pregel_worker.erl - 执行状态追踪

```erlang
%% 新增 superstep_state 字段
-record(state, {
    ...
    %% 新增：超步执行状态
    superstep_state :: #{vertex_id() => vertex_result()}
}).

%% 顶点执行结果
-type vertex_result() :: #{
    status := pending | completed | failed,
    vertex => vertex(),                    %% 新顶点状态（completed 时）
    outbox => [{vertex_id(), term()}],     %% 发出的消息（completed 时）
    error => term(),                       %% 错误原因（failed 时）
    attempts => non_neg_integer()          %% 尝试次数
}.
```

**职责：**
- 追踪每个顶点的执行状态（pending/completed/failed）
- 保存已完成顶点的结果（vertex 和 outbox）
- 支持失败顶点重试时复用已完成结果
- 超步结束后清空

### 2. graph_channel.erl - Channel 写入记录（新模块）

```erlang
-module(graph_channel).

%% Channel 类型定义
-type channel_type() ::
    last_value |           %% 后者覆盖（默认）
    first_value |          %% 保留首个
    append |               %% 列表追加
    {reducer, fun()} |     %% 自定义规约
    {merger, fun()}.       %% 自定义合并

%% Channel 配置
-type channel_config() :: #{
    atom() => channel_type()  %% 字段名 => 合并策略
}.

%% pending_writes 记录
-type pending_write() :: #{
    node_id := node_id(),
    channel := atom(),
    value := term(),
    timestamp := integer()
}.

-type pending_writes() :: [pending_write()].

%% API
-export([default_channels/0]).
-export([merge_with_channels/3]).
-export([record_write/4]).
-export([get_pending_writes/1]).
```

**职责：**
- 定义 Channel 类型和默认配置
- 实现状态合并逻辑（merge_with_channels）
- 记录每个节点对各 Channel 的写入
- 提供 pending_writes 查询接口

### 3. graph_compute.erl - 修改状态合并

```erlang
%% 修改 aggregate_state_messages 使用 Channel
aggregate_state_messages(Messages) ->
    aggregate_state_messages(Messages, graph_channel:default_channels()).

aggregate_state_messages(Messages, Channels) ->
    States = [S || {state, S} <- Messages],
    case States of
        [] -> {error, no_state_messages};
        [Single] -> {ok, Single};
        Multiple ->
            MergedState = merge_states_with_channels(Multiple, Channels),
            {ok, MergedState}
    end.

merge_states_with_channels([First | Rest], Channels) ->
    lists:foldl(
        fun(State, Acc) ->
            graph_channel:merge_with_channels(Acc, State, Channels)
        end,
        First,
        Rest
    ).
```

### 4. graph_checkpoint.erl - 持久化（可选，新模块）

```erlang
-module(graph_checkpoint).

%% 超步级 checkpoint 结构
-record(superstep_checkpoint, {
    execution_id :: binary(),
    superstep :: non_neg_integer(),
    vertices :: #{vertex_id() => vertex()},
    inbox :: #{vertex_id() => [term()]},
    pending_writes :: pending_writes(),
    timestamp :: integer()
}).

%% API
-export([save/2, load/1]).
-export([save_pending_writes/2]).
-export([recover_from_checkpoint/1]).
```

**职责：**
- 持久化 pending_writes（用于崩溃恢复）
- 超步级 checkpoint 保存和加载
- 支持从 checkpoint 恢复执行

---

## 数据流图

```
┌─────────────────────────────────────────────────────────────────┐
│                    pending_writes 数据流                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  节点执行:                                                       │
│  ┌─────────┐                                                    │
│  │ Node A  │                                                    │
│  │ 执行    │                                                    │
│  └────┬────┘                                                    │
│       │                                                         │
│       │ 1. 执行完成                                              │
│       ▼                                                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  pregel_worker.superstep_state                          │   │
│  │  #{                                                      │   │
│  │    node_a => #{                                         │   │
│  │      status => completed,                               │   │
│  │      vertex => NewVertex,                               │   │
│  │      outbox => [{node_b, {state, S}}]                   │   │
│  │    }                                                     │   │
│  │  }                                                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│       │                                                         │
│       │ 2. 发送消息                                              │
│       ▼                                                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  graph_channel.pending_writes                           │   │
│  │  [                                                       │   │
│  │    #{node_id => node_a, channel => messages, value => V} │   │
│  │  ]                                                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│       │                                                         │
│       │ 3. 接收节点合并                                          │
│       ▼                                                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  graph_compute.aggregate_state_messages                 │   │
│  │  使用 Channel 配置合并 pending_writes                    │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 错误恢复流程

```
┌─────────────────────────────────────────────────────────────────┐
│                使用 pending_writes 的错误恢复                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  超级步 N 执行中:                                                │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐                         │
│  │ Node A  │  │ Node B  │  │ Node C  │                         │
│  │   ✅    │  │   ❌    │  │   ✅    │                         │
│  │ 完成    │  │  失败   │  │ 完成    │                         │
│  └─────────┘  └─────────┘  └─────────┘                         │
│       │            │            │                               │
│       ▼            ▼            ▼                               │
│  superstep_state = #{                                           │
│    node_a => #{status => completed, vertex => V1, outbox => O1},│
│    node_b => #{status => failed, error => Reason},              │
│    node_c => #{status => completed, vertex => V3, outbox => O3} │
│  }                                                              │
│                                                                 │
│  重试 Node B:                                                    │
│  1. 从 superstep_state 获取 node_a, node_c 的已完成结果         │
│  2. 只重新执行 node_b                                           │
│  3. 合并所有结果                                                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 总结

| 问题 | 答案 |
|------|------|
| **pending_writes 放哪层？** | **分两部分**：执行追踪在 Pregel Worker，Channel 写入在 Graph 层 |
| **执行追踪（superstep_state）** | `pregel_worker.erl` - 新增字段 |
| **Channel 写入** | `graph_channel.erl` - 新模块 |
| **状态合并** | `graph_compute.erl` - 修改现有函数 |
| **持久化（可选）** | `graph_checkpoint.erl` - 新模块 |
| **为什么分层？** | Pregel 保持通用，Graph 层处理业务逻辑 |

---

## 建议实现顺序

```
┌─────────────────────────────────────────────────────────────────┐
│                      实现优先级                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 【必须】pregel_worker.erl 添加 superstep_state              │
│     → 支持顶点执行状态追踪                                       │
│     → 支持单顶点重试                                            │
│     → 修改 compute_vertices 添加 try-catch                      │
│                                                                 │
│  2. 【必须】graph_channel.erl 新模块                            │
│     → Channel 定义和配置                                        │
│     → 状态合并逻辑 merge_with_channels                          │
│     → pending_writes 记录                                       │
│                                                                 │
│  3. 【必须】graph_compute.erl 修改                              │
│     → aggregate_state_messages 使用 Channel                     │
│     → 调用 graph_channel 进行合并                               │
│                                                                 │
│  4. 【可选】graph_checkpoint.erl 新模块                         │
│     → 持久化 pending_writes                                     │
│     → 超步级 checkpoint（用于崩溃恢复）                          │
│     → 仅当需要 Level 2+ 恢复能力时实现                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 相关文档

- `pregel_pending_writes_analysis.md` - pending_writes 需求分析
- `pregel_superstep_state_location.md` - superstep_state 位置分析
- `pregel_channel_placement.md` - Channel 机制放置分析
- `pregel_channel_mechanism_analysis.md` - Channel 机制详细分析
- `pregel_checkpoint_layer_analysis.md` - Checkpoint 层分析
