# Pregel 执行元信息存储位置分析

## 概述

在当前实现中，Pregel 执行的元信息（如当前超步、执行到哪个节点、各节点执行结果等）分散存储在多个位置，没有统一的 checkpoint 机制。

## 元信息存储位置

### 1. Pregel Master 进程状态

代码位置：`pregel_master.erl:48-60`

```erlang
-record(state, {
    graph            :: graph(),                    %% 原始图
    compute_fn       :: compute_fn(),               %% 计算函数
    combiner         :: pregel_combiner:spec(),     %% 合并器
    max_supersteps   :: pos_integer(),              %% 最大超步数
    num_workers      :: pos_integer(),              %% Worker 数
    workers          :: #{non_neg_integer() => pid()}, %% Worker 映射
    superstep        :: non_neg_integer(),          %% ⭐ 当前超步
    barrier          :: pregel_barrier:t(),         %% 同步屏障
    caller           :: gen_server:from(),          %% 调用者
    on_superstep     :: fun(),                      %% 超步回调
    pending_messages :: #{...}                      %% 待发送消息
}).
```

**存储的元信息**：
- `superstep` - 当前超步编号
- `barrier` - 同步屏障状态（哪些 Worker 已完成）
- `pending_messages` - 待发送的消息

### 2. Pregel Worker 进程状态

代码位置：`pregel_worker.erl:55-66`

```erlang
-record(state, {
    worker_id     :: non_neg_integer(),    %% Worker ID
    master        :: pid(),                 %% Master 进程
    vertices      :: #{vertex_id() => vertex()},  %% 本地顶点
    inbox         :: #{vertex_id() => [term()]},  %% ⭐ 收件箱
    compute_fn    :: fun(),                 %% 计算函数
    combiner      :: pregel_combiner:spec(),%% 合并器
    superstep     :: non_neg_integer(),     %% ⭐ 当前超步
    num_workers   :: pos_integer(),         %% Worker 总数
    num_vertices  :: non_neg_integer(),     %% 全图顶点总数
    worker_pids   :: #{...}                 %% Worker PID 映射
}).
```

**存储的元信息**：
- `superstep` - 当前超步编号
- `inbox` - 待处理的消息（下一超步要处理的）
- `vertices` - 本地顶点的当前状态

### 3. 计算上下文 Context

代码位置：`pregel_worker.erl:46-52`

```erlang
-type context() :: #{
    vertex := vertex(),              %% 当前顶点
    messages := [term()],            %% 收到的消息
    superstep := non_neg_integer(),  %% ⭐ 当前超步编号
    num_vertices := non_neg_integer(),%% 全图顶点数
    outbox := [{vertex_id(), term()}] %% 待发送消息
}.
```

**传递的元信息**：
- `superstep` - 当前超步编号（传递给计算函数）

### 4. 顶点值 Vertex Value

代码位置：`graph_compute.erl:31-37`

```erlang
-type vertex_value() :: #{
    node := graph_node:graph_node(),     %% 节点定义
    edges := [graph_edge:edge()],        %% 出边定义
    result := undefined | {ok, state()} | {error, term()}, %% ⭐ 执行结果
    initial_state => graph_state:state(),%% 初始状态（仅 __start__）
    activated => boolean()               %% 是否激活（仅 __start__）
}.
```

**存储的元信息**：
- `result` - 该节点的执行结果（成功/失败/未执行）

### 5. Graph State（业务状态）

代码位置：`beamai_agent_runner.erl:85-96`

```erlang
InitState = graph:state(#{
    messages => AllMessages,           %% 对话消息
    full_messages => AllFullMessages,  %% 完整历史
    system_prompt => Prompt,           %% 系统提示
    tools => ToolSpecs,                %% 工具定义
    max_iterations => MaxIter,         %% 最大迭代数
    iteration => 0,                    %% ⭐ 迭代计数
    scratchpad => [],                  %% 中间步骤
    context => ContextWithInput,       %% 用户上下文
    callbacks => CallbacksMap,         %% 回调函数
    callback_meta => CallbackMeta      %% 回调元数据
}).
```

**存储的元信息**：
- `iteration` - Agent 层面的迭代计数

### 6. Pregel 执行结果

代码位置：`pregel_master.erl:40-45`

```erlang
-type result() :: #{
    status := completed | max_supersteps,  %% ⭐ 执行状态
    graph := graph(),                      %% 最终图（含所有顶点结果）
    supersteps := non_neg_integer(),       %% ⭐ 总超步数
    stats := #{atom() => term()}           %% 统计信息
}.
```

**返回的元信息**：
- `status` - 执行结束状态
- `supersteps` - 执行了多少超步
- `graph` - 最终图状态（包含各顶点的 result）

## 元信息分布图

```
┌─────────────────────────────────────────────────────────────────────┐
│                         元信息分布位置                               │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌────────────────────┐  ┌────────────────────┐  ┌───────────────┐ │
│  │  Master 进程状态    │  │  Worker 进程状态    │  │ Vertex Value  │ │
│  │  ──────────────    │  │  ──────────────    │  │ ───────────── │ │
│  │  • superstep       │  │  • superstep       │  │ • result      │ │
│  │  • barrier         │  │  • inbox           │  │ • node        │ │
│  │  • pending_msgs    │  │  • vertices        │  │ • edges       │ │
│  └────────────────────┘  └────────────────────┘  └───────────────┘ │
│           │                      │                      │          │
│           └──────────────────────┴──────────────────────┘          │
│                                  │                                  │
│                    ❌ 存在进程状态中，未持久化                        │
│                                                                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    Graph State（业务状态）                    │   │
│  │  ─────────────────────────────────────────────────────────  │   │
│  │  • iteration      • messages      • context                 │   │
│  │  • scratchpad     • full_messages • callbacks               │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                  │                                  │
│                    ✅ 执行完成后被 Checkpoint 保存                   │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## 与 LangGraph 的对比

| 元信息 | LangGraph 存储位置 | 我们的实现 |
|--------|-------------------|-----------|
| 当前超步 | `checkpoint.metadata.step` | Master/Worker 进程状态（未持久化） |
| 执行到哪个节点 | `checkpoint.metadata.source` | 隐含在消息传递中（无显式记录） |
| 各节点结果 | `checkpoint.channel_values` | `vertex_value.result`（在图结构中） |
| 待处理写入 | `checkpoint.pending_writes` | Master/Worker 进程状态（未持久化） |
| Channel 版本 | `checkpoint.channel_versions` | 无对应实现 |

## LangGraph Checkpoint 结构参考

```python
checkpoint = {
    "id": "checkpoint-uuid",
    "ts": "2024-01-01T00:00:00Z",
    "channel_values": {           # 所有 channel 的当前值
        "messages": [...],
        "current_node": "llm_call",
        ...
    },
    "channel_versions": {         # 版本号（冲突检测）
        "messages": 5,
        "current_node": 3,
        ...
    },
    "pending_writes": [           # 待处理写入（错误恢复）
        ("node_a", "messages", [...]),
        ...
    ],
    "metadata": {
        "source": "loop",         # 来源：loop/update/fork
        "step": 5,                # 当前步骤
        "parents": {...}          # 父节点信息
    }
}
```

## 当前实现的问题

### 1. 超步信息未持久化

`superstep` 存储在 Master/Worker 进程状态中，进程崩溃则丢失。

### 2. "执行到哪个节点" 无显式记录

当前执行位置隐含在消息传递中：
- 哪个顶点收到消息 = 下一步要执行的节点
- 没有显式的 `current_node` 字段

### 3. 待处理消息未持久化

`inbox` 和 `pending_messages` 存在进程状态中，无法恢复。

### 4. 无法实现超步级别的恢复

如果执行到超步 N 时失败，无法从超步 N 恢复，只能从头开始。

## 改进建议

如果要实现超步级别的 checkpoint，需要：

### 1. 定义统一的 Checkpoint 结构

```erlang
-record(superstep_checkpoint, {
    superstep :: non_neg_integer(),           %% 当前超步
    graph_state :: graph_state:state(),       %% 业务状态
    vertex_results :: #{vertex_id() => term()},%% 各顶点执行结果
    pending_messages :: [{vertex_id(), term()}],%% 待处理消息
    active_vertices :: [vertex_id()],         %% 活跃顶点列表
    metadata :: map()                         %% 元数据
}).
```

### 2. 在超步完成时保存

在 `pregel_master:complete_superstep/1` 中添加：

```erlang
complete_superstep(State) ->
    %% ... 现有逻辑 ...

    %% 检查是否需要 checkpoint
    case should_checkpoint(Superstep, CheckpointInterval) of
        true ->
            save_superstep_checkpoint(State);
        false ->
            ok
    end,

    %% ... 继续执行 ...
```

### 3. 支持从 Checkpoint 恢复

```erlang
restore_from_checkpoint(CheckpointId) ->
    {ok, Checkpoint} = load_checkpoint(CheckpointId),
    %% 重建 Master/Worker 状态
    %% 从 pending_messages 恢复消息
    %% 继续执行
```
