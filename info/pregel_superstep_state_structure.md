# superstep_state 数据结构设计

## 概述

`superstep_state` 用于在 Worker 中追踪当前超步内每个顶点的执行状态，支持单顶点重试和错误恢复。

---

## 类型定义

```erlang
%% 顶点执行状态
-type vertex_status() :: pending      %% 待执行
                       | in_progress  %% 执行中
                       | completed    %% 已完成
                       | failed.      %% 已失败

%% 单个顶点的执行结果
-type vertex_result() :: #{
    %% 必须字段
    status := vertex_status(),

    %% completed 时的字段
    vertex => vertex(),                    %% 执行后的新顶点状态
    outbox => [{vertex_id(), term()}],     %% 发出的消息

    %% failed 时的字段
    error => term(),                       %% 错误原因
    stacktrace => list(),                  %% 堆栈信息（可选）

    %% 重试相关
    attempts => non_neg_integer(),         %% 尝试次数

    %% 输入信息（用于重试）
    input_messages => [term()],            %% 收到的消息
    input_vertex => vertex()               %% 执行前的顶点状态
}.

%% superstep_state 类型
-type superstep_state() :: #{vertex_id() => vertex_result()}.
```

---

## 各状态下的数据示例

```
┌─────────────────────────────────────────────────────────────────┐
│                 superstep_state 各状态示例                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. pending（待执行）                                            │
│  ─────────────────────                                          │
│  #{                                                             │
│    status => pending,                                           │
│    attempts => 0,                                               │
│    input_messages => [{state, S1}],                            │
│    input_vertex => OriginalVertex                               │
│  }                                                              │
│                                                                 │
│  2. in_progress（执行中）                                        │
│  ─────────────────────────                                      │
│  #{                                                             │
│    status => in_progress,                                       │
│    attempts => 1,                                               │
│    input_messages => [{state, S1}],                            │
│    input_vertex => OriginalVertex                               │
│  }                                                              │
│                                                                 │
│  3. completed（已完成）                                          │
│  ─────────────────────────                                      │
│  #{                                                             │
│    status => completed,                                         │
│    attempts => 1,                                               │
│    vertex => NewVertex,           %% 执行后的顶点                │
│    outbox => [                    %% 发出的消息                  │
│      {next_node, {state, NewState}}                            │
│    ],                                                           │
│    input_messages => [{state, S1}],                            │
│    input_vertex => OriginalVertex                               │
│  }                                                              │
│                                                                 │
│  4. failed（已失败）                                             │
│  ─────────────────────                                          │
│  #{                                                             │
│    status => failed,                                            │
│    attempts => 3,                 %% 已重试 3 次                 │
│    error => {timeout, "LLM call timeout"},                     │
│    stacktrace => [...],                                         │
│    input_messages => [{state, S1}],                            │
│    input_vertex => OriginalVertex                               │
│  }                                                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 完整的 superstep_state 示例

```erlang
%% 超级步 N 执行过程中的 superstep_state
SuperstepState = #{
    %% 已完成的顶点
    llm_call => #{
        status => completed,
        attempts => 1,
        vertex => #{
            id => llm_call,
            value => #{node => ..., result => {ok, NewState}},
            edges => [...],
            halted => true
        },
        outbox => [
            {execute_tools, {state, NewState}}
        ],
        input_messages => [{state, InputState}],
        input_vertex => OriginalLLMVertex
    },

    %% 失败的顶点
    execute_tools => #{
        status => failed,
        attempts => 2,
        error => {tool_error, "API rate limit"},
        stacktrace => [...],
        input_messages => [{state, ToolInputState}],
        input_vertex => OriginalToolVertex
    },

    %% 待执行的顶点
    another_node => #{
        status => pending,
        attempts => 0,
        input_messages => [{state, S}],
        input_vertex => AnotherVertex
    }
}.
```

---

## 生命周期

```
┌─────────────────────────────────────────────────────────────────┐
│              superstep_state 生命周期                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  超步 N 开始                                                     │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────┐                   │
│  │  1. 初始化 superstep_state               │                   │
│  │                                          │                   │
│  │  根据 ActiveVertices 和 Inbox 初始化:    │                   │
│  │  superstep_state = #{                   │                   │
│  │    v1 => #{status => pending, ...},     │                   │
│  │    v2 => #{status => pending, ...}      │                   │
│  │  }                                      │                   │
│  └─────────────────────────────────────────┘                   │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────┐                   │
│  │  2. 执行顶点，更新状态                    │                   │
│  │                                          │                   │
│  │  执行 v1:                                │                   │
│  │  v1 => #{status => in_progress, ...}    │                   │
│  │  执行成功:                               │                   │
│  │  v1 => #{status => completed, ...}      │                   │
│  │                                          │                   │
│  │  执行 v2:                                │                   │
│  │  v2 => #{status => in_progress, ...}    │                   │
│  │  执行失败:                               │                   │
│  │  v2 => #{status => failed, ...}         │                   │
│  └─────────────────────────────────────────┘                   │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────┐                   │
│  │  3. 重试失败顶点                          │                   │
│  │                                          │                   │
│  │  检查 attempts < max_retries:            │                   │
│  │  v2.attempts = 1 < 3 → 重试             │                   │
│  │                                          │                   │
│  │  重试成功:                               │                   │
│  │  v2 => #{status => completed, ...}      │                   │
│  └─────────────────────────────────────────┘                   │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────┐                   │
│  │  4. 汇总结果                              │                   │
│  │                                          │                   │
│  │  从 superstep_state 提取:                │                   │
│  │  - 更新后的 vertices                     │                   │
│  │  - 合并的 outbox                         │                   │
│  │  - 失败顶点列表（如有）                   │                   │
│  └─────────────────────────────────────────┘                   │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────┐                   │
│  │  5. 上报 Master，清空 superstep_state    │                   │
│  │                                          │                   │
│  │  superstep_state = #{}                  │                   │
│  └─────────────────────────────────────────┘                   │
│       │                                                         │
│       ▼                                                         │
│  超步 N+1 开始                                                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 字段说明

| 字段 | 类型 | 必须 | 用途 |
|------|------|------|------|
| `status` | `vertex_status()` | ✅ | 判断顶点执行状态，决定是否需要重试 |
| `vertex` | `vertex()` | completed 时 | 成功时的新顶点状态，用于更新 vertices |
| `outbox` | `[{vertex_id(), term()}]` | completed 时 | 成功时发出的消息，用于路由到其他顶点 |
| `error` | `term()` | failed 时 | 失败原因，用于日志和上报 |
| `stacktrace` | `list()` | 可选 | 错误堆栈，用于调试 |
| `attempts` | `non_neg_integer()` | ✅ | 重试次数，避免无限重试 |
| `input_messages` | `[term()]` | ✅ | 重试时使用相同的输入消息 |
| `input_vertex` | `vertex()` | ✅ | 重试时使用相同的顶点初始状态 |

---

## 状态转换图

```
┌─────────────────────────────────────────────────────────────────┐
│                    顶点状态转换                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│                    ┌─────────┐                                  │
│                    │ pending │                                  │
│                    └────┬────┘                                  │
│                         │                                       │
│                         │ 开始执行                               │
│                         ▼                                       │
│                  ┌─────────────┐                                │
│                  │ in_progress │                                │
│                  └──────┬──────┘                                │
│                         │                                       │
│            ┌────────────┴────────────┐                          │
│            │                         │                          │
│            │ 成功                    │ 失败                      │
│            ▼                         ▼                          │
│     ┌───────────┐             ┌──────────┐                      │
│     │ completed │             │  failed  │                      │
│     └───────────┘             └────┬─────┘                      │
│                                    │                            │
│                         ┌─────────┴─────────┐                   │
│                         │                   │                   │
│                         │ attempts <        │ attempts >=       │
│                         │ max_retries       │ max_retries       │
│                         ▼                   ▼                   │
│                  ┌─────────────┐     ┌───────────┐              │
│                  │ in_progress │     │  failed   │              │
│                  │  (重试)      │     │ (最终)    │              │
│                  └─────────────┘     └───────────┘              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Worker 状态扩展

```erlang
%% pregel_worker.erl

-record(state, {
    worker_id     :: non_neg_integer(),
    master        :: pid(),
    vertices      :: #{vertex_id() => vertex()},
    inbox         :: #{vertex_id() => [term()]},
    compute_fn    :: fun(),
    combiner      :: pregel_combiner:spec() | undefined,
    superstep     :: non_neg_integer(),
    num_workers   :: pos_integer(),
    num_vertices  :: non_neg_integer(),
    worker_pids   :: #{non_neg_integer() => pid()},

    %% 新增字段
    superstep_state :: superstep_state(),      %% 超步执行状态
    max_retries     :: non_neg_integer()       %% 最大重试次数（默认 3）
}).
```

---

## 辅助函数

```erlang
%% 初始化 superstep_state
-spec init_superstep_state(#{vertex_id() => vertex()},
                           #{vertex_id() => [term()]}) -> superstep_state().
init_superstep_state(ActiveVertices, Inbox) ->
    maps:fold(
        fun(VertexId, Vertex, Acc) ->
            Messages = maps:get(VertexId, Inbox, []),
            Acc#{VertexId => #{
                status => pending,
                attempts => 0,
                input_messages => Messages,
                input_vertex => Vertex
            }}
        end,
        #{},
        ActiveVertices
    ).

%% 获取失败的顶点
-spec get_failed_vertices(superstep_state()) -> [vertex_id()].
get_failed_vertices(SuperstepState) ->
    [Id || {Id, #{status := failed}} <- maps:to_list(SuperstepState)].

%% 获取可重试的顶点
-spec get_retryable_vertices(superstep_state(), non_neg_integer()) -> [vertex_id()].
get_retryable_vertices(SuperstepState, MaxRetries) ->
    [Id || {Id, #{status := failed, attempts := A}} <- maps:to_list(SuperstepState),
           A < MaxRetries].

%% 汇总已完成顶点的结果
-spec collect_completed_results(superstep_state()) ->
    {#{vertex_id() => vertex()}, [{vertex_id(), term()}]}.
collect_completed_results(SuperstepState) ->
    maps:fold(
        fun(Id, #{status := completed, vertex := V, outbox := O}, {Vertices, Outbox}) ->
                {Vertices#{Id => V}, Outbox ++ O};
           (_, _, Acc) ->
                Acc
        end,
        {#{}, []},
        SuperstepState
    ).
```

---

## 总结

| 问题 | 答案 |
|------|------|
| **存储什么？** | 每个活跃顶点的执行状态、结果、输入 |
| **数据结构？** | `#{vertex_id() => vertex_result()}` |
| **生命周期？** | 超步开始时初始化，超步结束时清空 |
| **关键字段？** | status, vertex, outbox, error, attempts, input_* |
| **存储位置？** | `pregel_worker.erl` 的 `#state{}` 记录中 |

---

## 相关文档

- `pregel_superstep_state_location.md` - superstep_state 位置分析
- `pregel_pending_writes_placement.md` - pending_writes 放置分析
- `pregel_pending_writes_analysis.md` - pending_writes 需求分析
- `pregel_error_reporting.md` - 错误上报机制
