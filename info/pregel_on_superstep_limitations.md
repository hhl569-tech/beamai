# on_superstep 回调的局限性分析

## 问题

1. 在 `on_superstep` 回调中是否可以判断执行出错了？
2. 保存这些信息是否可以恢复 Pregel 的执行？

---

## on_superstep 回调获得的信息

### 回调签名

代码位置：`pregel_master.erl:236-239`

```erlang
on_superstep => fun((non_neg_integer(), graph()) -> ok)

%% 调用时
maybe_call_superstep_callback(Fun, Superstep, Graph) -> Fun(Superstep, Graph).
```

### 参数说明

| 参数 | 内容 | 说明 |
|------|------|------|
| `Superstep` | 当前超步编号 | ✅ 有用 |
| `Graph` | **原始图** | ❌ 不是当前状态！ |

---

## 问题 1：是否可以判断执行出错了？

### 答案：❌ 不能

```
┌─────────────────────────────────────────────────────────────────┐
│           on_superstep 无法判断执行错误                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  原因 1: 回调在超步开始前调用                                     │
│  ────────────────────────────                                   │
│  on_superstep(N, Graph)  ← 超步 N 开始前调用                    │
│         ↓                                                       │
│  Workers 执行超步 N       ← 这时才可能出错                       │
│         ↓                                                       │
│  on_superstep(N+1, Graph) ← 超步 N+1 开始前调用                 │
│                                                                 │
│  回调时，当前超步还没执行！                                       │
│                                                                 │
│  原因 2: Worker 不上报错误信息                                   │
│  ────────────────────────────                                   │
│  Worker 上报: #{active_count => N, message_count => M}         │
│  缺失: ❌ 没有 failed_count 或 failed_vertices                  │
│                                                                 │
│  原因 3: Graph 是原始图                                          │
│  ────────────────────────────                                   │
│  Master 的 #state.graph 是初始化时的原始图                       │
│  顶点的执行结果（包括错误）存储在 Worker 中                       │
│  on_superstep 收到的 Graph 不包含执行结果                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 回调时序图

```
┌─────────────────────────────────────────────────────────────────┐
│                    on_superstep 调用时序                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  时间 ──────────────────────────────────────────────────────→   │
│                                                                 │
│  超步 0:                                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │ on_superstep(0, OriginalGraph) ← 调用回调                 │  │
│  │         ↓                                                 │  │
│  │ Workers 执行超步 0                                         │  │
│  │         ↓                                                 │  │
│  │ Barrier 等待完成                                           │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              ↓                                  │
│  超步 1:                                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │ on_superstep(1, OriginalGraph) ← 调用回调                 │  │
│  │         ↓                      ↑                          │  │
│  │         ↓                      │                          │  │
│  │ 此时超步 0 已完成，但回调      │                          │  │
│  │ 收不到超步 0 的执行结果！      │ Graph 仍是原始图         │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 问题 2：保存这些信息是否可以恢复 Pregel 执行？

### 答案：❌ 不能

```
┌─────────────────────────────────────────────────────────────────┐
│           on_superstep 信息不足以恢复执行                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  on_superstep 提供的:           恢复 Pregel 需要的:             │
│  ─────────────────────          ────────────────────            │
│  ✅ Superstep 编号              ✅ Superstep 编号               │
│  ❌ 原始 Graph                  ✅ 所有顶点当前状态              │
│                                 ✅ 各顶点的 inbox               │
│                                 ✅ pending_messages             │
│                                 ✅ 顶点的 halted 状态           │
│                                                                 │
│  关键问题:                                                       │
│  ─────────                                                      │
│  Master 的 Graph 是原始图，从不更新！                            │
│  顶点的当前状态存储在各个 Worker 进程中                          │
│  on_superstep 无法访问 Worker 中的数据                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Master 的 Graph 从不更新

代码位置：`pregel_master.erl:100-114`

```erlang
init({Graph, ComputeFn, Opts}) ->
    State = #state{
        graph = Graph,  %% 设置一次，从不更新！
        ...
    },
    {ok, State}.
```

代码位置：`pregel_master.erl:328-343`

```erlang
%% 只在执行结束时才从 Workers 收集最终状态
finish_execution(Status, #state{
    workers = Workers,
    graph = OriginalGraph  %% 仍是原始图
} = State) ->
    %% 这时才收集
    FinalGraph = collect_final_graph(Workers, OriginalGraph),
    ...
```

---

## Pregel 运行时数据分布

```
┌─────────────────────────────────────────────────────────────────┐
│                    Pregel 运行时数据分布                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Master 进程:                                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  #state{                                                 │   │
│  │    graph = OriginalGraph,    %% 原始图，从不更新！        │   │
│  │    superstep = N,            %% 当前超步                  │   │
│  │    workers = #{...},         %% Worker pid 映射          │   │
│  │    barrier = ...,            %% 同步屏障                  │   │
│  │    pending_messages = #{...} %% 待路由消息                │   │
│  │  }                                                       │   │
│  │                                                          │   │
│  │  on_superstep 能访问: superstep, graph (原始)            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│              ┌───────────────┼───────────────┐                  │
│              ▼               ▼               ▼                  │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐       │
│  │   Worker 0    │  │   Worker 1    │  │   Worker 2    │       │
│  │               │  │               │  │               │       │
│  │ vertices = #{ │  │ vertices = #{ │  │ vertices = #{ │       │
│  │   v1 => V1',  │  │   v3 => V3',  │  │   v5 => V5',  │       │
│  │   v2 => V2'   │  │   v4 => V4'   │  │   v6 => V6'   │       │
│  │ }             │  │ }             │  │ }             │       │
│  │               │  │               │  │               │       │
│  │ inbox = #{    │  │ inbox = #{    │  │ inbox = #{    │       │
│  │   v1 => [...] │  │   v3 => [...] │  │   v5 => [...] │       │
│  │ }             │  │ }             │  │ }             │       │
│  └───────────────┘  └───────────────┘  └───────────────┘       │
│                                                                 │
│  ⚠️ 顶点当前状态和 inbox 都在 Worker 中，Master 不持有！         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 如果要支持错误检测和恢复

### 方案 1：增强 on_superstep 回调

```erlang
%% 修改回调签名，提供更多信息
on_superstep => fun((superstep_info()) -> ok)

-type superstep_info() :: #{
    superstep := non_neg_integer(),

    %% 新增：从 Workers 收集的数据
    vertices := #{vertex_id() => vertex()},
    inbox := #{vertex_id() => [term()]},

    %% 新增：上一超步的结果
    previous_results := #{
        active_count := non_neg_integer(),
        message_count := non_neg_integer(),
        failed_vertices := [{vertex_id(), term()}]  %% 错误信息
    }
}.
```

**代价：**
- 每个超步开始前需要从所有 Workers 收集数据
- 增加延迟和网络开销
- 破坏 Pregel 的分布式特性

### 方案 2：添加 on_superstep_complete 回调

```erlang
%% 在超步完成后调用，而不是开始前
on_superstep_complete => fun((superstep_complete_info()) -> ok)

-type superstep_complete_info() :: #{
    superstep := non_neg_integer(),

    %% 从 barrier 结果中获取
    results := #{
        active_count := non_neg_integer(),
        message_count := non_neg_integer(),
        failed_count := non_neg_integer(),
        failed_vertices := [{vertex_id(), term()}]
    },

    %% 可选：从 Workers 收集顶点状态用于 checkpoint
    vertices => #{vertex_id() => vertex()},
    inbox => #{vertex_id() => [term()]}
}.
```

**优点：**
- 在正确的时机调用（超步完成后）
- 可以获取执行结果
- 可选收集顶点状态（用于 checkpoint）

### 方案 3：Pregel 层 Checkpoint

```erlang
%% 在 opts 中配置 checkpoint
Opts = #{
    checkpoint_interval => 5,  %% 每 5 个超步保存一次
    checkpoint_fn => fun(CheckpointData) ->
        %% 保存到外部存储
        save_to_storage(CheckpointData)
    end
}.

-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    inbox := #{vertex_id() => [term()]},
    pending_messages := [{vertex_id(), term()}]
}.
```

---

## 总结

| 问题 | 答案 | 原因 |
|------|------|------|
| **能判断执行出错？** | ❌ 不能 | 回调在执行前调用，Worker 不上报错误 |
| **能恢复 Pregel 执行？** | ❌ 不能 | Graph 是原始图，顶点状态在 Workers 中 |

### on_superstep 提供的 vs 恢复所需的

| 恢复所需 | on_superstep 提供？ | 说明 |
|----------|---------------------|------|
| Superstep 编号 | ✅ 有 | |
| 顶点当前状态 | ❌ 无 | 在 Workers 中 |
| 顶点 inbox | ❌ 无 | 在 Workers 中 |
| pending_messages | ❌ 无 | 在 Master 中但不传给回调 |
| 错误信息 | ❌ 无 | Worker 不上报 |

### 建议

如果需要支持错误检测和恢复，建议：

1. **添加 on_superstep_complete 回调** - 在超步完成后调用
2. **修改 Worker 上报内容** - 增加 failed_count 和 failed_vertices
3. **实现 Pregel 层 Checkpoint** - 定期从 Workers 收集状态并持久化

---

## 相关文档

- `pregel_graph_superstep_awareness.md` - Graph 层超级步感知
- `pregel_error_reporting.md` - 错误上报机制
- `pregel_checkpoint_layer_analysis.md` - Checkpoint 层分析
- `pregel_pending_writes_placement.md` - pending_writes 放置分析
