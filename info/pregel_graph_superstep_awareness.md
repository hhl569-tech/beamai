# Graph 层对 Pregel 超级步的感知

## 问题

Graph 层是否知道 Pregel 每个超级步完成？

## 答案

**Graph 层当前不知道 Pregel 每个超级步完成。**

---

## 分析

### Pregel 层提供的机制

代码位置：`pregel_master.erl:36`

```erlang
-type opts() :: #{
    combiner => pregel_combiner:spec(),
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    on_superstep => fun((non_neg_integer(), graph()) -> ok)  %% 超步回调
}.
```

### 回调触发时机

代码位置：`pregel_master.erl:220-231`

```erlang
broadcast_start_superstep(#state{
    workers = Workers,
    superstep = Superstep,
    on_superstep = OnSuperstep,
    graph = Graph
}) ->
    %% 调用超步回调（如果有）
    maybe_call_superstep_callback(OnSuperstep, Superstep, Graph),  %% ← 这里调用
    %% 然后广播给 Workers
    maps:foreach(
        fun(_Id, Pid) -> pregel_worker:start_superstep(Pid, Superstep) end,
        Workers
    ).

maybe_call_superstep_callback(undefined, _, _) -> ok;
maybe_call_superstep_callback(Fun, Superstep, Graph) -> Fun(Superstep, Graph).
```

**注意：回调在超级步开始时触发，不是完成时。**

### Graph 层的调用

代码位置：`graph_runner.erl:86-93`

```erlang
PregelOpts = #{
    max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
    num_workers => maps:get(workers, Options, 1)
    %% ⚠️ 没有传递 on_superstep 回调！
},

ComputeFn = graph_compute:compute_fn(),
Result = pregel:run(PregelGraphWithState, ComputeFn, PregelOpts),
```

---

## 总结

```
┌─────────────────────────────────────────────────────────────────┐
│              Graph 层与 Pregel 超级步感知                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Pregel 层:                                                     │
│  ✅ 提供 on_superstep 回调机制                                  │
│  ✅ 在每个超级步开始时调用                                       │
│  ✅ 传入当前超步编号和图状态                                     │
│                                                                 │
│  Graph 层:                                                      │
│  ❌ 没有使用 on_superstep 回调                                  │
│  ❌ 不知道每个超级步何时完成                                     │
│  ❌ 只等待最终结果                                               │
│                                                                 │
│  当前流程:                                                       │
│  Graph 调用 pregel:run() → 阻塞等待 → 收到最终结果              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

| 问题 | 答案 |
|------|------|
| **Pregel 是否支持超级步回调？** | ✅ 支持 `on_superstep` |
| **回调触发时机？** | 每个超级步**开始**时（非完成时） |
| **Graph 层是否使用？** | ❌ 没有使用 |
| **Graph 层能否感知超级步？** | ❌ 当前不能 |

---

## 回调时机说明

```
┌─────────────────────────────────────────────────────────────────┐
│                    on_superstep 回调时机                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  超级步 0:                                                       │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │ on_superstep(0, Graph) ← 回调                            │  │
│  │         ↓                                                 │  │
│  │ broadcast_start_superstep → Workers 开始计算             │  │
│  │         ↓                                                 │  │
│  │ Workers 计算中...                                         │  │
│  │         ↓                                                 │  │
│  │ Barrier 等待所有 Workers 完成                             │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              ↓                                  │
│  超级步 1:                                                       │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │ on_superstep(1, Graph) ← 回调                            │  │
│  │         ↓                                                 │  │
│  │ ...                                                       │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                 │
│  问题：回调发生在超级步开始时，此时上一超级步的结果已汇总完成     │
│  所以实际上可以用于感知"上一超级步完成"                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 如果需要 Graph 层感知超级步

### 方案 1：使用现有 on_superstep 回调

```erlang
%% 修改 graph_runner.erl

run(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph, config := Config} = Graph,
    PregelGraphWithState = graph_compute:inject_initial_state(PregelGraph, InitialState),

    MaxIterations = maps:get(max_iterations, Config, 100),

    %% 定义超级步回调
    OnSuperstep = fun(Superstep, PregelGraph) ->
        %% 可以在这里：
        %% 1. 记录日志
        logger:info("Superstep ~p started", [Superstep]),
        %% 2. 保存 Checkpoint（如果需要）
        %% 3. 触发事件通知
        %% 4. 更新进度指示
        ok
    end,

    PregelOpts = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        num_workers => maps:get(workers, Options, 1),
        on_superstep => OnSuperstep  %% ← 添加回调
    },

    ComputeFn = graph_compute:compute_fn(),
    Result = pregel:run(PregelGraphWithState, ComputeFn, PregelOpts),
    ...
```

### 方案 2：添加超级步完成回调

如果需要在超级步完成时（而非开始时）触发回调，需要修改 Pregel 层：

```erlang
%% pregel_master.erl

-type opts() :: #{
    ...
    on_superstep => fun((non_neg_integer(), graph()) -> ok),
    on_superstep_complete => fun((non_neg_integer(), superstep_result()) -> ok)  %% 新增
}.

%% 在 complete_superstep 中调用
complete_superstep(#state{
    superstep = Superstep,
    on_superstep_complete = OnComplete,
    ...
} = State) ->
    Results = pregel_barrier:get_results(Barrier),

    %% 调用完成回调
    maybe_call_complete_callback(OnComplete, Superstep, Results),

    %% 继续原有逻辑
    ...
```

### 方案 3：支持 Graph 层传入回调

```erlang
%% graph_runner.erl 添加回调选项

-type run_options() :: #{
    workers => pos_integer(),
    trace => boolean(),
    max_iterations => pos_integer(),
    timeout => pos_integer(),
    on_superstep => fun((superstep_info()) -> ok)  %% 新增
}.

-type superstep_info() :: #{
    superstep := non_neg_integer(),
    active_vertices := non_neg_integer(),
    message_count := non_neg_integer()
}.
```

---

## 使用场景

| 场景 | 需要感知超级步？ | 说明 |
|------|------------------|------|
| **简单执行** | ❌ | 只需最终结果 |
| **进度显示** | ✅ | 向用户展示执行进度 |
| **超级步 Checkpoint** | ✅ | 每 N 个超级步保存状态 |
| **调试/日志** | ✅ | 记录每步执行情况 |
| **超时控制** | ✅ | 单个超级步超时检测 |
| **动态调整** | ✅ | 根据执行情况调整策略 |

---

## 相关文档

- `pregel_superstep_completion.md` - 超级步完成判断机制
- `pregel_checkpoint_layer_analysis.md` - Checkpoint 层分析
- `pregel_error_reporting.md` - 错误上报机制
