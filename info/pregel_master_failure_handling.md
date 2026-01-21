# pregel_master 和 pregel_barrier 失败处理

## 概述

当 pregel_worker 上报失败信息后，需要 pregel_master 和 pregel_barrier 配合处理这些信息。

---

## 3.1 pregel_master 处理失败信息

### 当前状态

代码位置：`pregel_master.erl:251-276`

```erlang
complete_superstep(#state{barrier = Barrier, ...} = State) ->
    %% 1. 汇总结果（只有 active_count 和 message_count）
    Results = pregel_barrier:get_results(Barrier),
    {TotalActive, TotalMessages} = pregel_barrier:aggregate_results(Results),

    %% 2. 检查终止条件（不检查失败）
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
    MaxReached = Superstep >= MaxSupersteps - 1,

    %% 3. 决定下一步（忽略失败信息）
    case {Halted, MaxReached} of
        {true, _} -> finish_execution(completed, State);
        {_, true} -> finish_execution(max_supersteps, State);
        {false, false} -> start_next_superstep(State)
    end.
```

**问题**：即使有顶点失败，Master 也会继续执行下一超步。

### 需要做的修改

```erlang
%% 修改后的 complete_superstep
complete_superstep(#state{
    barrier = Barrier,
    superstep = Superstep,
    max_supersteps = MaxSupersteps,
    on_vertex_error = ErrorStrategy  %% 新增：错误策略
} = State) ->
    Results = pregel_barrier:get_results(Barrier),

    %% 汇总结果（包含失败信息）
    {TotalActive, TotalMessages, TotalFailed, AllFailedVertices} =
        pregel_barrier:aggregate_results(Results),

    %% 检查是否有失败
    case TotalFailed > 0 of
        true ->
            %% 有顶点失败，根据策略处理
            handle_vertex_failures(AllFailedVertices, ErrorStrategy, State);
        false ->
            %% 无失败，继续原有逻辑
            Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
            MaxReached = Superstep >= MaxSupersteps - 1,
            case {Halted, MaxReached} of
                {true, _} -> finish_execution(completed, State);
                {_, true} -> finish_execution(max_supersteps, State);
                {false, false} -> start_next_superstep(State)
            end
    end.

%% 新增：处理顶点失败
-spec handle_vertex_failures([{vertex_id(), term()}], error_strategy(), #state{}) -> #state{}.
handle_vertex_failures(FailedVertices, Strategy, State) ->
    case Strategy of
        fail_fast ->
            %% 立即终止，返回错误
            finish_execution({error, {vertex_failures, FailedVertices}}, State);

        continue ->
            %% 忽略失败，继续执行
            %% 记录日志
            logger:warning("Vertices failed but continuing: ~p", [FailedVertices]),
            continue_after_failures(State);

        {callback, Fun} ->
            %% 回调上层决定
            case Fun(FailedVertices) of
                continue -> continue_after_failures(State);
                stop -> finish_execution({error, {vertex_failures, FailedVertices}}, State)
            end
    end.

continue_after_failures(#state{superstep = Superstep, max_supersteps = MaxSupersteps} = State) ->
    %% 继续检查其他终止条件
    Results = pregel_barrier:get_results(State#state.barrier),
    {TotalActive, TotalMessages, _, _} = pregel_barrier:aggregate_results(Results),
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
    MaxReached = Superstep >= MaxSupersteps - 1,
    case {Halted, MaxReached} of
        {true, _} -> finish_execution(completed, State);
        {_, true} -> finish_execution(max_supersteps, State);
        {false, false} -> start_next_superstep(State)
    end.
```

### 可选：支持错误策略配置

```erlang
%% 类型定义
-type error_strategy() :: fail_fast
                        | continue
                        | {callback, fun(([{vertex_id(), term()}]) -> continue | stop)}.

%% 在 opts 中添加
-type opts() :: #{
    ...
    on_vertex_error => error_strategy()  %% 默认 fail_fast
}.

%% init 中初始化
init({Graph, ComputeFn, Opts}) ->
    State = #state{
        ...
        on_vertex_error = maps:get(on_vertex_error, Opts, fail_fast)
    },
    {ok, State}.
```

### 修改 #state 记录

```erlang
-record(state, {
    graph            :: graph(),
    compute_fn       :: fun(),
    combiner         :: pregel_combiner:spec() | undefined,
    max_supersteps   :: pos_integer(),
    num_workers      :: pos_integer(),
    workers          :: #{non_neg_integer() => pid()},
    superstep        :: non_neg_integer(),
    barrier          :: pregel_barrier:t(),
    caller           :: gen_server:from() | undefined,
    on_superstep     :: fun() | undefined,
    pending_messages :: map(),
    on_vertex_error  :: error_strategy()  %% 新增
}).
```

---

## 3.2 pregel_barrier 汇总失败信息

### 当前状态

代码位置：`pregel_barrier.erl:66-78`

```erlang
%% 只汇总 active_count 和 message_count
-spec aggregate_results([map()]) -> {non_neg_integer(), non_neg_integer()}.
aggregate_results(Results) ->
    lists:foldl(
        fun(Result, {AccA, AccM}) ->
            Active = maps:get(active_count, Result, 0),
            Messages = maps:get(message_count, Result, 0),
            {AccA + Active, AccM + Messages}
        end,
        {0, 0},
        Results
    ).
```

### 需要做的修改

```erlang
%% 修改后：汇总包含失败信息
-spec aggregate_results([map()]) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer(), [{term(), term()}]}.
aggregate_results(Results) ->
    lists:foldl(
        fun(Result, {AccA, AccM, AccF, AccFV}) ->
            Active = maps:get(active_count, Result, 0),
            Messages = maps:get(message_count, Result, 0),
            Failed = maps:get(failed_count, Result, 0),
            FailedVertices = maps:get(failed_vertices, Result, []),
            {AccA + Active, AccM + Messages, AccF + Failed, FailedVertices ++ AccFV}
        end,
        {0, 0, 0, []},
        Results
    ).
```

### 返回值变化

| 字段 | 当前 | 修改后 |
|------|------|--------|
| TotalActive | ✅ | ✅ |
| TotalMessages | ✅ | ✅ |
| TotalFailed | ❌ | ✅ 新增 |
| AllFailedVertices | ❌ | ✅ 新增 |

---

## 流程图

```
┌─────────────────────────────────────────────────────────────────┐
│                    失败信息处理流程                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Worker 1          Worker 2          Worker 3                   │
│  ────────          ────────          ────────                   │
│  Result = #{       Result = #{       Result = #{                │
│    active: 2,        active: 1,        active: 0,               │
│    message: 3,       message: 2,       message: 0,              │
│    failed: 0,        failed: 1,        failed: 2,               │
│    failed_vertices:  failed_vertices:  failed_vertices:         │
│      []               [{v3, err1}]      [{v5, err2},            │
│  }                  }                    {v6, err3}]            │
│     │                  │                   │                    │
│     └──────────────────┼───────────────────┘                    │
│                        ▼                                        │
│              pregel_barrier:record_done                         │
│                        │                                        │
│                        ▼                                        │
│              pregel_barrier:aggregate_results                   │
│                        │                                        │
│                        ▼                                        │
│              {TotalActive: 3,                                   │
│               TotalMessages: 5,                                 │
│               TotalFailed: 3,                                   │
│               AllFailed: [{v3,err1},{v5,err2},{v6,err3}]}       │
│                        │                                        │
│                        ▼                                        │
│              pregel_master:complete_superstep                   │
│                        │                                        │
│                 TotalFailed > 0?                                │
│                        │                                        │
│              ┌─────────┴─────────┐                              │
│              │                   │                              │
│            是                   否                              │
│              │                   │                              │
│              ▼                   ▼                              │
│     handle_vertex_failures   继续原有逻辑                        │
│              │                                                  │
│     ┌────────┼────────┐                                         │
│     │        │        │                                         │
│  fail_fast continue callback                                    │
│     │        │        │                                         │
│     ▼        ▼        ▼                                         │
│  终止执行  继续下一  回调决定                                     │
│  返回错误  超步                                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 错误策略说明

| 策略 | 行为 | 适用场景 |
|------|------|----------|
| **fail_fast** | 有任何顶点失败立即终止 | 要求严格正确性 |
| **continue** | 忽略失败继续执行 | 允许部分失败 |
| **callback** | 回调上层决定 | 需要动态决策 |

---

## 任务清单

### pregel_barrier 修改

- [ ] 修改 `aggregate_results/1` 返回值类型
- [ ] 添加 failed_count 和 failed_vertices 汇总

### pregel_master 修改

- [ ] 添加 `on_vertex_error` 字段到 #state
- [ ] 添加 `error_strategy()` 类型定义
- [ ] 修改 `init/1` 初始化错误策略
- [ ] 修改 `complete_superstep/1` 检查失败
- [ ] 实现 `handle_vertex_failures/3`
- [ ] 可选：修改 `finish_execution/2` 支持错误状态

---

## 向后兼容

```erlang
%% pregel_barrier:aggregate_results 需要向后兼容
%% 方案 1：新函数
aggregate_results_with_failures(Results) -> {A, M, F, FV}.

%% 方案 2：检查字段存在性（当前方案）
Failed = maps:get(failed_count, Result, 0),  %% 默认 0
FailedVertices = maps:get(failed_vertices, Result, []),  %% 默认 []
```

---

## 如果不做这两个任务

| 影响 | 说明 |
|------|------|
| Worker 上报失败 | ✅ 会上报 |
| Master 感知失败 | ❌ 不感知，忽略 |
| 执行继续 | ✅ 会继续下一超步 |
| 最终结果 | ⚠️ 可能不完整 |

---

## 建议

```
┌─────────────────────────────────────────────────────────────────┐
│                      实现建议                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  P0 阶段（必须）:                                                │
│  • pregel_worker 修改（上报失败信息）                            │
│  • graph_compute 修改（异常处理）                                │
│                                                                 │
│  P1 阶段（可选）:                                                │
│  • pregel_barrier 修改（汇总失败信息）                           │
│  • pregel_master 修改（处理失败信息）                            │
│                                                                 │
│  先完成 P0，验证 Worker 正确上报失败                             │
│  再根据需要决定是否实现 P1                                       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 相关文档

- `pregel_worker_error_handling_design.md` - Worker 错误处理设计
- `pregel_error_reporting.md` - 错误上报机制分析
- `TASK.md` - 任务分解
