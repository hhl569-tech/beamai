# Pregel 单顶点重启支持设计方案

## 概述

当前 Pregel 实现中，如果超级步 S 中某个顶点计算失败，整个超步会失败，已完成顶点的结果也会丢失。本文档设计一个支持单顶点失败隔离和重试的方案。

## 当前实现的问题

代码位置：`pregel_worker.erl:217-230`

```erlang
compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    maps:fold(
        fun(Id, Vertex, {VAcc, OAcc}) ->
            %% ⚠️ 没有 try-catch，一个顶点失败则整个 fold 失败
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),
            #{vertex := NewVertex, outbox := Out} = ComputeFn(Context),
            {VAcc#{Id => NewVertex}, Out ++ OAcc}
        end,
        {AllVertices, []},
        ActiveVertices
    ).
```

### 问题

| 问题 | 说明 |
|------|------|
| 无异常隔离 | 一个顶点失败导致整个超步失败 |
| 结果丢失 | 已完成顶点的结果随 fold 失败而丢失 |
| 无法重试 | 只能重跑整个超步 |
| 无状态记录 | 不知道哪些顶点成功、哪些失败 |

---

## 设计方案

### 1. 定义顶点执行状态

```erlang
%% 顶点执行状态
-type vertex_status() :: pending      %% 待执行
                       | completed    %% 已完成
                       | failed.      %% 已失败

%% 顶点执行结果
-type vertex_result() :: #{
    status := vertex_status(),
    vertex => vertex(),                    %% 新顶点状态（completed 时）
    outbox => [{vertex_id(), term()}],     %% 发出的消息（completed 时）
    error => term(),                       %% 错误原因（failed 时）
    attempts => non_neg_integer()          %% 尝试次数
}.

%% 超步执行状态
-type superstep_state() :: #{
    vertex_id() => vertex_result()
}.
```

### 2. 单顶点执行（带异常捕获）

```erlang
%% @doc 执行单个顶点（带异常捕获）
-spec compute_single_vertex(
    vertex_id(), vertex(), map(), fun(), non_neg_integer(), non_neg_integer()
) -> vertex_result().

compute_single_vertex(Id, Vertex, Inbox, ComputeFn, Superstep, NumVertices) ->
    Messages = maps:get(Id, Inbox, []),
    ActiveVertex = activate_if_has_messages(Vertex, Messages),
    Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),

    try
        #{vertex := NewVertex, outbox := Out} = ComputeFn(Context),
        #{
            status => completed,
            vertex => NewVertex,
            outbox => Out,
            attempts => 1
        }
    catch
        Class:Reason:Stacktrace ->
            logger:warning("顶点 ~p 执行失败: ~p:~p~n~p",
                          [Id, Class, Reason, Stacktrace]),
            #{
                status => failed,
                error => {Class, Reason, Stacktrace},
                attempts => 1
            }
    end.
```

### 3. 改造 compute_vertices

```erlang
%% @doc 执行所有顶点计算（支持单顶点失败隔离）
-spec compute_vertices_safe(
    ActiveVertices :: #{vertex_id() => vertex()},
    AllVertices :: #{vertex_id() => vertex()},
    Inbox :: #{vertex_id() => [term()]},
    ComputeFn :: fun((context()) -> context()),
    Superstep :: non_neg_integer(),
    NumVertices :: non_neg_integer()
) -> {ok, #{vertex_id() => vertex()}, [{vertex_id(), term()}], superstep_state()} |
     {partial, #{vertex_id() => vertex()}, [{vertex_id(), term()}], superstep_state()}.

compute_vertices_safe(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    %% 逐个执行，捕获异常
    Results = maps:map(
        fun(Id, Vertex) ->
            compute_single_vertex(Id, Vertex, Inbox, ComputeFn, Superstep, NumVertices)
        end,
        ActiveVertices
    ),

    %% 汇总结果
    aggregate_results(Results, AllVertices).

%% @doc 汇总执行结果
-spec aggregate_results(#{vertex_id() => vertex_result()}, #{vertex_id() => vertex()}) ->
    {ok | partial, #{vertex_id() => vertex()}, [{vertex_id(), term()}], superstep_state()}.

aggregate_results(Results, AllVertices) ->
    {NewVertices, Outbox, HasFailure} = maps:fold(
        fun(Id, Result, {VAcc, OAcc, Failed}) ->
            case Result of
                #{status := completed, vertex := V, outbox := Out} ->
                    {VAcc#{Id => V}, Out ++ OAcc, Failed};
                #{status := failed} ->
                    %% 失败顶点保持原状态
                    {VAcc, OAcc, true}
            end
        end,
        {AllVertices, [], false},
        Results
    ),

    Status = case HasFailure of true -> partial; false -> ok end,
    {Status, NewVertices, Outbox, Results}.
```

### 4. 失败顶点重试机制

```erlang
%% @doc 重试失败的顶点
-spec retry_failed_vertices(
    SuperstepState :: superstep_state(),
    AllVertices :: #{vertex_id() => vertex()},
    Inbox :: #{vertex_id() => [term()]},
    ComputeFn :: fun(),
    Superstep :: non_neg_integer(),
    NumVertices :: non_neg_integer(),
    MaxRetries :: pos_integer()
) -> {ok | partial, #{vertex_id() => vertex()}, [{vertex_id(), term()}], superstep_state()}.

retry_failed_vertices(SuperstepState, AllVertices, Inbox, ComputeFn,
                      Superstep, NumVertices, MaxRetries) ->
    %% 找出失败且未超过重试次数的顶点
    FailedToRetry = maps:filter(
        fun(_Id, #{status := Status, attempts := Attempts}) ->
            Status =:= failed andalso Attempts < MaxRetries
        end,
        SuperstepState
    ),

    case maps:size(FailedToRetry) of
        0 ->
            %% 无需重试
            finalize_results(SuperstepState, AllVertices);
        _ ->
            %% 提取失败顶点
            FailedVertices = maps:with(maps:keys(FailedToRetry), AllVertices),

            %% 重试执行
            RetryResults = maps:map(
                fun(Id, _OldResult) ->
                    Vertex = maps:get(Id, AllVertices),
                    Result = compute_single_vertex(
                        Id, Vertex, Inbox, ComputeFn, Superstep, NumVertices
                    ),
                    %% 增加尝试次数
                    OldAttempts = maps:get(attempts, maps:get(Id, SuperstepState)),
                    Result#{attempts => OldAttempts + 1}
                end,
                FailedToRetry
            ),

            %% 合并结果
            NewState = maps:merge(SuperstepState, RetryResults),

            %% 检查是否还有失败，递归重试
            retry_failed_vertices(NewState, AllVertices, Inbox, ComputeFn,
                                  Superstep, NumVertices, MaxRetries)
    end.

%% @doc 最终化结果
-spec finalize_results(superstep_state(), #{vertex_id() => vertex()}) ->
    {ok | partial, #{vertex_id() => vertex()}, [{vertex_id(), term()}], superstep_state()}.

finalize_results(SuperstepState, AllVertices) ->
    {NewVertices, Outbox, HasFailure} = maps:fold(
        fun(Id, Result, {VAcc, OAcc, Failed}) ->
            case Result of
                #{status := completed, vertex := V, outbox := Out} ->
                    {VAcc#{Id => V}, Out ++ OAcc, Failed};
                #{status := failed} ->
                    {VAcc, OAcc, true}
            end
        end,
        {AllVertices, [], false},
        SuperstepState
    ),

    Status = case HasFailure of true -> partial; false -> ok end,
    {Status, NewVertices, Outbox, SuperstepState}.
```

### 5. 改造 execute_superstep

```erlang
%% @doc 执行超步（支持部分失败和重试）
-spec execute_superstep(#state{}) -> #state{}.
execute_superstep(#state{
    vertices = Vertices,
    inbox = Inbox,
    compute_fn = ComputeFn,
    superstep = Superstep,
    num_vertices = NumVertices,
    max_retries = MaxRetries  %% 新增配置
} = State) ->

    %% 1. 筛选活跃顶点
    ActiveVertices = filter_active_vertices(Vertices, Inbox),

    %% 2. 执行计算（带异常隔离）
    {Status, NewVertices, Outbox, SuperstepState} =
        compute_vertices_safe(ActiveVertices, Vertices, Inbox,
                              ComputeFn, Superstep, NumVertices),

    %% 3. 如果有失败，尝试重试
    {FinalStatus, FinalVertices, FinalOutbox, FinalState} =
        case Status of
            ok ->
                {ok, NewVertices, Outbox, SuperstepState};
            partial ->
                retry_failed_vertices(SuperstepState, NewVertices, Inbox,
                                      ComputeFn, Superstep, NumVertices, MaxRetries)
        end,

    %% 4. 处理最终结果
    case FinalStatus of
        ok ->
            %% 全部成功，继续正常流程
            route_messages(FinalOutbox, State),
            notify_master_done(State, FinalVertices, FinalOutbox),
            State#state{vertices = FinalVertices, inbox = #{}};
        partial ->
            %% 仍有失败，通知 Master
            FailedIds = get_failed_vertex_ids(FinalState),
            notify_master_partial_failure(State, FinalVertices, FinalOutbox,
                                          FinalState, FailedIds),
            State#state{
                vertices = FinalVertices,
                inbox = #{},
                last_superstep_state = FinalState  %% 保存状态供后续重试
            }
    end.

%% @doc 获取失败顶点 ID 列表
-spec get_failed_vertex_ids(superstep_state()) -> [vertex_id()].
get_failed_vertex_ids(SuperstepState) ->
    [Id || {Id, #{status := failed}} <- maps:to_list(SuperstepState)].
```

### 6. 持久化支持

```erlang
%% @doc 超步 Checkpoint（支持部分完成）
-record(superstep_checkpoint, {
    superstep :: non_neg_integer(),

    %% 顶点状态
    vertices :: #{vertex_id() => vertex()},

    %% 下一超步的消息
    inbox :: #{vertex_id() => [term()]},

    %% ⭐ 新增：顶点执行状态
    vertex_states :: superstep_state(),

    %% 已完成顶点发出的消息（用于恢复时避免重复发送）
    completed_outbox :: [{vertex_id(), term()}],

    %% 元信息
    num_vertices :: non_neg_integer(),
    timestamp :: integer()
}).

%% @doc 保存超步 Checkpoint
-spec save_superstep_checkpoint(#state{}, superstep_state()) -> ok.
save_superstep_checkpoint(State, SuperstepState) ->
    Checkpoint = #superstep_checkpoint{
        superstep = State#state.superstep,
        vertices = State#state.vertices,
        inbox = State#state.inbox,
        vertex_states = SuperstepState,
        completed_outbox = extract_completed_outbox(SuperstepState),
        num_vertices = State#state.num_vertices,
        timestamp = erlang:system_time(millisecond)
    },
    %% 持久化到存储
    store_checkpoint(Checkpoint).

%% @doc 提取已完成顶点的 outbox
-spec extract_completed_outbox(superstep_state()) -> [{vertex_id(), term()}].
extract_completed_outbox(SuperstepState) ->
    lists:flatten([
        maps:get(outbox, Result, [])
        || {_Id, #{status := completed} = Result} <- maps:to_list(SuperstepState)
    ]).

%% @doc 从 Checkpoint 恢复并重试失败顶点
-spec resume_from_checkpoint(superstep_checkpoint(), fun(), pos_integer()) ->
    {ok | partial, #state{}}.
resume_from_checkpoint(Checkpoint, ComputeFn, MaxRetries) ->
    #superstep_checkpoint{
        superstep = Superstep,
        vertices = Vertices,
        inbox = Inbox,
        vertex_states = VertexStates,
        num_vertices = NumVertices
    } = Checkpoint,

    %% 找出需要重试的顶点（失败状态）
    FailedStates = maps:filter(
        fun(_Id, #{status := Status}) -> Status =:= failed end,
        VertexStates
    ),

    case maps:size(FailedStates) of
        0 ->
            %% 无失败顶点，直接恢复
            {ok, build_state(Checkpoint)};
        _ ->
            %% 只重试失败顶点
            {FinalStatus, FinalVertices, FinalOutbox, FinalState} =
                retry_failed_vertices(VertexStates, Vertices, Inbox,
                                      ComputeFn, Superstep, NumVertices, MaxRetries),

            NewState = build_state(Checkpoint),
            {FinalStatus, NewState#state{
                vertices = FinalVertices,
                last_superstep_state = FinalState
            }}
    end.
```

### 7. Worker 状态扩展

```erlang
%% 扩展 Worker 内部状态
-record(state, {
    worker_id     :: non_neg_integer(),
    master        :: pid(),
    vertices      :: #{vertex_id() => vertex()},
    inbox         :: #{vertex_id() => [term()]},
    compute_fn    :: fun((context()) -> context()),
    combiner      :: pregel_combiner:spec() | undefined,
    superstep     :: non_neg_integer(),
    num_workers   :: pos_integer(),
    num_vertices  :: non_neg_integer(),
    worker_pids   :: #{non_neg_integer() => pid()},

    %% ⭐ 新增字段
    max_retries   :: non_neg_integer(),           %% 最大重试次数
    last_superstep_state :: superstep_state()     %% 上次超步执行状态
}).
```

### 8. 配置选项

```erlang
-type retry_opts() :: #{
    max_retries => non_neg_integer(),      %% 最大重试次数，默认 3
    retry_delay => non_neg_integer(),      %% 重试延迟（毫秒），默认 0
    on_vertex_failure => ignore            %% 失败处理策略
                       | abort             %%   ignore: 忽略失败顶点，继续执行
                       | {callback, fun()} %%   abort: 中止整个超步
                                           %%   callback: 调用自定义处理函数
}.
```

---

## 执行流程图

```
超级步 S 执行:

┌─────────────────────────────────────────────────────────────────┐
│                     compute_vertices_safe                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌────────┐   ┌────────┐   ┌────────┐   ┌────────┐            │
│  │ 顶点 A │   │ 顶点 B │   │ 顶点 C │   │ 顶点 D │            │
│  └───┬────┘   └───┬────┘   └───┬────┘   └───┬────┘            │
│      │            │            │            │                   │
│      ▼            ▼            ▼            ▼                   │
│  try-catch    try-catch    try-catch    try-catch              │
│      │            │            │            │                   │
│      ▼            ▼            ▼            ▼                   │
│  completed    completed     failed      completed              │
│      │            │            │            │                   │
│      └────────────┴─────┬──────┴────────────┘                   │
│                         │                                       │
│                         ▼                                       │
│              SuperstepState = #{                                │
│                  a => #{status => completed, ...},              │
│                  b => #{status => completed, ...},              │
│                  c => #{status => failed, error => ...},        │
│                  d => #{status => completed, ...}               │
│              }                                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     retry_failed_vertices                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  重试次数 < MaxRetries?                                         │
│      │                                                          │
│      ├── Yes ──→ 只重试顶点 C ──→ 成功? ──→ 更新状态           │
│      │                              │                           │
│      │                              └── 失败? ──→ 再次重试      │
│      │                                                          │
│      └── No ──→ 标记为最终失败                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        结果处理                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  全部成功 ──→ 正常继续超步 S+1                                   │
│                                                                 │
│  部分失败 ──→ 保存 Checkpoint ──→ 通知上层决定                  │
│                    │                    │                       │
│                    │                    ├── ignore: 继续执行    │
│                    │                    ├── abort: 中止         │
│                    │                    └── callback: 自定义    │
│                    │                                            │
│                    └── 可从 Checkpoint 恢复，只重试失败顶点      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 与 LangGraph 的对比

| 特性 | 改进后的实现 | LangGraph |
|------|-------------|-----------|
| 单顶点异常隔离 | ✅ try-catch | ✅ 支持 |
| 执行状态记录 | ✅ superstep_state | ✅ pending_writes |
| 部分结果保存 | ✅ completed_outbox | ✅ channel_values |
| 失败顶点重试 | ✅ retry_failed_vertices | ✅ 自动重试 |
| Checkpoint 持久化 | ✅ superstep_checkpoint | ✅ 每超步保存 |

---

## 总结

### 核心改动

| 改动项 | 说明 |
|--------|------|
| `compute_single_vertex` | 单顶点执行 + try-catch 异常捕获 |
| `vertex_result()` | 记录执行状态、结果、错误、尝试次数 |
| `superstep_state()` | 整个超步的顶点执行状态映射 |
| `compute_vertices_safe` | 安全版本的批量顶点执行 |
| `retry_failed_vertices` | 只重试失败顶点的递归函数 |
| `superstep_checkpoint` | 支持部分完成的 checkpoint 结构 |
| Worker `#state{}` | 新增 `max_retries` 和 `last_superstep_state` 字段 |

### 新增能力

1. **失败隔离**：单个顶点失败不影响其他顶点
2. **结果保留**：已完成顶点的结果被保存
3. **自动重试**：失败顶点自动重试（可配置次数）
4. **状态可见**：可以查看每个顶点的执行状态
5. **可恢复**：从 checkpoint 恢复后只需重试失败顶点
