# Pregel 错误处理与 Checkpoint 支持任务

## 任务概述

为 Pregel 设计统一的回调机制，支持：
1. Graph 层保存 checkpoint
2. Graph 层处理失败并决策是否继续
3. Human-in-the-loop（中断与恢复）

**设计原则**: Pregel 层只负责调用回调和执行返回的决策，不包含业务逻辑。

---

## P0: 回调函数设计与实现

**优先级**: P0 (必须)

**前置条件**: Worker 错误上报已完成

### 设计概述

```
  超级步编号:  0(initial)    1          2          3(final)
              │            │          │          │
  回调时机:   执行前      超步0后    超步1后     结束时
              │            │          │          │
  type:     initial      step       step       final

  注意：error 和 interrupt 不是 type，而是通过独立字段报告
```

### 统一回调设计

```erlang
%% opts 中的回调配置
-type opts() :: #{
    ...
    on_superstep_complete => fun((superstep_complete_info()) -> superstep_complete_result())
}.

%% 回调信息
-type superstep_complete_info() :: #{
    type := initial | step | final,
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    %% 错误信息
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],
    %% 中断信息（human-in-the-loop）
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}],
    %% Checkpoint 支持
    get_checkpoint_data := fun(() -> checkpoint_data())
}.

%% Checkpoint 数据（按需获取）
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    pending_messages := [{vertex_id(), term()}]
}.

%% 回调返回值
-type superstep_complete_result() :: continue | {stop, term()}.
```

---

### 4.0 扩展 pregel_worker 支持 interrupt ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

**目的**: 支持 human-in-the-loop 场景，顶点计算可返回 interrupt 状态

- [x] 修改 `compute_status()` 类型，增加 `{interrupt, term()}`
- [x] 修改 `process_compute_result/3`，处理 interrupt 状态
- [x] 修改 `execute_superstep/1`，收集 interrupted_vertices
- [x] 修改 `notify_master_done/5`，上报 interrupted_count 和 interrupted_vertices

```erlang
%% 修改后的计算状态
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果累加器（内部使用）
-type compute_acc() :: {
    Vertices :: #{vertex_id() => vertex()},
    Outbox :: [{vertex_id(), term()}],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
}.

%% process_compute_result 新增 interrupt 处理
process_compute_result(Id, #{status := {interrupt, Reason}}, {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 中断：记录中断信息，不更新顶点，不发消息
    {VAcc, OAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]}.

%% notify_master_done 上报中断信息
Result = #{
    worker_id => WorkerId,
    active_count => N,
    message_count => M,
    failed_count => F,
    failed_vertices => [...],
    interrupted_count => I,
    interrupted_vertices => [...]
}.
```

---

### 4.1 定义回调类型 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 定义 `superstep_complete_info()` 类型（含 interrupted 字段）
- [x] 定义 `checkpoint_data()` 类型
- [x] 定义 `superstep_complete_result()` 类型
- [x] 导出类型

---

### 4.2 实现 get_checkpoint_data 函数 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 实现 `collect_vertices_from_workers/1` - 从 Workers 收集顶点状态
- [x] 实现 `collect_pending_messages_list/1` - 收集待处理消息
- [x] 实现 `make_get_checkpoint_data/3` - 创建按需获取函数

```erlang
%% 创建按需获取 checkpoint 数据的函数
-spec make_get_checkpoint_data(non_neg_integer(),
                                #{non_neg_integer() => pid()},
                                #{non_neg_integer() => [{term(), term()}]}) ->
    fun(() -> checkpoint_data()).
make_get_checkpoint_data(Superstep, Workers, PendingMessages) ->
    fun() ->
        Vertices = collect_vertices_from_workers(Workers),
        Messages = collect_pending_messages_list(PendingMessages),
        #{
            superstep => Superstep,
            vertices => Vertices,
            pending_messages => Messages
        }
    end.
```

---

### 4.3 修改 pregel_master 调用回调 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 添加 `on_superstep_complete` 到 #state
- [x] 修改 `handle_call(start_execution)` - 启动后调用 initial 回调
- [x] 修改 `complete_superstep/1` - 调用 step/final 回调
- [x] 实现 `call_superstep_complete/3` - 统一的回调调用
- [x] 实现 `determine_callback_type/2` - 判断回调类型
- [x] 实现 `build_superstep_complete_info/3` - 构建回调信息
- [x] 实现 `empty_superstep_results/0` - 创建空结果（用于 initial 回调）

---

### 4.4 修改 pregel_barrier 汇总失败和中断信息 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_barrier.erl`

- [x] 修改 `aggregate_results/1` 返回 map 格式
- [x] 添加 failed_count 和 failed_vertices 汇总
- [x] 添加 interrupted_count 和 interrupted_vertices 汇总

```erlang
%% 修改后的返回值
-spec aggregate_results([map()]) -> superstep_results().

-type superstep_results() :: #{
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    %% 错误汇总
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],
    %% 中断汇总
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}]
}.
```

---

### 4.5 添加测试 ✅

**文件**: `apps/beamai_core/test/pregel_worker_tests.erl` (补充)

- [x] 测试 interrupt 状态处理
- [x] 测试 Worker 上报 interrupted_count 和 interrupted_vertices

**文件**: `apps/beamai_core/test/pregel_barrier_tests.erl`

- [x] 测试屏障基本功能
- [x] 测试结果汇总（含失败和中断信息）

**文件**: `apps/beamai_core/test/pregel_master_callback_tests.erl`

- [x] 测试 initial 回调调用
- [x] 测试 step 回调调用
- [x] 测试 final 回调调用
- [x] 测试 get_checkpoint_data 函数
- [x] 测试回调返回 continue
- [x] 测试回调返回 {stop, Reason}
- [x] 测试回调收到 failed_vertices 和 interrupted_vertices

---

## P1: Graph 层实现回调

**优先级**: P1 (依赖 P0)

**前置条件**: P0 回调机制已完成

### 5.1 实现 checkpoint 回调

**文件**: `apps/beamai_core/src/graph/graph_runner.erl` (或新文件)

- [ ] 实现 `make_superstep_complete_callback/1`
- [ ] 实现 checkpoint 保存逻辑
- [ ] 实现 checkpoint 恢复逻辑

```erlang
%% 创建回调函数
make_superstep_complete_callback(Opts) ->
    CheckpointFn = maps:get(checkpoint_fn, Opts, undefined),
    ErrorStrategy = maps:get(on_vertex_error, Opts, fail_fast),

    fun(#{
        type := Type,
        superstep := Superstep,
        failed_count := FailedCount,
        failed_vertices := FailedVertices,
        interrupted_count := InterruptedCount,
        interrupted_vertices := InterruptedVertices,
        get_checkpoint_data := GetData
    }) ->
        %% 1. 优先处理中断（human-in-the-loop）
        case InterruptedCount > 0 of
            true ->
                Data = GetData(),
                save_checkpoint(step, Data),
                {stop, {interrupted, InterruptedVertices}};
            false ->
                %% 2. 处理 checkpoint
                maybe_save_checkpoint(Type, Superstep, GetData, CheckpointFn),
                %% 3. 处理失败并决策
                handle_failures_and_decide(Type, FailedCount, FailedVertices, ErrorStrategy)
        end
    end.
```

### 5.2 实现失败和中断处理决策

- [ ] 实现 `handle_failures_and_decide/4`
- [ ] 支持 fail_fast 策略
- [ ] 支持 continue 策略
- [ ] 支持 callback 策略
- [ ] 实现中断恢复逻辑（从 checkpoint 恢复并注入人工输入）

---

### 5.3 可选：重试逻辑

**文件**: `apps/beamai_core/src/graph/pregel/graph_compute.erl`

- [ ] 实现 should_retry/1 函数
- [ ] 实现 retry_with_backoff/3 函数
- [ ] 配置最大重试次数

---

## 职责分离

```
┌─────────────────────────────────────────────────────────────────┐
│                    分层职责                                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Graph 层                                                       │
│  ─────────                                                      │
│  • 实现 on_superstep_complete 回调                              │
│  • 决定是否保存 checkpoint                                       │
│  • 决定失败/中断时是否继续                                       │
│  • 处理 human-in-the-loop（中断恢复）                            │
│  • 返回 continue | {stop, Reason}                               │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  Pregel 层 (pregel_master)                                      │
│  ─────────────────────────                                      │
│  • 在合适时机调用回调（initial/step/final）                      │
│  • 提供 get_checkpoint_data 函数                                │
│  • 汇总并传递 failed/interrupted 信息                           │
│  • 根据回调返回值执行（继续或停止）                              │
│  • 不做业务决策                                                  │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  Pregel 层 (pregel_barrier)                                     │
│  ─────────────────────────                                      │
│  • 汇总所有 Worker 的结果                                        │
│  • 包含 failed_count/failed_vertices                            │
│  • 包含 interrupted_count/interrupted_vertices                  │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  Pregel 层 (pregel_worker)                                      │
│  ─────────────────────────                                      │
│  • 处理 compute_status: ok | {error, R} | {interrupt, R}       │
│  • 上报 failed_count/failed_vertices                            │
│  • 上报 interrupted_count/interrupted_vertices                  │
│  • ✅ error 和 interrupt 上报已完成                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 依赖关系

```
P0-已完成: pregel_worker 错误上报 ✅
       ↓
P0-已完成: pregel_worker 中断支持 (4.0) ✅
       ↓
P0-已完成: pregel_barrier 汇总失败和中断信息 (4.4) ✅
       ↓
P0-已完成: pregel_master 回调机制 (4.1-4.3) ✅
       ↓
P0-已完成: 回调测试 (4.5) ✅
       ↓
P1-待做: Graph 层实现回调 (5.1-5.2)
       ↓
P1-可选: 重试逻辑 (5.3)
```

---

## 相关文档

- `info/pregel_callback_checkpoint_analysis.md` - 回调与 Checkpoint 分析
- `info/pregel_master_failure_handling.md` - Master 失败处理设计
- `info/pregel_worker_error_handling_design.md` - Worker 错误处理设计

---

## 已完成任务

见 `TASK_DONE.md`
