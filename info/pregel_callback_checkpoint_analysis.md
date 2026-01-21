# Pregel 回调函数与 Checkpoint 支持分析

## Checkpoint 需求

```
  超级步编号:  0(initial)    1          2          3(final)
              │            │          │          │
  保存时机:   执行前      超步1后    超步2后     结束时
              │            │          │          │
  type:     :initial     :step      :step      :final
```

| Checkpoint 类型 | 触发时机 | 需要的数据 |
|----------------|----------|-----------|
| **:initial** | 超步 0 执行前 | 初始图状态 |
| **:step** | 超步 N 完成后 | 当前顶点状态、消息、失败信息 |
| **:final** | 执行结束时 | 最终顶点状态、执行状态 |

---

## 当前回调的局限性

### on_superstep 现状

```erlang
%% 当前签名
on_superstep => fun((non_neg_integer(), graph()) -> ok)

%% 调用时机：超步开始前
%% 问题：
%% 1. Graph 是原始图，不是当前状态
%% 2. 顶点状态存储在 Workers 中，Master 不持有
%% 3. 无法获取当前消息和失败信息
```

### 无法支持 Checkpoint 的原因

```
┌─────────────────────────────────────────────────────────────────┐
│                    数据分布问题                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Master 进程:                                                    │
│  ├── graph = OriginalGraph    ← 原始图，从不更新！               │
│  ├── superstep = N                                              │
│  └── on_superstep(N, OriginalGraph)  ← 回调收到的是原始图        │
│                                                                 │
│  Worker 进程:                                                    │
│  ├── vertices = #{...}        ← 当前顶点状态在这里！             │
│  └── inbox = #{...}           ← 当前消息在这里！                 │
│                                                                 │
│  ⚠️ Checkpoint 需要的数据在 Workers 中，但回调无法访问           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 回调函数设计方案

### 设计目标

1. 支持 Graph 层在合适时机保存 checkpoint
2. 支持 Graph 层处理失败并决策是否继续
3. 保持 Pregel 层通用性，不包含业务逻辑
4. 提供获取顶点状态的能力（按需获取，避免性能开销）

### 回调时机与类型

```
┌─────────────────────────────────────────────────────────────────┐
│                    回调时机设计                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  执行流程:                                                       │
│                                                                 │
│  ┌─────────────┐                                                │
│  │ 初始化完成   │                                                │
│  └──────┬──────┘                                                │
│         │                                                       │
│         ▼                                                       │
│  on_superstep_complete({type => initial, superstep => 0, ...}) │
│         │                        ↑                              │
│         │                        │ :initial checkpoint          │
│         ▼                                                       │
│  ┌─────────────┐                                                │
│  │ 执行超步 0   │                                                │
│  └──────┬──────┘                                                │
│         │                                                       │
│         ▼                                                       │
│  on_superstep_complete({type => step, superstep => 0, ...})    │
│         │                        ↑                              │
│         │                        │ :step checkpoint             │
│         ▼                                                       │
│  ┌─────────────┐                                                │
│  │ 执行超步 1   │                                                │
│  └──────┬──────┘                                                │
│         │                                                       │
│         ▼                                                       │
│  on_superstep_complete({type => step, superstep => 1, ...})    │
│         │                                                       │
│        ...                                                      │
│         │                                                       │
│         ▼                                                       │
│  on_superstep_complete({type => final, superstep => N, ...})   │
│                                  ↑                              │
│                                  │ :final checkpoint            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 统一的回调设计

```erlang
%% 回调信息类型
-type superstep_complete_info() :: #{
    %% 基本信息
    type := initial | step | final,
    superstep := non_neg_integer(),

    %% 执行结果（initial 时为空）
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],

    %% Checkpoint 支持：获取当前状态的函数
    get_checkpoint_data := fun(() -> checkpoint_data())
}.

%% Checkpoint 数据
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    pending_messages := [{vertex_id(), term()}]
}.

%% 回调返回值
-type superstep_complete_result() ::
    continue |                    %% 继续下一超步
    {stop, term()}.               %% 停止执行
```

---

## 关键设计决策

### 1. 按需获取顶点状态

```erlang
%% 提供函数而非直接提供数据
get_checkpoint_data := fun(() -> checkpoint_data())

%% 优点：
%% - 不需要 checkpoint 时，不会收集顶点状态（避免性能开销）
%% - Graph 层决定何时需要 checkpoint
%% - 保持灵活性

%% 使用示例（Graph 层）
handle_superstep_complete(#{
    type := step,
    superstep := Superstep,
    get_checkpoint_data := GetData
} = Info) ->
    case should_checkpoint(Superstep) of
        true ->
            CheckpointData = GetData(),  %% 只在需要时调用
            save_checkpoint(step, CheckpointData);
        false ->
            ok
    end,
    decide_continue_or_stop(Info).
```

### 2. 统一的回调替代多个回调

```erlang
%% 方案 A：多个回调（不推荐）
on_execution_start => fun(...),
on_superstep_complete => fun(...),
on_execution_complete => fun(...)

%% 方案 B：统一回调 + type 区分（推荐）
on_superstep_complete => fun(#{type := initial | step | final, ...})

%% 方案 B 优点：
%% - 简化 API
%% - 统一的数据结构
%% - Graph 层可以用同一个函数处理所有情况
```

### 3. initial 类型的特殊处理

```erlang
%% initial 类型在超步 0 执行前调用
%% 此时没有执行结果，但有初始状态

on_superstep_complete(#{
    type := initial,
    superstep := 0,
    active_count := 0,      %% 还没执行，为 0
    message_count := 0,
    failed_count := 0,
    failed_vertices := [],
    get_checkpoint_data := fun() ->
        %% 返回初始状态
        #{
            superstep => 0,
            vertices => InitialVertices,
            pending_messages => []
        }
    end
})
```

---

## 实现要点

### Master 需要的修改

1. **初始化后调用 initial 回调**
   ```erlang
   init(...) ->
       ...
       call_superstep_complete(initial, 0, State),
       start_superstep_0(State).
   ```

2. **每个超步完成后调用 step/final 回调**
   ```erlang
   complete_superstep(State) ->
       Results = aggregate_results(),
       Type = determine_type(Results, State),  %% step 或 final
       Decision = call_superstep_complete(Type, Superstep, Results, State),
       handle_decision(Decision, State).
   ```

3. **实现 get_checkpoint_data 函数**
   ```erlang
   make_get_checkpoint_data(Workers, Superstep) ->
       fun() ->
           %% 从所有 Workers 收集顶点状态
           Vertices = collect_vertices_from_workers(Workers),
           PendingMessages = collect_pending_messages(Workers),
           #{
               superstep => Superstep,
               vertices => Vertices,
               pending_messages => PendingMessages
           }
       end.
   ```

---

## 与失败处理的整合

```erlang
%% Graph 层的统一回调处理
handle_superstep_complete(#{
    type := Type,
    superstep := Superstep,
    failed_count := FailedCount,
    failed_vertices := FailedVertices,
    get_checkpoint_data := GetData
} = Info) ->

    %% 1. 处理 checkpoint（如果需要）
    case should_checkpoint(Type, Superstep) of
        true ->
            CheckpointData = GetData(),
            save_checkpoint(Type, CheckpointData);
        false ->
            ok
    end,

    %% 2. 处理失败（如果有）
    case FailedCount > 0 of
        true ->
            handle_failures(FailedVertices);
        false ->
            ok
    end,

    %% 3. 决定是否继续
    case {Type, FailedCount} of
        {final, _} ->
            {stop, normal};
        {_, N} when N > 0 ->
            {stop, {vertex_failures, FailedVertices}};
        _ ->
            continue
    end.
```

---

## Interrupt 支持分析（Human-in-the-Loop）

### 为什么需要 Interrupt

在 human-in-the-loop 场景中，顶点计算可能需要暂停等待人工输入：

```
┌─────────────────────────────────────────────────────────────────┐
│                    Error vs Interrupt                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Error（错误）:                                                  │
│  ├── 非预期的失败状态                                            │
│  ├── 计算函数执行出错                                            │
│  └── 通常需要回滚或终止                                          │
│                                                                 │
│  Interrupt（中断）:                                              │
│  ├── 预期的暂停行为                                              │
│  ├── 需要外部输入才能继续                                        │
│  ├── 保存状态后可恢复执行                                        │
│  └── 典型场景：人工审批、用户确认、外部数据获取                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 设计决策：type 不增加 error/interrupt

```erlang
%% 回调 type 保持不变
type := initial | step | final

%% 原因：
%% 1. error 和 interrupt 不是超步完成的"类型"
%% 2. 它们是超步执行过程中发生的"条件"
%% 3. 通过独立字段报告，Graph 层根据字段值做决策
```

### 更新后的设计

#### 1. pregel_worker 计算状态扩展

```erlang
%% 当前
-type compute_status() :: ok | {error, term()}.

%% 扩展后
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% compute_result 不变
-type compute_result() :: #{
    vertex := vertex(),
    outbox := [{vertex_id(), term()}],
    status := compute_status()
}.
```

#### 2. 回调信息扩展

```erlang
-type superstep_complete_info() :: #{
    %% 基本信息
    type := initial | step | final,
    superstep := non_neg_integer(),

    %% 执行结果
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),

    %% 错误信息
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],

    %% 新增：中断信息
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}],

    %% Checkpoint 支持
    get_checkpoint_data := fun(() -> checkpoint_data())
}.
```

#### 3. Worker 上报结果扩展

```erlang
%% notify_master_done 发送的结果
Result = #{
    worker_id => WorkerId,
    active_count => N,
    message_count => M,
    %% 错误
    failed_count => F,
    failed_vertices => [{Id, Reason}, ...],
    %% 中断
    interrupted_count => I,
    interrupted_vertices => [{Id, Reason}, ...]
}.
```

### Interrupt 处理流程

```
┌─────────────────────────────────────────────────────────────────┐
│                    Interrupt 完整流程                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 顶点计算返回 interrupt                                       │
│     ┌─────────────────────────────────────────┐                 │
│     │ compute_fn(Ctx) ->                      │                 │
│     │     case needs_human_input(Ctx) of      │                 │
│     │         true ->                         │                 │
│     │             #{vertex => V,              │                 │
│     │               outbox => [],             │                 │
│     │               status => {interrupt,     │                 │
│     │                   #{reason => approval, │                 │
│     │                     data => ...}}};     │                 │
│     │         false ->                        │                 │
│     │             %% 正常计算                  │                 │
│     │     end                                 │                 │
│     └─────────────────────────────────────────┘                 │
│                          │                                      │
│                          ▼                                      │
│  2. Worker 收集中断顶点（类似失败顶点）                          │
│     process_compute_result(Id, #{status := {interrupt, R}}, Acc)│
│                          │                                      │
│                          ▼                                      │
│  3. Worker 上报 Master                                          │
│     #{interrupted_count => 1,                                   │
│       interrupted_vertices => [{Id, Reason}]}                   │
│                          │                                      │
│                          ▼                                      │
│  4. Master 汇总并调用回调                                        │
│     on_superstep_complete(#{                                    │
│         type => step,                                           │
│         interrupted_count => N,                                 │
│         interrupted_vertices => [...],                          │
│         get_checkpoint_data => GetData                          │
│     })                                                          │
│                          │                                      │
│                          ▼                                      │
│  5. Graph 层决策                                                 │
│     case InterruptedCount > 0 of                                │
│         true ->                                                 │
│             Data = GetData(),                                   │
│             save_checkpoint(step, Data),                        │
│             {stop, {interrupted, InterruptedVertices}};         │
│         false ->                                                │
│             continue                                            │
│     end                                                         │
│                          │                                      │
│                          ▼                                      │
│  6. 恢复执行                                                     │
│     - 外部系统提供人工输入                                       │
│     - 加载 checkpoint                                           │
│     - 将输入注入到中断的顶点                                     │
│     - 继续执行                                                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Graph 层处理示例

```erlang
%% Graph 层的统一回调处理（含 interrupt）
handle_superstep_complete(#{
    type := Type,
    superstep := Superstep,
    failed_count := FailedCount,
    failed_vertices := FailedVertices,
    interrupted_count := InterruptedCount,
    interrupted_vertices := InterruptedVertices,
    get_checkpoint_data := GetData
}) ->

    %% 1. 优先处理中断（保存 checkpoint 以便恢复）
    case InterruptedCount > 0 of
        true ->
            CheckpointData = GetData(),
            save_checkpoint(step, CheckpointData),
            {stop, {interrupted, InterruptedVertices}};
        false ->
            %% 2. 处理错误
            case FailedCount > 0 of
                true ->
                    handle_failures(FailedVertices, GetData);
                false ->
                    %% 3. 正常流程
                    case Type of
                        final ->
                            {stop, normal};
                        _ ->
                            continue
                    end
            end
    end.
```

---

## 总结

| 问题 | 解决方案 |
|------|----------|
| 回调无法获取当前顶点状态 | 提供 `get_checkpoint_data` 函数 |
| 多个时机需要不同回调 | 统一回调 + `type` 字段区分 |
| Checkpoint 可能影响性能 | 按需调用，不需要时不收集 |
| 失败处理与 checkpoint 整合 | 同一个回调处理两种需求 |
| Human-in-the-loop 支持 | 新增 `{interrupt, term()}` 状态和相关字段 |
