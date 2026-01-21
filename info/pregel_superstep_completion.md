# Pregel 超级步完成判断机制

## 概述

Pregel 使用 **Barrier（同步屏障）** 机制来判断一个超级步是否执行完成。当所有 Worker 都完成计算并报告结果后，超级步才算完成。

## 1. Barrier 数据结构

代码位置：`pregel_barrier.erl:21-25`

```erlang
-record(barrier, {
    expected :: non_neg_integer(),    %% 期望的 Worker 数
    received :: non_neg_integer(),    %% 已收到的完成通知数
    results  :: [map()]               %% Worker 结果列表
}).
```

| 字段 | 说明 |
|------|------|
| `expected` | 需要等待的 Worker 总数 |
| `received` | 已收到完成通知的 Worker 数 |
| `results` | 各 Worker 返回的执行结果 |

## 2. 完成判断逻辑

代码位置：`pregel_barrier.erl:52-54`

```erlang
%% @doc 检查屏障是否完成
-spec is_complete(t()) -> boolean().
is_complete(#barrier{expected = E, received = R}) ->
    R >= E.
```

**判断条件**：`received >= expected`

当收到的完成通知数大于或等于期望的 Worker 数时，超级步完成。

## 3. 执行流程

### 3.1 超级步开始时

代码位置：`pregel_master.erl:170` 和 `pregel_master.erl:297`

```erlang
%% 创建新的 Barrier，期望值为 Worker 数量
barrier = pregel_barrier:new(maps:size(Workers))
```

### 3.2 Worker 完成时

代码位置：`pregel_master.erl:242-246`

```erlang
-spec handle_worker_done(map(), #state{}) -> #state{}.
handle_worker_done(Result, #state{barrier = Barrier} = State) ->
    %% 1. 记录 Worker 完成
    NewBarrier = pregel_barrier:record_done(Result, Barrier),
    NewState = State#state{barrier = NewBarrier},

    %% 2. 检查是否所有 Worker 都完成了
    case pregel_barrier:is_complete(NewBarrier) of
        true ->
            %% 所有 Worker 完成，执行超级步完成逻辑
            complete_superstep(NewState);
        false ->
            %% 还有 Worker 未完成，继续等待
            NewState
    end.
```

### 3.3 记录完成通知

代码位置：`pregel_barrier.erl:44-49`

```erlang
%% @doc 记录 Worker 完成通知
-spec record_done(map(), t()) -> t().
record_done(Result, #barrier{received = R, results = Rs} = B) ->
    B#barrier{
        received = R + 1,        %% 计数 +1
        results = [Result | Rs]  %% 保存结果
    }.
```

## 4. 流程图

```
超级步 N 执行:

┌──────────────────────────────────────────────────────────────────┐
│                          Master                                  │
│                                                                  │
│  Barrier = #barrier{expected = 3, received = 0, results = []}   │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
         ▲                    ▲                    ▲
         │                    │                    │
    Worker 0 完成        Worker 1 完成        Worker 2 完成
    {worker_done, R0}    {worker_done, R1}    {worker_done, R2}
         │                    │                    │
         ▼                    ▼                    ▼
┌──────────────────────────────────────────────────────────────────┐
│  收到 Worker 0:                                                  │
│  Barrier = #barrier{expected = 3, received = 1, results = [R0]} │
│  is_complete? 1 >= 3 → false，继续等待                           │
├──────────────────────────────────────────────────────────────────┤
│  收到 Worker 1:                                                  │
│  Barrier = #barrier{expected = 3, received = 2, results = [...]}│
│  is_complete? 2 >= 3 → false，继续等待                           │
├──────────────────────────────────────────────────────────────────┤
│  收到 Worker 2:                                                  │
│  Barrier = #barrier{expected = 3, received = 3, results = [...]}│
│  is_complete? 3 >= 3 → true，超级步完成！                        │
│                                                                  │
│  → 调用 complete_superstep()                                     │
└──────────────────────────────────────────────────────────────────┘
```

## 5. Worker 结果汇总

超级步完成后，Master 会汇总所有 Worker 的结果。

代码位置：`pregel_master.erl:261-262`

```erlang
Results = pregel_barrier:get_results(Barrier),
{TotalActive, TotalMessages} = pregel_barrier:aggregate_results(Results),
```

汇总逻辑：`pregel_barrier.erl:68-78`

```erlang
%% @doc 汇总 Worker 结果
%% 返回 {活跃顶点数, 消息数}
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

汇总结果用于判断是否继续执行下一超级步：
- `TotalActive > 0`：还有活跃顶点，继续
- `TotalMessages > 0`：还有待处理消息，继续
- 两者都为 0：图计算完成

## 6. Barrier 重置

下一超级步开始前，Barrier 会被重置。

代码位置：`pregel_barrier.erl:62-64`

```erlang
%% @doc 重置屏障以供下一超步使用
-spec reset(non_neg_integer(), t()) -> t().
reset(NumWorkers, _Barrier) ->
    new(NumWorkers).
```

## 7. 完整生命周期

```
┌─────────────────────────────────────────────────────────────────┐
│                    超级步 N 生命周期                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 创建 Barrier                                                │
│     barrier = pregel_barrier:new(NumWorkers)                    │
│     expected = NumWorkers, received = 0                         │
│                                                                 │
│  2. 启动所有 Worker 执行计算                                     │
│     pregel_worker:execute_superstep(Pid, Superstep)            │
│                                                                 │
│  3. 等待 Worker 完成通知                                         │
│     handle_info({worker_done, Result}, State)                   │
│                                                                 │
│  4. 记录每个完成的 Worker                                        │
│     NewBarrier = pregel_barrier:record_done(Result, Barrier)   │
│                                                                 │
│  5. 检查是否完成                                                 │
│     pregel_barrier:is_complete(NewBarrier)                      │
│     received >= expected ?                                      │
│                                                                 │
│  6. 如果完成，执行 complete_superstep()                          │
│     - 汇总结果                                                   │
│     - 投递 pending_messages                                     │
│     - 判断是否继续                                               │
│                                                                 │
│  7. 重置 Barrier 进入下一超级步                                   │
│     barrier = pregel_barrier:new(NumWorkers)                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 8. 总结

| 项目 | 说明 |
|------|------|
| **判断方式** | Barrier 同步屏障机制 |
| **判断条件** | `received >= expected` |
| **expected** | Worker 总数（图初始化时确定） |
| **received** | 收到 `{worker_done, Result}` 的次数 |
| **触发动作** | 当条件满足时调用 `complete_superstep()` |
| **结果汇总** | 统计活跃顶点数和消息数 |
| **下一步决策** | 根据汇总结果决定继续执行还是终止 |

## 9. 关键点

1. **同步等待**：Master 必须等待所有 Worker 完成才能进入下一超级步
2. **无序到达**：Worker 完成通知可以以任意顺序到达
3. **结果收集**：每个 Worker 的执行结果都会被保存
4. **原子转换**：超级步完成是一个原子操作，要么完成要么未完成
