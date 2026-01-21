# Pregel 超级步执行机制

## 概述

Pregel 采用 BSP（Bulk Synchronous Parallel，批量同步并行）模型执行图计算。每个超级步包含：**计算 → 消息发送 → 同步屏障 → 下一超步**。

## 架构图

```
┌─────────────────────────────────────────────────────────────────┐
│                        pregel_master                             │
│  (协调者 - BSP 同步屏障)                                          │
├─────────────────────────────────────────────────────────────────┤
│  1. broadcast_start_superstep() → 通知所有 Worker 开始           │
│  2. 等待所有 Worker 完成 (pregel_barrier)                        │
│  3. complete_superstep() → 汇总结果、检查终止条件                │
│  4. 继续下一超步 或 结束执行                                      │
└─────────────────────────────────────────────────────────────────┘
           │                    ▲
           ▼                    │
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   Worker 0   │  │   Worker 1   │  │   Worker N   │
│  (分区 0)    │  │  (分区 1)    │  │  (分区 N)    │
└──────────────┘  └──────────────┘  └──────────────┘
```

## 核心模块

| 模块 | 职责 |
|------|------|
| `pregel.erl` | 门面模块，提供统一 API |
| `pregel_master.erl` | 协调者，管理超步同步和终止检测 |
| `pregel_worker.erl` | 工作进程，执行本地顶点计算和消息路由 |
| `pregel_graph.erl` | 图数据结构 |
| `pregel_vertex.erl` | 顶点数据结构 |
| `pregel_barrier.erl` | 同步屏障实现 |

## 单个超级步执行流程

### Worker 端执行 (`pregel_worker.erl:168-194`)

```erlang
execute_superstep(State) ->
    %% 1. 筛选需要计算的顶点（有消息 OR 活跃）
    ActiveVertices = filter_active_vertices(Vertices, Inbox),

    %% 2. 执行所有顶点的计算函数
    {NewVertices, Outbox} = compute_vertices(...),

    %% 3. 应用消息合并器（如果有）
    CombinedOutbox = apply_combiner(Outbox, Combiner),

    %% 4. 路由消息到目标 Worker
    route_messages(CombinedOutbox, State),

    %% 5. 通知 Master 完成
    notify_master_done(State, NewVertices, CombinedOutbox),

    %% 6. 清空收件箱，返回新状态
    State#state{vertices = NewVertices, inbox = #{}}.
```

### 顶点计算过程 (`pregel_worker.erl:217-230`)

对每个活跃顶点：

```erlang
%% 创建计算上下文
Context = #{
    vertex => Vertex,        %% 当前顶点
    messages => Messages,    %% 收到的消息
    superstep => Superstep,  %% 当前超步编号
    num_vertices => N,       %% 全图顶点数
    outbox => []             %% 待发送消息（初始为空）
},

%% 执行用户定义的计算函数
NewContext = ComputeFn(Context),

%% 提取结果
NewVertex = NewContext.vertex,
OutMessages = NewContext.outbox.
```

### Master 协调流程 (`pregel_master.erl:252-276`)

```erlang
complete_superstep(State) ->
    %% 1. 汇总所有 Worker 结果
    {TotalActive, TotalMessages} = aggregate_results(Results),

    %% 2. 发送待处理消息
    deliver_pending_messages(PendingMessages, Workers),

    %% 3. 检查终止条件
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
    MaxReached = Superstep >= MaxSupersteps - 1,

    %% 4. 决定下一步
    case {Halted, MaxReached} of
        {true, _}      -> finish_execution(completed, State);
        {_, true}      -> finish_execution(max_supersteps, State);
        {false, false} -> start_next_superstep(State)
    end.
```

## 顶点状态

顶点有两种状态：

| 状态 | 说明 |
|------|------|
| **活跃 (active)** | 顶点会在下一超步执行计算 |
| **停止 (halted)** | 顶点不会执行计算，除非收到消息 |

顶点可以通过以下方式改变状态：
- `pregel:vote_to_halt(Ctx)` - 投票停止（标记为非活跃）
- 收到消息时自动激活

## 终止条件

| 条件 | 结果状态 |
|------|----------|
| 所有顶点停止 **AND** 无待发消息 | `completed` |
| 达到最大超步数 | `max_supersteps` |

## 消息路由

```
顶点发送消息
    ↓
按目标 Worker 分组 (group_by_target_worker)
    ↓
┌─────────────────────────────────────┐
│ 本地消息: self() ! {local_messages} │
│ 远程消息: receive_messages(Pid)     │
│ 未知目标: Master 转发               │
└─────────────────────────────────────┘
```

### 消息路由代码 (`pregel_worker.erl:273-322`)

```erlang
route_messages(Outbox, State) ->
    %% 按目标 Worker 分组
    GroupedByWorker = group_by_target_worker(Outbox, NumWorkers),

    %% 发送到各 Worker
    maps:foreach(
        fun(TargetId, Messages) ->
            case TargetId of
                MyId ->
                    %% 本地消息
                    self() ! {local_messages, Messages};
                _ ->
                    %% 远程消息
                    case maps:get(TargetId, WorkerPids, undefined) of
                        undefined ->
                            %% 通过 Master 转发
                            gen_server:cast(Master, {route_messages, TargetId, Messages});
                        Pid ->
                            receive_messages(Pid, Messages)
                    end
            end
        end,
        GroupedByWorker
    ).
```

## 计算上下文 API

在计算函数中可使用的 API：

### 读取操作

| 函数 | 说明 |
|------|------|
| `pregel:get_vertex_id(Ctx)` | 获取当前顶点 ID |
| `pregel:get_vertex_value(Ctx)` | 获取当前顶点值 |
| `pregel:get_messages(Ctx)` | 获取收到的消息列表 |
| `pregel:get_superstep(Ctx)` | 获取当前超步编号 |
| `pregel:get_neighbors(Ctx)` | 获取邻居 ID 列表 |
| `pregel:get_edges(Ctx)` | 获取出边列表 |
| `pregel:get_num_vertices(Ctx)` | 获取全局顶点数量 |

### 修改操作

| 函数 | 说明 |
|------|------|
| `pregel:set_value(Ctx, Value)` | 设置顶点值 |
| `pregel:vote_to_halt(Ctx)` | 投票停止 |
| `pregel:send_message(Ctx, Target, Msg)` | 发送消息给指定顶点 |
| `pregel:send_to_all_neighbors(Ctx, Msg)` | 向所有邻居发送消息 |

## 使用示例

```erlang
%% 构建图
G0 = pregel:new_graph(),
G1 = pregel:add_vertex(G0, a, 1.0),
G2 = pregel:add_vertex(G1, b, 2.0),
G3 = pregel:add_edge(G2, a, b),

%% 定义计算函数
ComputeFn = fun(Ctx) ->
    Value = pregel:get_vertex_value(Ctx),
    Ctx1 = pregel:send_to_all_neighbors(Ctx, Value),
    pregel:vote_to_halt(Ctx1)
end,

%% 执行 Pregel 计算
Result = pregel:run(G3, ComputeFn, #{
    max_supersteps => 10,
    num_workers => 4
}).
```

## 执行选项

| 选项 | 说明 | 默认值 |
|------|------|--------|
| `max_supersteps` | 最大超步数 | 100 |
| `num_workers` | Worker 数量 | CPU 核心数 |
| `combiner` | 消息合并器 (sum, min, max 等) | undefined |
| `on_superstep` | 超步回调函数 | undefined |

## 注意事项

1. **checkpoint_interval 未实现**：虽然 `pregel_graph.erl` 定义了 `checkpoint_interval` 配置，但当前版本没有在超级步级别实现自动 checkpoint。

2. **消息顺序**：同一超步内发送的消息会在下一超步开始时送达，消息顺序不保证。

3. **顶点激活**：收到消息的顶点会自动激活，即使之前已经投票停止。

4. **分区策略**：顶点按哈希分配到 Worker，由 `pregel_partition` 模块处理。
