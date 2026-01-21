# Pregel Worker Outbox 消息分析

## 问题

1. Outbox 发的消息的结构是什么？
2. 是否应该合并 Outbox 中的消息？

---

## 问题 1：Outbox 消息的结构

### 类型定义

代码位置：`pregel_worker.erl:51`

```erlang
outbox := [{vertex_id(), term()}]
```

### 在 Graph 层的具体消息

代码位置：`graph_compute.erl:217-227`

```erlang
pregel:send_message(Ctx, TargetNode, {state, State})
```

### 消息结构

```erlang
%% Outbox 中的每条消息
{TargetVertexId, {state, GraphState}}

%% 完整示例
Outbox = [
    {llm_call, {state, #{messages => [...], context => #{...}}}},
    {execute_tools, {state, #{messages => [...], tools => [...]}}}
]
```

### send_message 实现

代码位置：`pregel.erl:205-207`

```erlang
-spec send_message(context(), vertex_id(), term()) -> context().
send_message(#{outbox := Outbox} = Ctx, Target, Value) ->
    Ctx#{outbox => [{Target, Value} | Outbox]}.
```

### Outbox 消息流程图

```
┌─────────────────────────────────────────────────────────────────┐
│                    Outbox 消息结构                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  顶点计算过程:                                                   │
│                                                                 │
│  Context 初始状态:                                               │
│  #{                                                             │
│    vertex => ...,                                               │
│    messages => [...],                                           │
│    outbox => []  ← 初始为空                                     │
│  }                                                              │
│                                                                 │
│  调用 send_message 后:                                          │
│  #{                                                             │
│    outbox => [                                                  │
│      {target_vertex_id, {state, GraphState}}                   │
│    ]                                                            │
│  }                                                              │
│                                                                 │
│  多次调用后:                                                     │
│  #{                                                             │
│    outbox => [                                                  │
│      {node_c, {state, S1}},                                    │
│      {node_b, {state, S2}},                                    │
│      {node_a, {state, S3}}                                     │
│    ]                                                            │
│  }                                                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 问题 2：是否应该合并 Outbox 中的消息？

### 当前实现

代码位置：`pregel_worker.erl:262-270`

```erlang
apply_combiner(Outbox, undefined) ->
    Outbox;  %% 无合并器，直接返回
apply_combiner(Outbox, Combiner) ->
    CombinerFn = pregel_combiner:get(Combiner),
    Grouped = pregel_utils:group_messages(Outbox),      %% 按目标分组
    Combined = pregel_utils:apply_to_groups(CombinerFn, Grouped),  %% 应用合并器
    [{Target, Value} || {Target, Value} <- maps:to_list(Combined)].
```

### 当前支持的合并器

代码位置：`pregel_combiner.erl`

| 合并器 | 说明 | 适用场景 |
|--------|------|----------|
| `sum` | 累加 | 数值统计 |
| `min` | 取最小值 | 最短路径 |
| `max` | 取最大值 | 最大值传播 |
| `count` | 计数 | 消息统计 |
| `first` | 取首个 | 只需一个值 |
| `last` | 取最后 | 最新值 |
| `list` | 保留所有 | 不合并 |
| `and` | 逻辑与 | 布尔聚合 |
| `or` | 逻辑或 | 布尔聚合 |

### Graph 场景消息特点

```
┌─────────────────────────────────────────────────────────────────┐
│                    Graph 场景消息特点                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  消息格式: {state, GraphState}                                  │
│                                                                 │
│  GraphState = #{                                                │
│    messages => [...],        %% 对话消息列表                    │
│    full_messages => [...],   %% 完整历史                        │
│    context => #{...},        %% 用户上下文                      │
│    tools => [...],           %% 工具定义                        │
│    iteration => N,           %% 迭代计数                        │
│    ...                                                          │
│  }                                                              │
│                                                                 │
│  特点:                                                          │
│  1. 消息是完整的状态，不是简单值                                  │
│  2. 通常一个顶点只向一个目标发送一条消息                          │
│  3. 并行节点可能向同一目标发送多条消息                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 场景分析

#### 场景 1: 顺序执行（当前主要场景）

```
__start__ → llm_call → execute_tools → __end__

每个节点只向一个目标发送一条消息
❌ 不需要合并（没有重复目标）
```

#### 场景 2: 并行节点向同一目标发送

```
        ┌─── Node A ───┐
        │              │
        │ {state, SA}  │
        │              ├───→ Node C
        │ {state, SB}  │
        │              │
        └─── Node B ───┘

Node C 收到两条 {state, ...} 消息
当前处理: aggregate_state_messages → maps:merge
✅ 已在接收端合并，不需要在发送端合并
```

#### 场景 3: 同一顶点多次向同一目标发送

```
Node A 执行中:
  send_message(Ctx, node_b, {state, S1})
  send_message(Ctx, node_b, {state, S2})

Outbox = [
  {node_b, {state, S1}},
  {node_b, {state, S2}}
]

⚠️ 可能需要合并（减少消息数量）
```

### 合并的权衡

| 方面 | 合并（发送端） | 不合并（接收端处理） |
|------|--------------|-------------------|
| **网络开销** | ✅ 减少消息数 | ❌ 多条消息 |
| **内存使用** | ✅ Worker 内合并 | ❌ 跨 Worker 传输 |
| **灵活性** | ❌ 需要知道合并策略 | ✅ 接收端决定 |
| **复杂度** | ❌ 增加发送端逻辑 | ✅ 简单 |

### 建议

```
┌─────────────────────────────────────────────────────────────────┐
│                         建议                                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 当前 Graph 场景: ❌ 不需要在 Outbox 合并                      │
│     - 顺序执行为主                                               │
│     - 消息是完整状态，不适合用简单合并器                          │
│     - 已在接收端 aggregate_state_messages 处理                   │
│                                                                 │
│  2. 如果需要合并（优化网络）:                                     │
│     - 使用 `last` 合并器（取最后一个状态）                        │
│     - 或自定义合并器用 maps:merge                               │
│                                                                 │
│  3. 合并器配置方式:                                              │
│     pregel:run(Graph, ComputeFn, #{                            │
│       combiner => last  %% 或自定义                             │
│     })                                                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 如果要为 Graph 定义专用合并器

```erlang
%% Graph 状态合并器
graph_state_combiner() ->
    fun(States) ->
        %% States = [{state, S1}, {state, S2}, ...]
        case States of
            [] -> undefined;
            [{state, S}] -> {state, S};
            Multiple ->
                %% 提取所有 state，合并
                AllStates = [S || {state, S} <- Multiple],
                MergedState = lists:foldl(
                    fun(S, Acc) ->
                        maps:merge(graph_state:to_map(Acc), graph_state:to_map(S))
                    end,
                    hd(AllStates),
                    tl(AllStates)
                ),
                {state, graph_state:new(MergedState)}
        end
    end.
```

---

## Outbox 消息路由流程

代码位置：`pregel_worker.erl:273-322`

```
┌─────────────────────────────────────────────────────────────────┐
│                   Outbox 消息路由流程                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 顶点计算完成后                                               │
│     Outbox = [{node_a, msg1}, {node_b, msg2}, {node_a, msg3}]  │
│                                                                 │
│  2. 应用合并器（可选）                                           │
│     apply_combiner(Outbox, Combiner)                           │
│                                                                 │
│  3. 按目标 Worker 分组                                          │
│     group_by_target_worker(Outbox, NumWorkers)                 │
│     #{                                                          │
│       0 => [{node_a, msg1}, {node_a, msg3}],  %% Worker 0      │
│       1 => [{node_b, msg2}]                   %% Worker 1      │
│     }                                                           │
│                                                                 │
│  4. 发送到各 Worker                                             │
│     - 本地 Worker: self() ! {local_messages, Messages}         │
│     - 远程 Worker: receive_messages(Pid, Messages)             │
│     - 未知 Worker: gen_server:cast(Master, {route_messages})   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 总结

| 问题 | 答案 |
|------|------|
| **Outbox 消息结构** | `[{vertex_id(), {state, GraphState}}]` |
| **是否应该合并** | 当前场景不需要，已在接收端处理 |
| **合并器支持** | 支持，但需要自定义适合 GraphState 的合并器 |
| **建议** | 保持当前实现，需要时可配置 `last` 合并器 |

## 相关文档

- `pregel_parallel_node_merge.md` - 并行节点状态合并机制
- `pregel_channel_mechanism_analysis.md` - Channel 机制分析
