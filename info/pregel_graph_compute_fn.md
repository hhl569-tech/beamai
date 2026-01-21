# Graph 的 Pregel 计算函数

## 概述

计算函数是 Pregel 执行的核心，定义了每个顶点在每个超级步中如何计算。

## 计算函数定义

代码位置：`graph_compute.erl:52-67`

```erlang
-spec compute_fn() -> pregel:compute_fn().
compute_fn() ->
    fun(Ctx) ->
        VertexId = pregel:get_vertex_id(Ctx),
        VertexValue = pregel:get_vertex_value(Ctx),
        Messages = pregel:get_messages(Ctx),
        Superstep = pregel:get_superstep(Ctx),

        case VertexId of
            '__start__' ->
                handle_start_node(Ctx, VertexValue, Superstep);
            '__end__' ->
                handle_end_node(Ctx, Messages);
            _ ->
                handle_regular_node(Ctx, VertexValue, Messages)
        end
    end.
```

## 计算函数结构图

```
┌─────────────────────────────────────────────────────────────────┐
│                    compute_fn() 计算函数                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  输入: Context (Ctx)                                            │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ vertex: 当前顶点                                         │   │
│  │ messages: 收到的消息列表                                  │   │
│  │ superstep: 当前超步编号                                   │   │
│  │ outbox: 待发送消息（初始为空）                            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │              根据 VertexId 分发处理                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│         │                    │                    │             │
│         ▼                    ▼                    ▼             │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐       │
│  │ __start__   │     │  __end__    │     │ 普通节点     │       │
│  │             │     │             │     │             │       │
│  │ handle_     │     │ handle_     │     │ handle_     │       │
│  │ start_node  │     │ end_node    │     │ regular_node│       │
│  └─────────────┘     └─────────────┘     └─────────────┘       │
│                                                                 │
│  输出: 更新后的 Context                                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ vertex: 可能更新了 value.result                          │   │
│  │ outbox: 可能添加了发送给其他顶点的消息                     │   │
│  │ halted: 可能设置为 true                                  │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 三种节点处理逻辑

### 1. `__start__` 节点处理

代码位置：`graph_compute.erl:123-138`

```erlang
handle_start_node(Ctx, VertexValue, 0) ->
    case maps:get(activated, VertexValue, false) of
        true  -> execute_start_node(Ctx, VertexValue);  %% 执行并发送状态
        false -> pregel:vote_to_halt(Ctx)               %% 未激活，停止
    end;
handle_start_node(Ctx, _VertexValue, _Superstep) ->
    pregel:vote_to_halt(Ctx).  %% 非超步0，停止

execute_start_node(Ctx, VertexValue) ->
    Node = maps:get(node, VertexValue),
    State = maps:get(initial_state, VertexValue),
    Edges = maps:get(edges, VertexValue, []),
    finish_node_execution(Ctx, VertexValue, Node, State, Edges).
```

**执行条件：**
- 超步 = 0
- `activated = true`

**行为：**
- 从 `initial_state` 获取初始状态
- 执行节点逻辑
- 发送状态到下一节点

### 2. `__end__` 节点处理

代码位置：`graph_compute.erl:141-153`

```erlang
handle_end_node(Ctx, []) ->
    pregel:vote_to_halt(Ctx);  %% 无消息，停止
handle_end_node(Ctx, Messages) ->
    case aggregate_state_messages(Messages) of
        {ok, State} ->
            VertexValue = pregel:get_vertex_value(Ctx),
            %% 保存最终状态到 result
            NewValue = VertexValue#{result => {ok, State}},
            Ctx1 = pregel:set_value(Ctx, NewValue),
            pregel:vote_to_halt(Ctx1);
        {error, _} ->
            pregel:vote_to_halt(Ctx)
    end.
```

**行为：**
- 收集所有前驱节点发来的状态
- 合并状态（如果有多个）
- 保存到 `result` 字段
- 投票停止

### 3. 普通节点处理

代码位置：`graph_compute.erl:156-167`

```erlang
handle_regular_node(Ctx, _VertexValue, []) ->
    pregel:vote_to_halt(Ctx);  %% 无消息，停止
handle_regular_node(Ctx, VertexValue, Messages) ->
    case aggregate_state_messages(Messages) of
        {ok, State} ->
            Node = maps:get(node, VertexValue),
            Edges = maps:get(edges, VertexValue, []),
            finish_node_execution(Ctx, VertexValue, Node, State, Edges);
        {error, no_state_messages} ->
            pregel:vote_to_halt(Ctx)
    end.
```

**执行条件：**
- 收到消息

**行为：**
- 从消息中提取状态
- 执行节点逻辑
- 发送结果到下一节点

## 节点执行核心逻辑

代码位置：`graph_compute.erl:172-181`

```erlang
finish_node_execution(Ctx, VertexValue, Node, State, Edges) ->
    case graph_node:execute(Node, State) of
        {ok, NewState} ->
            %% 成功：发送状态到下一节点
            Ctx1 = send_to_next_nodes(Ctx, Edges, NewState),
            NewValue = VertexValue#{result => {ok, NewState}},
            pregel:vote_to_halt(pregel:set_value(Ctx1, NewValue));
        {error, Reason} ->
            %% 失败：保存错误，不发送消息
            NewValue = VertexValue#{result => {error, Reason}},
            pregel:vote_to_halt(pregel:set_value(Ctx, NewValue))
    end.
```

**执行步骤：**
1. 调用 `graph_node:execute(Node, State)` 执行节点逻辑
2. 成功时：发送新状态到下一节点，保存结果
3. 失败时：保存错误，不发送消息
4. 调用 `vote_to_halt()` 投票停止

## 执行流程示例

```
超级步 0:
┌─────────────┐
│ __start__   │
│ activated=T │
└──────┬──────┘
       │ 1. execute_start_node()
       │ 2. graph_node:execute(Node, InitialState)
       │ 3. send_to_next_nodes() → {state, S0}
       │ 4. vote_to_halt()
       ▼

超级步 1:
┌─────────────┐
│  llm_call   │ ← 收到 {state, S0}
└──────┬──────┘
       │ 1. aggregate_state_messages()
       │ 2. graph_node:execute(Node, S0)
       │ 3. send_to_next_nodes() → {state, S1}
       │ 4. vote_to_halt()
       ▼

超级步 2:
┌─────────────────┐
│ execute_tools   │ ← 收到 {state, S1}
└──────┬──────────┘
       │ 1. aggregate_state_messages()
       │ 2. graph_node:execute(Node, S1)
       │ 3. send_to_next_nodes() → {state, S2}
       │ 4. vote_to_halt()
       ▼

超级步 3:
┌─────────────┐
│  __end__    │ ← 收到 {state, S2}
└──────┬──────┘
       │ 1. aggregate_state_messages()
       │ 2. result = {ok, S2}
       │ 3. vote_to_halt()
       ▼

执行完成，从 __end__.result 提取最终状态
```

## 计算函数特点

| 特点 | 说明 |
|------|------|
| **无状态** | 计算函数本身不持有状态 |
| **依赖 vertex.value** | 所有信息从顶点值中获取 |
| **统一入口** | 所有顶点使用同一个计算函数 |
| **分发模式** | 根据 VertexId 分发到不同处理逻辑 |

## vertex_value 结构

```erlang
-type vertex_value() :: #{
    node := graph_node:graph_node(),     %% 节点定义（包含执行函数）
    edges := [graph_edge:edge()],        %% 出边定义
    result := undefined | {ok, state()} | {error, term()},  %% 执行结果
    initial_state => graph_state:state(),%% 仅 __start__ 有
    activated => boolean()               %% 仅 __start__ 有
}.
```

## 关键操作

| 操作 | 函数 | 说明 |
|------|------|------|
| **获取顶点信息** | `pregel:get_vertex_id/1` | 获取顶点 ID |
| **获取顶点值** | `pregel:get_vertex_value/1` | 获取 vertex.value |
| **获取消息** | `pregel:get_messages/1` | 获取收到的消息列表 |
| **设置顶点值** | `pregel:set_value/2` | 更新 vertex.value |
| **发送消息** | `pregel:send_message/3` | 发送消息给其他顶点 |
| **投票停止** | `pregel:vote_to_halt/1` | 标记顶点为停止状态 |

## 总结

| 组件 | 说明 |
|------|------|
| **计算函数** | `graph_compute:compute_fn/0` |
| **类型** | `fun((context()) -> context())` |
| **特点** | 无状态，依赖 vertex.value 中的信息 |
| **分发逻辑** | 根据 VertexId 分发到不同处理函数 |
| **核心操作** | 执行节点、发送消息、保存结果、vote_to_halt |
| **节点类型** | `__start__`、`__end__`、普通节点 |
