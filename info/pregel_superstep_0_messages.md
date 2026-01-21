# Pregel 超级步 0 的消息发送

## 概述

在超级步 0 中，只有 `__start__` 节点被激活并发送消息，其他所有节点都处于未激活状态。

## 1. 只有 `__start__` 节点被激活

代码位置：`graph_compute.erl` 中的 `handle_start_node`

```erlang
handle_start_node(Ctx) ->
    Superstep = pregel:get_superstep(Ctx),
    VertexValue = pregel:get_vertex_value(Ctx),
    Activated = maps:get(activated, VertexValue, false),

    case Superstep =:= 0 andalso Activated of
        true ->
            %% 超级步 0 且被激活，执行启动逻辑
            execute_start_node(Ctx, VertexValue);
        false ->
            %% 非超级步 0 或未激活，投票停止
            pregel:vote_to_halt(Ctx)
    end.
```

### 激活条件

- `Superstep =:= 0`：必须是超级步 0
- `Activated =:= true`：必须被标记为激活

`__start__` 节点在图初始化时通过 `inject_initial_state` 被设置为 `activated => true`。

## 2. 发送的消息内容

`__start__` 节点发送 `{state, InitialState}` 消息给第一个真正的业务节点。

```erlang
%% send_to_next_nodes 发送消息
pregel:send_message(Ctx, TargetNode, {state, State})
```

### 消息格式

```erlang
{state, GraphState}
```

其中 `GraphState` 是 `graph_state:state()` 类型，包含：

| 字段 | 说明 |
|------|------|
| `messages` | 对话消息列表 |
| `full_messages` | 完整对话历史 |
| `system_prompt` | 系统提示词 |
| `tools` | 工具定义列表 |
| `max_iterations` | 最大迭代次数 |
| `iteration` | 当前迭代计数（初始为 0） |
| `scratchpad` | 中间步骤记录（初始为空） |
| `context` | 用户上下文数据 |
| `callbacks` | 回调函数映射 |
| `callback_meta` | 回调元数据 |

## 3. 超级步 0 消息流程图

```
超级步 0:

┌─────────────────────────────────────────────────────────────────┐
│                        激活的顶点                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  __start__ (activated = true)                                   │
│      │                                                          │
│      │ 1. 从 vertex.value.initial_state 获取初始状态            │
│      │                                                          │
│      │ 2. 发送消息: {state, InitialState}                       │
│      │                                                          │
│      ▼                                                          │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │  发送到第一个业务节点（如 llm_call）                         │  │
│  │  消息内容: {state, GraphState}                             │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│                        未激活的顶点                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  llm_call      ─→ 无消息，执行 vote_to_halt()                   │
│  execute_tools ─→ 无消息，执行 vote_to_halt()                   │
│  __end__       ─→ 无消息，执行 vote_to_halt()                   │
│  ...                                                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 4. 超级步 0 结束后的状态

| 顶点 | 收到消息 | 执行动作 | halted 状态 |
|------|---------|----------|-------------|
| `__start__` | 无（但 activated=true） | 发送初始状态 | true |
| `llm_call` | 无 | vote_to_halt | true |
| `execute_tools` | 无 | vote_to_halt | true |
| `__end__` | 无 | vote_to_halt | true |

## 5. 超级步 1 开始时

`llm_call` 节点会收到来自 `__start__` 的消息 `{state, InitialState}`，被激活并开始执行。

```
超级步 1:

__start__ 的消息到达 llm_call
        │
        ▼
┌───────────────────┐
│    llm_call       │ ← 收到 {state, InitialState}
│                   │
│ 1. 提取 state     │
│ 2. 调用 LLM       │
│ 3. 处理响应       │
│ 4. 发送新 state   │
└───────────────────┘
        │
        ▼
发送 {state, NewState} 到下一节点
```

## 6. 消息传递时序

```
时间 ──────────────────────────────────────────────────────────────→

超级步 0                              超级步 1
┌─────────────────────────────┐      ┌─────────────────────────────┐
│                             │      │                             │
│  __start__                  │      │  llm_call                   │
│     │                       │      │     │                       │
│     │ 执行计算              │      │     │ 收到消息              │
│     │                       │      │     │ {state, InitialState} │
│     ▼                       │      │     │                       │
│  发送消息 ─────────────────────────────→ │ 执行计算              │
│  {state, InitialState}      │      │     │                       │
│                             │      │     ▼                       │
│  vote_to_halt()             │      │  发送消息到下一节点         │
│                             │      │                             │
├─────────────────────────────┤      ├─────────────────────────────┤
│  其他顶点:                   │      │  其他顶点:                   │
│  vote_to_halt()             │      │  等待消息或 vote_to_halt   │
└─────────────────────────────┘      └─────────────────────────────┘
```

## 7. 总结

| 项目 | 说明 |
|------|------|
| **超级步 0 激活顶点** | 只有 `__start__` |
| **发送的消息格式** | `{state, GraphState}` |
| **消息目标** | 第一个业务节点（由边定义决定） |
| **其他顶点行为** | 无消息收到，执行 `vote_to_halt()` |
| **消息生效时机** | 超级步 1 开始时 |

## 8. 关键点

1. **延迟投递**：超级步 0 发送的消息在超级步 1 才被目标顶点接收
2. **单一激活源**：整个图的执行从 `__start__` 节点开始
3. **状态传递**：初始状态通过消息机制传递给第一个业务节点
4. **惰性执行**：未收到消息的顶点不会执行任何业务逻辑
