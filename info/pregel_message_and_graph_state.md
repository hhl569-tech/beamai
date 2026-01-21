# Pregel Message 与 Graph State 的关系

## 结论

**Pregel 中的 message 就是 graph 的 state。**

## 消息格式

消息是一个包装了 `graph_state` 的元组：

```erlang
{state, GraphState}
```

其中 `GraphState` 是 `graph_state:state()` 类型，本质上是一个 Map。

## 发送消息

节点执行完成后，将 state 发送给下一个节点。

代码位置：`graph_compute.erl:214-238`

```erlang
%% @doc 根据边定义发送状态到下一个节点
-spec send_to_next_nodes(pregel:context(), [graph_edge:edge()], graph_state:state()) -> pregel:context().
send_to_next_nodes(Ctx, [], State) ->
    %% 无出边时发送到 __end__ 节点
    pregel:send_message(Ctx, ?END_NODE, {state, State});
send_to_next_nodes(Ctx, Edges, State) ->
    lists:foldl(
        fun(Edge, AccCtx) ->
            case graph_edge:resolve(Edge, State) of
                {ok, TargetNode} when is_atom(TargetNode) ->
                    %% 发送 state 到目标节点
                    pregel:send_message(AccCtx, TargetNode, {state, State});
                {ok, TargetNodes} when is_list(TargetNodes) ->
                    %% 并行边：发送到多个目标节点
                    lists:foldl(
                        fun(Target, Acc) ->
                            pregel:send_message(Acc, Target, {state, State})
                        end,
                        AccCtx,
                        TargetNodes
                    );
                {error, _Reason} ->
                    AccCtx
            end
        end,
        Ctx,
        Edges
    ).
```

## 接收消息

节点接收消息时，从消息中提取 state。

代码位置：`graph_compute.erl:187-199`

```erlang
%% @doc 聚合多个状态消息
-spec aggregate_state_messages([term()]) -> {ok, graph_state:state()} | {error, no_state_messages}.
aggregate_state_messages(Messages) ->
    %% 从消息中提取所有 state
    States = [S || {state, S} <- Messages],
    case States of
        [] ->
            {error, no_state_messages};
        [SingleState] ->
            %% 只有一个 state，直接返回
            {ok, SingleState};
        MultipleStates ->
            %% 多个 state，合并后返回
            MergedState = merge_states(MultipleStates),
            {ok, MergedState}
    end.
```

## State 合并逻辑

当一个节点收到多条消息（来自多个前驱节点）时，会合并 state。

代码位置：`graph_compute.erl:201-212`

```erlang
%% @doc 合并多个状态
-spec merge_states([graph_state:state()]) -> graph_state:state().
merge_states([First | Rest]) ->
    lists:foldl(fun merge_two_states/2, First, Rest).

%% @doc 合并两个状态
-spec merge_two_states(graph_state:state(), graph_state:state()) -> graph_state:state().
merge_two_states(State1, State2) ->
    Data1 = graph_state:to_map(State1),
    Data2 = graph_state:to_map(State2),
    %% 使用 maps:merge，后者覆盖前者
    MergedData = maps:merge(Data1, Data2),
    graph_state:new(MergedData).
```

## 流程图

```
┌─────────────┐                              ┌─────────────┐
│   Node A    │                              │   Node B    │
│             │     {state, GraphState}      │             │
│ 执行计算    │ ────────────────────────────→│ 收到消息    │
│ 产生新State │        Pregel Message        │ 提取 State  │
│             │                              │ 继续执行    │
└─────────────┘                              └─────────────┘

并行节点合并示例：

┌─────────────┐
│   Node A    │──{state, StateA}──┐
└─────────────┘                   │
                                  ▼
                            ┌───────────┐
                            │  Node C   │
                            │           │
                            │ 合并State │
                            │ A + B     │
                            └───────────┘
                                  ▲
┌─────────────┐                   │
│   Node B    │──{state, StateB}──┘
└─────────────┘
```

## GraphState 内容

在 Agent 执行时，GraphState 通常包含以下字段：

| 字段 | 说明 |
|------|------|
| `messages` | 对话消息列表（可能已压缩） |
| `full_messages` | 完整对话历史 |
| `system_prompt` | 系统提示词 |
| `tools` | 工具定义列表 |
| `max_iterations` | 最大迭代次数 |
| `iteration` | 当前迭代计数 |
| `scratchpad` | 中间步骤记录 |
| `context` | 用户上下文数据 |
| `callbacks` | 回调函数映射 |
| `callback_meta` | 回调元数据 |

执行过程中还会动态添加：
- `finish_reason` - 结束原因
- `validated_content` - 验证后的结构化内容
- `interrupted` / `rejected` - 中断/拒绝标志

## 总结

| 概念 | 说明 |
|------|------|
| Pregel Message | `{state, GraphState}` 元组 |
| GraphState | `graph_state:state()` 类型，本质是 Map |
| 发送时机 | 节点执行完成后，根据边定义发送给下一节点 |
| 多消息处理 | 收到多条消息时，使用 `maps:merge` 合并 state |
| 合并策略 | 后者覆盖前者（相同 key 时） |

**Pregel 的消息传递机制本质上就是在图节点之间传递执行状态（graph_state）。**
