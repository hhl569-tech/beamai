# Pregel 并行节点状态合并机制

## 问题

如果两个并行节点在不同的 Worker 中修改了相同的数据，我们是如何合并的？

## 并行节点执行场景

```
              Worker 0                    Worker 1
              ┌─────────┐                ┌─────────┐
              │ Node A  │                │ Node B  │
              │         │                │         │
              │ 修改:   │                │ 修改:   │
              │ counter │                │ counter │
              │ = 5     │                │ = 3     │
              └────┬────┘                └────┬────┘
                   │                          │
                   │ {state, StateA}          │ {state, StateB}
                   │                          │
                   └──────────┬───────────────┘
                              │
                              ▼
                        ┌───────────┐
                        │  Node C   │  ← 收到两条消息
                        │  (inbox)  │
                        └───────────┘
```

## 合并发生在哪里？

**合并发生在接收节点（Node C），不是在 Worker 层面。**

## 合并流程详解

### 1. 接收节点收到多条消息

代码位置：`graph_compute.erl:156-167`

```erlang
%% handle_regular_node 被调用时，Messages 包含来自多个前驱的消息
handle_regular_node(Ctx, VertexValue, Messages) ->
    case aggregate_state_messages(Messages) of  %% ← 在这里合并
        {ok, State} ->
            %% 使用合并后的 State 执行节点逻辑
            Node = maps:get(node, VertexValue),
            Edges = maps:get(edges, VertexValue, []),
            finish_node_execution(Ctx, VertexValue, Node, State, Edges);
        {error, no_state_messages} ->
            pregel:vote_to_halt(Ctx)
    end.
```

### 2. 聚合状态消息

代码位置：`graph_compute.erl:188-199`

```erlang
aggregate_state_messages(Messages) ->
    %% 从消息中提取所有 state
    States = [S || {state, S} <- Messages],
    case States of
        [] ->
            {error, no_state_messages};
        [SingleState] ->
            %% 只有一个，直接返回
            {ok, SingleState};
        MultipleStates ->
            %% 多个状态，需要合并
            MergedState = merge_states(MultipleStates),
            {ok, MergedState}
    end.
```

### 3. 合并多个状态

代码位置：`graph_compute.erl:201-212`

```erlang
merge_states([First | Rest]) ->
    lists:foldl(fun merge_two_states/2, First, Rest).

merge_two_states(State1, State2) ->
    Data1 = graph_state:to_map(State1),
    Data2 = graph_state:to_map(State2),
    MergedData = maps:merge(Data1, Data2),  %% 后者覆盖前者！
    graph_state:new(MergedData).
```

## 具体合并示例

```
并行执行场景:

┌─────────────────┐              ┌─────────────────┐
│     Node A      │              │     Node B      │
│                 │              │                 │
│ State = #{      │              │ State = #{      │
│   counter => 5, │              │   counter => 3, │
│   from_a => ok  │              │   from_b => ok  │
│ }               │              │ }               │
└────────┬────────┘              └────────┬────────┘
         │                                │
         │ {state, StateA}                │ {state, StateB}
         │                                │
         └────────────┬───────────────────┘
                      │
                      ▼
               ┌─────────────┐
               │   Node C    │
               │             │
               │ Messages = [│
               │   {state, StateA},
               │   {state, StateB}
               │ ]           │
               └─────────────┘
                      │
                      │ aggregate_state_messages()
                      │ merge_states([StateA, StateB])
                      ▼
               ┌─────────────────────────────┐
               │ MergedState = #{            │
               │   counter => 3 或 5,  ← ⚠️  │
               │   from_a => ok,             │
               │   from_b => ok              │
               │ }                           │
               └─────────────────────────────┘
```

## 当前合并策略：maps:merge

```
┌─────────────────────────────────────────────────────────────────┐
│                    maps:merge 的行为                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  maps:merge(#{a => 1, b => 2}, #{b => 3, c => 4})              │
│                                                                 │
│  结果: #{a => 1, b => 3, c => 4}                                │
│                                                                 │
│  规则: 相同 key 时，第二个参数覆盖第一个                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 合并顺序由什么决定？

```
消息到达 inbox 的顺序:

Worker 0 (Node A)                    Worker 1 (Node B)
      │                                    │
      │ 发送消息到 Node C                   │ 发送消息到 Node C
      │ (可能先到达)                        │ (可能后到达)
      │                                    │
      └──────────────┬─────────────────────┘
                     │
                     ▼
              Node C 的 inbox

inbox = #{
    node_c => [
        {state, StateA},  %% 先到达的在前
        {state, StateB}   %% 后到达的在后
    ]
}

合并顺序: fold 从左到右
merge_states([StateA, StateB])
  → merge_two_states(StateA, StateB)
  → maps:merge(StateA, StateB)
  → StateB 的值覆盖 StateA 的相同 key
```

## 当前实现的问题

| 问题 | 说明 |
|------|------|
| **不确定性** | 相同 key 的合并结果取决于消息顺序 |
| **数据丢失** | 一个节点的修改会被另一个覆盖 |
| **无法累加** | counter=5 + counter=3 应该=8，但当前只能取其一 |
| **无法追加** | 列表无法合并，只能覆盖 |

## 什么情况下当前实现没问题？

### 场景 1: 并行节点修改不同的 key ✅

```
Node A: #{result_a => "xxx"}
Node B: #{result_b => "yyy"}

合并后: #{result_a => "xxx", result_b => "yyy"}

没问题，不同 key 正常合并
```

### 场景 2: 顺序执行（非并行）✅

```
Node A → Node B → Node C

每个节点只收到一条消息，不需要合并
没问题
```

### 场景 3: 并行节点修改相同的 key ⚠️

```
Node A: #{counter => 5}
Node B: #{counter => 3}

合并后: #{counter => ?}

有问题，结果不确定，取决于消息到达顺序
```

## 消息顺序的不确定性来源

```
┌─────────────────────────────────────────────────────────────────┐
│                 影响消息顺序的因素                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 网络延迟                                                    │
│     - 跨 Worker 消息需要网络传输                                 │
│     - 本地 Worker 消息更快                                       │
│                                                                 │
│  2. Erlang 调度                                                 │
│     - Worker 进程的调度顺序                                      │
│     - 消息队列处理顺序                                           │
│                                                                 │
│  3. 计算时间                                                    │
│     - Node A 和 Node B 的执行时间不同                            │
│     - 先完成的先发送消息                                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 解决方案

如果需要支持复杂的并行合并，需要引入 **Channel 机制**：

```erlang
%% 1. 定义 Channel 类型
-type channel_type() :: last_value           %% 后者覆盖（当前默认）
                      | topic                %% 追加列表
                      | {reducer, fun()}.    %% 自定义合并函数

%% 2. 在图定义中指定 Channel
Graph = #{
    channels => #{
        counter => {reducer, fun erlang:'+'/2},  %% 累加: 5 + 3 = 8
        results => topic,                         %% 追加: [a] ++ [b]
        status => last_value                      %% 覆盖（默认）
    }
}.

%% 3. 合并时根据 Channel 类型处理
merge_with_channels(State1, State2, Channels) ->
    maps:fold(
        fun(Key, Value2, Acc) ->
            Value1 = maps:get(Key, Acc, undefined),
            ChannelType = maps:get(Key, Channels, last_value),
            MergedValue = apply_channel(ChannelType, Value1, Value2),
            maps:put(Key, MergedValue, Acc)
        end,
        State1,
        State2
    ).

apply_channel(last_value, _Old, New) -> New;
apply_channel(topic, Old, New) when is_list(Old) -> Old ++ [New];
apply_channel(topic, undefined, New) -> [New];
apply_channel({reducer, Fun}, Old, New) -> Fun(Old, New).
```

## 总结

| 问题 | 回答 |
|------|------|
| **合并在哪发生？** | 接收节点的 `aggregate_state_messages` |
| **合并策略？** | `maps:merge`，后者覆盖前者 |
| **不同 key？** | ✅ 正常合并，无问题 |
| **相同 key？** | ⚠️ 顺序不确定，结果不可预测 |
| **需要 Channel？** | 如果有相同 key 的复杂合并需求，需要 |

## 建议

| 场景 | 建议 |
|------|------|
| **当前主要场景（顺序执行）** | 无需修改 |
| **并行节点修改不同 key** | 无需修改 |
| **并行节点修改相同 key** | 设计时避免，或引入 Channel |
| **需要累加/追加** | 引入 Channel 机制 |

详细的 Channel 机制设计参考：`pregel_channel_mechanism_analysis.md`
