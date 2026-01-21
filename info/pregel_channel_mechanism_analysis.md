# Pregel 是否需要 Channel 机制分析

## 概述

本文档分析增加 `superstep_state` 后，是否需要像 LangGraph 那样的 Channel 机制来对最终状态进行合并。

## 两个概念的区别

| 概念 | 作用 | 关注点 |
|------|------|--------|
| **superstep_state** | 追踪顶点执行状态 | 哪些顶点成功/失败 |
| **Channel 机制** | 定义状态合并策略 | 多个状态如何合并 |

**它们是不同层面的问题，superstep_state 不需要 Channel 机制。**

## 当前的状态合并机制

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
    MergedData = maps:merge(Data1, Data2),  %% 后者覆盖前者
    graph_state:new(MergedData).
```

**当前策略：后者覆盖前者（Last Writer Wins）**

## 什么时候需要 Channel 机制？

### 场景分析

```
场景 1：顺序执行（当前主要场景）
─────────────────────────────────
__start__ → llm_call → execute_tools → __end__

只有一个前驱节点，不需要合并
✅ 当前实现足够


场景 2：简单并行（fan-out → fan-in）
───────────────────────────────────
              ┌─── Node A ───┐
__start__ ────┤              ├──→ Node C
              └─── Node B ───┘

Node C 收到两个状态，用 maps:merge 合并
✅ 当前实现基本够用（相同 key 后者覆盖）


场景 3：需要自定义合并（Channel 场景）
────────────────────────────────────
              ┌─── Node A: counter += 5 ───┐
__start__ ────┤                            ├──→ Node C
              └─── Node B: counter += 3 ───┘

期望结果: counter = 8（累加）
当前结果: counter = 3 或 5（覆盖）
❌ 需要 Channel 机制
```

## LangGraph Channel 类型

```python
# LangGraph 的三种 Channel 类型

# 1. LastValue - 后者覆盖（我们当前的实现）
class LastValue:
    def update(self, old, new):
        return new

# 2. Topic - 追加列表
class Topic:
    def update(self, old, new):
        return old + [new]

# 3. BinaryOperatorAggregate - 自定义操作
class BinaryOperatorAggregate:
    def __init__(self, operator):
        self.operator = operator  # 如 operator.add

    def update(self, old, new):
        return self.operator(old, new)
```

## 是否需要 Channel？

### 决策树

```
需要合并多个状态？
    │
    ├─ 否 → ✅ 不需要 Channel（顺序执行）
    │
    └─ 是 → 合并策略是什么？
              │
              ├─ 后者覆盖 → ✅ 当前 maps:merge 足够
              │
              ├─ 追加列表 → ⚠️ 需要 Topic Channel
              │
              └─ 自定义操作 → ⚠️ 需要 BinaryOperator Channel
```

### superstep_state 与 Channel 的关系

```
┌─────────────────────────────────────────────────────────────────┐
│                    superstep_state                              │
├─────────────────────────────────────────────────────────────────┤
│  作用: 追踪顶点执行状态                                          │
│  内容: #{vertex_id => #{status, vertex, outbox, error}}        │
│  位置: Worker 进程内                                            │
│  用途: 错误重试、进度追踪                                        │
│                                                                 │
│  ❌ 不涉及状态合并策略                                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ 独立的
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Channel 机制                                  │
├─────────────────────────────────────────────────────────────────┤
│  作用: 定义状态合并策略                                          │
│  内容: #{channel_name => {type, reducer}}                       │
│  位置: 图定义层                                                  │
│  用途: 多状态合并                                                │
│                                                                 │
│  只在并行节点合并时需要                                           │
└─────────────────────────────────────────────────────────────────┘
```

## 结论

| 问题 | 回答 |
|------|------|
| **增加 superstep_state 需要 Channel 吗？** | ❌ 不需要，它们解决不同问题 |
| **当前实现需要 Channel 吗？** | 取决于使用场景 |
| **顺序执行** | ✅ 不需要 |
| **简单并行（后者覆盖）** | ✅ 不需要，maps:merge 足够 |
| **复杂并行（累加、追加等）** | ⚠️ 需要 Channel |

## 如果未来需要 Channel，如何实现？

### 1. 定义 Channel 类型

```erlang
-type channel_type() :: last_value      %% 后者覆盖（当前默认）
                      | topic           %% 追加列表
                      | {reducer, fun()}. %% 自定义
```

### 2. 在图定义中指定 Channel

```erlang
Graph = #{
    channels => #{
        messages => topic,              %% 消息列表追加
        counter => {reducer, fun erlang:'+'/2},  %% 计数器累加
        result => last_value            %% 结果覆盖
    }
}.
```

### 3. 修改合并逻辑

```erlang
merge_two_states(State1, State2, Channels) ->
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

## 建议实施路径

```
┌─────────────────────────────────────────────────────────────────┐
│                      实施优先级                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. 当前阶段：不需要 Channel                                     │
│     - maps:merge 足够                                           │
│     - 主要是顺序执行场景                                         │
│                                                                 │
│  2. superstep_state：独立实现                                    │
│     - 不依赖 Channel                                            │
│     - 解决错误重试问题                                           │
│                                                                 │
│  3. 未来扩展：按需引入 Channel                                   │
│     - 当需要复杂并行模式时                                       │
│     - 当需要自定义合并策略时                                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 总结

| 功能 | 是否需要 | 说明 |
|------|---------|------|
| superstep_state | ✅ 需要 | 解决错误追踪和重试 |
| Channel 机制 | ⚠️ 暂不需要 | 当前 maps:merge 足够 |
| 两者关系 | 独立 | 解决不同层面问题 |
