# Channel 机制放置位置分析

## 问题

如果我们想实现 Channel 机制对 GraphState 进行合并，应该将它放到哪一层？Graph 层还是 Pregel 层？

## 两层的职责对比

```
┌─────────────────────────────────────────────────────────────────┐
│                    Channel 机制放置位置分析                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                      Graph 层                            │   │
│  │                                                          │   │
│  │  • 理解 GraphState 结构                                  │   │
│  │  • aggregate_state_messages 已在此处                     │   │
│  │  • 知道 messages, context, tools 等字段语义              │   │
│  │  • 应用特定的合并逻辑                                     │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              │ 调用                             │
│                              ▼                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                     Pregel 层                            │   │
│  │                                                          │   │
│  │  • 通用 BSP 框架                                         │   │
│  │  • vertex.value 是黑盒（不理解内部结构）                  │   │
│  │  • Combiner 只处理简单值（sum, min, max）                │   │
│  │  • 不应该知道 GraphState 的存在                          │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 建议：放在 Graph 层

### 原因分析

| 考虑因素 | Graph 层 | Pregel 层 |
|----------|----------|-----------|
| **理解 GraphState** | ✅ 理解字段语义 | ❌ 黑盒 |
| **已有合并逻辑** | ✅ `aggregate_state_messages` | ❌ 只有简单 Combiner |
| **职责分离** | ✅ 应用逻辑 | ❌ 框架不应知道应用细节 |
| **配置灵活性** | ✅ 可按节点/边配置 | ❌ 全局配置 |
| **复用性** | ✅ 不影响 Pregel 通用性 | ❌ 耦合到特定应用 |

---

## 当前合并位置

代码位置：`graph_compute.erl:188-199`

```erlang
aggregate_state_messages(Messages) ->
    States = [S || {state, S} <- Messages],
    case States of
        [] -> {error, no_state_messages};
        [Single] -> {ok, Single};
        Multiple ->
            MergedState = merge_states(Multiple),  %% ← 在这里应用 Channel
            {ok, MergedState}
    end.
```

---

## 建议的 Channel 实现架构

### 新模块：graph_channel.erl

```erlang
%% graph_channel.erl（新模块，Graph 层）

-module(graph_channel).

-type channel_type() ::
    last_value |           %% 后者覆盖（默认）
    first_value |          %% 保留首个
    append |               %% 列表追加
    {reducer, fun()} |     %% 自定义规约
    {merger, fun()}.       %% 自定义合并

-type channel_config() :: #{
    atom() => channel_type()  %% 字段名 => 合并策略
}.

%% 默认 Channel 配置
-spec default_channels() -> channel_config().
default_channels() ->
    #{
        messages => append,           %% 消息列表追加
        full_messages => append,      %% 完整消息追加
        context => last_value,        %% 上下文覆盖
        tools => last_value,          %% 工具定义覆盖
        iteration => {reducer, fun erlang:max/2},  %% 取最大迭代数
        scratchpad => {merger, fun maps:merge/2}   %% 深度合并
    }.

%% 应用 Channel 合并
-spec merge_with_channels(graph_state:state(), graph_state:state(),
                          channel_config()) -> graph_state:state().
merge_with_channels(State1, State2, Channels) ->
    Data1 = graph_state:to_map(State1),
    Data2 = graph_state:to_map(State2),
    MergedData = maps:fold(
        fun(Key, Value2, Acc) ->
            Value1 = maps:get(Key, Acc, undefined),
            ChannelType = maps:get(Key, Channels, last_value),
            MergedValue = apply_channel(ChannelType, Value1, Value2),
            maps:put(Key, MergedValue, Acc)
        end,
        Data1,
        Data2
    ),
    graph_state:new(MergedData).

%% 应用单个 Channel 策略
apply_channel(last_value, _Old, New) -> New;
apply_channel(first_value, undefined, New) -> New;
apply_channel(first_value, Old, _New) -> Old;
apply_channel(append, undefined, New) -> [New];
apply_channel(append, Old, New) when is_list(Old), is_list(New) -> Old ++ New;
apply_channel(append, Old, New) when is_list(Old) -> Old ++ [New];
apply_channel({reducer, Fun}, undefined, New) -> New;
apply_channel({reducer, Fun}, Old, New) -> Fun(Old, New);
apply_channel({merger, Fun}, Old, New) -> Fun(Old, New).
```

### 修改 graph_compute.erl

```erlang
%% 修改 merge_states 使用 Channel
merge_states(States) ->
    merge_states(States, graph_channel:default_channels()).

merge_states([First | Rest], Channels) ->
    lists:foldl(
        fun(State, Acc) ->
            graph_channel:merge_with_channels(Acc, State, Channels)
        end,
        First,
        Rest
    ).
```

---

## 可选：支持边级别 Channel 配置

```erlang
%% 在 Edge 定义中指定 Channel
Edge = #{
    from => node_a,
    to => node_c,
    channels => #{
        counter => {reducer, fun erlang:'+'/2}  %% 这条边的特殊合并规则
    }
}.

%% 在 aggregate_state_messages 中使用边的 Channel 配置
aggregate_state_messages(Messages, EdgeChannels) ->
    %% 获取当前边的 Channel 配置
    Channels = maps:merge(
        graph_channel:default_channels(),
        EdgeChannels
    ),
    %% 应用合并
    ...
```

---

## 架构层次图

```
┌─────────────────────────────────────────────────────────────────┐
│                      架构层次                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Graph 定义层                                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  graph_definition.erl                                   │   │
│  │  • 定义节点、边                                          │   │
│  │  • 可选：指定 Channel 配置                               │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  Graph 执行层                                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  graph_compute.erl + graph_channel.erl (新)             │   │
│  │  • compute_fn 计算函数                                   │   │
│  │  • aggregate_state_messages 使用 Channel 合并            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  Pregel 框架层                                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  pregel_*.erl                                           │   │
│  │  • 通用 BSP 执行                                         │   │
│  │  • 不感知 GraphState 和 Channel                          │   │
│  │  • Combiner 保持简单（可选，用于网络优化）                 │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Pregel Combiner vs Graph Channel

```
┌─────────────────────────────────────────────────────────────────┐
│              Pregel Combiner vs Graph Channel                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Pregel Combiner                    Graph Channel               │
│  ──────────────────                 ─────────────               │
│  位置: 发送端（Outbox）             位置: 接收端（Messages）     │
│  目的: 网络优化                     目的: 语义正确合并           │
│  粒度: 整条消息                     粒度: 字段级别               │
│  类型: 简单值（sum, min）           类型: 复杂结构（map, list）  │
│  配置: 全局                         配置: 可按节点/边            │
│                                                                 │
│  两者互补:                                                       │
│  1. Combiner 在发送前减少消息数量（可选）                        │
│  2. Channel 在接收时正确合并字段（必要）                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 默认 Channel 配置说明

| 字段 | Channel 类型 | 说明 |
|------|--------------|------|
| `messages` | `append` | 对话消息列表追加 |
| `full_messages` | `append` | 完整历史追加 |
| `context` | `last_value` | 上下文覆盖 |
| `tools` | `last_value` | 工具定义覆盖 |
| `iteration` | `{reducer, max}` | 取最大迭代数 |
| `scratchpad` | `{merger, maps:merge}` | 中间状态深度合并 |
| `result` | `last_value` | 结果覆盖 |

---

## 总结

| 问题 | 答案 |
|------|------|
| **放哪一层？** | Graph 层 |
| **为什么？** | Channel 是应用特定逻辑，Pregel 应保持通用 |
| **修改哪里？** | 新增 `graph_channel.erl`，修改 `graph_compute.erl` 的 `merge_states` |
| **Pregel Combiner？** | 保留，用于网络优化（可选），与 Channel 互补 |
| **配置方式？** | 默认配置 + 可选的节点/边级别覆盖 |

---

## 相关文档

- `pregel_channel_mechanism_analysis.md` - Channel 机制详细分析
- `pregel_parallel_node_merge.md` - 并行节点状态合并机制
- `pregel_worker_outbox_analysis.md` - Outbox 消息分析
