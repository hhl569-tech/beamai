# Graph Channel 合并策略设计

## 核心定义

1. **context** - 用户信息，由用户传入，使用用户自定义函数合并
2. **messages** - 当前对话消息，可能被压缩，使用 block_append
3. **full_messages** - 完整历史消息，不压缩，使用 block_append

---

## Channel 合并策略

```
┌─────────────────────────────────────────────────────────────────┐
│                    Channel 合并策略                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  字段            策略              说明                          │
│  ─────────────────────────────────────────────────────────────  │
│  context         custom_fn        用户自定义合并函数             │
│  messages        block_append     整块追加（可被压缩）           │
│  full_messages   block_append     整块追加（完整历史）           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Block Append 策略

### 定义

- 单个节点产生的消息保持连续，不被其他并行节点的消息打断
- 两个并行节点之间的消息块顺序可以是任意的

### 示意图

```
┌─────────────────────────────────────────────────────────────────┐
│                    Block Append 示意                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  初始: messages = [M0]                                          │
│                                                                 │
│  并行执行:                                                       │
│  ┌─────────────┐              ┌─────────────┐                   │
│  │   Node A    │              │   Node B    │                   │
│  │             │              │             │                   │
│  │ 产生消息:   │              │ 产生消息:   │                   │
│  │ [A1, A2]    │              │ [B1, B2]    │                   │
│  └──────┬──────┘              └──────┬──────┘                   │
│         │                            │                          │
│         └──────────┬─────────────────┘                          │
│                    ▼                                            │
│  合并结果 (两种都可接受):                                         │
│                                                                 │
│  ✅ [M0, A1, A2, B1, B2]   Node A 的块在前                      │
│  ✅ [M0, B1, B2, A1, A2]   Node B 的块在前                      │
│                                                                 │
│  ❌ [M0, A1, B1, A2, B2]   消息交错（不允许！）                  │
│                                                                 │
│  关键: 单个节点的消息保持连续，不被其他节点打断                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 为什么这样设计

| 特点 | 原因 |
|------|------|
| **节点内消息连续** | 保证节点内消息的逻辑完整性 |
| **并行节点间可乱序** | 并行执行本质上没有确定顺序 |
| **不交错** | 避免消息语义被破坏 |

---

## messages vs full_messages

```
┌─────────────────────────────────────────────────────────────────┐
│              messages 与 full_messages 的关系                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  full_messages (完整历史):                                       │
│  [M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, ...]                │
│   │   │   │   │   │   │   │   │   │   │                        │
│   └───┴───┴───┴───┴───┘   └───┴───┴───┴───→ 全部保留           │
│                                                                 │
│  messages (工作集，可压缩):                                       │
│  [Summary("M1-M6的摘要"), M7, M8, M9, M10]                       │
│   │                       │   │   │   │                        │
│   └─ 压缩的历史           └───┴───┴───┴───→ 最近的消息          │
│                                                                 │
│  压缩时机: 当 messages 超过某个阈值（如 token 数量）              │
│  压缩方式: 保留最近 N 条，其余用摘要替代                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

| 字段 | 特点 | 用途 |
|------|------|------|
| **messages** | 可压缩，工作集 | 发送给 LLM，适应上下文窗口限制 |
| **full_messages** | 不压缩，完整 | 审计、恢复、历史查询 |

---

## context 自定义合并

### 为什么使用自定义函数

- context 包含用户特定的业务数据
- 不同应用可能有不同的 context 结构
- 只有用户最了解如何正确合并自己的 context

### 示例

```erlang
%% 用户定义的 context 合并函数
ContextMergeFn = fun(Ctx1, Ctx2) ->
    #{
        %% 用户 ID 保持不变
        user_id => maps:get(user_id, Ctx1),

        %% 会话信息取最新
        session => maps:get(session, Ctx2, maps:get(session, Ctx1, #{})),

        %% 计数器累加
        request_count => maps:get(request_count, Ctx1, 0) +
                        maps:get(request_count, Ctx2, 0),

        %% 标签合并
        tags => lists:usort(
            maps:get(tags, Ctx1, []) ++
            maps:get(tags, Ctx2, [])
        )
    }
end.
```

---

## 类型定义

```erlang
%% Channel 策略类型
-type channel_strategy() ::
    {custom, fun((term(), term()) -> term())} |  %% context 用
    block_append.                                 %% messages/full_messages 用

%% Graph Channel 配置
-type graph_channel_config() :: #{
    context := {custom, fun((context(), context()) -> context())},
    messages := block_append,
    full_messages := block_append
}.

%% 默认配置（context 需要用户提供）
-spec default_config(fun()) -> graph_channel_config().
default_config(ContextMergeFn) ->
    #{
        context => {custom, ContextMergeFn},
        messages => block_append,
        full_messages => block_append
    }.
```

---

## 实现

```erlang
%% 合并两个 GraphState
-spec merge_graph_states(state(), state(), graph_channel_config()) -> state().
merge_graph_states(State1, State2, Config) ->
    #{
        context => apply_context_merge(
            maps:get(context, Config),
            maps:get(context, State1, #{}),
            maps:get(context, State2, #{})
        ),
        messages => block_append(
            maps:get(messages, State1, []),
            maps:get(messages, State2, [])
        ),
        full_messages => block_append(
            maps:get(full_messages, State1, []),
            maps:get(full_messages, State2, [])
        )
    }.

%% Block append - 将整个列表作为块追加
-spec block_append(list(), list()) -> list().
block_append(List1, List2) ->
    List1 ++ List2.  %% List2 作为整块追加到 List1 后面

%% 应用自定义 context 合并
-spec apply_context_merge({custom, fun()}, context(), context()) -> context().
apply_context_merge({custom, Fun}, Ctx1, Ctx2) ->
    Fun(Ctx1, Ctx2).
```

---

## 设计合理性分析

| 设计决策 | 合理性 | 原因 |
|----------|--------|------|
| **context 用自定义函数** | ✅ | 业务特定，用户最清楚如何合并 |
| **messages/full_messages 用 block_append** | ✅ | 保持节点内消息逻辑完整 |
| **并行节点间顺序不确定** | ✅ | 并行执行本质如此，且不影响语义 |
| **messages 可压缩** | ✅ | 适应 LLM 上下文窗口限制 |
| **full_messages 完整** | ✅ | 审计和恢复需要 |
| **只关注 3 个字段** | ✅ | 简化设计，聚焦核心数据流 |

---

## 总结

```
┌─────────────────────────────────────────────────────────────────┐
│                    Graph Channel 设计总结                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  核心原则:                                                       │
│  1. 只关注 3 个核心字段: context, messages, full_messages       │
│  2. context 使用用户自定义函数，灵活适配不同业务                  │
│  3. messages/full_messages 使用 block_append，保证节点内连续     │
│  4. 并行节点间顺序可以任意，但节点内消息不交错                    │
│  5. messages 可压缩适应 LLM，full_messages 保留完整历史          │
│                                                                 │
│  优点:                                                           │
│  • 简单: 只需关注 3 个字段                                       │
│  • 灵活: context 支持自定义合并                                  │
│  • 正确: block_append 保证消息逻辑完整                           │
│  • 实用: 区分 messages 和 full_messages 满足不同需求             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 相关文档

- `pregel_channel_placement.md` - Channel 机制放置分析
- `pregel_channel_mechanism_analysis.md` - Channel 机制详细分析
- `pregel_parallel_node_merge.md` - 并行节点状态合并机制
