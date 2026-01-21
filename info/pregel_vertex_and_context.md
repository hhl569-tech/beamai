# Pregel Vertex 结构与计算函数 Context

## 概述

本文档从纯 Pregel 层面分析 Vertex 结构和计算函数的输入，将上层的 GraphState 视为黑盒。

## 1. Vertex 结构

代码位置：`pregel_vertex.erl:43-48`

```erlang
-type vertex() :: #{
    id := vertex_id(),      %% 顶点唯一标识
    value := term(),        %% 顶点值（黑盒数据）
    edges := [edge()],      %% 出边列表
    halted := boolean()     %% 是否已停止
}.

-type edge() :: #{
    target := vertex_id(),  %% 目标顶点 ID
    weight := number()      %% 边权重
}.
```

### 字段说明

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | `term()` | 顶点唯一标识（如 `llm_call`, `execute_tools`） |
| `value` | `term()` | 顶点值，**上层 GraphState 存储在这里** |
| `edges` | `[edge()]` | 出边列表，定义可以发送消息的目标顶点 |
| `halted` | `boolean()` | 是否已投票停止 |

## 2. GraphState 与 Vertex 的关系

**GraphState 存储在 Vertex 的 `value` 字段中。**

对于 Pregel 来说，`value` 是一个黑盒（`term()` 类型），它不关心内部结构，只负责：
- 存储它
- 传递给计算函数
- 接收计算函数返回的新值

### 关系图

```
┌─────────────────────────────────────────────────────────┐
│                      Vertex                             │
├─────────────────────────────────────────────────────────┤
│  id: llm_call                                           │
│  edges: [{target: record_llm, weight: 1}]               │
│  halted: false                                          │
│                                                         │
│  value: ┌─────────────────────────────────────────┐    │
│         │           GraphState (黑盒)              │    │
│         │  ┌─────────────────────────────────┐    │    │
│         │  │ 对 Pregel 来说是 term()         │    │    │
│         │  │ 上层可以放任何数据              │    │    │
│         │  │ (messages, context, tools...)   │    │    │
│         │  └─────────────────────────────────┘    │    │
│         └─────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
```

## 3. 计算函数 Context

当执行顶点的计算函数时，Pregel 会构建一个 Context 传入。

### Context 结构

代码位置：`pregel_worker.erl:46-52`

```erlang
-type context() :: #{
    vertex := vertex(),              %% 整个顶点（包含 value）
    messages := [term()],            %% 收到的消息列表
    superstep := non_neg_integer(),  %% 当前超步编号
    num_vertices := non_neg_integer(),%% 全图顶点总数
    outbox := [{vertex_id(), term()}] %% 待发送消息（初始为空）
}.
```

### Context 构建

代码位置：`pregel_worker.erl:237-246`

```erlang
%% @private 创建计算上下文
-spec make_context(vertex(), [term()], non_neg_integer(), non_neg_integer()) -> context().
make_context(Vertex, Messages, Superstep, NumVertices) ->
    #{
        vertex => Vertex,
        messages => Messages,
        superstep => Superstep,
        num_vertices => NumVertices,
        outbox => []
    }.
```

### 字段详解

| 字段 | 类型 | 说明 | 来源 |
|------|------|------|------|
| `vertex` | `vertex()` | 完整顶点结构 | Worker 本地存储 |
| `messages` | `[term()]` | 本超步收到的消息 | Worker inbox |
| `superstep` | `non_neg_integer()` | 当前超步编号（从 0 开始） | Worker 状态 |
| `num_vertices` | `non_neg_integer()` | 图中顶点总数 | 初始化时传入 |
| `outbox` | `[{vertex_id(), term()}]` | 待发送消息 | 初始为空 |

## 4. 计算函数的调用

代码位置：`pregel_worker.erl:217-230`

```erlang
compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    maps:fold(
        fun(Id, Vertex, {VAcc, OAcc}) ->
            Messages = maps:get(Id, Inbox, []),
            %% 有消息时激活顶点
            ActiveVertex = activate_if_has_messages(Vertex, Messages),
            %% 创建上下文并执行计算
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),
            %% 执行计算函数
            #{vertex := NewVertex, outbox := Out} = ComputeFn(Context),
            {VAcc#{Id => NewVertex}, Out ++ OAcc}
        end,
        {AllVertices, []},
        ActiveVertices
    ).
```

### 执行流程

```
1. 获取顶点收到的消息
         ↓
2. 如果有消息，激活顶点
         ↓
3. 构建 Context
         ↓
4. 调用 ComputeFn(Context)
         ↓
5. 提取返回的 NewVertex 和 Outbox
         ↓
6. 更新顶点，收集待发送消息
```

## 5. 计算函数内部操作

计算函数通过 `pregel` 模块提供的 API 操作 Context。

### 读取操作

```erlang
%% 获取顶点 ID
VertexId = pregel:get_vertex_id(Ctx),        %% Ctx.vertex.id

%% 获取顶点值（GraphState）
Value = pregel:get_vertex_value(Ctx),        %% Ctx.vertex.value

%% 获取收到的消息
Messages = pregel:get_messages(Ctx),         %% Ctx.messages

%% 获取当前超步
Superstep = pregel:get_superstep(Ctx),       %% Ctx.superstep

%% 获取邻居列表
Neighbors = pregel:get_neighbors(Ctx),       %% Ctx.vertex.edges 提取 target

%% 获取出边列表
Edges = pregel:get_edges(Ctx),               %% Ctx.vertex.edges

%% 获取全图顶点数
N = pregel:get_num_vertices(Ctx),            %% Ctx.num_vertices
```

### 修改操作

```erlang
%% 设置新的顶点值（GraphState）
Ctx1 = pregel:set_value(Ctx, NewValue),      %% 更新 Ctx.vertex.value

%% 发送消息给指定顶点
Ctx2 = pregel:send_message(Ctx1, TargetId, Msg), %% 追加到 Ctx.outbox

%% 向所有邻居发送消息
Ctx3 = pregel:send_to_all_neighbors(Ctx2, Msg),

%% 投票停止
Ctx4 = pregel:vote_to_halt(Ctx3),            %% 设置 Ctx.vertex.halted = true
```

## 6. 完整示例

```erlang
ComputeFn = fun(Ctx) ->
    %% 1. 读取信息
    VertexId = pregel:get_vertex_id(Ctx),
    GraphState = pregel:get_vertex_value(Ctx),   %% 黑盒数据
    Messages = pregel:get_messages(Ctx),
    Superstep = pregel:get_superstep(Ctx),

    %% 2. 执行计算（上层逻辑）
    NewGraphState = do_something(GraphState, Messages),

    %% 3. 更新顶点值
    Ctx1 = pregel:set_value(Ctx, NewGraphState),

    %% 4. 发送消息给其他顶点
    Ctx2 = pregel:send_message(Ctx1, next_vertex, {state, NewGraphState}),

    %% 5. 投票停止
    pregel:vote_to_halt(Ctx2)
end.
```

## 7. 总结图

```
┌─────────────────────────────────────────────────────────────────┐
│                    计算函数输入 (Context)                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  vertex ─────────┬─── id: atom()           顶点标识             │
│                  ├─── value: term()        ⭐ GraphState (黑盒) │
│                  ├─── edges: [edge()]      出边列表             │
│                  └─── halted: boolean()    停止标志             │
│                                                                 │
│  messages ────────── [term()]              收到的消息列表        │
│                                            （也是黑盒）          │
│                                                                 │
│  superstep ──────── non_neg_integer()      当前超步编号         │
│                                                                 │
│  num_vertices ───── non_neg_integer()      全图顶点数           │
│                                                                 │
│  outbox ─────────── []                     待发送消息（初始空）  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘

                              │
                              ▼

┌─────────────────────────────────────────────────────────────────┐
│                   计算函数输出 (Context)                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  vertex ─────────┬─── id: (不变)                                │
│                  ├─── value: term()        ⭐ 新的 GraphState   │
│                  ├─── edges: (可能修改)                         │
│                  └─── halted: true/false   可能投票停止         │
│                                                                 │
│  outbox ─────────── [{target, msg}, ...]   待发送的消息         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 8. 关键点

1. **GraphState 是 `vertex.value`**：对 Pregel 完全透明，是黑盒
2. **Messages 也是黑盒**：Pregel 只负责传递，不解析内容
3. **Pregel 的职责**：
   - 管理超步执行
   - 构建 Context
   - 路由消息
   - 检测终止条件
4. **上层的职责**：
   - 定义 GraphState 结构
   - 定义 Message 格式
   - 实现计算逻辑
