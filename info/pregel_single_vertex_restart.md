# Pregel 单顶点重启设计

## 背景

当超步 S 中顶点 A 计算失败时，需要支持只重启顶点 A 的计算，而不是重启整个图。

## checkpoint_data 提供的信息

```erlang
checkpoint_data = #{
    superstep => S,
    vertices => #{A => VertexA, B => VertexB, ...},  %% 顶点状态
    pending_messages => [{C, msg1}, {D, msg2}],      %% 本超步的 outbox
    vertex_inbox => #{A => [msg_a, msg_b], B => [msg_c]}  %% 各顶点的收件箱
}
```

## 关键设计：失败顶点状态未被更新

根据 `pregel_worker.erl` 的 `process_compute_result/3` 实现：

```erlang
process_compute_result(Id, #{status := {error, Reason}},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 失败：记录失败信息，不更新顶点，不收集消息
    {VAcc, OAcc, [{Id, Reason} | FailedAcc], InterruptedAcc};
```

**失败的顶点保持计算前的状态**，因此：
- `vertices[A]` = 顶点 A 在超步 S 开始时的状态
- `vertex_inbox[A]` = 顶点 A 在超步 S 收到的消息

这两个信息足以重启单个顶点的计算。

---

## 方案 1：Graph 层直接重计算（当前支持）

```erlang
%% on_superstep_complete 回调中处理失败
handle_superstep_complete(#{
    type := step,
    failed_count := FailedCount,
    failed_vertices := FailedVertices,
    get_checkpoint_data := GetCheckpoint
}) when FailedCount > 0 ->
    %% 1. 获取 checkpoint 数据
    #{vertices := Vertices, vertex_inbox := Inbox} = GetCheckpoint(),

    %% 2. 对每个失败顶点重试
    Results = lists:map(fun({VertexId, _Reason}) ->
        Vertex = maps:get(VertexId, Vertices),
        Messages = maps:get(VertexId, Inbox, []),

        %% 直接调用计算函数
        Context = #{
            vertex => Vertex,
            messages => Messages,
            superstep => S,
            num_vertices => NumVertices,
            outbox => []
        },
        ComputeFn(Context)
    end, FailedVertices),

    %% 3. 根据重试结果决定
    case all_succeeded(Results) of
        true -> continue;
        false -> {stop, {failed_after_retry, Results}}
    end.
```

**优点**：无需修改 Pregel 层
**缺点**：重试产生的 outbox 消息无法自动路由到其他顶点

---

## 方案 2：Pregel 层原生单顶点重启 API（待实现）

### 设计概述

```
┌─────────────────────────────────────────────────────────────┐
│  超步 S 执行完成，顶点 A 失败                                │
│         │                                                    │
│         ▼                                                    │
│  on_superstep_complete 回调                                  │
│         │                                                    │
│         ▼                                                    │
│  返回 {retry, [A]} ──► Pregel 自动重试顶点 A               │
│         │                                                    │
│         ▼                                                    │
│  重试成功 ──► 合并 outbox，继续下一超步                     │
│  重试失败 ──► 再次调用回调，由 Graph 层决策                 │
└─────────────────────────────────────────────────────────────┘
```

### 类型定义

```erlang
%% 扩展回调返回值
-type superstep_complete_result() ::
    continue
    | {stop, term()}
    | {retry, retry_opts()}.    %% 新增

%% 重试选项
-type retry_opts() :: #{
    vertices := [vertex_id()],   %% 要重试的顶点列表
    max_retries => pos_integer() %% 最大重试次数（默认 1）
}.
```

### API 使用示例

```erlang
%% Graph 层回调
on_superstep_complete(#{
    failed_count := FailedCount,
    failed_vertices := FailedVertices
}) when FailedCount > 0 ->
    %% 提取失败的顶点 ID
    VertexIds = [Id || {Id, _Reason} <- FailedVertices],
    %% 请求 Pregel 层重试这些顶点
    {retry, #{vertices => VertexIds, max_retries => 3}};

on_superstep_complete(_Info) ->
    continue.
```

### 实现要点

1. **pregel_master 处理 {retry, Opts}**
   - 从当前状态获取失败顶点的 vertex + inbox
   - 调用 Worker 重新计算指定顶点
   - 收集 outbox 并合并到待路由消息

2. **pregel_worker 支持部分顶点重计算**
   - 新增 `retry_vertices/3` API
   - 只计算指定顶点，使用保存的 inbox

3. **重试结果处理**
   - 成功：合并结果，继续超步
   - 失败：再次调用回调，让 Graph 层决策

---

## 支持程度对比

| 场景 | 方案 1 (Graph 层) | 方案 2 (Pregel 原生) |
|------|------------------|---------------------|
| 单顶点重计算 | ✅ | ✅ |
| 重试后 outbox 自动路由 | ❌ | ✅ |
| 重试次数控制 | 手动实现 | ✅ 内置 |
| 实现复杂度 | 低 | 中 |

---

## 职责分离

```
┌─────────────────────────────────────────────────────────────┐
│  Graph 层                                                    │
│  ─────────                                                   │
│  • 决定是否重试、重试哪些顶点                                │
│  • 决定最大重试次数                                          │
│  • 决定重试失败后的行为                                      │
│                                                              │
│  Pregel 层                                                   │
│  ──────────                                                  │
│  • 执行重试（重新计算指定顶点）                              │
│  • 自动使用保存的 vertex_inbox                               │
│  • 合并重试结果到主流程                                      │
│  • 路由重试产生的 outbox 消息                                │
└─────────────────────────────────────────────────────────────┘
```
