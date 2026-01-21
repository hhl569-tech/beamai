# pregel_worker 错误处理重构任务

## 任务概述

为 `pregel_worker` 增加错误处理能力，使其能够识别顶点计算的成功/失败状态，并将失败信息上报给 Master。

**设计原则**：pregel_worker 不做异常处理和重试，只约定成功/失败的数据结构契约，根据结果决定下一步动作。异常处理和重试逻辑由 graph_compute 层负责。

---

## 当前状态

- `compute_vertices` 不检查计算结果状态
- `notify_master_done` 不上报错误信息
- Master 无法感知顶点执行失败

---

## 目标状态

- 约定计算结果的成功/失败数据结构
- `compute_vertices` 根据 status 字段处理结果
- `notify_master_done` 上报失败顶点信息
- Master 可感知并处理错误

---

## 任务分解

### 阶段 1：pregel_worker 修改

#### 1.1 定义数据结构契约

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

- [ ] 添加类型定义

```erlang
%% 计算结果状态
-type compute_status() :: ok | {error, term()}.

%% 计算结果（计算函数必须返回此结构）
-type compute_result() :: #{
    vertex := vertex(),
    outbox := [{vertex_id(), term()}],
    status := compute_status()
}.
```

- [ ] 导出类型

```erlang
-export_type([compute_result/0, compute_status/0]).
```

#### 1.2 修改 compute_vertices 函数

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

**当前代码** (行 217-230):
```erlang
compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    maps:fold(
        fun(Id, Vertex, {VAcc, OAcc}) ->
            Messages = maps:get(Id, Inbox, []),
            ActiveVertex = activate_if_has_messages(Vertex, Messages),
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),
            #{vertex := NewVertex, outbox := Out} = ComputeFn(Context),
            {VAcc#{Id => NewVertex}, Out ++ OAcc}
        end,
        {AllVertices, []},
        ActiveVertices
    ).
```

**修改内容**:

- [ ] 修改函数签名，返回值增加 FailedVertices

```erlang
-spec compute_vertices(...) ->
    {#{vertex_id() => vertex()}, [{vertex_id(), term()}], [{vertex_id(), term()}]}.
```

- [ ] 修改 fold 初始累加器

```erlang
{AllVertices, [], []}  %% 增加 FailedVertices 列表
```

- [ ] 添加 status 检查逻辑

```erlang
Result = ComputeFn(Context),
case maps:get(status, Result, ok) of
    ok ->
        #{vertex := NewVertex, outbox := Out} = Result,
        {VAcc#{Id => NewVertex}, Out ++ OAcc, FailedAcc};
    {error, Reason} ->
        {VAcc, OAcc, [{Id, Reason} | FailedAcc]}
end
```

#### 1.3 修改 execute_superstep 函数

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

**当前代码** (行 167-194):
```erlang
execute_superstep(...) ->
    ...
    {NewVertices, Outbox} = compute_vertices(...),
    ...
    notify_master_done(State, NewVertices, CombinedOutbox),
    ...
```

**修改内容**:

- [ ] 接收 compute_vertices 的三元组返回值

```erlang
{NewVertices, Outbox, FailedVertices} = compute_vertices(...)
```

- [ ] 传递 FailedVertices 给 notify_master_done

```erlang
notify_master_done(State, NewVertices, CombinedOutbox, FailedVertices)
```

#### 1.4 修改 notify_master_done 函数

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

**当前代码** (行 331-337):
```erlang
notify_master_done(#state{worker_id = WorkerId, master = Master}, Vertices, Outbox) ->
    Result = #{
        worker_id => WorkerId,
        active_count => pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices),
        message_count => length(Outbox)
    },
    gen_server:cast(Master, {worker_done, self(), Result}).
```

**修改内容**:

- [ ] 修改函数签名，增加 FailedVertices 参数

```erlang
notify_master_done(#state{...}, Vertices, Outbox, FailedVertices) ->
```

- [ ] 添加失败信息到 Result

```erlang
Result = #{
    worker_id => WorkerId,
    active_count => ...,
    message_count => ...,
    failed_count => length(FailedVertices),
    failed_vertices => FailedVertices
},
```

---

### 阶段 2：graph_compute 修改

#### 2.1 修改 compute_fn 返回值

**文件**: `apps/beamai_core/src/graph/pregel/graph_compute.erl`

- [ ] 修改 finish_node_execution 成功时添加 status

```erlang
%% 成功时
Ctx1#{status => ok}
```

- [ ] 修改 finish_node_execution 失败时返回 status

```erlang
%% 失败时
Ctx#{
    outbox => [],
    status => {error, Reason}
}
```

#### 2.2 添加异常处理包装

**文件**: `apps/beamai_core/src/graph/pregel/graph_compute.erl`

- [ ] 在 compute_fn 中添加 try-catch

```erlang
compute_fn() ->
    fun(Ctx) ->
        try
            do_compute(Ctx)
        catch
            Class:Reason:_Stacktrace ->
                #{
                    vertex => maps:get(vertex, Ctx),
                    outbox => [],
                    status => {error, {Class, Reason}}
                }
        end
    end.
```

#### 2.3 可选：添加重试逻辑

**文件**: `apps/beamai_core/src/graph/pregel/graph_compute.erl`

- [ ] 实现 should_retry/1 函数
- [ ] 实现 retry_with_backoff/3 函数
- [ ] 配置最大重试次数

---

### 阶段 3：pregel_master 修改（可选）

#### 3.1 处理失败信息

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [ ] 修改 handle_worker_done 处理 failed_count
- [ ] 修改 complete_superstep 汇总失败信息
- [ ] 可选：实现错误策略（fail_fast/continue）

#### 3.2 修改 pregel_barrier

**文件**: `apps/beamai_core/src/graph/pregel/pregel_barrier.erl`

- [ ] 修改 aggregate_results 汇总失败信息

---

## 验收标准

### 功能验收

- [ ] 顶点计算成功时，正常更新顶点和发送消息
- [ ] 顶点计算失败时，不更新顶点，不发送消息
- [ ] Worker 正确上报 failed_count 和 failed_vertices
- [ ] Master 可以接收并处理失败信息

### 代码验收

- [ ] 新增类型定义并导出
- [ ] 函数签名更新
- [ ] 单元测试通过
- [ ] 现有功能不受影响（向后兼容）

### 测试用例

- [ ] 所有顶点成功的场景
- [ ] 部分顶点失败的场景
- [ ] 所有顶点失败的场景
- [ ] 并行执行时的失败处理

---

## 依赖关系

```
pregel_worker 类型定义
       ↓
pregel_worker compute_vertices 修改
       ↓
pregel_worker execute_superstep 修改
       ↓
pregel_worker notify_master_done 修改
       ↓
graph_compute compute_fn 修改
       ↓
(可选) pregel_master 处理失败信息
```

---

## 文件修改清单

| 文件 | 修改类型 | 优先级 |
|------|----------|--------|
| `pregel_worker.erl` | 类型定义、函数修改 | P0 |
| `graph_compute.erl` | 返回值修改、异常处理 | P0 |
| `pregel_master.erl` | 处理失败信息 | P1 |
| `pregel_barrier.erl` | 汇总失败信息 | P1 |

---

## 相关文档

- `info/pregel_worker_error_handling_design.md` - 详细设计文档
- `info/pregel_error_reporting.md` - 错误上报机制分析
- `info/pregel_superstep_state_structure.md` - 备选方案参考

---

## 风险和注意事项

| 风险 | 缓解措施 |
|------|----------|
| 向后兼容 | compute_fn 返回值需要兼容旧格式（status 默认 ok） |
| 测试覆盖 | 增加失败场景的单元测试 |
| 性能影响 | 失败列表在内存中，超步后清空，影响可忽略 |
