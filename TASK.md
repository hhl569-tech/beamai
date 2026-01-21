# pregel 错误处理任务

## 待完成任务

### P1: pregel_master 和 pregel_barrier 失败处理

**优先级**: P1 (可选)

**前置条件**: P0 阶段已完成，Worker 已能正确上报失败信息

---

### 3.1 pregel_master 处理失败信息

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [ ] 添加 `on_vertex_error` 字段到 #state
- [ ] 添加 `error_strategy()` 类型定义
- [ ] 修改 `init/1` 初始化错误策略
- [ ] 修改 `complete_superstep/1` 检查失败
- [ ] 实现 `handle_vertex_failures/3`
- [ ] 可选：修改 `finish_execution/2` 支持错误状态

#### 错误策略

```erlang
-type error_strategy() :: fail_fast
                        | continue
                        | {callback, fun(([{vertex_id(), term()}]) -> continue | stop)}.
```

| 策略 | 行为 | 适用场景 |
|------|------|----------|
| **fail_fast** | 有任何顶点失败立即终止 | 要求严格正确性 |
| **continue** | 忽略失败继续执行 | 允许部分失败 |
| **callback** | 回调上层决定 | 需要动态决策 |

---

### 3.2 修改 pregel_barrier 汇总失败信息

**文件**: `apps/beamai_core/src/graph/pregel/pregel_barrier.erl`

- [ ] 修改 `aggregate_results/1` 返回值类型
- [ ] 添加 failed_count 和 failed_vertices 汇总

**当前**:
```erlang
-spec aggregate_results([map()]) -> {non_neg_integer(), non_neg_integer()}.
```

**修改后**:
```erlang
-spec aggregate_results([map()]) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer(), [{term(), term()}]}.
%% {TotalActive, TotalMessages, TotalFailed, AllFailedVertices}
```

---

### 2.3 可选：添加重试逻辑

**文件**: `apps/beamai_core/src/graph/pregel/graph_compute.erl`

- [ ] 实现 should_retry/1 函数
- [ ] 实现 retry_with_backoff/3 函数
- [ ] 配置最大重试次数

---

## 相关文档

- `info/pregel_master_failure_handling.md` - Master 失败处理详细设计
- `info/pregel_worker_error_handling_design.md` - Worker 错误处理设计
- `info/pregel_error_reporting.md` - 错误上报机制分析

---

## 已完成任务

见 `TASK_DONE.md`
