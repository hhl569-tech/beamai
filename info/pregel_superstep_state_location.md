# Pregel superstep_state 存储位置

## 结论

**superstep_state 应该存储在 Worker 进程中，而不是 Master 进程。**

## 原因分析

### Master vs Worker 职责对比

| 职责 | Master | Worker |
|------|--------|--------|
| 执行顶点计算 | ❌ 否 | ✅ 是 |
| 管理本地顶点 | ❌ 否 | ✅ 是 |
| 知道哪个顶点失败 | ❌ 否 | ✅ 是 |
| 需要重试失败顶点 | ❌ 否 | ✅ 是 |
| 协调超步执行 | ✅ 是 | ❌ 否 |
| 管理 Barrier 同步 | ✅ 是 | ❌ 否 |
| 收集汇总结果 | ✅ 是 | ❌ 否 |

### 职责分工图

```
┌─────────────────────────────────────────────────────────────────┐
│                         Master                                  │
├─────────────────────────────────────────────────────────────────┤
│  职责:                                                          │
│  - 协调超步执行                                                  │
│  - 管理 Barrier 同步                                            │
│  - 收集 Worker 汇总结果                                         │
│  - 决定继续/终止                                                 │
│                                                                 │
│  不关心:                                                         │
│  - 单个顶点的执行状态                                            │
│  - 哪些顶点成功/失败                                             │
│  - 重试逻辑                                                      │
│                                                                 │
│  ❌ 不存储 superstep_state                                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                    触发超步执行
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         Worker                                  │
├─────────────────────────────────────────────────────────────────┤
│  职责:                                                          │
│  - 执行本地顶点计算                                              │
│  - 追踪每个顶点的执行状态                                        │
│  - 重试失败的顶点                                                │
│  - 汇总结果上报 Master                                          │
│                                                                 │
│  ✅ 存储 superstep_state                                        │
│                                                                 │
│  superstep_state = #{                                           │
│    vertex_a => #{status => completed, vertex => V, outbox => O},│
│    vertex_b => #{status => failed, error => Reason},            │
│    vertex_c => #{status => pending}                             │
│  }                                                              │
└─────────────────────────────────────────────────────────────────┘
```

## Worker 状态扩展

### 当前 Worker 状态

代码位置：`pregel_worker.erl:55-66`

```erlang
-record(state, {
    worker_id     :: non_neg_integer(),
    master        :: pid(),
    vertices      :: #{vertex_id() => vertex()},
    inbox         :: #{vertex_id() => [term()]},
    compute_fn    :: fun(),
    combiner      :: pregel_combiner:spec(),
    superstep     :: non_neg_integer(),
    num_workers   :: pos_integer(),
    num_vertices  :: non_neg_integer(),
    worker_pids   :: #{non_neg_integer() => pid()}
}).
```

### 扩展后的 Worker 状态

```erlang
-record(state, {
    worker_id     :: non_neg_integer(),
    master        :: pid(),
    vertices      :: #{vertex_id() => vertex()},
    inbox         :: #{vertex_id() => [term()]},
    compute_fn    :: fun(),
    combiner      :: pregel_combiner:spec(),
    superstep     :: non_neg_integer(),
    num_workers   :: pos_integer(),
    num_vertices  :: non_neg_integer(),
    worker_pids   :: #{non_neg_integer() => pid()},

    %% 新增：超步执行状态
    superstep_state :: #{vertex_id() => vertex_result()}
}).
```

## superstep_state 数据结构

```erlang
%% 顶点执行状态
-type vertex_status() :: pending      %% 待执行
                       | completed    %% 已完成
                       | failed.      %% 已失败

%% 顶点执行结果
-type vertex_result() :: #{
    status := vertex_status(),
    vertex => vertex(),                    %% 新顶点状态（completed 时）
    outbox => [{vertex_id(), term()}],     %% 发出的消息（completed 时）
    error => term(),                       %% 错误原因（failed 时）
    attempts => non_neg_integer()          %% 尝试次数
}.

%% superstep_state 类型
-type superstep_state() :: #{vertex_id() => vertex_result()}.
```

## 生命周期

```
超步 N 开始
    │
    ▼
┌─────────────────────────────────────────┐
│  初始化 superstep_state                  │
│  superstep_state = #{                   │
│    v1 => #{status => pending},          │
│    v2 => #{status => pending},          │
│    v3 => #{status => pending}           │
│  }                                      │
└─────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────┐
│  执行顶点计算，更新状态                   │
│  superstep_state = #{                   │
│    v1 => #{status => completed, ...},   │
│    v2 => #{status => failed, ...},      │
│    v3 => #{status => completed, ...}    │
│  }                                      │
└─────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────┐
│  重试失败顶点                            │
│  superstep_state = #{                   │
│    v1 => #{status => completed, ...},   │
│    v2 => #{status => completed, ...},   │ ← 重试成功
│    v3 => #{status => completed, ...}    │
│  }                                      │
└─────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────┐
│  上报 Master，清空 superstep_state       │
│  superstep_state = #{}                  │
└─────────────────────────────────────────┘
    │
    ▼
超步 N+1 开始
```

## 关键点

1. **Worker 管理自己的顶点**：每个 Worker 只追踪自己负责的顶点状态
2. **Master 不参与**：Master 只关心汇总结果，不关心单个顶点状态
3. **超步内有效**：superstep_state 在超步结束后清空
4. **支持重试**：通过 superstep_state 可以识别并重试失败的顶点
