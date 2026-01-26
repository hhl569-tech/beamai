%%%-------------------------------------------------------------------
%%% @doc Checkpoint 记录和常量定义
%%%
%%% 定义 Graph Engine (Pregel) 专用的 Checkpoint 记录。
%%% 用于保存和恢复图计算执行状态。
%%%
%%% == 核心概念 ==
%%%
%%% - Run: 单次图执行，由 run_id 标识
%%% - Checkpoint: 某一超步完成后的执行状态
%%% - Version: 检查点在时间线中的版本号
%%%
%%% == 时间旅行模型 ==
%%%
%%% ```
%%% superstep_0 ──→ superstep_1 ──→ superstep_2 ──→ superstep_3
%%%      │              │              │              │
%%%     v1             v2             v3             v4
%%%      │                             │
%%%      └──── go_back(2) ─────────────┘
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_CHECKPOINT_HRL).
-define(BEAMAI_CHECKPOINT_HRL, true).

%%====================================================================
%% Graph Checkpoint 记录
%%====================================================================

%% Graph Checkpoint - 面向 Graph Engine (Pregel)
-record(graph_checkpoint, {
    %%--------------------------------------------------------------------
    %% 基础标识（时间线通用字段）
    %%--------------------------------------------------------------------

    %% 唯一标识
    id :: binary(),

    %% 执行标识（所属者）
    run_id :: binary(),

    %% 父检查点 ID（用于链式追溯）
    parent_id :: binary() | undefined,

    %% 分支标识
    branch_id :: binary(),

    %% 版本号（用于时间旅行定位）
    version :: non_neg_integer(),

    %% 创建时间戳
    created_at :: integer(),

    %%--------------------------------------------------------------------
    %% Pregel 引擎状态
    %%--------------------------------------------------------------------

    %% 当前超步
    superstep :: non_neg_integer(),

    %% 迭代次数
    iteration :: non_neg_integer(),

    %% 顶点状态
    vertices :: #{atom() => vertex_state()},

    %% 待激活顶点列表
    pending_activations :: [atom()],

    %% 延迟提交的 deltas（出错时暂存，用于恢复后重试）
    pending_deltas :: [map()] | undefined,

    %% 全局共享状态
    global_state :: map(),

    %%--------------------------------------------------------------------
    %% 执行分类
    %%--------------------------------------------------------------------

    %% 活跃顶点列表
    active_vertices :: [atom()],

    %% 已完成顶点列表
    completed_vertices :: [atom()],

    %% 失败顶点（用于重试）
    failed_vertices :: [atom()],

    %% 中断顶点（等待用户输入）
    interrupted_vertices :: [atom()],

    %%--------------------------------------------------------------------
    %% 检查点类型
    %%--------------------------------------------------------------------

    %% 类型: initial | superstep | error | interrupt | final
    checkpoint_type :: checkpoint_type(),

    %%--------------------------------------------------------------------
    %% 恢复信息
    %%--------------------------------------------------------------------

    %% 是否可恢复
    resumable :: boolean(),

    %% 恢复时注入的数据
    resume_data :: #{atom() => term()},

    %% 重试计数
    retry_count :: non_neg_integer(),

    %%--------------------------------------------------------------------
    %% 扩展信息
    %%--------------------------------------------------------------------

    %% 图定义名称
    graph_name :: atom() | undefined,

    %% 自定义元数据
    metadata = #{} :: map()
}).

%% 顶点状态
-type vertex_state() :: #{
    value := term(),
    active := boolean(),
    messages := [term()],
    halt_voted := boolean()
}.

%% 检查点类型
-type checkpoint_type() ::
    initial |       %% 图初始化
    superstep |     %% 超步完成
    error |         %% 执行出错
    interrupt |     %% 等待用户输入
    final.          %% 图执行完成

%%====================================================================
%% 检查点配置
%%====================================================================

-record(checkpoint_config, {
    %% 执行 ID（必需）
    run_id :: binary(),

    %% 最大检查点数量
    max_checkpoints :: pos_integer(),

    %% 自动清理
    auto_prune :: boolean(),

    %% 自定义配置
    options = #{} :: map()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间
-define(NS_GRAPH_CHECKPOINTS, <<"graph_checkpoints">>).

%% ID 前缀
-define(CHECKPOINT_ID_PREFIX, <<"gcp_">>).

%% 默认值
-define(DEFAULT_MAX_CHECKPOINTS, 100).
-define(DEFAULT_CHECKPOINT_BRANCH, <<"main">>).

%% 检查点类型常量
-define(CP_INITIAL, initial).
-define(CP_SUPERSTEP, superstep).
-define(CP_ERROR, error).
-define(CP_INTERRUPT, interrupt).
-define(CP_FINAL, final).

-endif.
