%%%-------------------------------------------------------------------
%%% @doc Checkpoint 模块 - Graph Engine 专用
%%%
%%% 为 Graph Engine (Pregel) 提供检查点功能：
%%% - 保存/恢复图执行状态
%%% - 时间旅行（回退/前进超步）
%%% - 分支管理
%%% - 顶点重试
%%%
%%% == 实现 beamai_timeline 行为 ==
%%%
%%% 本模块实现 beamai_timeline 行为，提供 Graph 特定的
%%% 条目访问器、修改器和工厂函数。
%%%
%%% == 时间旅行模型 ==
%%%
%%% Graph Engine 的时间线由超步驱动：
%%% ```
%%% superstep_0 → superstep_1 → superstep_2 → superstep_3 → superstep_4
%%%      │            │             │             │             │
%%%     cp_1         cp_2          cp_3          cp_4          cp_5
%%%                   ↑                           ↑
%%%                   └─── go_back(2) ────────────┘
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_checkpoint).

-behaviour(beamai_timeline).

-include_lib("beamai_memory/include/beamai_checkpoint.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 类型导出
-export_type([
    checkpoint/0,
    manager/0,
    checkpoint_type/0,
    vertex_state/0
]).

%% 构造函数
-export([
    new/1,
    new/2
]).

%% 核心操作（委托给 timeline）
-export([
    save/3,
    load/2,
    delete/2,
    get_latest/2
]).

%% 时间旅行（委托给 timeline）
-export([
    go_back/3,
    go_forward/3,
    goto/3,
    undo/2,
    redo/2,
    get_current_position/2
]).

%% 分支管理（委托给 timeline）
-export([
    fork_from/4,
    fork_from/5,
    list_branches/1,
    switch_branch/2
]).

%% 历史查询（委托给 timeline）
-export([
    get_history/2,
    get_history/3,
    get_lineage/2,
    compare/3
]).

%% Graph 专用操作
-export([
    save_from_pregel/3,
    save_from_pregel/4,
    create_checkpoint/2,
    create_checkpoint/3,
    get_vertex_state/2,
    get_vertices/1,
    get_active_vertices/1,
    get_failed_vertices/1,
    get_interrupted_vertices/1,
    get_pending_activations/1,
    get_pending_deltas/1,
    is_resumable/1,
    get_resume_data/1,
    set_resume_data/2,
    get_superstep/1,
    get_global_state/1,
    retry_vertex/3,
    inject_resume_data/3,
    to_pregel_restore_opts/1
]).

%% Timeline 行为回调
-export([
    entry_id/1,
    entry_owner_id/1,
    entry_parent_id/1,
    entry_version/1,
    entry_branch_id/1,
    entry_created_at/1,
    entry_state/1,
    set_entry_id/2,
    set_entry_parent_id/2,
    set_entry_version/2,
    set_entry_branch_id/2,
    new_entry/3,
    entry_to_state_entry/1,
    state_entry_to_entry/1,
    namespace/0,
    id_prefix/0,
    entry_type/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type checkpoint() :: #graph_checkpoint{}.
-type manager() :: beamai_timeline:manager().

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Checkpoint 管理器
-spec new(beamai_state_store:store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

%% @doc 创建 Checkpoint 管理器（带选项）
-spec new(beamai_state_store:store(), map()) -> manager().
new(StateStore, Opts) ->
    beamai_timeline:new(?MODULE, StateStore, Opts).

%%====================================================================
%% 核心操作
%%====================================================================

%% @doc 保存检查点
-spec save(manager(), binary(), checkpoint()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
save(Mgr, RunId, Checkpoint) ->
    beamai_timeline:save(Mgr, RunId, Checkpoint).

%% @doc 加载检查点
-spec load(manager(), binary()) -> {ok, checkpoint()} | {error, term()}.
load(Mgr, CheckpointId) ->
    beamai_timeline:load(Mgr, CheckpointId).

%% @doc 删除检查点
-spec delete(manager(), binary()) -> {ok, manager()} | {error, term()}.
delete(Mgr, CheckpointId) ->
    beamai_timeline:delete(Mgr, CheckpointId).

%% @doc 获取最新检查点
-spec get_latest(manager(), binary()) -> {ok, checkpoint()} | {error, term()}.
get_latest(Mgr, RunId) ->
    beamai_timeline:get_latest(Mgr, RunId).

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 个超步
-spec go_back(manager(), binary(), pos_integer()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
go_back(Mgr, RunId, Steps) ->
    beamai_timeline:go_back(Mgr, RunId, Steps).

%% @doc 前进 N 个超步
-spec go_forward(manager(), binary(), pos_integer()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
go_forward(Mgr, RunId, Steps) ->
    beamai_timeline:go_forward(Mgr, RunId, Steps).

%% @doc 跳转到指定检查点
-spec goto(manager(), binary(), binary()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
goto(Mgr, RunId, CheckpointId) ->
    beamai_timeline:goto(Mgr, RunId, CheckpointId).

%% @doc 撤销
-spec undo(manager(), binary()) -> {ok, checkpoint(), manager()} | {error, term()}.
undo(Mgr, RunId) ->
    beamai_timeline:undo(Mgr, RunId).

%% @doc 重做
-spec redo(manager(), binary()) -> {ok, checkpoint(), manager()} | {error, term()}.
redo(Mgr, RunId) ->
    beamai_timeline:redo(Mgr, RunId).

%% @doc 获取当前位置
-spec get_current_position(manager(), binary()) ->
    {ok, beamai_timeline:position()} | {error, term()}.
get_current_position(Mgr, RunId) ->
    beamai_timeline:get_current_position(Mgr, RunId).

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 从指定检查点创建分支
-spec fork_from(manager(), binary(), binary(), binary()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
fork_from(Mgr, CheckpointId, NewBranchName, RunId) ->
    beamai_timeline:fork_from(Mgr, CheckpointId, NewBranchName, RunId).

-spec fork_from(manager(), binary(), binary(), binary(), map()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
fork_from(Mgr, CheckpointId, NewBranchName, RunId, Opts) ->
    beamai_timeline:fork_from(Mgr, CheckpointId, NewBranchName, RunId, Opts).

%% @doc 列出所有分支
-spec list_branches(manager()) -> [beamai_timeline:branch()].
list_branches(Mgr) ->
    beamai_timeline:list_branches(Mgr).

%% @doc 切换分支
-spec switch_branch(manager(), binary()) -> {ok, manager()} | {error, term()}.
switch_branch(Mgr, BranchId) ->
    beamai_timeline:switch_branch(Mgr, BranchId).

%%====================================================================
%% 历史查询
%%====================================================================

%% @doc 获取执行路径（历史记录）
-spec get_history(manager(), binary()) -> {ok, [checkpoint()]} | {error, term()}.
get_history(Mgr, RunId) ->
    beamai_timeline:get_history(Mgr, RunId).

-spec get_history(manager(), binary(), map()) -> {ok, [checkpoint()]} | {error, term()}.
get_history(Mgr, RunId, Opts) ->
    beamai_timeline:get_history(Mgr, RunId, Opts).

%% @doc 获取血统
-spec get_lineage(manager(), binary()) -> {ok, [checkpoint()]} | {error, term()}.
get_lineage(Mgr, CheckpointId) ->
    beamai_timeline:get_lineage(Mgr, CheckpointId).

%% @doc 比较两个检查点
-spec compare(manager(), binary(), binary()) ->
    {ok, #{from := checkpoint(), to := checkpoint(), version_diff := integer()}} |
    {error, term()}.
compare(Mgr, CheckpointId1, CheckpointId2) ->
    beamai_timeline:compare(Mgr, CheckpointId1, CheckpointId2).

%%====================================================================
%% Graph 专用操作
%%====================================================================

%% @doc 从 Pregel 状态创建并保存检查点
-spec save_from_pregel(manager(), binary(), map()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
save_from_pregel(Mgr, RunId, PregelState) ->
    save_from_pregel(Mgr, RunId, PregelState, #{}).

-spec save_from_pregel(manager(), binary(), map(), map()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
save_from_pregel(Mgr, RunId, PregelState, Opts) ->
    Checkpoint = create_checkpoint(RunId, PregelState, Opts),
    save(Mgr, RunId, Checkpoint).

%% @doc 创建检查点（不保存）
-spec create_checkpoint(binary(), map()) -> checkpoint().
create_checkpoint(RunId, PregelState) ->
    create_checkpoint(RunId, PregelState, #{}).

-spec create_checkpoint(binary(), map(), map()) -> checkpoint().
create_checkpoint(RunId, PregelState, Opts) ->
    Now = erlang:system_time(millisecond),

    #graph_checkpoint{
        id = undefined,  %% 由 timeline 分配
        run_id = RunId,
        parent_id = undefined,  %% 由 timeline 设置
        branch_id = maps:get(branch_id, Opts, ?DEFAULT_CHECKPOINT_BRANCH),
        version = 0,  %% 由 timeline 设置
        created_at = Now,

        %% Pregel 状态
        superstep = maps:get(superstep, PregelState, 0),
        iteration = maps:get(iteration, PregelState, 0),
        vertices = maps:get(vertices, PregelState, #{}),
        pending_activations = maps:get(pending_activations, PregelState, []),
        pending_deltas = maps:get(pending_deltas, PregelState, undefined),
        global_state = maps:get(global_state, PregelState, #{}),

        %% 执行分类
        active_vertices = maps:get(active_vertices, PregelState, []),
        completed_vertices = maps:get(completed_vertices, PregelState, []),
        failed_vertices = maps:get(failed_vertices, PregelState, []),
        interrupted_vertices = maps:get(interrupted_vertices, PregelState, []),

        %% 检查点类型
        checkpoint_type = maps:get(checkpoint_type, Opts, superstep),

        %% 恢复信息
        resumable = maps:get(resumable, Opts, true),
        resume_data = maps:get(resume_data, Opts, #{}),
        retry_count = maps:get(retry_count, Opts, 0),

        %% 扩展信息
        graph_name = maps:get(graph_name, Opts, undefined),
        metadata = maps:get(metadata, Opts, #{})
    }.

%% @doc 获取指定顶点的状态
-spec get_vertex_state(checkpoint(), atom()) -> {ok, vertex_state()} | {error, not_found}.
get_vertex_state(#graph_checkpoint{vertices = Vertices}, VertexId) ->
    case maps:find(VertexId, Vertices) of
        {ok, State} -> {ok, State};
        error -> {error, not_found}
    end.

%% @doc 获取所有顶点
-spec get_vertices(checkpoint()) -> #{atom() => vertex_state()}.
get_vertices(#graph_checkpoint{vertices = Vertices}) ->
    Vertices.

%% @doc 获取活跃顶点列表
-spec get_active_vertices(checkpoint()) -> [atom()].
get_active_vertices(#graph_checkpoint{active_vertices = Active}) ->
    Active.

%% @doc 获取失败顶点列表
-spec get_failed_vertices(checkpoint()) -> [atom()].
get_failed_vertices(#graph_checkpoint{failed_vertices = Failed}) ->
    Failed.

%% @doc 获取中断顶点列表
-spec get_interrupted_vertices(checkpoint()) -> [atom()].
get_interrupted_vertices(#graph_checkpoint{interrupted_vertices = Interrupted}) ->
    Interrupted.

%% @doc 获取待激活顶点列表
-spec get_pending_activations(checkpoint()) -> [atom()].
get_pending_activations(#graph_checkpoint{pending_activations = Pending}) ->
    Pending.

%% @doc 获取延迟提交的 deltas
-spec get_pending_deltas(checkpoint()) -> [map()] | undefined.
get_pending_deltas(#graph_checkpoint{pending_deltas = Deltas}) ->
    Deltas.

%% @doc 检查是否可恢复
-spec is_resumable(checkpoint()) -> boolean().
is_resumable(#graph_checkpoint{resumable = Resumable}) ->
    Resumable.

%% @doc 获取恢复数据
-spec get_resume_data(checkpoint()) -> #{atom() => term()}.
get_resume_data(#graph_checkpoint{resume_data = ResumeData}) ->
    ResumeData.

%% @doc 设置恢复数据
-spec set_resume_data(checkpoint(), #{atom() => term()}) -> checkpoint().
set_resume_data(Checkpoint, ResumeData) ->
    Checkpoint#graph_checkpoint{resume_data = ResumeData}.

%% @doc 获取当前超步
-spec get_superstep(checkpoint()) -> non_neg_integer().
get_superstep(#graph_checkpoint{superstep = Superstep}) ->
    Superstep.

%% @doc 获取全局状态
-spec get_global_state(checkpoint()) -> map().
get_global_state(#graph_checkpoint{global_state = GlobalState}) ->
    GlobalState.

%% @doc 标记顶点需要重试
-spec retry_vertex(manager(), binary(), atom()) ->
    {ok, checkpoint(), manager()} | {error, term()}.
retry_vertex(Mgr, RunId, VertexId) ->
    case get_latest(Mgr, RunId) of
        {ok, #graph_checkpoint{
            failed_vertices = Failed,
            pending_activations = Pending,
            retry_count = RetryCount
        } = Cp} ->
            %% 从失败列表移除，加入待激活列表
            NewFailed = lists:delete(VertexId, Failed),
            NewPending = case lists:member(VertexId, Pending) of
                true -> Pending;
                false -> [VertexId | Pending]
            end,
            NewCp = Cp#graph_checkpoint{
                failed_vertices = NewFailed,
                pending_activations = NewPending,
                retry_count = RetryCount + 1
            },
            save(Mgr, RunId, NewCp);
        {error, _} = Error ->
            Error
    end.

%% @doc 为指定顶点注入恢复数据
-spec inject_resume_data(manager(), binary(), #{atom() => term()}) ->
    {ok, checkpoint(), manager()} | {error, term()}.
inject_resume_data(Mgr, RunId, Data) ->
    case get_latest(Mgr, RunId) of
        {ok, #graph_checkpoint{resume_data = Existing} = Cp} ->
            NewResumeData = maps:merge(Existing, Data),
            NewCp = Cp#graph_checkpoint{resume_data = NewResumeData},
            save(Mgr, RunId, NewCp);
        {error, _} = Error ->
            Error
    end.

%% @doc 将 checkpoint 转换为 Pregel 恢复选项
%%
%% 返回的 map 可直接用于 pregel:start/3 的 restore_from 选项。
%% 注意：vertices 字段需要调用方结合原始图定义重建完整顶点结构。
%%
%% 返回格式:
%% ```
%% #{
%%     superstep := non_neg_integer(),
%%     global_state := map(),
%%     pending_deltas => [map()] | undefined,
%%     pending_activations => [atom()],
%%     vertices => #{atom() => vertex_state()}
%% }
%% ```
-spec to_pregel_restore_opts(checkpoint()) -> map().
to_pregel_restore_opts(#graph_checkpoint{} = Cp) ->
    Base = #{
        superstep => Cp#graph_checkpoint.superstep,
        global_state => Cp#graph_checkpoint.global_state,
        vertices => Cp#graph_checkpoint.vertices
    },
    %% 添加可选字段
    WithActivations = case Cp#graph_checkpoint.pending_activations of
        [] -> Base;
        Activations -> Base#{pending_activations => Activations}
    end,
    case Cp#graph_checkpoint.pending_deltas of
        undefined -> WithActivations;
        Deltas -> WithActivations#{pending_deltas => Deltas}
    end.

%%====================================================================
%% Timeline 行为回调 - 条目访问器
%%====================================================================

%% @private
entry_id(#graph_checkpoint{id = Id}) -> Id.

%% @private
entry_owner_id(#graph_checkpoint{run_id = RunId}) -> RunId.

%% @private
entry_parent_id(#graph_checkpoint{parent_id = ParentId}) -> ParentId.

%% @private
entry_version(#graph_checkpoint{version = Version}) -> Version.

%% @private
entry_branch_id(#graph_checkpoint{branch_id = BranchId}) -> BranchId.

%% @private
entry_created_at(#graph_checkpoint{created_at = CreatedAt}) -> CreatedAt.

%% @private
entry_state(#graph_checkpoint{} = Cp) ->
    #{
        superstep => Cp#graph_checkpoint.superstep,
        iteration => Cp#graph_checkpoint.iteration,
        vertices => Cp#graph_checkpoint.vertices,
        pending_activations => Cp#graph_checkpoint.pending_activations,
        pending_deltas => Cp#graph_checkpoint.pending_deltas,
        global_state => Cp#graph_checkpoint.global_state,
        active_vertices => Cp#graph_checkpoint.active_vertices,
        completed_vertices => Cp#graph_checkpoint.completed_vertices,
        failed_vertices => Cp#graph_checkpoint.failed_vertices,
        interrupted_vertices => Cp#graph_checkpoint.interrupted_vertices,
        checkpoint_type => Cp#graph_checkpoint.checkpoint_type,
        resumable => Cp#graph_checkpoint.resumable,
        resume_data => Cp#graph_checkpoint.resume_data,
        retry_count => Cp#graph_checkpoint.retry_count,
        graph_name => Cp#graph_checkpoint.graph_name,
        metadata => Cp#graph_checkpoint.metadata
    }.

%%====================================================================
%% Timeline 行为回调 - 条目修改器
%%====================================================================

%% @private
set_entry_id(Checkpoint, Id) ->
    Checkpoint#graph_checkpoint{id = Id}.

%% @private
set_entry_parent_id(Checkpoint, ParentId) ->
    Checkpoint#graph_checkpoint{parent_id = ParentId}.

%% @private
set_entry_version(Checkpoint, Version) ->
    Checkpoint#graph_checkpoint{version = Version}.

%% @private
set_entry_branch_id(Checkpoint, BranchId) ->
    Checkpoint#graph_checkpoint{branch_id = BranchId}.

%%====================================================================
%% Timeline 行为回调 - 工厂和转换
%%====================================================================

%% @private
new_entry(RunId, State, Opts) ->
    create_checkpoint(RunId, State, Opts).

%% @private
entry_to_state_entry(#graph_checkpoint{} = Cp) ->
    #state_entry{
        id = Cp#graph_checkpoint.id,
        owner_id = Cp#graph_checkpoint.run_id,
        parent_id = Cp#graph_checkpoint.parent_id,
        branch_id = Cp#graph_checkpoint.branch_id,
        version = Cp#graph_checkpoint.version,
        state = checkpoint_to_map(Cp),
        entry_type = graph_checkpoint,
        created_at = Cp#graph_checkpoint.created_at,
        metadata = Cp#graph_checkpoint.metadata
    }.

%% @private
state_entry_to_entry(#state_entry{} = Entry) ->
    map_to_checkpoint(Entry#state_entry.state, Entry).

%% @private
namespace() -> ?NS_GRAPH_CHECKPOINTS.

%% @private
id_prefix() -> ?CHECKPOINT_ID_PREFIX.

%% @private
entry_type() -> graph_checkpoint.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查点转 Map（用于存储）
-spec checkpoint_to_map(checkpoint()) -> map().
checkpoint_to_map(#graph_checkpoint{} = Cp) ->
    #{
        <<"superstep">> => Cp#graph_checkpoint.superstep,
        <<"iteration">> => Cp#graph_checkpoint.iteration,
        <<"vertices">> => encode_vertices(Cp#graph_checkpoint.vertices),
        <<"pending_activations">> => encode_atom_list(Cp#graph_checkpoint.pending_activations),
        <<"pending_deltas">> => Cp#graph_checkpoint.pending_deltas,
        <<"global_state">> => Cp#graph_checkpoint.global_state,
        <<"active_vertices">> => encode_atom_list(Cp#graph_checkpoint.active_vertices),
        <<"completed_vertices">> => encode_atom_list(Cp#graph_checkpoint.completed_vertices),
        <<"failed_vertices">> => encode_atom_list(Cp#graph_checkpoint.failed_vertices),
        <<"interrupted_vertices">> => encode_atom_list(Cp#graph_checkpoint.interrupted_vertices),
        <<"checkpoint_type">> => atom_to_binary(Cp#graph_checkpoint.checkpoint_type, utf8),
        <<"resumable">> => Cp#graph_checkpoint.resumable,
        <<"resume_data">> => encode_resume_data(Cp#graph_checkpoint.resume_data),
        <<"retry_count">> => Cp#graph_checkpoint.retry_count,
        <<"graph_name">> => maybe_atom_to_binary(Cp#graph_checkpoint.graph_name)
    }.

%% @private Map 转检查点（从存储恢复）
-spec map_to_checkpoint(map(), #state_entry{}) -> checkpoint().
map_to_checkpoint(State, Entry) ->
    #graph_checkpoint{
        id = Entry#state_entry.id,
        run_id = Entry#state_entry.owner_id,
        parent_id = Entry#state_entry.parent_id,
        branch_id = Entry#state_entry.branch_id,
        version = Entry#state_entry.version,
        created_at = Entry#state_entry.created_at,

        superstep = maps:get(<<"superstep">>, State, 0),
        iteration = maps:get(<<"iteration">>, State, 0),
        vertices = decode_vertices(maps:get(<<"vertices">>, State, #{})),
        pending_activations = decode_atom_list(maps:get(<<"pending_activations">>, State, [])),
        pending_deltas = maps:get(<<"pending_deltas">>, State, undefined),
        global_state = maps:get(<<"global_state">>, State, #{}),

        active_vertices = decode_atom_list(maps:get(<<"active_vertices">>, State, [])),
        completed_vertices = decode_atom_list(maps:get(<<"completed_vertices">>, State, [])),
        failed_vertices = decode_atom_list(maps:get(<<"failed_vertices">>, State, [])),
        interrupted_vertices = decode_atom_list(maps:get(<<"interrupted_vertices">>, State, [])),

        checkpoint_type = binary_to_existing_atom(
            maps:get(<<"checkpoint_type">>, State, <<"superstep">>), utf8),

        resumable = maps:get(<<"resumable">>, State, true),
        resume_data = decode_resume_data(maps:get(<<"resume_data">>, State, #{})),
        retry_count = maps:get(<<"retry_count">>, State, 0),

        graph_name = maybe_binary_to_atom(maps:get(<<"graph_name">>, State, undefined)),
        metadata = Entry#state_entry.metadata
    }.

%% @private 编码顶点状态
-spec encode_vertices(#{atom() => vertex_state()}) -> map().
encode_vertices(Vertices) ->
    maps:fold(
        fun(VertexId, VertexState, Acc) ->
            Key = atom_to_binary(VertexId, utf8),
            maps:put(Key, VertexState, Acc)
        end,
        #{},
        Vertices
    ).

%% @private 解码顶点状态
-spec decode_vertices(map()) -> #{atom() => vertex_state()}.
decode_vertices(EncodedVertices) ->
    maps:fold(
        fun(Key, VertexState, Acc) ->
            VertexId = binary_to_existing_atom(Key, utf8),
            maps:put(VertexId, VertexState, Acc)
        end,
        #{},
        EncodedVertices
    ).

%% @private 编码原子列表
-spec encode_atom_list([atom()]) -> [binary()].
encode_atom_list(Atoms) ->
    [atom_to_binary(A, utf8) || A <- Atoms].

%% @private 解码原子列表
-spec decode_atom_list([binary()]) -> [atom()].
decode_atom_list(Binaries) ->
    [binary_to_existing_atom(B, utf8) || B <- Binaries].

%% @private 编码恢复数据
-spec encode_resume_data(#{atom() => term()}) -> map().
encode_resume_data(ResumeData) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            BinKey = atom_to_binary(Key, utf8),
            maps:put(BinKey, Value, Acc)
        end,
        #{},
        ResumeData
    ).

%% @private 解码恢复数据
-spec decode_resume_data(map()) -> #{atom() => term()}.
decode_resume_data(EncodedData) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            AtomKey = binary_to_existing_atom(Key, utf8),
            maps:put(AtomKey, Value, Acc)
        end,
        #{},
        EncodedData
    ).

%% @private 安全的原子转二进制
-spec maybe_atom_to_binary(atom() | undefined) -> binary() | undefined.
maybe_atom_to_binary(undefined) -> undefined;
maybe_atom_to_binary(Atom) -> atom_to_binary(Atom, utf8).

%% @private 安全的二进制转原子
-spec maybe_binary_to_atom(binary() | undefined) -> atom() | undefined.
maybe_binary_to_atom(undefined) -> undefined;
maybe_binary_to_atom(<<>>) -> undefined;
maybe_binary_to_atom(Bin) -> binary_to_existing_atom(Bin, utf8).
