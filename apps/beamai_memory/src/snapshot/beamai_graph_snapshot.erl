%%%-------------------------------------------------------------------
%%% @doc Graph Snapshot 模块 - Graph Engine 专用
%%%
%%% 为 Graph Engine (Pregel) 提供快照功能：
%%% - 保存/恢复图执行状态
%%% - 时间旅行（回退/前进超步）
%%% - 分支管理
%%% - 顶点重试
%%%
%%% == 实现 beamai_snapshot 行为 ==
%%%
%%% 本模块实现 beamai_snapshot 行为，提供 Graph 特定的
%%% 条目访问器、修改器和工厂函数。
%%%
%%% == 时间旅行模型 ==
%%%
%%% Graph Engine 的时间线由超步驱动：
%%% ```
%%% superstep_0 → superstep_1 → superstep_2 → superstep_3 → superstep_4
%%%      │            │             │             │             │
%%%     sn_1         sn_2          sn_3          sn_4          sn_5
%%%                   ↑                           ↑
%%%                   └─── go_back(2) ────────────┘
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_snapshot).

-behaviour(beamai_snapshot).

-include_lib("beamai_memory/include/beamai_graph_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 类型导出
-export_type([
    graph_snapshot/0,
    manager/0,
    graph_snapshot_type/0,
    vertex_state/0
]).

%% 构造函数
-export([
    new/1,
    new/2
]).

%% 核心操作（委托给 snapshot）
-export([
    save/3,
    load/2,
    delete/2,
    get_latest/2
]).

%% 时间旅行（委托给 snapshot）
-export([
    go_back/3,
    go_forward/3,
    goto/3,
    undo/2,
    redo/2,
    get_current_position/2
]).

%% 分支管理（委托给 snapshot）
-export([
    fork_from/4,
    fork_from/5,
    list_branches/1,
    switch_branch/2
]).

%% 历史查询（委托给 snapshot）
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
    create_graph_snapshot/2,
    create_graph_snapshot/3,
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

%% Snapshot 行为回调
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

-type graph_snapshot() :: #graph_snapshot{}.
-type manager() :: beamai_snapshot:manager().

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Graph Snapshot 管理器
-spec new(beamai_state_store:store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

%% @doc 创建 Graph Snapshot 管理器（带选项）
-spec new(beamai_state_store:store(), map()) -> manager().
new(StateStore, Opts) ->
    beamai_snapshot:new(?MODULE, StateStore, Opts).

%%====================================================================
%% 核心操作
%%====================================================================

%% @doc 保存快照
-spec save(manager(), binary(), graph_snapshot()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
save(Mgr, RunId, Snapshot) ->
    beamai_snapshot:save(Mgr, RunId, Snapshot).

%% @doc 加载快照
-spec load(manager(), binary()) -> {ok, graph_snapshot()} | {error, term()}.
load(Mgr, SnapshotId) ->
    beamai_snapshot:load(Mgr, SnapshotId).

%% @doc 删除快照
-spec delete(manager(), binary()) -> {ok, manager()} | {error, term()}.
delete(Mgr, SnapshotId) ->
    beamai_snapshot:delete(Mgr, SnapshotId).

%% @doc 获取最新快照
-spec get_latest(manager(), binary()) -> {ok, graph_snapshot()} | {error, term()}.
get_latest(Mgr, RunId) ->
    beamai_snapshot:get_latest(Mgr, RunId).

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 个超步
-spec go_back(manager(), binary(), pos_integer()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
go_back(Mgr, RunId, Steps) ->
    beamai_snapshot:go_back(Mgr, RunId, Steps).

%% @doc 前进 N 个超步
-spec go_forward(manager(), binary(), pos_integer()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
go_forward(Mgr, RunId, Steps) ->
    beamai_snapshot:go_forward(Mgr, RunId, Steps).

%% @doc 跳转到指定快照
-spec goto(manager(), binary(), binary()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
goto(Mgr, RunId, SnapshotId) ->
    beamai_snapshot:goto(Mgr, RunId, SnapshotId).

%% @doc 撤销
-spec undo(manager(), binary()) -> {ok, graph_snapshot(), manager()} | {error, term()}.
undo(Mgr, RunId) ->
    beamai_snapshot:undo(Mgr, RunId).

%% @doc 重做
-spec redo(manager(), binary()) -> {ok, graph_snapshot(), manager()} | {error, term()}.
redo(Mgr, RunId) ->
    beamai_snapshot:redo(Mgr, RunId).

%% @doc 获取当前位置
-spec get_current_position(manager(), binary()) ->
    {ok, beamai_snapshot:position()} | {error, term()}.
get_current_position(Mgr, RunId) ->
    beamai_snapshot:get_current_position(Mgr, RunId).

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 从指定快照创建分支
-spec fork_from(manager(), binary(), binary(), binary()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
fork_from(Mgr, SnapshotId, NewBranchName, RunId) ->
    beamai_snapshot:fork_from(Mgr, SnapshotId, NewBranchName, RunId).

-spec fork_from(manager(), binary(), binary(), binary(), map()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
fork_from(Mgr, SnapshotId, NewBranchName, RunId, Opts) ->
    beamai_snapshot:fork_from(Mgr, SnapshotId, NewBranchName, RunId, Opts).

%% @doc 列出所有分支
-spec list_branches(manager()) -> [beamai_snapshot:branch()].
list_branches(Mgr) ->
    beamai_snapshot:list_branches(Mgr).

%% @doc 切换分支
-spec switch_branch(manager(), binary()) -> {ok, manager()} | {error, term()}.
switch_branch(Mgr, BranchId) ->
    beamai_snapshot:switch_branch(Mgr, BranchId).

%%====================================================================
%% 历史查询
%%====================================================================

%% @doc 获取执行路径（历史记录）
-spec get_history(manager(), binary()) -> {ok, [graph_snapshot()]} | {error, term()}.
get_history(Mgr, RunId) ->
    beamai_snapshot:get_history(Mgr, RunId).

-spec get_history(manager(), binary(), map()) -> {ok, [graph_snapshot()]} | {error, term()}.
get_history(Mgr, RunId, Opts) ->
    beamai_snapshot:get_history(Mgr, RunId, Opts).

%% @doc 获取血统
-spec get_lineage(manager(), binary()) -> {ok, [graph_snapshot()]} | {error, term()}.
get_lineage(Mgr, SnapshotId) ->
    beamai_snapshot:get_lineage(Mgr, SnapshotId).

%% @doc 比较两个快照
-spec compare(manager(), binary(), binary()) ->
    {ok, #{from := graph_snapshot(), to := graph_snapshot(), version_diff := integer()}} |
    {error, term()}.
compare(Mgr, SnapshotId1, SnapshotId2) ->
    beamai_snapshot:compare(Mgr, SnapshotId1, SnapshotId2).

%%====================================================================
%% Graph 专用操作
%%====================================================================

%% @doc 从 Pregel 状态创建并保存快照
-spec save_from_pregel(manager(), binary(), map()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
save_from_pregel(Mgr, RunId, PregelState) ->
    save_from_pregel(Mgr, RunId, PregelState, #{}).

-spec save_from_pregel(manager(), binary(), map(), map()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
save_from_pregel(Mgr, RunId, PregelState, Opts) ->
    Snapshot = create_graph_snapshot(RunId, PregelState, Opts),
    save(Mgr, RunId, Snapshot).

%% @doc 创建快照（不保存）
-spec create_graph_snapshot(binary(), map()) -> graph_snapshot().
create_graph_snapshot(RunId, PregelState) ->
    create_graph_snapshot(RunId, PregelState, #{}).

-spec create_graph_snapshot(binary(), map(), map()) -> graph_snapshot().
create_graph_snapshot(RunId, PregelState, Opts) ->
    Now = erlang:system_time(millisecond),

    #graph_snapshot{
        id = undefined,  %% 由 snapshot 分配
        run_id = RunId,
        parent_id = undefined,  %% 由 snapshot 设置
        branch_id = maps:get(branch_id, Opts, ?DEFAULT_GRAPH_SNAPSHOT_BRANCH),
        version = 0,  %% 由 snapshot 设置
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

        %% 快照类型
        snapshot_type = maps:get(snapshot_type, Opts, superstep),

        %% 恢复信息
        resumable = maps:get(resumable, Opts, true),
        resume_data = maps:get(resume_data, Opts, #{}),
        retry_count = maps:get(retry_count, Opts, 0),

        %% 扩展信息
        graph_name = maps:get(graph_name, Opts, undefined),
        metadata = maps:get(metadata, Opts, #{})
    }.

%% @doc 获取指定顶点的状态
-spec get_vertex_state(graph_snapshot(), atom()) -> {ok, vertex_state()} | {error, not_found}.
get_vertex_state(#graph_snapshot{vertices = Vertices}, VertexId) ->
    case maps:find(VertexId, Vertices) of
        {ok, State} -> {ok, State};
        error -> {error, not_found}
    end.

%% @doc 获取所有顶点
-spec get_vertices(graph_snapshot()) -> #{atom() => vertex_state()}.
get_vertices(#graph_snapshot{vertices = Vertices}) ->
    Vertices.

%% @doc 获取活跃顶点列表
-spec get_active_vertices(graph_snapshot()) -> [atom()].
get_active_vertices(#graph_snapshot{active_vertices = Active}) ->
    Active.

%% @doc 获取失败顶点列表
-spec get_failed_vertices(graph_snapshot()) -> [atom()].
get_failed_vertices(#graph_snapshot{failed_vertices = Failed}) ->
    Failed.

%% @doc 获取中断顶点列表
-spec get_interrupted_vertices(graph_snapshot()) -> [atom()].
get_interrupted_vertices(#graph_snapshot{interrupted_vertices = Interrupted}) ->
    Interrupted.

%% @doc 获取待激活顶点列表
-spec get_pending_activations(graph_snapshot()) -> [atom()].
get_pending_activations(#graph_snapshot{pending_activations = Pending}) ->
    Pending.

%% @doc 获取延迟提交的 deltas
-spec get_pending_deltas(graph_snapshot()) -> [map()] | undefined.
get_pending_deltas(#graph_snapshot{pending_deltas = Deltas}) ->
    Deltas.

%% @doc 检查是否可恢复
-spec is_resumable(graph_snapshot()) -> boolean().
is_resumable(#graph_snapshot{resumable = Resumable}) ->
    Resumable.

%% @doc 获取恢复数据
-spec get_resume_data(graph_snapshot()) -> #{atom() => term()}.
get_resume_data(#graph_snapshot{resume_data = ResumeData}) ->
    ResumeData.

%% @doc 设置恢复数据
-spec set_resume_data(graph_snapshot(), #{atom() => term()}) -> graph_snapshot().
set_resume_data(Snapshot, ResumeData) ->
    Snapshot#graph_snapshot{resume_data = ResumeData}.

%% @doc 获取当前超步
-spec get_superstep(graph_snapshot()) -> non_neg_integer().
get_superstep(#graph_snapshot{superstep = Superstep}) ->
    Superstep.

%% @doc 获取全局状态
-spec get_global_state(graph_snapshot()) -> map().
get_global_state(#graph_snapshot{global_state = GlobalState}) ->
    GlobalState.

%% @doc 标记顶点需要重试
-spec retry_vertex(manager(), binary(), atom()) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
retry_vertex(Mgr, RunId, VertexId) ->
    case get_latest(Mgr, RunId) of
        {ok, #graph_snapshot{
            failed_vertices = Failed,
            pending_activations = Pending,
            retry_count = RetryCount
        } = Sn} ->
            %% 从失败列表移除，加入待激活列表
            NewFailed = lists:delete(VertexId, Failed),
            NewPending = case lists:member(VertexId, Pending) of
                true -> Pending;
                false -> [VertexId | Pending]
            end,
            NewSn = Sn#graph_snapshot{
                failed_vertices = NewFailed,
                pending_activations = NewPending,
                retry_count = RetryCount + 1
            },
            save(Mgr, RunId, NewSn);
        {error, _} = Error ->
            Error
    end.

%% @doc 为指定顶点注入恢复数据
-spec inject_resume_data(manager(), binary(), #{atom() => term()}) ->
    {ok, graph_snapshot(), manager()} | {error, term()}.
inject_resume_data(Mgr, RunId, Data) ->
    case get_latest(Mgr, RunId) of
        {ok, #graph_snapshot{resume_data = Existing} = Sn} ->
            NewResumeData = maps:merge(Existing, Data),
            NewSn = Sn#graph_snapshot{resume_data = NewResumeData},
            save(Mgr, RunId, NewSn);
        {error, _} = Error ->
            Error
    end.

%% @doc 将 graph snapshot 转换为 Pregel 恢复选项
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
-spec to_pregel_restore_opts(graph_snapshot()) -> map().
to_pregel_restore_opts(#graph_snapshot{} = Sn) ->
    Base = #{
        superstep => Sn#graph_snapshot.superstep,
        global_state => Sn#graph_snapshot.global_state,
        vertices => Sn#graph_snapshot.vertices
    },
    %% 添加可选字段
    WithActivations = case Sn#graph_snapshot.pending_activations of
        [] -> Base;
        Activations -> Base#{pending_activations => Activations}
    end,
    case Sn#graph_snapshot.pending_deltas of
        undefined -> WithActivations;
        Deltas -> WithActivations#{pending_deltas => Deltas}
    end.

%%====================================================================
%% Snapshot 行为回调 - 条目访问器
%%====================================================================

%% @private
entry_id(#graph_snapshot{id = Id}) -> Id.

%% @private
entry_owner_id(#graph_snapshot{run_id = RunId}) -> RunId.

%% @private
entry_parent_id(#graph_snapshot{parent_id = ParentId}) -> ParentId.

%% @private
entry_version(#graph_snapshot{version = Version}) -> Version.

%% @private
entry_branch_id(#graph_snapshot{branch_id = BranchId}) -> BranchId.

%% @private
entry_created_at(#graph_snapshot{created_at = CreatedAt}) -> CreatedAt.

%% @private
entry_state(#graph_snapshot{} = Sn) ->
    #{
        superstep => Sn#graph_snapshot.superstep,
        iteration => Sn#graph_snapshot.iteration,
        vertices => Sn#graph_snapshot.vertices,
        pending_activations => Sn#graph_snapshot.pending_activations,
        pending_deltas => Sn#graph_snapshot.pending_deltas,
        global_state => Sn#graph_snapshot.global_state,
        active_vertices => Sn#graph_snapshot.active_vertices,
        completed_vertices => Sn#graph_snapshot.completed_vertices,
        failed_vertices => Sn#graph_snapshot.failed_vertices,
        interrupted_vertices => Sn#graph_snapshot.interrupted_vertices,
        snapshot_type => Sn#graph_snapshot.snapshot_type,
        resumable => Sn#graph_snapshot.resumable,
        resume_data => Sn#graph_snapshot.resume_data,
        retry_count => Sn#graph_snapshot.retry_count,
        graph_name => Sn#graph_snapshot.graph_name,
        metadata => Sn#graph_snapshot.metadata
    }.

%%====================================================================
%% Snapshot 行为回调 - 条目修改器
%%====================================================================

%% @private
set_entry_id(Snapshot, Id) ->
    Snapshot#graph_snapshot{id = Id}.

%% @private
set_entry_parent_id(Snapshot, ParentId) ->
    Snapshot#graph_snapshot{parent_id = ParentId}.

%% @private
set_entry_version(Snapshot, Version) ->
    Snapshot#graph_snapshot{version = Version}.

%% @private
set_entry_branch_id(Snapshot, BranchId) ->
    Snapshot#graph_snapshot{branch_id = BranchId}.

%%====================================================================
%% Snapshot 行为回调 - 工厂和转换
%%====================================================================

%% @private
new_entry(RunId, State, Opts) ->
    create_graph_snapshot(RunId, State, Opts).

%% @private
entry_to_state_entry(#graph_snapshot{} = Sn) ->
    #state_entry{
        id = Sn#graph_snapshot.id,
        owner_id = Sn#graph_snapshot.run_id,
        parent_id = Sn#graph_snapshot.parent_id,
        branch_id = Sn#graph_snapshot.branch_id,
        version = Sn#graph_snapshot.version,
        state = graph_snapshot_to_map(Sn),
        entry_type = graph_snapshot,
        created_at = Sn#graph_snapshot.created_at,
        metadata = Sn#graph_snapshot.metadata
    }.

%% @private
state_entry_to_entry(#state_entry{} = Entry) ->
    map_to_graph_snapshot(Entry#state_entry.state, Entry).

%% @private
namespace() -> ?NS_GRAPH_SNAPSHOTS.

%% @private
id_prefix() -> ?GRAPH_SNAPSHOT_ID_PREFIX.

%% @private
entry_type() -> graph_snapshot.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 快照转 Map（用于存储）
-spec graph_snapshot_to_map(graph_snapshot()) -> map().
graph_snapshot_to_map(#graph_snapshot{} = Sn) ->
    #{
        <<"superstep">> => Sn#graph_snapshot.superstep,
        <<"iteration">> => Sn#graph_snapshot.iteration,
        <<"vertices">> => encode_vertices(Sn#graph_snapshot.vertices),
        <<"pending_activations">> => encode_atom_list(Sn#graph_snapshot.pending_activations),
        <<"pending_deltas">> => Sn#graph_snapshot.pending_deltas,
        <<"global_state">> => Sn#graph_snapshot.global_state,
        <<"active_vertices">> => encode_atom_list(Sn#graph_snapshot.active_vertices),
        <<"completed_vertices">> => encode_atom_list(Sn#graph_snapshot.completed_vertices),
        <<"failed_vertices">> => encode_atom_list(Sn#graph_snapshot.failed_vertices),
        <<"interrupted_vertices">> => encode_atom_list(Sn#graph_snapshot.interrupted_vertices),
        <<"snapshot_type">> => atom_to_binary(Sn#graph_snapshot.snapshot_type, utf8),
        <<"resumable">> => Sn#graph_snapshot.resumable,
        <<"resume_data">> => encode_resume_data(Sn#graph_snapshot.resume_data),
        <<"retry_count">> => Sn#graph_snapshot.retry_count,
        <<"graph_name">> => maybe_atom_to_binary(Sn#graph_snapshot.graph_name)
    }.

%% @private Map 转快照（从存储恢复）
-spec map_to_graph_snapshot(map(), #state_entry{}) -> graph_snapshot().
map_to_graph_snapshot(State, Entry) ->
    #graph_snapshot{
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

        snapshot_type = binary_to_existing_atom(
            maps:get(<<"snapshot_type">>, State, <<"superstep">>), utf8),

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
