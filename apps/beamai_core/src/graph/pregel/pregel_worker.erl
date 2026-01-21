%%%-------------------------------------------------------------------
%%% @doc Pregel Worker 进程模块
%%%
%%% 每个 Worker 管理一个图分区，职责包括:
%%% - 执行本地顶点的计算函数
%%% - 处理消息的本地路由和跨 Worker 转发
%%% - 与 Master 进程同步超步状态
%%%
%%% 设计模式: gen_server 行为模式 + 策略模式（计算函数）
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_worker).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).
-export([start_superstep/2, receive_messages/2]).
-export([get_state/1, get_vertices/1]).

%% 内部函数导出（用于测试）
-export([compute_vertices/6]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, context/0, compute_result/0, compute_status/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().

%% Worker 配置选项
-type opts() :: #{
    worker_id := non_neg_integer(),
    master := pid(),
    vertices := #{vertex_id() => vertex()},
    compute_fn := fun((context()) -> context()),
    combiner => pregel_combiner:spec(),
    num_workers := pos_integer(),
    num_vertices => non_neg_integer(),
    worker_pids => #{non_neg_integer() => pid()}
}.

%% 计算上下文（传递给计算函数）
-type context() :: #{
    vertex := vertex(),
    messages := [term()],
    superstep := non_neg_integer(),
    num_vertices := non_neg_integer(),
    outbox := [{vertex_id(), term()}]
}.

%% 计算结果状态
%% ok - 计算成功
%% {error, Reason} - 计算失败，Reason 为失败原因
%% {interrupt, Reason} - 计算中断，用于 human-in-the-loop 场景
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果（计算函数必须返回此结构）
%% vertex - 新顶点状态（成功时更新，失败/中断时保持原值）
%% outbox - 发出的消息（失败/中断时应为空列表）
%% status - 计算状态（必需）
-type compute_result() :: #{
    vertex := vertex(),
    outbox := [{vertex_id(), term()}],
    status := compute_status()
}.

%% 计算结果累加器（内部使用）
%% 用于 fold 过程中收集顶点计算结果
-type compute_acc() :: {
    Vertices :: #{vertex_id() => vertex()},
    Outbox :: [{vertex_id(), term()}],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
}.

%% 内部状态
-record(state, {
    worker_id     :: non_neg_integer(),    % Worker ID
    master        :: pid(),                 % Master 进程
    vertices      :: #{vertex_id() => vertex()},  % 本地顶点
    inbox         :: #{vertex_id() => [term()]},  % 收件箱
    compute_fn    :: fun((context()) -> context()),  % 计算函数
    combiner      :: pregel_combiner:spec() | undefined,  % 合并器
    superstep     :: non_neg_integer(),    % 当前超步
    num_workers   :: pos_integer(),        % Worker 总数
    num_vertices  :: non_neg_integer(),    % 全图顶点总数
    worker_pids   :: #{non_neg_integer() => pid()}  % Worker PID 映射
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Worker 进程
-spec start_link(non_neg_integer(), opts()) -> {ok, pid()} | {error, term()}.
start_link(WorkerId, Opts) ->
    gen_server:start_link(?MODULE, Opts#{worker_id => WorkerId}, []).

%% @doc 停止 Worker
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 开始新的超步
-spec start_superstep(pid(), non_neg_integer()) -> ok.
start_superstep(Pid, Superstep) ->
    gen_server:cast(Pid, {start_superstep, Superstep}).

%% @doc 接收来自其他 Worker 的消息
-spec receive_messages(pid(), [{vertex_id(), term()}]) -> ok.
receive_messages(Pid, Messages) ->
    gen_server:cast(Pid, {receive_messages, Messages}).

%% @doc 获取 Worker 状态（调试用）
-spec get_state(pid()) -> map().
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%% @doc 获取 Worker 的所有顶点
-spec get_vertices(pid()) -> #{vertex_id() => vertex()}.
get_vertices(Pid) ->
    gen_server:call(Pid, get_vertices).

%%====================================================================
%% gen_server 回调
%%====================================================================

init(Opts) ->
    #{
        worker_id := WorkerId,
        master := Master,
        vertices := Vertices,
        compute_fn := ComputeFn,
        num_workers := NumWorkers
    } = Opts,

    State = #state{
        worker_id = WorkerId,
        master = Master,
        vertices = Vertices,
        inbox = #{},
        compute_fn = ComputeFn,
        combiner = maps:get(combiner, Opts, undefined),
        superstep = 0,
        num_workers = NumWorkers,
        num_vertices = maps:get(num_vertices, Opts, maps:size(Vertices)),
        worker_pids = maps:get(worker_pids, Opts, #{})
    },
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, state_to_map(State), State};

handle_call(get_vertices, _From, #state{vertices = Vertices} = State) ->
    {reply, Vertices, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({start_superstep, Superstep}, State) ->
    NewState = execute_superstep(State#state{superstep = Superstep}),
    {noreply, NewState};

handle_cast({receive_messages, Messages}, State) ->
    NewState = add_to_inbox(Messages, State),
    {noreply, NewState};

handle_cast({update_worker_pids, WorkerPids}, State) ->
    {noreply, State#state{worker_pids = WorkerPids}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({local_messages, Messages}, State) ->
    NewState = add_to_inbox(Messages, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% 超步执行
%%====================================================================

%% @private 执行一个超步
%% 返回更新后的 Worker 状态
-spec execute_superstep(#state{}) -> #state{}.
execute_superstep(#state{
    vertices = Vertices,
    inbox = Inbox,
    compute_fn = ComputeFn,
    combiner = Combiner,
    superstep = Superstep,
    num_vertices = NumVertices
} = State) ->
    %% 1. 筛选需要计算的顶点（有消息或活跃）
    ActiveVertices = filter_active_vertices(Vertices, Inbox),

    %% 2. 执行所有顶点计算（返回失败和中断列表）
    {NewVertices, Outbox, FailedVertices, InterruptedVertices} = compute_vertices(
        ActiveVertices, Vertices, Inbox, ComputeFn, Superstep, NumVertices
    ),

    %% 3. 应用合并器（如果有）
    CombinedOutbox = apply_combiner(Outbox, Combiner),

    %% 4. 路由消息到目标 Worker
    route_messages(CombinedOutbox, State),

    %% 5. 通知 Master 完成（含失败和中断信息）
    notify_master_done(State, NewVertices, CombinedOutbox, FailedVertices, InterruptedVertices),

    %% 6. 返回更新后的状态
    State#state{vertices = NewVertices, inbox = #{}}.

%% @private 筛选需要计算的顶点
-spec filter_active_vertices(#{vertex_id() => vertex()},
                             #{vertex_id() => [term()]}) ->
    #{vertex_id() => vertex()}.
filter_active_vertices(Vertices, Inbox) ->
    InboxKeys = maps:keys(Inbox),
    maps:filter(
        fun(Id, V) ->
            lists:member(Id, InboxKeys) orelse pregel_vertex:is_active(V)
        end,
        Vertices
    ).

%% @doc 执行所有顶点计算
%%
%% 根据计算函数返回的 status 字段处理计算结果：
%% - status == ok: 更新顶点，收集 outbox
%% - status == {error, Reason}: 记录失败，不更新顶点，不发消息
%% - status == {interrupt, Reason}: 记录中断，不更新顶点，不发消息
%%
%% @returns {顶点集合, 输出消息, 失败顶点, 中断顶点}
-spec compute_vertices(
    ActiveVertices :: #{vertex_id() => vertex()},
    AllVertices :: #{vertex_id() => vertex()},
    Inbox :: #{vertex_id() => [term()]},
    ComputeFn :: fun((context()) -> compute_result()),
    Superstep :: non_neg_integer(),
    NumVertices :: non_neg_integer()
) -> compute_acc().
compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    InitAcc = {AllVertices, [], [], []},  %% {Vertices, Outbox, Failed, Interrupted}
    maps:fold(
        fun(Id, Vertex, Acc) ->
            Messages = maps:get(Id, Inbox, []),
            ActiveVertex = activate_if_has_messages(Vertex, Messages),
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),
            Result = ComputeFn(Context),
            process_compute_result(Id, Result, Acc)
        end,
        InitAcc,
        ActiveVertices
    ).

%% @private 处理单个顶点的计算结果
%% 根据 status 字段决定如何处理结果：
%% - ok: 更新顶点状态，收集输出消息
%% - error: 记录到失败列表
%% - interrupt: 记录到中断列表
-spec process_compute_result(vertex_id(), compute_result(), compute_acc()) -> compute_acc().
process_compute_result(Id, #{status := ok, vertex := NewVertex, outbox := Out},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 成功：更新顶点，收集消息
    {VAcc#{Id => NewVertex}, Out ++ OAcc, FailedAcc, InterruptedAcc};
process_compute_result(Id, #{status := {error, Reason}},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 失败：记录失败信息，不更新顶点，不收集消息
    {VAcc, OAcc, [{Id, Reason} | FailedAcc], InterruptedAcc};
process_compute_result(Id, #{status := {interrupt, Reason}},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 中断：记录中断信息，不更新顶点，不收集消息
    {VAcc, OAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]}.

%% @private 如果有消息则激活顶点
-spec activate_if_has_messages(vertex(), [term()]) -> vertex().
activate_if_has_messages(Vertex, []) -> Vertex;
activate_if_has_messages(Vertex, _) -> pregel_vertex:activate(Vertex).

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

%%====================================================================
%% 消息处理
%%====================================================================

%% @private 添加消息到收件箱
-spec add_to_inbox([{vertex_id(), term()}], #state{}) -> #state{}.
add_to_inbox(Messages, #state{inbox = Inbox} = State) ->
    NewInbox = pregel_utils:merge_message_groups(
        Inbox,
        pregel_utils:group_messages(Messages)
    ),
    State#state{inbox = NewInbox}.

%% @private 应用合并器到消息
-spec apply_combiner([{vertex_id(), term()}], pregel_combiner:spec() | undefined) ->
    [{vertex_id(), term()}].
apply_combiner(Outbox, undefined) ->
    Outbox;
apply_combiner(Outbox, Combiner) ->
    CombinerFn = pregel_combiner:get(Combiner),
    Grouped = pregel_utils:group_messages(Outbox),
    Combined = pregel_utils:apply_to_groups(CombinerFn, Grouped),
    [{Target, Value} || {Target, Value} <- maps:to_list(Combined)].

%% @private 路由消息到目标 Worker
-spec route_messages([{vertex_id(), term()}], #state{}) -> ok.
route_messages(Outbox, #state{
    worker_id = MyId,
    num_workers = NumWorkers,
    worker_pids = WorkerPids,
    master = Master
}) ->
    %% 按目标 Worker 分组
    GroupedByWorker = group_by_target_worker(Outbox, NumWorkers),

    %% 发送到各 Worker
    maps:foreach(
        fun(TargetId, Messages) ->
            send_to_worker(TargetId, Messages, MyId, WorkerPids, Master)
        end,
        GroupedByWorker
    ).

%% @private 按目标 Worker 分组消息
-spec group_by_target_worker([{vertex_id(), term()}], pos_integer()) ->
    #{non_neg_integer() => [{vertex_id(), term()}]}.
group_by_target_worker(Messages, NumWorkers) ->
    lists:foldl(
        fun({Target, Value}, Acc) ->
            WorkerId = pregel_partition:worker_id(Target, NumWorkers, hash),
            Existing = maps:get(WorkerId, Acc, []),
            Acc#{WorkerId => [{Target, Value} | Existing]}
        end,
        #{},
        Messages
    ).

%% @private 发送消息到指定 Worker
-spec send_to_worker(non_neg_integer(), [{vertex_id(), term()}],
                     non_neg_integer(), #{non_neg_integer() => pid()}, pid()) -> ok.
send_to_worker(TargetId, Messages, MyId, WorkerPids, Master) ->
    case TargetId of
        MyId ->
            %% 本地消息
            self() ! {local_messages, Messages};
        _ ->
            %% 远程消息
            case maps:get(TargetId, WorkerPids, undefined) of
                undefined ->
                    gen_server:cast(Master, {route_messages, TargetId, Messages});
                Pid ->
                    receive_messages(Pid, Messages)
            end
    end,
    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 通知 Master 超步完成
%% 包含失败和中断顶点信息供 Master 处理
-spec notify_master_done(
    State :: #state{},
    Vertices :: #{vertex_id() => vertex()},
    Outbox :: [{vertex_id(), term()}],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
) -> ok.
notify_master_done(#state{worker_id = WorkerId, master = Master},
                   Vertices, Outbox, FailedVertices, InterruptedVertices) ->
    Result = #{
        worker_id => WorkerId,
        active_count => pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices),
        message_count => length(Outbox),
        %% 失败信息
        failed_count => length(FailedVertices),
        failed_vertices => FailedVertices,
        %% 中断信息（human-in-the-loop）
        interrupted_count => length(InterruptedVertices),
        interrupted_vertices => InterruptedVertices
    },
    gen_server:cast(Master, {worker_done, self(), Result}).

%% @private 将状态转换为 map（调试用）
-spec state_to_map(#state{}) -> map().
state_to_map(#state{
    worker_id = WorkerId,
    vertices = Vertices,
    inbox = Inbox,
    superstep = Superstep
}) ->
    #{
        worker_id => WorkerId,
        vertex_count => maps:size(Vertices),
        inbox_count => maps:size(Inbox),
        superstep => Superstep
    }.
