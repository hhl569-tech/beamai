%%%-------------------------------------------------------------------
%%% @doc pregel_worker 错误处理单元测试
%%%
%%% 测试 pregel_worker 的错误处理能力：
%%% - compute_result 数据结构契约
%%% - compute_vertices 检查 status 字段
%%% - notify_master_done 上报失败信息
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_worker_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建测试用的顶点
make_test_vertex(Id, Value) ->
    pregel_vertex:new(Id, Value).

%% 创建成功的计算函数
%% 计算函数返回 #{vertex, outbox, status => ok}
make_success_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        %% 成功时：更新顶点值，发送消息，状态为 ok
        NewValue = increment_value(pregel_vertex:value(Vertex)),
        NewVertex = pregel_vertex:set_value(Vertex, NewValue),
        #{
            vertex => pregel_vertex:halt(NewVertex),
            outbox => [{target_id, {msg, NewValue}}],
            status => ok
        }
    end.

%% 创建失败的计算函数
%% 计算函数返回 #{vertex, outbox => [], status => {error, Reason}}
make_failure_compute_fn(Reason) ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        #{
            vertex => Vertex,
            outbox => [],
            status => {error, Reason}
        }
    end.

%% 创建部分失败的计算函数（根据顶点 ID 决定成功或失败）
make_partial_failure_compute_fn(FailIds) ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        case lists:member(Id, FailIds) of
            true ->
                #{
                    vertex => Vertex,
                    outbox => [],
                    status => {error, {vertex_error, Id}}
                };
            false ->
                NewValue = increment_value(pregel_vertex:value(Vertex)),
                NewVertex = pregel_vertex:set_value(Vertex, NewValue),
                #{
                    vertex => pregel_vertex:halt(NewVertex),
                    outbox => [{target_id, {msg, NewValue}}],
                    status => ok
                }
        end
    end.

%% 辅助函数：增加顶点值
increment_value(undefined) -> 1;
increment_value(N) when is_integer(N) -> N + 1;
increment_value(Other) -> Other.

%%====================================================================
%% compute_vertices 测试
%%====================================================================

%% 测试：所有顶点计算成功
compute_vertices_all_success_test() ->
    %% 准备：创建两个活跃顶点
    V1 = pregel_vertex:activate(make_test_vertex(v1, 0)),
    V2 = pregel_vertex:activate(make_test_vertex(v2, 10)),
    ActiveVertices = #{v1 => V1, v2 => V2},
    AllVertices = ActiveVertices,
    Inbox = #{},
    ComputeFn = make_success_compute_fn(),

    %% 执行
    {NewVertices, Outbox, FailedVertices} = pregel_worker:compute_vertices(
        ActiveVertices, AllVertices, Inbox, ComputeFn, 0, 2
    ),

    %% 验证：无失败顶点
    ?assertEqual([], FailedVertices),
    %% 验证：顶点值已更新
    ?assertEqual(1, pregel_vertex:value(maps:get(v1, NewVertices))),
    ?assertEqual(11, pregel_vertex:value(maps:get(v2, NewVertices))),
    %% 验证：有输出消息
    ?assertEqual(2, length(Outbox)).

%% 测试：所有顶点计算失败
compute_vertices_all_failure_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1, 0)),
    V2 = pregel_vertex:activate(make_test_vertex(v2, 10)),
    ActiveVertices = #{v1 => V1, v2 => V2},
    AllVertices = ActiveVertices,
    Inbox = #{},
    ComputeFn = make_failure_compute_fn(test_error),

    %% 执行
    {NewVertices, Outbox, FailedVertices} = pregel_worker:compute_vertices(
        ActiveVertices, AllVertices, Inbox, ComputeFn, 0, 2
    ),

    %% 验证：两个失败顶点
    ?assertEqual(2, length(FailedVertices)),
    %% 验证：顶点值未更新（保持原值）
    ?assertEqual(0, pregel_vertex:value(maps:get(v1, NewVertices))),
    ?assertEqual(10, pregel_vertex:value(maps:get(v2, NewVertices))),
    %% 验证：无输出消息
    ?assertEqual([], Outbox),
    %% 验证：失败信息包含错误原因
    FailedIds = [Id || {Id, _Reason} <- FailedVertices],
    ?assert(lists:member(v1, FailedIds)),
    ?assert(lists:member(v2, FailedIds)).

%% 测试：部分顶点失败
compute_vertices_partial_failure_test() ->
    %% 准备：v2 会失败
    V1 = pregel_vertex:activate(make_test_vertex(v1, 0)),
    V2 = pregel_vertex:activate(make_test_vertex(v2, 10)),
    V3 = pregel_vertex:activate(make_test_vertex(v3, 20)),
    ActiveVertices = #{v1 => V1, v2 => V2, v3 => V3},
    AllVertices = ActiveVertices,
    Inbox = #{},
    ComputeFn = make_partial_failure_compute_fn([v2]),  %% v2 会失败

    %% 执行
    {NewVertices, Outbox, FailedVertices} = pregel_worker:compute_vertices(
        ActiveVertices, AllVertices, Inbox, ComputeFn, 0, 3
    ),

    %% 验证：只有 v2 失败
    ?assertEqual(1, length(FailedVertices)),
    [{FailedId, _FailedReason}] = FailedVertices,
    ?assertEqual(v2, FailedId),
    %% 验证：v1, v3 成功更新
    ?assertEqual(1, pregel_vertex:value(maps:get(v1, NewVertices))),
    ?assertEqual(21, pregel_vertex:value(maps:get(v3, NewVertices))),
    %% 验证：v2 保持原值
    ?assertEqual(10, pregel_vertex:value(maps:get(v2, NewVertices))),
    %% 验证：只有成功顶点发送消息
    ?assertEqual(2, length(Outbox)).

%%====================================================================
%% notify_master_done 结果验证测试
%%====================================================================

%% 由于 notify_master_done 是发送消息给 Master，我们需要通过
%% 完整的 Worker 流程来测试。这里创建一个假的 Master 来接收消息。

%% 测试辅助：启动 Worker 并执行超步
start_worker_for_test(Vertices, ComputeFn) ->
    %% 创建一个假的 Master（就是当前测试进程）
    Master = self(),
    Opts = #{
        worker_id => 0,
        master => Master,
        vertices => Vertices,
        compute_fn => ComputeFn,
        num_workers => 1,
        num_vertices => maps:size(Vertices),
        worker_pids => #{}
    },
    {ok, Worker} = pregel_worker:start_link(0, Opts),
    Worker.

%% 测试：Worker 上报失败信息（无失败）
worker_done_no_failures_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1, 0)),
    Vertices = #{v1 => V1},
    ComputeFn = make_success_compute_fn(),
    Worker = start_worker_for_test(Vertices, ComputeFn),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0),

    %% 等待 Worker 完成并发送结果
    %% 注意：gen_server:cast 发送的消息格式是 {'$gen_cast', Msg}
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证结果包含失败信息字段
            ?assertEqual(0, maps:get(failed_count, Result)),
            ?assertEqual([], maps:get(failed_vertices, Result)),
            ?assertEqual(0, maps:get(active_count, Result)),
            ?assertEqual(1, maps:get(message_count, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%% 测试：Worker 上报失败信息（有失败）
worker_done_with_failures_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1, 0)),
    V2 = pregel_vertex:activate(make_test_vertex(v2, 10)),
    Vertices = #{v1 => V1, v2 => V2},
    ComputeFn = make_partial_failure_compute_fn([v2]),  %% v2 失败
    Worker = start_worker_for_test(Vertices, ComputeFn),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0),

    %% 等待 Worker 完成
    %% 注意：gen_server:cast 发送的消息格式是 {'$gen_cast', Msg}
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证失败计数
            ?assertEqual(1, maps:get(failed_count, Result)),
            %% 验证失败顶点列表
            FailedVertices = maps:get(failed_vertices, Result),
            ?assertEqual(1, length(FailedVertices)),
            [{FailedId, _Reason}] = FailedVertices,
            ?assertEqual(v2, FailedId)
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%% 测试：Worker 上报失败信息（全部失败）
worker_done_all_failures_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1, 0)),
    V2 = pregel_vertex:activate(make_test_vertex(v2, 10)),
    Vertices = #{v1 => V1, v2 => V2},
    ComputeFn = make_failure_compute_fn(all_failed),
    Worker = start_worker_for_test(Vertices, ComputeFn),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0),

    %% 等待 Worker 完成
    %% 注意：gen_server:cast 发送的消息格式是 {'$gen_cast', Msg}
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证全部失败
            ?assertEqual(2, maps:get(failed_count, Result)),
            ?assertEqual(0, maps:get(message_count, Result)),
            FailedVertices = maps:get(failed_vertices, Result),
            ?assertEqual(2, length(FailedVertices))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%%====================================================================
%% 类型契约验证测试
%%====================================================================

%% 测试：compute_result 结构验证 - 成功
compute_result_success_structure_test() ->
    Result = #{
        vertex => make_test_vertex(v1, 10),
        outbox => [{v2, msg1}],
        status => ok
    },
    %% 验证必需字段存在
    ?assert(maps:is_key(vertex, Result)),
    ?assert(maps:is_key(outbox, Result)),
    ?assert(maps:is_key(status, Result)),
    ?assertEqual(ok, maps:get(status, Result)).

%% 测试：compute_result 结构验证 - 失败
compute_result_failure_structure_test() ->
    Result = #{
        vertex => make_test_vertex(v1, 10),
        outbox => [],
        status => {error, some_reason}
    },
    ?assert(maps:is_key(status, Result)),
    ?assertMatch({error, _}, maps:get(status, Result)).
