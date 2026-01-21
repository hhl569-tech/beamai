%%%-------------------------------------------------------------------
%%% @doc 单顶点重试功能测试
%%%
%%% 测试 Pregel 层步进式 API 的重试功能
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_single_vertex_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助宏
%%====================================================================

-define(TIMEOUT, 5000).

%%====================================================================
%% 测试数据生成
%%====================================================================

%% 创建简单测试图（三个独立顶点，无边）
simple_graph() ->
    Edges = [],
    InitialValues = #{v1 => 0, v2 => 0, v3 => 0},
    pregel_graph:from_edges(Edges, InitialValues).

%% 总是成功的计算函数
success_compute_fn() ->
    fun(#{vertex := V, messages := Msgs} = _Ctx) ->
        OldValue = pregel_vertex:value(V),
        NewValue = OldValue + length(Msgs) + 1,
        NewVertex = pregel_vertex:set_value(
            pregel_vertex:halt(V),
            NewValue
        ),
        #{status => ok, vertex => NewVertex, outbox => []}
    end.

%% 第一次失败，第二次成功的计算函数
%% 使用 ETS 表跟踪重试次数（因为失败时顶点状态不会被更新）
fail_then_success_compute_fn(FailVertexId) ->
    %% 创建 ETS 表来跟踪调用次数
    Table = ets:new(retry_tracker, [public, set]),
    fun(#{vertex := V} = _Ctx) ->
        Id = pregel_vertex:id(V),
        case Id =:= FailVertexId of
            true ->
                %% 获取并更新调用次数
                Count = case ets:lookup(Table, Id) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
                ets:insert(Table, {Id, Count + 1}),
                case Count of
                    0 ->
                        %% 第一次：失败
                        #{status => {error, first_attempt_failed},
                          vertex => V,
                          outbox => []};
                    _ ->
                        %% 第二次及以后：成功
                        NewVertex = pregel_vertex:set_value(
                            pregel_vertex:halt(V),
                            {retried, Count + 1}
                        ),
                        #{status => ok, vertex => NewVertex, outbox => []}
                end;
            false ->
                %% 其他顶点：成功
                NewVertex = pregel_vertex:set_value(
                    pregel_vertex:halt(V),
                    success
                ),
                #{status => ok, vertex => NewVertex, outbox => []}
        end
    end.

%% 带消息的计算函数（失败后重试时需要消息）
%% 使用 ETS 表跟踪重试次数
fail_then_success_with_messages_fn(FailVertexId) ->
    Table = ets:new(retry_tracker_msgs, [public, set]),
    fun(#{vertex := V, messages := Msgs} = _Ctx) ->
        Id = pregel_vertex:id(V),
        case Id =:= FailVertexId of
            true ->
                Count = case ets:lookup(Table, Id) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
                ets:insert(Table, {Id, Count + 1}),
                case Count of
                    0 ->
                        #{status => {error, first_attempt_failed},
                          vertex => V,
                          outbox => []};
                    _ ->
                        %% 重试成功，使用消息
                        NewVertex = pregel_vertex:set_value(
                            pregel_vertex:halt(V),
                            {retried_with_messages, Msgs}
                        ),
                        #{status => ok, vertex => NewVertex, outbox => []}
                end;
            false ->
                NewVertex = pregel_vertex:set_value(
                    pregel_vertex:halt(V),
                    success
                ),
                %% 发送消息给 FailVertexId
                #{status => ok, vertex => NewVertex, outbox => [{FailVertexId, hello}]}
        end
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% 运行步进式 Pregel，遇到失败时重试
run_with_retry(Master) ->
    run_with_retry(Master, 10).  %% 最大重试 10 次

run_with_retry(_Master, MaxRetries) when MaxRetries =< 0 ->
    %% 超过最大重试次数
    {error, max_retries_exceeded};
run_with_retry(Master, MaxRetries) ->
    case pregel_master:step(Master) of
        {continue, #{failed_count := FC, failed_vertices := FVs}} when FC > 0 ->
            %% 有失败的顶点，重试
            VertexIds = [Id || {Id, _} <- FVs],
            case pregel_master:retry(Master, VertexIds) of
                {continue, _Info} ->
                    run_with_retry(Master, MaxRetries - 1);
                {done, _Reason, _Info} ->
                    pregel_master:get_result(Master)
            end;
        {continue, _Info} ->
            %% 无失败，继续下一步
            run_with_retry(Master, MaxRetries);
        {done, _Reason, _Info} ->
            pregel_master:get_result(Master)
    end.

%%====================================================================
%% 测试用例
%%====================================================================

%% 测试：基本重试功能 - 失败顶点可以被重试
basic_retry_test() ->
    Graph = simple_graph(),
    ComputeFn = fail_then_success_compute_fn(v1),

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        Result = run_with_retry(Master),

        %% 验证执行完成
        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 v1 被重试成功
        FinalGraph = maps:get(graph, Result),
        V1 = pregel_graph:get(FinalGraph, v1),
        V1Value = pregel_vertex:value(V1),
        ?assertEqual({retried, 2}, V1Value)
    after
        pregel_master:stop(Master)
    end.

%% 测试：重试时使用保存的 inbox 消息
%% 注意：默认 state_reducer 使用 last_write_win，多条相同消息会被合并
%% 如需保留所有消息，需要使用自定义 state_reducer
retry_uses_saved_inbox_test() ->
    Graph = simple_graph(),
    ComputeFn = fail_then_success_with_messages_fn(v1),

    %% 使用 append reducer 保留所有消息
    AppendReducer = fun(#{messages := Msgs}) -> Msgs end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        state_reducer => AppendReducer
    }),
    try
        Result = run_with_retry(Master),

        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 v1 重试时收到了消息
        FinalGraph = maps:get(graph, Result),
        V1 = pregel_graph:get(FinalGraph, v1),
        V1Value = pregel_vertex:value(V1),

        %% 使用 append reducer，应该收到来自 v2 和 v3 的所有消息
        ?assertMatch({retried_with_messages, [hello, hello]}, V1Value)
    after
        pregel_master:stop(Master)
    end.

%% 测试：重试成功后 outbox 消息被正确路由
retry_outbox_routing_test() ->
    %% 创建图：v1 -> v2 -> v3
    Edges = [],
    InitialValues = #{v1 => start, v2 => 0, v3 => 0},
    Graph = pregel_graph:from_edges(Edges, InitialValues),

    %% 使用 ETS 跟踪 v1 的调用次数
    Table = ets:new(outbox_routing_tracker, [public, set]),

    %% v1 第一次失败，重试后发消息给 v2
    ComputeFn = fun(#{vertex := V, messages := Msgs, superstep := Step} = _Ctx) ->
        Id = pregel_vertex:id(V),
        case {Id, Step} of
            {v1, 0} ->
                Count = case ets:lookup(Table, v1) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
                ets:insert(Table, {v1, Count + 1}),
                case Count of
                    0 ->
                        #{status => {error, first_fail}, vertex => V, outbox => []};
                    _ ->
                        NewV = pregel_vertex:halt(pregel_vertex:set_value(V, retried)),
                        #{status => ok, vertex => NewV, outbox => [{v2, from_v1}]}
                end;
            {v2, _} ->
                NewV = pregel_vertex:halt(pregel_vertex:set_value(V, {received, Msgs})),
                #{status => ok, vertex => NewV, outbox => []};
            _ ->
                NewV = pregel_vertex:halt(V),
                #{status => ok, vertex => NewV, outbox => []}
        end
    end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        Result = run_with_retry(Master),

        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 v2 收到了 v1 重试后发送的消息
        FinalGraph = maps:get(graph, Result),
        V2Final = pregel_graph:get(FinalGraph, v2),
        V2Value = pregel_vertex:value(V2Final),
        ?assertEqual({received, [from_v1]}, V2Value)
    after
        pregel_master:stop(Master)
    end.

%% 测试：pregel:run 简化 API 正常工作（无失败情况）
simple_run_test() ->
    Graph = simple_graph(),
    ComputeFn = success_compute_fn(),

    Result = pregel:run(Graph, ComputeFn, #{num_workers => 1}),

    %% 应该正常完成
    ?assertEqual(completed, maps:get(status, Result)).
