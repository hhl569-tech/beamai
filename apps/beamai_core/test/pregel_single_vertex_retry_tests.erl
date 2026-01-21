%%%-------------------------------------------------------------------
%%% @doc 单顶点重试功能测试
%%%
%%% 测试 Pregel 层原生的单顶点重试 API
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

%% 总是失败的计算函数（用于测试超过最大重试次数）
always_fail_compute_fn(FailVertexId) ->
    fun(#{vertex := V} = _Ctx) ->
        Id = pregel_vertex:id(V),
        case Id =:= FailVertexId of
            true ->
                #{status => {error, always_fails},
                  vertex => V,
                  outbox => []};
            false ->
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
%% 测试用例
%%====================================================================

%% 测试：基本重试功能 - 失败顶点可以被重试
basic_retry_test() ->
    Graph = simple_graph(),
    ComputeFn = fail_then_success_compute_fn(v1),

    %% 创建回调：遇到失败时请求重试
    Callback = fun(#{failed_count := FailedCount, failed_vertices := FailedVs}) ->
        case FailedCount > 0 of
            true ->
                VertexIds = [Id || {Id, _} <- FailedVs],
                {retry, #{vertices => VertexIds}};
            false ->
                continue
        end
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    %% 验证执行完成
    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证 v1 被重试成功
    FinalGraph = maps:get(graph, Result),
    V1 = pregel_graph:get(FinalGraph, v1),
    V1Value = pregel_vertex:value(V1),
    ?assertEqual({retried, 2}, V1Value).

%% 测试：重试时使用保存的 inbox 消息
retry_uses_saved_inbox_test() ->
    Graph = simple_graph(),
    ComputeFn = fail_then_success_with_messages_fn(v1),

    Callback = fun(#{failed_count := FailedCount, failed_vertices := FailedVs}) ->
        case FailedCount > 0 of
            true ->
                VertexIds = [Id || {Id, _} <- FailedVs],
                {retry, #{vertices => VertexIds}};
            false ->
                continue
        end
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证 v1 重试时收到了消息
    FinalGraph = maps:get(graph, Result),
    V1 = pregel_graph:get(FinalGraph, v1),
    V1Value = pregel_vertex:value(V1),

    %% 应该收到来自 v2 和 v3 的消息
    ?assertMatch({retried_with_messages, [hello, hello]}, V1Value).

%% 测试：超过最大重试次数后停止
max_retries_exceeded_test() ->
    Graph = simple_graph(),
    ComputeFn = always_fail_compute_fn(v1),

    Callback = fun(#{failed_count := FailedCount, failed_vertices := FailedVs}) ->
        case FailedCount > 0 of
            true ->
                VertexIds = [Id || {Id, _} <- FailedVs],
                %% 设置最大重试 2 次
                {retry, #{vertices => VertexIds, max_retries => 2}};
            false ->
                continue
        end
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    %% 应该因为超过最大重试次数而停止
    Status = maps:get(status, Result),
    ?assertMatch({stopped, {max_retries_exceeded, _}}, Status).

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

    Callback = fun(#{failed_count := FC, failed_vertices := FVs}) ->
        case FC > 0 of
            true -> {retry, #{vertices => [Id || {Id, _} <- FVs]}};
            false -> continue
        end
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证 v2 收到了 v1 重试后发送的消息
    FinalGraph = maps:get(graph, Result),
    V2Final = pregel_graph:get(FinalGraph, v2),
    V2Value = pregel_vertex:value(V2Final),
    ?assertEqual({received, [from_v1]}, V2Value).

%% 测试：只重试指定的顶点，其他顶点不受影响
partial_retry_test() ->
    Graph = simple_graph(),

    %% 使用 ETS 跟踪 v1 的调用次数
    Table = ets:new(partial_retry_tracker, [public, set]),

    %% v1 和 v2 都失败，但只重试 v1
    %% 注意：失败的顶点状态不会被更新，所以 v2 会保持 active
    ComputeFn = fun(#{vertex := V} = _Ctx) ->
        Id = pregel_vertex:id(V),
        case Id of
            v1 ->
                Count = case ets:lookup(Table, v1) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
                ets:insert(Table, {v1, Count + 1}),
                case Count of
                    0 -> #{status => {error, v1_fail}, vertex => V, outbox => []};
                    _ ->
                        NewV = pregel_vertex:halt(pregel_vertex:set_value(V, v1_retried)),
                        #{status => ok, vertex => NewV, outbox => []}
                end;
            v2 ->
                %% v2 总是失败
                #{status => {error, v2_fail}, vertex => V, outbox => []};
            v3 ->
                NewV = pregel_vertex:halt(pregel_vertex:set_value(V, v3_ok)),
                #{status => ok, vertex => NewV, outbox => []}
        end
    end,

    %% 只重试 v1，不重试 v2
    %% 当 v1 成功后（不在 failed_vertices 中），停止执行
    %% （因为 v2 会继续失败，但我们选择不重试它）
    Callback = fun(#{type := Type, failed_vertices := FVs}) ->
        case Type of
            initial ->
                %% 初始阶段，继续执行
                continue;
            _ ->
                case lists:keyfind(v1, 1, FVs) of
                    {v1, _} ->
                        %% v1 仍在失败列表中，重试 v1
                        {retry, #{vertices => [v1]}};
                    false ->
                        %% v1 已成功，停止执行（接受 v2 的失败状态）
                        {stop, partial_success}
                end
        end
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    %% 应该因为 partial_success 停止
    ?assertEqual({stopped, partial_success}, maps:get(status, Result)),

    FinalGraph = maps:get(graph, Result),

    %% v1 应该被重试成功
    V1Final = pregel_graph:get(FinalGraph, v1),
    ?assertEqual(v1_retried, pregel_vertex:value(V1Final)),

    %% v3 不受影响
    V3Final = pregel_graph:get(FinalGraph, v3),
    ?assertEqual(v3_ok, pregel_vertex:value(V3Final)).

%% 测试：无失败时返回 {retry, ...} 被忽略
retry_with_no_failures_test() ->
    Graph = simple_graph(),
    ComputeFn = success_compute_fn(),

    Callback = fun(_Info) ->
        %% 即使没有失败也返回 retry（应该被忽略）
        {retry, #{vertices => [v1]}}
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    %% 应该正常完成
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：回调返回 {retry, ...} 的类型验证
retry_opts_type_test() ->
    Graph = simple_graph(),
    ComputeFn = fail_then_success_compute_fn(v1),

    %% 使用完整的 retry_opts
    Callback = fun(#{failed_count := FC, failed_vertices := FVs}) ->
        case FC > 0 of
            true ->
                {retry, #{
                    vertices => [Id || {Id, _} <- FVs],
                    max_retries => 5
                }};
            false ->
                continue
        end
    end,

    Result = pregel_master:run(Graph, ComputeFn, #{
        on_superstep_complete => Callback,
        num_workers => 1
    }),

    ?assertEqual(completed, maps:get(status, Result)).

