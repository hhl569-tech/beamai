%%%-------------------------------------------------------------------
%%% @doc pregel_master state reducer 单元测试
%%%
%%% 测试 pregel_master 的 state reducer 功能：
%%% - 默认 last_write_win 行为
%%% - 自定义 reducer（append all）
%%% - 数值求和 reducer
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_master_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单测试图（三顶点链式）
%% v1 -> v2 -> v3
make_chain_graph() ->
    Edges = [{v1, v2}, {v2, v3}],
    InitialValues = #{v1 => 1, v2 => 2, v3 => 3},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建扇形图（一个中心顶点，多个源顶点）
%% v1 -> v_center
%% v2 -> v_center
%% v3 -> v_center
make_fan_graph() ->
    Edges = [{v1, v_center}, {v2, v_center}, {v3, v_center}],
    InitialValues = #{v1 => 10, v2 => 20, v3 => 30, v_center => 0},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建简单计算函数（所有顶点立即停止）
make_halt_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        HaltedVertex = pregel_vertex:halt(Vertex),
        #{vertex => HaltedVertex, outbox => [], status => ok}
    end.

%% 创建发送消息的计算函数
%% 第一个超步：每个源顶点向 v_center 发送多条消息
%% 后续超步：停止
make_multi_message_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        case Superstep of
            0 ->
                case Id of
                    v_center ->
                        %% 中心顶点保持活跃
                        #{vertex => Vertex, outbox => [], status => ok};
                    _ ->
                        %% 源顶点发送多条消息到中心
                        Value = pregel_vertex:value(Vertex),
                        Outbox = [{v_center, Value}, {v_center, Value * 2}],
                        #{vertex => pregel_vertex:halt(Vertex), outbox => Outbox, status => ok}
                end;
            _ ->
                %% 后续超步：保存收到的消息并停止
                Messages = maps:get(messages, Ctx, []),
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end.

%% 创建发送同一目标多条消息的计算函数
make_send_multi_to_target_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        Messages = maps:get(messages, Ctx, []),
        case Superstep of
            0 ->
                case Id of
                    v1 ->
                        %% v1 向 v2 发送 3 条消息
                        Outbox = [{v2, msg1}, {v2, msg2}, {v2, msg3}],
                        #{vertex => pregel_vertex:halt(Vertex), outbox => Outbox, status => ok};
                    _ ->
                        #{vertex => Vertex, outbox => [], status => ok}
                end;
            _ ->
                %% 后续超步：保存收到的消息并停止
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end.

%%====================================================================
%% 默认 State Reducer 测试（last_write_win）
%%====================================================================

%% 测试：默认 reducer 只保留最后一条消息
default_state_reducer_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_multi_to_target_compute_fn(),

    %% 不传 state_reducer，使用默认的 last_write_win
    Opts = #{num_workers => 1},

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证执行成功
    ?assertEqual(completed, maps:get(status, Result)),

    %% 获取结果图
    FinalGraph = maps:get(graph, Result),

    %% 获取 v2 的最终值（应该只有最后一条消息）
    V2 = pregel_graph:get(FinalGraph, v2),
    V2Value = pregel_vertex:value(V2),

    %% 默认 last_write_win 应该只保留最后一条消息
    ?assertEqual([msg3], V2Value).

%%====================================================================
%% 自定义 State Reducer 测试
%%====================================================================

%% 测试：append reducer 保留所有消息
custom_state_reducer_append_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_multi_to_target_compute_fn(),

    %% 自定义 reducer：保留所有消息
    AppendReducer = fun(#{messages := Msgs}) -> Msgs end,

    Opts = #{
        num_workers => 1,
        state_reducer => AppendReducer
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证执行成功
    ?assertEqual(completed, maps:get(status, Result)),

    %% 获取结果图
    FinalGraph = maps:get(graph, Result),

    %% 获取 v2 的最终值（应该包含所有消息）
    V2 = pregel_graph:get(FinalGraph, v2),
    V2Value = pregel_vertex:value(V2),

    %% append reducer 应该保留所有消息
    ?assertEqual([msg1, msg2, msg3], V2Value).

%% 测试：数值求和 reducer
state_reducer_sum_test() ->
    Graph = make_fan_graph(),

    %% 创建发送数值消息的计算函数
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        Messages = maps:get(messages, Ctx, []),
        case Superstep of
            0 ->
                case Id of
                    v_center ->
                        #{vertex => Vertex, outbox => [], status => ok};
                    _ ->
                        %% 源顶点向中心发送自己的值
                        Value = pregel_vertex:value(Vertex),
                        #{vertex => pregel_vertex:halt(Vertex), outbox => [{v_center, Value}], status => ok}
                end;
            _ ->
                %% 保存收到的消息并停止
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end,

    %% 数值求和 reducer
    SumReducer = fun(#{messages := Msgs}) -> [lists:sum(Msgs)] end,

    Opts = #{
        num_workers => 1,
        state_reducer => SumReducer
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证执行成功
    ?assertEqual(completed, maps:get(status, Result)),

    %% 获取结果图
    FinalGraph = maps:get(graph, Result),

    %% 获取 v_center 的最终值
    VCenter = pregel_graph:get(FinalGraph, v_center),
    VCenterValue = pregel_vertex:value(VCenter),

    %% 求和 reducer：10 + 20 + 30 = 60
    ?assertEqual([60], VCenterValue).

%% 测试：根据 superstep 决定策略的动态 reducer
state_reducer_dynamic_test() ->
    Graph = make_chain_graph(),

    %% 创建发送多条消息的计算函数
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        Messages = maps:get(messages, Ctx, []),
        case Superstep of
            0 ->
                case Id of
                    v1 ->
                        Outbox = [{v2, a}, {v2, b}, {v2, c}],
                        #{vertex => pregel_vertex:halt(Vertex), outbox => Outbox, status => ok};
                    _ ->
                        #{vertex => Vertex, outbox => [], status => ok}
                end;
            _ ->
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end,

    %% 动态 reducer：superstep 0 保留所有，其他只保留最后一个
    DynamicReducer = fun(#{superstep := S, messages := Msgs}) ->
        case S of
            0 -> Msgs;
            _ -> [lists:last(Msgs)]
        end
    end,

    Opts = #{
        num_workers => 1,
        state_reducer => DynamicReducer
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证执行成功
    ?assertEqual(completed, maps:get(status, Result)),

    %% 获取结果图
    FinalGraph = maps:get(graph, Result),

    %% 在 superstep 0 发送消息，应该保留所有
    V2 = pregel_graph:get(FinalGraph, v2),
    V2Value = pregel_vertex:value(V2),

    %% superstep 0 时应该保留所有消息
    ?assertEqual([a, b, c], V2Value).

%% 测试：reducer 可以访问 target_vertex
state_reducer_target_vertex_test() ->
    Graph = make_chain_graph(),

    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        Messages = maps:get(messages, Ctx, []),
        case Superstep of
            0 ->
                case Id of
                    v1 ->
                        %% 向 v2 和 v3 发送消息
                        Outbox = [{v2, msg_to_v2}, {v3, msg_to_v3}],
                        #{vertex => pregel_vertex:halt(Vertex), outbox => Outbox, status => ok};
                    v2 ->
                        %% v2 也向 v3 发送消息
                        Outbox = [{v3, another_msg_to_v3}],
                        #{vertex => Vertex, outbox => Outbox, status => ok};
                    _ ->
                        #{vertex => Vertex, outbox => [], status => ok}
                end;
            _ ->
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end,

    %% 记录 target_vertex 的 reducer
    Self = self(),
    TargetRecordingReducer = fun(#{target_vertex := Target, messages := Msgs}) ->
        Self ! {reducer_called, Target, Msgs},
        Msgs
    end,

    Opts = #{
        num_workers => 1,
        state_reducer => TargetRecordingReducer
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集所有 reducer 调用
    ReducerCalls = receive_all_reducer_calls([]),

    %% 验证 reducer 被调用时收到了正确的 target_vertex
    Targets = [T || {reducer_called, T, _} <- ReducerCalls],
    ?assert(lists:member(v2, Targets) orelse lists:member(v3, Targets)).

receive_all_reducer_calls(Acc) ->
    receive
        {reducer_called, Target, Msgs} ->
            receive_all_reducer_calls([{reducer_called, Target, Msgs} | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% 边界情况测试
%%====================================================================

%% 测试：空消息列表
state_reducer_empty_messages_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 自定义 reducer
    AppendReducer = fun(#{messages := Msgs}) -> Msgs end,

    Opts = #{
        num_workers => 1,
        state_reducer => AppendReducer
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证执行成功
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：reducer 返回空列表
state_reducer_returns_empty_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_multi_to_target_compute_fn(),

    %% 过滤掉所有消息的 reducer
    FilterReducer = fun(#{messages := _Msgs}) -> [] end,

    Opts = #{
        num_workers => 1,
        state_reducer => FilterReducer
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证执行成功
    ?assertEqual(completed, maps:get(status, Result)),

    %% 获取结果图
    FinalGraph = maps:get(graph, Result),

    %% v2 不应该收到任何消息
    V2 = pregel_graph:get(FinalGraph, v2),
    V2Value = pregel_vertex:value(V2),

    %% reducer 返回空列表，v2 不应该收到消息
    ?assertEqual([], V2Value).
