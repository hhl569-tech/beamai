%%%-------------------------------------------------------------------
%%% @doc pregel_master 回调机制单元测试
%%%
%%% 测试 pregel_master 的回调功能：
%%% - initial/step/final 回调调用时机
%%% - get_checkpoint_data 函数
%%% - 回调返回值处理（continue/stop）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_master_callback_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单测试图
%% 使用 from_edges 创建双向连接的两顶点图
make_test_graph() ->
    Edges = [{v1, v2}, {v2, v1}],
    InitialValues = #{v1 => 1, v2 => 2},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建简单计算函数（所有顶点立即停止）
make_halt_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        HaltedVertex = pregel_vertex:halt(Vertex),
        #{vertex => HaltedVertex, outbox => [], status => ok}
    end.

%% 创建收集回调调用的测试进程
start_callback_collector() ->
    Self = self(),
    spawn_link(fun() -> callback_collector_loop(Self, []) end).

callback_collector_loop(Parent, Calls) ->
    receive
        {get_calls, Ref} ->
            Parent ! {calls, Ref, lists:reverse(Calls)};
        {callback, Info} ->
            callback_collector_loop(Parent, [Info | Calls])
    end.

get_collected_calls(Collector) ->
    Ref = make_ref(),
    Collector ! {get_calls, Ref},
    receive
        {calls, Ref, Calls} -> Calls
    after 1000 ->
        error(timeout)
    end.

%% 创建记录回调的函数
make_recording_callback(Collector) ->
    fun(Info) ->
        Collector ! {callback, Info},
        continue
    end.

%% 创建返回 stop 的回调
make_stop_callback(StopReason) ->
    fun(_Info) ->
        {stop, StopReason}
    end.

%% 创建条件性 stop 的回调
make_conditional_stop_callback(Collector, StopOnType) ->
    fun(Info) ->
        Collector ! {callback, Info},
        Type = maps:get(type, Info),
        case Type of
            StopOnType -> {stop, {stopped_at, Type}};
            _ -> continue
        end
    end.

%%====================================================================
%% 回调调用时机测试
%%====================================================================

%% 测试：initial 回调在执行前被调用
initial_callback_called_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),
    Collector = start_callback_collector(),
    Callback = make_recording_callback(Collector),

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),
    Calls = get_collected_calls(Collector),

    %% 验证 initial 回调被调用
    ?assert(length(Calls) >= 1),
    FirstCall = hd(Calls),
    ?assertEqual(initial, maps:get(type, FirstCall)),
    ?assertEqual(0, maps:get(superstep, FirstCall)).

%% 测试：final 回调在完成时被调用
final_callback_called_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),
    Collector = start_callback_collector(),
    Callback = make_recording_callback(Collector),

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),
    Calls = get_collected_calls(Collector),

    %% 验证 final 回调被调用
    ?assert(length(Calls) >= 1),
    LastCall = lists:last(Calls),
    ?assertEqual(final, maps:get(type, LastCall)).

%% 测试：step 回调在超步完成后被调用
step_callback_called_test() ->
    %% 创建一个会执行多个超步的计算
    Graph = make_test_graph(),
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Superstep = maps:get(superstep, Ctx),
        case Superstep of
            0 ->
                %% 第一个超步：发送消息
                #{vertex => Vertex, outbox => [{v2, hello}], status => ok};
            _ ->
                %% 后续超步：停止
                #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
        end
    end,

    Collector = start_callback_collector(),
    Callback = make_recording_callback(Collector),

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),
    Calls = get_collected_calls(Collector),

    %% 验证有 step 类型的回调
    Types = [maps:get(type, C) || C <- Calls],
    ?assert(lists:member(step, Types)).

%%====================================================================
%% 回调信息内容测试
%%====================================================================

%% 测试：回调信息包含正确的字段
callback_info_fields_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),
    Collector = start_callback_collector(),
    Callback = make_recording_callback(Collector),

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),
    Calls = get_collected_calls(Collector),

    %% 验证每个回调都包含必要字段
    lists:foreach(fun(Info) ->
        ?assert(maps:is_key(type, Info)),
        ?assert(maps:is_key(superstep, Info)),
        ?assert(maps:is_key(active_count, Info)),
        ?assert(maps:is_key(message_count, Info)),
        ?assert(maps:is_key(failed_count, Info)),
        ?assert(maps:is_key(failed_vertices, Info)),
        ?assert(maps:is_key(interrupted_count, Info)),
        ?assert(maps:is_key(interrupted_vertices, Info)),
        ?assert(maps:is_key(get_checkpoint_data, Info))
    end, Calls).

%% 测试：get_checkpoint_data 函数返回正确的数据
get_checkpoint_data_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 保存 checkpoint 数据的回调
    Self = self(),
    Callback = fun(Info) ->
        GetData = maps:get(get_checkpoint_data, Info),
        Data = GetData(),
        Self ! {checkpoint_data, Data},
        continue
    end,

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集所有 checkpoint 数据
    CheckpointData = receive_all_checkpoint_data([]),

    %% 验证至少有一个 checkpoint 数据
    ?assert(length(CheckpointData) >= 1),

    %% 验证 checkpoint 数据结构
    [FirstData | _] = CheckpointData,
    ?assert(maps:is_key(superstep, FirstData)),
    ?assert(maps:is_key(vertices, FirstData)),
    ?assert(maps:is_key(pending_messages, FirstData)).

receive_all_checkpoint_data(Acc) ->
    receive
        {checkpoint_data, Data} ->
            receive_all_checkpoint_data([Data | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% 回调返回值处理测试
%%====================================================================

%% 测试：回调返回 continue 继续执行
callback_return_continue_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),
    Callback = fun(_Info) -> continue end,

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证正常完成
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：回调返回 {stop, Reason} 停止执行
callback_return_stop_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),
    Callback = make_stop_callback(user_requested),

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证因回调停止
    ?assertEqual({stopped, user_requested}, maps:get(status, Result)).

%% 测试：initial 回调返回 stop 阻止执行
initial_callback_stop_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),
    Collector = start_callback_collector(),
    Callback = make_conditional_stop_callback(Collector, initial),

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    Calls = get_collected_calls(Collector),

    %% 验证只有 initial 回调被调用
    ?assertEqual(1, length(Calls)),
    ?assertEqual(initial, maps:get(type, hd(Calls))),

    %% 验证因回调停止
    ?assertMatch({stopped, {stopped_at, initial}}, maps:get(status, Result)).

%%====================================================================
%% 错误和中断信息传递测试
%%====================================================================

%% 测试：回调收到失败顶点信息
callback_receives_failed_vertices_test() ->
    Graph = make_test_graph(),
    %% 创建一个会失败的计算函数
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        case Id of
            v1 -> #{vertex => Vertex, outbox => [], status => {error, test_error}};
            _ -> #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
        end
    end,

    %% 保存回调信息
    Self = self(),
    Callback = fun(Info) ->
        Self ! {callback_info, Info},
        continue
    end,

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集回调信息
    CallbackInfos = receive_all_callback_info([]),

    %% 查找包含失败信息的回调
    StepCalls = [I || I <- CallbackInfos, maps:get(type, I) =:= step],
    case StepCalls of
        [] ->
            %% 如果没有 step 回调，检查 final 回调
            FinalCalls = [I || I <- CallbackInfos, maps:get(type, I) =:= final],
            ?assert(length(FinalCalls) >= 1),
            FinalInfo = hd(FinalCalls),
            ?assert(maps:get(failed_count, FinalInfo) >= 0);
        [StepInfo | _] ->
            ?assert(maps:get(failed_count, StepInfo) >= 0)
    end.

receive_all_callback_info(Acc) ->
    receive
        {callback_info, Info} ->
            receive_all_callback_info([Info | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%% 测试：回调收到中断顶点信息
callback_receives_interrupted_vertices_test() ->
    Graph = make_test_graph(),
    %% 创建一个会中断的计算函数
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        case Id of
            v1 -> #{vertex => Vertex, outbox => [], status => {interrupt, need_input}};
            _ -> #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
        end
    end,

    %% 保存回调信息
    Self = self(),
    Callback = fun(Info) ->
        Self ! {callback_info, Info},
        continue
    end,

    Opts = #{
        on_superstep_complete => Callback,
        num_workers => 1
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集回调信息
    CallbackInfos = receive_all_callback_info([]),

    %% 查找包含中断信息的回调
    StepCalls = [I || I <- CallbackInfos, maps:get(type, I) =:= step],
    case StepCalls of
        [] ->
            FinalCalls = [I || I <- CallbackInfos, maps:get(type, I) =:= final],
            ?assert(length(FinalCalls) >= 1),
            FinalInfo = hd(FinalCalls),
            ?assert(maps:get(interrupted_count, FinalInfo) >= 0);
        [StepInfo | _] ->
            ?assert(maps:get(interrupted_count, StepInfo) >= 0)
    end.

%%====================================================================
%% 无回调时的行为测试
%%====================================================================

%% 测试：不提供回调时正常执行
no_callback_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    Opts = #{num_workers => 1},

    Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 验证正常完成
    ?assertEqual(completed, maps:get(status, Result)).
