%%%-------------------------------------------------------------------
%%% @doc Checkpoint 测试套件
%%%
%%% 测试 Layer 2 Graph Checkpoint 功能。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_checkpoint_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_memory/include/beamai_checkpoint.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% CT 回调
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% 测试用例
-export([
    test_new/1,
    test_save_and_load/1,
    test_save_from_pregel/1,
    test_get_latest/1,
    test_time_travel_go_back/1,
    test_time_travel_go_forward/1,
    test_time_travel_goto/1,
    test_undo_redo/1,
    test_get_current_position/1,
    test_fork_from/1,
    test_branch_management/1,
    test_get_history/1,
    test_get_lineage/1,
    test_checkpoint_accessors/1,
    test_retry_vertex/1,
    test_inject_resume_data/1,
    test_pending_deltas/1,
    test_to_pregel_restore_opts/1
]).

%%====================================================================
%% CT 回调
%%====================================================================

all() ->
    [
        test_new,
        test_save_and_load,
        test_save_from_pregel,
        test_get_latest,
        test_time_travel_go_back,
        test_time_travel_go_forward,
        test_time_travel_goto,
        test_undo_redo,
        test_get_current_position,
        test_fork_from,
        test_branch_management,
        test_get_history,
        test_pending_deltas,
        test_to_pregel_restore_opts,
        test_get_lineage,
        test_checkpoint_accessors,
        test_retry_vertex,
        test_inject_resume_data
    ].

init_per_suite(Config) ->
    application:ensure_all_started(beamai_memory),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% 为每个测试创建新的环境
    StoreName = list_to_atom("checkpoint_store_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, _} = beamai_store_ets:start_link(StoreName, #{}),
    Backend = {beamai_store_ets, StoreName},
    StateStore = beamai_state_store:new(Backend, #{namespace => <<"checkpoint_test">>}),
    Mgr = beamai_checkpoint:new(StateStore, #{max_entries => 50, auto_prune => false}),
    [{mgr, Mgr}, {store_name, StoreName} | Config].

end_per_testcase(_TestCase, Config) ->
    StoreName = ?config(store_name, Config),
    catch gen_server:stop(StoreName),
    ok.

%%====================================================================
%% 测试用例
%%====================================================================

%% @doc 测试创建 Checkpoint Manager
test_new(Config) ->
    Mgr = ?config(mgr, Config),
    ?assertMatch(#{module := beamai_checkpoint, state_store := _}, Mgr).

%% @doc 测试保存和加载检查点
test_save_and_load(Config) ->
    Mgr = ?config(mgr, Config),
    RunId = <<"run_1">>,

    %% 创建检查点
    Checkpoint = beamai_checkpoint:create_checkpoint(RunId, #{
        superstep => 0,
        iteration => 0,
        vertices => #{},
        pending_activations => [],
        global_state => #{}
    }, #{checkpoint_type => initial}),

    %% 保存
    {ok, SavedCp, Mgr2} = beamai_checkpoint:save(Mgr, RunId, Checkpoint),
    ?assertNotEqual(undefined, SavedCp#graph_checkpoint.id),
    ?assertEqual(RunId, SavedCp#graph_checkpoint.run_id),

    %% 加载
    CpId = SavedCp#graph_checkpoint.id,
    {ok, LoadedCp} = beamai_checkpoint:load(Mgr2, CpId),
    ?assertEqual(CpId, LoadedCp#graph_checkpoint.id),
    ?assertEqual(0, LoadedCp#graph_checkpoint.superstep).

%% @doc 测试从 Pregel 状态保存检查点
test_save_from_pregel(Config) ->
    Mgr = ?config(mgr, Config),
    RunId = <<"run_2">>,

    PregelState = #{
        superstep => 3,
        iteration => 5,
        vertices => #{
            node_a => #{value => 10, active => true, messages => [], halt_voted => false},
            node_b => #{value => 20, active => false, messages => [], halt_voted => true}
        },
        pending_activations => [node_c],
        global_state => #{total => 30},
        active_vertices => [node_a],
        completed_vertices => [node_b],
        failed_vertices => [],
        interrupted_vertices => []
    },

    {ok, Cp, _Mgr2} = beamai_checkpoint:save_from_pregel(Mgr, RunId, PregelState, #{
        checkpoint_type => superstep,
        graph_name => test_graph
    }),

    ?assertEqual(3, Cp#graph_checkpoint.superstep),
    ?assertEqual(5, Cp#graph_checkpoint.iteration),
    ?assertEqual([node_a], Cp#graph_checkpoint.active_vertices),
    ?assertEqual([node_b], Cp#graph_checkpoint.completed_vertices).

%% @doc 测试获取最新检查点
test_get_latest(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_3">>,

    %% 保存多个检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{checkpoint_type => initial}),
    {ok, _, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{checkpoint_type => superstep}),
    {ok, Cp3, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{checkpoint_type => superstep}),

    %% 获取最新
    {ok, Latest} = beamai_checkpoint:get_latest(Mgr3, RunId),
    ?assertEqual(Cp3#graph_checkpoint.id, Latest#graph_checkpoint.id),
    ?assertEqual(2, Latest#graph_checkpoint.superstep).

%% @doc 测试时间旅行 - 回退
test_time_travel_go_back(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_4">>,

    %% 创建 3 个检查点
    {ok, Cp1, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, _, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),
    {ok, _, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{}),

    %% 回退 2 步
    {ok, BackCp, _Mgr4} = beamai_checkpoint:go_back(Mgr3, RunId, 2),
    ?assertEqual(Cp1#graph_checkpoint.id, BackCp#graph_checkpoint.id),
    ?assertEqual(0, BackCp#graph_checkpoint.superstep).

%% @doc 测试时间旅行 - 前进
test_time_travel_go_forward(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_5">>,

    %% 创建 3 个检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, _, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),
    {ok, Cp3, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{}),

    %% 先回退
    {ok, _, Mgr4} = beamai_checkpoint:go_back(Mgr3, RunId, 2),

    %% 再前进
    {ok, ForwardCp, _Mgr5} = beamai_checkpoint:go_forward(Mgr4, RunId, 2),
    ?assertEqual(Cp3#graph_checkpoint.id, ForwardCp#graph_checkpoint.id),
    ?assertEqual(2, ForwardCp#graph_checkpoint.superstep).

%% @doc 测试时间旅行 - 跳转
test_time_travel_goto(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_6">>,

    %% 创建 3 个检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, Cp2, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),
    {ok, _, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{}),

    %% 跳转到第 2 个检查点
    {ok, GotoCp, _Mgr4} = beamai_checkpoint:goto(Mgr3, RunId, Cp2#graph_checkpoint.id),
    ?assertEqual(Cp2#graph_checkpoint.id, GotoCp#graph_checkpoint.id),
    ?assertEqual(1, GotoCp#graph_checkpoint.superstep).

%% @doc 测试撤销/重做
test_undo_redo(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_7">>,

    %% 创建 2 个检查点
    {ok, Cp1, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, Cp2, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),

    %% 撤销
    {ok, UndoCp, Mgr3} = beamai_checkpoint:undo(Mgr2, RunId),
    ?assertEqual(Cp1#graph_checkpoint.id, UndoCp#graph_checkpoint.id),

    %% 重做
    {ok, RedoCp, _Mgr4} = beamai_checkpoint:redo(Mgr3, RunId),
    ?assertEqual(Cp2#graph_checkpoint.id, RedoCp#graph_checkpoint.id).

%% @doc 测试获取当前位置
test_get_current_position(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_8">>,

    %% 创建 3 个检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, _, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),
    {ok, _, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{}),

    %% 获取位置
    {ok, Pos1} = beamai_checkpoint:get_current_position(Mgr3, RunId),
    ?assertEqual(2, maps:get(current, Pos1)),
    ?assertEqual(3, maps:get(total, Pos1)),

    %% 回退后获取位置
    {ok, _, Mgr4} = beamai_checkpoint:go_back(Mgr3, RunId, 1),
    {ok, Pos2} = beamai_checkpoint:get_current_position(Mgr4, RunId),
    ?assertEqual(1, maps:get(current, Pos2)).

%% @doc 测试分支创建
test_fork_from(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_9">>,

    %% 创建检查点
    {ok, Cp1, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, _, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),

    %% 从 Cp1 创建分支
    {ok, ForkedCp, Mgr3} = beamai_checkpoint:fork_from(Mgr2, Cp1#graph_checkpoint.id, <<"alternate">>, RunId),

    %% 验证分支
    ?assertNotEqual(<<"main">>, ForkedCp#graph_checkpoint.branch_id),
    ?assertEqual(Cp1#graph_checkpoint.id, ForkedCp#graph_checkpoint.parent_id),

    %% 验证分支列表
    Branches = beamai_checkpoint:list_branches(Mgr3),
    ?assertEqual(2, length(Branches)).

%% @doc 测试分支管理
test_branch_management(Config) ->
    Mgr0 = ?config(mgr, Config),

    %% 初始只有 main 分支
    Branches0 = beamai_checkpoint:list_branches(Mgr0),
    ?assertEqual(1, length(Branches0)),

    %% 创建分支
    RunId = <<"run_10">>,
    {ok, Cp1, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, _, Mgr2} = beamai_checkpoint:fork_from(Mgr1, Cp1#graph_checkpoint.id, <<"retry">>, RunId),

    Branches1 = beamai_checkpoint:list_branches(Mgr2),
    ?assertEqual(2, length(Branches1)),

    %% 切换回 main
    {ok, Mgr3} = beamai_checkpoint:switch_branch(Mgr2, <<"main">>),
    ?assertMatch(#{current_branch := <<"main">>}, Mgr3).

%% @doc 测试获取历史
test_get_history(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_11">>,

    %% 创建多个检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, _, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),
    {ok, _, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{}),
    {ok, _, Mgr4} = beamai_checkpoint:save_from_pregel(Mgr3, RunId, #{superstep => 3}, #{}),

    %% 获取历史
    {ok, History} = beamai_checkpoint:get_history(Mgr4, RunId),
    ?assertEqual(4, length(History)),

    %% 验证顺序
    Supersteps = [Cp#graph_checkpoint.superstep || Cp <- History],
    ?assertEqual([0, 1, 2, 3], Supersteps).

%% @doc 测试获取血统
test_get_lineage(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_12">>,

    %% 创建检查点链
    {ok, Cp1, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{superstep => 0}, #{}),
    {ok, Cp2, Mgr2} = beamai_checkpoint:save_from_pregel(Mgr1, RunId, #{superstep => 1}, #{}),
    {ok, Cp3, Mgr3} = beamai_checkpoint:save_from_pregel(Mgr2, RunId, #{superstep => 2}, #{}),

    %% 获取 Cp3 的血统
    {ok, Lineage} = beamai_checkpoint:get_lineage(Mgr3, Cp3#graph_checkpoint.id),

    %% 验证血统
    ?assertEqual(3, length(Lineage)),
    [L1, L2, L3] = Lineage,
    ?assertEqual(Cp1#graph_checkpoint.id, L1#graph_checkpoint.id),
    ?assertEqual(Cp2#graph_checkpoint.id, L2#graph_checkpoint.id),
    ?assertEqual(Cp3#graph_checkpoint.id, L3#graph_checkpoint.id).

%% @doc 测试检查点访问器
test_checkpoint_accessors(Config) ->
    Mgr = ?config(mgr, Config),
    RunId = <<"run_13">>,

    PregelState = #{
        superstep => 5,
        iteration => 3,
        vertices => #{
            v1 => #{value => 100, active => true, messages => [msg1], halt_voted => false},
            v2 => #{value => 200, active => false, messages => [], halt_voted => true}
        },
        pending_activations => [],
        global_state => #{count => 2},
        active_vertices => [v1],
        completed_vertices => [v2],
        failed_vertices => [v3],
        interrupted_vertices => [v4]
    },

    {ok, Cp, _} = beamai_checkpoint:save_from_pregel(Mgr, RunId, PregelState, #{
        checkpoint_type => superstep,
        resumable => true,
        resume_data => #{v1 => input_data}
    }),

    %% 测试顶点状态访问
    {ok, V1State} = beamai_checkpoint:get_vertex_state(Cp, v1),
    ?assertEqual(100, maps:get(value, V1State)),
    ?assertEqual(true, maps:get(active, V1State)),

    %% 测试所有顶点
    Vertices = beamai_checkpoint:get_vertices(Cp),
    ?assertEqual(2, maps:size(Vertices)),

    %% 测试分类顶点
    ?assertEqual([v1], beamai_checkpoint:get_active_vertices(Cp)),
    ?assertEqual([v3], beamai_checkpoint:get_failed_vertices(Cp)),
    ?assertEqual([v4], beamai_checkpoint:get_interrupted_vertices(Cp)),

    %% 测试恢复信息
    ?assertEqual(true, beamai_checkpoint:is_resumable(Cp)),
    ?assertEqual(#{v1 => input_data}, beamai_checkpoint:get_resume_data(Cp)),

    %% 测试超步和全局状态
    ?assertEqual(5, beamai_checkpoint:get_superstep(Cp)),
    ?assertEqual(#{count => 2}, beamai_checkpoint:get_global_state(Cp)).

%% @doc 测试重试顶点
test_retry_vertex(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_14">>,

    %% 创建带失败顶点的检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{
        superstep => 1,
        failed_vertices => [failed_v1, failed_v2],
        pending_activations => []
    }, #{checkpoint_type => error}),

    %% 重试失败顶点
    {ok, RetryCp, _Mgr2} = beamai_checkpoint:retry_vertex(Mgr1, RunId, failed_v1),

    %% 验证：失败顶点移到待激活列表
    ?assertEqual([failed_v2], RetryCp#graph_checkpoint.failed_vertices),
    ?assert(lists:member(failed_v1, RetryCp#graph_checkpoint.pending_activations)),
    ?assertEqual(1, RetryCp#graph_checkpoint.retry_count).

%% @doc 测试注入恢复数据
test_inject_resume_data(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_15">>,

    %% 创建检查点
    {ok, _, Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{
        superstep => 1,
        interrupted_vertices => [v1, v2]
    }, #{
        checkpoint_type => interrupt,
        resume_data => #{v1 => old_data}
    }),

    %% 注入新的恢复数据
    {ok, UpdatedCp, _Mgr2} = beamai_checkpoint:inject_resume_data(Mgr1, RunId, #{
        v1 => new_data,
        v2 => v2_data
    }),

    %% 验证数据已合并
    ResumeData = beamai_checkpoint:get_resume_data(UpdatedCp),
    ?assertEqual(new_data, maps:get(v1, ResumeData)),  %% 覆盖旧数据
    ?assertEqual(v2_data, maps:get(v2, ResumeData)).   %% 新数据

%% @doc 测试 pending_deltas 保存和恢复
test_pending_deltas(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_16">>,

    %% 创建带 pending_deltas 的检查点（模拟出错时的延迟提交）
    PendingDeltas = [
        #{<<"key1">> => <<"value1">>},
        #{<<"key2">> => <<"value2">>}
    ],

    {ok, Cp, _Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{
        superstep => 3,
        global_state => #{total => 100},
        pending_deltas => PendingDeltas,
        pending_activations => [v1, v2],
        failed_vertices => [v3]
    }, #{checkpoint_type => error}),

    %% 验证 pending_deltas 已保存
    ?assertEqual(PendingDeltas, beamai_checkpoint:get_pending_deltas(Cp)),
    ?assertEqual([v1, v2], beamai_checkpoint:get_pending_activations(Cp)),
    ?assertEqual(error, Cp#graph_checkpoint.checkpoint_type).

%% @doc 测试 to_pregel_restore_opts
test_to_pregel_restore_opts(Config) ->
    Mgr0 = ?config(mgr, Config),
    RunId = <<"run_17">>,

    %% 创建完整的检查点
    PendingDeltas = [#{<<"delta">> => 1}],
    Vertices = #{
        v1 => #{value => 10, active => true, messages => [], halt_voted => false},
        v2 => #{value => 20, active => false, messages => [msg1], halt_voted => true}
    },

    {ok, Cp, _Mgr1} = beamai_checkpoint:save_from_pregel(Mgr0, RunId, #{
        superstep => 5,
        iteration => 2,
        global_state => #{counter => 50},
        pending_deltas => PendingDeltas,
        pending_activations => [v1],
        vertices => Vertices
    }, #{checkpoint_type => error}),

    %% 转换为 Pregel 恢复选项
    RestoreOpts = beamai_checkpoint:to_pregel_restore_opts(Cp),

    %% 验证必需字段
    ?assertEqual(5, maps:get(superstep, RestoreOpts)),
    ?assertEqual(#{counter => 50}, maps:get(global_state, RestoreOpts)),
    ?assertEqual(Vertices, maps:get(vertices, RestoreOpts)),

    %% 验证可选字段
    ?assertEqual([v1], maps:get(pending_activations, RestoreOpts)),
    ?assertEqual(PendingDeltas, maps:get(pending_deltas, RestoreOpts)).
