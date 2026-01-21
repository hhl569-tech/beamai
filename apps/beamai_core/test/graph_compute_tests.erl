%%%-------------------------------------------------------------------
%%% @doc graph_compute 模块的错误处理单元测试
%%%
%%% 测试 graph_compute 的异常处理能力：
%%% - compute_fn 的 try-catch 包装
%%% - 成功时返回 status => ok
%%% - 异常时返回 status => {error, Reason}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_compute_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单的测试上下文
make_test_context(VertexId, Value) ->
    Vertex = pregel_vertex:new(VertexId, Value),
    #{
        vertex => Vertex,
        messages => [],
        superstep => 0,
        num_vertices => 1,
        outbox => []
    }.

%%====================================================================
%% compute_fn 错误处理测试
%%====================================================================

%% 测试：成功执行时返回 status => ok
compute_fn_success_returns_ok_status_test() ->
    %% 准备：创建一个会成功的上下文
    %% 使用 __end__ 节点，没有消息时会 vote_to_halt
    Ctx = make_test_context('__end__', #{
        node => undefined,
        edges => [],
        result => undefined
    }),

    %% 执行
    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 ok
    ?assertEqual(ok, maps:get(status, Result)).

%% 测试：异常时返回 status => {error, Reason}
compute_fn_exception_returns_error_status_test() ->
    %% 准备：创建一个会导致异常的上下文
    %% 使用缺少必要字段的 vertex value 触发异常
    Ctx = #{
        vertex => pregel_vertex:new(test_node, #{
            node => undefined  %% 缺少 edges 字段
            %% 不包含 edges 和其他必要字段
        }),
        messages => [{state, #{}}],  %% 有消息以触发处理
        superstep => 0,
        num_vertices => 1,
        outbox => []
    },

    %% 执行
    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 error
    ?assertMatch({error, _}, maps:get(status, Result)),
    %% 验证：返回结构包含必要字段
    ?assert(maps:is_key(vertex, Result)),
    ?assert(maps:is_key(outbox, Result)),
    %% 验证：outbox 为空（失败时不发送消息）
    ?assertEqual([], maps:get(outbox, Result)).

%% 测试：返回结构符合 compute_result 类型
compute_fn_result_structure_test() ->
    %% 准备
    Ctx = make_test_context('__end__', #{
        node => undefined,
        edges => [],
        result => undefined
    }),

    %% 执行
    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：包含所有必要字段
    ?assert(maps:is_key(vertex, Result)),
    ?assert(maps:is_key(outbox, Result)),
    ?assert(maps:is_key(status, Result)).

%%====================================================================
%% make_error_result 测试
%%====================================================================

%% 测试：make_error_result 构造正确的错误结果
%% 注意：make_error_result 是私有函数，通过 compute_fn 的异常处理来间接测试
make_error_result_structure_test() ->
    %% 创建会触发异常的上下文
    BadCtx = #{
        vertex => pregel_vertex:new(bad_node, #{
            node => undefined
        }),
        messages => [{state, #{}}],
        superstep => 0,
        num_vertices => 1,
        outbox => []
    },

    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(BadCtx),

    %% 验证错误结果结构
    ?assertMatch({error, _}, maps:get(status, Result)),
    ?assertEqual([], maps:get(outbox, Result)),
    %% 验证顶点保持原值
    OriginalVtx = maps:get(vertex, BadCtx),
    ResultVtx = maps:get(vertex, Result),
    ?assertEqual(pregel_vertex:id(OriginalVtx), pregel_vertex:id(ResultVtx)).
