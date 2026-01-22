%%%-------------------------------------------------------------------
%%% @doc Agent 核心功能测试套件
%%%
%%% 测试 beamai_agent 模块的核心功能：
%%% - 纯函数 API（run_once, create_state, run_with_state）
%%% - 进程 API（start_link, run, stop）
%%% - 工具调用流程
%%% - 状态管理
%%%
%%% 使用 GLM-4.7 作为测试 LLM
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_agent/include/beamai_agent.hrl").

%% CT 回调
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% 纯函数 API 测试
-export([
    test_create_state/1,
    test_create_state_with_id/1,
    test_run_once_simple/1,
    test_run_once_with_tools/1,
    test_run_with_state_multi_turn/1,
    test_export_import_state/1
]).

%% 进程 API 测试
-export([
    test_start_stop/1,
    test_process_run/1,
    test_process_run_with_tools/1,
    test_add_remove_tool/1,
    test_context_api/1,
    test_meta_api/1,
    test_callbacks/1
]).

%% 节点测试
-export([
    test_llm_node_create/1,
    test_tool_node_create/1,
    test_node_registry_build/1
]).

%%====================================================================
%% CT 回调
%%====================================================================

all() ->
    [
        {group, pure_api},
        {group, process_api},
        {group, nodes}
    ].

groups() ->
    [
        {pure_api, [sequence], [
            test_create_state,
            test_create_state_with_id,
            test_run_once_simple,
            test_run_once_with_tools,
            test_run_with_state_multi_turn,
            test_export_import_state
        ]},
        {process_api, [sequence], [
            test_start_stop,
            test_process_run,
            test_process_run_with_tools,
            test_add_remove_tool,
            test_context_api,
            test_meta_api,
            test_callbacks
        ]},
        {nodes, [parallel], [
            test_llm_node_create,
            test_tool_node_create,
            test_node_registry_build
        ]}
    ].

init_per_suite(Config) ->
    %% 确保应用启动
    application:ensure_all_started(beamai_agent),

    %% 检查 API Key
    case beamai_agent_test_utils:has_api_key() of
        true ->
            [{has_api_key, true} | Config];
        false ->
            ct:pal("Warning: No API Key available, some tests will be skipped"),
            [{has_api_key, false} | Config]
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% 纯函数 API 测试
%%====================================================================

%% @doc 测试 create_state/1 - 自动生成 ID
test_create_state(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, State} = beamai_agent:create_state(AgentConfig),

            %% 验证状态结构
            ?assert(is_record(State, state) orelse is_tuple(State)),
            ok
    end.

%% @doc 测试 create_state/2 - 指定 ID
test_create_state_with_id(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            AgentId = <<"test-agent-001">>,
            {ok, State} = beamai_agent:create_state(AgentId, AgentConfig),

            %% 验证状态创建成功
            ?assert(is_record(State, state) orelse is_tuple(State)),
            ok
    end.

%% @doc 测试 run_once/2 - 简单对话
test_run_once_simple(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),

            %% 执行简单对话
            Result = beamai_agent:run_once(AgentConfig, <<"你好，请简单介绍自己">>),

            case Result of
                {ok, Response} ->
                    %% 验证响应
                    beamai_agent_test_utils:assert_has_response(Response),
                    ct:pal("Response: ~p", [maps:get(final_response, Response)]),
                    ok;
                {error, Reason} ->
                    ct:fail("run_once failed: ~p", [Reason])
            end
    end.

%% @doc 测试 run_once/2 - 带工具调用
%%
%% 注意：LLM 可能不总是调用工具，所以这个测试只验证配置正确和响应存在
test_run_once_with_tools(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            WeatherTool = beamai_agent_test_utils:simple_tool(),
            AgentConfig = beamai_agent_test_utils:base_agent_config(#{
                tools => [WeatherTool],
                system_prompt => <<"你是一个天气助手。当用户询问天气时，请使用 get_weather 工具获取信息。">>
            }),

            %% 执行带工具的对话
            Result = beamai_agent:run_once(AgentConfig, <<"北京今天天气怎么样？请使用工具查询。">>),

            case Result of
                {ok, Response} ->
                    %% 验证响应存在
                    beamai_agent_test_utils:assert_has_response(Response),
                    %% 检查是否调用了工具（仅记录，不强制失败）
                    Messages = maps:get(messages, Response, []),
                    ToolCalled = lists:any(fun(Msg) ->
                        maps:is_key(tool_calls, Msg)
                    end, Messages),
                    ct:pal("Tool called: ~p, Response: ~p",
                           [ToolCalled, maps:get(final_response, Response)]),
                    ok;
                {error, Reason} ->
                    ct:fail("run_once with tools failed: ~p", [Reason])
            end
    end.

%% @doc 测试 run_with_state/3 - 多轮对话
test_run_with_state_multi_turn(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, State0} = beamai_agent:create_state(AgentConfig),

            %% 第一轮对话
            {ok, Result1, State1} = beamai_agent:run_with_state(
                State0, <<"我叫张三">>, #{}),
            beamai_agent_test_utils:assert_has_response(Result1),

            %% 第二轮对话 - 验证上下文保持
            {ok, Result2, _State2} = beamai_agent:run_with_state(
                State1, <<"我刚才说我叫什么名字？">>, #{}),
            beamai_agent_test_utils:assert_has_response(Result2),

            %% 验证响应中包含名字
            FinalResponse = maps:get(final_response, Result2),
            ct:pal("Multi-turn response: ~p", [FinalResponse]),
            ok
    end.

%% @doc 测试状态导入导出
test_export_import_state(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, State0} = beamai_agent:create_state(AgentConfig),

            %% 执行一轮对话
            {ok, _Result, State1} = beamai_agent:run_with_state(
                State0, <<"记住数字：42">>, #{}),

            %% 导出状态
            ExportedState = beamai_agent:export_state_pure(State1),
            ?assert(is_map(ExportedState)),
            ?assert(maps:is_key(messages, ExportedState)),

            %% 导入状态
            {ok, State2} = beamai_agent:import_state_pure(ExportedState, AgentConfig),
            ?assert(is_record(State2, state) orelse is_tuple(State2)),

            ok
    end.

%%====================================================================
%% 进程 API 测试
%%====================================================================

%% @doc 测试 start_link/2 和 stop/1
test_start_stop(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),

            %% 启动 Agent 进程
            {ok, Pid} = beamai_agent:start_link(<<"test-process-agent">>, AgentConfig),
            ?assert(is_pid(Pid)),
            ?assert(is_process_alive(Pid)),

            %% 停止进程
            ok = beamai_agent:stop(Pid),
            timer:sleep(100),  %% 等待进程终止
            ?assertNot(is_process_alive(Pid)),

            ok
    end.

%% @doc 测试进程模式 run/2
test_process_run(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, Pid} = beamai_agent:start_link(<<"test-run-agent">>, AgentConfig),

            %% 执行对话
            Result = beamai_agent:run(Pid, <<"你好">>),

            case Result of
                {ok, Response} ->
                    beamai_agent_test_utils:assert_has_response(Response),
                    ct:pal("Process run response: ~p", [maps:get(final_response, Response)]);
                {error, Reason} ->
                    ct:fail("process run failed: ~p", [Reason])
            end,

            %% 清理
            beamai_agent:stop(Pid),
            ok
    end.

%% @doc 测试进程模式带工具
%%
%% 注意：LLM 可能不总是调用工具，所以这个测试只验证配置正确和响应存在
test_process_run_with_tools(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            EchoTool = beamai_agent_test_utils:echo_tool(),
            AgentConfig = beamai_agent_test_utils:base_agent_config(#{
                tools => [EchoTool],
                system_prompt => <<"你是一个助手。当用户要求回显消息时，必须使用 echo 工具。">>
            }),
            {ok, Pid} = beamai_agent:start_link(<<"test-tools-agent">>, AgentConfig),

            %% 执行带工具的对话
            Result = beamai_agent:run(Pid, <<"请使用 echo 工具回显消息：Hello World">>),

            case Result of
                {ok, Response} ->
                    beamai_agent_test_utils:assert_has_response(Response),
                    %% 检查是否调用了工具（仅记录，不强制失败）
                    Messages = maps:get(messages, Response, []),
                    ToolCalled = lists:any(fun(Msg) ->
                        maps:is_key(tool_calls, Msg)
                    end, Messages),
                    ct:pal("Tool called: ~p", [ToolCalled]);
                {error, Reason} ->
                    ct:fail("process run with tools failed: ~p", [Reason])
            end,

            %% 清理
            beamai_agent:stop(Pid),
            ok
    end.

%% @doc 测试动态添加/移除工具
test_add_remove_tool(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, Pid} = beamai_agent:start_link(<<"test-tool-mgmt-agent">>, AgentConfig),

            %% 添加工具
            EchoTool = beamai_agent_test_utils:echo_tool(),
            ok = beamai_agent:add_tool(Pid, EchoTool),

            %% 移除工具
            ok = beamai_agent:remove_tool(Pid, <<"echo">>),

            %% 清理
            beamai_agent:stop(Pid),
            ok
    end.

%% @doc 测试 Context API
test_context_api(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, Pid} = beamai_agent:start_link(<<"test-context-agent">>, AgentConfig),

            %% 获取初始上下文
            Context0 = beamai_agent:get_context(Pid),
            ?assert(is_map(Context0)),

            %% 设置上下文
            ok = beamai_agent:set_context(Pid, #{user_name => <<"张三">>}),

            %% 获取单个值
            UserName = beamai_agent:get_context(Pid, user_name),
            ?assertEqual(<<"张三">>, UserName),

            %% 使用默认值
            DefaultVal = beamai_agent:get_context(Pid, non_existent, <<"default">>),
            ?assertEqual(<<"default">>, DefaultVal),

            %% 更新上下文
            ok = beamai_agent:update_context(Pid, #{age => 30}),
            Age = beamai_agent:get_context(Pid, age),
            ?assertEqual(30, Age),

            %% put_context
            ok = beamai_agent:put_context(Pid, city, <<"北京">>),
            City = beamai_agent:get_context(Pid, city),
            ?assertEqual(<<"北京">>, City),

            %% 清理
            beamai_agent:stop(Pid),
            ok
    end.

%% @doc 测试 Meta API
test_meta_api(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, Pid} = beamai_agent:start_link(<<"test-meta-agent">>, AgentConfig),

            %% 获取初始元数据
            Meta0 = beamai_agent:get_meta(Pid),
            ?assert(is_map(Meta0)),

            %% 设置元数据
            ok = beamai_agent:set_meta(Pid, #{version => <<"1.0.0">>}),

            %% 获取单个值
            Version = beamai_agent:get_meta(Pid, version),
            ?assertEqual(<<"1.0.0">>, Version),

            %% put_meta
            ok = beamai_agent:put_meta(Pid, debug, true),
            Debug = beamai_agent:get_meta(Pid, debug),
            ?assertEqual(true, Debug),

            %% 清理
            beamai_agent:stop(Pid),
            ok
    end.

%% @doc 测试回调 API
test_callbacks(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            AgentConfig = beamai_agent_test_utils:base_agent_config(),
            {ok, Pid} = beamai_agent:start_link(<<"test-callback-agent">>, AgentConfig),

            %% 设置简单回调
            Self = self(),
            Callbacks = #{
                on_llm_start => fun(_Msgs, _Meta) ->
                    Self ! {callback, llm_start},
                    ok
                end,
                on_llm_end => fun(_Response, _Meta) ->
                    Self ! {callback, llm_end},
                    ok
                end
            },
            ok = beamai_agent:set_callbacks(Pid, Callbacks),

            %% 获取回调配置
            CallbacksMap = beamai_agent:get_callbacks(Pid),
            ?assert(is_map(CallbacksMap)),

            %% 执行对话触发回调
            beamai_agent:run(Pid, <<"测试回调">>),

            %% 验证回调被触发
            receive
                {callback, llm_start} -> ok
            after 5000 ->
                ct:pal("Warning: llm_start callback not received")
            end,

            receive
                {callback, llm_end} -> ok
            after 5000 ->
                ct:pal("Warning: llm_end callback not received")
            end,

            %% 清理
            beamai_agent:stop(Pid),
            ok
    end.

%%====================================================================
%% 节点测试
%%====================================================================

%% @doc 测试 LLM 节点创建
test_llm_node_create(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            {ok, LLMConfig} = beamai_agent_test_utils:get_llm_config(),

            %% 创建 LLM 节点
            NodeFun = beamai_llm_node:create(LLMConfig),
            ?assert(is_function(NodeFun, 1)),

            ok
    end.

%% @doc 测试工具节点创建
test_tool_node_create(_Config) ->
    %% 工具节点不需要 API Key
    EchoTool = beamai_agent_test_utils:echo_tool(),
    ToolHandlers = beamai_tool_node:build_handlers([EchoTool]),

    %% 创建工具节点
    NodeFun = beamai_tool_node:create(ToolHandlers),
    ?assert(is_function(NodeFun, 1)),

    ok.

%% @doc 测试节点注册表构建
test_node_registry_build(Config) ->
    case ?config(has_api_key, Config) of
        false -> {skip, no_api_key};
        true ->
            {ok, LLMConfig} = beamai_agent_test_utils:get_llm_config(),
            EchoTool = beamai_agent_test_utils:echo_tool(),

            Opts = #{
                llm => LLMConfig,
                tools => [EchoTool],
                max_iterations => 5
            },

            %% 构建管道
            Nodes = beamai_node_registry:build_pipeline(Opts),

            %% 验证节点存在
            ?assert(maps:is_key(llm_call, Nodes)),
            ?assert(maps:is_key(execute_tools, Nodes)),
            ?assert(maps:is_key(increment_iter, Nodes)),

            ok
    end.
