%%%-------------------------------------------------------------------
%%% @doc 工具执行节点模块
%%%
%%% 创建用于工具执行的图节点。
%%% 核心逻辑委托给 beamai_tool_core 实现共享。
%%%
%%% == 功能 ==
%%% - 执行工具调用
%%% - 处理工具结果
%%% - 构建工具结果消息
%%% - 触发工具回调
%%% - 可选中间件集成
%%%
%%% == 使用方式 ==
%%% ```erlang
%%% %% 无中间件
%%% Node = beamai_tool_node:create(ToolHandlers).
%%%
%%% %% 带中间件
%%% Node = beamai_tool_node:create(ToolHandlers, #{middlewares => Middlewares}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_node).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 节点创建 API
-export([create/1, create/2]).

%% 工具辅助函数
-export([build_handlers/1]).

%%====================================================================
%% 节点创建 API
%%====================================================================

%% @doc 创建工具执行节点
%%
%% 创建一个执行工具调用的图节点函数。
%% 从状态中获取 tool_calls 并使用 handlers 执行。
%%
%% @param ToolHandlers 工具处理器映射 #{ToolName => Handler}
%% @returns 节点函数 fun(State) -> {ok, NewState}
-spec create(#{binary() => function()}) -> fun((map()) -> {ok, map()}).
create(ToolHandlers) ->
    create(ToolHandlers, #{}).

%% @doc 创建工具执行节点（带选项）
%%
%% 选项说明：
%%   - middlewares: 中间件列表（默认: []）
%%
%% @param ToolHandlers 工具处理器映射
%% @param Opts 节点选项
%% @returns 节点函数
-spec create(#{binary() => function()}, map()) -> fun((map()) -> {ok, map()}).
create(ToolHandlers, Opts) ->
    Middlewares = maps:get(middlewares, Opts, []),

    case Middlewares of
        [] ->
            %% 无中间件模式
            create_basic_node(ToolHandlers);
        _ ->
            %% 有中间件模式
            MwChain = beamai_middleware_runner:init(Middlewares),
            create_middleware_node(ToolHandlers, MwChain)
    end.

%%====================================================================
%% 工具辅助函数
%%====================================================================

%% @doc 从工具定义构建处理器映射
%%
%% 将工具定义列表转换为 #{Name => Handler} 映射。
%% 忽略没有 name 或 handler 字段的工具。
%%
%% @param Tools 工具定义列表
%% @returns 处理器映射
-spec build_handlers([map()]) -> #{binary() => function()}.
build_handlers(Tools) ->
    lists:foldl(fun(#{name := Name, handler := Handler}, Acc) ->
        Acc#{Name => Handler};
    (_, Acc) ->
        Acc
    end, #{}, Tools).

%%====================================================================
%% 私有函数 - 基础节点（无中间件）
%%====================================================================

%% @private 创建基础工具节点
-spec create_basic_node(map()) -> fun((map()) -> {ok, map()}).
create_basic_node(ToolHandlers) ->
    fun(State) ->
        %% 从状态获取数据
        ToolCalls = beamai_state_helpers:get_tool_calls(State),
        Context = beamai_state_helpers:get_context(State),

        %% 使用共享核心执行所有工具调用
        {Results, CtxUpdates} = beamai_tool_core:execute_calls(
            ToolCalls, ToolHandlers, Context, State),

        %% 构建工具结果消息
        ToolMessages = beamai_agent_utils:build_tool_messages(ToolCalls, Results),

        %% 批量更新状态（使用增量模式：只设置新消息）
        %% append_reducer 会将新消息追加到现有列表
        NewCtx = maps:merge(Context, CtxUpdates),

        BaseUpdates = [
            {messages, ToolMessages},  %% 只设置新消息，不包含历史
            {tool_results, Results},
            {tool_calls, []},
            {context, NewCtx}
        ],

        %% 同步 full_messages（如果存在，也使用增量模式）
        AllUpdates = beamai_state_helpers:sync_full_messages_list(
            BaseUpdates, ToolMessages, State),
        NewState = beamai_state_helpers:set_many(State, AllUpdates),

        {ok, NewState}
    end.

%%====================================================================
%% 私有函数 - 中间件节点
%%====================================================================

%% @private 创建带中间件的工具节点
-spec create_middleware_node(map(), list()) -> fun((map()) -> {ok, map()}).
create_middleware_node(ToolHandlers, MwChain) ->
    fun(State) ->
        Chain = graph:get(State, mw_chain, MwChain),
        handle_before_tools(
            beamai_middleware_runner:run_hook(before_tools, State, Chain),
            State, ToolHandlers, Chain)
    end.

%% @private 处理 before_tools 钩子结果
-spec handle_before_tools(term(), map(), map(), list()) ->
    {ok, map()} | {interrupt, term(), map()}.
handle_before_tools({ok, State1}, _State, ToolHandlers, MwChain) ->
    execute_tools_with_hooks(State1, ToolHandlers, MwChain);

handle_before_tools({goto, '__end__', State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, graph:set(State1, tool_calls, [])};

handle_before_tools({goto, model, State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, graph:set(State1, mw_goto_model, true)};

handle_before_tools({halt, Reason}, State, _ToolHandlers, _MwChain) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_before_tools({interrupt, Action, State1}, _State, _ToolHandlers, _MwChain) ->
    %% 返回中断给 pregel 层处理
    State2 = beamai_state_helpers:set_interrupt(State1, Action, before_tools),
    {interrupt, Action, State2}.

%% @private 执行带钩子的工具调用
-spec execute_tools_with_hooks(map(), map(), list()) ->
    {ok, map()} | {interrupt, term(), map()}.
execute_tools_with_hooks(State, ToolHandlers, MwChain) ->
    %% 从状态获取数据
    ToolCalls = beamai_state_helpers:get_tool_calls(State),
    Context = beamai_state_helpers:get_context(State),

    %% 存储原始调用供中间件访问
    State0 = graph:set(State, mw_original_tool_calls, ToolCalls),

    %% 使用共享核心执行
    {Results, CtxUpdates} = beamai_tool_core:execute_calls(
        ToolCalls, ToolHandlers, Context, State0),

    %% 构建工具消息并更新状态（使用增量模式）
    ToolMessages = beamai_agent_utils:build_tool_messages(ToolCalls, Results),
    NewCtx = maps:merge(Context, CtxUpdates),

    BaseUpdates = [
        {messages, ToolMessages},  %% 只设置新消息，不包含历史
        {tool_results, Results},
        {tool_calls, []},
        {context, NewCtx}
    ],
    AllUpdates = beamai_state_helpers:sync_full_messages_list(
        BaseUpdates, ToolMessages, State0),
    State1 = beamai_state_helpers:set_many(State0, AllUpdates),

    %% 运行 after_tools 钩子
    handle_after_tools(
        beamai_middleware_runner:run_hook(after_tools, State1, MwChain),
        State1).

%% @private 处理 after_tools 钩子结果
-spec handle_after_tools(term(), map()) -> {ok, map()} | {interrupt, term(), map()}.
handle_after_tools({ok, State1}, _State) ->
    {ok, State1};

handle_after_tools({goto, tools, State1}, _State) ->
    {ok, graph:set(State1, mw_retry_tools, true)};

handle_after_tools({goto, Target, State1}, _State) ->
    {ok, graph:set(State1, mw_goto, Target)};

handle_after_tools({halt, Reason}, State) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_after_tools({interrupt, Action, State1}, _State) ->
    State2 = beamai_state_helpers:set_interrupt(State1, Action, after_tools),
    {interrupt, Action, State2}.
