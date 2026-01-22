%%%-------------------------------------------------------------------
%%% @doc LLM 调用节点模块
%%%
%%% 创建用于 LLM 交互的图节点。
%%% 核心逻辑委托给 beamai_llm_core 实现共享。
%%%
%%% == 功能 ==
%%% - 构建 LLM 请求
%%% - 处理 LLM 响应
%%% - 触发回调
%%% - 构建 assistant 消息
%%% - 可选中间件集成
%%%
%%% == 使用方式 ==
%%% ```erlang
%%% %% 无中间件
%%% Node = beamai_llm_node:create(LLMConfig).
%%%
%%% %% 带中间件
%%% Node = beamai_llm_node:create(LLMConfig, #{middlewares => Middlewares}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_node).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 节点创建 API
-export([create/1, create/2]).

%% 内部函数（供 beamai_nodes 委托使用）
-export([build_llm_opts/3]).
-export([process_llm_response/4]).
-export([build_assistant_message/2]).

%%====================================================================
%% 节点创建 API
%%====================================================================

%% @doc 创建 LLM 调用节点（使用默认配置键）
%%
%% 创建一个执行 LLM 调用的图节点函数。
%% 使用默认状态键：tools, messages, system_prompt。
%%
%% @param LLMConfig LLM 配置（provider, model 等）
%% @returns 节点函数 fun(State) -> {ok, NewState} | {error, Reason}
-spec create(map()) -> fun((map()) -> {ok, map()} | {error, term()}).
create(LLMConfig) ->
    create(LLMConfig, #{}).

%% @doc 创建 LLM 调用节点（带选项）
%%
%% 选项说明：
%%   - tools_key: 工具列表的状态键（默认: tools）
%%   - messages_key: 消息列表的状态键（默认: messages）
%%   - system_prompt_key: 系统提示词的状态键（默认: system_prompt）
%%   - middlewares: 中间件列表（默认: []）
%%
%% @param LLMConfig LLM 配置
%% @param Opts 节点选项
%% @returns 节点函数
-spec create(map(), map()) -> fun((map()) -> {ok, map()} | {error, term()}).
create(LLMConfig, Opts) ->
    %% 提取配置键
    ToolsKey = maps:get(tools_key, Opts, tools),
    MsgsKey = maps:get(messages_key, Opts, messages),
    PromptKey = maps:get(system_prompt_key, Opts, system_prompt),
    Middlewares = maps:get(middlewares, Opts, []),

    Keys = #{
        tools_key => ToolsKey,
        msgs_key => MsgsKey,
        prompt_key => PromptKey
    },

    case Middlewares of
        [] ->
            %% 无中间件模式
            create_basic_node(LLMConfig, Keys);
        _ ->
            %% 有中间件模式
            MwChain = beamai_middleware_runner:init(Middlewares),
            create_middleware_node(LLMConfig, Keys, MwChain)
    end.

%%====================================================================
%% 内部函数（供 beamai_nodes 委托使用）
%%====================================================================

%% @doc 构建 LLM 调用选项
%%
%% 委托给 beamai_llm_core。
%%
%% @param Tools 工具定义
%% @param LLMConfig LLM 配置
%% @param State 图状态
%% @returns LLM 选项 map
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    beamai_llm_core:build_llm_opts(Tools, LLMConfig, State).

%% @doc 处理 LLM 响应
%%
%% 委托给 beamai_llm_core 实现共享。
%%
%% @param Response LLM 响应
%% @param Messages 原始消息
%% @param State 图状态
%% @param MsgsKey 消息状态键
%% @returns {ok, NewState}
-spec process_llm_response(map(), [map()], map(), atom()) -> {ok, map()}.
process_llm_response(Response, Messages, State, MsgsKey) ->
    beamai_llm_core:process_response(Response, Messages, State, MsgsKey).

%% @doc 构建 assistant 消息
%%
%% 委托给 beamai_llm_core。
%%
%% @param Content 消息内容
%% @param ToolCalls 工具调用列表
%% @returns Assistant 消息 map
-spec build_assistant_message(binary() | null, [map()]) -> map().
build_assistant_message(Content, ToolCalls) ->
    beamai_llm_core:build_assistant_msg(Content, ToolCalls).

%%====================================================================
%% 私有函数 - 基础节点（无中间件）
%%====================================================================

%% @private 创建基础 LLM 节点
-spec create_basic_node(map(), map()) -> fun((map()) -> {ok, map()}).
create_basic_node(LLMConfig, #{tools_key := ToolsKey, msgs_key := MsgsKey,
                                prompt_key := PromptKey}) ->
    fun(State) ->
        %% 从状态获取数据
        Messages = graph:get(State, MsgsKey, []),
        SystemPrompt = graph:get(State, PromptKey, <<>>),
        Tools = graph:get(State, ToolsKey, []),

        %% 构建完整消息（含系统提示词）
        AllMsgs = beamai_message:prepend_system(SystemPrompt, Messages),

        %% 触发 on_llm_start 回调
        ?INVOKE_CALLBACK_FROM_STATE(on_llm_start, [AllMsgs], State),

        %% 构建 LLM 选项并执行调用
        LLMOpts = build_llm_opts(Tools, LLMConfig, State),
        execute_llm_call(LLMConfig, AllMsgs, LLMOpts, Messages, State, MsgsKey)
    end.

%% @private 执行 LLM 调用并处理结果
-spec execute_llm_call(map(), [map()], map(), [map()], map(), atom()) ->
    {ok, map()}.
execute_llm_call(LLMConfig, AllMsgs, LLMOpts, Messages, State, MsgsKey) ->
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            %% 成功：触发回调并处理响应
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_end, [Response], State),
            process_llm_response(Response, Messages, State, MsgsKey);

        {error, Reason} ->
            %% 失败：触发错误回调并设置错误状态
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_error, [Reason], State),
            ErrorState = beamai_state_helpers:set_error(State, Reason),
            {ok, ErrorState}
    end.

%%====================================================================
%% 私有函数 - 中间件节点
%%====================================================================

%% @private 创建带中间件的 LLM 节点
-spec create_middleware_node(map(), map(), list()) -> fun((map()) -> {ok, map()}).
create_middleware_node(LLMConfig, Keys, MwChain) ->
    fun(State) ->
        State0 = graph:set(State, mw_chain, MwChain),
        handle_before_model(
            beamai_middleware_runner:run_hook(before_model, State0, MwChain),
            State0, LLMConfig, MwChain, Keys)
    end.

%% @private 处理 before_model 钩子结果
-spec handle_before_model(term(), map(), map(), list(), map()) ->
    {ok, map()} | {error, term()} | {interrupt, term(), map()}.
handle_before_model({ok, State1}, _State0, LLMConfig, MwChain, Keys) ->
    execute_llm_with_hooks(State1, LLMConfig, MwChain, Keys);

handle_before_model({goto, '__end__', State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, graph:set(State1, finish_reason, <<"middleware_skip">>)};

handle_before_model({goto, tools, State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, graph:set(State1, mw_goto_tools, true)};

handle_before_model({halt, Reason}, State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, beamai_state_helpers:set_halt(State0, Reason, <<"middleware_halt">>)};

handle_before_model({interrupt, Action, State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    %% 返回中断给 pregel 层处理
    State2 = beamai_state_helpers:set_interrupt(State1, Action, before_model),
    {interrupt, Action, State2}.

%% @private 执行带钩子的 LLM 调用
-spec execute_llm_with_hooks(map(), map(), list(), map()) ->
    {ok, map()} | {error, term()} | {interrupt, term(), map()}.
execute_llm_with_hooks(State, LLMConfig, MwChain, #{tools_key := ToolsKey,
                                                     msgs_key := MsgsKey,
                                                     prompt_key := PromptKey}) ->
    %% 从状态获取数据
    Messages = graph:get(State, MsgsKey, []),
    SystemPrompt = graph:get(State, PromptKey, <<>>),
    Tools = graph:get(State, ToolsKey, []),
    AllMsgs = beamai_message:prepend_system(SystemPrompt, Messages),

    %% 触发回调并构建选项
    invoke_callback(on_llm_start, [AllMsgs], State),
    LLMOpts = beamai_llm_core:build_llm_opts(Tools, LLMConfig, State),

    %% 执行 LLM 调用
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            invoke_callback(on_llm_end, [Response], State),
            %% 使用共享核心处理响应
            {ok, State1} = beamai_llm_core:process_response(Response, Messages, State, MsgsKey),
            %% 运行 after_model 钩子
            handle_after_model(
                beamai_middleware_runner:run_hook(after_model, State1, MwChain),
                State1);
        {error, Reason} ->
            invoke_callback(on_llm_error, [Reason], State),
            {ok, beamai_state_helpers:set_error(State, Reason)}
    end.

%% @private 处理 after_model 钩子结果
-spec handle_after_model(term(), map()) -> {ok, map()} | {interrupt, term(), map()}.
handle_after_model({ok, State1}, _State) ->
    {ok, State1};

handle_after_model({goto, Target, State1}, _State) ->
    {ok, graph:set(State1, mw_goto, Target)};

handle_after_model({halt, Reason}, State) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_after_model({interrupt, Action, State1}, _State) ->
    State2 = beamai_state_helpers:set_interrupt(State1, Action, after_model),
    {interrupt, Action, State2}.

%% @private 回调调用辅助函数
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = beamai_state_helpers:get_callbacks(State),
    Meta = beamai_state_helpers:get_callback_meta(State),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
