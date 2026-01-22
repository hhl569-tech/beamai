%%%-------------------------------------------------------------------
%%% @doc LLM 调用节点模块
%%%
%%% 创建用于 LLM 交互的图节点。
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
        execute_llm_call(LLMConfig, AllMsgs, LLMOpts, State, MsgsKey)
    end.

%% @private 执行 LLM 调用并处理结果
-spec execute_llm_call(map(), [map()], map(), map(), atom()) ->
    {ok, map()}.
execute_llm_call(LLMConfig, AllMsgs, LLMOpts, State, MsgsKey) ->
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            %% 成功：触发回调并处理响应
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_end, [Response], State),
            process_response(Response, State, MsgsKey);

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
    LLMOpts = build_llm_opts(Tools, LLMConfig, State),

    %% 执行 LLM 调用
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            invoke_callback(on_llm_end, [Response], State),
            %% 处理响应
            {ok, State1} = process_response(Response, State, MsgsKey),
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

%%====================================================================
%% 私有函数 - LLM 响应处理
%%====================================================================

%% @private 处理 LLM 响应并更新状态
%%
%% 从响应中提取内容、工具调用和完成原因。
%% 触发文本和动作回调。
%% 使用增量更新模式更新 messages 和 full_messages。
%%
%% 重要：增量更新模式
%% - 只设置新的 assistant 消息，不包含历史消息
%% - append_reducer 负责将新消息追加到现有列表
%% - 避免消息重复问题
-spec process_response(map(), map(), atom()) -> {ok, map()}.
process_response(Response, State, MsgsKey) ->
    %% 提取响应数据
    Content = extract_content(Response),
    ToolCalls = extract_tool_calls(Response),
    FinishReason = extract_finish_reason(Response),

    %% 触发文本和动作回调
    beamai_agent_utils:invoke_text_callback(Content, State),
    beamai_agent_utils:invoke_action_callback(ToolCalls, Content, FinishReason, State),

    %% 构建 assistant 消息
    AssistantMsg = build_assistant_msg(Content, ToolCalls),

    %% 使用增量模式更新状态：只设置新消息
    %% append_reducer 会将新消息追加到现有消息列表
    BaseUpdates = [
        {MsgsKey, [AssistantMsg]},  %% 只设置新消息，不包含历史
        {last_response, Response},
        {last_content, Content},
        {tool_calls, ToolCalls},
        {finish_reason, FinishReason}
    ],

    %% 同步到 full_messages（如果存在，也使用增量模式）
    AllUpdates = beamai_state_helpers:sync_full_messages(BaseUpdates, AssistantMsg, State),
    NewState = beamai_state_helpers:set_many(State, AllUpdates),

    {ok, NewState}.

%%====================================================================
%% 私有函数 - 辅助函数
%%====================================================================

%% @private 构建 LLM 调用选项
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    beamai_agent_utils:build_llm_opts(Tools, LLMConfig, State).

%% @private 构建 assistant 消息
-spec build_assistant_msg(binary() | null, [map()]) -> map().
build_assistant_msg(Content, ToolCalls) ->
    beamai_agent_utils:build_assistant_message(Content, ToolCalls).

%% @private 从 LLM 响应提取内容
-spec extract_content(map()) -> binary() | null.
extract_content(Response) ->
    maps:get(content, Response, null).

%% @private 从 LLM 响应提取工具调用
-spec extract_tool_calls(map()) -> [map()].
extract_tool_calls(Response) ->
    maps:get(tool_calls, Response, []).

%% @private 从 LLM 响应提取完成原因
-spec extract_finish_reason(map()) -> binary().
extract_finish_reason(Response) ->
    maps:get(finish_reason, Response, <<"stop">>).

%% @private 回调调用辅助函数
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = beamai_state_helpers:get_callbacks(State),
    Meta = beamai_state_helpers:get_callback_meta(State),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
