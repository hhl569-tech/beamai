%%%-------------------------------------------------------------------
%%% @doc LLM 调用和响应处理核心模块
%%%
%%% 提供统一的 LLM 执行逻辑，被以下模块使用：
%%% - beamai_llm_node（标准 LLM 调用）
%%% - 带中间件的 LLM 调用
%%%
%%% 通过提取共享逻辑消除约 80 行重复代码。
%%%
%%% == 主要功能 ==
%%% - execute_call: 执行 LLM 调用（含回调）
%%% - process_response: 处理 LLM 响应
%%% - build_llm_opts: 构建 LLM 调用选项
%%%
%%% == 增量更新模式 ==
%%% 使用增量更新（delta pattern）：只设置新消息，
%%% append_reducer 负责追加到现有列表。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_core).

-include_lib("beamai_core/include/beamai_common.hrl").

%% LLM 上下文记录（预留）
%% -record(llm_ctx, {
%%     config    :: map(),
%%     messages  :: [map()],
%%     msgs_key  :: atom(),
%%     state     :: map()
%% }).

%% 主要 API
-export([
    execute_call/4,         %% 执行 LLM 调用
    execute_call/5,         %% 执行 LLM 调用（自定义消息键）
    process_response/4,     %% 处理 LLM 响应
    build_assistant_msg/2,  %% 构建 assistant 消息
    build_llm_opts/3        %% 构建 LLM 调用选项
]).

%% 响应提取
-export([
    extract_content/1,       %% 提取内容
    extract_tool_calls/1,    %% 提取工具调用
    extract_finish_reason/1  %% 提取完成原因
]).

%%====================================================================
%% 主要 API 函数
%%====================================================================

%% @doc 执行 LLM 调用（含回调）
%%
%% 完整的 LLM 调用流程：
%% 1. 构建 LLM 选项
%% 2. 触发 on_llm_start 回调
%% 3. 调用 LLM
%% 4. 触发 on_llm_end/on_llm_error 回调
%% 5. 处理响应
%%
%% @param LLMConfig LLM 提供商配置
%% @param AllMsgs 完整消息列表（含系统提示词）
%% @param Messages 原始消息（用于追加）
%% @param State 图状态
%% @returns {ok, NewState}
-spec execute_call(map(), [map()], [map()], map()) -> {ok, map()}.
execute_call(LLMConfig, AllMsgs, Messages, State) ->
    execute_call(LLMConfig, AllMsgs, Messages, State, messages).

%% @doc 执行 LLM 调用（自定义消息键）
%%
%% @param LLMConfig LLM 提供商配置
%% @param AllMsgs 完整消息列表（含系统提示词）
%% @param Messages 原始消息（用于追加）
%% @param State 图状态
%% @param MsgsKey 状态中存储消息的键
%% @returns {ok, NewState}
-spec execute_call(map(), [map()], [map()], map(), atom()) -> {ok, map()}.
execute_call(LLMConfig, AllMsgs, Messages, State, MsgsKey) ->
    %% 构建 LLM 选项
    Tools = graph:get(State, tools, []),
    LLMOpts = build_llm_opts(Tools, LLMConfig, State),

    %% 触发开始回调
    invoke_callback(on_llm_start, [AllMsgs], State),

    %% 调用 LLM 并处理结果
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            invoke_callback(on_llm_end, [Response], State),
            process_response(Response, Messages, State, MsgsKey);
        {error, Reason} ->
            invoke_callback(on_llm_error, [Reason], State),
            ErrorState = beamai_state_helpers:set_error(State, Reason),
            {ok, ErrorState}
    end.

%% @doc 处理 LLM 响应并更新状态
%%
%% 从响应中提取内容、工具调用和完成原因。
%% 触发文本和动作回调。
%% 使用增量更新模式更新 messages 和 full_messages。
%%
%% 重要：增量更新模式
%% - 只设置新的 assistant 消息，不包含历史消息
%% - append_reducer 负责将新消息追加到现有列表
%% - 避免消息重复问题
%%
%% @param Response LLM 响应 map
%% @param _Messages 原始消息列表（保留用于 API 兼容）
%% @param State 图状态
%% @param MsgsKey 状态中存储消息的键
%% @returns {ok, NewState}
-spec process_response(map(), [map()], map(), atom()) -> {ok, map()}.
process_response(Response, _Messages, State, MsgsKey) ->
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

%% @doc 构建 assistant 消息
%%
%% 委托给 beamai_agent_utils 以保持消息格式一致。
%%
%% @param Content 消息内容（binary 或 null）
%% @param ToolCalls 工具调用列表
%% @returns Assistant 消息 map
-spec build_assistant_msg(binary() | null, [map()]) -> map().
build_assistant_msg(Content, ToolCalls) ->
    beamai_agent_utils:build_assistant_message(Content, ToolCalls).

%% @doc 构建 LLM 调用选项
%%
%% 委托给 beamai_agent_utils 以保持选项格式一致。
%%
%% @param Tools 工具定义列表
%% @param LLMConfig LLM 配置
%% @param State 图状态
%% @returns LLM 选项 map
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    beamai_agent_utils:build_llm_opts(Tools, LLMConfig, State).

%%====================================================================
%% 响应提取
%%====================================================================

%% @doc 从 LLM 响应提取内容
-spec extract_content(map()) -> binary() | null.
extract_content(Response) ->
    maps:get(content, Response, null).

%% @doc 从 LLM 响应提取工具调用
-spec extract_tool_calls(map()) -> [map()].
extract_tool_calls(Response) ->
    maps:get(tool_calls, Response, []).

%% @doc 从 LLM 响应提取完成原因
-spec extract_finish_reason(map()) -> binary().
extract_finish_reason(Response) ->
    maps:get(finish_reason, Response, <<"stop">>).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 回调调用辅助
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
