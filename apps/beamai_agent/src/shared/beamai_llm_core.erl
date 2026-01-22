%%%-------------------------------------------------------------------
%%% @doc Shared LLM call and response processing core
%%%
%%% Provides unified LLM execution logic used by both:
%%% - beamai_llm_node (standard LLM calls)
%%% - beamai_middleware_nodes (middleware-wrapped LLM calls)
%%%
%%% This eliminates ~80 lines of duplicated code between the two modules.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_core).

-include_lib("beamai_core/include/beamai_common.hrl").

%% LLM context record (reserved for future use)
%% -record(llm_ctx, {
%%     config    :: map(),
%%     messages  :: [map()],
%%     msgs_key  :: atom(),
%%     state     :: map()
%% }).

%% API
-export([
    execute_call/4,
    execute_call/5,
    process_response/4,
    build_assistant_msg/2,
    build_llm_opts/3
]).

%% Response extraction
-export([
    extract_content/1,
    extract_tool_calls/1,
    extract_finish_reason/1
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Execute LLM call with callback invocation
%%
%% Complete LLM call flow:
%% 1. Build LLM options
%% 2. Invoke on_llm_start callback
%% 3. Call LLM
%% 4. Invoke on_llm_end/on_llm_error callback
%% 5. Process response
%%
%% @param LLMConfig LLM provider configuration
%% @param AllMsgs Complete message list (with system prompt)
%% @param Messages Original messages (for appending)
%% @param State Graph state
%% @returns {ok, NewState}
-spec execute_call(map(), [map()], [map()], map()) -> {ok, map()}.
execute_call(LLMConfig, AllMsgs, Messages, State) ->
    execute_call(LLMConfig, AllMsgs, Messages, State, messages).

%% @doc Execute LLM call with custom messages key
%%
%% @param LLMConfig LLM provider configuration
%% @param AllMsgs Complete message list (with system prompt)
%% @param Messages Original messages (for appending)
%% @param State Graph state
%% @param MsgsKey Key to store messages in state
%% @returns {ok, NewState}
-spec execute_call(map(), [map()], [map()], map(), atom()) -> {ok, map()}.
execute_call(LLMConfig, AllMsgs, Messages, State, MsgsKey) ->
    %% Build LLM options
    Tools = graph:get(State, tools, []),
    LLMOpts = build_llm_opts(Tools, LLMConfig, State),

    %% Invoke start callback
    invoke_callback(on_llm_start, [AllMsgs], State),

    %% Call LLM and handle result
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            invoke_callback(on_llm_end, [Response], State),
            process_response(Response, Messages, State, MsgsKey);
        {error, Reason} ->
            invoke_callback(on_llm_error, [Reason], State),
            ErrorState = beamai_state_helpers:set_error(State, Reason),
            {ok, ErrorState}
    end.

%% @doc Process LLM response and update state
%%
%% Extracts content, tool calls, and finish reason from response.
%% Invokes text and action callbacks.
%% Updates messages and full_messages with ONLY the new message (delta pattern).
%%
%% 重要：使用增量更新模式
%% - 只设置新的 assistant 消息，不包含历史消息
%% - field_reducer (append_reducer) 负责将新消息追加到现有列表
%% - 这避免了 compute_delta 返回完整列表导致的消息重复问题
%%
%% @param Response LLM response map
%% @param _Messages Original messages list (unused, kept for API compatibility)
%% @param State Graph state
%% @param MsgsKey Key for messages in state
%% @returns {ok, NewState}
-spec process_response(map(), [map()], map(), atom()) -> {ok, map()}.
process_response(Response, _Messages, State, MsgsKey) ->
    %% Extract response data
    Content = extract_content(Response),
    ToolCalls = extract_tool_calls(Response),
    FinishReason = extract_finish_reason(Response),

    %% Invoke callbacks for text and action
    beamai_agent_utils:invoke_text_callback(Content, State),
    beamai_agent_utils:invoke_action_callback(ToolCalls, Content, FinishReason, State),

    %% Build assistant message
    AssistantMsg = build_assistant_msg(Content, ToolCalls),

    %% Update state with ONLY the new message (delta pattern)
    %% append_reducer 会将新消息追加到现有消息列表
    BaseUpdates = [
        {MsgsKey, [AssistantMsg]},  %% 只设置新消息，不包含历史
        {last_response, Response},
        {last_content, Content},
        {tool_calls, ToolCalls},
        {finish_reason, FinishReason}
    ],

    %% Sync full_messages if present (also uses delta pattern)
    AllUpdates = beamai_state_helpers:sync_full_messages(BaseUpdates, AssistantMsg, State),
    NewState = beamai_state_helpers:set_many(State, AllUpdates),

    {ok, NewState}.

%% @doc Build assistant message from content and tool calls
%%
%% Delegates to beamai_agent_utils for consistent message format.
%%
%% @param Content Message content (binary or null)
%% @param ToolCalls List of tool calls
%% @returns Assistant message map
-spec build_assistant_msg(binary() | null, [map()]) -> map().
build_assistant_msg(Content, ToolCalls) ->
    beamai_agent_utils:build_assistant_message(Content, ToolCalls).

%% @doc Build LLM call options
%%
%% Delegates to beamai_agent_utils for consistent options format.
%%
%% @param Tools Tool definitions
%% @param LLMConfig LLM configuration
%% @param State Graph state
%% @returns LLM options map
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    beamai_agent_utils:build_llm_opts(Tools, LLMConfig, State).

%%====================================================================
%% Response Extraction
%%====================================================================

%% @doc Extract content from LLM response
-spec extract_content(map()) -> binary() | null.
extract_content(Response) ->
    maps:get(content, Response, null).

%% @doc Extract tool calls from LLM response
-spec extract_tool_calls(map()) -> [map()].
extract_tool_calls(Response) ->
    maps:get(tool_calls, Response, []).

%% @doc Extract finish reason from LLM response
-spec extract_finish_reason(map()) -> binary().
extract_finish_reason(Response) ->
    maps:get(finish_reason, Response, <<"stop">>).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Invoke callback helper
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
