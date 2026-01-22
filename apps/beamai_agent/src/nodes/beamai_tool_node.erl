%%%-------------------------------------------------------------------
%%% @doc Tool Execution Node Module
%%%
%%% Creates graph nodes for tool execution.
%%% Delegates core logic to beamai_tool_core for shared implementation.
%%%
%%% Responsibilities:
%%%   - Execute tool calls
%%%   - Process tool results
%%%   - Build tool result messages
%%%   - Trigger tool callbacks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_node).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Node creation API
-export([create/1]).

%% Tool helper functions
-export([build_handlers/1]).

%%====================================================================
%% Node Creation API
%%====================================================================

%% @doc Create tool execution node
%%
%% Creates a graph node function that executes tool calls.
%% Gets tool_calls from state and executes each with handlers.
%%
%% @param ToolHandlers Tool handler map #{ToolName => Handler}
%% @returns Node function fun(State) -> {ok, NewState}
-spec create(#{binary() => function()}) -> fun((map()) -> {ok, map()}).
create(ToolHandlers) ->
    fun(State) ->
        %% Get data from state
        ToolCalls = beamai_state_helpers:get_tool_calls(State),
        Context = beamai_state_helpers:get_context(State),

        %% Execute all tool calls using shared core
        {Results, CtxUpdates} = beamai_tool_core:execute_calls(
            ToolCalls, ToolHandlers, Context, State),

        %% Build tool result messages
        ToolMessages = beamai_agent_utils:build_tool_messages(ToolCalls, Results),

        %% Batch update state (使用增量模式：只设置新消息)
        %% append_reducer 会将新消息追加到现有列表
        NewCtx = maps:merge(Context, CtxUpdates),

        BaseUpdates = [
            {messages, ToolMessages},  %% 只设置新消息，不包含历史
            {tool_results, Results},
            {tool_calls, []},
            {context, NewCtx}
        ],

        %% Sync full_messages if present (also uses delta pattern)
        AllUpdates = beamai_state_helpers:sync_full_messages_list(
            BaseUpdates, ToolMessages, State),
        NewState = beamai_state_helpers:set_many(State, AllUpdates),

        {ok, NewState}
    end.

%%====================================================================
%% Tool Helper Functions
%%====================================================================

%% @doc Build handler map from tool definitions
%%
%% Converts tool definition list to #{Name => Handler} map.
%% Ignores tools without name or handler fields.
%%
%% @param Tools Tool definition list
%% @returns Handler map
-spec build_handlers([map()]) -> #{binary() => function()}.
build_handlers(Tools) ->
    lists:foldl(fun(#{name := Name, handler := Handler}, Acc) ->
        Acc#{Name => Handler};
    (_, Acc) ->
        Acc
    end, #{}, Tools).
