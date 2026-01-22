%%%-------------------------------------------------------------------
%%% @doc 图状态辅助模块
%%%
%%% 提供可重用的状态操作函数，消除跨模块的代码重复。
%%% 被 beamai_tool_node、beamai_llm_node 等节点模块使用。
%%%
%%% == 功能分组 ==
%%% - 状态设置器：set_error, set_halt, set_interrupt, set_many
%%% - 状态获取器：get_messages, get_tool_calls, get_context 等
%%% - 消息操作：append_message, sync_full_messages 等
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_state_helpers).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 状态设置器
-export([
    set_error/2,        %% 设置错误状态
    set_halt/3,         %% 设置停止状态
    set_interrupt/3,    %% 设置中断状态（人机交互）
    set_many/2          %% 批量设置状态
]).

%% 状态获取器
-export([
    get_messages/1,     %% 获取消息列表（默认键）
    get_messages/2,     %% 获取消息列表（指定键）
    get_tool_calls/1,   %% 获取工具调用列表
    get_context/1,      %% 获取上下文
    get_callbacks/1,    %% 获取回调函数
    get_callback_meta/1 %% 获取回调元数据
]).

%% 消息操作
-export([
    append_message/2,           %% 追加单条消息
    append_messages/2,          %% 追加多条消息
    sync_full_messages/3,       %% 同步单条消息到 full_messages
    sync_full_messages_list/3   %% 同步多条消息到 full_messages
]).

%%====================================================================
%% 状态设置器
%%====================================================================

%% @doc 设置错误状态
%%
%% 同时设置 error 和 status 字段，用于错误处理。
%%
%% @param State 当前图状态
%% @param Reason 错误原因
%% @returns 带错误信息的新状态
-spec set_error(map(), term()) -> map().
set_error(State, Reason) ->
    set_many(State, [{error, Reason}, {status, error}]).

%% @doc 设置停止状态
%%
%% 当中间件或验证停止执行时使用。
%%
%% @param State 当前图状态
%% @param Reason 停止原因
%% @param FinishReason 响应的完成原因
%% @returns 带停止信息的新状态
-spec set_halt(map(), term(), binary()) -> map().
set_halt(State, Reason, FinishReason) ->
    set_many(State, [
        {halted, true},
        {halt_reason, Reason},
        {finish_reason, FinishReason}
    ]).

%% @doc 设置中断状态（人机交互）
%%
%% 当执行需要用户确认时使用。
%% 配合 before_model、before_tools 等钩子点使用。
%%
%% @param State 当前图状态
%% @param Action 待确认的动作
%% @param Point 中断点（before_model, before_tools 等）
%% @returns 带中断信息的新状态
-spec set_interrupt(map(), map(), atom()) -> map().
set_interrupt(State, Action, Point) ->
    set_many(State, [
        {interrupted, true},
        {pending_action, Action},
        {interrupt_point, Point}
    ]).

%% @doc 批量设置状态值
%%
%% 封装 graph:set_many/2 宏。
%%
%% @param State 当前图状态
%% @param Pairs {Key, Value} 元组列表
%% @returns 更新后的状态
-spec set_many(map(), [{atom(), term()}]) -> map().
set_many(State, Pairs) ->
    ?SET_STATE_MANY(State, Pairs).

%%====================================================================
%% 状态获取器
%%====================================================================

%% @doc 获取消息列表（使用默认 messages 键）
-spec get_messages(map()) -> [map()].
get_messages(State) ->
    get_messages(State, messages).

%% @doc 获取消息列表（使用指定键）
-spec get_messages(map(), atom()) -> [map()].
get_messages(State, Key) ->
    graph:get(State, Key, []).

%% @doc 获取工具调用列表
-spec get_tool_calls(map()) -> [map()].
get_tool_calls(State) ->
    graph:get(State, tool_calls, []).

%% @doc 获取执行上下文
-spec get_context(map()) -> map().
get_context(State) ->
    graph:get(State, context, #{}).

%% @doc 获取回调函数映射
-spec get_callbacks(map()) -> map().
get_callbacks(State) ->
    graph:get(State, callbacks, #{}).

%% @doc 获取回调元数据
-spec get_callback_meta(map()) -> map().
get_callback_meta(State) ->
    graph:get(State, callback_meta, #{}).

%%====================================================================
%% 消息操作
%%====================================================================

%% @doc 追加单条消息到消息列表
-spec append_message(map(), map()) -> [map()].
append_message(State, Message) ->
    Messages = get_messages(State),
    Messages ++ [Message].

%% @doc 追加多条消息到消息列表
-spec append_messages(map(), [map()]) -> [map()].
append_messages(State, NewMessages) ->
    Messages = get_messages(State),
    Messages ++ NewMessages.

%% @doc 同步单条消息到 full_messages（如果存在）
%%
%% 追加消息到 full_messages 列表以保持完整历史记录。
%% 使用增量更新模式：只设置新消息，append_reducer 负责追加。
%%
%% @param BasePairs 基础状态更新对列表
%% @param Message 要追加的消息
%% @param State 当前图状态
%% @returns 更新后的更新对列表
-spec sync_full_messages([{atom(), term()}], map(), map()) -> [{atom(), term()}].
sync_full_messages(BasePairs, Message, State) ->
    case graph:get(State, full_messages, undefined) of
        undefined -> BasePairs;
        _FullMessages ->
            %% 只设置新消息，append_reducer 会追加到现有列表
            BasePairs ++ [{full_messages, [Message]}]
    end.

%% @doc 同步多条消息到 full_messages（如果存在）
%%
%% @param BasePairs 基础状态更新对列表
%% @param Messages 要追加的消息列表
%% @param State 当前图状态
%% @returns 更新后的更新对列表
-spec sync_full_messages_list([{atom(), term()}], [map()], map()) -> [{atom(), term()}].
sync_full_messages_list(BasePairs, Messages, State) ->
    case graph:get(State, full_messages, undefined) of
        undefined -> BasePairs;
        _FullMessages ->
            %% 只设置新消息列表，append_reducer 会追加到现有列表
            BasePairs ++ [{full_messages, Messages}]
    end.
