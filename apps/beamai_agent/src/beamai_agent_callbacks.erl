%%%-------------------------------------------------------------------
%%% @doc 回调管理模块
%%%
%%% 负责 Agent 回调系统的管理：
%%% - 回调处理器初始化
%%% - 回调更新
%%% - 回调调用
%%% - 回调记录与 Map 转换
%%%
%%% 支持 19 种生命周期回调，覆盖 LLM、Tool、Agent、Chain、Retriever 等。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_callbacks).

-include("beamai_agent.hrl").

%% API 导出
-export([
    init/1,
    update/2,
    invoke/3,
    to_map/1,
    build_metadata/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化回调处理器
%%
%% 从配置选项初始化回调。
-spec init(map()) -> #callbacks{}.
init(Opts) ->
    CallbackOpts = maps:get(callbacks, Opts, #{}),

    #callbacks{
        on_llm_start = maps:get(on_llm_start, CallbackOpts, undefined),
        on_llm_end = maps:get(on_llm_end, CallbackOpts, undefined),
        on_llm_error = maps:get(on_llm_error, CallbackOpts, undefined),
        on_llm_new_token = maps:get(on_llm_new_token, CallbackOpts, undefined),
        on_tool_start = maps:get(on_tool_start, CallbackOpts, undefined),
        on_tool_end = maps:get(on_tool_end, CallbackOpts, undefined),
        on_tool_error = maps:get(on_tool_error, CallbackOpts, undefined),
        on_agent_action = maps:get(on_agent_action, CallbackOpts, undefined),
        on_agent_finish = maps:get(on_agent_finish, CallbackOpts, undefined),
        on_chain_start = maps:get(on_chain_start, CallbackOpts, undefined),
        on_chain_end = maps:get(on_chain_end, CallbackOpts, undefined),
        on_chain_error = maps:get(on_chain_error, CallbackOpts, undefined),
        on_retriever_start = maps:get(on_retriever_start, CallbackOpts, undefined),
        on_retriever_end = maps:get(on_retriever_end, CallbackOpts, undefined),
        on_retriever_error = maps:get(on_retriever_error, CallbackOpts, undefined),
        on_text = maps:get(on_text, CallbackOpts, undefined),
        on_retry = maps:get(on_retry, CallbackOpts, undefined),
        on_custom_event = maps:get(on_custom_event, CallbackOpts, undefined)
    }.

%% @doc 更新回调处理器
%%
%% 使用新的选项更新回调，未指定的保持原值。
-spec update(#callbacks{}, map()) -> #callbacks{}.
update(Callbacks, Opts) ->
    #callbacks{
        on_llm_start = maps:get(on_llm_start, Opts, Callbacks#callbacks.on_llm_start),
        on_llm_end = maps:get(on_llm_end, Opts, Callbacks#callbacks.on_llm_end),
        on_llm_error = maps:get(on_llm_error, Opts, Callbacks#callbacks.on_llm_error),
        on_llm_new_token = maps:get(on_llm_new_token, Opts, Callbacks#callbacks.on_llm_new_token),
        on_tool_start = maps:get(on_tool_start, Opts, Callbacks#callbacks.on_tool_start),
        on_tool_end = maps:get(on_tool_end, Opts, Callbacks#callbacks.on_tool_end),
        on_tool_error = maps:get(on_tool_error, Opts, Callbacks#callbacks.on_tool_error),
        on_agent_action = maps:get(on_agent_action, Opts, Callbacks#callbacks.on_agent_action),
        on_agent_finish = maps:get(on_agent_finish, Opts, Callbacks#callbacks.on_agent_finish),
        on_chain_start = maps:get(on_chain_start, Opts, Callbacks#callbacks.on_chain_start),
        on_chain_end = maps:get(on_chain_end, Opts, Callbacks#callbacks.on_chain_end),
        on_chain_error = maps:get(on_chain_error, Opts, Callbacks#callbacks.on_chain_error),
        on_retriever_start = maps:get(on_retriever_start, Opts, Callbacks#callbacks.on_retriever_start),
        on_retriever_end = maps:get(on_retriever_end, Opts, Callbacks#callbacks.on_retriever_end),
        on_retriever_error = maps:get(on_retriever_error, Opts, Callbacks#callbacks.on_retriever_error),
        on_text = maps:get(on_text, Opts, Callbacks#callbacks.on_text),
        on_retry = maps:get(on_retry, Opts, Callbacks#callbacks.on_retry),
        on_custom_event = maps:get(on_custom_event, Opts, Callbacks#callbacks.on_custom_event)
    }.

%% @doc 调用回调函数
%%
%% 从回调记录中获取处理器并安全调用。
%% 回调失败只记录警告，不影响主流程。
-spec invoke(atom(), list(), #callbacks{}) -> ok.
invoke(CallbackName, Args, Callbacks) ->
    Handler = get_handler(CallbackName, Callbacks),
    call_handler(Handler, Args).

%% @doc 将回调记录转换为 map
%%
%% 用于传递给图状态。
-spec to_map(#callbacks{}) -> map().
to_map(#callbacks{} = C) ->
    #{
        on_llm_start => C#callbacks.on_llm_start,
        on_llm_end => C#callbacks.on_llm_end,
        on_llm_error => C#callbacks.on_llm_error,
        on_llm_new_token => C#callbacks.on_llm_new_token,
        on_tool_start => C#callbacks.on_tool_start,
        on_tool_end => C#callbacks.on_tool_end,
        on_tool_error => C#callbacks.on_tool_error,
        on_agent_action => C#callbacks.on_agent_action,
        on_agent_finish => C#callbacks.on_agent_finish,
        on_chain_start => C#callbacks.on_chain_start,
        on_chain_end => C#callbacks.on_chain_end,
        on_chain_error => C#callbacks.on_chain_error,
        on_retriever_start => C#callbacks.on_retriever_start,
        on_retriever_end => C#callbacks.on_retriever_end,
        on_retriever_error => C#callbacks.on_retriever_error,
        on_text => C#callbacks.on_text,
        on_retry => C#callbacks.on_retry,
        on_custom_event => C#callbacks.on_custom_event
    }.

%% @doc 构建回调元数据
%%
%% 从状态记录构建元数据 map。
%% run_id 由图执行层管理，不在 Agent 层传递。
-spec build_metadata(#state{}) -> map().
build_metadata(#state{config = #agent_config{id = Id, name = Name}}) ->
    #{
        agent_id => Id,
        agent_name => Name,
        timestamp => erlang:system_time(millisecond)
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取回调处理器（使用模式匹配，O(1) 查找）
-spec get_handler(atom(), #callbacks{}) -> function() | undefined.
get_handler(on_llm_start, #callbacks{on_llm_start = H}) -> H;
get_handler(on_llm_end, #callbacks{on_llm_end = H}) -> H;
get_handler(on_llm_error, #callbacks{on_llm_error = H}) -> H;
get_handler(on_llm_new_token, #callbacks{on_llm_new_token = H}) -> H;
get_handler(on_tool_start, #callbacks{on_tool_start = H}) -> H;
get_handler(on_tool_end, #callbacks{on_tool_end = H}) -> H;
get_handler(on_tool_error, #callbacks{on_tool_error = H}) -> H;
get_handler(on_agent_action, #callbacks{on_agent_action = H}) -> H;
get_handler(on_agent_finish, #callbacks{on_agent_finish = H}) -> H;
get_handler(on_chain_start, #callbacks{on_chain_start = H}) -> H;
get_handler(on_chain_end, #callbacks{on_chain_end = H}) -> H;
get_handler(on_chain_error, #callbacks{on_chain_error = H}) -> H;
get_handler(on_retriever_start, #callbacks{on_retriever_start = H}) -> H;
get_handler(on_retriever_end, #callbacks{on_retriever_end = H}) -> H;
get_handler(on_retriever_error, #callbacks{on_retriever_error = H}) -> H;
get_handler(on_text, #callbacks{on_text = H}) -> H;
get_handler(on_retry, #callbacks{on_retry = H}) -> H;
get_handler(on_custom_event, #callbacks{on_custom_event = H}) -> H;
get_handler(_, _) -> undefined.

%% @private 调用回调处理器
-spec call_handler(function() | undefined, list()) -> ok.
call_handler(undefined, _Args) ->
    ok;
call_handler(Handler, Args) when is_function(Handler) ->
    try
        erlang:apply(Handler, Args)
    catch
        Class:Reason:Stack ->
            logger:warning("回调执行失败: ~p:~p~n堆栈: ~p", [Class, Reason, Stack])
    end,
    ok.
