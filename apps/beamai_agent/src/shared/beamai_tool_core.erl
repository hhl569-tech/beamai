%%%-------------------------------------------------------------------
%%% @doc 工具执行核心模块
%%%
%%% 提供统一的工具执行逻辑，被以下模块使用：
%%% - beamai_tool_node（标准工具执行）
%%% - 带中间件的工具执行
%%%
%%% 通过提取共享逻辑消除约 100 行重复代码。
%%%
%%% == 主要功能 ==
%%% - execute_calls: 执行工具调用列表
%%% - execute_single: 执行单个工具调用
%%% - safe_execute: 安全执行（带异常处理）
%%% - process_result: 处理工具返回值
%%%
%%% == 工具处理器签名 ==
%%% 支持两种处理器签名：
%%% - Handler(Args) -> Result
%%% - Handler(Args, Context) -> Result
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_core).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 执行上下文记录（替代 5+ 参数函数）
-record(tool_ctx, {
    handlers  :: #{binary() => function()},  %% 工具处理器映射
    context   :: map(),                       %% 执行上下文
    state     :: map()                        %% 图状态
}).

%% 主要 API
-export([
    execute_calls/4,     %% 执行工具调用列表
    execute_single/3,    %% 执行单个工具调用
    safe_execute/3,      %% 安全执行（带异常处理）
    call_handler/3,      %% 调用处理器
    process_result/1     %% 处理执行结果
]).

%% 工具信息提取
-export([
    extract_tool_info/1  %% 提取工具名称和参数
]).

%% 回调辅助
-export([
    invoke_tool_callbacks/4  %% 触发工具回调
]).

%%====================================================================
%% 主要 API 函数
%%====================================================================

%% @doc 执行工具调用列表
%%
%% 遍历工具调用，使用提供的处理器执行每个调用。
%% 累积结果和上下文更新。
%%
%% @param ToolCalls 工具调用 map 列表
%% @param Handlers 工具名到处理器函数的映射
%% @param Context 当前执行上下文
%% @param State 图状态（用于回调）
%% @returns {Results, ContextUpdates}
-spec execute_calls([map()], #{binary() => function()}, map(), map()) ->
    {[{ok, binary()} | {error, term()}], map()}.
execute_calls(ToolCalls, Handlers, Context, State) ->
    Ctx = #tool_ctx{handlers = Handlers, context = Context, state = State},
    lists:foldl(fun(ToolCall, {Results, CtxAcc}) ->
        UpdatedCtx = Ctx#tool_ctx{context = CtxAcc},
        {Result, NewCtx} = execute_single(ToolCall, UpdatedCtx, true),
        {Results ++ [Result], maps:merge(CtxAcc, NewCtx)}
    end, {[], Context}, ToolCalls).

%% @doc 执行单个工具调用
%%
%% @param ToolCall 工具调用 map
%% @param Ctx 工具执行上下文记录
%% @param InvokeCallbacks 是否触发回调
%% @returns {Result, ContextUpdates}
-spec execute_single(map(), #tool_ctx{}, boolean()) ->
    {{ok, binary()} | {error, term()}, map()}.
execute_single(ToolCall, #tool_ctx{handlers = Handlers, context = Context,
                                    state = State}, InvokeCallbacks) ->
    {Name, Args} = extract_tool_info(ToolCall),

    %% 如果启用，触发开始回调
    case InvokeCallbacks of
        true -> invoke_callback(on_tool_start, [Name, Args], State);
        false -> ok
    end,

    %% 执行工具
    case maps:get(Name, Handlers, undefined) of
        undefined ->
            Error = {unknown_tool, Name},
            case InvokeCallbacks of
                true -> invoke_callback(on_tool_error, [Name, Error], State);
                false -> ok
            end,
            {{error, Error}, #{}};
        Handler ->
            {Result, CtxUpdates} = safe_execute(Handler, Args, Context),
            case InvokeCallbacks of
                true -> invoke_tool_callbacks(Name, Result, State, tool_result);
                false -> ok
            end,
            {Result, CtxUpdates}
    end.

%% @doc 安全执行工具处理器
%%
%% 使用 try/catch 包装工具执行以处理异常。
%% 将返回值规范化为标准格式。
%%
%% @param Handler 工具处理器函数
%% @param Args 工具参数
%% @param Context 执行上下文
%% @returns {Result, ContextUpdates}
-spec safe_execute(function(), map(), map()) ->
    {{ok, binary()} | {error, term()}, map()}.
safe_execute(Handler, Args, Context) ->
    try
        RawResult = call_handler(Handler, Args, Context),
        process_result(RawResult)
    catch
        Class:Reason:_Stack ->
            {{error, {Class, Reason}}, #{}}
    end.

%% @doc 根据 arity 调用处理器
%%
%% 支持两种处理器签名：
%%   - Handler(Args) -> Result
%%   - Handler(Args, Context) -> Result
%%
%% @param Handler 处理器函数
%% @param Args 工具参数
%% @param Context 执行上下文
%% @returns 处理器返回值
-spec call_handler(function(), map(), map()) -> term().
call_handler(Handler, Args, Context) ->
    case erlang:fun_info(Handler, arity) of
        {arity, 1} -> Handler(Args);
        {arity, 2} -> Handler(Args, Context);
        _ -> Handler(Args)
    end.

%% @doc 处理工具执行结果
%%
%% 将各种返回格式规范化为标准的 {Result, ContextUpdates}。
%%
%% 支持的格式：
%%   - {Result, CtxUpdates} when is_map(CtxUpdates) -> {{ok, Binary}, CtxUpdates}
%%   - {ok, Result, CtxUpdates} -> {{ok, Binary}, CtxUpdates}
%%   - {ok, Result} -> {{ok, Binary}, #{}}
%%   - {error, Reason} -> {{error, Reason}, #{}}
%%   - Result -> {{ok, Binary}, #{}}
%%
%% @param Result 原始工具结果
%% @returns {标准化结果, 上下文更新}
-spec process_result(term()) -> {{ok, binary()} | {error, term()}, map()}.
process_result({Result, CtxUpdates}) when is_map(CtxUpdates) ->
    {{ok, beamai_utils:to_binary(Result)}, CtxUpdates};
process_result({ok, Result, CtxUpdates}) when is_map(CtxUpdates) ->
    {{ok, beamai_utils:to_binary(Result)}, CtxUpdates};
process_result({ok, Result}) ->
    {{ok, beamai_utils:to_binary(Result)}, #{}};
process_result({error, Reason}) ->
    {{error, Reason}, #{}};
process_result(Result) ->
    {{ok, beamai_utils:to_binary(Result)}, #{}}.

%%====================================================================
%% 工具信息提取
%%====================================================================

%% @doc 从工具调用中提取工具名称和参数
%%
%% 处理 OpenAI 格式和直接格式的工具调用。
%%
%% @param ToolCall 工具调用 map
%% @returns {Name, Args}
-spec extract_tool_info(map()) -> {binary(), map()}.
extract_tool_info(ToolCall) ->
    beamai_agent_utils:extract_tool_info(ToolCall).

%%====================================================================
%% 回调辅助
%%====================================================================

%% @doc 触发工具结果回调
%%
%% @param Name 工具名称
%% @param Result 工具执行结果
%% @param State 图状态
%% @param Type 回调类型（标准回调使用 tool_result）
-spec invoke_tool_callbacks(binary(), {ok, binary()} | {error, term()}, map(), atom()) -> ok.
invoke_tool_callbacks(Name, {ok, Result}, State, _Type) ->
    invoke_callback(on_tool_end, [Name, Result], State);
invoke_tool_callbacks(Name, {error, Reason}, State, _Type) ->
    invoke_callback(on_tool_error, [Name, Reason], State).

%% @private 回调调用辅助
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
