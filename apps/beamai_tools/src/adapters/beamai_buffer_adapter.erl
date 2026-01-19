%%%-------------------------------------------------------------------
%%% @doc 对话缓冲适配器
%%%
%%% 封装对话缓冲调用，支持运行时注入自定义实现。
%%% 默认使用 beamai_conversation_buffer (beamai_memory)，但可以通过配置替换。
%%%
%%% 这使得 beamai_tools 不需要硬依赖 beamai_memory。
%%%
%%% == 配置方式 ==
%%%
%%% 1. Application 环境变量（全局配置）：
%%% ```
%%% application:set_env(beamai_tools, buffer_module, my_buffer).
%%% ```
%%%
%%% 2. 调用时传入（局部配置）：
%%% ```
%%% beamai_buffer_adapter:new(Opts, #{buffer_module => my_buffer}).
%%% ```
%%%
%%% == 自定义实现 ==
%%%
%%% 自定义模块需实现 beamai_buffer_behaviour：
%%% ```
%%% -module(my_buffer).
%%% -behaviour(beamai_buffer_behaviour).
%%% -export([new/1, count_tokens/2, build_context/2]).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_buffer_adapter).

%% API
-export([new/1, new/2]).
-export([count_tokens/2, count_tokens/3]).
-export([build_context/2, build_context/3]).
-export([get_module/0, get_module/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新的缓冲配置
%%
%% @param BufferOpts 缓冲配置选项
%% @returns 初始化后的配置
-spec new(BufferOpts :: map()) -> Config :: map().
new(BufferOpts) ->
    new(BufferOpts, #{}).

%% @doc 创建新的缓冲配置（带适配器选项）
%%
%% @param BufferOpts 缓冲配置选项
%% @param AdapterOpts 适配器选项，可包含 buffer_module
%% @returns 初始化后的配置
-spec new(BufferOpts :: map(), AdapterOpts :: map()) -> Config :: map().
new(BufferOpts, AdapterOpts) ->
    Module = get_module(AdapterOpts),
    try
        Module:new(BufferOpts)
    catch
        error:undef ->
            %% 模块不存在，返回默认配置
            default_config(BufferOpts);
        _:_ ->
            default_config(BufferOpts)
    end.

%% @doc 估算消息列表的 token 数
%%
%% @param Config 缓冲配置
%% @param Messages 消息列表
%% @returns 估算的 token 数
-spec count_tokens(Config :: map(), Messages :: list()) -> integer().
count_tokens(Config, Messages) ->
    count_tokens(Config, Messages, #{}).

%% @doc 估算消息列表的 token 数（带选项）
%%
%% @param Config 缓冲配置
%% @param Messages 消息列表
%% @param AdapterOpts 适配器选项
%% @returns 估算的 token 数
-spec count_tokens(Config :: map(), Messages :: list(), AdapterOpts :: map()) -> integer().
count_tokens(Config, Messages, AdapterOpts) ->
    Module = get_module(AdapterOpts),
    try
        Module:count_tokens(Config, Messages)
    catch
        error:undef ->
            %% 模块不存在，使用简单估算
            simple_token_count(Messages);
        _:_ ->
            simple_token_count(Messages)
    end.

%% @doc 构建适合 LLM 的上下文
%%
%% @param Config 缓冲配置
%% @param Messages 完整的消息列表
%% @returns {ok, Context} | {error, Reason}
-spec build_context(Config :: map(), Messages :: list()) ->
    {ok, Context :: map()} | {error, term()}.
build_context(Config, Messages) ->
    build_context(Config, Messages, #{}).

%% @doc 构建适合 LLM 的上下文（带选项）
%%
%% @param Config 缓冲配置
%% @param Messages 完整的消息列表
%% @param AdapterOpts 适配器选项
%% @returns {ok, Context} | {error, Reason}
-spec build_context(Config :: map(), Messages :: list(), AdapterOpts :: map()) ->
    {ok, Context :: map()} | {error, term()}.
build_context(Config, Messages, AdapterOpts) ->
    Module = get_module(AdapterOpts),
    try
        Module:build_context(Config, Messages)
    catch
        error:undef ->
            {error, {buffer_module_not_found, Module}};
        Class:Reason:Stacktrace ->
            logger:error("Buffer adapter call failed: ~p:~p~n~p",
                        [Class, Reason, Stacktrace]),
            {error, {buffer_call_failed, Reason}}
    end.

%% @doc 获取当前配置的缓冲模块
%%
%% @returns 模块名
-spec get_module() -> module().
get_module() ->
    get_module(#{}).

%% @doc 获取缓冲模块（支持选项覆盖）
%%
%% 优先级：
%% 1. Opts 中的 buffer_module
%% 2. Application 环境变量 beamai_tools.buffer_module
%% 3. 默认值 beamai_conversation_buffer
%%
%% @param Opts 选项
%% @returns 模块名
-spec get_module(Opts :: map()) -> module().
get_module(Opts) ->
    case maps:get(buffer_module, Opts, undefined) of
        undefined ->
            application:get_env(beamai_tools, buffer_module, beamai_conversation_buffer);
        Module ->
            Module
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认配置
default_config(Opts) ->
    #{
        window_size => maps:get(window_size, Opts, 20),
        max_tokens => maps:get(max_tokens, Opts, 4000),
        summarize => maps:get(summarize, Opts, false)
    }.

%% @private 简单的 token 估算
simple_token_count(Messages) when is_list(Messages) ->
    lists:foldl(fun(Msg, Acc) ->
        Content = maps:get(content, Msg, maps:get(<<"content">>, Msg, <<>>)),
        Acc + estimate_tokens(Content)
    end, 0, Messages);
simple_token_count(_) ->
    0.

%% @private 估算单个内容的 token 数
estimate_tokens(Content) when is_binary(Content) ->
    %% 简单估算：每 4 个字符约 1 个 token
    byte_size(Content) div 4;
estimate_tokens(Content) when is_list(Content) ->
    %% 列表内容（如图片等），每个元素估算
    lists:foldl(fun
        (#{<<"text">> := Text}, Acc) -> Acc + estimate_tokens(Text);
        (#{text := Text}, Acc) -> Acc + estimate_tokens(Text);
        (_, Acc) -> Acc + 100  %% 非文本内容给个固定值
    end, 0, Content);
estimate_tokens(_) ->
    0.
