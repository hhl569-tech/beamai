%%%-------------------------------------------------------------------
%%% @doc 对话缓冲行为定义
%%%
%%% 定义对话缓冲/上下文管理的标准接口，用于解耦 beamai_tools 与具体实现。
%%% 任何实现此 behaviour 的模块都可以作为对话缓冲使用。
%%%
%%% 默认实现: beamai_conversation_buffer (beamai_memory)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_buffer_behaviour).

%% 类型导出
-export_type([config/0, message/0, context/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type config() :: map().
%% 缓冲配置，通常包含:
%% - max_tokens: integer() - 最大 token 数
%% - window_size: integer() - 滑动窗口大小
%% - strategy: atom() - 缓冲策略 (sliding_window, summarize, etc.)
%% - preserve_system: boolean() - 是否保留系统消息

-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | list(),
    name => binary(),
    tool_call_id => binary(),
    tool_calls => list()
}.
%% 对话消息

-type context() :: #{
    messages := [message()],
    summary => binary(),
    token_count => integer(),
    truncated => boolean()
}.
%% 构建后的上下文

%%====================================================================
%% 回调定义
%%====================================================================

%% @doc 创建新的缓冲配置
%%
%% @param Opts 配置选项
%% @returns 初始化后的配置
-callback new(Opts :: map()) -> config().

%% @doc 估算消息列表的 token 数
%%
%% 用于判断是否需要进行上下文压缩。
%%
%% @param Config 缓冲配置
%% @param Messages 消息列表
%% @returns 估算的 token 数
-callback count_tokens(Config :: config(), Messages :: [message()]) -> integer().

%% @doc 构建适合 LLM 的上下文
%%
%% 根据配置的策略（滑动窗口、摘要等）构建上下文。
%% 可能会截断或压缩消息以适应 token 限制。
%%
%% @param Config 缓冲配置
%% @param Messages 完整的消息列表
%% @returns {ok, Context} 构建后的上下文
%% @returns {error, Reason} 构建失败
-callback build_context(Config :: config(), Messages :: [message()]) ->
    {ok, Context :: context()} | {error, term()}.

%%====================================================================
%% 可选回调
%%====================================================================

%% @doc 添加消息到缓冲
%%
%% @param Config 缓冲配置
%% @param Message 要添加的消息
%% @returns 更新后的配置
-callback add_message(Config :: config(), Message :: message()) -> config().

%% @doc 清空缓冲
%%
%% @param Config 缓冲配置
%% @returns 清空后的配置
-callback clear(Config :: config()) -> config().

%% @doc 获取所有消息
%%
%% @param Config 缓冲配置
%% @returns 消息列表
-callback get_messages(Config :: config()) -> [message()].

%% @doc 生成对话摘要
%%
%% 将消息列表压缩为摘要文本。
%%
%% @param Config 缓冲配置
%% @param Messages 要摘要的消息
%% @param LLMConfig LLM 配置（用于调用摘要模型）
%% @returns {ok, Summary} 摘要文本
%% @returns {error, Reason} 摘要失败
-callback summarize(Config :: config(), Messages :: [message()], LLMConfig :: map()) ->
    {ok, Summary :: binary()} | {error, term()}.

-optional_callbacks([add_message/2, clear/1, get_messages/1, summarize/3]).
