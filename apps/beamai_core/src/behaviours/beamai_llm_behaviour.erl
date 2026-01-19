%%%-------------------------------------------------------------------
%%% @doc LLM 客户端行为定义
%%%
%%% 定义 LLM 客户端的标准接口，用于解耦 beamai_tools 与具体 LLM 实现。
%%% 任何实现此 behaviour 的模块都可以作为 LLM 客户端使用。
%%%
%%% 默认实现: llm_client (beamai_llm)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_behaviour).

%% 类型导出
-export_type([config/0, request/0, response/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type config() :: map().
%% LLM 配置，通常包含:
%% - provider: atom() - 提供商 (openai, anthropic, etc.)
%% - api_key: binary() - API 密钥
%% - model: binary() - 模型名称
%% - base_url: binary() - API 基础 URL (可选)
%% - max_tokens: integer() - 最大 token 数 (可选)
%% - timeout: integer() - 超时时间 (可选)

-type message() :: #{
    role := user | assistant | system,
    content := binary() | list()
}.

-type request() :: #{
    messages := [message()],
    tools => list(),
    tool_choice => auto | none | map(),
    temperature => float(),
    max_tokens => integer()
}.
%% LLM 请求参数

-type response() :: #{
    content => binary() | list(),
    tool_calls => list(),
    usage => map(),
    model => binary(),
    finish_reason => binary()
}.
%% LLM 响应

%%====================================================================
%% 回调定义
%%====================================================================

%% @doc 发送聊天请求到 LLM
%%
%% 这是主要的 LLM 调用接口。
%%
%% @param Config LLM 配置
%% @param Request 请求参数，包含 messages 等
%% @returns {ok, Response} 成功时返回响应
%% @returns {error, Reason} 失败时返回错误原因
-callback chat(Config :: config(), Request :: request()) ->
    {ok, Response :: response()} | {error, term()}.

%% @doc 验证 LLM 配置是否有效
%%
%% 检查配置是否通过 llm_client:create/2 创建且包含必要字段。
%%
%% @param Config 要验证的配置
%% @returns true 如果配置有效
%% @returns false 如果配置无效
-callback is_valid_config(Config :: config()) -> boolean().

%%====================================================================
%% 可选回调
%%====================================================================

%% @doc 创建 LLM 配置
%%
%% 根据 provider 和选项创建经过验证的配置。
%%
%% @param Provider 提供商名称
%% @param Opts 配置选项
%% @returns 经过验证的配置 map
-callback create(Provider :: atom(), Opts :: map()) -> config().

%% @doc 流式聊天请求
%%
%% 发送流式请求，通过回调函数接收增量响应。
%%
%% @param Config LLM 配置
%% @param Request 请求参数
%% @param Callback 接收流式数据的回调函数
%% @returns {ok, FinalResponse} 流结束时返回完整响应
%% @returns {error, Reason} 失败时返回错误
-callback chat_stream(Config :: config(), Request :: request(),
                      Callback :: fun((Event :: map()) -> ok)) ->
    {ok, Response :: response()} | {error, term()}.

-optional_callbacks([create/2, chat_stream/3]).
