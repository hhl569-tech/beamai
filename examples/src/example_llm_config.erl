%%%-------------------------------------------------------------------
%%% @doc LLM 配置构建工具
%%%
%%% 提供两种 Zhipu LLM 配置：
%%%   - anthropic(): 通过 Zhipu 的 Anthropic 兼容 API，使用 GLM-4.7
%%%   - zhipu(): 通过 Zhipu 原生 API，使用 GLM-4.6
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%%
%%% %% Anthropic 兼容方式 (GLM-4.7)
%%% Config = example_llm_config:anthropic().
%%%
%%% %% Zhipu 原生方式 (GLM-4.6)
%%% Config = example_llm_config:zhipu().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_llm_config).

-export([
    anthropic/0,
    anthropic/1,
    claude/0,
    claude/1,
    zhipu/0,
    zhipu/1
]).

%%====================================================================
%% 常量
%%====================================================================

-define(ZHIPU_ANTHROPIC_BASE_URL, <<"https://open.bigmodel.cn/api/anthropic">>).
-define(ANTHROPIC_DEFAULT_MODEL, <<"glm-4.7">>).
-define(CLAUDE_DEFAULT_MODEL, <<"claude-sonnet-4-20250514">>).
-define(ZHIPU_DEFAULT_MODEL, <<"glm-4.6">>).
-define(DEFAULT_MAX_TOKENS, 2048).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建 Anthropic 兼容 LLM 配置（从环境变量获取 API Key）
%%
%% 使用 Zhipu 的 Anthropic 兼容 API，模型为 GLM-4.7。
-spec anthropic() -> beamai_chat_completion:config().
anthropic() ->
    ApiKey = require_env("ZHIPU_API_KEY"),
    anthropic(#{api_key => ApiKey}).

%% @doc 创建 Anthropic 兼容 LLM 配置
%%
%% 通过 Zhipu 的 Anthropic 兼容 URL 调用，provider 为 anthropic。
%%
%% Opts 支持:
%%   - api_key: API Key (必填)
%%   - model: 模型名 (默认 glm-4.7)
%%   - max_tokens: 最大 token 数 (默认 2048)
-spec anthropic(map()) -> beamai_chat_completion:config().
anthropic(Opts) ->
    beamai_chat_completion:create(anthropic, #{
        api_key => maps:get(api_key, Opts),
        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
        model => maps:get(model, Opts, ?ANTHROPIC_DEFAULT_MODEL),
        max_tokens => maps:get(max_tokens, Opts, ?DEFAULT_MAX_TOKENS)
    }).

%% @doc 创建 Claude 原生 LLM 配置（从环境变量获取 API Key）
%%
%% 使用 Anthropic 原生 API，模型为 claude-sonnet-4-20250514。
-spec claude() -> beamai_chat_completion:config().
claude() ->
    ApiKey = require_env("ANTHROPIC_API_KEY"),
    claude(#{api_key => ApiKey}).

%% @doc 创建 Claude 原生 LLM 配置
%%
%% Opts 支持:
%%   - api_key: API Key (必填)
%%   - model: 模型名 (默认 claude-sonnet-4-20250514)
%%   - max_tokens: 最大 token 数 (默认 2048)
-spec claude(map()) -> beamai_chat_completion:config().
claude(Opts) ->
    beamai_chat_completion:create(anthropic, #{
        api_key => maps:get(api_key, Opts),
        model => maps:get(model, Opts, ?CLAUDE_DEFAULT_MODEL),
        max_tokens => maps:get(max_tokens, Opts, ?DEFAULT_MAX_TOKENS)
    }).

%% @doc 创建 Zhipu 原生 LLM 配置（从环境变量获取 API Key）
%%
%% 使用 Zhipu 原生 API，模型为 GLM-4.6。
-spec zhipu() -> beamai_chat_completion:config().
zhipu() ->
    ApiKey = require_env("ZHIPU_API_KEY"),
    zhipu(#{api_key => ApiKey}).

%% @doc 创建 Zhipu 原生 LLM 配置
%%
%% 直接使用 zhipu provider 调用原生 API。
%%
%% Opts 支持:
%%   - api_key: API Key (必填)
%%   - model: 模型名 (默认 glm-4.6)
%%   - max_tokens: 最大 token 数 (默认 2048)
-spec zhipu(map()) -> beamai_chat_completion:config().
zhipu(Opts) ->
    beamai_chat_completion:create(zhipu, #{
        api_key => maps:get(api_key, Opts),
        model => maps:get(model, Opts, ?ZHIPU_DEFAULT_MODEL),
        max_tokens => maps:get(max_tokens, Opts, ?DEFAULT_MAX_TOKENS)
    }).

%%====================================================================
%% 内部函数
%%====================================================================

-spec require_env(string()) -> binary().
require_env(VarName) ->
    case os:getenv(VarName) of
        false ->
            error({missing_env, VarName});
        Value ->
            list_to_binary(Value)
    end.
