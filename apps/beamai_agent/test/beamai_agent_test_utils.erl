%%%-------------------------------------------------------------------
%%% @doc Agent 测试辅助模块
%%%
%%% 提供测试用的 LLM 配置和辅助函数。
%%% 使用智谱 GLM-4.7 模型（通过 Anthropic 兼容 API）进行测试。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_test_utils).

%% LLM 配置 API
-export([
    get_llm_config/0,
    has_api_key/0,
    skip_without_api_key/1
]).

%% 测试数据 API
-export([
    simple_tool/0,
    calculator_tool/0,
    echo_tool/0,
    base_agent_config/0,
    base_agent_config/1
]).

%% 断言辅助 API
-export([
    assert_has_response/1,
    assert_tool_called/2,
    assert_no_error/1
]).

%%====================================================================
%% 配置常量
%%====================================================================

-define(ZHIPU_ANTHROPIC_BASE_URL, <<"https://open.bigmodel.cn/api/anthropic">>).
-define(DEFAULT_MODEL, <<"glm-4.7">>).
-define(DEFAULT_MAX_TOKENS, 2048).
-define(DEFAULT_TIMEOUT, 120000).

%%====================================================================
%% LLM 配置 API
%%====================================================================

%% @doc 获取测试用 LLM 配置
%%
%% 优先使用 ZHIPU_API_KEY 环境变量。
%% 如果没有设置，尝试 ANTHROPIC_API_KEY 或 OPENAI_API_KEY。
-spec get_llm_config() -> {ok, map()} | {error, no_api_key}.
get_llm_config() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            case os:getenv("ANTHROPIC_API_KEY") of
                false ->
                    case os:getenv("OPENAI_API_KEY") of
                        false ->
                            {error, no_api_key};
                        Key ->
                            {ok, llm_client:create(openai, #{
                                api_key => list_to_binary(Key),
                                timeout => ?DEFAULT_TIMEOUT
                            })}
                    end;
                Key ->
                    {ok, llm_client:create(anthropic, #{
                        api_key => list_to_binary(Key),
                        timeout => ?DEFAULT_TIMEOUT
                    })}
            end;
        Key ->
            {ok, llm_client:create(anthropic, #{
                api_key => list_to_binary(Key),
                base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
                model => ?DEFAULT_MODEL,
                max_tokens => ?DEFAULT_MAX_TOKENS,
                timeout => ?DEFAULT_TIMEOUT
            })}
    end.

%% @doc 检查 API Key 是否可用
-spec has_api_key() -> boolean().
has_api_key() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            case os:getenv("ANTHROPIC_API_KEY") of
                false ->
                    case os:getenv("OPENAI_API_KEY") of
                        false -> false;
                        "" -> false;
                        _ -> true
                    end;
                "" -> false;
                _ -> true
            end;
        "" -> false;
        _ -> true
    end.

%% @doc 跳过没有 API Key 的测试
%%
%% 如果有 API Key，执行测试函数；否则输出跳过信息。
-spec skip_without_api_key(fun(() -> term())) -> term().
skip_without_api_key(TestFun) ->
    case has_api_key() of
        true -> TestFun();
        false ->
            io:format("Skipping test: No API Key available~n"),
            skipped
    end.

%%====================================================================
%% 测试数据 API
%%====================================================================

%% @doc 获取简单工具定义（用于测试工具调用）
-spec simple_tool() -> map().
simple_tool() ->
    #{
        name => <<"get_weather">>,
        description => <<"获取指定城市的天气信息">>,
        parameters => #{
            type => object,
            properties => #{
                city => #{
                    type => string,
                    description => <<"城市名称">>
                }
            },
            required => [<<"city">>]
        },
        handler => fun(#{<<"city">> := City}) ->
            {ok, <<"天气晴朗，温度25度，城市：", City/binary>>}
        end
    }.

%% @doc 获取计算器工具定义
-spec calculator_tool() -> map().
calculator_tool() ->
    #{
        name => <<"calculator">>,
        description => <<"执行数学计算">>,
        parameters => #{
            type => object,
            properties => #{
                expression => #{
                    type => string,
                    description => <<"要计算的数学表达式，如 2+3*4">>
                }
            },
            required => [<<"expression">>]
        },
        handler => fun(#{<<"expression">> := Expr}) ->
            try
                %% 简单的表达式求值（仅用于测试）
                {ok, Tokens, _} = erl_scan:string(binary_to_list(Expr) ++ "."),
                {ok, Parsed} = erl_parse:parse_exprs(Tokens),
                {value, Result, _} = erl_eval:exprs(Parsed, []),
                {ok, list_to_binary(io_lib:format("~p", [Result]))}
            catch
                _:_ -> {error, <<"计算失败">>}
            end
        end
    }.

%% @doc 获取回显工具定义（用于简单测试）
-spec echo_tool() -> map().
echo_tool() ->
    #{
        name => <<"echo">>,
        description => <<"回显输入的消息">>,
        parameters => #{
            type => object,
            properties => #{
                message => #{
                    type => string,
                    description => <<"要回显的消息">>
                }
            },
            required => [<<"message">>]
        },
        handler => fun(#{<<"message">> := Msg}) ->
            {ok, <<"Echo: ", Msg/binary>>}
        end
    }.

%% @doc 获取基础 Agent 配置
-spec base_agent_config() -> map().
base_agent_config() ->
    base_agent_config(#{}).

%% @doc 获取基础 Agent 配置（带选项合并）
-spec base_agent_config(map()) -> map().
base_agent_config(Opts) ->
    case get_llm_config() of
        {ok, LLMConfig} ->
            BaseConfig = #{
                llm => LLMConfig,
                system_prompt => <<"你是一个有用的助手，用于测试 Agent 功能。请简洁回答。">>,
                max_iterations => 5,
                tools => []
            },
            maps:merge(BaseConfig, Opts);
        {error, _} ->
            error({missing_api_key, "请设置 ZHIPU_API_KEY 环境变量"})
    end.

%%====================================================================
%% 断言辅助 API
%%====================================================================

%% @doc 断言结果包含响应
-spec assert_has_response(map()) -> ok.
assert_has_response(Result) ->
    case maps:get(final_response, Result, undefined) of
        undefined ->
            error({assertion_failed, no_final_response, Result});
        <<>> ->
            error({assertion_failed, empty_response, Result});
        _ ->
            ok
    end.

%% @doc 断言特定工具被调用
-spec assert_tool_called(binary(), map()) -> ok.
assert_tool_called(ToolName, Result) ->
    Messages = maps:get(messages, Result, []),
    ToolCalled = lists:any(fun(Msg) ->
        case Msg of
            #{role := assistant, tool_calls := ToolCalls} ->
                lists:any(fun(TC) ->
                    case TC of
                        #{function := #{name := Name}} -> Name =:= ToolName;
                        #{name := Name} -> Name =:= ToolName;
                        _ -> false
                    end
                end, ToolCalls);
            _ -> false
        end
    end, Messages),
    case ToolCalled of
        true -> ok;
        false -> error({assertion_failed, tool_not_called, ToolName, Messages})
    end.

%% @doc 断言没有错误
-spec assert_no_error({ok, term()} | {error, term()}) -> ok.
assert_no_error({ok, _}) -> ok;
assert_no_error({error, Reason}) ->
    error({assertion_failed, unexpected_error, Reason}).
