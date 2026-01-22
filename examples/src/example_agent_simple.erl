%%%-------------------------------------------------------------------
%%% @doc 简单 Agent 示例
%%%
%%% 演示如何创建和使用带有自定义工具的基础 Agent。
%%% 使用纯函数 API。
%%%
%%% 使用方法:
%%% ```
%%% %% 方式 1: 使用环境变量配置 API Key（推荐使用智谱 GLM-4.7）
%%% %% 设置环境变量: export ZHIPU_API_KEY=your-api-key
%%% example_agent_simple:run().
%%%
%%% %% 方式 2: 直接传入 LLM 配置
%%% LLMConfig = llm_client:create(anthropic, #{
%%%     model => <<"glm-4.7">>,
%%%     api_key => <<"your-api-key">>,
%%%     base_url => <<"https://open.bigmodel.cn/api/anthropic">>
%%% }),
%%% example_agent_simple:run(LLMConfig).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_simple).

-export([run/0, run/1]).
-export([create_calculator_agent/0, create_calculator_agent/1]).
-export([create_weather_agent/0, create_weather_agent/1]).

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 运行所有示例（使用环境变量配置）
-spec run() -> ok.
run() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} ->
            run(LLMConfig);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason]),
            io:format("请设置环境变量 ZHIPU_API_KEY（推荐），~n"),
            io:format("或者使用 run(LLMConfig) 直接传入配置。~n"),
            {error, Reason}
    end.

%% @doc 运行所有示例（使用指定的 LLM 配置）
-spec run(map()) -> ok.
run(LLMConfig) ->
    io:format("=== 简单 Agent 示例（纯函数模式）===~n~n"),

    io:format("1. 计算器 Agent~n"),
    calculator_example(LLMConfig),

    io:format("~n2. 天气 Agent~n"),
    weather_example(LLMConfig),

    io:format("~n3. 多轮对话示例~n"),
    multi_turn_example(LLMConfig),

    io:format("~n示例运行完成!~n"),
    ok.

%%====================================================================
%% 示例函数
%%====================================================================

%% @doc 计算器 Agent 示例
calculator_example(LLMConfig) ->
    {ok, State} = create_calculator_agent(LLMConfig),

    %% 运行计算
    case beamai_agent:run(State, <<"请计算 25 * 4 + 100 的结果"/utf8>>) of
        {ok, Result, _NewState} ->
            Response = maps:get(final_response, Result, <<"无响应"/utf8>>),
            io:format("结果: ~ts~n", [Response]);
        {error, Reason, _NewState} ->
            io:format("错误: ~p~n", [Reason])
    end.

%% @doc 天气 Agent 示例
weather_example(LLMConfig) ->
    {ok, State} = create_weather_agent(LLMConfig),

    %% 查询天气
    case beamai_agent:run(State, <<"东京现在的天气怎么样?"/utf8>>) of
        {ok, Result, _NewState} ->
            Response = maps:get(final_response, Result, <<"无响应"/utf8>>),
            io:format("结果: ~ts~n", [Response]);
        {error, Reason, _NewState} ->
            io:format("错误: ~p~n", [Reason])
    end.

%% @doc 多轮对话示例
multi_turn_example(LLMConfig) ->
    {ok, State0} = beamai_agent:new(#{
        llm => LLMConfig,
        system_prompt => <<"你是一个记忆助手。记住用户告诉你的信息。"/utf8>>
    }),

    io:format("第一轮: 告诉 Agent 一个数字~n"),
    {ok, _, State1} = beamai_agent:run(State0, <<"请记住数字 42"/utf8>>),
    io:format("消息数量: ~p~n", [length(beamai_agent:get_messages(State1))]),

    io:format("第二轮: 询问之前的数字~n"),
    case beamai_agent:run(State1, <<"我刚才说的数字是多少?"/utf8>>) of
        {ok, Result, State2} ->
            Response = maps:get(final_response, Result, <<"无响应"/utf8>>),
            io:format("结果: ~ts~n", [Response]),
            io:format("最终消息数量: ~p~n", [length(beamai_agent:get_messages(State2))]);
        {error, Reason, _} ->
            io:format("错误: ~p~n", [Reason])
    end.

%%====================================================================
%% Agent 创建函数
%%====================================================================

%% @doc 创建计算器 Agent（使用环境变量配置）
-spec create_calculator_agent() -> {ok, beamai_agent:state()} | {error, term()}.
create_calculator_agent() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} -> create_calculator_agent(LLMConfig);
        Error -> Error
    end.

%% @doc 创建计算器 Agent（使用指定的 LLM 配置）
-spec create_calculator_agent(map()) -> {ok, beamai_agent:state()} | {error, term()}.
create_calculator_agent(LLMConfig) ->
    Tools = [
        #{
            name => <<"add">>,
            description => <<"将两个数字相加"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"a">> => #{type => number, description => <<"第一个数"/utf8>>},
                    <<"b">> => #{type => number, description => <<"第二个数"/utf8>>}
                },
                required => [<<"a">>, <<"b">>]
            },
            handler => fun(Args) ->
                A = maps:get(<<"a">>, Args),
                B = maps:get(<<"b">>, Args),
                #{result => A + B}
            end
        },
        #{
            name => <<"subtract">>,
            description => <<"从第一个数减去第二个数"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"a">> => #{type => number, description => <<"被减数"/utf8>>},
                    <<"b">> => #{type => number, description => <<"减数"/utf8>>}
                },
                required => [<<"a">>, <<"b">>]
            },
            handler => fun(Args) ->
                A = maps:get(<<"a">>, Args),
                B = maps:get(<<"b">>, Args),
                #{result => A - B}
            end
        },
        #{
            name => <<"multiply">>,
            description => <<"将两个数字相乘"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"a">> => #{type => number, description => <<"第一个数"/utf8>>},
                    <<"b">> => #{type => number, description => <<"第二个数"/utf8>>}
                },
                required => [<<"a">>, <<"b">>]
            },
            handler => fun(Args) ->
                A = maps:get(<<"a">>, Args),
                B = maps:get(<<"b">>, Args),
                #{result => A * B}
            end
        },
        #{
            name => <<"divide">>,
            description => <<"将第一个数除以第二个数"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"a">> => #{type => number, description => <<"被除数"/utf8>>},
                    <<"b">> => #{type => number, description => <<"除数"/utf8>>}
                },
                required => [<<"a">>, <<"b">>]
            },
            handler => fun(Args) ->
                A = maps:get(<<"a">>, Args),
                B = maps:get(<<"b">>, Args),
                case B of
                    0 -> #{error => <<"除数不能为零"/utf8>>};
                    _ -> #{result => A / B}
                end
            end
        }
    ],

    Opts = #{
        system_prompt => <<"你是一个有用的计算器助手。使用提供的数学工具来执行计算。"/utf8>>,
        tools => Tools,
        llm => LLMConfig
    },

    beamai_agent:new(Opts).

%% @doc 创建天气 Agent（使用环境变量配置）
-spec create_weather_agent() -> {ok, beamai_agent:state()} | {error, term()}.
create_weather_agent() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} -> create_weather_agent(LLMConfig);
        Error -> Error
    end.

%% @doc 创建天气 Agent（使用指定的 LLM 配置，模拟数据）
-spec create_weather_agent(map()) -> {ok, beamai_agent:state()} | {error, term()}.
create_weather_agent(LLMConfig) ->
    %% 模拟天气数据
    WeatherData = #{
        <<"Tokyo">> => #{temp => 22, condition => <<"多云"/utf8>>, humidity => 65},
        <<"东京"/utf8>> => #{temp => 22, condition => <<"多云"/utf8>>, humidity => 65},
        <<"London">> => #{temp => 15, condition => <<"下雨"/utf8>>, humidity => 80},
        <<"伦敦"/utf8>> => #{temp => 15, condition => <<"下雨"/utf8>>, humidity => 80},
        <<"New York">> => #{temp => 18, condition => <<"晴天"/utf8>>, humidity => 55},
        <<"纽约"/utf8>> => #{temp => 18, condition => <<"晴天"/utf8>>, humidity => 55},
        <<"Sydney">> => #{temp => 28, condition => <<"晴朗"/utf8>>, humidity => 45},
        <<"悉尼"/utf8>> => #{temp => 28, condition => <<"晴朗"/utf8>>, humidity => 45},
        <<"北京"/utf8>> => #{temp => 12, condition => <<"晴"/utf8>>, humidity => 40},
        <<"上海"/utf8>> => #{temp => 16, condition => <<"阴"/utf8>>, humidity => 70}
    },

    Tools = [
        #{
            name => <<"get_weather">>,
            description => <<"获取指定城市的当前天气"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"city">> => #{
                        type => string,
                        description => <<"城市名称"/utf8>>
                    }
                },
                required => [<<"city">>]
            },
            handler => fun(Args) ->
                City = maps:get(<<"city">>, Args),
                case maps:get(City, WeatherData, undefined) of
                    undefined ->
                        #{error => <<"未找到该城市"/utf8>>,
                          available => maps:keys(WeatherData)};
                    Weather ->
                        Weather#{city => City}
                end
            end
        },
        #{
            name => <<"get_forecast">>,
            description => <<"获取指定城市的三天天气预报"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"city">> => #{type => string, description => <<"城市名称"/utf8>>}
                },
                required => [<<"city">>]
            },
            handler => fun(Args) ->
                City = maps:get(<<"city">>, Args),
                case maps:get(City, WeatherData, undefined) of
                    undefined ->
                        #{error => <<"未找到该城市"/utf8>>};
                    #{temp := BaseTemp} ->
                        #{
                            city => City,
                            forecast => [
                                #{day => <<"今天"/utf8>>, temp => BaseTemp, condition => <<"晴"/utf8>>},
                                #{day => <<"明天"/utf8>>, temp => BaseTemp + 2, condition => <<"多云"/utf8>>},
                                #{day => <<"后天"/utf8>>, temp => BaseTemp - 1, condition => <<"阴"/utf8>>}
                            ]
                        }
                end
            end
        }
    ],

    Opts = #{
        system_prompt => <<"你是一个有用的天气助手。使用天气工具来提供当前天气和预报信息。"/utf8>>,
        tools => Tools,
        llm => LLMConfig
    },

    beamai_agent:new(Opts).
