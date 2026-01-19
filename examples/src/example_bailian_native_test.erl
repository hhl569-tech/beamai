%%%-------------------------------------------------------------------
%%% @doc 百炼 DashScope 原生 API 测试
%%%
%%% 验证新实现的 DashScope 原生 API provider。
%%%
%%% 使用方法:
%%% ```
%%% export BAILIAN_API_KEY=your-api-key
%%% rebar3 shell
%%% example_bailian_native_test:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_bailian_native_test).

-export([run/0]).
-export([test_simple_chat/0]).
-export([test_chat_with_tools/0]).
-export([test_stream_chat/0]).

%%====================================================================
%% 运行所有测试
%%====================================================================

run() ->
    io:format("~n=== 百炼 DashScope 原生 API 测试 ===~n~n"),

    %% 测试 1: 简单对话
    io:format("【测试 1】简单对话~n"),
    test_simple_chat(),

    %% 测试 2: 工具调用
    io:format("~n【测试 2】工具调用~n"),
    test_chat_with_tools(),

    %% 测试 3: 流式输出
    io:format("~n【测试 3】流式输出~n"),
    test_stream_chat(),

    io:format("~n=== 所有测试完成 ===~n"),
    ok.

%%====================================================================
%% 测试 1: 简单对话
%%====================================================================

test_simple_chat() ->
    case get_config() of
        {ok, Config} ->
            io:format("  使用模型: ~s~n", [maps:get(model, Config)]),
            io:format("  发送请求: 你好，请用一句话介绍自己~n"),

            case llm_client:simple_chat(Config, <<"你好，请用一句话介绍自己。"/utf8>>) of
                {ok, Response} ->
                    io:format("  ✓ 成功~n"),
                    io:format("  响应: ~ts~n", [truncate(Response, 200)]);
                {error, Reason} ->
                    io:format("  ✗ 失败: ~p~n", [Reason])
            end;
        {error, no_api_key} ->
            io:format("  ! 跳过（未设置 BAILIAN_API_KEY）~n")
    end.

%%====================================================================
%% 测试 2: 工具调用
%%====================================================================

test_chat_with_tools() ->
    case get_config() of
        {ok, Config} ->
            io:format("  使用模型: ~s~n", [maps:get(model, Config)]),

            %% 定义天气查询工具
            Tools = [#{
                type => function,
                function => #{
                    name => <<"get_weather">>,
                    description => <<"查询指定城市的天气"/utf8>>,
                    parameters => #{
                        type => object,
                        properties => #{
                            <<"city">> => #{
                                type => string,
                                description => <<"城市名称"/utf8>>
                            }
                        },
                        required => [<<"city">>]
                    }
                }
            }],

            Messages = [
                #{role => user, content => <<"北京今天天气怎么样？"/utf8>>}
            ],

            io:format("  发送请求: 北京今天天气怎么样？~n"),

            case llm_client:with_tools(Config, Messages, Tools) of
                {ok, #{tool_calls := ToolCalls}} when ToolCalls =/= [] ->
                    io:format("  ✓ 成功（模型请求工具调用）~n"),
                    lists:foreach(fun(#{name := Name, arguments := Args}) ->
                        io:format("    工具: ~s, 参数: ~s~n", [Name, Args])
                    end, ToolCalls);
                {ok, #{content := Content}} ->
                    io:format("  ✓ 成功（模型直接回复）~n"),
                    io:format("  响应: ~ts~n", [truncate(Content, 200)]);
                {error, Reason} ->
                    io:format("  ✗ 失败: ~p~n", [Reason])
            end;
        {error, no_api_key} ->
            io:format("  ! 跳过（未设置 BAILIAN_API_KEY）~n")
    end.

%%====================================================================
%% 测试 3: 流式输出
%%====================================================================

test_stream_chat() ->
    case get_config() of
        {ok, Config} ->
            io:format("  使用模型: ~s~n", [maps:get(model, Config)]),
            io:format("  发送请求: 写一首关于春天的短诗~n"),
            io:format("  流式响应: "),

            Messages = [
                #{role => user, content => <<"写一首关于春天的短诗，不超过4句。"/utf8>>}
            ],

            %% 流式回调 - 打印每个 token
            Callback = fun(Event) ->
                case Event of
                    #{<<"output">> := #{<<"choices">> := [#{<<"message">> := #{<<"content">> := Content}} | _]}} ->
                        io:format("~ts", [Content]);
                    #{<<"output">> := #{<<"text">> := Text}} ->
                        io:format("~ts", [Text]);
                    _ ->
                        ok
                end
            end,

            case llm_client:stream_chat(Config, Messages, Callback) of
                {ok, #{content := Content}} ->
                    io:format("~n  ✓ 成功~n"),
                    io:format("  完整响应: ~ts~n", [truncate(Content, 300)]);
                {error, Reason} ->
                    io:format("~n  ✗ 失败: ~p~n", [Reason])
            end;
        {error, no_api_key} ->
            io:format("  ! 跳过（未设置 BAILIAN_API_KEY）~n")
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 获取配置
get_config() ->
    case os:getenv("BAILIAN_API_KEY") of
        false ->
            {error, no_api_key};
        ApiKey ->
            Config = llm_client:create(bailian, #{
                api_key => list_to_binary(ApiKey),
                model => <<"qwen-plus">>,
                max_tokens => 1024
            }),
            {ok, Config}
    end.

%% @private 截断字符串
truncate(Bin, MaxLen) when is_binary(Bin), byte_size(Bin) > MaxLen ->
    <<Prefix:MaxLen/binary, _/binary>> = Bin,
    <<Prefix/binary, "...">>;
truncate(Bin, _MaxLen) ->
    Bin.
