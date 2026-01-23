%%%-------------------------------------------------------------------
%%% @doc Kernel 简单对话示例
%%%
%%% 演示如何使用 BeamAI Kernel 进行对话，等价于 C# Semantic Kernel:
%%%
%%% ```csharp
%%% var kernel = Kernel.CreateBuilder()
%%%     .AddChatCompletion(...)
%%%     .Build();
%%% var chatService = kernel.GetRequiredService<IChatCompletionService>();
%%% var chatHistory = new ChatHistory();
%%% chatHistory.AddSystemMessage("你是一个有帮助的助手。");
%%% chatHistory.AddUserMessage("你好，请介绍一下自己");
%%% var response = await chatService.GetChatMessageContentAsync(chatHistory);
%%% ```
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_kernel_chat:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_kernel_chat).

-export([run/0, run/1, multi_turn/0, multi_turn/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行单轮对话示例（使用 Zhipu GLM-4.6）
-spec run() -> ok.
run() ->
    LLMConfig = example_llm_config:zhipu(),
    run(LLMConfig).

%% @doc 运行单轮对话示例（使用指定 LLM 配置）
-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("=== BeamAI Kernel Chat Example ===~n~n"),

    %% 1. 构建 Kernel，添加 Chat Completion 服务
    K0 = beamai:kernel(),
    K1 = beamai:add_llm(K0, LLMConfig),

    %% 2. 创建对话历史（消息列表）
    History0 = [
        #{role => system, content => <<"你是一个有帮助的助手。"/utf8>>}
    ],

    %% 3. 添加用户消息
    UserMsg = <<"你好，请介绍一下自己"/utf8>>,
    History1 = History0 ++ [#{role => user, content => UserMsg}],
    io:format("User: ~ts~n~n", [UserMsg]),

    %% 4. 调用 Kernel 获取回复
    case beamai:chat(K1, History1) of
        {ok, #{content := Content}} ->
            io:format("Assistant: ~ts~n~n", [Content]),

            %% 5. 将回复加入历史
            _History2 = History1 ++ [#{role => assistant, content => Content}],
            io:format("(Chat history now has ~B messages)~n", [3]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%% @doc 运行多轮对话示例（使用 Zhipu GLM-4.6）
-spec multi_turn() -> ok.
multi_turn() ->
    LLMConfig = example_llm_config:zhipu(),
    multi_turn(LLMConfig).

%% @doc 运行多轮对话示例
%%
%% 演示如何维护对话历史进行多轮交互。
-spec multi_turn(beamai_chat_completion:config()) -> ok.
multi_turn(LLMConfig) ->
    io:format("=== BeamAI Kernel Multi-Turn Chat ===~n~n"),

    %% 1. 构建 Kernel
    K0 = beamai:kernel(),
    K1 = beamai:add_llm(K0, LLMConfig),

    %% 2. 初始化对话历史
    History0 = [
        #{role => system, content => <<"你是一个有帮助的助手。请简洁回答。"/utf8>>}
    ],

    %% 3. 多轮对话
    Questions = [
        <<"Erlang 是什么？"/utf8>>,
        <<"它的主要优势是什么？"/utf8>>,
        <<"给我一个简单的代码示例"/utf8>>
    ],

    chat_loop(K1, History0, Questions, 1).

%%====================================================================
%% 内部函数
%%====================================================================

-spec chat_loop(beamai_kernel:kernel(), [map()], [binary()], pos_integer()) -> ok.
chat_loop(_Kernel, _History, [], _Turn) ->
    io:format("~n=== Conversation ended ===~n"),
    ok;
chat_loop(Kernel, History, [Question | Rest], Turn) ->
    io:format("--- Turn ~B ---~n", [Turn]),
    io:format("User: ~ts~n~n", [Question]),

    History1 = History ++ [#{role => user, content => Question}],

    case beamai:chat(Kernel, History1) of
        {ok, #{content := Content}} ->
            io:format("Assistant: ~ts~n~n", [Content]),
            History2 = History1 ++ [#{role => assistant, content => Content}],
            chat_loop(Kernel, History2, Rest, Turn + 1);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.
