%%%-------------------------------------------------------------------
%%% @doc llm_response 模块单元测试
%%%
%%% 测试 new/1 构造器和统一访问接口。
%%% Provider 特定解析测试已迁移至 llm_response_parser_tests。
%%% @end
%%%-------------------------------------------------------------------
-module(llm_response_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% new/1 构造器测试
%%====================================================================

new_defaults_test() ->
    Resp = llm_response:new(#{}),
    ?assertEqual(<<>>, llm_response:id(Resp)),
    ?assertEqual(<<>>, llm_response:model(Resp)),
    ?assertEqual(unknown, llm_response:provider(Resp)),
    ?assertEqual(null, llm_response:content(Resp)),
    ?assertEqual([], llm_response:content_blocks(Resp)),
    ?assertEqual([], llm_response:tool_calls(Resp)),
    ?assertEqual(unknown, llm_response:finish_reason(Resp)),
    ?assertEqual(#{input_tokens => 0, output_tokens => 0, total_tokens => 0}, llm_response:usage(Resp)),
    ?assertEqual(#{}, llm_response:raw(Resp)),
    ?assertEqual(#{}, llm_response:metadata(Resp)).

new_with_fields_test() ->
    Resp = llm_response:new(#{
        id => <<"test-123">>,
        model => <<"gpt-4">>,
        provider => openai,
        content => <<"Hello!">>,
        finish_reason => complete,
        usage => #{input_tokens => 10, output_tokens => 20, total_tokens => 30}
    }),
    ?assertEqual(<<"test-123">>, llm_response:id(Resp)),
    ?assertEqual(<<"gpt-4">>, llm_response:model(Resp)),
    ?assertEqual(openai, llm_response:provider(Resp)),
    ?assertEqual(<<"Hello!">>, llm_response:content(Resp)),
    ?assertEqual(complete, llm_response:finish_reason(Resp)),
    ?assertEqual(10, llm_response:input_tokens(Resp)),
    ?assertEqual(20, llm_response:output_tokens(Resp)),
    ?assertEqual(30, llm_response:total_tokens(Resp)).

%%====================================================================
%% 访问器测试
%%====================================================================

has_tool_calls_test() ->
    Empty = llm_response:new(#{}),
    ?assertEqual(false, llm_response:has_tool_calls(Empty)),

    WithTools = llm_response:new(#{tool_calls => [#{id => <<"1">>, name => <<"test">>, arguments => #{}, raw_arguments => <<>>}]}),
    ?assertEqual(true, llm_response:has_tool_calls(WithTools)).

is_complete_test() ->
    Complete = llm_response:new(#{finish_reason => complete}),
    ?assertEqual(true, llm_response:is_complete(Complete)),

    Incomplete = llm_response:new(#{finish_reason => tool_use}),
    ?assertEqual(false, llm_response:is_complete(Incomplete)).

needs_tool_call_test() ->
    ToolUse = llm_response:new(#{finish_reason => tool_use}),
    ?assertEqual(true, llm_response:needs_tool_call(ToolUse)),

    WithTools = llm_response:new(#{tool_calls => [#{id => <<"1">>, name => <<"test">>, arguments => #{}, raw_arguments => <<>>}]}),
    ?assertEqual(true, llm_response:needs_tool_call(WithTools)),

    NoTools = llm_response:new(#{finish_reason => complete}),
    ?assertEqual(false, llm_response:needs_tool_call(NoTools)).

metadata_test() ->
    Resp = llm_response:new(#{metadata => #{created => 1234567890}}),
    Meta = llm_response:metadata(Resp),
    ?assertEqual(1234567890, maps:get(created, Meta)),

    %% 设置新的元数据
    Resp2 = llm_response:set_metadata(Resp, latency_ms, 150),
    ?assertEqual(150, maps:get(latency_ms, llm_response:metadata(Resp2))).

to_map_test() ->
    Resp = llm_response:new(#{id => <<"test-1">>, content => <<"Test">>}),
    Map = llm_response:to_map(Resp),
    ?assertEqual(false, maps:is_key('__struct__', Map)),
    ?assertEqual(<<"test-1">>, maps:get(id, Map)).

raw_get_test() ->
    RawData = #{<<"key1">> => <<"val1">>, <<"deep">> => #{<<"nested">> => <<"value">>}},
    Resp = llm_response:new(#{raw => RawData}),

    ?assertEqual(<<"val1">>, llm_response:raw_get(Resp, <<"key1">>)),
    ?assertEqual(undefined, llm_response:raw_get(Resp, <<"nonexistent">>)),
    ?assertEqual(default_val, llm_response:raw_get(Resp, <<"nonexistent">>, default_val)),
    ?assertEqual(<<"value">>, llm_response:raw_get(Resp, [<<"deep">>, <<"nested">>])),
    ?assertEqual(default_val, llm_response:raw_get(Resp, [<<"deep">>, <<"missing">>], default_val)).

reasoning_content_test() ->
    NoReasoning = llm_response:new(#{}),
    ?assertEqual(null, llm_response:reasoning_content(NoReasoning)),

    WithReasoning = llm_response:new(#{metadata => #{reasoning_content => <<"thinking...">>}}),
    ?assertEqual(<<"thinking...">>, llm_response:reasoning_content(WithReasoning)).
