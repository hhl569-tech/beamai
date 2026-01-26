%%%-------------------------------------------------------------------
%%% @doc LLM 统一响应数据结构（beamai_core 核心模块）
%%%
%%% 提供统一的 LLM 响应格式，抽象不同 Provider 的差异，
%%% 同时保留原始数据以便获取 Provider 特有信息。
%%%
%%% 该模块位于 beamai_core 是因为：
%%% - 作为核心数据结构，被 LLM 层生成、被 Kernel 层消费
%%% - beamai_kernel 的工具调用循环需要可靠访问 tool_calls 和 content
%%% - 使用访问器函数比直接 map 模式匹配更健壮
%%%
%%% == 设计原则 ==
%%%
%%% 1. 统一访问：常用字段通过统一接口访问
%%% 2. 保留原始：原始响应完整保存，支持 Provider 特有字段
%%% 3. 类型安全：统一的类型定义，避免运行时错误
%%% 4. 可扩展：易于添加新 Provider 支持
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 从 Provider 响应创建
%%% {ok, Resp} = llm_response:from_provider(RawResponse, anthropic),
%%%
%%% %% 统一接口访问
%%% Content = llm_response:content(Resp),
%%% ToolCalls = llm_response:tool_calls(Resp),
%%% Usage = llm_response:usage(Resp),
%%%
%%% %% 获取 Provider 特有信息
%%% Raw = llm_response:raw(Resp),
%%% CacheTokens = maps:get(<<"cache_creation_input_tokens">>,
%%%                        maps:get(<<"usage">>, Raw, #{}), 0).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_response).

%% 构造函数
-export([from_provider/2, from_openai/1, from_anthropic/1]).
-export([from_ollama/1, from_dashscope/1, from_zhipu/1]).

%% HTTP Client 解析器（可直接传给 llm_http_client:request/5）
-export([parser_openai/0, parser_anthropic/0, parser/1]).
-export([parser_ollama/0, parser_dashscope/0, parser_zhipu/0]).

%% 统一访问接口
-export([id/1, model/1, provider/1]).
-export([content/1, content_blocks/1, reasoning_content/1]).
-export([tool_calls/1, has_tool_calls/1]).
-export([finish_reason/1, is_complete/1, needs_tool_call/1]).
-export([usage/1, input_tokens/1, output_tokens/1, total_tokens/1]).

%% 原始数据访问
-export([raw/1, raw_get/2, raw_get/3]).

%% 元数据
-export([metadata/1, set_metadata/3]).

%% 序列化
-export([to_map/1]).

%% 类型定义
-type response() :: #{
    '__struct__' := ?MODULE,
    id := binary(),
    model := binary(),
    provider := provider(),
    content := binary() | null,
    content_blocks := [content_block()],
    tool_calls := [tool_call()],
    finish_reason := finish_reason(),
    usage := usage(),
    raw := map(),
    metadata := map()
}.

-type provider() :: openai | anthropic | deepseek | zhipu | ollama | bailian | unknown.

-type content_block() ::
    #{type := text, text := binary()} |
    #{type := tool_use, id := binary(), name := binary(), input := map()}.

-type tool_call() :: #{
    id := binary(),
    name := binary(),
    arguments := map(),
    raw_arguments := binary()
}.

-type finish_reason() ::
    complete |           % 正常完成
    tool_use |           % 需要工具调用
    length_limit |       % 达到长度限制
    content_filtered |   % 内容过滤
    stop_sequence |      % 停止序列触发
    error |              % 错误
    unknown.             % 未知

-type usage() :: #{
    input_tokens := non_neg_integer(),
    output_tokens := non_neg_integer(),
    total_tokens := non_neg_integer(),
    details => map()  % Provider 特有的详细统计
}.

-export_type([response/0, provider/0, content_block/0, tool_call/0, finish_reason/0, usage/0]).

%%====================================================================
%% HTTP Client 解析器
%%====================================================================

%% @doc 返回 OpenAI 格式的解析器函数
%% 可直接用于 llm_http_client:request/5
-spec parser_openai() -> fun((map()) -> {ok, response()} | {error, term()}).
parser_openai() ->
    fun from_openai/1.

%% @doc 返回 Anthropic 格式的解析器函数
-spec parser_anthropic() -> fun((map()) -> {ok, response()} | {error, term()}).
parser_anthropic() ->
    fun from_anthropic/1.

%% @doc 返回指定 Provider 的解析器函数
-spec parser(provider()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser(openai) -> parser_openai();
parser(anthropic) -> parser_anthropic();
parser(deepseek) -> parser_openai();
parser(zhipu) -> parser_zhipu();
parser(ollama) -> parser_ollama();
parser(bailian) -> parser_dashscope();
parser(_) -> parser_openai().

%% @doc 返回 Ollama 格式的解析器函数
-spec parser_ollama() -> fun((map()) -> {ok, response()} | {error, term()}).
parser_ollama() ->
    fun from_ollama/1.

%% @doc 返回 DashScope 格式的解析器函数
-spec parser_dashscope() -> fun((map()) -> {ok, response()} | {error, term()}).
parser_dashscope() ->
    fun from_dashscope/1.

%% @doc 返回智谱格式的解析器函数（OpenAI兼容 + reasoning_content）
-spec parser_zhipu() -> fun((map()) -> {ok, response()} | {error, term()}).
parser_zhipu() ->
    fun from_zhipu/1.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 从指定 Provider 的原始响应创建统一响应
-spec from_provider(map(), provider()) -> {ok, response()} | {error, term()}.
from_provider(Raw, openai) -> from_openai(Raw);
from_provider(Raw, anthropic) -> from_anthropic(Raw);
from_provider(Raw, deepseek) -> from_openai(Raw);  % DeepSeek 使用 OpenAI 格式
from_provider(Raw, zhipu) -> from_zhipu(Raw);      % 智谱使用 OpenAI 格式 + reasoning_content
from_provider(Raw, ollama) -> from_ollama(Raw);    % Ollama 支持原生和 OpenAI 格式
from_provider(Raw, bailian) -> from_dashscope(Raw); % 百炼使用 DashScope 格式
from_provider(Raw, Provider) ->
    %% 默认尝试 OpenAI 格式
    case from_openai(Raw) of
        {ok, Resp} -> {ok, Resp#{provider => Provider}};
        Error -> Error
    end.

%% @doc 从 OpenAI 格式响应创建
-spec from_openai(map()) -> {ok, response()} | {error, term()}.
from_openai(#{<<"choices">> := [Choice | _]} = Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    {ok, #{
        '__struct__' => ?MODULE,
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => openai,
        content => maps:get(<<"content">>, Message, null),
        content_blocks => build_content_blocks_openai(Message),
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_openai(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_openai(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            created => maps:get(<<"created">>, Raw, undefined),
            system_fingerprint => maps:get(<<"system_fingerprint">>, Raw, undefined),
            object => maps:get(<<"object">>, Raw, undefined)
        }
    }};
from_openai(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_openai(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从 Anthropic 格式响应创建
-spec from_anthropic(map()) -> {ok, response()} | {error, term()}.
from_anthropic(#{<<"content">> := ContentBlocks} = Raw) when is_list(ContentBlocks) ->
    {Content, ToolCalls, Blocks} = extract_anthropic_content(ContentBlocks),
    {ok, #{
        '__struct__' => ?MODULE,
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => anthropic,
        content => Content,
        content_blocks => Blocks,
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_anthropic(maps:get(<<"stop_reason">>, Raw, <<>>)),
        usage => parse_usage_anthropic(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            type => maps:get(<<"type">>, Raw, undefined),
            role => maps:get(<<"role">>, Raw, undefined),
            stop_sequence => maps:get(<<"stop_sequence">>, Raw, undefined)
        }
    }};
from_anthropic(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_anthropic(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从智谱 AI 格式响应创建
%% 智谱使用 OpenAI 兼容格式，但有 reasoning_content 字段
-spec from_zhipu(map()) -> {ok, response()} | {error, term()}.
from_zhipu(#{<<"choices">> := [Choice | _]} = Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    ReasoningContent = maps:get(<<"reasoning_content">>, Message, null),
    %% 内容处理：如果 content 为空但有 reasoning_content，使用 reasoning_content
    Content = case maps:get(<<"content">>, Message, null) of
        null -> ReasoningContent;
        <<>> -> case ReasoningContent of null -> null; _ -> ReasoningContent end;
        C -> C
    end,
    {ok, #{
        '__struct__' => ?MODULE,
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => zhipu,
        content => Content,
        content_blocks => build_content_blocks_openai(Message),
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_openai(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_openai(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            created => maps:get(<<"created">>, Raw, undefined),
            reasoning_content => ReasoningContent
        }
    }};
from_zhipu(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_zhipu(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从 Ollama 格式响应创建
%% 支持 Ollama 原生格式和 OpenAI 兼容格式
-spec from_ollama(map()) -> {ok, response()} | {error, term()}.
from_ollama(#{<<"message">> := Message} = Raw) ->
    %% Ollama 原生格式
    ToolCalls = parse_tool_calls_openai(Message),
    Content = maps:get(<<"content">>, Message, <<>>),
    {ok, #{
        '__struct__' => ?MODULE,
        id => maps:get(<<"created_at">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => ollama,
        content => normalize_content(Content),
        content_blocks => case Content of <<>> -> []; _ -> [#{type => text, text => Content}] end,
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_ollama(maps:get(<<"done_reason">>, Raw, <<"stop">>)),
        usage => parse_usage_ollama(Raw),
        raw => Raw,
        metadata => #{
            done => maps:get(<<"done">>, Raw, false),
            total_duration => maps:get(<<"total_duration">>, Raw, undefined),
            load_duration => maps:get(<<"load_duration">>, Raw, undefined),
            prompt_eval_duration => maps:get(<<"prompt_eval_duration">>, Raw, undefined),
            eval_duration => maps:get(<<"eval_duration">>, Raw, undefined)
        }
    }};
from_ollama(#{<<"choices">> := _} = Raw) ->
    %% OpenAI 兼容格式，直接委托
    case from_openai(Raw) of
        {ok, Resp} -> {ok, Resp#{provider => ollama}};
        Error -> Error
    end;
from_ollama(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_ollama(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从阿里云 DashScope 格式响应创建
%% DashScope 使用 {output: {choices: [...]}, usage: {...}} 格式
-spec from_dashscope(map()) -> {ok, response()} | {error, term()}.
from_dashscope(#{<<"output">> := Output} = Raw) ->
    parse_dashscope_output(Output, Raw);
from_dashscope(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_dashscope(#{<<"code">> := Code, <<"message">> := Message}) ->
    {error, {api_error, #{code => Code, message => Message}}};
from_dashscope(Raw) ->
    {error, {invalid_response, Raw}}.

%%====================================================================
%% 统一访问接口
%%====================================================================

%% @doc 获取响应 ID
-spec id(response()) -> binary().
id(#{id := Id}) -> Id.

%% @doc 获取模型名称
-spec model(response()) -> binary().
model(#{model := Model}) -> Model.

%% @doc 获取 Provider 类型
-spec provider(response()) -> provider().
provider(#{provider := Provider}) -> Provider.

%% @doc 获取文本内容（合并后的）
-spec content(response()) -> binary() | null.
content(#{content := Content}) -> Content;
content(_) -> null.

%% @doc 获取原始内容块列表
-spec content_blocks(response()) -> [content_block()].
content_blocks(#{content_blocks := Blocks}) -> Blocks;
content_blocks(_) -> [].

%% @doc 获取推理内容（智谱 GLM-4.6+ 特有）
%% 返回 reasoning_content，如果不存在则返回 null
-spec reasoning_content(response()) -> binary() | null.
reasoning_content(#{metadata := #{reasoning_content := RC}}) -> RC;
reasoning_content(_) -> null.

%% @doc 获取工具调用列表
-spec tool_calls(response()) -> [tool_call()].
tool_calls(#{tool_calls := Calls}) -> Calls;
tool_calls(_) -> [].

%% @doc 是否有工具调用
-spec has_tool_calls(response()) -> boolean().
has_tool_calls(#{tool_calls := []}) -> false;
has_tool_calls(#{tool_calls := Calls}) when is_list(Calls), Calls =/= [] -> true;
has_tool_calls(_) -> false.

%% @doc 获取统一的结束原因
-spec finish_reason(response()) -> finish_reason().
finish_reason(#{finish_reason := Reason}) -> Reason;
finish_reason(_) -> unknown.

%% @doc 是否正常完成（无需进一步操作）
-spec is_complete(response()) -> boolean().
is_complete(#{finish_reason := complete}) -> true;
is_complete(_) -> false.

%% @doc 是否需要执行工具调用
-spec needs_tool_call(response()) -> boolean().
needs_tool_call(#{finish_reason := tool_use}) -> true;
needs_tool_call(#{tool_calls := Calls}) when length(Calls) > 0 -> true;
needs_tool_call(_) -> false.

%% @doc 获取 Token 使用统计
-spec usage(response()) -> usage().
usage(#{usage := Usage}) -> Usage;
usage(_) -> #{}.

%% @doc 获取输入 Token 数
-spec input_tokens(response()) -> non_neg_integer().
input_tokens(#{usage := #{input_tokens := N}}) -> N;
input_tokens(_) -> 0.

%% @doc 获取输出 Token 数
-spec output_tokens(response()) -> non_neg_integer().
output_tokens(#{usage := #{output_tokens := N}}) -> N;
output_tokens(_) -> 0.

%% @doc 获取总 Token 数
-spec total_tokens(response()) -> non_neg_integer().
total_tokens(#{usage := #{total_tokens := N}}) -> N;
total_tokens(_) -> 0.

%%====================================================================
%% 原始数据访问
%%====================================================================

%% @doc 获取原始响应数据
-spec raw(response()) -> map().
raw(#{raw := Raw}) -> Raw.

%% @doc 从原始数据中获取指定路径的值
-spec raw_get(response(), [binary()] | binary()) -> term() | undefined.
raw_get(Resp, Key) ->
    raw_get(Resp, Key, undefined).

%% @doc 从原始数据中获取指定路径的值，带默认值
-spec raw_get(response(), [binary()] | binary(), term()) -> term().
raw_get(#{raw := Raw}, Path, Default) when is_list(Path) ->
    get_nested(Raw, Path, Default);
raw_get(#{raw := Raw}, Key, Default) when is_binary(Key) ->
    maps:get(Key, Raw, Default).

%%====================================================================
%% 元数据
%%====================================================================

%% @doc 获取元数据
-spec metadata(response()) -> map().
metadata(#{metadata := Meta}) -> Meta.

%% @doc 设置元数据字段
-spec set_metadata(response(), term(), term()) -> response().
set_metadata(#{metadata := Meta} = Resp, Key, Value) ->
    Resp#{metadata => Meta#{Key => Value}}.

%%====================================================================
%% 序列化
%%====================================================================

%% @doc 转换为普通 map（不含 __struct__）
-spec to_map(response()) -> map().
to_map(Resp) ->
    maps:remove('__struct__', Resp).

%%====================================================================
%% OpenAI 内部函数
%%====================================================================

%% @private 解析 OpenAI 格式的工具调用
parse_tool_calls_openai(#{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    [parse_single_tool_call_openai(C) || C <- Calls];
parse_tool_calls_openai(_) ->
    [].

parse_single_tool_call_openai(#{<<"id">> := Id, <<"function">> := Func}) ->
    RawArgs = maps:get(<<"arguments">>, Func, <<"{}">>),
    #{
        id => Id,
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => safe_decode_json(RawArgs),
        raw_arguments => RawArgs
    };
parse_single_tool_call_openai(_) ->
    #{id => <<>>, name => <<>>, arguments => #{}, raw_arguments => <<"{}">>}.

%% @private 构建 OpenAI 内容块
build_content_blocks_openai(#{<<"content">> := Content, <<"tool_calls">> := ToolCalls})
  when Content =/= null, is_list(ToolCalls), length(ToolCalls) > 0 ->
    TextBlock = #{type => text, text => Content},
    ToolBlocks = [#{
        type => tool_use,
        id => maps:get(<<"id">>, TC, <<>>),
        name => maps:get(<<"name">>, maps:get(<<"function">>, TC, #{}), <<>>),
        input => safe_decode_json(maps:get(<<"arguments">>, maps:get(<<"function">>, TC, #{}), <<"{}">>))
    } || TC <- ToolCalls],
    [TextBlock | ToolBlocks];
build_content_blocks_openai(#{<<"content">> := Content}) when Content =/= null ->
    [#{type => text, text => Content}];
build_content_blocks_openai(#{<<"tool_calls">> := ToolCalls}) when is_list(ToolCalls), length(ToolCalls) > 0 ->
    [#{
        type => tool_use,
        id => maps:get(<<"id">>, TC, <<>>),
        name => maps:get(<<"name">>, maps:get(<<"function">>, TC, #{}), <<>>),
        input => safe_decode_json(maps:get(<<"arguments">>, maps:get(<<"function">>, TC, #{}), <<"{}">>))
    } || TC <- ToolCalls];
build_content_blocks_openai(_) ->
    [].

%% @private 解析 OpenAI usage
parse_usage_openai(Usage, Raw) ->
    InputTokens = maps:get(<<"prompt_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"completion_tokens">>, Usage, 0),
    Base = #{
        input_tokens => InputTokens,
        output_tokens => OutputTokens,
        total_tokens => maps:get(<<"total_tokens">>, Usage, InputTokens + OutputTokens)
    },
    %% 提取 OpenAI 特有的详细统计
    Details = extract_openai_usage_details(Usage, Raw),
    case maps:size(Details) of
        0 -> Base;
        _ -> Base#{details => Details}
    end.

%% @private 提取 OpenAI 特有的 usage 详情
extract_openai_usage_details(Usage, _Raw) ->
    Details = #{},
    %% completion_tokens_details (如 reasoning_tokens for o1)
    Details1 = case maps:get(<<"completion_tokens_details">>, Usage, undefined) of
        undefined -> Details;
        CompDetails -> Details#{completion_details => CompDetails}
    end,
    %% prompt_tokens_details (如 cached_tokens)
    case maps:get(<<"prompt_tokens_details">>, Usage, undefined) of
        undefined -> Details1;
        PromptDetails -> Details1#{prompt_details => PromptDetails}
    end.

%% @private 标准化 OpenAI finish_reason
normalize_finish_reason_openai(<<"stop">>) -> complete;
normalize_finish_reason_openai(<<"tool_calls">>) -> tool_use;
normalize_finish_reason_openai(<<"length">>) -> length_limit;
normalize_finish_reason_openai(<<"content_filter">>) -> content_filtered;
normalize_finish_reason_openai(<<>>) -> unknown;
normalize_finish_reason_openai(null) -> unknown;
normalize_finish_reason_openai(_) -> unknown.

%%====================================================================
%% Anthropic 内部函数
%%====================================================================

%% @private 提取 Anthropic 内容和工具调用
extract_anthropic_content(Blocks) ->
    extract_anthropic_content(Blocks, <<>>, [], []).

extract_anthropic_content([], Content, ToolCalls, ContentBlocks) ->
    {normalize_content(Content), lists:reverse(ToolCalls), lists:reverse(ContentBlocks)};
extract_anthropic_content([#{<<"type">> := <<"text">>, <<"text">> := T} | Rest], Content, ToolCalls, Blocks) ->
    Block = #{type => text, text => T},
    NewContent = <<Content/binary, T/binary>>,
    extract_anthropic_content(Rest, NewContent, ToolCalls, [Block | Blocks]);
extract_anthropic_content([#{<<"type">> := <<"tool_use">>} = B | Rest], Content, ToolCalls, Blocks) ->
    Id = maps:get(<<"id">>, B, <<>>),
    Name = maps:get(<<"name">>, B, <<>>),
    Input = maps:get(<<"input">>, B, #{}),
    ToolCall = #{
        id => Id,
        name => Name,
        arguments => Input,
        raw_arguments => jsx:encode(Input)
    },
    Block = #{type => tool_use, id => Id, name => Name, input => Input},
    extract_anthropic_content(Rest, Content, [ToolCall | ToolCalls], [Block | Blocks]);
extract_anthropic_content([_ | Rest], Content, ToolCalls, Blocks) ->
    extract_anthropic_content(Rest, Content, ToolCalls, Blocks).

%% @private 解析 Anthropic usage
parse_usage_anthropic(Usage, _Raw) ->
    InputTokens = maps:get(<<"input_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"output_tokens">>, Usage, 0),
    Base = #{
        input_tokens => InputTokens,
        output_tokens => OutputTokens,
        total_tokens => InputTokens + OutputTokens
    },
    %% 提取 Anthropic 特有的详细统计
    Details = extract_anthropic_usage_details(Usage),
    case maps:size(Details) of
        0 -> Base;
        _ -> Base#{details => Details}
    end.

%% @private 提取 Anthropic 特有的 usage 详情
extract_anthropic_usage_details(Usage) ->
    Details = #{},
    %% Cache tokens
    Details1 = case maps:get(<<"cache_creation_input_tokens">>, Usage, undefined) of
        undefined -> Details;
        CacheCreation -> Details#{cache_creation_input_tokens => CacheCreation}
    end,
    Details2 = case maps:get(<<"cache_read_input_tokens">>, Usage, undefined) of
        undefined -> Details1;
        CacheRead -> Details1#{cache_read_input_tokens => CacheRead}
    end,
    Details2.

%% @private 标准化 Anthropic stop_reason
normalize_finish_reason_anthropic(<<"end_turn">>) -> complete;
normalize_finish_reason_anthropic(<<"tool_use">>) -> tool_use;
normalize_finish_reason_anthropic(<<"max_tokens">>) -> length_limit;
normalize_finish_reason_anthropic(<<"stop_sequence">>) -> stop_sequence;
normalize_finish_reason_anthropic(<<>>) -> unknown;
normalize_finish_reason_anthropic(null) -> unknown;
normalize_finish_reason_anthropic(_) -> unknown.

%%====================================================================
%% Ollama 内部函数
%%====================================================================

%% @private 解析 Ollama 原生格式的 usage
parse_usage_ollama(Resp) ->
    PromptTokens = maps:get(<<"prompt_eval_count">>, Resp, 0),
    CompletionTokens = maps:get(<<"eval_count">>, Resp, 0),
    #{
        input_tokens => PromptTokens,
        output_tokens => CompletionTokens,
        total_tokens => PromptTokens + CompletionTokens
    }.

%% @private 标准化 Ollama done_reason
normalize_finish_reason_ollama(<<"stop">>) -> complete;
normalize_finish_reason_ollama(<<"length">>) -> length_limit;
normalize_finish_reason_ollama(<<>>) -> complete;  % Ollama 默认为完成
normalize_finish_reason_ollama(null) -> complete;
normalize_finish_reason_ollama(_) -> unknown.

%%====================================================================
%% DashScope 内部函数
%%====================================================================

%% @private 解析 DashScope output
parse_dashscope_output(#{<<"choices">> := [Choice | _]}, Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    Content = maps:get(<<"content">>, Message, null),
    {ok, #{
        '__struct__' => ?MODULE,
        id => maps:get(<<"request_id">>, Raw, <<>>),
        model => <<>>,  %% DashScope 响应不包含 model
        provider => bailian,
        content => Content,
        content_blocks => case Content of null -> []; _ -> [#{type => text, text => Content}] end,
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_dashscope(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_dashscope(maps:get(<<"usage">>, Raw, #{})),
        raw => Raw,
        metadata => #{
            request_id => maps:get(<<"request_id">>, Raw, undefined)
        }
    }};
%% 兼容旧格式：text + finish_reason 直接在 output 下
parse_dashscope_output(#{<<"text">> := Text, <<"finish_reason">> := FinishReason}, Raw) ->
    {ok, #{
        '__struct__' => ?MODULE,
        id => maps:get(<<"request_id">>, Raw, <<>>),
        model => <<>>,
        provider => bailian,
        content => Text,
        content_blocks => [#{type => text, text => Text}],
        tool_calls => [],
        finish_reason => normalize_finish_reason_dashscope(FinishReason),
        usage => parse_usage_dashscope(maps:get(<<"usage">>, Raw, #{})),
        raw => Raw,
        metadata => #{
            request_id => maps:get(<<"request_id">>, Raw, undefined)
        }
    }};
parse_dashscope_output(Output, _Raw) ->
    {error, {invalid_output, Output}}.

%% @private 解析 DashScope usage（使用 input_tokens/output_tokens）
parse_usage_dashscope(Usage) ->
    InputTokens = maps:get(<<"input_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"output_tokens">>, Usage, 0),
    #{
        input_tokens => InputTokens,
        output_tokens => OutputTokens,
        total_tokens => maps:get(<<"total_tokens">>, Usage, InputTokens + OutputTokens)
    }.

%% @private 标准化 DashScope finish_reason
normalize_finish_reason_dashscope(<<"stop">>) -> complete;
normalize_finish_reason_dashscope(<<"tool_calls">>) -> tool_use;
normalize_finish_reason_dashscope(<<"length">>) -> length_limit;
normalize_finish_reason_dashscope(<<>>) -> unknown;
normalize_finish_reason_dashscope(null) -> complete;
normalize_finish_reason_dashscope(_) -> unknown.

%%====================================================================
%% 通用内部函数
%%====================================================================

%% @private 安全解码 JSON
safe_decode_json(Bin) when is_binary(Bin) ->
    try jsx:decode(Bin, [return_maps])
    catch _:_ -> #{}
    end;
safe_decode_json(_) -> #{}.

%% @private 标准化内容（空字符串变为 null）
normalize_content(<<>>) -> null;
normalize_content(Content) -> Content.

%% @private 获取嵌套值
get_nested(Map, [], _Default) -> Map;
get_nested(Map, [Key | Rest], Default) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Value -> get_nested(Value, Rest, Default)
    end;
get_nested(_, _, Default) -> Default.
