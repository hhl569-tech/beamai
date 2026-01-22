%%%-------------------------------------------------------------------
%%% @doc Agent 模块 - 纯函数 API
%%%
%%% 本模块提供 Agent 的纯函数 API，适用于：
%%% - 单次对话
%%% - 多轮对话
%%% - 高并发场景
%%% - 无状态服务
%%%
%%% == 使用示例 ==
%%%
%%% 单次执行：
%%% ```
%%% Config = #{llm => LLMConfig, system_prompt => <<"你是助手">>},
%%% {ok, State} = beamai_agent:new(Config),
%%% {ok, Result, _} = beamai_agent:run(State, <<"你好">>).
%%% ```
%%%
%%% 多轮对话：
%%% ```
%%% {ok, State0} = beamai_agent:new(Config),
%%% {ok, _, State1} = beamai_agent:run(State0, <<"你好">>),
%%% {ok, _, State2} = beamai_agent:run(State1, <<"继续">>).
%%% ```
%%%
%%% 带 Memory 持久化：
%%% ```
%%% {ok, Memory} = beamai_memory:new(#{...}),
%%% {ok, State} = beamai_agent:new(#{llm => LLMConfig, storage => Memory}),
%%% {ok, _, NewState} = beamai_agent:run(State, <<"你好">>),
%%% %% checkpoint 自动保存
%%%
%%% %% 恢复会话
%%% {ok, Restored} = beamai_agent:restore_from_memory(#{llm => LLMConfig}, Memory).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent).

-include("beamai_agent.hrl").

%%====================================================================
%% 核心 API
%%====================================================================
-export([
    new/1,                  %% 创建 Agent 状态
    run/2, run/3,           %% 带状态执行
    export_state/1,         %% 导出状态
    import_state/2,         %% 导入状态
    restore_from_memory/2   %% 从 Memory 恢复
]).

%%====================================================================
%% 辅助 API - 状态查询
%%====================================================================
-export([
    get_messages/1,         %% 获取消息历史
    get_full_messages/1,    %% 获取完整消息历史
    get_scratchpad/1,       %% 获取中间步骤
    get_context/1,          %% 获取完整上下文
    get_context/2,          %% 获取上下文值
    get_context/3           %% 获取上下文值（带默认值）
]).

%%====================================================================
%% 辅助 API - 状态修改
%%====================================================================
-export([
    set_context/2,          %% 设置完整上下文
    update_context/2,       %% 更新上下文（合并）
    put_context/3,          %% 设置单个上下文值
    clear_messages/1,       %% 清空消息历史
    clear_scratchpad/1      %% 清空中间步骤
]).

%%====================================================================
%% 图构建 API
%%====================================================================
-export([build_graph/1]).

%%====================================================================
%% 类型定义
%%====================================================================
-type state() :: #state{}.
-type result() :: map().
-type config() :: #{
    llm := map(),
    tools => [map()],
    system_prompt => binary(),
    storage => beamai_memory:memory(),
    context => map(),
    max_iterations => pos_integer(),
    response_format => map(),
    callbacks => map(),
    middlewares => [term()],
    meta => map()
}.

-export_type([state/0, result/0, config/0]).

%%====================================================================
%% 核心 API 实现
%%====================================================================

%% @doc 创建 Agent 状态
%%
%% 根据配置创建 Agent 状态。支持可选的 storage 配置用于自动 checkpoint。
%%
%% Config 选项：
%% - llm: LLM 配置（必须通过 llm_client:create/2 创建）
%% - tools: 工具列表（可选）
%% - system_prompt: 系统提示词（可选）
%% - storage: beamai_memory 实例（可选，用于自动 checkpoint）
%% - context: 初始上下文（可选）
%% - restore_latest: true 时从 storage 恢复最新状态（可选）
%% - restore_checkpoint: checkpoint ID，从指定 checkpoint 恢复（可选）
%%
%% @param Config 配置选项
%% @returns {ok, State} | {error, Reason}
-spec new(config()) -> {ok, state()} | {error, term()}.
new(Config) ->
    Id = maps:get(id, Config, beamai_utils:gen_id()),
    beamai_agent_init:create_state(Id, Config).

%% @doc 执行对话（简化版）
%%
%% @param State Agent 状态
%% @param Message 用户消息
%% @returns {ok, Result, NewState} | {error, Reason, NewState}
-spec run(state(), binary()) -> {ok, result(), state()} | {error, term(), state()}.
run(State, Message) ->
    run(State, Message, #{}).

%% @doc 执行对话（带选项）
%%
%% 执行一轮对话，返回结果和新状态。
%% 如果配置了 storage，执行完成后会自动保存 checkpoint。
%%
%% Opts 选项：
%% - timeout: 超时时间（毫秒）
%% - restore_from: 从指定 checkpoint 恢复后执行
%%
%% @param State Agent 状态
%% @param Message 用户消息
%% @param Opts 执行选项
%% @returns {ok, Result, NewState} | {error, Reason, NewState}
-spec run(state(), binary(), map()) -> {ok, result(), state()} | {error, term(), state()}.
run(#state{config = #agent_config{callbacks = Callbacks}} = State, Message, Opts) ->
    %% 触发 chain_start 回调
    Metadata = beamai_agent_callbacks:build_metadata(State),
    beamai_agent_callbacks:invoke(on_chain_start, [Message, Metadata], Callbacks),

    %% 执行图
    case beamai_agent_runner:execute(Message, Opts, State) of
        {ok, Result, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_end, [Result, Metadata],
                NewState#state.config#agent_config.callbacks),
            %% 自动保存 checkpoint（如果配置了 storage）
            FinalState = beamai_agent_checkpoint:maybe_auto_save(Result, NewState),
            {ok, Result, FinalState};
        {error, Reason, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_error, [Reason, Metadata],
                NewState#state.config#agent_config.callbacks),
            {error, Reason, NewState}
    end.

%% @doc 导出状态（用于外部持久化）
%%
%% 导出对话状态为可序列化的 map，可存储到 Redis、PostgreSQL、文件等。
%%
%% 导出内容：
%% - messages: 压缩后的消息（用于 LLM 调用）
%% - full_messages: 完整历史（用于审计/调试/回滚）
%% - scratchpad: 中间步骤记录
%% - context: 用户自定义上下文
%%
%% 配置数据不导出（在 import_state 时传入）。
%%
%% @param State Agent 状态
%% @returns 导出的状态 map
-spec export_state(state()) -> map().
export_state(#state{} = State) ->
    #{
        messages => State#state.messages,
        full_messages => State#state.full_messages,
        scratchpad => State#state.scratchpad,
        context => State#state.context
    }.

%% @doc 导入状态（从导出数据恢复）
%%
%% 从 export_state/1 的输出恢复对话状态。
%% 配置（tools, llm, middlewares 等）通过 Config 传入。
%%
%% @param ExportedData export_state/1 的输出
%% @param Config Agent 配置
%% @returns {ok, State} | {error, Reason}
-spec import_state(map(), config()) -> {ok, state()} | {error, term()}.
import_state(ExportedData, Config) when is_map(ExportedData), is_map(Config) ->
    Messages = maps:get(messages, ExportedData, []),
    FullMessages = maps:get(full_messages, ExportedData, []),
    Scratchpad = maps:get(scratchpad, ExportedData, []),
    Context = maps:get(context, ExportedData, #{}),

    %% 创建新状态
    case new(Config) of
        {ok, State} ->
            %% 恢复对话状态
            CurrentCtx = State#state.context,
            MergedCtx = maps:merge(CurrentCtx, Context),
            {ok, State#state{
                messages = Messages,
                full_messages = FullMessages,
                scratchpad = Scratchpad,
                context = MergedCtx
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 从 Memory 恢复最新状态
%%
%% 从 beamai_memory 恢复最新的 checkpoint 状态。
%% 适用于会话恢复场景。
%%
%% @param Config Agent 配置（必须包含 llm）
%% @param Memory beamai_memory 实例
%% @returns {ok, State} | {error, Reason}
-spec restore_from_memory(config(), beamai_memory:memory()) -> {ok, state()} | {error, term()}.
restore_from_memory(Config, Memory) ->
    ConfigWithStorage = Config#{
        storage => Memory,
        restore_latest => true
    },
    new(ConfigWithStorage).

%%====================================================================
%% 状态查询 API 实现
%%====================================================================

%% @doc 获取消息历史（可能已压缩）
-spec get_messages(state()) -> [map()].
get_messages(#state{messages = Messages}) ->
    Messages.

%% @doc 获取完整消息历史
-spec get_full_messages(state()) -> [map()].
get_full_messages(#state{full_messages = FullMessages}) ->
    FullMessages.

%% @doc 获取中间步骤记录
-spec get_scratchpad(state()) -> [map()].
get_scratchpad(#state{scratchpad = Scratchpad}) ->
    lists:reverse(Scratchpad).

%% @doc 获取完整上下文
-spec get_context(state()) -> map().
get_context(#state{context = Context}) ->
    Context.

%% @doc 获取上下文值
-spec get_context(state(), atom() | binary()) -> term() | undefined.
get_context(#state{context = Context}, Key) ->
    maps:get(Key, Context, undefined).

%% @doc 获取上下文值（带默认值）
-spec get_context(state(), atom() | binary(), term()) -> term().
get_context(#state{context = Context}, Key, Default) ->
    maps:get(Key, Context, Default).

%%====================================================================
%% 状态修改 API 实现
%%====================================================================

%% @doc 设置完整上下文
-spec set_context(state(), map()) -> state().
set_context(State, Context) when is_map(Context) ->
    State#state{context = Context}.

%% @doc 更新上下文（合并）
-spec update_context(state(), map()) -> state().
update_context(#state{context = Context} = State, Updates) when is_map(Updates) ->
    NewContext = maps:merge(Context, Updates),
    State#state{context = NewContext}.

%% @doc 设置单个上下文值
-spec put_context(state(), atom() | binary(), term()) -> state().
put_context(#state{context = Context} = State, Key, Value) ->
    NewContext = maps:put(Key, Value, Context),
    State#state{context = NewContext}.

%% @doc 清空消息历史
-spec clear_messages(state()) -> state().
clear_messages(State) ->
    State#state{messages = [], full_messages = []}.

%% @doc 清空中间步骤
-spec clear_scratchpad(state()) -> state().
clear_scratchpad(State) ->
    State#state{scratchpad = []}.

%%====================================================================
%% 图构建 API 实现
%%====================================================================

%% @doc 构建 Agent 执行图
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    beamai_agent_runner:build_graph(Opts).
