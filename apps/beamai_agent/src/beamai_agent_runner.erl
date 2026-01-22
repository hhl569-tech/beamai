%%%-------------------------------------------------------------------
%%% @doc 图执行模块
%%%
%%% 处理 Agent 图执行：
%%% - 图构建（通过节点注册表）
%%% - 图执行
%%% - 路由决策
%%% - 中断处理
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_runner).

-include("beamai_agent.hrl").

%% API 导出
-export([
    build_graph/1,
    execute/3,
    rebuild_graph/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 构建 Agent 执行图
%%
%% 创建用于 graph:run/2 的编译图。
%% 使用节点注册表构建可配置的管道。
%%
%% @param Opts 图选项
%% @returns {ok, CompiledGraph} | {error, Reason}
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    MaxIterations = maps:get(max_iterations, Opts, 10),

    %% 使用注册表构建节点
    Nodes = beamai_node_registry:build_pipeline(Opts),

    %% 从节点构建图
    build_graph_from_nodes(Nodes, MaxIterations).

%% @doc 执行图
%%
%% 执行编译后的图并返回结果。
%% 直接使用 Agent 的 graph_state，将新消息追加到历史记录。
%%
%% 消息压缩由 middleware_summarization 在
%% before_model 钩子中处理。
%%
%% @param Msg 用户消息
%% @param Opts 执行选项
%% @param State Agent 状态记录
%% @returns {ok, Result, NewState} | {error, Reason, NewState}
-spec execute(binary(), map(), #state{}) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
execute(Msg, Opts, #state{config = #agent_config{graph = Graph, system_prompt = Prompt,
                                                  tools = Tools, max_iterations = MaxIter,
                                                  callbacks = Callbacks,
                                                  id = AgentId, name = AgentName},
                           graph_state = GS} = State) ->
    %% 构建回调元数据（run_id 由图执行层管理，在图状态中获取）
    CallbackMeta = #{
        agent_id => AgentId,
        agent_name => AgentName,
        timestamp => erlang:system_time(millisecond)
    },

    %% 从 graph_state 获取历史消息
    HistoryMsgs = graph_state:get(GS, <<"messages">>, []),
    HistoryFullMsgs = graph_state:get(GS, <<"full_messages">>, []),
    Context = graph_state:get_context(GS),
    Scratchpad = graph_state:get(GS, <<"scratchpad">>, []),

    %% 将新用户消息追加到历史记录
    NewUserMsg = #{role => user, content => Msg},
    AllMessages = HistoryMsgs ++ [NewUserMsg],
    AllFullMessages = HistoryFullMsgs ++ [NewUserMsg],

    %% 构建包含原始输入的上下文（使用 binary 键）
    ContextWithInput = Context#{
        <<"original_input">> => Msg,
        <<"last_user_message">> => Msg
    },

    %% 设置图执行所需字段
    GS1 = graph_state:set_many(GS, [
        {<<"messages">>, AllMessages},
        {<<"full_messages">>, AllFullMessages},
        {<<"system_prompt">>, Prompt},
        {<<"tools">>, [to_tool_spec(Tool) || Tool <- Tools]},
        {<<"max_iterations">>, MaxIter},
        {<<"iteration">>, 0},
        {<<"scratchpad">>, Scratchpad},
        {<<"callbacks">>, beamai_agent_callbacks:to_map(Callbacks)},
        {<<"callback_meta">>, CallbackMeta}
    ]),
    %% 使用专用函数设置用户上下文
    InitState = graph_state:set_context(GS1, ContextWithInput),

    %% 构建执行选项（run_id 由图层自动生成，on_checkpoint 由 Agent 提供）
    RunOptions = build_run_options(State, Opts),

    %% 执行图（Pregel 引擎）
    handle_graph_result(graph:run(Graph, InitState, RunOptions), State).

%% @doc 从当前状态重建图
%%
%% 在工具/配置变更后使用。
%%
%% @param State 当前 Agent 状态
%% @returns {ok, NewState} | {error, Reason}
-spec rebuild_graph(#state{}) -> {ok, #state{}} | {error, term()}.
rebuild_graph(#state{config = #agent_config{tools = Tools, system_prompt = Prompt,
                                             llm_config = LLMConfig, max_iterations = MaxIter,
                                             middlewares = Middlewares, response_format = RF} = Config} = State) ->
    Opts = #{
        tools => Tools,
        system_prompt => Prompt,
        llm => LLMConfig,
        max_iterations => MaxIter,
        middlewares => Middlewares,
        response_format => RF
    },
    case build_graph(Opts) of
        {ok, NewGraph} ->
            NewConfig = Config#agent_config{graph = NewGraph},
            {ok, State#state{config = NewConfig}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数 - 执行选项
%%====================================================================

%% @private 构建图执行选项
%%
%% 提取 checkpoint 相关选项，并设置 Agent 特定的字段 Reducer。
%% run_id 由图执行层（pregel）自动生成。
%% 如果配置了 storage，自动创建 on_checkpoint 回调。
-spec build_run_options(#state{}, map()) -> map().
build_run_options(#state{config = #agent_config{storage = undefined,
                                                 context_reducers = CtxReducers}} = _State, Opts) ->
    %% 无存储，不创建 checkpoint 回调
    BaseOpts = maps:with([restore_from], Opts),
    BaseOpts#{field_reducers => agent_field_reducers(CtxReducers)};
build_run_options(#state{config = #agent_config{storage = Memory,
                                                 context_reducers = CtxReducers}} = State, Opts) ->
    %% 有存储，创建 checkpoint 回调
    OnCheckpoint = beamai_agent_checkpoint_callback:create_callback(State, Memory),
    BaseOpts = maps:with([restore_from], Opts),
    BaseOpts#{
        field_reducers => agent_field_reducers(CtxReducers),
        on_checkpoint => OnCheckpoint
    }.

%% @private Agent 特定的字段 Reducer 配置
%%
%% 定义 Agent 状态字段的合并策略：
%% - messages: append（追加，节点只设置新消息）
%% - full_messages: append（追加，完整历史）
%% - scratchpad: append（追加，中间步骤）
%% - user_context: 动态 reducer（基于 context_reducers 配置）
%% - 其他字段: last_write_win（默认）
%%
%% 重要：节点使用增量更新模式
%% - 节点只设置新增的消息，不包含历史消息
%% - append_reducer 负责将新消息追加到现有列表
%% - 例如：LLM 节点返回 [assistant_msg]，reducer 执行 existing ++ [assistant_msg]
-spec agent_field_reducers(map()) -> graph_state_reducer:field_reducers().
agent_field_reducers(ContextReducers) ->
    #{
        <<"messages">> => fun graph_state_reducer:append_reducer/2,
        <<"full_messages">> => fun graph_state_reducer:append_reducer/2,
        <<"scratchpad">> => fun graph_state_reducer:append_reducer/2,
        graph_state:context_key() => make_context_reducer(ContextReducers)
    }.

%% @private 创建用户上下文的动态 Reducer
%%
%% 根据 context_reducers 配置，为每个字段应用相应的 reducer。
%% 未配置的字段使用 last_write_win 策略（新值覆盖旧值）。
%%
%% 支持两种 reducer 格式：
%% 1. 普通 reducer: fun(Old, New) -> Merged
%% 2. 转换型 reducer: {transform, TargetKey, ReducerFun}
%%
%% 示例：
%% ```
%% ContextReducers = #{
%%     <<"items">> => fun graph_state_reducer:append_reducer/2,
%%     <<"counter_incr">> => {transform, <<"counter">>, fun graph_state_reducer:increment_reducer/2}
%% },
%% %% 节点返回：#{counter_incr => 5, items => [new_item]}
%% %% 合并结果：counter += 5（counter_incr 不保留），items 追加
%% ```
-spec make_context_reducer(map()) -> function().
make_context_reducer(FieldReducers) when is_map(FieldReducers), map_size(FieldReducers) > 0 ->
    fun(OldCtx, NewCtx) when is_map(OldCtx), is_map(NewCtx) ->
        %% 复用 graph_state_reducer:apply_delta 的逻辑
        %% 注意：这里直接操作 map，不是 graph_state
        %% 但 apply_delta 内部使用 graph_state:get/set，对 map 也兼容
        graph_state_reducer:apply_delta(OldCtx, NewCtx, FieldReducers);
       (OldCtx, _NewCtx) ->
        %% NewCtx 不是 map，保持原值
        OldCtx
    end;
make_context_reducer(_) ->
    %% 无配置，使用默认的 merge_reducer
    fun graph_state_reducer:merge_reducer/2.

%%====================================================================
%% 内部函数 - 图构建
%%====================================================================

%% @private 从节点 map 构建图
-spec build_graph_from_nodes(map(), pos_integer()) -> {ok, map()} | {error, term()}.
build_graph_from_nodes(Nodes, MaxIterations) ->
    %% 创建构建器
    %% 每次迭代（LLM 调用 + 工具执行）大约需要 10 个超步
    %% 为每次迭代分配 15 个超步以确保足够
    Builder0 = graph:builder(#{
        max_iterations => MaxIterations * 15
    }),

    %% 添加所有节点
    Builder1 = add_all_nodes(Builder0, Nodes),

    %% 添加边
    Builder2 = add_all_edges(Builder1),

    %% 设置入口点并编译
    Builder3 = graph:set_entry(Builder2, llm_call),
    graph:compile(Builder3).

%% @private 向构建器添加所有节点
-spec add_all_nodes(map(), map()) -> map().
add_all_nodes(Builder, Nodes) ->
    NodeList = [
        {llm_call, maps:get(llm_call, Nodes)},
        {record_llm, maps:get(record_llm, Nodes)},
        {validate_response, maps:get(validate_response, Nodes)},
        {check_before_tools, maps:get(check_before_tools, Nodes)},
        {execute_tools, maps:get(execute_tools, Nodes)},
        {record_tools, maps:get(record_tools, Nodes)},
        {check_after_tools, maps:get(check_after_tools, Nodes)},
        {increment_iter, maps:get(increment_iter, Nodes)}
    ],
    lists:foldl(
        fun({Name, Node}, Acc) -> graph:add_node(Acc, Name, Node) end,
        Builder,
        NodeList
    ).

%% @private 向构建器添加所有边
-spec add_all_edges(map()) -> map().
add_all_edges(Builder) ->
    %% 顺序边
    Builder1 = graph:add_edge(Builder, llm_call, record_llm),
    Builder2 = graph:add_edge(Builder1, record_llm, validate_response),
    Builder3 = graph:add_edge(Builder2, execute_tools, record_tools),
    Builder4 = graph:add_edge(Builder3, record_tools, check_after_tools),
    Builder5 = graph:add_edge(Builder4, increment_iter, llm_call),

    %% 条件边
    Builder6 = graph:add_conditional_edge(Builder5, validate_response, fun route_after_llm/1),
    Builder7 = graph:add_conditional_edge(Builder6, check_before_tools, fun route_before_tools/1),
    graph:add_conditional_edge(Builder7, check_after_tools, fun route_after_tools/1).

%%====================================================================
%% 内部函数 - 路由
%%====================================================================

%% @private LLM 调用后的路由
-spec route_after_llm(map()) -> atom().
route_after_llm(State) ->
    case beamai_nodes:route_after_llm(State) of
        '__end__' -> '__end__';
        execute_tools -> check_before_tools
    end.

%% @private check_before_tools 后的路由
%%
%% 处理工具前检查点后的路由：
%% - rejected: 结束执行（用户拒绝了操作）
%% - 正常流程: 继续到 execute_tools
%%
%% 注意: interrupt 现在在 pregel 层处理，不再路由到 '__end__'
-spec route_before_tools(map()) -> atom().
route_before_tools(State) ->
    Rejected = graph:get(State, rejected, false),
    case Rejected of
        true -> '__end__';
        false -> execute_tools
    end.

%% @private check_after_tools 后的路由
%%
%% 处理工具后检查点后的路由：
%% - rejected: 结束执行
%% - 正常流程: 继续到 increment_iter（下一次 LLM 调用）
%%
%% 注意: interrupt 现在在 pregel 层处理，不再路由到 '__end__'
-spec route_after_tools(map()) -> atom().
route_after_tools(State) ->
    Rejected = graph:get(State, rejected, false),
    case Rejected of
        true -> '__end__';
        false -> increment_iter
    end.

%%====================================================================
%% 内部函数 - 结果处理
%%====================================================================

%% @private 处理图执行结果
%%
%% 直接使用 FinalState 作为新的 graph_state，无需提取和重建。
-spec handle_graph_result(map(), #state{}) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
handle_graph_result(#{status := completed, final_state := FinalState}, State) ->
    Result = build_result(FinalState),
    NewState = State#state{graph_state = FinalState},
    {ok, Result, NewState};

handle_graph_result(#{status := max_iterations, final_state := FinalState}, State) ->
    Result = build_result(FinalState),
    NewState = State#state{graph_state = FinalState},
    {ok, Result#{warning => max_iterations_reached}, NewState};

%% 处理停止状态（pregel 层的中断）
handle_graph_result(#{status := stopped, final_state := FinalState,
                      checkpoint := Checkpoint, checkpoint_type := Type}, State) ->
    Result = build_result(FinalState),
    NewState = State#state{graph_state = FinalState},
    %% 返回中断信息供调用方处理
    {ok, Result#{
        status => interrupted,
        checkpoint => Checkpoint,
        checkpoint_type => Type,
        pending_action => graph_state:get(FinalState, <<"pending_action">>, undefined),
        interrupt_point => graph_state:get(FinalState, <<"interrupt_point">>, undefined)
    }, NewState};

handle_graph_result(#{status := error, error := Reason, final_state := FinalState}, State) ->
    NewState = State#state{graph_state = FinalState},
    {error, Reason, NewState};

handle_graph_result(#{status := error, final_state := FinalState}, State) ->
    NewState = State#state{graph_state = FinalState},
    {error, unknown_error, NewState};

handle_graph_result(UnexpectedResult, State) ->
    logger:warning("意外的图执行结果: ~p", [UnexpectedResult]),
    {error, {unexpected_graph_result, UnexpectedResult}, State}.

%% @private 从图状态构建结果
-spec build_result(graph_state:state()) -> map().
build_result(FinalState) ->
    Messages = graph_state:get(FinalState, <<"messages">>, []),
    FinalResponse = beamai_nodes:extract_final_response(Messages),
    FinishReason = graph_state:get(FinalState, <<"finish_reason">>, <<"stop">>),
    ValidatedContent = graph_state:get(FinalState, <<"validated_content">>, undefined),

    Result = #{
        status => completed,
        messages => Messages,
        final_response => FinalResponse,
        finish_reason => FinishReason,
        iterations => graph_state:get(FinalState, <<"iteration">>, 0)
    },

    maybe_add_validated_content(Result, ValidatedContent).

%% @private 如果存在则添加验证后的内容
-spec maybe_add_validated_content(map(), term()) -> map().
maybe_add_validated_content(Result, undefined) -> Result;
maybe_add_validated_content(Result, ValidatedContent) ->
    Result#{validated_content => ValidatedContent}.

%%====================================================================
%% 内部函数 - 辅助函数
%%====================================================================

%% @private 将工具定义转换为规格（移除 handler）
-spec to_tool_spec(map()) -> map().
to_tool_spec(Tool) ->
    case maps:is_key(handler, Tool) of
        true -> maps:remove(handler, Tool);
        false -> Tool
    end.
