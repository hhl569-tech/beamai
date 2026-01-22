%%%-------------------------------------------------------------------
%%% @doc 节点注册表模块 - 管道配置
%%%
%%% 提供可配置的管道节点定义，支持：
%%% - 默认管道配置
%%% - 自定义管道构建
%%% - 节点覆盖支持
%%% - 中间件集成（统一接口）
%%%
%%% 本模块将管道配置与 beamai_agent_runner 中的硬编码节点列表解耦。
%%%
%%% == 节点创建统一接口 ==
%%% 所有节点通过统一的选项 map 传递参数：
%%% - llm_config: LLM 配置
%%% - tool_handlers: 工具处理器映射
%%% - middlewares: 中间件列表
%%% - response_format: 响应格式约束
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_node_registry).

-export([
    default_pipeline/0,
    build_pipeline/1,
    build_pipeline/2,
    get_node/2,
    get_node/3
]).

%% 节点规格类型
-type node_spec() :: {atom(), {module(), atom(), list()}} | {atom(), passthrough}.
-type pipeline() :: [node_spec()].

-export_type([node_spec/0, pipeline/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取默认管道配置
%%
%% 返回标准 Agent 执行管道：
%% 1. llm_call - 调用 LLM
%% 2. record_llm - 记录 LLM 响应到 scratchpad
%% 3. validate_response - 验证响应格式
%% 4. check_before_tools - 工具前检查点
%% 5. execute_tools - 执行工具调用
%% 6. record_tools - 记录工具结果到 scratchpad
%% 7. check_after_tools - 工具后检查点
%% 8. increment_iter - 递增迭代计数器
%%
%% @returns 管道规格列表
-spec default_pipeline() -> pipeline().
default_pipeline() ->
    [
        {llm_call, {beamai_nodes, llm_node, []}},
        {record_llm, {beamai_nodes, scratchpad_node, [llm_response]}},
        {validate_response, {beamai_nodes, validate_node, []}},
        {check_before_tools, passthrough},
        {execute_tools, {beamai_nodes, tool_node, []}},
        {record_tools, {beamai_nodes, scratchpad_node, [tool_result]}},
        {check_after_tools, passthrough},
        {increment_iter, {beamai_nodes, iteration_node, []}}
    ].

%% @doc 使用选项构建管道
%%
%% 从配置创建管道节点。支持：
%% - 自定义管道选择（通过 pipeline 键）
%% - LLM 配置注入
%% - 工具处理器注入
%% - 响应格式验证
%% - 中间件配置（统一注入到节点）
%%
%% @param Opts 管道选项 map
%% @returns 节点名称到节点函数的映射
-spec build_pipeline(map()) -> #{atom() => function()}.
build_pipeline(Opts) ->
    build_pipeline(Opts, #{}).

%% @doc 使用选项和覆盖构建管道
%%
%% @param Opts 管道选项
%% @param Overrides 节点覆盖映射 #{node_name => custom_fun}
%% @returns 节点名称到节点函数的映射
-spec build_pipeline(map(), map()) -> #{atom() => function()}.
build_pipeline(Opts, Overrides) ->
    %% 获取基础管道
    BasePipeline = maps:get(pipeline, Opts, default_pipeline()),

    %% 提取配置
    LLMConfig = maps:get(llm, Opts, #{}),
    Tools = maps:get(tools, Opts, []),
    ToolHandlers = beamai_nodes:build_tool_handlers(Tools),
    ResponseFormat = maps:get(response_format, Opts, undefined),
    Middlewares = maps:get(middlewares, Opts, []),

    %% 构建节点创建的上下文
    Context = #{
        llm_config => LLMConfig,
        tool_handlers => ToolHandlers,
        response_format => ResponseFormat,
        middlewares => Middlewares
    },

    %% 使用覆盖构建节点
    build_nodes_from_pipeline(BasePipeline, Context, Overrides).

%% @doc 从管道配置获取特定节点
%%
%% @param NodeName 要获取的节点名称
%% @param Opts 管道选项
%% @returns 节点函数或 undefined
-spec get_node(atom(), map()) -> function() | undefined.
get_node(NodeName, Opts) ->
    get_node(NodeName, Opts, undefined).

%% @doc 获取特定节点（带默认值）
%%
%% @param NodeName 要获取的节点名称
%% @param Opts 管道选项
%% @param Default 未找到时的默认值
%% @returns 节点函数或默认值
-spec get_node(atom(), map(), function() | undefined) -> function() | undefined.
get_node(NodeName, Opts, Default) ->
    Nodes = build_pipeline(Opts),
    maps:get(NodeName, Nodes, Default).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从管道规格构建节点函数
-spec build_nodes_from_pipeline(pipeline(), map(), map()) -> #{atom() => function()}.
build_nodes_from_pipeline(Pipeline, Context, Overrides) ->
    lists:foldl(fun({NodeName, Spec}, Acc) ->
        NodeFun = case maps:get(NodeName, Overrides, undefined) of
            undefined -> create_node(NodeName, Spec, Context);
            CustomFun -> CustomFun
        end,
        Acc#{NodeName => NodeFun}
    end, #{}, Pipeline).

%% @private 从规格创建节点函数
%%
%% 统一的节点创建接口，通过 Context 注入所有需要的参数。
%% 中间件通过选项 map 传递给节点，由节点自行决定是否使用。
-spec create_node(atom(), node_spec() | passthrough, map()) -> function().
create_node(_NodeName, passthrough, _Context) ->
    fun(State) -> {ok, State} end;

create_node(llm_call, {Module, Fun, _Args}, Context) ->
    %% LLM 节点：传入 LLM 配置和中间件
    LLMConfig = maps:get(llm_config, Context),
    Middlewares = maps:get(middlewares, Context),
    Module:Fun(LLMConfig, #{middlewares => Middlewares});

create_node(execute_tools, {Module, Fun, _Args}, Context) ->
    %% 工具节点：传入处理器映射和中间件
    ToolHandlers = maps:get(tool_handlers, Context),
    Middlewares = maps:get(middlewares, Context),
    Module:Fun(ToolHandlers, #{middlewares => Middlewares});

create_node(validate_response, {Module, Fun, _Args}, Context) ->
    %% 验证节点：传入响应格式
    ResponseFormat = maps:get(response_format, Context),
    Module:Fun(ResponseFormat);

create_node(record_llm, {Module, Fun, Args}, _Context) ->
    %% Scratchpad 节点：直接应用参数
    apply(Module, Fun, Args);

create_node(record_tools, {Module, Fun, Args}, _Context) ->
    %% Scratchpad 节点：直接应用参数
    apply(Module, Fun, Args);

create_node(increment_iter, {Module, Fun, _Args}, _Context) ->
    %% 迭代节点：无参数
    Module:Fun();

create_node(_NodeName, {Module, Fun, Args}, _Context) ->
    %% 其他节点：直接应用参数
    apply(Module, Fun, Args).
