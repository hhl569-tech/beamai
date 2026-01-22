%%%-------------------------------------------------------------------
%%% @doc 协调器功能模块（纯函数模式）
%%%
%%% 提供便捷的 API 来创建多 Agent 协调器。
%%% 基于 beamai_agent 纯函数 API 实现，支持 Pipeline 和 Orchestrator 两种协调模式。
%%%
%%% == 协调模式 ==
%%%
%%% === Pipeline 模式（顺序协调）===
%%% 任务在 workers 间依次传递，每个 worker 的输出作为下一个的输入。
%%% 适合：内容生产流程、代码审查流程、数据分析流程。
%%%
%%% === Orchestrator 模式（编排协调）===
%%% 协调器可以委托、路由、并行调用多个 workers，并综合结果。
%%% 适合：复杂任务分解、多角度分析、专家咨询。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 创建 Pipeline 协调器
%%% Agents = [
%%%     #{name => <<"researcher">>, role => <<"研究员">>},
%%%     #{name => <<"writer">>, role => <<"写作者">>}
%%% ],
%%% {ok, Coord} = beamai_coordinator:new_pipeline(#{
%%%     agents => Agents,
%%%     llm => LLMConfig
%%% }),
%%%
%%% %% 运行任务
%%% {ok, Result, NewCoord} = beamai_coordinator:run(Coord, <<"研究AI发展趋势">>).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_coordinator).

-include_lib("beamai_agent/include/beamai_agent.hrl").

%% 核心 API
-export([
    new/1,                      %% 创建协调器（自动选择类型）
    new_pipeline/1,             %% 创建 Pipeline 协调器
    new_orchestrator/1,         %% 创建 Orchestrator 协调器
    run/2, run/3                %% 执行任务
]).

%% 状态查询 API
-export([
    get_type/1,                 %% 获取协调器类型
    get_workers/1,              %% 获取所有 worker 状态
    get_worker/2,               %% 获取指定 worker 状态
    get_coordinator_state/1     %% 获取协调器 agent 状态
]).

%% 状态导出/导入 API
-export([
    export/1,                   %% 导出协调器完整状态
    import/2                    %% 导入协调器状态
]).

%% 便捷函数
-export([
    delegate/3,                 %% 委托任务给指定 Worker
    delegate_parallel/3         %% 并行委托任务
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(coordinator, {
    type            :: pipeline | orchestrator,
    agents          :: [map()],             %% Agent 定义列表
    llm_config      :: map(),               %% LLM 配置
    coordinator_state :: beamai_agent:state(),  %% 协调器 agent 状态
    workers         :: #{binary() => beamai_agent:state()}  %% Worker 状态映射
}).

-type coordinator() :: #coordinator{}.
-type result() :: map().

-export_type([coordinator/0, result/0]).

%%====================================================================
%% 核心 API 实现
%%====================================================================

%% @doc 创建协调器（自动选择类型）
%%
%% 根据配置自动选择 Pipeline 或 Orchestrator 模式。
%%
%% @param Opts 配置选项
%%   - type: pipeline | orchestrator（默认 pipeline）
%%   - agents: Agent 定义列表
%%   - llm: LLM 配置
%% @returns {ok, Coordinator} | {error, Reason}
-spec new(map()) -> {ok, coordinator()} | {error, term()}.
new(Opts) ->
    Type = maps:get(type, Opts, pipeline),
    case Type of
        pipeline -> new_pipeline(Opts);
        orchestrator -> new_orchestrator(Opts);
        _ -> {error, {unknown_type, Type}}
    end.

%% @doc 创建 Pipeline 模式协调器
%%
%% @param Opts 配置选项
%%   - agents: Agent 定义列表
%%   - llm: LLM 配置
%%   - system_prompt: 可选，系统提示词
%%   - max_iterations: 可选，最大迭代次数
%% @returns {ok, Coordinator} | {error, Reason}
-spec new_pipeline(map()) -> {ok, coordinator()} | {error, term()}.
new_pipeline(Opts) ->
    Agents = maps:get(agents, Opts, []),
    LLMConfig = maps:get(llm, Opts, #{}),
    SystemPrompt = maps:get(system_prompt, Opts,
        beamai_coordinator_common:build_pipeline_prompt()),

    %% 创建 workers
    case beamai_coordinator_common:create_workers(Agents, LLMConfig) of
        {ok, Workers} ->
            %% 构建委托工具
            Tools = beamai_coordinator_common:build_delegate_tools(Agents, Workers),

            %% 创建协调器 agent
            AgentOpts = #{
                system_prompt => SystemPrompt,
                tools => Tools,
                llm => LLMConfig,
                max_iterations => maps:get(max_iterations, Opts, 10)
            },

            case beamai_agent:new(AgentOpts) of
                {ok, CoordState} ->
                    Coordinator = #coordinator{
                        type = pipeline,
                        agents = Agents,
                        llm_config = LLMConfig,
                        coordinator_state = CoordState,
                        workers = Workers
                    },
                    {ok, Coordinator};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建 Orchestrator 模式协调器
%%
%% @param Opts 配置选项
%%   - agents: Agent 定义列表
%%   - llm: LLM 配置
%%   - system_prompt: 可选，系统提示词
%%   - max_iterations: 可选，最大迭代次数
%% @returns {ok, Coordinator} | {error, Reason}
-spec new_orchestrator(map()) -> {ok, coordinator()} | {error, term()}.
new_orchestrator(Opts) ->
    Agents = maps:get(agents, Opts, []),
    LLMConfig = maps:get(llm, Opts, #{}),
    SystemPrompt = maps:get(system_prompt, Opts,
        beamai_coordinator_common:build_orchestrator_prompt()),

    %% 创建 workers
    case beamai_coordinator_common:create_workers(Agents, LLMConfig) of
        {ok, Workers} ->
            %% 构建工具（委托 + 路由 + 并行 + 综合）
            DelegateTools = beamai_coordinator_common:build_delegate_tools(Agents, Workers),
            RouterTool = beamai_coordinator_common:build_router_tool(Agents),
            ParallelTool = beamai_coordinator_common:build_parallel_tool(Agents, Workers),
            SynthesizeTool = beamai_coordinator_common:build_synthesize_tool(),

            Tools = DelegateTools ++ [RouterTool, ParallelTool, SynthesizeTool],

            %% 创建协调器 agent
            AgentOpts = #{
                system_prompt => SystemPrompt,
                tools => Tools,
                llm => LLMConfig,
                max_iterations => maps:get(max_iterations, Opts, 10)
            },

            case beamai_agent:new(AgentOpts) of
                {ok, CoordState} ->
                    Coordinator = #coordinator{
                        type = orchestrator,
                        agents = Agents,
                        llm_config = LLMConfig,
                        coordinator_state = CoordState,
                        workers = Workers
                    },
                    {ok, Coordinator};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 执行任务
%%
%% @param Coordinator 协调器状态
%% @param Message 任务消息
%% @returns {ok, Result, NewCoordinator} | {error, Reason, NewCoordinator}
-spec run(coordinator(), binary()) ->
    {ok, result(), coordinator()} | {error, term(), coordinator()}.
run(Coordinator, Message) ->
    run(Coordinator, Message, #{}).

%% @doc 执行任务（带选项）
%%
%% @param Coordinator 协调器状态
%% @param Message 任务消息
%% @param Opts 执行选项
%% @returns {ok, Result, NewCoordinator} | {error, Reason, NewCoordinator}
-spec run(coordinator(), binary(), map()) ->
    {ok, result(), coordinator()} | {error, term(), coordinator()}.
run(#coordinator{coordinator_state = CoordState, workers = Workers} = Coordinator,
    Message, Opts) ->
    %% 将 workers 放入上下文，供工具函数使用
    CoordState1 = beamai_agent:put_context(CoordState, workers, Workers),

    %% 执行协调器
    case beamai_agent:run(CoordState1, Message, Opts) of
        {ok, Result, NewCoordState} ->
            %% 从上下文中获取更新后的 workers
            NewWorkers = beamai_agent:get_context(NewCoordState, workers, Workers),
            NewCoordinator = Coordinator#coordinator{
                coordinator_state = NewCoordState,
                workers = NewWorkers
            },
            {ok, Result, NewCoordinator};
        {error, Reason, NewCoordState} ->
            NewWorkers = beamai_agent:get_context(NewCoordState, workers, Workers),
            NewCoordinator = Coordinator#coordinator{
                coordinator_state = NewCoordState,
                workers = NewWorkers
            },
            {error, Reason, NewCoordinator}
    end.

%%====================================================================
%% 状态查询 API 实现
%%====================================================================

%% @doc 获取协调器类型
-spec get_type(coordinator()) -> pipeline | orchestrator.
get_type(#coordinator{type = Type}) -> Type.

%% @doc 获取所有 worker 状态
-spec get_workers(coordinator()) -> #{binary() => beamai_agent:state()}.
get_workers(#coordinator{workers = Workers}) -> Workers.

%% @doc 获取指定 worker 状态
-spec get_worker(coordinator(), binary()) -> {ok, beamai_agent:state()} | {error, not_found}.
get_worker(#coordinator{workers = Workers}, Name) ->
    case maps:find(Name, Workers) of
        {ok, State} -> {ok, State};
        error -> {error, not_found}
    end.

%% @doc 获取协调器 agent 状态
-spec get_coordinator_state(coordinator()) -> beamai_agent:state().
get_coordinator_state(#coordinator{coordinator_state = State}) -> State.

%%====================================================================
%% 状态导出/导入 API 实现
%%====================================================================

%% @doc 导出协调器完整状态
%%
%% @param Coordinator 协调器
%% @returns 导出的状态 map
-spec export(coordinator()) -> map().
export(#coordinator{type = Type, agents = Agents, llm_config = LLMConfig,
                    coordinator_state = CoordState, workers = Workers}) ->
    %% 导出协调器状态
    CoordinatorExport = beamai_agent:export_state(CoordState),

    %% 导出所有 workers 状态
    WorkersExport = maps:map(fun(_Name, WorkerState) ->
        beamai_agent:export_state(WorkerState)
    end, Workers),

    #{
        type => Type,
        agents => Agents,
        llm_config => LLMConfig,
        coordinator_state => CoordinatorExport,
        workers_states => WorkersExport,
        exported_at => erlang:system_time(millisecond)
    }.

%% @doc 导入协调器状态
%%
%% @param ExportedData 通过 export/1 导出的数据
%% @param Config 配置（主要是 LLM 配置，如果导出数据中没有）
%% @returns {ok, Coordinator} | {error, Reason}
-spec import(map(), map()) -> {ok, coordinator()} | {error, term()}.
import(ExportedData, Config) ->
    #{
        type := Type,
        agents := Agents,
        coordinator_state := CoordinatorExport,
        workers_states := WorkersExport
    } = ExportedData,

    %% LLM 配置优先从 Config 获取，否则从导出数据获取
    LLMConfig = maps:get(llm, Config, maps:get(llm_config, ExportedData, #{})),

    %% 根据类型创建新协调器
    CreateFun = case Type of
        pipeline -> fun new_pipeline/1;
        orchestrator -> fun new_orchestrator/1
    end,

    case CreateFun(#{agents => Agents, llm => LLMConfig}) of
        {ok, #coordinator{coordinator_state = CoordState, workers = Workers} = Coordinator} ->
            %% 恢复协调器状态
            {ok, NewCoordState} = beamai_agent:import_state(CoordinatorExport,
                #{llm => LLMConfig}),

            %% 恢复 workers 状态
            NewWorkers = maps:fold(fun(Name, WorkerExport, Acc) ->
                case maps:find(Name, Workers) of
                    {ok, _WorkerState} ->
                        case beamai_agent:import_state(WorkerExport, #{llm => LLMConfig}) of
                            {ok, RestoredWorker} ->
                                Acc#{Name => RestoredWorker};
                            {error, _} ->
                                Acc
                        end;
                    error ->
                        Acc
                end
            end, Workers, WorkersExport),

            %% 保留原始 context（不被 import 覆盖的部分）
            FinalCoordState = restore_context_from_old(CoordState, NewCoordState),

            {ok, Coordinator#coordinator{
                coordinator_state = FinalCoordState,
                workers = NewWorkers
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 从旧状态恢复部分 context
-spec restore_context_from_old(beamai_agent:state(), beamai_agent:state()) ->
    beamai_agent:state().
restore_context_from_old(_OldState, NewState) ->
    NewState.

%%====================================================================
%% 便捷函数实现
%%====================================================================

%% @doc 委托任务给指定 Worker
%%
%% 直接调用某个 worker 执行任务并更新协调器状态。
%%
%% @param Coordinator 协调器状态
%% @param WorkerName Worker 名称
%% @param Task 任务描述
%% @returns {ok, Response, NewCoordinator} | {error, Reason, Coordinator}
-spec delegate(coordinator(), binary(), binary()) ->
    {ok, binary(), coordinator()} | {error, term(), coordinator()}.
delegate(#coordinator{workers = Workers} = Coordinator, WorkerName, Task) ->
    case maps:find(WorkerName, Workers) of
        {ok, WorkerState} ->
            case beamai_agent:run(WorkerState, Task) of
                {ok, Result, NewWorkerState} ->
                    Response = maps:get(final_response, Result, <<>>),
                    NewWorkers = Workers#{WorkerName => NewWorkerState},
                    NewCoordinator = Coordinator#coordinator{workers = NewWorkers},
                    {ok, Response, NewCoordinator};
                {error, Reason, NewWorkerState} ->
                    NewWorkers = Workers#{WorkerName => NewWorkerState},
                    NewCoordinator = Coordinator#coordinator{workers = NewWorkers},
                    {error, Reason, NewCoordinator}
            end;
        error ->
            {error, {worker_not_found, WorkerName}, Coordinator}
    end.

%% @doc 并行委托任务
%%
%% 同时调用多个 workers 执行相同任务。
%%
%% @param Coordinator 协调器状态
%% @param WorkerNames Worker 名称列表
%% @param Task 任务描述
%% @returns {ok, Results, NewCoordinator} | {error, Reason, Coordinator}
%%   Results 是 #{WorkerName => Response} 的映射
-spec delegate_parallel(coordinator(), [binary()], binary()) ->
    {ok, map(), coordinator()} | {error, term(), coordinator()}.
delegate_parallel(#coordinator{workers = Workers} = Coordinator, WorkerNames, Task) ->
    {Results, NewWorkers} = lists:foldl(fun(Name, {ResultsAcc, WorkersAcc}) ->
        case maps:find(Name, WorkersAcc) of
            {ok, WorkerState} ->
                case beamai_agent:run(WorkerState, Task) of
                    {ok, Result, NewWorkerState} ->
                        Response = maps:get(final_response, Result, <<>>),
                        {ResultsAcc#{Name => {ok, Response}},
                         WorkersAcc#{Name => NewWorkerState}};
                    {error, Reason, NewWorkerState} ->
                        {ResultsAcc#{Name => {error, Reason}},
                         WorkersAcc#{Name => NewWorkerState}}
                end;
            error ->
                {ResultsAcc#{Name => {error, not_found}}, WorkersAcc}
        end
    end, {#{}, Workers}, WorkerNames),

    NewCoordinator = Coordinator#coordinator{workers = NewWorkers},
    {ok, Results, NewCoordinator}.
