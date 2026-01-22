%%%-------------------------------------------------------------------
%%% @doc Agent Checkpoint 回调模块
%%%
%%% 创建 on_checkpoint 回调函数，注入给图执行层。
%%% 回调函数会在图执行的关键点被调用，自动保存检查点。
%%%
%%% == 回调签名 ==
%%%
%%% 回调函数签名: fun(Info, CheckpointData) -> continue | interrupt
%%% - Info: #{type => checkpoint_type(), superstep => integer(), ...}
%%% - CheckpointData: #{type, pregel_checkpoint, iteration, run_id, ...}
%%%
%%% == 回调类型 ==
%%%
%%% - initial: 初始化完成
%%% - step: 超步完成
%%% - final: 最终结果
%%% - error: 执行出错
%%% - interrupt: 用户中断
%%%
%%% == 使用方式 ==
%%%
%%% Agent 在构建图执行选项时调用 create_callback/2，
%%% 将返回的回调函数传给 graph:run/3 的 on_checkpoint 选项。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_checkpoint_callback).

-include("beamai_agent.hrl").

%% API 导出
-export([create_callback/2]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 创建 checkpoint 回调函数
%%
%% 回调函数签名: fun(Info, CheckpointData) -> continue | interrupt
%% - Info: pregel 超步信息 #{type, superstep, ...}
%% - CheckpointData: 检查点数据 #{type, pregel_checkpoint, iteration, run_id, ...}
%%
%% @param AgentState Agent 的当前状态（#state{}）
%% @param Memory beamai_memory 实例
%% @returns on_checkpoint 回调函数
-spec create_callback(#state{}, beamai_memory:memory()) -> function().
create_callback(#state{config = #agent_config{id = AgentId, name = AgentName}}, Memory) ->
    ThreadId = beamai_memory:get_thread_id(Memory),

    fun(Info, CheckpointData) ->
        %% 提取检查点类型
        CheckpointType = maps:get(type, CheckpointData, maps:get(type, Info, step)),

        %% 从 pregel_checkpoint 提取状态
        PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData, #{}),
        GraphState = extract_graph_state(PregelCheckpoint),

        %% 构建要保存的状态数据
        StateData = #{
            messages => maps:get(<<"messages">>, GraphState, maps:get(messages, GraphState, [])),
            full_messages => maps:get(<<"full_messages">>, GraphState, maps:get(full_messages, GraphState, [])),
            scratchpad => maps:get(<<"scratchpad">>, GraphState, maps:get(scratchpad, GraphState, [])),
            context => maps:get(<<"context">>, GraphState, maps:get(context, GraphState, #{})),
            %% 执行上下文信息
            checkpoint_type => CheckpointType,
            run_id => maps:get(run_id, CheckpointData, undefined),
            iteration => maps:get(iteration, CheckpointData, 0),
            superstep => maps:get(superstep, CheckpointData, maps:get(superstep, Info, 0)),
            active_vertices => maps:get(active_vertices, CheckpointData, []),
            completed_vertices => maps:get(completed_vertices, CheckpointData, [])
        },

        %% 构建配置
        Config = #{
            thread_id => ThreadId,
            run_id => maps:get(run_id, CheckpointData, undefined),
            agent_id => AgentId,
            agent_name => AgentName
        },

        %% 构建元数据
        MetadataMap = #{
            checkpoint_type => CheckpointType,
            iteration => maps:get(iteration, CheckpointData, 0),
            superstep => maps:get(superstep, CheckpointData, maps:get(superstep, Info, 0)),
            active_vertices => maps:get(active_vertices, CheckpointData, []),
            completed_vertices => maps:get(completed_vertices, CheckpointData, [])
        },

        %% 保存 checkpoint
        case beamai_memory:save_checkpoint(Memory, Config, StateData, MetadataMap) of
            ok -> continue;
            {error, Reason} ->
                logger:warning("Checkpoint 保存失败: ~p", [Reason]),
                continue
        end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从 pregel checkpoint 提取图状态
%%
%% 状态存储在 __start__ 顶点的 initial_state 字段中
-spec extract_graph_state(map()) -> map().
extract_graph_state(PregelCheckpoint) ->
    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    case maps:get('__start__', Vertices, undefined) of
        undefined ->
            #{};
        StartVertex ->
            Value = maps:get(value, StartVertex, #{}),
            maps:get(initial_state, Value, #{})
    end.
