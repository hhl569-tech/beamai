%%%-------------------------------------------------------------------
%%% @doc Deep Agent Checkpoint 回调模块
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
%%% == 与 beamai_agent 的区别 ==
%%%
%%% - 保存 Deep Agent 特有状态（plan、trace、subtasks 等）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_checkpoint_callback).

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
%% @param Config Deep Agent 配置
%% @param Memory beamai_memory 实例
%% @returns on_checkpoint 回调函数
-spec create_callback(map(), beamai_memory:memory()) -> function().
create_callback(Config, Memory) ->
    ThreadId = beamai_memory:get_thread_id(Memory),
    AgentId = maps:get(agent_id, Config, <<"deepagent">>),
    AgentName = maps:get(agent_name, Config, <<"DeepAgent">>),

    fun(Info, CheckpointData) ->
        %% 提取检查点类型
        CheckpointType = maps:get(type, CheckpointData, maps:get(type, Info, step)),

        %% 从 pregel_checkpoint 提取状态
        PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData, #{}),
        GraphState = extract_graph_state(PregelCheckpoint),

        %% 构建要保存的状态数据（Deep Agent 特有字段）
        StateData = #{
            %% 基础状态
            messages => get_state_value(GraphState, messages, []),
            system_prompt => get_state_value(GraphState, system_prompt, <<>>),

            %% Deep Agent 特有状态
            plan => serialize_plan(get_state_value(GraphState, plan, undefined)),
            trace => get_state_value(GraphState, trace, beamai_deepagent_trace:new()),
            subtasks => get_state_value(GraphState, subtasks, []),
            subtask_results => get_state_value(GraphState, subtask_results, []),

            %% 控制状态
            depth => get_state_value(GraphState, depth, 0),
            max_depth => get_state_value(GraphState, max_depth, 3),
            pending_tools => get_state_value(GraphState, pending_tools, []),
            tool_results => get_state_value(GraphState, tool_results, []),

            %% 执行上下文信息
            checkpoint_type => CheckpointType,
            run_id => maps:get(run_id, CheckpointData, undefined),
            iteration => maps:get(iteration, CheckpointData, 0),
            superstep => maps:get(superstep, CheckpointData, maps:get(superstep, Info, 0)),
            active_vertices => maps:get(active_vertices, CheckpointData, []),
            completed_vertices => maps:get(completed_vertices, CheckpointData, [])
        },

        %% 构建配置
        SaveConfig = #{
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
        case beamai_memory:save_checkpoint(Memory, SaveConfig, StateData, MetadataMap) of
            ok -> continue;
            {error, Reason} ->
                logger:warning("Deep Agent Checkpoint 保存失败: ~p", [Reason]),
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

%% @private 从状态中获取值，支持二进制和原子键
-spec get_state_value(map(), atom(), term()) -> term().
get_state_value(State, Key, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:get(BinKey, State, undefined) of
        undefined -> maps:get(Key, State, Default);
        Value -> Value
    end.

%% @private 序列化 plan
-spec serialize_plan(term()) -> map() | undefined.
serialize_plan(undefined) ->
    undefined;
serialize_plan(Plan) when is_map(Plan) ->
    Plan;
serialize_plan(_) ->
    undefined.
