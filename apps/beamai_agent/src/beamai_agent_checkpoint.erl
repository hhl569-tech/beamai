%%%-------------------------------------------------------------------
%%% @doc 检查点管理模块（内部使用）
%%%
%%% 负责 Agent 状态持久化的内部逻辑：
%%% - 存储初始化
%%% - 检查点恢复
%%%
%%% == 变更说明 ==
%%%
%%% 外部 Checkpoint API 已移除（save_checkpoint、load_checkpoint 等）。
%%% Checkpoint 现在通过以下方式管理：
%%% 1. 自动保存：通过 on_checkpoint 回调在图执行时自动保存
%%% 2. 访问数据：用户通过 beamai_memory API 直接访问 checkpoints
%%%
%%% 保留的内部功能：
%%% - init_storage/2: 从配置选项获取 Memory 实例
%%% - maybe_restore/2: 根据配置恢复检查点
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_checkpoint).

-include("beamai_agent.hrl").
-include_lib("beamai_memory/include/beamai_checkpointer.hrl").

%% 内部 API 导出
-export([
    init_storage/2,
    maybe_restore/2,
    maybe_auto_save/2
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化存储
%%
%% 从配置选项获取预创建的 Memory 实例。
%% Memory 必须由外部创建后通过 storage 选项传入。
-spec init_storage(binary(), map()) -> beamai_memory:memory() | undefined.
init_storage(_AgentId, Opts) ->
    case maps:get(storage, Opts, undefined) of
        undefined ->
            undefined;
        Memory when is_map(Memory) ->
            Memory
    end.

%% @doc 根据配置恢复检查点
%%
%% 支持两种恢复方式：
%% 1. restore_checkpoint => CpId - 恢复指定检查点
%% 2. restore_latest => true - 恢复最新检查点
-spec maybe_restore(map(), #state{}) -> #state{}.
maybe_restore(_Opts, #state{config = #agent_config{storage = undefined}} = State) ->
    State;
maybe_restore(Opts, #state{config = #agent_config{storage = Memory}} = State) ->
    ThreadId = beamai_memory:get_thread_id(Memory),
    case maps:get(restore_checkpoint, Opts, undefined) of
        undefined ->
            maybe_restore_latest(Opts, Memory, ThreadId, State);
        CpId when is_binary(CpId) ->
            restore_checkpoint_state(Memory, ThreadId, CpId, State)
    end.

%% @doc 运行完成后自动保存 checkpoint
%%
%% 在 Agent 执行完成后调用，将当前状态保存为 checkpoint。
%% 仅当配置了 storage (Memory) 时才会保存。
%%
%% 参数:
%% - Result: 运行结果 map，包含 status, final_response 等
%% - State: 当前 Agent 状态
%%
%% 返回: 原始 State（不修改）
-spec maybe_auto_save(map(), #state{}) -> #state{}.
maybe_auto_save(_Result, #state{config = #agent_config{storage = undefined}} = State) ->
    %% 未配置 storage，跳过保存
    State;
maybe_auto_save(Result, #state{config = #agent_config{storage = Memory, id = AgentId}} = State) ->
    ThreadId = beamai_memory:get_thread_id(Memory),
    %% 构建要保存的状态数据
    StateData = build_checkpoint_state(State),
    %% 构建元数据
    Metadata = #{
        checkpoint_type => final,
        agent_id => AgentId,
        status => maps:get(status, Result, unknown),
        iterations => maps:get(iterations, Result, 0)
    },
    Config = #{thread_id => ThreadId},
    %% 保存 checkpoint
    case beamai_memory:save_checkpoint(Memory, Config, StateData, Metadata) of
        ok ->
            State;
        {ok, _CpId} ->
            State;
        {error, Reason} ->
            logger:warning("自动保存 checkpoint 失败: ~p", [Reason]),
            State
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 恢复最新检查点
-spec maybe_restore_latest(map(), beamai_memory:memory(), binary(), #state{}) -> #state{}.
maybe_restore_latest(Opts, Memory, ThreadId, State) ->
    case maps:get(restore_latest, Opts, false) of
        true -> restore_latest_state(Memory, ThreadId, State);
        false -> State
    end.

%% @private 恢复最新检查点状态
-spec restore_latest_state(beamai_memory:memory(), binary(), #state{}) -> #state{}.
restore_latest_state(Memory, ThreadId, State) ->
    Config = #{thread_id => ThreadId},
    case beamai_memory:load_latest_checkpoint(Memory, Config) of
        {ok, Data} ->
            apply_checkpoint_data(Data, State);
        {error, not_found} ->
            State;
        {error, Reason} ->
            logger:warning("恢复最新检查点失败: ~p", [Reason]),
            State
    end.

%% @private 恢复指定检查点状态
-spec restore_checkpoint_state(beamai_memory:memory(), binary(), binary(), #state{}) -> #state{}.
restore_checkpoint_state(Memory, ThreadId, CpId, State) ->
    Config = #{thread_id => ThreadId, checkpoint_id => CpId},
    case beamai_memory:load_checkpoint(Memory, Config) of
        {ok, Data} ->
            apply_checkpoint_data(Data, State);
        {error, Reason} ->
            logger:warning("恢复检查点 ~s 失败: ~p", [CpId, Reason]),
            State
    end.

%% @private 应用检查点数据到状态
%%
%% 恢复 messages、full_messages、scratchpad、context 到状态。
%% checkpoint.values 直接是 global_state，键为 binary 格式。
%%
%% 注意：
%% - context 使用合并策略，保留当前状态中存在但检查点中不存在的键
-spec apply_checkpoint_data(map(), #state{}) -> #state{}.
apply_checkpoint_data(Data, #state{context = CurrentCtx} = State) ->
    %% global_state 使用 binary 键，所以同时检查 atom 和 binary 键
    Messages = get_field(Data, messages, []),
    FullMessages = get_field(Data, full_messages, []),
    Scratchpad = get_field(Data, scratchpad, []),
    %% Context 恢复：检查点数据覆盖当前值，但保留检查点中没有的当前键
    SavedCtx = get_field(Data, context, #{}),
    NewCtx = maps:merge(CurrentCtx, SavedCtx),
    State#state{
        messages = Messages,
        full_messages = FullMessages,
        scratchpad = Scratchpad,
        context = NewCtx
    }.

%% @private 从 checkpoint data 获取字段
%% 支持 atom 和 binary 键（global_state 使用 binary 键）
-spec get_field(map(), atom(), term()) -> term().
get_field(Data, Key, Default) when is_atom(Key) ->
    BinaryKey = atom_to_binary(Key, utf8),
    case maps:get(BinaryKey, Data, undefined) of
        undefined -> maps:get(Key, Data, Default);
        Value -> Value
    end.

%% @private 构建要保存的 checkpoint 状态数据
%%
%% 从 Agent 状态中提取需要持久化的数据：
%% - messages: 精简的消息历史
%% - full_messages: 完整的消息历史
%% - scratchpad: 中间步骤记录
%% - context: 上下文数据
-spec build_checkpoint_state(#state{}) -> map().
build_checkpoint_state(#state{
    messages = Messages,
    full_messages = FullMessages,
    scratchpad = Scratchpad,
    context = Context
}) ->
    %% 使用 binary 键以保持一致性
    #{
        <<"messages">> => Messages,
        <<"full_messages">> => FullMessages,
        <<"scratchpad">> => Scratchpad,
        <<"context">> => Context
    }.
