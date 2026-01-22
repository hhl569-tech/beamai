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
    maybe_restore/2
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
