%%%-------------------------------------------------------------------
%%% @doc beamai_cognition 命名空间工具模块
%%%
%%% 提供认知记忆模块专用的命名空间生成函数。
%%%
%%% == 命名空间结构 ==
%%%
%%% - 语义记忆: [<<"semantic">>, UserId, ...]
%%% - 情景记忆: [<<"episodic">>, UserId, ...]
%%% - 程序记忆: [<<"procedural">>, UserId, ...]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_cognition_helpers).

%%====================================================================
%%% API 导出
%%====================================================================

-export([
    %% 偏好记忆命名空间
    preference_namespace/1,

    %% 实体记忆命名空间
    entity_namespace/1,
    entity_namespace/2,

    %% 知识记忆命名空间
    knowledge_namespace/1,

    %% 技能记忆命名空间
    skill_namespace/1,

    %% 工作流记忆命名空间
    workflow_namespace/1,

    %% 对话片段命名空间
    episode_namespace/1,

    %% 交互事件命名空间
    event_namespace/1,
    event_namespace/2,

    %% 经验记忆命名空间
    experience_namespace/1
]).

%%====================================================================
%%% 类型定义
%%====================================================================

-type user_id() :: binary().
-type namespace() :: [binary()].

%%====================================================================
%%% 命名空间处理函数
%%====================================================================

%% @doc 生成偏好记忆命名空间
%%
%% 构造格式：[<<"procedural">>, UserId, <<"preferences">>]
-spec preference_namespace(user_id()) -> namespace().
preference_namespace(UserId) ->
    [<<"procedural">>, UserId, <<"preferences">>].

%% @doc 生成实体记忆命名空间（仅用户ID）
%%
%% 构造格式：[<<"semantic">>, UserId, <<"entities">>]
-spec entity_namespace(user_id()) -> namespace().
entity_namespace(UserId) ->
    [<<"semantic">>, UserId, <<"entities">>].

%% @doc 生成实体记忆命名空间（用户ID + 实体类型）
%%
%% 构造格式：[<<"semantic">>, UserId, <<"entities">>, EntityType]
-spec entity_namespace(user_id(), binary()) -> namespace().
entity_namespace(UserId, EntityType) ->
    [<<"semantic">>, UserId, <<"entities">>, EntityType].

%% @doc 生成知识记忆命名空间
%%
%% 构造格式：[<<"semantic">>, UserId, <<"knowledge">>]
-spec knowledge_namespace(user_id()) -> namespace().
knowledge_namespace(UserId) ->
    [<<"semantic">>, UserId, <<"knowledge">>].

%% @doc 生成技能记忆命名空间
%%
%% 构造格式：[<<"procedural">>, UserId, <<"skills">>]
-spec skill_namespace(user_id()) -> namespace().
skill_namespace(UserId) ->
    [<<"procedural">>, UserId, <<"skills">>].

%% @doc 生成工作流记忆命名空间
%%
%% 构造格式：[<<"procedural">>, UserId, <<"workflows">>]
-spec workflow_namespace(user_id()) -> namespace().
workflow_namespace(UserId) ->
    [<<"procedural">>, UserId, <<"workflows">>].

%% @doc 生成对话片段命名空间
%%
%% 构造格式：[<<"episodic">>, UserId, <<"episodes">>]
-spec episode_namespace(user_id()) -> namespace().
episode_namespace(UserId) ->
    [<<"episodic">>, UserId, <<"episodes">>].

%% @doc 生成交互事件命名空间（仅用户ID）
%%
%% 构造格式：[<<"episodic">>, UserId, <<"events">>]
-spec event_namespace(user_id()) -> namespace().
event_namespace(UserId) ->
    [<<"episodic">>, UserId, <<"events">>].

%% @doc 生成交互事件命名空间（用户ID + 对话ID）
%%
%% 构造格式：[<<"episodic">>, UserId, <<"events">>, EpisodeId]
-spec event_namespace(user_id(), binary()) -> namespace().
event_namespace(UserId, EpisodeId) ->
    [<<"episodic">>, UserId, <<"events">>, EpisodeId].

%% @doc 生成经验记忆命名空间
%%
%% 构造格式：[<<"episodic">>, UserId, <<"experiences">>]
-spec experience_namespace(user_id()) -> namespace().
experience_namespace(UserId) ->
    [<<"episodic">>, UserId, <<"experiences">>].
