%%%-------------------------------------------------------------------
%%% @doc Agent 模块 - 统一 API 入口
%%%
%%% 本模块作为 Agent 系统的统一 API 入口。
%%% 提供两种使用模式：
%%%
%%% == 纯函数模式（推荐） ==
%%% 适用于单次对话、高并发、无状态服务。
%%% 主要 API: run_once/2, create_state/1, run_with_state/3
%%%
%%% == 进程模式（高级） ==
%%% 适用于长期运行的服务、监督树、自动状态管理。
%%% 主要 API: start_link/2, run/2, stop/1
%%%
%%% == 设计说明 ==
%%% 本模块采用门面模式，将核心功能委托给专门的模块：
%%% - beamai_agent_server: gen_server 实现（有状态进程）
%%% - beamai_agent_api: 纯函数 API（无状态）
%%% - beamai_agent_runner: 图执行逻辑
%%% - beamai_coordinator: 多 Agent 协调
%%%
%%% 对于高级功能（如 Checkpoint、Middleware），可直接使用具体模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent).

-include("beamai_agent.hrl").

%%====================================================================
%% 核心 API - 纯函数模式
%%====================================================================
-export([
    %% 状态创建
    create_state/1,          %% 创建状态（自动 ID）
    create_state/2,          %% 创建状态（指定 ID）
    %% 执行
    run_once/2,              %% 单次执行（无状态）
    run_with_state/3,        %% 带状态执行（多轮对话）
    %% 状态导入导出
    export_state/1,          %% 导出状态
    export_state_pure/1,     %% 导出纯函数状态
    import_state_pure/2      %% 导入纯函数状态
]).

%%====================================================================
%% 核心 API - 进程模式
%%====================================================================
-export([
    %% 生命周期
    start_link/2,            %% 启动 Agent 进程
    stop/1,                  %% 停止 Agent 进程
    %% 执行
    run/2,                   %% 执行对话
    run/3,                   %% 执行对话（带选项）
    %% 状态导入
    import_state/2           %% 导入状态到进程
]).

%%====================================================================
%% 扩展 API - 工具和配置管理
%%====================================================================
-export([
    add_tool/2,              %% 添加工具
    remove_tool/2,           %% 移除工具
    set_system_prompt/2,     %% 设置系统提示词
    get_messages/1,          %% 获取消息历史
    clear_messages/1,        %% 清空消息历史
    get_scratchpad/1,        %% 获取中间步骤
    clear_scratchpad/1       %% 清空中间步骤
]).

%%====================================================================
%% 扩展 API - Context 和 Meta
%%====================================================================
-export([
    %% Context（用户自定义上下文，参与对话）
    get_context/1,           %% 获取完整上下文
    get_context/2,           %% 获取上下文值
    get_context/3,           %% 获取上下文值（带默认值）
    set_context/2,           %% 设置完整上下文
    update_context/2,        %% 更新上下文（合并）
    put_context/3,           %% 设置单个上下文值
    %% Meta（进程级元数据，不参与对话）
    get_meta/1,              %% 获取完整元数据
    get_meta/2,              %% 获取元数据值
    get_meta/3,              %% 获取元数据值（带默认值）
    set_meta/2,              %% 设置完整元数据
    put_meta/3               %% 设置单个元数据值
]).

%%====================================================================
%% 扩展 API - 回调和中间件
%%====================================================================
-export([
    %% 回调
    set_callbacks/2,         %% 设置回调处理器
    get_callbacks/1,         %% 获取当前回调配置
    emit_custom_event/3,     %% 发送自定义事件
    emit_custom_event/4,     %% 发送自定义事件（带元数据）
    %% 中间件
    get_middlewares/1,       %% 获取中间件列表
    add_middleware/2,        %% 添加中间件
    remove_middleware/2,     %% 移除中间件
    set_middlewares/2        %% 设置所有中间件
]).

%%====================================================================
%% 扩展 API - 中断控制
%%====================================================================
-export([
    resume/2,                %% 恢复中断的执行
    abort/1                  %% 中止当前执行
]).

%%====================================================================
%% 协调器 API
%%====================================================================
-export([
    start_pipeline/2,        %% 启动 Pipeline 协调器
    start_orchestrator/2     %% 启动 Orchestrator 协调器
]).

%%====================================================================
%% 图构建 API
%%====================================================================
-export([build_graph/1]).

%%====================================================================
%% 纯函数 API 实现
%%====================================================================

%% @doc 创建 Agent 状态（自动生成 ID）
%%
%% 根据配置创建 Agent 状态，用于纯函数模式的多轮对话。
%%
%% @param Config 配置选项，必须包含 llm 配置
%% @returns {ok, State} | {error, Reason}
-spec create_state(map()) -> {ok, #state{}} | {error, term()}.
create_state(Config) ->
    beamai_agent_api:create_state(Config).

%% @doc 创建 Agent 状态（指定 ID）
%%
%% @param Id Agent 唯一标识符
%% @param Config 配置选项
%% @returns {ok, State} | {error, Reason}
-spec create_state(binary(), map()) -> {ok, #state{}} | {error, term()}.
create_state(Id, Config) ->
    beamai_agent_api:create_state(Id, Config).

%% @doc 单次执行（无状态保留）
%%
%% 适用于单轮对话，每次调用创建新状态。
%% 这是最简单的使用方式，推荐用于简单场景。
%%
%% 示例:
%% ```erlang
%% Config = #{llm => LLMConfig, system_prompt => <<"你是助手">>},
%% {ok, Result} = beamai_agent:run_once(Config, <<"你好">>).
%% ```
%%
%% @param Config Agent 配置
%% @param Message 用户消息
%% @returns {ok, Result} | {error, Reason}
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
run_once(Config, Message) ->
    beamai_agent_api:run_once(Config, Message).

%% @doc 带状态执行（多轮对话）
%%
%% 保持对话上下文，适用于多轮对话场景。
%% 返回新状态，需要传递给下一次调用。
%%
%% 示例:
%% ```erlang
%% {ok, State0} = beamai_agent:create_state(Config),
%% {ok, Result1, State1} = beamai_agent:run_with_state(State0, <<"你好">>, #{}),
%% {ok, Result2, State2} = beamai_agent:run_with_state(State1, <<"继续">>, #{}).
%% ```
%%
%% @param State Agent 状态
%% @param Message 用户消息
%% @param Opts 执行选项
%% @returns {ok, Result, NewState} | {error, Reason, NewState}
-spec run_with_state(#state{}, binary(), map()) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
run_with_state(State, Message, Opts) ->
    beamai_agent_api:run_with_state(State, Message, Opts).

%% @doc 导出状态（用于外部持久化）
%%
%% 支持 pid（进程模式）或 #state{}（纯函数模式）。
-spec export_state(pid() | #state{}) -> map().
export_state(Pid) when is_pid(Pid) ->
    beamai_agent_server:export_state(Pid);
export_state(#state{} = State) ->
    beamai_agent_api:export_state(State).

%% @doc 导出纯函数状态
-spec export_state_pure(#state{}) -> map().
export_state_pure(#state{} = State) ->
    beamai_agent_api:export_state(State).

%% @doc 导入纯函数状态
-spec import_state_pure(map(), map()) -> {ok, #state{}} | {error, term()}.
import_state_pure(ExportedData, Config) ->
    beamai_agent_api:import_state(ExportedData, Config).

%%====================================================================
%% 进程 API 实现
%%====================================================================

%% @doc 启动 Agent 进程
%%
%% 创建一个 gen_server 进程来管理 Agent 状态。
%% 适用于需要长期运行的场景。
%%
%% 示例:
%% ```erlang
%% Config = #{llm => LLMConfig, system_prompt => <<"你是助手">>},
%% {ok, Pid} = beamai_agent:start_link(<<"agent-1">>, Config),
%% {ok, Result} = beamai_agent:run(Pid, <<"你好">>).
%% ```
%%
%% @param Id Agent 唯一标识符
%% @param Opts 配置选项
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Opts) ->
    beamai_agent_server:start_link(Id, Opts).

%% @doc 停止 Agent 进程
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_agent_server:stop(Pid).

%% @doc 执行对话
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
run(Pid, Msg) ->
    beamai_agent_server:run(Pid, Msg).

%% @doc 执行对话（带选项）
-spec run(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
run(Pid, Msg, Opts) ->
    beamai_agent_server:run(Pid, Msg, Opts).

%% @doc 导入状态到进程
-spec import_state(pid(), map()) -> ok.
import_state(Pid, ExportedData) when is_pid(Pid) ->
    beamai_agent_server:import_state(Pid, ExportedData).

%%====================================================================
%% 工具和配置管理实现
%%====================================================================

%% @doc 添加工具
-spec add_tool(pid(), map()) -> ok | {error, term()}.
add_tool(Pid, Tool) ->
    beamai_agent_server:add_tool(Pid, Tool).

%% @doc 移除工具
-spec remove_tool(pid(), binary()) -> ok | {error, term()}.
remove_tool(Pid, Name) ->
    beamai_agent_server:remove_tool(Pid, Name).

%% @doc 设置系统提示词
-spec set_system_prompt(pid(), binary()) -> ok | {error, term()}.
set_system_prompt(Pid, Prompt) ->
    beamai_agent_server:set_system_prompt(Pid, Prompt).

%% @doc 获取消息历史
-spec get_messages(pid()) -> [map()].
get_messages(Pid) ->
    beamai_agent_server:get_messages(Pid).

%% @doc 清空消息历史
-spec clear_messages(pid()) -> ok.
clear_messages(Pid) ->
    beamai_agent_server:clear_messages(Pid).

%% @doc 获取中间步骤记录
-spec get_scratchpad(pid()) -> [map()].
get_scratchpad(Pid) ->
    beamai_agent_server:get_scratchpad(Pid).

%% @doc 清空中间步骤
-spec clear_scratchpad(pid()) -> ok.
clear_scratchpad(Pid) ->
    beamai_agent_server:clear_scratchpad(Pid).

%%====================================================================
%% Context API 实现
%%====================================================================

%% @doc 获取完整上下文
-spec get_context(pid()) -> map().
get_context(Pid) ->
    beamai_agent_server:get_context(Pid).

%% @doc 获取上下文值
-spec get_context(pid(), atom() | binary()) -> term() | undefined.
get_context(Pid, Key) ->
    maps:get(Key, beamai_agent_server:get_context(Pid), undefined).

%% @doc 获取上下文值（带默认值）
-spec get_context(pid(), atom() | binary(), term()) -> term().
get_context(Pid, Key, Default) ->
    maps:get(Key, beamai_agent_server:get_context(Pid), Default).

%% @doc 设置完整上下文
-spec set_context(pid(), map()) -> ok.
set_context(Pid, Context) when is_map(Context) ->
    beamai_agent_server:set_context(Pid, Context).

%% @doc 更新上下文（合并）
-spec update_context(pid(), map()) -> ok.
update_context(Pid, Updates) when is_map(Updates) ->
    beamai_agent_server:update_context(Pid, Updates).

%% @doc 设置单个上下文值
-spec put_context(pid(), atom() | binary(), term()) -> ok.
put_context(Pid, Key, Value) ->
    beamai_agent_server:put_context(Pid, Key, Value).

%%====================================================================
%% Meta API 实现
%%====================================================================

%% @doc 获取完整元数据
-spec get_meta(pid()) -> map().
get_meta(Pid) ->
    beamai_agent_server:get_meta(Pid).

%% @doc 获取元数据值
-spec get_meta(pid(), atom() | binary()) -> term() | undefined.
get_meta(Pid, Key) ->
    maps:get(Key, beamai_agent_server:get_meta(Pid), undefined).

%% @doc 获取元数据值（带默认值）
-spec get_meta(pid(), atom() | binary(), term()) -> term().
get_meta(Pid, Key, Default) ->
    maps:get(Key, beamai_agent_server:get_meta(Pid), Default).

%% @doc 设置完整元数据
-spec set_meta(pid(), map()) -> ok.
set_meta(Pid, Meta) when is_map(Meta) ->
    beamai_agent_server:set_meta(Pid, Meta).

%% @doc 设置单个元数据值
-spec put_meta(pid(), atom() | binary(), term()) -> ok.
put_meta(Pid, Key, Value) ->
    beamai_agent_server:put_meta(Pid, Key, Value).

%%====================================================================
%% 回调和中间件 API 实现
%%====================================================================

%% @doc 设置回调处理器
-spec set_callbacks(pid(), map()) -> ok.
set_callbacks(Pid, CallbackOpts) ->
    beamai_agent_server:set_callbacks(Pid, CallbackOpts).

%% @doc 获取当前回调配置
-spec get_callbacks(pid()) -> map().
get_callbacks(Pid) ->
    beamai_agent_server:get_callbacks(Pid).

%% @doc 发送自定义事件
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
emit_custom_event(Pid, EventName, Data) ->
    beamai_agent_server:emit_custom_event(Pid, EventName, Data).

%% @doc 发送自定义事件（带元数据）
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
emit_custom_event(Pid, EventName, Data, Metadata) ->
    beamai_agent_server:emit_custom_event(Pid, EventName, Data, Metadata).

%% @doc 获取当前中间件列表
-spec get_middlewares(pid()) -> [term()].
get_middlewares(Pid) ->
    beamai_agent_server:get_middlewares(Pid).

%% @doc 添加中间件
-spec add_middleware(pid(), term()) -> ok | {error, term()}.
add_middleware(Pid, MiddlewareSpec) ->
    beamai_agent_server:add_middleware(Pid, MiddlewareSpec).

%% @doc 移除中间件
-spec remove_middleware(pid(), module()) -> ok | {error, term()}.
remove_middleware(Pid, Module) ->
    beamai_agent_server:remove_middleware(Pid, Module).

%% @doc 设置所有中间件
-spec set_middlewares(pid(), [term()]) -> ok | {error, term()}.
set_middlewares(Pid, Middlewares) ->
    beamai_agent_server:set_middlewares(Pid, Middlewares).

%%====================================================================
%% 中断控制 API 实现
%%====================================================================

%% @doc 恢复中断的执行
-spec resume(pid(), confirm | reject | {modify, term()}) -> ok | {error, term()}.
resume(Pid, Action) ->
    beamai_agent_server:resume(Pid, Action).

%% @doc 中止当前执行
-spec abort(pid()) -> ok.
abort(Pid) ->
    beamai_agent_server:abort(Pid).

%%====================================================================
%% 协调器 API 实现
%%====================================================================

%% @doc 启动 Pipeline 协调器
%%
%% 创建顺序执行的多 Agent 协调器。
%% 任务在 workers 间依次传递。
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
start_pipeline(Id, Opts) ->
    beamai_coordinator:start_pipeline(Id, Opts).

%% @doc 启动 Orchestrator 协调器
%%
%% 创建编排式的多 Agent 协调器。
%% 协调器可委托、路由、并行调用多个 workers。
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
start_orchestrator(Id, Opts) ->
    beamai_coordinator:start_orchestrator(Id, Opts).

%%====================================================================
%% 图构建 API 实现
%%====================================================================

%% @doc 构建 Agent 执行图
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    beamai_agent_runner:build_graph(Opts).
