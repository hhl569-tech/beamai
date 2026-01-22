%%%-------------------------------------------------------------------
%%% @doc beamai_agent 共享头文件
%%%
%%% 定义 Agent 状态和回调记录，供子模块共享使用。
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_SIMPLE_HRL).
-define(AGENT_SIMPLE_HRL, true).

%% 回调处理器记录 (类似 LangChain BaseCallbackHandler)
-record(callbacks, {
    %% LLM 回调
    on_llm_start        :: function() | undefined,  %% fun(Prompts, Metadata) -> ok
    on_llm_end          :: function() | undefined,  %% fun(Response, Metadata) -> ok
    on_llm_error        :: function() | undefined,  %% fun(Error, Metadata) -> ok
    on_llm_new_token    :: function() | undefined,  %% fun(Token, Metadata) -> ok (流式)
    %% Tool 回调
    on_tool_start       :: function() | undefined,  %% fun(ToolName, Input, Metadata) -> ok
    on_tool_end         :: function() | undefined,  %% fun(ToolName, Output, Metadata) -> ok
    on_tool_error       :: function() | undefined,  %% fun(ToolName, Error, Metadata) -> ok
    %% Agent 回调
    on_agent_action     :: function() | undefined,  %% fun(Action, Metadata) -> ok
    on_agent_finish     :: function() | undefined,  %% fun(Result, Metadata) -> ok
    %% Chain 回调
    on_chain_start      :: function() | undefined,  %% fun(Input, Metadata) -> ok
    on_chain_end        :: function() | undefined,  %% fun(Output, Metadata) -> ok
    on_chain_error      :: function() | undefined,  %% fun(Error, Metadata) -> ok
    %% Retriever 回调 (RAG 相关)
    on_retriever_start  :: function() | undefined,  %% fun(Query, Metadata) -> ok
    on_retriever_end    :: function() | undefined,  %% fun(Documents, Metadata) -> ok
    on_retriever_error  :: function() | undefined,  %% fun(Error, Metadata) -> ok
    %% 其他回调
    on_text             :: function() | undefined,  %% fun(Text, Metadata) -> ok
    on_retry            :: function() | undefined,  %% fun(RetryState, Metadata) -> ok
    on_custom_event     :: function() | undefined   %% fun(EventName, Data, Metadata) -> ok
}).

%% Agent 配置（创建时设置，运行期间不变）
-record(agent_config, {
    id              :: binary(),                       %% Agent ID
    name            :: binary(),                       %% Agent 名称
    system_prompt   :: binary(),                       %% 系统提示词
    tools           :: [map()],                        %% 工具定义列表
    tool_handlers   :: #{binary() => function()},     %% 工具处理器映射
    llm_config      :: map(),                          %% LLM 配置
    graph           :: map() | undefined,              %% 编译后的图
    max_iterations  :: pos_integer(),                  %% 最大迭代次数
    response_format :: map() | undefined,              %% 输出格式约束
    callbacks       :: #callbacks{},                   %% 回调处理器集合
    middlewares     :: [term()],                       %% Middleware 配置列表
    middleware_chain :: list() | undefined,            %% 已初始化的 Middleware 链
    storage         :: beamai_memory:memory() | undefined, %% 存储后端
    meta            :: map()                           %% 进程级元数据
}).

%% Agent 运行时状态
%%
%% graph_state 使用 binary 键存储以下字段：
%% - <<"messages">>                 - 压缩的消息历史（用于 LLM 调用）
%% - <<"full_messages">>            - 完整消息历史（用于审计/调试）
%% - <<"scratchpad">>               - 中间步骤记录
%% - <<"__beamai_user_context__">>  - 用户上下文（使用特殊键避免冲突）
%% - <<"pending_action">>           - 等待确认的动作
%%
%% 注意：用户上下文使用特殊键名 <<"__beamai_user_context__">> 存储，
%% 应通过 graph_state:get_context/1,2 和 graph_state:set_context/2 访问，
%% 不要直接使用键名以避免与其他数据冲突。
%%
%% 设计优势：
%% 1. 数据单一来源 - 消除 Agent 状态和图状态之间的数据复制
%% 2. 直接持久化 - checkpoint 直接使用 graph_state，无需二次转换
%% 3. 简化 API - 状态操作通过 graph_state 统一管理
-record(state, {
    config          :: #agent_config{},        %% 配置（不可变）
    graph_state     :: graph_state:state()     %% 可变状态（存储在 graph_state 中）
}).

-endif. %% AGENT_SIMPLE_HRL
