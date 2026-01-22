%%%-------------------------------------------------------------------
%%% @doc 协调器公共工具模块（纯函数模式）
%%%
%%% 提供 Pipeline 和 Orchestrator 协调器的公共函数。
%%% 所有函数都是纯函数，使用 Agent 状态而非 PID。
%%%
%%% 功能分类：
%%%   - Worker 管理：创建 worker 状态
%%%   - 工具构建：委托、路由、并行、综合
%%%   - 提示词：协调器系统提示词
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_coordinator_common).

-include_lib("beamai_agent/include/beamai_agent.hrl").

%% 导出公共 API
-export([create_workers/2]).
-export([build_delegate_tools/2, build_router_tool/1]).
-export([build_parallel_tool/2, build_synthesize_tool/0]).
-export([build_pipeline_prompt/0, build_orchestrator_prompt/0]).

%%====================================================================
%% Worker 管理函数
%%====================================================================

%% @doc 创建所有 workers
%%
%% 为协调器创建多个子 Agent 状态。
%%
%% @param Agents Agent 定义列表
%% @param LLMConfig LLM 配置
%% @returns {ok, Workers} | {error, Reason}
%%   Workers 是 #{Name => State} 的映射
-spec create_workers([map()], map()) ->
    {ok, #{binary() => beamai_agent:state()}} | {error, term()}.
create_workers(Agents, LLMConfig) ->
    try
        Workers = lists:foldl(fun(AgentDef, Acc) ->
            case create_worker(AgentDef, LLMConfig) of
                {ok, Name, State} ->
                    Acc#{Name => State};
                {error, Reason} ->
                    throw({worker_creation_failed, Reason})
            end
        end, #{}, Agents),
        {ok, Workers}
    catch
        throw:{worker_creation_failed, Reason} ->
            {error, Reason}
    end.

%% @private 创建单个 worker
-spec create_worker(map(), map()) ->
    {ok, binary(), beamai_agent:state()} | {error, term()}.
create_worker(AgentDef, LLMConfig) ->
    Name = maps:get(name, AgentDef),
    Role = maps:get(role, AgentDef, Name),
    SystemPrompt = maps:get(system_prompt, AgentDef,
        <<"你是 ", Role/binary, "，负责完成分配给你的任务。"/utf8>>),

    WorkerOpts = #{
        system_prompt => SystemPrompt,
        llm => LLMConfig
    },

    case beamai_agent:new(WorkerOpts) of
        {ok, State} ->
            {ok, Name, State};
        {error, Reason} ->
            {error, {Name, Reason}}
    end.

%%====================================================================
%% 工具构建函数
%%====================================================================

%% @doc 构建委托工具列表
%%
%% 为每个 Agent 创建一个委托工具。
%% 工具通过 Context 中的 workers 来访问和更新 worker 状态。
%%
%% @param Agents Agent 定义列表
%% @param Workers Worker 状态映射（用于生成工具描述）
%% @returns 工具列表
-spec build_delegate_tools([map()], #{binary() => beamai_agent:state()}) -> [map()].
build_delegate_tools(Agents, _Workers) ->
    [build_delegate_tool(Agent) || Agent <- Agents].

%% @private 构建单个委托工具
-spec build_delegate_tool(map()) -> map().
build_delegate_tool(#{name := Name} = Agent) ->
    Role = maps:get(role, Agent, Name),
    Description = <<"委托任务给 ", Role/binary, "（", Name/binary,
        "）。必须提供 task 参数，描述要委托的具体任务。"/utf8>>,
    #{
        name => <<"delegate_to_", Name/binary>>,
        description => Description,
        parameters => #{
            type => object,
            properties => #{
                <<"task">> => #{
                    type => string,
                    description => <<"要执行的任务描述，必须提供具体任务内容"/utf8>>
                }
            },
            required => [<<"task">>]
        },
        handler => fun(Args, Context) ->
            delegate_handler(Name, Args, Context)
        end
    }.

%% @private 委托工具处理函数
%%
%% 从 Context 中获取 workers，执行委托，更新 workers 后存回 Context。
-spec delegate_handler(binary(), map(), map()) ->
    {ok, binary()} | {error, term()}.
delegate_handler(Name, Args, Context) ->
    %% 从参数或上下文中获取任务描述
    Task = case maps:find(<<"task">>, Args) of
        {ok, T} when is_binary(T), byte_size(T) > 0 -> T;
        _ ->
            case maps:find(original_input, Context) of
                {ok, Input} -> Input;
                _ ->
                    case maps:find(last_user_message, Context) of
                        {ok, Msg} -> Msg;
                        _ -> <<"请完成分配的任务"/utf8>>
                    end
            end
    end,

    %% 从上下文获取 workers
    Workers = maps:get(workers, Context, #{}),

    case maps:find(Name, Workers) of
        {ok, WorkerState} ->
            %% 执行 worker
            case beamai_agent:run(WorkerState, Task) of
                {ok, Result, NewWorkerState} ->
                    %% 更新 workers 到上下文
                    %% 注意：这里返回的 Context 更新需要在调用方处理
                    Response = maps:get(final_response, Result, <<>>),
                    %% 将更新后的 worker 状态存入上下文供后续使用
                    %% 由于工具 handler 不能直接修改 Context，我们通过返回特殊格式来处理
                    NewWorkers = Workers#{Name => NewWorkerState},
                    %% 返回结果并附带更新的 workers 信息
                    {ok, Response, #{updated_workers => NewWorkers}};
                {error, Reason, _NewWorkerState} ->
                    {error, Reason}
            end;
        error ->
            {error, {worker_not_found, Name}}
    end.

%% @doc 构建路由工具
%%
%% 创建一个工具，用于根据任务特点选择最合适的 worker。
%%
%% @param Agents Agent 定义列表
%% @returns 路由工具定义
-spec build_router_tool([map()]) -> map().
build_router_tool(Agents) ->
    WorkerNames = [maps:get(name, A) || A <- Agents],
    WorkersList = iolist_to_binary(lists:join(<<", ">>, WorkerNames)),
    Description = iolist_to_binary([
        <<"根据任务特点自动选择最合适的 worker。可用 workers: "/utf8>>,
        WorkersList
    ]),
    #{
        name => <<"route_to_workers">>,
        description => Description,
        parameters => #{
            type => object,
            properties => #{
                <<"task">> => #{
                    type => string,
                    description => <<"任务描述"/utf8>>
                },
                <<"candidates">> => #{
                    type => array,
                    description => <<"候选 worker 列表"/utf8>>,
                    items => #{type => string}
                }
            },
            required => [<<"task">>]
        },
        handler => fun(#{<<"task">> := Task}, _Context) ->
            %% 简单返回任务描述和可用 workers
            {ok, iolist_to_binary([<<"任务: "/utf8>>, Task, <<"\n可用 workers: ">>, WorkersList])}
        end
    }.

%% @doc 构建并行执行工具
%%
%% 创建一个工具，用于并行调用多个 workers。
%%
%% @param Agents Agent 定义列表
%% @param Workers Worker 状态映射
%% @returns 并行执行工具定义
-spec build_parallel_tool([map()], #{binary() => beamai_agent:state()}) -> map().
build_parallel_tool(Agents, _Workers) ->
    WorkerNames = [maps:get(name, A) || A <- Agents],
    WorkersList = iolist_to_binary(lists:join(<<", ">>, WorkerNames)),
    Description = iolist_to_binary([
        <<"并行调用多个 workers 并收集结果。可用 workers: "/utf8>>,
        WorkersList
    ]),
    #{
        name => <<"execute_parallel">>,
        description => Description,
        parameters => #{
            type => object,
            properties => #{
                <<"task">> => #{
                    type => string,
                    description => <<"要执行的并行任务"/utf8>>
                },
                <<"workers">> => #{
                    type => array,
                    description => <<"要调用的 worker 名称列表"/utf8>>,
                    items => #{type => string}
                }
            },
            required => [<<"task">>, <<"workers">>]
        },
        handler => fun(#{<<"task">> := Task, <<"workers">> := RequestedWorkers}, Context) ->
            parallel_handler(Task, RequestedWorkers, Context)
        end
    }.

%% @private 并行执行处理函数
-spec parallel_handler(binary(), [binary()], map()) ->
    {ok, binary()} | {error, term()}.
parallel_handler(Task, RequestedWorkers, Context) ->
    Workers = maps:get(workers, Context, #{}),

    %% 并行执行所有请求的 workers
    {Results, NewWorkers} = lists:foldl(fun(Name, {ResultsAcc, WorkersAcc}) ->
        case maps:find(Name, WorkersAcc) of
            {ok, WorkerState} ->
                case beamai_agent:run(WorkerState, Task) of
                    {ok, Result, NewWorkerState} ->
                        Response = maps:get(final_response, Result, <<>>),
                        {ResultsAcc#{Name => Response},
                         WorkersAcc#{Name => NewWorkerState}};
                    {error, Reason, NewWorkerState} ->
                        {ResultsAcc#{Name => <<"错误: ", (atom_to_binary(Reason))/binary>>},
                         WorkersAcc#{Name => NewWorkerState}}
                end;
            error ->
                {ResultsAcc#{Name => <<"未找到"/utf8>>}, WorkersAcc}
        end
    end, {#{}, Workers}, RequestedWorkers),

    %% 格式化结果
    FormattedResults = format_parallel_results(Results),
    {ok, FormattedResults, #{updated_workers => NewWorkers}}.

%% @private 格式化并行执行结果
-spec format_parallel_results(map()) -> binary().
format_parallel_results(Results) ->
    Lines = maps:fold(fun(Name, Response, Acc) ->
        [<<"\n## ", Name/binary, " 的结果:\n", Response/binary>> | Acc]
    end, [], Results),
    iolist_to_binary([<<"并行执行结果："/utf8>> | lists:reverse(Lines)]).

%% @doc 构建综合工具
%%
%% 创建一个工具，用于综合多个 worker 的结果。
%%
%% @returns 综合工具定义
-spec build_synthesize_tool() -> map().
build_synthesize_tool() ->
    #{
        name => <<"synthesize_results">>,
        description => <<"综合多个 worker 的结果，生成统一的总结"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"results">> => #{
                    type => object,
                    description => <<"各个 worker 的结果，格式: {worker_name: result}"/utf8>>
                }
            },
            required => [<<"results">>]
        },
        handler => fun(#{<<"results">> := Results}, _Context) ->
            {ok, synthesize_results(Results)}
        end
    }.

%% @private 综合多个结果
-spec synthesize_results(map()) -> binary().
synthesize_results(Results) ->
    FormattedResults = maps:fold(fun(Name, Result, Acc) ->
        NameBin = if
            is_binary(Name) -> Name;
            is_atom(Name) -> atom_to_binary(Name);
            true -> iolist_to_binary(io_lib:format("~p", [Name]))
        end,
        ResultBin = if
            is_binary(Result) -> Result;
            true -> iolist_to_binary(io_lib:format("~p", [Result]))
        end,
        [<<"\n### ", NameBin/binary, ":\n", ResultBin/binary, "\n">> | Acc]
    end, [], Results),
    iolist_to_binary([
        <<"## 综合结果\n\n"/utf8>>,
        lists:reverse(FormattedResults)
    ]).

%%====================================================================
%% 提示词构建函数
%%====================================================================

%% @doc 构建 Pipeline 模式的系统提示词
-spec build_pipeline_prompt() -> binary().
build_pipeline_prompt() ->
    <<"你是一个流水线协调器。你的职责是按顺序协调团队成员完成任务。\n\n"
      "重要规则：\n"
      "1. 必须按顺序调用每一个团队成员，不能跳过任何人\n"
      "2. 将前一个成员的结果传递给下一个成员\n"
      "3. 每个成员的输出是下一个成员的输入\n"
      "4. 最后汇总所有成员的工作成果\n\n"
      "工具调用格式：\n"
      "- 必须提供 task 参数，包含具体任务描述\n"
      "- 第一个成员：原始任务\n"
      "- 后续成员：基于前一个成员的结果给出新任务\n"
      "- 例如：{\"task\": \"根据以下研究资料撰写文章：[资料内容]\"}\n\n"
      "流程示例（3人团队：研究员→写作者→审核员）：\n"
      "1. delegate_to_researcher: 研究某主题\n"
      "2. delegate_to_writer: 基于研究结果撰写文章\n"
      "3. delegate_to_reviewer: 审核并改进文章质量\n\n"
      "注意：即使你认为任务已完成，也必须让所有成员参与！"/utf8>>.

%% @doc 构建 Orchestrator 模式的系统提示词
-spec build_orchestrator_prompt() -> binary().
build_orchestrator_prompt() ->
    <<"你是一个任务编排器。你的职责是：\n"
      "1. 分析任务，决定由哪个团队成员处理\n"
      "2. 可以委托给单个成员，或并行调用多个成员\n"
      "3. 综合各成员的结果，给出最终答案\n\n"
      "重要：调用 delegate_to_xxx 工具时，必须提供明确的 task 参数。\n"
      "例如：{\"task\": \"分析用户需求并设计系统架构\"}\n\n"
      "不要调用空参数的工具，这会导致执行失败。"/utf8>>.
