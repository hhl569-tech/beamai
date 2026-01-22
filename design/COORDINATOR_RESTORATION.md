# Coordinator 状态恢复设计方案

## 方案概述

使用独立的 Memory 实例（带不同 thread_id）为每个 Agent 提供独立的 checkpoint 存储，从而支持 Coordinator 的状态恢复。

## 核心思路

1. **共享 Store，独立 Memory**: 所有 Agent 使用同一个底层 Store，但通过不同的 `thread_id` 隔离各自的 checkpoint 数据
2. **Coordinator 元数据存储**: Coordinator 自身的配置信息存储在特殊的 thread_id 下
3. **按需恢复**: 恢复时先加载 Coordinator 元数据，再根据配置恢复各个 Agent

## 数据结构设计

### Coordinator Record 扩展

```erlang
-record(coordinator, {
    id :: binary(),
    type :: pipeline | orchestrator,
    agents :: [agent_def()],           %% Agent 定义列表
    llm_config :: map(),               %% LLM 配置
    coordinator_state :: map(),        %% Coordinator 内部状态
    workers :: #{binary() => pid()},   %% Worker Agent PID 映射

    %% 新增字段
    base_memory :: beamai_memory:memory() | undefined,  %% 基础 Memory（用于派生）
    thread_id :: binary()              %% Coordinator 自身的 thread_id
}).
```

### Memory 派生函数

```erlang
%% 在 beamai_memory 模块中添加
-spec with_thread_id(memory(), binary()) -> memory().
with_thread_id(Memory, ThreadId) ->
    Memory#{thread_id => ThreadId}.

%% 在 beamai_coordinator 中使用
derive_memory(BaseMemory, AgentName) ->
    ThreadId = <<(maps:get(thread_id, BaseMemory, <<"default">>))/binary,
                 "/", AgentName/binary>>,
    beamai_memory:with_thread_id(BaseMemory, ThreadId).
```

## 实现流程

### 1. 创建 Coordinator

```erlang
start_pipeline(Id, #{storage := BaseMemory} = Opts) ->
    %% 1. 为 Coordinator 创建专属 thread_id
    CoordinatorThreadId = <<"coordinator/", Id/binary>>,
    CoordinatorMemory = beamai_memory:with_thread_id(BaseMemory, CoordinatorThreadId),

    %% 2. 为每个 Agent 派生独立的 Memory
    Agents = maps:get(agents, Opts, []),
    AgentMemories = lists:map(
        fun(#{name := Name} = AgentDef) ->
            AgentThreadId = <<"coordinator/", Id/binary, "/agent/", Name/binary>>,
            AgentMemory = beamai_memory:with_thread_id(BaseMemory, AgentThreadId),
            {Name, AgentDef, AgentMemory}
        end,
        Agents
    ),

    %% 3. 创建 Worker Agents
    Workers = lists:foldl(
        fun({Name, AgentDef, AgentMemory}, Acc) ->
            Config = build_agent_config(AgentDef, Opts#{storage => AgentMemory}),
            {ok, State} = beamai_agent:new(Config),
            Acc#{Name => State}
        end,
        #{},
        AgentMemories
    ),

    %% 4. 保存 Coordinator 元数据
    Metadata = #{
        id => Id,
        type => pipeline,
        agents => Agents,
        llm_config => maps:get(llm, Opts),
        agent_thread_ids => [{Name, <<"coordinator/", Id/binary, "/agent/", Name/binary>>}
                             || #{name := Name} <- Agents]
    },
    ok = beamai_memory:save_checkpoint(CoordinatorMemory, Metadata),

    %% 5. 构建 Coordinator record
    Coordinator = #coordinator{
        id = Id,
        type = pipeline,
        agents = Agents,
        llm_config = maps:get(llm, Opts),
        workers = Workers,
        base_memory = BaseMemory,
        thread_id = CoordinatorThreadId
    },

    {ok, Coordinator}.
```

### 2. 恢复 Coordinator

```erlang
-spec restore_from_memory(map()) -> {ok, coordinator()} | {error, term()}.
restore_from_memory(#{storage := BaseMemory, coordinator_id := Id} = Opts) ->
    %% 1. 加载 Coordinator 元数据
    CoordinatorThreadId = <<"coordinator/", Id/binary>>,
    CoordinatorMemory = beamai_memory:with_thread_id(BaseMemory, CoordinatorThreadId),

    case beamai_memory:load_checkpoint(CoordinatorMemory) of
        {ok, Metadata} ->
            #{
                type := Type,
                agents := Agents,
                llm_config := LLMConfig,
                agent_thread_ids := AgentThreadIds
            } = Metadata,

            %% 2. 恢复每个 Agent
            Workers = lists:foldl(
                fun({Name, AgentThreadId}, Acc) ->
                    AgentMemory = beamai_memory:with_thread_id(BaseMemory, AgentThreadId),
                    AgentDef = lists:keyfind(Name, #agent_def.name, Agents),
                    Config = build_agent_config(AgentDef, #{
                        llm => LLMConfig,
                        storage => AgentMemory
                    }),
                    {ok, State} = beamai_agent:restore_from_memory(Config, AgentMemory),
                    Acc#{Name => State}
                end,
                #{},
                AgentThreadIds
            ),

            %% 3. 构建 Coordinator
            Coordinator = #coordinator{
                id = Id,
                type = Type,
                agents = Agents,
                llm_config = LLMConfig,
                workers = Workers,
                base_memory = BaseMemory,
                thread_id = CoordinatorThreadId
            },

            {ok, Coordinator};

        {error, not_found} ->
            {error, coordinator_not_found};

        {error, Reason} ->
            {error, Reason}
    end.
```

## 使用示例

### 创建带持久化的 Coordinator

```erlang
%% 1. 创建共享 Store
{ok, _} = beamai_store_ets:start_link(my_store, #{}),

%% 2. 创建基础 Memory
{ok, BaseMemory} = beamai_memory:new(#{
    context_store => {beamai_store_ets, my_store}
}),

%% 3. 创建 LLM 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% 4. 定义 Agents
Agents = [
    #{name => <<"translator">>, system_prompt => <<"You are a translator.">>},
    #{name => <<"polisher">>, system_prompt => <<"You are a polisher.">>}
],

%% 5. 创建 Coordinator（自动为每个 Agent 派生 Memory）
{ok, Coordinator} = beamai_coordinator:start_pipeline(<<"my_pipeline">>, #{
    agents => Agents,
    llm => LLM,
    storage => BaseMemory  %% 传入基础 Memory
}).
```

### 恢复 Coordinator

```erlang
%% 使用相同的 Store 和 Coordinator ID 恢复
{ok, RestoredCoordinator} = beamai_coordinator:restore_from_memory(#{
    storage => BaseMemory,
    coordinator_id => <<"my_pipeline">>
}).
```

## Checkpoint 存储结构

```
Store (ETS/Redis/PostgreSQL)
├── coordinator/my_pipeline                    # Coordinator 元数据
│   └── checkpoint: {id, type, agents, llm_config, agent_thread_ids}
│
├── coordinator/my_pipeline/agent/translator   # Translator Agent checkpoint
│   └── checkpoint: {messages, scratchpad, context, ...}
│
└── coordinator/my_pipeline/agent/polisher     # Polisher Agent checkpoint
    └── checkpoint: {messages, scratchpad, context, ...}
```

## 优势

1. **独立恢复**: 每个 Agent 可以独立恢复，不影响其他 Agent
2. **共享存储**: 所有数据在同一个 Store 中，便于管理和备份
3. **层级结构**: thread_id 的层级结构清晰表达了 Coordinator 和 Agent 的关系
4. **灵活性**: 可以只恢复部分 Agent，或者替换某个 Agent 的配置

## 注意事项

1. **thread_id 命名规范**: 使用 `coordinator/{id}/agent/{name}` 格式确保唯一性
2. **版本兼容**: 元数据中应包含版本信息，便于未来升级
3. **错误处理**: 恢复过程中某个 Agent 失败不应影响其他 Agent
4. **清理机制**: 提供清理指定 Coordinator 所有数据的方法

## 相关模块

- `beamai_memory` - 添加 `with_thread_id/2` 函数
- `beamai_coordinator` - 添加 `restore_from_memory/1` 函数
- `beamai_agent` - 现有的 `restore_from_memory/2` 函数
