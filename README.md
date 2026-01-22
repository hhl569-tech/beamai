# Erlang Agent Framework

[English](README_EN.md) | 中文

基于 Erlang/OTP 的高性能 AI Agent 应用框架。

## 特性

- **🤖 Simple Agent**: 基于 Graph 引擎的 ReAct Agent
  - 支持自定义工具和系统提示词
  - 内置 Scratchpad 执行历史
  - 支持 Checkpoint 持久化
  - 完整的回调系统

- **🔄 协调器模式**: 统一的多 Agent 协调
  - **Pipeline 模式**: 顺序协调（研究员 → 写作者 → 审核员）
  - **Orchestrator 模式**: 编排协调（委托、路由、并行调用多个 workers）

- **🧠 Deep Agent**: 递归规划 Agent
  - 支持任务规划（Planning）
  - 支持自我反思（Reflection）
  - 支持子任务分发

- **📦 Output Parser**: 结构化输出
  - JSON Schema 解析
  - 自动重试机制

## 快速开始

### 1. 启动 Shell

```bash
export ZHIPU_API_KEY=your_key_here
rebar3 shell
```

### 2. Simple Agent（基本用法）

```erlang
%% 创建 LLM 配置（必须使用 llm_client:create/2）
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

%% 创建 Agent 状态（纯函数 API）
{ok, State} = beamai_agent:new(#{
    system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
    llm => LLM
}),

%% 运行 Agent
{ok, Result, _NewState} = beamai_agent:run(State, <<"你好！"/utf8>>),

%% 查看结果
Response = maps:get(final_response, Result).
```

### 3. Simple Agent（多轮对话）

```erlang
%% 多轮对话通过状态传递实现
{ok, State0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是一个记忆助手。"/utf8>>
}),
{ok, _, State1} = beamai_agent:run(State0, <<"我叫张三"/utf8>>),
{ok, Result, _State2} = beamai_agent:run(State1, <<"我叫什么名字？"/utf8>>).
%% Result 中 Agent 会记得用户叫张三
```

### 4. Simple Agent（带工具）

```erlang
%% 定义工具
SearchTool = #{
    name => <<"search">>,
    description => <<"搜索信息"/utf8>>,
    parameters => #{
        type => object,
        properties => #{
            <<"query">> => #{type => string, description => <<"搜索关键词"/utf8>>}
        },
        required => [<<"query">>]
    },
    handler => fun(#{<<"query">> := Query}) ->
        {ok, <<"搜索结果: ", Query/binary>>}
    end
},

%% 使用 Registry 构建工具列表
Tools = beamai_tool_registry:from_config(#{
    tools => [SearchTool],
    providers => [beamai_tool_provider_builtin]
}),

%% 创建带工具的 Agent
{ok, State} = beamai_agent:new(#{
    system_prompt => <<"你是搜索助手。"/utf8>>,
    tools => Tools,
    llm => LLM
}),

{ok, Result, _} = beamai_agent:run(State, <<"搜索 Erlang 教程"/utf8>>).
```

### 5. Simple Agent（带 Memory 持久化）

```erlang
%% 创建存储后端
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 创建带 Memory 的 Agent（checkpoint 自动保存）
{ok, State0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是持久化助手。"/utf8>>,
    storage => Memory
}),

%% 对话（checkpoint 自动保存）
{ok, _, State1} = beamai_agent:run(State0, <<"记住：密码是 12345"/utf8>>),
{ok, _, _State2} = beamai_agent:run(State1, <<"好的"/utf8>>),

%% 稍后恢复会话
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory),
{ok, Result, _} = beamai_agent:run(RestoredState, <<"密码是多少？"/utf8>>).
%% Agent 会记得密码是 12345
```

### 6. Pipeline 协调器（顺序协调）

```erlang
%% 创建研究团队（研究员 → 写作者 → 审核员）
{ok, Coord} = beamai_coordinator:new_pipeline(#{
    agents => [
        #{name => <<"researcher">>, system_prompt => <<"你是研究员，负责收集资料。"/utf8>>},
        #{name => <<"writer">>, system_prompt => <<"你是写作者，负责撰写文章。"/utf8>>},
        #{name => <<"reviewer">>, system_prompt => <<"你是审核员，负责质量检查。"/utf8>>}
    ],
    llm => LLM
}),

%% 运行任务（协调器自动在 workers 间传递）
{ok, Result, _NewCoord} = beamai_coordinator:run(Coord,
    <<"研究并撰写一篇关于 Erlang 并发模型的 100 字介绍。"/utf8>>).
```

### 7. Orchestrator 协调器（编排协调）

```erlang
%% 创建专家团队
{ok, Coord} = beamai_coordinator:new_orchestrator(#{
    agents => [
        #{name => <<"tech_expert">>, system_prompt => <<"你是技术专家。"/utf8>>},
        #{name => <<"business_expert">>, system_prompt => <<"你是商业专家。"/utf8>>}
    ],
    llm => LLM
}),

%% 方式一：运行任务（协调器智能分配）
{ok, Result, _NewCoord} = beamai_coordinator:run(Coord,
    <<"从技术和商业角度分析 AI 的影响。"/utf8>>),

%% 方式二：并行委托给多个 workers
{ok, Results} = beamai_coordinator:delegate_parallel(Coord,
    [<<"tech_expert">>, <<"business_expert">>],
    <<"分析 AI 的影响"/utf8>>).
%% Results = #{<<"tech_expert">> => {ok, "..."}, <<"business_expert">> => {ok, "..."}}
```

### 8. Deep Agent（规划 + 反思）

```erlang
%% 创建 Deep Agent 配置
{ok, Config} = beamai_deepagent:new(#{
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    system_prompt => <<"你是一个研究专家。"/utf8>>,
    llm => LLM
}),

%% 运行复杂任务
{ok, Result} = beamai_deepagent:run(Config,
    <<"分析这个代码库的架构并给出优化建议。"/utf8>>),

%% 查看执行计划和轨迹
Plan = beamai_deepagent:get_plan(Result),
Trace = beamai_deepagent:get_trace(Result).
```

## 架构

### 应用结构

```
apps/
├── beamai_core/      # 核心功能 + Persistence
│   ├── Behaviours   # beamai_behaviour, agent_persistence_behaviour
│   ├── HTTP         # beamai_http (Gun/Hackney 客户端, 默认 Gun)
│   ├── Graph        # Graph 执行引擎
│   ├── Pregel       # Pregel 分布式计算
│   └── Persistence      # agent_storage_ets, agent_storage_sup
│
├── beamai_llm/       # LLM 客户端
│   └── Providers    # OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama
│
├── beamai_rag/       # RAG 功能
│   ├── Embeddings   # 向量嵌入
│   └── Vector Store # 向量存储
│
├── beamai_memory/    # 内存和上下文存储
│   ├── Context      # 上下文管理
│   └── Store        # ETS/SQLite 存储后端
│
├── beamai_a2a/       # A2A 协议实现
│   ├── Server       # A2A 服务端
│   └── Client       # A2A 客户端
│
├── beamai_mcp/       # MCP 协议实现
│   ├── Server       # MCP 服务端
│   └── Client       # MCP 客户端
│
├── beamai_tools/    # 公共工具库 + 中间件系统
│   ├── Tools        # 工具注册和执行
│   ├── Providers    # 工具来源 (内置、MCP)
│   └── Middleware   # 执行中间件（拦截、增强）
│
├── beamai_agent/    # Simple Agent + 协调器
│   ├── Graph Engine # 基于 Graph 的执行
│   ├── Scratchpad   # 执行历史
│   ├── Checkpoint   # 状态持久化
│   ├── Callbacks    # 回调系统
│   └── Coordinator  # Multi/Supervisor 协调器
│
└── beamai_deepagent/      # Deep Agent
    ├── Planning     # 任务规划
    ├── Reflection   # 自我反思
    └── Router      # 智能路由
```

### 依赖关系

```
┌─────────────────────────────────┐
│   Agent 实现                     │
│  (beamai_agent, beamai_deepagent)     │
└────────────┬────────────────────┘
             │
┌────────────┴────────────────────┐
│   工具与服务层                    │
│  (beamai_tools, beamai_llm,       │
│   beamai_rag, beamai_a2a, beamai_mcp) │
└────────────┬────────────────────┘
             │
┌────────────┴────────────────────┐
│   核心层                         │
│  (beamai_core, beamai_memory)     │
└─────────────────────────────────┘
```

详见 [DEPENDENCIES.md](doc/DEPENDENCIES.md)

## 核心概念

### 1. Graph 执行引擎

beamai_agent 使用 Graph 引擎执行 Agent：

```erlang
%% Graph 定义
Graph = #{
    nodes => #{
        llm => {beamai_llm_node, #{}},
        tools => {beamai_tools_node, #{}}
    },
    edges => [
        {llm, tools, {condition, fun should_use_tools/1}}
    ]
}

%% 执行 Graph
{ok, Result} = graph_runner:run(Graph, Input).
```

### 2. Scratchpad（执行历史）

Scratchpad 记录每一步的执行过程：

```erlang
%% 获取 Scratchpad（从状态中获取）
Steps = beamai_agent:get_scratchpad(State).

%% 每一步包含：
%% - step_id: 步骤 ID
%% - type: 步骤类型 (llm_call, tool_use, tool_result)
%% - content: 内容
%% - timestamp: 时间戳
```

### 3. Memory 持久化

使用 beamai_memory 实现会话持久化：

```erlang
%% 创建 Memory
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 创建带 storage 的 Agent（checkpoint 自动保存）
{ok, State} = beamai_agent:new(#{
    llm => LLM,
    storage => Memory
}),

%% 对话后 checkpoint 自动保存
{ok, _, NewState} = beamai_agent:run(State, <<"你好"/utf8>>),

%% 从 Memory 恢复会话
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory).
```

### 4. Callbacks（回调系统）

监听 Agent 执行事件，支持 18 种回调类型：

```erlang
%% 在创建 Agent 时配置回调
{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是助手"/utf8>>,
    callbacks => #{
        %% LLM 回调
        on_llm_start => fun(Prompts, Meta) ->
            io:format("LLM 调用开始，消息数: ~p~n", [length(Prompts)])
        end,
        on_llm_end => fun(Response, Meta) ->
            io:format("LLM 响应收到~n")
        end,
        %% 工具回调
        on_tool_start => fun(ToolName, Args, Meta) ->
            io:format("执行工具: ~ts~n", [ToolName])
        end,
        on_tool_end => fun(ToolName, Result, Meta) ->
            io:format("工具完成: ~ts~n", [ToolName])
        end,
        %% Agent 回调
        on_agent_finish => fun(Result, Meta) ->
            io:format("Agent 完成~n")
        end
    }
}),

%% 运行 Agent，回调会在执行过程中自动触发
{ok, Result, _NewState} = beamai_agent:run(State, <<"你好"/utf8>>).
```

详见 [doc/CALLBACKS.md](doc/CALLBACKS.md)

## 配置

### LLM 配置

LLM 配置必须使用 `llm_client:create/2` 创建，可在多个 Agent 间复用：

```erlang
%% 创建 LLM 配置（必须使用 llm_client:create/2）
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}),

%% 配置可在多个 Agent 间复用
{ok, State1} = beamai_agent:new(#{
    llm => LLM,
    tools => Tools1,
    system_prompt => <<"你是研究助手。"/utf8>>
}),

{ok, State2} = beamai_agent:new(#{
    llm => LLM,
    tools => Tools2,
    system_prompt => <<"你是写作助手。"/utf8>>
}).

%% 基于现有配置创建新配置
HighTempLLM = llm_client:merge_config(LLM, #{temperature => 0.9}).
```

**支持的 Provider：**

| Provider | 模块 | API 模式 | 说明 |
|----------|------|----------|------|
| `anthropic` | llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | llm_provider_deepseek | OpenAI 兼容 | DeepSeek API (deepseek-chat, deepseek-reasoner) |
| `zhipu` | llm_provider_zhipu | OpenAI 兼容 | 智谱 AI (GLM 系列) |
| `bailian` | llm_provider_bailian | DashScope 原生 | 阿里云百炼 (通义千问系列) |
| `ollama` | llm_provider_ollama | OpenAI 兼容 | Ollama 本地模型 |

### Agent 配置选项

```erlang
Opts = #{
    %% 基础配置
    id => <<"agent_id">>,
    system_prompt => Prompt,
    tools => [Tool1, Tool2],

    %% LLM 配置
    llm => LLMConfig,

    %% 执行配置
    max_iterations => 10,       %% 最大迭代次数
    timeout => 300000,          %% 超时时间

    %% Checkpoint 配置
    enable_storage => true,     %% 启用存储
    auto_save => true,          %% 自动保存检查点

    %% 回调配置
    callbacks => #{
        on_llm_start => fun(...), ...
    }
}.
```

### HTTP 后端配置

BeamAI 支持 Gun 和 Hackney 两种 HTTP 后端，默认使用 Gun（支持 HTTP/2）。

```erlang
%% 在 sys.config 中配置（可选）
{beamai_core, [
    %% HTTP 后端选择：beamai_http_gun（默认）或 beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun 连接池配置（仅当使用 Gun 后端时）
    {http_pool, #{
        max_connections => 100,        %% 最大连接数
        connection_timeout => 30000    %% 连接超时（毫秒）
    }}
]}.
```

**后端对比：**

| 特性 | Gun（默认） | Hackney |
|------|-------------|---------|
| HTTP/2 | 支持 | 不支持 |
| 连接池 | 内置 beamai_http_pool | 依赖 hackney 池 |
| TLS | 自动使用系统 CA 证书 | hackney 默认配置 |
| 适用场景 | 推荐生产环境 | 兼容旧系统 |

## 高级功能

### 自定义工具

```erlang
%% 工具定义（使用 parameters 字段）
#{name => <<"my_tool">>,
  description => <<"工具描述"/utf8>>,
  parameters => #{
      type => object,
      properties => #{
          <<"param1">> => #{type => string},
          <<"param2">> => #{type => integer}
      },
      required => [<<"param1">>]
  },
  handler => fun(Args, Context) ->
      %% 工具逻辑
      {ok, Result}
  end}

%% 使用 Registry 注册多个工具
Tools = beamai_tool_registry:from_config(#{
    tools => [MyTool1, MyTool2],
    providers => [
        beamai_tool_provider_builtin,     %% 内置工具
        {beamai_tool_provider_mcp, #{}}   %% MCP 工具
    ]
}).
```

### Output Parser

```erlang
%% 定义输出 schema
Schema = #{
    type => object,
    properties => #{
        <<"title">> => #{type => string},
        <<"count">> => #{type => integer},
        <<"items">> => #{
            type => array,
            items => #{type => string}
        }
    },
    required => [<<"title">>, <<"count">>]
}.

%% 使用 Parser
{ok, Parsed} = agent_output_parser:parse(
    LLMResponse,
    Schema,
    #{max_retries => 3}
).
```

## 文档

### 核心文档

- **[doc/API_REFERENCE.md](doc/API_REFERENCE.md)** - API 参考文档
- **[doc/MIDDLEWARE.md](doc/MIDDLEWARE.md)** - Middleware 系统文档
- **[doc/CALLBACKS.md](doc/CALLBACKS.md)** - Callback 回调系统文档
- **[doc/ARCHITECTURE.md](doc/ARCHITECTURE.md)** - 架构设计
- **[DEPENDENCIES.md](doc/DEPENDENCIES.md)** - 依赖关系详解

### 模块文档

| 模块 | 说明 | 文档 |
|------|------|------|
| **beamai_core** | 核心框架：Graph 引擎、Pregel 分布式计算、行为定义 | [README](apps/beamai_core/README.md) |
| **beamai_llm** | LLM 客户端：支持 OpenAI、Anthropic、DeepSeek、Zhipu、Bailian、Ollama | [README](apps/beamai_llm/README.md) |
| **beamai_agent** | Simple Agent：ReAct 模式、回调系统、Checkpoint | [README](apps/beamai_agent/README.md) |
| **beamai_deepagent** | Deep Agent：任务规划、并行执行、自我反思 | [README](apps/beamai_deepagent/README.md) |
| **beamai_memory** | 记忆管理：Checkpoint、Store、时间旅行 | [README](apps/beamai_memory/README.md) |
| **beamai_tools** | 工具库 + 中间件：Provider 机制、工具注册、Middleware 系统 | [README](apps/beamai_tools/README.md) |
| **beamai_a2a** | A2A 协议：Agent 间通信、服务端/客户端 | [README](apps/beamai_a2a/README.md) |
| **beamai_mcp** | MCP 协议：Model Context Protocol 实现 | [README](apps/beamai_mcp/README.md) |
| **beamai_rag** | RAG 功能：向量嵌入、相似度搜索 | [README](apps/beamai_rag/README.md) |

### 设计与实现文档

- **[doc/DESIGN_PATTERNS.md](doc/DESIGN_PATTERNS.md)** - 设计模式
- **[doc/OUTPUT_PARSER.md](doc/OUTPUT_PARSER.md)** - Output Parser 指南
- **[REFACTORING_REPORT.md](REFACTORING_REPORT.md)** - 重构总结报告

## 运行示例

```bash
# 编译
rebar3 compile

# 启动 Shell
rebar3 shell

# 运行交互式 Deep Agent
examples/interactive_deep_agent.erl
```

## 项目统计

- **应用数量**: 8 个
- **代码行数**: ~15,000 行
- **测试覆盖**: 持续改进中
- **文档**: 完整的 API 和架构文档

## 性能

- ✅ 基于 Erlang/OTP 轻量级进程
- ✅ Graph 引擎优化执行路径
- ✅ 并发工具调用
- ✅ HTTP 连接池（Gun，支持 HTTP/2）
- ✅ ETS 高速存储

## 设计原则

- **简单**: 清晰的 API，易于理解
- **模块化**: 每个模块职责单一
- **可扩展**: Behaviour 设计，易于自定义
- **高性能**: 利用 Erlang 并发特性
- **可观测**: 完善的日志、追踪、监控

## 许可证

Apache-2.0

## 贡献

欢迎提交 Issue 和 Pull Request！

---

**开始构建你的 AI Agent 应用！** 🚀
