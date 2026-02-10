# API Reference

English | [中文](API_REFERENCE_EN.md)

This document provides the main API reference for the core modules of the BeamAI Framework.

## Table of Contents

- [beamai_core - Core Module](#beamai_core---core-module)
  - [HTTP Client](#http-client)
  - [HTTP Backend Configuration](#http-backend-configuration)
- [beamai_llm - LLM Client](#beamai_llm---llm-client)
- [beamai_memory - Memory Management](#beamai_memory---memory-management)

---

## beamai_core - Core Module

### Graph Execution Engine

The graph engine is organized in three layers: **Builder** (construction), **Pregel** (algorithm primitives), **Runtime** (execution lifecycle).

```erlang
%% DSL Build (declarative)
-spec build([term()]) -> {ok, graph()} | {error, term()}.
{ok, Graph} = beamai_graph:build([
    {node, NodeName, fun(State, Ctx) -> {ok, NewState} end},
    {edge, From, To},
    {conditional_edge, From, fun(State, Ctx) -> TargetNode end},
    {entry, EntryNode}
]).

%% Builder API (imperative)
Builder = beamai_graph:builder(),
Builder1 = beamai_graph:add_node(Builder, NodeName, NodeFun),
Builder2 = beamai_graph:add_edge(Builder1, From, To),
Builder3 = beamai_graph:set_entry(Builder2, EntryNode),
{ok, Graph} = beamai_graph:compile(Builder3).

%% Execute Graph (async)
-spec run(graph(), state()) -> {ok, pid()}.
{ok, Pid} = beamai_graph:run(Graph, InitialState).

%% Execute Graph (sync, supports interrupt/resume)
-spec run_sync(graph(), state()) -> {ok, state()} | {interrupted, [vertex()], snapshot()}.
{ok, FinalState} = beamai_graph:run_sync(Graph, InitialState).

%% Resume from interrupt
{ok, FinalState} = beamai_graph:run_sync(Graph, State, #{
    snapshot => Snapshot,
    resume_data => #{review => approved}
}).
```

### Graph State

Graph state is a plain Erlang map. Node functions receive `(State, Context)`:

```erlang
%% Node function signature (2-arity)
fun(State :: map(), Context :: map()) -> {ok, NewState :: map()}.

%% Node function with interrupt support (3-arity)
fun(State :: map(), Input :: map(), ResumeData :: undefined | map()) ->
    {ok, NewState :: map()} | {interrupt, InterruptData :: map()}.

%% Read/Write state (plain map operations)
Value = maps:get(Key, State, Default).
NewState = State#{Key => Value}.
```

### Process Branch & Time Travel API

The Process framework provides branching and time-travel capabilities through a pluggable store backend.
All operations use the `{Module, Ref}` dynamic dispatch pattern via `beamai_process_store_behaviour` optional callbacks.

```erlang
-type store() :: {module(), term()}.

%% Branch API
-spec branch_from(store(), BranchName :: binary(), Opts :: map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
-spec restore_branch(store(), BranchThreadId :: binary(),
                     ProcessSpec, Opts :: map()) -> {ok, pid()} | {error, term()}.
-spec list_branches(store(), Opts :: map()) -> {ok, [map()]} | {error, term()}.
-spec get_lineage(store(), Opts :: map()) -> {ok, [map()]} | {error, term()}.

%% Time Travel API
-spec go_back(store(), Steps :: pos_integer(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec go_back(store(), Steps :: pos_integer(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec go_forward(store(), Steps :: pos_integer(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec go_forward(store(), Steps :: pos_integer(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec goto_snapshot(store(), SnapshotId :: binary(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec goto_snapshot(store(), SnapshotId :: binary(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec list_history(store()) -> {ok, [map()]} | {error, term()}.
```

**Usage Example:**

```erlang
%% Create store reference
Store = {beamai_process_memory_store, {Mgr, #{thread_id => ThreadId}}},

%% Create a branch
{ok, #{branch_thread_id := BranchId}} =
    beamai_process:branch_from(Store, <<"experiment">>, #{}),

%% Time travel: go back 2 steps
{ok, Pid} = beamai_process:go_back(Store, 2, ProcessSpec),

%% List execution history
{ok, History} = beamai_process:list_history(Store).
```

### HTTP Client

BeamAI provides a unified HTTP client interface, supporting both Gun and Hackney backends.

```erlang
%% Send request (automatically uses configured backend)
-spec request(method(), url(), headers(), body(), opts()) -> {ok, response()} | {error, term()}.
beamai_http:request(Method, Url, Headers, Body, Opts).

%% Convenience functions
-spec get(url(), headers()) -> {ok, response()} | {error, term()}.
-spec post(url(), headers(), body()) -> {ok, response()} | {error, term()}.
beamai_http:get(Url, Headers).
beamai_http:post(Url, Headers, Body).

%% Streaming request (SSE)
-spec stream_request(url(), headers(), body(), callback(), opts()) -> {ok, term()} | {error, term()}.
beamai_http:stream_request(Url, Headers, Body, Callback, Opts).
```

### HTTP Backend Configuration

```erlang
%% Application configuration (sys.config)
{beamai_core, [
    %% Select HTTP backend: beamai_http_gun (default) or beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun connection pool configuration
    {http_pool, #{
        max_connections => 100,        %% Maximum connections
        connection_timeout => 30000,   %% Connection timeout (milliseconds)
        idle_timeout => 60000          %% Idle timeout (milliseconds)
    }}
]}.
```

**Backend Comparison:**

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection pool | beamai_http_pool | hackney built-in pool |
| TLS | Automatically uses system CA certificates (OTP 25+) | hackney default config |
| Dependency | gun 2.1.0 | hackney |
| Recommended scenario | Production, needs HTTP/2 | Legacy system compatibility |

### HTTP Connection Pool (Gun Backend)

When using the Gun backend, beamai_http_pool is automatically started as a child process of the beamai_core application.

```erlang
%% Connection pool API
-spec checkout(host(), port(), protocol()) -> {ok, connection()} | {error, term()}.
beamai_http_pool:checkout(Host, Port, Protocol).

-spec checkin(connection()) -> ok.
beamai_http_pool:checkin(Conn).

%% View connection pool status
-spec get_stats() -> map().
beamai_http_pool:get_stats().
```

---

## beamai_llm - LLM Client

LLM client with multi-provider support.

### LLM Configuration Management

LLM configuration must be created using `beamai_chat_completion:create/2`, achieving separation of configuration from Agent:

```erlang
%% Create LLM configuration
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}).

%% Configuration reuse: multiple components share the same LLM configuration
%% LLM config is a plain map with '__llm_config__' => true marker
```

**Advantages:**
- Configuration reuse: multiple components share the same LLM configuration
- Centralized management: API Key, model parameters unified configuration
- Type safety: configuration contains `'__llm_config__' => true` marker
- Easy testing: LLM configuration can be validated independently

### Configuration and Chat

```erlang
%% Create configuration
-spec create(provider(), map()) -> config().
beamai_chat_completion:create(Provider, Opts).

%% Chat
-spec chat(config(), [message()]) -> {ok, response()} | {error, term()}.
-spec chat(config(), [message()], opts()) -> {ok, response()} | {error, term()}.
beamai_chat_completion:chat(Config, Messages).
beamai_chat_completion:chat(Config, Messages, Opts).

%% Streaming chat
-spec stream_chat(config(), [message()], callback()) -> {ok, response()} | {error, term()}.
-spec stream_chat(config(), [message()], callback(), opts()) -> {ok, response()} | {error, term()}.
beamai_chat_completion:stream_chat(Config, Messages, Callback).
beamai_chat_completion:stream_chat(Config, Messages, Callback, Opts).
```

### Supported Providers

| Provider | Module | API Mode | Features |
|----------|--------|----------|----------|
| `openai` | llm_provider_openai | OpenAI | Chat, streaming, tool calling |
| `anthropic` | llm_provider_anthropic | Anthropic | Chat, streaming, tool calling |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | Chat, streaming, tool calling |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Chat, streaming, tool calling, async |
| `bailian` | llm_provider_bailian | DashScope native | Chat, streaming, tool calling, web search |
| `ollama` | llm_provider_ollama | OpenAI compatible | Chat, streaming |

### DeepSeek Detailed Description

DeepSeek Provider uses OpenAI compatible API, supporting `deepseek-chat` and `deepseek-reasoner` models.

**Supported Models:**
- `deepseek-chat`: General conversation model (default)
- `deepseek-reasoner`: Reasoning enhanced model

**Configuration Example:**
```erlang
LLM = beamai_chat_completion:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY")),
    max_tokens => 4096,
    temperature => 1.0
}).
```

### Alibaba Cloud Bailian (DashScope) Detailed Description

Bailian Provider uses DashScope native API, automatically selecting endpoints based on model type:
- **Text generation models** (`qwen-plus`, `qwen-max`, `qwen-turbo`): Use `/api/v1/services/aigc/text-generation/generation`
- **Multimodal models** (`qwen-vl-plus`, `qwen-audio`, etc.): Use `/api/v1/services/aigc/multimodal-generation/generation`

**Unique Parameters:**
- `enable_search => true`: Enable web search functionality
- `tool_choice => <<"required">>`: Force tool calling

**Streaming Output:**
- Request header: `X-DashScope-SSE: enable`
- Parameter: `parameters.incremental_output: true`

### LLM Configuration Parameters

`beamai_chat_completion:create/2` supports the following parameters:

```erlang
LLM = beamai_chat_completion:create(Provider, #{
    model => binary(),                   %% Model name (required)
    api_key => binary(),                 %% API Key (required, except ollama)
    base_url => binary(),                %% Optional: custom URL
    timeout => integer(),                %% Optional: timeout (milliseconds)
    max_tokens => integer(),             %% Optional: maximum tokens
    temperature => float()               %% Optional: temperature parameter (0.0 - 2.0)
}).
```

**Provider Types:** `openai | anthropic | deepseek | zhipu | bailian | ollama`

---

## beamai_memory - Memory Management

Snapshot-based storage engine with branching and time travel support.

### Store Setup

```erlang
%% Create store backend
{ok, _} = beamai_store_ets:start_link(StoreName, #{}).
Store = {beamai_store_ets, StoreName}.

%% Create state store (namespaced KV abstraction)
-spec new(store()) -> state_store().
StateStore = beamai_state_store:new(Store).
```

### Process Snapshot Operations

```erlang
%% Create snapshot manager
-spec new(state_store()) -> manager().
-spec new(state_store(), opts()) -> manager().
Mgr = beamai_process_snapshot:new(StateStore).
Mgr = beamai_process_snapshot:new(StateStore, #{max_entries => 100}).

%% Save snapshot from process state
-spec save_from_state(manager(), thread_id(), state_map()) -> {ok, snapshot(), manager()}.
-spec save_from_state(manager(), thread_id(), state_map(), opts()) -> {ok, snapshot(), manager()}.
{ok, Snapshot, Mgr1} = beamai_process_snapshot:save_from_state(Mgr, ThreadId, StateMap).

%% Load snapshot
-spec load(manager(), snapshot_id()) -> {ok, snapshot()} | {error, not_found}.
{ok, Loaded} = beamai_process_snapshot:load(Mgr, SnapshotId).

%% Time travel
-spec go_back(manager(), thread_id(), steps()) -> {ok, snapshot(), manager()}.
-spec go_forward(manager(), thread_id(), steps()) -> {ok, snapshot(), manager()}.
{ok, OlderSnapshot, Mgr2} = beamai_process_snapshot:go_back(Mgr1, ThreadId, 1).

%% Branch management
-spec fork_from(manager(), snapshot_id(), branch_name(), opts()) -> {ok, manager()}.
-spec list_branches(manager()) -> [branch()].
-spec switch_branch(manager(), branch_name()) -> {ok, manager()}.
```

### Graph Snapshot Operations

```erlang
%% Create graph snapshot manager
GraphMgr = beamai_graph_snapshot:new(StateStore).

%% Save from Pregel state
-spec save_from_pregel(manager(), run_id(), pregel_state()) -> {ok, snapshot(), manager()}.
{ok, GraphSnapshot, GraphMgr1} = beamai_graph_snapshot:save_from_pregel(GraphMgr, RunId, PregelState).

%% Load, time travel, branch — same API as process snapshot
{ok, Loaded} = beamai_graph_snapshot:load(GraphMgr, SnapshotId).
```

---

## Common Types

### Tool Definition

```erlang
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters := json_schema(),
    handler := fun((map()) -> {ok, term()} | {error, term()})
                | fun((map(), map()) -> {ok, term()} | {error, term()})
}.
```

### Message Types

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary()
}.
```

### LLM Response

```erlang
-type llm_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls := [tool_call()],
    finish_reason := binary(),
    usage => usage_info()
}.
```

---

## Error Handling

All APIs return `{ok, Result}` or `{error, Reason}` format. Common error types:

| Error | Description |
|-------|-------------|
| `{error, missing_api_key}` | API Key not configured |
| `{error, timeout}` | Request timeout |
| `{error, {http_error, Code, Body}}` | HTTP error |
| `{error, {api_error, Details}}` | API returned error |
| `{error, not_found}` | Resource not found |
| `{error, storage_not_enabled}` | Storage not enabled |

---

## More Documentation

- [README.md](../README.md) - Project overview
- [ARCHITECTURE.md](ARCHITECTURE.md) - Architecture design
- Module READMEs:
  - [beamai_core](../apps/beamai_core/README.md) - Kernel, Process, Graph, HTTP
  - [beamai_llm](../apps/beamai_llm/README.md) - LLM providers
  - [beamai_memory](../apps/beamai_memory/README.md) - Snapshot, Store
