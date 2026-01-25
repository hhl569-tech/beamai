# Process Framework - Event-Driven Input System

BeamAI's Process Framework supports input events similar to C# Semantic Kernel's Process Framework.

## Core Concepts

### Event Structure

```erlang
-type event() :: #{
    '__process_event__' := true,
    id := binary(),
    name := atom(),           %% Event name for routing
    type := public | internal | error | system,
    source := atom(),         %% Source step ID
    data := term(),           %% Event payload
    timestamp := integer()
}.
```

### Event Binding

Bindings route events to step inputs:

```erlang
-type event_binding() :: #{
    '__event_binding__' := true,
    event_name := atom(),      %% Event to match
    target_step := atom(),     %% Target step ID
    target_input := atom(),    %% Input slot name
    transform := fun((term()) -> term()) | undefined
}.
```

## API

### Creating Events

```erlang
%% Public event
beamai_process_event:new(Name, Data) -> event().
beamai_process_event:new(Name, Data, #{type => Type, source => Source}) -> event().

%% Error event
beamai_process_event:error_event(Source, Reason) -> event().

%% System event
beamai_process_event:system_event(Name, Data) -> event().
```

### Creating Bindings

```erlang
%% Basic binding
beamai_process_event:binding(EventName, TargetStep, TargetInput) -> event_binding().

%% Binding with transform function
beamai_process_event:binding(EventName, TargetStep, TargetInput, TransformFun) -> event_binding().
```

### Building Process with Events

```erlang
Builder = beamai_process_builder:new(my_process),

%% Add step with required inputs
Builder1 = beamai_process_builder:add_step(Builder, step_a, my_step_module, #{
    required_inputs => [input1, input2]  %% Step activates when both inputs received
}),

%% Add event bindings
Builder2 = beamai_process_builder:add_binding(Builder1,
    beamai_process_event:binding(event_x, step_a, input1)),
Builder3 = beamai_process_builder:add_binding(Builder2,
    beamai_process_event:binding(event_y, step_a, input2)),

%% Add initial event (auto-triggered on start)
Builder4 = beamai_process_builder:add_initial_event(Builder3,
    beamai_process_event:new(start, InitialData)),

{ok, Process} = beamai_process_builder:compile(Builder4).
```

### Sending Events at Runtime

```erlang
%% Send event to running process
beamai_process_runtime:send_event(Pid, beamai_process_event:new(event_name, Data)).
```

## Step Activation

Steps activate only when **all required_inputs** have received event data:

```erlang
%% Step module callbacks
-callback init(Config :: map()) -> {ok, State :: term()} | {error, term()}.
-callback can_activate(Inputs :: map(), State :: term()) -> boolean().
-callback on_activate(Inputs :: map(), State :: term(), Context :: beamai_context:t()) ->
    {ok, #{events => [event()], state => term()}} |
    {pause, Reason :: term(), State :: term()} |
    {error, term()}.
```

Activation check flow:
1. Check all `required_inputs` present in `collected_inputs`
2. Call `Module:can_activate(Inputs, State)` for custom logic
3. If both pass, execute `Module:on_activate/3`

## Event Routing Flow

```
Event enqueued
    │
    ▼
route(Event, Bindings)
    │
    ├─► Match event_name with bindings
    │
    ▼
deliver_inputs() ─► collect_input(StepId, InputName, Value)
    │
    ▼
check_activation() for all steps
    │
    ├─► Required inputs satisfied?
    │   └─► can_activate/2 returns true?
    │
    ▼
execute on_activate/3
    │
    ▼
Step produces new events ─► Enqueue ─► Loop
```

## Comparison with C# SK Process

| Feature | C# SK Process | BeamAI Process |
|---------|---------------|----------------|
| Input Event | `KernelProcessStepInput` | `event_binding()` |
| Multiple Inputs | Supported | `required_inputs` list |
| Event Transform | Supported | `transform` function |
| Dynamic Send | `SendEventAsync` | `send_event/2` |
| Initial Event | `WithInitialEvent` | `add_initial_event/2` |
| Error Event | `OnError` | `error_event/2` |
| Execution Mode | - | `concurrent` / `sequential` |

## Example: Multi-Input Step

```erlang
%% Step that requires two inputs to activate
-module(my_combiner_step).
-export([init/1, can_activate/2, on_activate/3]).

init(_Config) -> {ok, #{}}.

can_activate(Inputs, _State) ->
    %% Additional custom logic if needed
    maps:is_key(data_a, Inputs) andalso maps:is_key(data_b, Inputs).

on_activate(#{data_a := A, data_b := B}, State, _Context) ->
    Combined = combine(A, B),
    {ok, #{
        events => [beamai_process_event:new(combined_result, Combined)],
        state => State
    }}.
```

```erlang
%% Build process
Builder = beamai_process_builder:new(combiner_process),
Builder1 = beamai_process_builder:add_step(Builder, combiner, my_combiner_step, #{
    required_inputs => [data_a, data_b]
}),
Builder2 = beamai_process_builder:add_binding(Builder1,
    beamai_process_event:binding(source_a_event, combiner, data_a)),
Builder3 = beamai_process_builder:add_binding(Builder2,
    beamai_process_event:binding(source_b_event, combiner, data_b)),
{ok, Process} = beamai_process_builder:compile(Builder3).
```

## Related Modules

- `beamai_process_event` - Event and binding types
- `beamai_process_step` - Step activation and execution
- `beamai_process_builder` - Process construction
- `beamai_process_runtime` - Event-driven execution (gen_statem)
