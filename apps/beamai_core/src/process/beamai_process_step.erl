%%%-------------------------------------------------------------------
%%% @doc Process step activation and execution logic
%%%
%%% Manages input collection, activation checks, and step execution.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_step).

-export([
    init_step/1,
    collect_input/4,
    check_activation/2,
    execute/3,
    clear_inputs/1,
    invoke_with_kernel/4,
    chat_with_kernel/3
]).

-export_type([step_runtime_state/0]).

-type step_runtime_state() :: #{
    '__step_runtime__' := true,
    step_def := beamai_process_builder:step_def(),
    state := term(),
    collected_inputs := #{atom() => term()},
    activation_count := non_neg_integer()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Initialize a step runtime state from its definition
-spec init_step(beamai_process_builder:step_def()) ->
    {ok, step_runtime_state()} | {error, term()}.
init_step(#{module := Module, config := Config} = StepDef) ->
    case Module:init(Config) of
        {ok, State} ->
            {ok, #{
                '__step_runtime__' => true,
                step_def => StepDef,
                state => State,
                collected_inputs => #{},
                activation_count => 0
            }};
        {error, Reason} ->
            {error, {step_init_failed, maps:get(id, StepDef), Reason}}
    end.

%% @doc Collect an input value for a step
-spec collect_input(atom(), atom(), term(), #{atom() => step_runtime_state()}) ->
    #{atom() => step_runtime_state()}.
collect_input(StepId, InputName, Value, StepsState) ->
    case maps:find(StepId, StepsState) of
        {ok, #{collected_inputs := Inputs} = StepState} ->
            StepsState#{StepId => StepState#{
                collected_inputs => Inputs#{InputName => Value}
            }};
        error ->
            StepsState
    end.

%% @doc Check if a step can be activated (all required inputs present)
-spec check_activation(step_runtime_state(), beamai_process_builder:step_def()) -> boolean().
check_activation(#{collected_inputs := Inputs, state := State,
                   step_def := #{module := Module, required_inputs := Required}}, _StepDef) ->
    AllPresent = lists:all(
        fun(InputName) -> maps:is_key(InputName, Inputs) end,
        Required
    ),
    case AllPresent of
        true -> Module:can_activate(Inputs, State);
        false -> false
    end.

%% @doc Execute a step with collected inputs
-spec execute(step_runtime_state(), #{atom() => term()}, beamai_context:t()) ->
    {events, [beamai_process_event:event()], step_runtime_state()} |
    {pause, term(), step_runtime_state()} |
    {error, term()}.
execute(#{step_def := #{module := Module, id := StepId},
          state := State, activation_count := Count} = StepRuntimeState,
        Inputs, Context) ->
    try Module:on_activate(Inputs, State, Context) of
        {ok, #{events := Events, state := NewState}} ->
            TaggedEvents = tag_events(Events, StepId),
            {events, TaggedEvents, StepRuntimeState#{
                state => NewState,
                activation_count => Count + 1
            }};
        {ok, #{state := NewState}} ->
            {events, [], StepRuntimeState#{
                state => NewState,
                activation_count => Count + 1
            }};
        {ok, #{events := Events}} ->
            TaggedEvents = tag_events(Events, StepId),
            {events, TaggedEvents, StepRuntimeState#{
                activation_count => Count + 1
            }};
        {error, Reason} ->
            {error, {step_execution_failed, StepId, Reason}};
        {pause, Reason, NewState} ->
            {pause, Reason, StepRuntimeState#{
                state => NewState,
                activation_count => Count + 1
            }}
    catch
        Class:Error:Stacktrace ->
            {error, {step_exception, StepId, {Class, Error, Stacktrace}}}
    end.

%% @doc Clear collected inputs after activation (enables cycles)
-spec clear_inputs(step_runtime_state()) -> step_runtime_state().
clear_inputs(StepRuntimeState) ->
    StepRuntimeState#{collected_inputs => #{}}.

%% @doc Helper: invoke a kernel tool from within a step
-spec invoke_with_kernel(beamai_context:t(), binary(), map(), map()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
invoke_with_kernel(Context, ToolName, Args, _Opts) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context)
    end.

%% @doc Helper: chat with LLM via kernel from within a step
-spec chat_with_kernel(beamai_context:t(), [beamai_context:message()], map()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
chat_with_kernel(Context, Messages, Opts) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            beamai_kernel:invoke(Kernel, Messages, Opts)
    end.

%%====================================================================
%% Internal
%%====================================================================

tag_events(Events, StepId) ->
    lists:map(
        fun(#{source := undefined} = E) -> E#{source => StepId};
           (E) -> E
        end,
        Events
    ).
