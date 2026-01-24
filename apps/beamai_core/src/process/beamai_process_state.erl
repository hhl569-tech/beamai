%%%-------------------------------------------------------------------
%%% @doc Process state snapshot and restore
%%%
%%% Supports persistence by serializing/deserializing runtime state.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_state).

-export([
    take_snapshot/1,
    restore_from_snapshot/1
]).

-export_type([snapshot/0]).

-type snapshot() :: #{
    '__process_snapshot__' := true,
    process_def := beamai_process_builder:process_def(),
    current_state := atom(),
    steps_state := #{atom() => step_snapshot()},
    event_queue := [beamai_process_event:event()],
    paused_step := atom() | undefined,
    pause_reason := term() | undefined,
    timestamp := integer()
}.

-type step_snapshot() :: #{
    state := term(),
    collected_inputs := #{atom() => term()},
    activation_count := non_neg_integer()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Take a snapshot of runtime state
-spec take_snapshot(map()) -> snapshot().
take_snapshot(#{process_def := ProcessDef, current_state := CurrentState,
               steps_state := StepsState, event_queue := EventQueue,
               paused_step := PausedStep, pause_reason := PauseReason}) ->
    #{
        '__process_snapshot__' => true,
        process_def => ProcessDef,
        current_state => CurrentState,
        steps_state => snapshot_steps(StepsState),
        event_queue => EventQueue,
        paused_step => PausedStep,
        pause_reason => PauseReason,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Restore runtime state from a snapshot
-spec restore_from_snapshot(snapshot()) ->
    {ok, map()} | {error, term()}.
restore_from_snapshot(#{process_def := ProcessDef, current_state := CurrentState,
                        steps_state := StepsSnapshots, event_queue := EventQueue,
                        paused_step := PausedStep, pause_reason := PauseReason}) ->
    case restore_steps(StepsSnapshots, maps:get(steps, ProcessDef)) of
        {ok, StepsState} ->
            {ok, #{
                process_def => ProcessDef,
                current_state => CurrentState,
                steps_state => StepsState,
                event_queue => EventQueue,
                paused_step => PausedStep,
                pause_reason => PauseReason
            }};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Internal
%%====================================================================

snapshot_steps(StepsState) ->
    maps:map(
        fun(_StepId, #{state := State, collected_inputs := Inputs,
                       activation_count := Count}) ->
            #{
                state => State,
                collected_inputs => Inputs,
                activation_count => Count
            }
        end,
        StepsState
    ).

restore_steps(StepsSnapshots, StepDefs) ->
    try
        Restored = maps:map(
            fun(StepId, #{state := State, collected_inputs := Inputs,
                         activation_count := Count}) ->
                StepDef = maps:get(StepId, StepDefs),
                #{
                    '__step_runtime__' => true,
                    step_def => StepDef,
                    state => State,
                    collected_inputs => Inputs,
                    activation_count => Count
                }
            end,
            StepsSnapshots
        ),
        {ok, Restored}
    catch
        _:Reason ->
            {error, {restore_failed, Reason}}
    end.
