%%%-------------------------------------------------------------------
%%% @doc Public API facade for the Process Framework
%%%
%%% Provides a unified interface for building and running processes.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process).

-export([
    %% Builder API
    builder/1,
    add_step/3,
    add_step/4,
    on_event/4,
    on_event/5,
    set_initial_event/2,
    set_initial_event/3,
    set_execution_mode/2,
    build/1,

    %% Runtime API
    start/1,
    start/2,
    send_event/2,
    resume/2,
    stop/1,
    get_status/1,
    snapshot/1,
    restore/1,
    restore/2,

    %% Sync API
    run_sync/1,
    run_sync/2
]).

%%====================================================================
%% Builder API
%%====================================================================

%% @doc Create a new process builder
-spec builder(atom()) -> beamai_process_builder:builder().
builder(Name) ->
    beamai_process_builder:new(Name).

%% @doc Add a step with default config
-spec add_step(beamai_process_builder:builder(), atom(), module()) ->
    beamai_process_builder:builder().
add_step(Builder, StepId, Module) ->
    beamai_process_builder:add_step(Builder, StepId, Module).

%% @doc Add a step with config
-spec add_step(beamai_process_builder:builder(), atom(), module(), map()) ->
    beamai_process_builder:builder().
add_step(Builder, StepId, Module, Config) ->
    beamai_process_builder:add_step(Builder, StepId, Module, Config).

%% @doc Bind an event to a step input
-spec on_event(beamai_process_builder:builder(), atom(), atom(), atom()) ->
    beamai_process_builder:builder().
on_event(Builder, EventName, TargetStep, TargetInput) ->
    Binding = beamai_process_event:binding(EventName, TargetStep, TargetInput),
    beamai_process_builder:add_binding(Builder, Binding).

%% @doc Bind an event to a step input with transform
-spec on_event(beamai_process_builder:builder(), atom(), atom(), atom(),
               beamai_process_event:transform_fun()) ->
    beamai_process_builder:builder().
on_event(Builder, EventName, TargetStep, TargetInput, Transform) ->
    Binding = beamai_process_event:binding(EventName, TargetStep, TargetInput, Transform),
    beamai_process_builder:add_binding(Builder, Binding).

%% @doc Set an initial event (with name and data)
-spec set_initial_event(beamai_process_builder:builder(), atom()) ->
    beamai_process_builder:builder().
set_initial_event(Builder, EventName) ->
    set_initial_event(Builder, EventName, #{}).

-spec set_initial_event(beamai_process_builder:builder(), atom(), term()) ->
    beamai_process_builder:builder().
set_initial_event(Builder, EventName, Data) ->
    Event = beamai_process_event:new(EventName, Data),
    beamai_process_builder:add_initial_event(Builder, Event).

%% @doc Set execution mode (concurrent | sequential)
-spec set_execution_mode(beamai_process_builder:builder(), concurrent | sequential) ->
    beamai_process_builder:builder().
set_execution_mode(Builder, Mode) ->
    beamai_process_builder:set_execution_mode(Builder, Mode).

%% @doc Compile the builder into a process definition
-spec build(beamai_process_builder:builder()) ->
    {ok, beamai_process_builder:process_def()} | {error, [term()]}.
build(Builder) ->
    beamai_process_builder:compile(Builder).

%%====================================================================
%% Runtime API
%%====================================================================

%% @doc Start a process from a compiled definition
-spec start(beamai_process_builder:process_def()) ->
    {ok, pid()} | {error, term()}.
start(ProcessDef) ->
    start(ProcessDef, #{}).

-spec start(beamai_process_builder:process_def(), map()) ->
    {ok, pid()} | {error, term()}.
start(ProcessDef, Opts) ->
    beamai_process_sup:start_runtime(ProcessDef, Opts).

%% @doc Send an event to a running process
-spec send_event(pid(), beamai_process_event:event()) -> ok.
send_event(Pid, Event) ->
    beamai_process_runtime:send_event(Pid, Event).

%% @doc Resume a paused process with data
-spec resume(pid(), term()) -> ok | {error, term()}.
resume(Pid, Data) ->
    beamai_process_runtime:resume(Pid, Data).

%% @doc Stop a running process
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_process_runtime:stop(Pid).

%% @doc Get process status
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    beamai_process_runtime:get_status(Pid).

%% @doc Take a snapshot of the process state
-spec snapshot(pid()) -> {ok, beamai_process_state:snapshot()}.
snapshot(Pid) ->
    beamai_process_runtime:snapshot(Pid).

%% @doc Restore a process from a snapshot
-spec restore(beamai_process_state:snapshot()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot) ->
    restore(Snapshot, #{}).

-spec restore(beamai_process_state:snapshot(), map()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot, Opts) ->
    case beamai_process_state:restore_from_snapshot(Snapshot) of
        {ok, #{process_def := ProcessDef, event_queue := EventQueue,
               current_state := _CurrentState} = _Restored} ->
            %% Start fresh runtime with restored events
            RestoreOpts = Opts#{restored => true},
            ProcessDefWithQueue = ProcessDef#{
                initial_events => EventQueue
            },
            start(ProcessDefWithQueue, RestoreOpts);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Sync API
%%====================================================================

%% @doc Start a process and wait for completion
-spec run_sync(beamai_process_builder:process_def()) ->
    {ok, map()} | {error, term()}.
run_sync(ProcessDef) ->
    run_sync(ProcessDef, #{}).

-spec run_sync(beamai_process_builder:process_def(), map()) ->
    {ok, map()} | {error, term()}.
run_sync(ProcessDef, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),
    OptsWithCaller = Opts#{caller => self()},
    case start(ProcessDef, OptsWithCaller) of
        {ok, Pid} ->
            MonRef = monitor(process, Pid),
            receive
                {process_completed, Pid, StepsState} ->
                    demonitor(MonRef, [flush]),
                    {ok, StepsState};
                {process_failed, Pid, Reason} ->
                    demonitor(MonRef, [flush]),
                    {error, Reason};
                {'DOWN', MonRef, process, Pid, Reason} ->
                    {error, {process_died, Reason}}
            after Timeout ->
                demonitor(MonRef, [flush]),
                stop(Pid),
                {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.
