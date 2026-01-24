%%%-------------------------------------------------------------------
%%% @doc Process runtime - gen_statem execution engine
%%%
%%% States: idle -> running -> paused/completed/failed
%%% Drives event-based step activation with concurrent or sequential
%%% execution via poolboy worker pool.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_runtime).

-behaviour(gen_statem).

%% API
-export([
    start_link/2,
    send_event/2,
    resume/2,
    stop/1,
    get_status/1,
    snapshot/1
]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([idle/3, running/3, paused/3, completed/3, failed/3]).

-define(POOL_NAME, beamai_process_pool).

-record(data, {
    process_def :: beamai_process_builder:process_def(),
    steps_state :: #{atom() => beamai_process_step:step_runtime_state()},
    event_queue :: queue:queue(beamai_process_event:event()),
    context :: beamai_context:t(),
    paused_step :: atom() | undefined,
    pause_reason :: term() | undefined,
    pending_steps :: #{atom() => reference()},
    pending_results :: [{atom(), term()}],
    expected_count :: non_neg_integer(),
    caller :: pid() | undefined,
    opts :: map()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(beamai_process_builder:process_def(), map()) ->
    {ok, pid()} | {error, term()}.
start_link(ProcessDef, Opts) ->
    gen_statem:start_link(?MODULE, {ProcessDef, Opts}, []).

-spec send_event(pid(), beamai_process_event:event()) -> ok.
send_event(Pid, Event) ->
    gen_statem:cast(Pid, {send_event, Event}).

-spec resume(pid(), term()) -> ok | {error, not_paused}.
resume(Pid, Data) ->
    gen_statem:call(Pid, {resume, Data}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    gen_statem:call(Pid, get_status).

-spec snapshot(pid()) -> {ok, beamai_process_state:snapshot()}.
snapshot(Pid) ->
    gen_statem:call(Pid, snapshot).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() -> state_functions.

init({ProcessDef, Opts}) ->
    case init_steps(ProcessDef) of
        {ok, StepsState} ->
            Context = maps:get(context, Opts, beamai_context:new()),
            InitialEvents = maps:get(initial_events, ProcessDef, []),
            Queue = queue:from_list(InitialEvents),
            Data = #data{
                process_def = ProcessDef,
                steps_state = StepsState,
                event_queue = Queue,
                context = Context,
                paused_step = undefined,
                pause_reason = undefined,
                pending_steps = #{},
                pending_results = [],
                expected_count = 0,
                caller = maps:get(caller, Opts, undefined),
                opts = Opts
            },
            case queue:is_empty(Queue) of
                true ->
                    {ok, idle, Data};
                false ->
                    {ok, running, Data, [{state_timeout, 0, process_queue}]}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

terminate(_Reason, _State, _Data) ->
    ok.

%%====================================================================
%% State: idle
%%====================================================================

idle(cast, {send_event, Event}, #data{event_queue = Queue} = Data) ->
    NewQueue = queue:in(Event, Queue),
    {next_state, running, Data#data{event_queue = NewQueue},
     [{state_timeout, 0, process_queue}]};

idle({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(idle, Data)}}]};

idle({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(idle, Data)}}]};

idle({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, idle}}]}.

%%====================================================================
%% State: running
%%====================================================================

running(state_timeout, process_queue, Data) ->
    process_event_queue(Data);

running(cast, {send_event, Event}, #data{event_queue = Queue} = Data) ->
    NewQueue = queue:in(Event, Queue),
    {keep_state, Data#data{event_queue = NewQueue}};

running(info, {step_result, StepId, Result}, Data) ->
    handle_step_result(StepId, Result, Data);

running({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(running, Data)}}]};

running({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(running, Data)}}]};

running({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, busy}}]}.

%%====================================================================
%% State: paused
%%====================================================================

paused({call, From}, {resume, ResumeData}, #data{paused_step = StepId,
                                                  steps_state = StepsState} = Data) ->
    case StepId of
        undefined ->
            {keep_state, Data, [{reply, From, {error, no_paused_step}}]};
        _ ->
            StepState = maps:get(StepId, StepsState),
            #{step_def := #{module := Module}} = StepState,
            case erlang:function_exported(Module, on_resume, 3) of
                true ->
                    handle_resume(Module, ResumeData, StepState, StepId, From, Data);
                false ->
                    {keep_state, Data, [{reply, From, {error, resume_not_supported}}]}
            end
    end;

paused(cast, {send_event, Event}, #data{event_queue = Queue} = Data) ->
    NewQueue = queue:in(Event, Queue),
    {keep_state, Data#data{event_queue = NewQueue}};

paused({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(paused, Data)}}]};

paused({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(paused, Data)}}]};

paused({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, paused}}]}.

%%====================================================================
%% State: completed
%%====================================================================

completed(cast, {send_event, _Event}, _Data) ->
    keep_state_and_data;

completed({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(completed, Data)}}]};

completed({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(completed, Data)}}]};

completed({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, completed}}]}.

%%====================================================================
%% State: failed
%%====================================================================

failed(cast, {send_event, _Event}, _Data) ->
    keep_state_and_data;

failed({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(failed, Data)}}]};

failed({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(failed, Data)}}]};

failed({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, failed}}]}.

%%====================================================================
%% Internal - Event Processing
%%====================================================================

process_event_queue(#data{event_queue = Queue} = Data) ->
    case queue:out(Queue) of
        {empty, _} ->
            transition_to_completed(Data);
        {{value, Event}, RestQueue} ->
            Data1 = Data#data{event_queue = RestQueue},
            route_and_activate(Event, Data1)
    end.

route_and_activate(Event, #data{process_def = #{bindings := Bindings},
                                steps_state = StepsState} = Data) ->
    Deliveries = beamai_process_event:route(Event, Bindings),
    NewStepsState = deliver_inputs(Deliveries, StepsState),
    Data1 = Data#data{steps_state = NewStepsState},
    ActivatedSteps = find_activated_steps(NewStepsState),
    case ActivatedSteps of
        [] ->
            process_event_queue(Data1);
        _ ->
            execute_steps(ActivatedSteps, Data1)
    end.

deliver_inputs(Deliveries, StepsState) ->
    lists:foldl(
        fun({StepId, InputName, Value}, Acc) ->
            beamai_process_step:collect_input(StepId, InputName, Value, Acc)
        end,
        StepsState,
        Deliveries
    ).

find_activated_steps(StepsState) ->
    maps:fold(
        fun(StepId, StepState, Acc) ->
            #{step_def := StepDef} = StepState,
            case beamai_process_step:check_activation(StepState, StepDef) of
                true -> [StepId | Acc];
                false -> Acc
            end
        end,
        [],
        StepsState
    ).

execute_steps(ActivatedSteps, #data{process_def = #{execution_mode := Mode}} = Data) ->
    case Mode of
        sequential -> execute_sequential(ActivatedSteps, Data);
        concurrent -> execute_concurrent(ActivatedSteps, Data)
    end.

%%====================================================================
%% Sequential Execution
%%====================================================================

execute_sequential([], Data) ->
    process_event_queue(Data);
execute_sequential([StepId | Rest], #data{steps_state = StepsState,
                                          context = Context} = Data) ->
    StepState = maps:get(StepId, StepsState),
    #{collected_inputs := Inputs} = StepState,
    ClearedState = beamai_process_step:clear_inputs(StepState),
    StepsState1 = StepsState#{StepId => ClearedState},
    case beamai_process_step:execute(ClearedState, Inputs, Context) of
        {events, Events, NewStepState} ->
            StepsState2 = StepsState1#{StepId => NewStepState},
            Data1 = enqueue_events(Events, Data#data{steps_state = StepsState2}),
            execute_sequential(Rest, Data1);
        {pause, Reason, NewStepState} ->
            StepsState2 = StepsState1#{StepId => NewStepState},
            {next_state, paused, Data#data{
                steps_state = StepsState2,
                paused_step = StepId,
                pause_reason = Reason
            }};
        {error, Reason} ->
            handle_error(Reason, Data#data{steps_state = StepsState1})
    end.

%%====================================================================
%% Concurrent Execution
%%====================================================================

execute_concurrent(ActivatedSteps, #data{steps_state = StepsState,
                                          context = Context} = Data) ->
    Self = self(),
    {NewStepsState, Monitors} = lists:foldl(
        fun(StepId, {StAcc, MonAcc}) ->
            StepState = maps:get(StepId, StAcc),
            #{collected_inputs := Inputs} = StepState,
            ClearedState = beamai_process_step:clear_inputs(StepState),
            StAcc1 = StAcc#{StepId => ClearedState},
            spawn_step_worker(Self, StepId, ClearedState, Inputs, Context),
            {StAcc1, MonAcc#{StepId => true}}
        end,
        {StepsState, #{}},
        ActivatedSteps
    ),
    {keep_state, Data#data{
        steps_state = NewStepsState,
        pending_steps = Monitors,
        pending_results = [],
        expected_count = map_size(Monitors)
    }}.

spawn_step_worker(Parent, StepId, StepRuntimeState, Inputs, Context) ->
    spawn_link(fun() ->
        Result = try
            Worker = poolboy:checkout(?POOL_NAME),
            try
                beamai_process_worker:execute_step(Worker, StepRuntimeState, Inputs, Context)
            after
                poolboy:checkin(?POOL_NAME, Worker)
            end
        catch
            Class:Error ->
                {error, {worker_exception, StepId, {Class, Error}}}
        end,
        Parent ! {step_result, StepId, Result}
    end).

handle_step_result(StepId, Result, #data{pending_steps = Pending,
                                          pending_results = Results,
                                          expected_count = Expected} = Data) ->
    NewPending = maps:remove(StepId, Pending),
    NewResults = [{StepId, Result} | Results],
    Data1 = Data#data{pending_steps = NewPending, pending_results = NewResults},
    case length(NewResults) >= Expected of
        true ->
            apply_step_results(NewResults, Data1#data{
                pending_steps = #{},
                pending_results = [],
                expected_count = 0
            });
        false ->
            {keep_state, Data1}
    end.

apply_step_results(Results, Data) ->
    apply_step_results_loop(Results, Data).

apply_step_results_loop([], Data) ->
    process_event_queue(Data);
apply_step_results_loop([{StepId, Result} | Rest], #data{steps_state = StepsState} = Data) ->
    case Result of
        {events, Events, NewStepState} ->
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = enqueue_events(Events, Data#data{steps_state = StepsState1}),
            apply_step_results_loop(Rest, Data1);
        {pause, Reason, NewStepState} ->
            StepsState1 = StepsState#{StepId => NewStepState},
            {next_state, paused, Data#data{
                steps_state = StepsState1,
                paused_step = StepId,
                pause_reason = Reason
            }};
        {error, Reason} ->
            handle_error(Reason, Data)
    end.

%%====================================================================
%% Internal - Helpers
%%====================================================================

init_steps(#{steps := StepDefs}) ->
    init_steps_iter(maps:to_list(StepDefs), #{}).

init_steps_iter([], Acc) -> {ok, Acc};
init_steps_iter([{StepId, StepDef} | Rest], Acc) ->
    case beamai_process_step:init_step(StepDef) of
        {ok, StepState} ->
            init_steps_iter(Rest, Acc#{StepId => StepState});
        {error, _} = Error ->
            Error
    end.

enqueue_events(Events, #data{event_queue = Queue} = Data) ->
    NewQueue = lists:foldl(fun(E, Q) -> queue:in(E, Q) end, Queue, Events),
    Data#data{event_queue = NewQueue}.

transition_to_completed(#data{caller = Caller, steps_state = StepsState} = Data) ->
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_completed, self(), StepsState}
    end,
    {next_state, completed, Data}.

transition_to_failed(Reason, #data{caller = Caller} = Data) ->
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_failed, self(), Reason}
    end,
    {next_state, failed, Data#data{pause_reason = Reason}}.

handle_error(Reason, #data{process_def = #{error_handler := undefined}} = Data) ->
    transition_to_failed(Reason, Data);
handle_error(Reason, #data{process_def = #{error_handler := Handler},
                           context = Context} = Data) ->
    #{module := Module} = Handler,
    ErrorEvent = beamai_process_event:error_event(runtime, Reason),
    ErrorInputs = #{error => ErrorEvent},
    case Module:on_activate(ErrorInputs, #{}, Context) of
        {ok, #{events := Events}} ->
            Data1 = enqueue_events(Events, Data),
            process_event_queue(Data1);
        _ ->
            transition_to_failed(Reason, Data)
    end.

handle_resume(Module, ResumeData, StepState, StepId, From,
              #data{steps_state = StepsState, context = Context} = Data) ->
    #{state := InnerState} = StepState,
    case Module:on_resume(ResumeData, InnerState, Context) of
        {ok, #{events := Events, state := NewInnerState}} ->
            NewStepState = StepState#{state => NewInnerState},
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = enqueue_events(Events, Data#data{
                steps_state = StepsState1,
                paused_step = undefined,
                pause_reason = undefined
            }),
            {next_state, running, Data1,
             [{reply, From, ok}, {state_timeout, 0, process_queue}]};
        {ok, #{state := NewInnerState}} ->
            NewStepState = StepState#{state => NewInnerState},
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = Data#data{
                steps_state = StepsState1,
                paused_step = undefined,
                pause_reason = undefined
            },
            {next_state, running, Data1,
             [{reply, From, ok}, {state_timeout, 0, process_queue}]};
        {error, Reason} ->
            {next_state, failed, Data#data{pause_reason = Reason},
             [{reply, From, {error, Reason}}]}
    end.

format_status(StateName, #data{process_def = #{name := Name},
                                paused_step = PausedStep,
                                pause_reason = PauseReason,
                                event_queue = Queue}) ->
    #{
        state => StateName,
        name => Name,
        queue_length => queue:len(Queue),
        paused_step => PausedStep,
        pause_reason => PauseReason
    }.

do_snapshot(StateName, #data{process_def = ProcessDef,
                              steps_state = StepsState,
                              event_queue = Queue,
                              paused_step = PausedStep,
                              pause_reason = PauseReason}) ->
    beamai_process_state:take_snapshot(#{
        process_def => ProcessDef,
        current_state => StateName,
        steps_state => StepsState,
        event_queue => queue:to_list(Queue),
        paused_step => PausedStep,
        pause_reason => PauseReason
    }).
