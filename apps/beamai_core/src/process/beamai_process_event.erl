%%%-------------------------------------------------------------------
%%% @doc Process event types and routing
%%%
%%% Events are tagged maps that flow between steps in a process.
%%% Event bindings define how events are routed to step inputs.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_event).

-export([
    new/2,
    new/3,
    error_event/2,
    error_event/3,
    system_event/2,
    binding/3,
    binding/4,
    route/2
]).

-export_type([event/0, event_type/0, event_binding/0, delivery/0]).

-type event_type() :: public | internal | error | system.

-type event() :: #{
    '__process_event__' := true,
    id := binary(),
    name := atom(),
    type := event_type(),
    source := atom() | undefined,
    data := term(),
    timestamp := integer()
}.

-type transform_fun() :: fun((term()) -> term()) | undefined.

-type event_binding() :: #{
    '__event_binding__' := true,
    event_name := atom(),
    target_step := atom(),
    target_input := atom(),
    transform := transform_fun()
}.

-type delivery() :: {StepId :: atom(), InputName :: atom(), Data :: term()}.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new public event
-spec new(atom(), term()) -> event().
new(Name, Data) ->
    new(Name, Data, #{}).

%% @doc Create a new event with options
-spec new(atom(), term(), map()) -> event().
new(Name, Data, Opts) ->
    #{
        '__process_event__' => true,
        id => beamai_id:gen_id(<<"evt">>),
        name => Name,
        type => maps:get(type, Opts, public),
        source => maps:get(source, Opts, undefined),
        data => Data,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Create an error event
-spec error_event(atom(), term()) -> event().
error_event(Source, Reason) ->
    error_event(Source, Reason, #{}).

-spec error_event(atom(), term(), map()) -> event().
error_event(Source, Reason, Opts) ->
    new(error, #{reason => Reason, source => Source},
        Opts#{type => error, source => Source}).

%% @doc Create a system event
-spec system_event(atom(), term()) -> event().
system_event(Name, Data) ->
    new(Name, Data, #{type => system}).

%% @doc Create an event binding
-spec binding(atom(), atom(), atom()) -> event_binding().
binding(EventName, TargetStep, TargetInput) ->
    binding(EventName, TargetStep, TargetInput, undefined).

%% @doc Create an event binding with transform
-spec binding(atom(), atom(), atom(), transform_fun()) -> event_binding().
binding(EventName, TargetStep, TargetInput, Transform) ->
    #{
        '__event_binding__' => true,
        event_name => EventName,
        target_step => TargetStep,
        target_input => TargetInput,
        transform => Transform
    }.

%% @doc Route an event through bindings, producing deliveries
-spec route(event(), [event_binding()]) -> [delivery()].
route(#{name := EventName, data := Data}, Bindings) ->
    lists:filtermap(
        fun(#{event_name := BoundName, target_step := Step,
              target_input := Input, transform := Transform}) ->
            case BoundName =:= EventName of
                true ->
                    TransformedData = apply_transform(Transform, Data),
                    {true, {Step, Input, TransformedData}};
                false ->
                    false
            end
        end,
        Bindings
    ).

%%====================================================================
%% Internal
%%====================================================================

apply_transform(undefined, Data) -> Data;
apply_transform(Fun, Data) when is_function(Fun, 1) -> Fun(Data).
