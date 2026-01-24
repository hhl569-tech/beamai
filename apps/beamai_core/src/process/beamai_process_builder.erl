%%%-------------------------------------------------------------------
%%% @doc Process builder - pure data construction
%%%
%%% Builds a process_def() that is serializable and inspectable.
%%% Validates step modules and binding references at compile time.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_builder).

-export([
    new/1,
    add_step/3,
    add_step/4,
    add_binding/2,
    add_initial_event/2,
    set_error_handler/2,
    set_execution_mode/2,
    compile/1
]).

-export_type([builder/0, process_def/0, step_def/0]).

-type step_def() :: #{
    '__step_def__' := true,
    id := atom(),
    module := module(),
    config := map(),
    required_inputs := [atom()]
}.

-type error_handler_def() :: #{
    module := module(),
    config := map()
}.

-type builder() :: #{
    '__process_builder__' := true,
    name := atom(),
    steps := #{atom() => step_def()},
    bindings := [beamai_process_event:event_binding()],
    initial_events := [beamai_process_event:event()],
    error_handler := error_handler_def() | undefined,
    execution_mode := concurrent | sequential
}.

-type process_def() :: #{
    '__process_def__' := true,
    name := atom(),
    steps := #{atom() => step_def()},
    bindings := [beamai_process_event:event_binding()],
    initial_events := [beamai_process_event:event()],
    error_handler := error_handler_def() | undefined,
    execution_mode := concurrent | sequential
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new builder
-spec new(atom()) -> builder().
new(Name) when is_atom(Name) ->
    #{
        '__process_builder__' => true,
        name => Name,
        steps => #{},
        bindings => [],
        initial_events => [],
        error_handler => undefined,
        execution_mode => concurrent
    }.

%% @doc Add a step with default config
-spec add_step(builder(), atom(), module()) -> builder().
add_step(Builder, StepId, Module) ->
    add_step(Builder, StepId, Module, #{}).

%% @doc Add a step with config
-spec add_step(builder(), atom(), module(), map()) -> builder().
add_step(#{steps := Steps} = Builder, StepId, Module, Config) when is_atom(StepId), is_atom(Module) ->
    StepDef = #{
        '__step_def__' => true,
        id => StepId,
        module => Module,
        config => Config,
        required_inputs => maps:get(required_inputs, Config, [input])
    },
    Builder#{steps => Steps#{StepId => StepDef}}.

%% @doc Add an event binding
-spec add_binding(builder(), beamai_process_event:event_binding()) -> builder().
add_binding(#{bindings := Bindings} = Builder, Binding) ->
    Builder#{bindings => Bindings ++ [Binding]}.

%% @doc Add an initial event to trigger at process start
-spec add_initial_event(builder(), beamai_process_event:event()) -> builder().
add_initial_event(#{initial_events := Events} = Builder, Event) ->
    Builder#{initial_events => Events ++ [Event]}.

%% @doc Set the error handler step
-spec set_error_handler(builder(), error_handler_def()) -> builder().
set_error_handler(Builder, Handler) ->
    Builder#{error_handler => Handler}.

%% @doc Set execution mode (concurrent or sequential)
-spec set_execution_mode(builder(), concurrent | sequential) -> builder().
set_execution_mode(Builder, Mode) when Mode =:= concurrent; Mode =:= sequential ->
    Builder#{execution_mode => Mode}.

%% @doc Compile builder into a validated process_def
-spec compile(builder()) -> {ok, process_def()} | {error, [term()]}.
compile(#{name := Name, steps := Steps, bindings := Bindings,
          initial_events := InitEvents, error_handler := ErrorHandler,
          execution_mode := ExecMode}) ->
    Errors = validate_steps(Steps) ++
             validate_bindings(Bindings, Steps) ++
             validate_error_handler(ErrorHandler),
    case Errors of
        [] ->
            {ok, #{
                '__process_def__' => true,
                name => Name,
                steps => Steps,
                bindings => Bindings,
                initial_events => InitEvents,
                error_handler => ErrorHandler,
                execution_mode => ExecMode
            }};
        _ ->
            {error, Errors}
    end.

%%====================================================================
%% Internal - Validation
%%====================================================================

validate_steps(Steps) ->
    maps:fold(
        fun(StepId, #{module := Module}, Acc) ->
            case code:which(Module) of
                non_existing ->
                    [{step_module_not_found, StepId, Module} | Acc];
                _ ->
                    validate_step_callbacks(StepId, Module, Acc)
            end
        end,
        [],
        Steps
    ).

validate_step_callbacks(StepId, Module, Acc) ->
    Exports = Module:module_info(exports),
    Required = [{init, 1}, {can_activate, 2}, {on_activate, 3}],
    lists:foldl(
        fun({Fun, Arity}, InnerAcc) ->
            case lists:member({Fun, Arity}, Exports) of
                true -> InnerAcc;
                false ->
                    [{missing_callback, StepId, Module, {Fun, Arity}} | InnerAcc]
            end
        end,
        Acc,
        Required
    ).

validate_bindings(Bindings, Steps) ->
    lists:foldl(
        fun(#{target_step := TargetStep}, Acc) ->
            case maps:is_key(TargetStep, Steps) of
                true -> Acc;
                false ->
                    [{binding_target_not_found, TargetStep} | Acc]
            end
        end,
        [],
        Bindings
    ).

validate_error_handler(undefined) -> [];
validate_error_handler(#{module := Module}) ->
    case code:which(Module) of
        non_existing ->
            [{error_handler_module_not_found, Module}];
        _ ->
            []
    end.
