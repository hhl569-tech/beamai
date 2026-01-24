%%%-------------------------------------------------------------------
%%% @doc poolboy worker for step execution
%%%
%%% Each worker executes a single step's on_activate callback.
%%% Workers are checked out from the pool, execute, and return.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_worker).

-behaviour(gen_server).

%% API
-export([execute_step/4]).

%% poolboy / gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-define(POOL_NAME, beamai_process_pool).

%%====================================================================
%% API
%%====================================================================

%% @doc Execute a step via poolboy worker pool
-spec execute_step(pid(), beamai_process_step:step_runtime_state(),
                   #{atom() => term()}, beamai_context:t()) ->
    {events, [beamai_process_event:event()], beamai_process_step:step_runtime_state()} |
    {pause, term(), beamai_process_step:step_runtime_state()} |
    {error, term()}.
execute_step(Worker, StepRuntimeState, Inputs, Context) ->
    gen_server:call(Worker, {execute_step, StepRuntimeState, Inputs, Context}, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    {ok, #{}}.

handle_call({execute_step, StepRuntimeState, Inputs, Context}, _From, State) ->
    Result = beamai_process_step:execute(StepRuntimeState, Inputs, Context),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
