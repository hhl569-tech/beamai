%%%-------------------------------------------------------------------
%%% @doc Dynamic supervisor for process runtimes
%%%
%%% Manages beamai_process_runtime gen_statem processes as temporary
%%% children via simple_one_for_one strategy.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_runtime/1, start_runtime/2]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a new process runtime under this supervisor
-spec start_runtime(beamai_process_builder:process_def()) ->
    {ok, pid()} | {error, term()}.
start_runtime(ProcessDef) ->
    start_runtime(ProcessDef, #{}).

-spec start_runtime(beamai_process_builder:process_def(), map()) ->
    {ok, pid()} | {error, term()}.
start_runtime(ProcessDef, Opts) ->
    supervisor:start_child(?MODULE, [ProcessDef, Opts]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => beamai_process_runtime,
        start => {beamai_process_runtime, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamai_process_runtime]
    },
    {ok, {SupFlags, [ChildSpec]}}.
