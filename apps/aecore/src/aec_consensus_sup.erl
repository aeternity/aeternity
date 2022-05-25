-module(aec_consensus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Spec) ->
    supervisor:start_child(?SERVER, Spec).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{one_for_one, 3, 30}, []}}.
