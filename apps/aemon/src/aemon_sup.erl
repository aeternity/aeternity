-module(aemon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).

%% ==================================================================
%% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ==================================================================
%% supervisor callbacks

init(_) ->
    ChildSpecs = child_specs(aemon_config:is_active()),
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

%% ==================================================================
%% internal functions

child_specs(false) ->
    [];
child_specs(true) ->
    [ ?CHILD(aemon_mon_ttl, 5000, worker)
    , ?CHILD(aemon_mon_on_chain, 5000, worker)
    , ?CHILD(aemon_mon_gen_stats, 5000, worker)
    , ?CHILD(aemon_mon, 5000, worker)
    , ?CHILD(aemon_publisher, 5000, worker)
    ].
