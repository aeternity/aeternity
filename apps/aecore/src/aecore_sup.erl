-module(aecore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).
-define(CHILD(Mod,N,Type,Params), {Mod,{Mod,start_link,Params},permanent,N,Type,[Mod]}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [watchdog_childspec(),
                                 ?CHILD(aec_metrics_rpt_dest, 5000, worker),
                                 ?CHILD(aec_keys, 5000, worker),
                                 ?CHILD(aec_peers, 5000, worker),
                                 ?CHILD(aec_tx_pool, 5000, worker),
                                 ?CHILD(aec_sync, 5000, worker),
                                 ?CHILD(aec_conductor, 5000, worker),
                                 ?CHILD(aec_subscribe, 5000, worker)
                                ]
         }}.


watchdog_childspec() ->
    {watchdog, {gen_serv, start, [watchdog]},
     permanent, 5000, worker, [watchdog]}.
