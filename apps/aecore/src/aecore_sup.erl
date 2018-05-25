%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Supervisor for the core application
%%%
%%%  Full supervision tree is
%%%```
%%%       aecore_sup  (one_for_one)
%%%           |
%%%           ------------------------------------------------------------
%%%           |         |         |           |            |             |
%%%           |   aec_metrics  aec_keys  aec_tx_pool aec_conductor aec_subscribe
%%%           |
%%%   aec_connection_sup  (one_for_all)
%%%           |
%%%           ---------------------------------------------------
%%%           |                     |          |                |
%%%   aec_peer_connection_sup   aec_peers   aec_sync  aec_connection_listener
%%%     (simple_one_for_one)
%%%           |
%%%           --------------------
%%%           |             |
%%%   aec_peer_connection  ...
%%%'''
%%%
%%% @end
%%%=============================================================================
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
                                 ?CHILD(aec_tx_pool, 5000, worker),
                                 ?CHILD(aec_block_generator, 5000, worker),
                                 ?CHILD(aec_conductor, 5000, worker),
                                 ?CHILD(aec_subscribe, 5000, worker),
                                 ?CHILD(aec_connection_sup, 5000, supervisor)
                                ]
         }}.


watchdog_childspec() ->
    {watchdog, {gen_serv, start, [watchdog]},
     permanent, 5000, worker, [watchdog]}.
