%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Supervisor for the core application
%%%
%%%  Full supervision tree is
%%%```
%%%       aecore_sup
%%%     (one_for_one)
%%%           |
%%%           -----------------------------------------
%%%           |                  |                    |
%%%           |             aec_worker_sup            |
%%%           |                  |                    |
%%%           |         -----------------------       |
%%%           |         |         |           |       |
%%%           |   aec_metrics  aec_keys  aec_tx_pool  |
%%%           |                                       |
%%%   aec_connection_sup                      aec_conductor_sup
%%%     (one_for_all)                          (rest_for_one)
%%%           |                                       |
%%%           |                                       ---------------------
%%%           |                                       |                   |
%%%           |                                 aec_block_generator  aec_conductor
%%%           |
%%%           -------------------------------------------------------------------
%%%           |                    |         |            |                     |
%%%   aec_peer_connection_sup  aec_peers  aec_sync  aec_tx_pool_sync  aec_connection_listener
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
    ChildSpecs =
        [?CHILD(aec_worker_sup, 5000, worker),
         ?CHILD(aec_consensus_sup, 5000, supervisor),
         ?CHILD(aec_conductor_sup, 5000, supervisor) ],

    {ok, {{one_for_one, 0, 10}, ChildSpecs}}.
