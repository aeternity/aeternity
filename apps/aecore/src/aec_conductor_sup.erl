%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Supervisor (resr_for_one) for block candidate generation and conductor
%%%
%%%  Supervision tree is
%%%```
%%%   aec_conductor_sup (rest_for_one)
%%%         |
%%%         ---------------------
%%%         |                   |
%%%   aec_block_generator  aec_conductor
%%%'''
%%%
%%% @end
%%%=============================================================================
-module(aec_conductor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{rest_for_one, 5, 10}, [?CHILD(aec_block_generator, 5000, worker),
                                  ?CHILD(aec_conductor, 5000, worker)]}}.

