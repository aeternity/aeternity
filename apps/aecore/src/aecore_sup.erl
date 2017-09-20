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
    {ok, {{one_for_one, 5, 10}, [?CHILD(aec_peers, 5000, worker),
                                  %% XXX empty block instead ot genesis block,
                                  %% ticket #84
                                 ?CHILD(aec_chain, 5000, worker, [#block{}]),
                                 ?CHILD(aec_keys, 5000, worker)]
         }}.

