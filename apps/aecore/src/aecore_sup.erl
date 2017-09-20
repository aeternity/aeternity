-module(aecore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
    GB = aec_block_genesis:genesis_block_as_deserialized_from_network(),
    {ok, {{one_for_one, 5, 10}, [?CHILD(aec_peers, 5000, worker),
                                 ?CHILD(aec_chain, 5000, worker, [GB]),
                                 ?CHILD(aec_keys, 5000, worker)]
         }}.

