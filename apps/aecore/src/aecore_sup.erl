-module(aecore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
                                 {peers,
                                  {aec_peers, start_link, []},
                                  permanent,
                                  5000,
                                  worker,
                                  [aec_peers]},
                                 {chain,
                                  %% XXX empty block instead ot genesis block,
                                  %% ticket #84
                                  {aec_chain, start_link, [#block{}]},
                                  permanent,
                                  5000,
                                  worker,
                                  [aec_peers]}
                                ]} }.
