%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Supervisor for peer connections
%%% @end
%%%=============================================================================
-module(aec_peer_connection_sup).

-behaviour(supervisor).

%% API
-export([ start_link/1
        , start_peer_connection/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

start_peer_connection(Opts) ->
    supervisor:start_child(?SERVER, [Opts]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Port]) ->
    SupFlags = #{ strategy => simple_one_for_one
                , intensity => 5
                , period => 10},
    ChildSpecs = [#{ id => aec_peer_connection
                   , start => {aec_peer_connection, connect_start_link, [Port]}
                   , type => worker
                   , restart => temporary
                   , shutdown => 5000
                   }],
    {ok, {SupFlags, ChildSpecs}}.


