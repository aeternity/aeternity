%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Supervisor for servers dealing with inter node communication.
%%%
%%%    Individual connections cannot bring down the central servers (aec_peers,
%%%    aec_sync), but if one of those goes down, all connections are brought
%%%    down, and the whole peer/sync handling is restarted.
%%%
%%%    It is the responsibility of aec_peers to handle that a connection
%%%    (aec_peer_connections) goes down. And the strategy there is to
%%%    re-establish connections where the node is the initiator, and delegate
%%%    the corresponding responsibility to the remote node.
%%%
%%% @end
%%%=============================================================================
-module(aec_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    %% We want all children brought down and restarted
    SupFlags = #{ strategy => one_for_all
                , intensity => 5
                , period => 10},
    ChildSpecs = [ aec_peer_connection_sup_spec() % outgoing connections
                 , worker(aec_peers)
                 , worker(aec_sync)
                 , peer_listener_spec()           % incoming connections
                 ],
    {ok, {SupFlags, ChildSpecs}}.

worker(Mod) ->
    child(Mod, worker, []).

peer_listener_spec() ->
    NumAcceptors = acceptors(),
    MaxConnections = max_connections(),
    {ok, SecKey} = aec_keys:peer_privkey(),
    {ok, PubKey} = aec_keys:peer_pubkey(),
    ranch:child_spec(aec_peer, NumAcceptors,
                     ranch_tcp, [
                         {port, sync_port()},
                         {ip, sync_listen_address()},
                         {max_connections, MaxConnections}
                     ],
                     aec_peer_connection, #{
                         ext_sync_port => ext_sync_port(),
                         seckey => SecKey,
                         pubkey => PubKey
                     }
                    ).

aec_peer_connection_sup_spec() ->
    child(aec_peer_connection_sup, supervisor, [ext_sync_port()]).

child(Mod, Type, Args) ->
    #{ id => Mod
     , start => {Mod, start_link, Args}
     , restart => permanent
     , shutdown => 5000
     , type => Type
     }.

%%====================================================================
%% Shared configs
%%====================================================================

-define(DEFAULT_SYNC_PORT, 3015).
-define(DEFAULT_SYNC_LISTEN_ADDRESS, <<"0.0.0.0">>).
-define(DEFAULT_ACCEPTORS, 10).
-define(DEFAULT_MAX_CONNECTIONS, 100).

sync_port() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"port">>], aecore, sync_port, ?DEFAULT_SYNC_PORT).

ext_sync_port() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"external_port">>],
        aecore, ext_sync_port, sync_port()).

sync_listen_address() ->
    Config = aeu_env:user_config_or_env([<<"sync">>, <<"listen_address">>],
                aecore, sync_listen_address, ?DEFAULT_SYNC_LISTEN_ADDRESS),
    {ok, IpAddress} = inet:parse_address(binary_to_list(Config)),
    IpAddress.

acceptors() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"acceptors">>],
                               aecore, sync_acceptors,
                               ?DEFAULT_ACCEPTORS).

max_connections() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"max_connections">>],
                               aecore, sync_max_connections,
                               ?DEFAULT_MAX_CONNECTIONS).
