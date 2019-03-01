-module(aestratum_connection_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [server]).

init([server]) ->
    %% Extra nonce cache must be started before ranch listener - connection
    %% handling process uses the extra nonce cache.
    %% Procs = [ranch_sup(), extra_nonce_cache(), ranch_listener()],
    Procs = [extra_nonce_cache(), ranch_listener()],
    {ok, {{rest_for_one, 5, 10}, Procs}}.

%% ranch_sup() ->
%%     {ranch_sup, {ranch_sup, start_link, []},
%%      permanent, 5000, supervisor, [ranch_sup]}.

extra_nonce_cache() ->
    {aestratum_extra_nonce_cache,
     {aestratum_extra_nonce_cache, start_link, []},
      permanent, 5000, worker, [aestratum_extra_nonce_cache]}.

ranch_listener() ->
    %% TODO: ip - interface to listen on (all by default)
    Transport      = transport(get_env(transport, tcp)),
    %% The maximum number of connections is a soft limit. In practice, it can
    %% reach max_connections + the number of acceptors.
    MaxConnections = get_env(max_connections, 1024),
    NumAcceptors   = get_env(num_acceptors, 100),
    Port           = get_env(port, 9999),
    TransportOpts  = [{max_connections, MaxConnections},
                      {num_acceptors, NumAcceptors},
                      {port, Port}],
    Protocol       = aestratum_handler,
    ProtocolOpts   = [{module, aestratum_server_session},
                      {max_connections, NumAcceptors + MaxConnections}],
    ranch:child_spec(aestratum_listener, Transport, TransportOpts,
                     Protocol, ProtocolOpts).

transport(tcp) -> ranch_tcp;
transport(ssl) -> ranch_ssl.

get_env(Var, Default) ->
    application:get_env(aestratum, Var, Default).
