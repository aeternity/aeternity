-module(aestratum_connection_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [server]).

init([server]) ->
    %% Extra nonce cache must be started before ranch listener - connection
    %% handling process uses the extra nonce cache.
    Procs = [extra_nonce_cache(), ranch_listener()],
    {ok, {{rest_for_one, 5, 10}, Procs}}.

extra_nonce_cache() ->
    {aestratum_extra_nonce_cache,
     {aestratum_extra_nonce_cache, start_link, []},
      permanent, 5000, worker, [aestratum_extra_nonce_cache]}.

ranch_listener() ->
    {ok, Conf} = aeu_env:user_config(<<"stratum">>),
    %% TODO: ip - interface to listen on (all by default)
    %% %% The maximum number of connections is a soft limit. In practice, it can
    %% %% reach max_connections + the number of acceptors.
    #{<<"transport">> := Transport,
      <<"max_connections">> := MaxConnections,
      <<"num_acceptors">> := NumAcceptors,
      <<"port">> := Port} = maps:from_list(Conf),
    TransportOpts = [{max_connections, MaxConnections},
                     {num_acceptors, NumAcceptors},
                     {port, Port}],
    Protocol      = aestratum_handler,
    ProtocolOpts  = [{module, aestratum_server_session},
                     {max_connections, NumAcceptors + MaxConnections}],
    ranch:child_spec(aestratum_listener,
                     ranch_mod(Transport), TransportOpts,
                     Protocol, ProtocolOpts).

ranch_mod(<<"tcp">>) -> ranch_tcp;
ranch_mod(<<"ssl">>) -> ranch_ssl.
