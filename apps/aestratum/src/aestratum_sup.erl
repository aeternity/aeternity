-module(aestratum_sup).

-behaviour(supervisor).

%% API.
-export([start_link/1]).

%% supervisor callbacks.
-export([init/1]).

%% API.

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% supervisor callbacks.

init(Config) ->
    Childs = [aestratum_extra_nonce_cache(Config),
              aestratum_user_register(Config),
              ranch_listener(Config),
              aestratum_node_sup(Config)],
    {ok, {{one_for_all, 1, 5}, Childs}}.

%% Internal functions.

aestratum_node_sup(Config) ->
    {aestratum_node_sup, {aestratum_node_sup, start_link, [Config]},
     permanent, 5000, supervisor, [aestratum_node_sup]}.

aestratum_extra_nonce_cache(_Config) ->
    {aestratum_extra_nonce_cache,
     {aestratum_extra_nonce_cache, start_link, []},
     permanent, 5000, worker, [aestratum_extra_nonce_cache]}.

aestratum_user_register(_Config) ->
    {aestratum_user_register,
     {aestratum_user_register, start_link, []},
     permanent, 5000, worker, [aestratum_user_register]}.

ranch_listener(Config) ->
    %% TODO: ip - interface to listen on (all by default)
    %% %% The maximum number of connections is a soft limit. In practice, it can
    %% reach max_connections + the number of acceptors.
    #{<<"transport">> := Transport,
      <<"max_connections">> := MaxConnections,
      <<"num_acceptors">> := NumAcceptors,
      <<"port">> := Port} = maps:from_list(Config),
    TransportOpts = [{max_connections, MaxConnections},
                     {num_acceptors, NumAcceptors},
                     {port, Port}],
    Protocol      = aestratum_handler,
    ProtocolOpts  = [{module, aestratum_session},
                     {max_connections, NumAcceptors + MaxConnections}],
    ranch:child_spec(aestratum_listener,
                     ranch_mod(Transport), TransportOpts,
                     Protocol, ProtocolOpts).

ranch_mod(<<"tcp">>) -> ranch_tcp;
ranch_mod(<<"ssl">>) -> ranch_ssl.
