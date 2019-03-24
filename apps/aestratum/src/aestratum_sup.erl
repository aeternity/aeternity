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
    Procs =
        [aestratum_chain(Config),
         aestratum_reward(Config),
         aestratum_extra_nonce_cache(Config),
         aestratum_user_register(Config),
         ranch_listener(Config)],
    {ok, {{one_for_all, 1, 5}, Procs}}.

%% Internal functions.

aestratum_chain(_Config) ->
    {aestratum_chain,
     {aestratum_chain, start_link, []},
     permanent, 5000, worker, [aestratum_chain]}.

aestratum_reward(_Config) ->
    %% All the config should be read in aestratum_app, converted to a map
    %% and this map should be passed to aestratum_sup. aeu_env won't be
    %% used anywhere apart from aestratum_config/aestratum_app.
    {ok, Conf} = aeu_env:user_config(<<"stratum">>),
    #{<<"beneficiaries">> := Benefs,
      <<"reward_last_rounds">> := LastN} = maps:from_list(Conf),
    Beneficiaries =
        lists:foldl(fun (Benef, M) ->
                            [Addr, ShareBin] = binary:split(Benef, <<":">>),
                            Share = binary_to_number(ShareBin),
                            M#{Addr => maps:get(Addr, M, 0) + Share}
                    end, #{}, Benefs),
    {aestratum_reward,
     {aestratum_reward, start_link, [LastN, Beneficiaries]},
     permanent, 5000, worker, [aestratum_reward]}.

aestratum_extra_nonce_cache(_Config) ->
    {aestratum_extra_nonce_cache,
     {aestratum_extra_nonce_cache, start_link, []},
     permanent, 5000, worker, [aestratum_extra_nonce_cache]}.

aestratum_user_register(_Config) ->
    {aestratum_user_register,
     {aestratum_user_register, start_link, []},
     permanent, 5000, worker, [aestratum_user_register]}.

ranch_listener(_Config) ->
    {ok, Conf} = aeu_env:user_config(<<"stratum">>),
    %% TODO: ip - interface to listen on (all by default)
    %% %% The maximum number of connections is a soft limit. In practice, it can
    %% reach max_connections + the number of acceptors.
    #{<<"transport">> := Transport,
      <<"max_connections">> := MaxConnections,
      <<"num_acceptors">> := NumAcceptors,
      <<"port">> := Port} = maps:from_list(Conf),
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

binary_to_number(B) ->
    case catch binary_to_integer(B) of
        {'EXIT', {badarg, _}} -> binary_to_float(B);
        I -> I
    end.

