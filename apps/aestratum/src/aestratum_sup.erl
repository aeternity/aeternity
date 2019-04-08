-module(aestratum_sup).

-behaviour(supervisor).

%% API.
-export([start_link/1]).

%% supervisor callbacks.
-export([init/1]).

-type config() :: aestratum_config:config().

%% API.

-spec start_link(config()) -> {ok, pid()}.
start_link(Cfg) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Cfg).

%% supervisor callbacks.

init(Cfg) ->
    Procs = case maps:get(enabled, Cfg) of
                true ->
                    aestratum_db:create_tables(disc), % ensure needed tables are present
                    [aestratum_chain(Cfg),
                     aestratum_reward(Cfg),
                     aestratum_extra_nonce_cache(Cfg),
                     aestratum_user_register(Cfg),
                     ranch_listener(Cfg)];
                false ->
                    []
            end,
    {ok, {{one_for_all, 1, 5}, Procs}}.

%% Internal functions.

aestratum_chain(#{reward_cfg :=
                  #{key_pair := KeyPair,
                    beneficiaries_reward := BeneficiariesReward}}) ->
    {aestratum_chain,
     {aestratum_chain, start_link, [BeneficiariesReward, KeyPair]},
     permanent, 5000, worker, [aestratum_chain]}.

aestratum_reward(#{reward_cfg :=
                   #{reward_last_nrounds := RewardLastNRounds,
                     beneficiaries := Beneficiaries,
                     beneficiaries_reward := BeneficiariesReward}}) ->
    {aestratum_reward,
     {aestratum_reward, start_link,
      [RewardLastNRounds, {BeneficiariesReward, Beneficiaries}]},
     permanent, 5000, worker, [aestratum_reward]}.

aestratum_extra_nonce_cache(_Cfg) ->
    {aestratum_extra_nonce_cache,
     {aestratum_extra_nonce_cache, start_link, []},
     permanent, 5000, worker, [aestratum_extra_nonce_cache]}.

aestratum_user_register(_Cfg) ->
    {aestratum_user_register,
     {aestratum_user_register, start_link, []},
     permanent, 5000, worker, [aestratum_user_register]}.

ranch_listener(#{conn_cfg :=
                 #{transport := Transport, max_conns := MaxConns,
                   nacceptors := NAcceptors, port := Port}}) ->
    %% TODO: ip - interface to listen on (all by default)
    TransportOpts = [{max_connections, MaxConns},
                     {num_acceptors, NAcceptors},
                     {port, Port}],
    Protocol      = aestratum_handler,
    ProtocolOpts  = [],
    ranch:child_spec(aestratum_listener,
                     ranch_mod(Transport), TransportOpts,
                     Protocol, ProtocolOpts).

ranch_mod(<<"tcp">>) -> ranch_tcp;
ranch_mod(<<"ssl">>) -> ranch_ssl.
