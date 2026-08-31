-module(aemon_publisher_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(HEIGHT, 100).
-define(PUBKEY, <<42:32/unit:8>>).
-define(PROTOCOL, ?CERES_PROTOCOL_VSN).

adjust_tx_fee_floor_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"With no configured fee, the floor is unchanged (protocol minimum vs miner gas price)",
        fun default_fee_does_not_raise_the_floor/0}
     , {"A configured fee below the floor does not lower it",
        fun configured_fee_below_the_floor_is_ignored/0}
     , {"A configured fee above the floor wins",
        fun configured_fee_above_the_floor_wins/0}
     ]}.

setup() ->
    Saved = [{K, application:get_env(aemon, K)} || K <- [publisher_pubkey, publisher_fee]],
    ok = application:set_env(aemon, publisher_pubkey,
                             aeser_api_encoder:encode(account_pubkey, ?PUBKEY)),
    Saved.

teardown(Saved) ->
    lists:foreach(
      fun({K, undefined}) -> application:unset_env(aemon, K);
         ({K, {ok, V}})    -> application:set_env(aemon, K, V)
      end, Saved).

default_fee_does_not_raise_the_floor() ->
    ok = application:unset_env(aemon, publisher_fee),
    ?assertEqual(floor_fee(), fee_of(adjust_tx())).

configured_fee_below_the_floor_is_ignored() ->
    ok = application:set_env(aemon, publisher_fee, 1),
    ?assertEqual(floor_fee(), fee_of(adjust_tx())).

configured_fee_above_the_floor_wins() ->
    Configured = floor_fee() * 1000,
    ok = application:set_env(aemon, publisher_fee, Configured),
    ?assertEqual(Configured, fee_of(adjust_tx())).

%% The same floor adjust_tx/5 computes, derived independently rather than by
%% calling adjust_tx/5 itself.
floor_fee() ->
    Tx0 = raw_tx(1),
    GasPrice = aec_tx_pool:minimum_miner_gas_price(),
    GasLimit = aetx:gas_limit(Tx0, ?HEIGHT, protocol()),
    MinFee = aetx:min_fee(Tx0, ?HEIGHT, protocol()),
    lists:max([MinFee, GasPrice * GasLimit]).

adjust_tx() ->
    aemon_publisher:adjust_tx(?HEIGHT, protocol(), _Nonce = 1, _Payload = <<>>, raw_tx(1)).

raw_tx(Fee) ->
    {ok, Tx} = aec_spend_tx:new(
                 #{ sender_id    => aeser_id:create(account, ?PUBKEY)
                  , recipient_id => aeser_id:create(account, ?PUBKEY)
                  , amount       => 0
                  , nonce        => 1
                  , ttl          => 0
                  , payload      => <<>>
                  , fee          => Fee
                  }),
    Tx.

fee_of(Tx) ->
    aetx:fee(Tx).

protocol() ->
    ?PROTOCOL.
