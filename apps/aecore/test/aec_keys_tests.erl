%%%-------------------------------------------------------------------
%%% @author sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2017 23:31
%%%-------------------------------------------------------------------
-module(aec_keys_tests).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [fun(_) ->
              [{"Sign coinbase transaction",
                fun() ->
                        {ok, PubKey} = aec_keys:pubkey(),
                        {ok, Tx} =
                            aec_coinbase_tx:new(#{account => PubKey,
                                                  block_height => 1}),
                        {ok, SignedTx} = aec_keys:sign(Tx),
                        ?assertEqual(Tx, aetx_sign:tx(SignedTx))
                end},
               {"Sign spend transaction",
                fun() ->
                        {ok, PubKey} = aec_keys:pubkey(),
                        {RecipientPubkey, _PrivKey} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
                        {ok, Tx} =
                            aec_spend_tx:new(#{sender => PubKey,
                                               recipient => RecipientPubkey,
                                               amount => 10,
                                               fee => 2,
                                               nonce => 3}),
                        {ok, SignedTx} = aec_keys:sign(Tx),
                        ?assertEqual(Tx, aetx_sign:tx(SignedTx))
                end},
               {"Keys validation (positive case)",
                fun() ->
                        {Pubkey, PrivKey} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
                        ValidPair = aec_keys:check_keys_pair(Pubkey, PrivKey,
                                                         ecdsa, sha256,
                                                         secp256k1),
                        ?assertEqual(true, ValidPair)
                end},
               {"Keys validation (negative case)",
                fun() ->
                        Pubkey = <<"invalid">>,
                        Privkey = <<"key pair">>,
                        ValidPair = aec_keys:check_keys_pair(Pubkey, Privkey,
                                                         ecdsa, sha256,
                                                         secp256k1),
                        ?assertEqual(false, ValidPair)
                end}]
      end]}.

start_test_() ->
    TF =
        fun() ->
                ?_test(
                   aec_test_utils:aec_keys_bare_cleanup(
                     aec_test_utils:aec_keys_bare_setup()))
        end,
    {setup,
     fun() -> ok = application:ensure_started(crypto) end,
     fun(_) -> ok = application:stop(crypto) end,
     lazy_gen(100, TF)}.

lazy_gen(N, TF) ->
    {generator,
     fun () ->
             if N > 0 ->
                     [TF()
                      | lazy_gen(N-1, TF)];
                true ->
                     []
             end
     end}.

-endif.
