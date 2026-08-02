%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Foundation
%%%-------------------------------------------------------------------

-module(aec_hc_vote_pool_tests).

-include_lib("eunit/include/eunit.hrl").

hc_vote_pool_test_() ->
    {foreach,
     fun() ->
             application:ensure_started(gproc),
             ok = application:ensure_started(crypto),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis_and_forks(),
             GB = aec_test_utils:genesis_block(),
             {ok, _} = aec_chain_state:insert_block(GB),
             meck:new(aec_jobs_queues),
             meck:expect(aec_jobs_queues, run, fun(_, F) -> F() end),
             %% HCVoteTxs are only valid on a PoS (hyperchains) consensus.
             meck:new(aec_consensus, [passthrough]),
             meck:expect(aec_consensus, get_consensus_type, 0, pos),
             {ok, _} = aec_hc_vote_pool:start_link(),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_hc_vote_pool:stop(),
             meck:unload(aec_consensus),
             meck:unload(aec_jobs_queues),
             aec_test_utils:unmock_genesis_and_forks(),
             aec_test_utils:stop_chain_db(),
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok
     end,
     [{"Valid vote tx is accepted and peekable",
       fun() ->
               STx = signed_vote_tx(1, 0),
               ?assertEqual(ok, aec_hc_vote_pool:push(STx, tx_received)),
               {hc_vote_tx, Tx} = aetx:specialize_type(aetx_sign:tx(STx)),
               ?assertEqual({ok, [Tx]}, aec_hc_vote_pool:peek(1)),
               ?assertEqual({ok, [Tx]}, aec_hc_vote_pool:peek({1, 0})),
               ?assertEqual({ok, []}, aec_hc_vote_pool:peek(2))
       end},
      {"Non-vote tx is rejected",
       fun() ->
               #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
               {ok, Spend} =
                   aec_spend_tx:new(#{ sender_id    => aeser_id:create(account, Pub)
                                     , recipient_id => aeser_id:create(account, Pub)
                                     , amount       => 1
                                     , nonce        => 1
                                     , fee          => 20000
                                     , ttl          => 0
                                     , payload      => <<"">> }),
               STx = aec_test_utils:sign_tx(Spend, Priv),
               ?assertEqual({error, only_hc_vote_tx_allowed},
                            aec_hc_vote_pool:push(STx, tx_received)),
               ?assertEqual({ok, []}, aec_hc_vote_pool:peek(1))
       end},
      {"Vote tx with invalid signature is rejected",
       fun() ->
               #{ public := Pub } = enacl:sign_keypair(),
               #{ secret := WrongPriv } = enacl:sign_keypair(),
               STx = signed_vote_tx(1, 0, Pub, WrongPriv),
               ?assertEqual({error, signature_check_failed},
                            aec_hc_vote_pool:push(STx, tx_received)),
               ?assertEqual({ok, []}, aec_hc_vote_pool:peek(1))
       end},
      {"Vote tx is rejected under PoW consensus",
       fun() ->
               meck:expect(aec_consensus, get_consensus_type, 0, pow),
               STx = signed_vote_tx(1, 0),
               ?assertEqual({error, invalid_at_protocol},
                            aec_hc_vote_pool:push(STx, tx_received)),
               ?assertEqual({ok, []}, aec_hc_vote_pool:peek(1))
       end},
      {"Push on empty chain db falls back to the genesis env",
       fun() ->
               %% Restart the chain db without inserting the genesis block:
               %% aec_chain has no top block node and get_onchain_env/0 must
               %% fall back to the genesis header and state.
               aec_test_utils:stop_chain_db(),
               aec_test_utils:start_chain_db(),
               ?assertEqual(undefined, aec_chain:top_header_hash_and_state()),

               STx = signed_vote_tx(1, 0),
               ?assertEqual(ok, aec_hc_vote_pool:push(STx, tx_received)),
               {hc_vote_tx, Tx} = aetx:specialize_type(aetx_sign:tx(STx)),
               ?assertEqual({ok, [Tx]}, aec_hc_vote_pool:peek(1))
       end}
     ]}.

signed_vote_tx(Epoch, Type) ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    signed_vote_tx(Epoch, Type, Pub, Priv).

signed_vote_tx(Epoch, Type, Pub, Priv) ->
    {ok, Aetx} = aec_hc_vote_tx:new(#{ voter_id => aeser_id:create(account, Pub)
                                     , epoch    => Epoch
                                     , type     => Type
                                     , data     => #{<<"key">> => <<"value">>} }),
    aec_test_utils:sign_tx(Aetx, Priv).
