%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("apps/aecore/include/blocks.hrl").

-define(TEST_MODULE, aetx).

-define(RECIPIENT_PUBKEY, <<"_________recipient_pubkey_______">>).

%% Probably to be moved to common tests
apply_signed_txs_test_() ->
    {setup,
     fun() ->
             ok = meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_top_state, 0, {ok, aec_trees:new()}),
             ok = meck:new(aec_governance, [passthrough]),
             meck:expect(aec_governance, minimum_gas_price, 1, 1),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             meck:unload(aec_governance),
             meck:unload(aec_chain)
     end,
     [{"Apply txs and check resulting balances",
       fun() ->
               %% Init state tree with 2 accounts
               {ok, MinerPubkey} = aec_keys:pubkey(),
               {ok, MinerPrivkey} = aec_keys:sign_privkey(),

               MinerAccount = aec_accounts:set_nonce(aec_accounts:new(MinerPubkey, 100000), 10),
               AnotherAccount = aec_accounts:set_nonce(aec_accounts:new(?RECIPIENT_PUBKEY, 80000), 12),
               StateTree0 = aec_test_utils:create_state_tree_with_accounts([MinerAccount, AnotherAccount]),

               BlockHeight = 30,
               %% Create 2 signed transactions (1 valid, 1 invalid)
               {ok, SpendTx} = aec_spend_tx:new(
                                 #{sender_id => aec_id:create(account, MinerPubkey),
                                   recipient_id => aec_id:create(account, ?RECIPIENT_PUBKEY),
                                   amount => 40,
                                   fee => 20000,
                                   ttl => 100,
                                   nonce => 11,
                                   payload => <<"">>}),
               {ok, OverBalanceTx} = aec_spend_tx:new(
                                       #{sender_id => aec_id:create(account, MinerPubkey),
                                         recipient_id => aec_id:create(account, ?RECIPIENT_PUBKEY),
                                         amount => 30000,
                                         fee => 20000,
                                         ttl => 100,
                                         nonce => 13,
                                         payload => <<"">>}),
               SignedSpendTx = aec_test_utils:sign_tx(SpendTx, MinerPrivkey),
               SignedOverBalanceTx = aec_test_utils:sign_tx(OverBalanceTx, MinerPrivkey),

               SignedTxs = [SignedSpendTx, SignedOverBalanceTx],
               Env = aetx_env:tx_env(BlockHeight),
               {ok, ValidSignedTxs, StateTree} =
                  aec_block_micro_candidate:apply_block_txs(SignedTxs, StateTree0, Env),

               ?assertEqual([SignedSpendTx], ValidSignedTxs),

               ResultAccountsTree = aec_trees:accounts(StateTree),
               {value, ResultMinerAccount} = aec_accounts_trees:lookup(MinerPubkey, ResultAccountsTree),
               {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee
               ?assertEqual(100000 - 40 - 20000, aec_accounts:balance(ResultMinerAccount)),
               ?assertEqual(80000 + 40, aec_accounts:balance(ResultRecipientAccount))
       end
      }]}.
