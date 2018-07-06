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
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             meck:unload(aec_chain)
     end,
     [{"Apply txs and check resulting balances",
       fun() ->
               %% Init state tree with 2 accounts
               {ok, MinerPubkey} = aec_keys:pubkey(),
               MinerAccount =
                    aec_accounts:set_nonce(aec_accounts:new(MinerPubkey, 100), 10),
               AnotherAccount =
                    aec_accounts:set_nonce(aec_accounts:new(?RECIPIENT_PUBKEY, 80), 12),
               StateTree0 = aec_test_utils:create_state_tree_with_accounts(
                              [MinerAccount, AnotherAccount]),

               BlockHeight = 30,
               %% Create 2 signed transactions (1 valid, 1 invalid)
               {ok, SpendTx} = aec_spend_tx:new(
                                 #{sender => aec_id:create(account, MinerPubkey),
                                   recipient => aec_id:create(account, ?RECIPIENT_PUBKEY),
                                   amount => 40,
                                   fee => 9,
                                   ttl => 100,
                                   nonce => 11,
                                   payload => <<"">>}),
               {ok, OverBalanceTx} = aec_spend_tx:new(
                                       #{sender => aec_id:create(account, MinerPubkey),
                                         recipient => aec_id:create(account, ?RECIPIENT_PUBKEY),
                                         amount => 30000,
                                         fee => 10,
                                         ttl => 100,
                                         nonce => 13,
                                         payload => <<"">>}),
               {ok, SignedSpendTx} = aec_keys:sign_tx(SpendTx),
               {ok, SignedOverBalanceTx} = aec_keys:sign_tx(OverBalanceTx),
               SignedTxs = [SignedSpendTx, SignedOverBalanceTx],

               {ok, ValidSignedTxs, StateTree} =
                  aec_block_micro_candidate:apply_block_txs(SignedTxs, MinerPubkey,
                                                            StateTree0, BlockHeight, ?PROTOCOL_VERSION),

               ?assertEqual([SignedSpendTx], ValidSignedTxs),

               ResultAccountsTree = aec_trees:accounts(StateTree),
               {value, ResultMinerAccount} = aec_accounts_trees:lookup(MinerPubkey, ResultAccountsTree),
               {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee
               ?assertEqual(100 - 40 - 9, aec_accounts:balance(ResultMinerAccount)),
               ?assertEqual(80 + 40, aec_accounts:balance(ResultRecipientAccount))
       end
      }]}.
