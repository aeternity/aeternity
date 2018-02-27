%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("apps/aecore/include/common.hrl").

-define(TEST_MODULE, aetx).

-define(RECIPIENT_PUBKEY, <<"recipient_pubkey">>).

%% Probably to be moved to common tests
apply_signed_txs_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [{"Apply txs and add total fee to miner's account",
       fun() ->
               %% Init state tree with 2 accounts
               {ok, MinerPubkey} = aec_keys:pubkey(),
               MinerAccount =
                    aec_accounts:set_nonce(aec_accounts:new(MinerPubkey, 100, 10), 10),
               AnotherAccount =
                    aec_accounts:set_nonce(aec_accounts:new(?RECIPIENT_PUBKEY, 80, 7), 12),
               StateTree0 = aec_test_utils:create_state_tree_with_accounts(
                              [MinerAccount, AnotherAccount]),

               BlockHeight = 30,
               %% Create 3 signed transactions (2 valid, 1 invalid)
               {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => MinerPubkey,
                                                        block_height => BlockHeight}),
               {ok, SpendTx} = aec_spend_tx:new(
                                 #{sender => MinerPubkey,
                                   recipient => ?RECIPIENT_PUBKEY,
                                   amount => 40,
                                   fee => 9,
                                   nonce => 11}),
               {ok, OverBalanceTx} = aec_spend_tx:new(
                                       #{sender => MinerPubkey,
                                         recipient => ?RECIPIENT_PUBKEY,
                                         amount => 30000,
                                         fee => 10,
                                         nonce => 13}),
               {ok, SignedCoinbase} = aec_keys:sign(CoinbaseTx),
               {ok, SignedSpendTx} = aec_keys:sign(SpendTx),
               {ok, SignedOverBalanceTx} = aec_keys:sign(OverBalanceTx),
               SignedTxs = [SignedCoinbase, SignedSpendTx, SignedOverBalanceTx],

               {ok, ValidSignedTxs, StateTree} =
                  aec_trees:apply_signed_txs(SignedTxs, StateTree0, BlockHeight),
               ?assertEqual([SignedCoinbase, SignedSpendTx], ValidSignedTxs),

               ResultAccountsTree = aec_trees:accounts(StateTree),
               {value, ResultMinerAccount} = aec_accounts_trees:lookup(MinerPubkey, ResultAccountsTree),
               {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee + spend_tx fee + coinbase_t[x reward
               ?assertEqual(100 - 40 - 9 + 9 + 10, aec_accounts:balance(ResultMinerAccount)),
               ?assertEqual(80 + 40, aec_accounts:balance(ResultRecipientAccount))
       end
      }]}.
