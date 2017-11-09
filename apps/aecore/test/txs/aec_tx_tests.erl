-module(aec_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("trees.hrl").

-define(TEST_MODULE, aec_tx).

-define(RECIPIENT_PUBKEY, <<"recipient_pubkey">>).

%% Probably to be moved to common tests
apply_signed_test_() ->
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
               MinerAccount = #account{pubkey = MinerPubkey,
                                       balance = 100,
                                       nonce = 10,
                                       height = 10},
               AnotherAccount = #account{pubkey = ?RECIPIENT_PUBKEY,
                                         balance = 80,
                                         nonce = 12,
                                         height = 7},
               StateTree0 = aec_test_utils:create_state_tree_with_accounts(
                              [MinerAccount, AnotherAccount]),

               %% Create 3 signed transactions (2 valid, 1 invalid)
               {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => MinerPubkey}),
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

               {ok, StateTree} = ?TEST_MODULE:apply_signed(SignedTxs, StateTree0, 30),

               ResultAccountsTree = aec_trees:accounts(StateTree),
               {ok, ResultMinerAccount} = aec_accounts:get(MinerPubkey, ResultAccountsTree),
               {ok, ResultRecipientAccount} = aec_accounts:get(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee + spend_tx fee + coinbase_t[x reward
               ?assertEqual(100 - 40 - 9 + 9 + 10, aec_accounts:balance(ResultMinerAccount)),
               ?assertEqual(80 + 40, aec_accounts:balance(ResultRecipientAccount))
       end
      }]}.
