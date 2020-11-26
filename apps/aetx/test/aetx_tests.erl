%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../../aecore/include/blocks.hrl").

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

               Nonce = 10,
               SomeAmt = 40,
               Fee = 20000,
               SenderBalance = Fee + SomeAmt + 10, %% some extra, so we don't end with a balance of 0
               MinerAccount =
                  aec_accounts:set_nonce(aec_accounts:new(MinerPubkey, SenderBalance),
                                         Nonce),
               RecipientBalance = 80000,
               AnotherAccount =
                  aec_accounts:set_nonce(aec_accounts:new(?RECIPIENT_PUBKEY,
                                                          RecipientBalance), 12),
               StateTree0 = aec_test_utils:create_state_tree_with_accounts([MinerAccount, AnotherAccount]),

               BlockHeight = 30,
               %% Create 2 signed transactions (1 valid, 1 invalid)
               {ok, SpendTx} =
                    spend_tx(#{sender_id => aeser_id:create(account, MinerPubkey),
                               nonce => Nonce + 1,
                               fee => Fee,
                               amount => SomeAmt}),
               {ok, OverBalanceTx} = spend_tx(
                                       #{sender_id => aeser_id:create(account, MinerPubkey),
                                         amount => SenderBalance + 1,
                                         fee => Fee,
                                         nonce => Nonce + 2}),
               SignedSpendTx = aec_test_utils:sign_tx(SpendTx, MinerPrivkey),
               SignedOverBalanceTx = aec_test_utils:sign_tx(OverBalanceTx, MinerPrivkey),

               SignedTxs = [SignedSpendTx, SignedOverBalanceTx],
               Env = aetx_env:tx_env(BlockHeight),
               {ok, ValidSignedTxs, StateTree, _Events} =
                  aec_block_micro_candidate:apply_block_txs(SignedTxs, StateTree0, Env),

               ?assertEqual([SignedSpendTx], ValidSignedTxs),

               ResultAccountsTree = aec_trees:accounts(StateTree),
               {value, ResultMinerAccount} = aec_accounts_trees:lookup(MinerPubkey, ResultAccountsTree),
               {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee
               ?assertEqual(SenderBalance - SomeAmt - Fee, aec_accounts:balance(ResultMinerAccount)),
               ?assertEqual(RecipientBalance + SomeAmt, aec_accounts:balance(ResultRecipientAccount))
       end
      }]}.

spend_tx(Opts) ->
    {ok, MinerPubkey} = aec_keys:pubkey(),
    DefaultOpts =
        #{sender_id => aeser_id:create(account, MinerPubkey),
          recipient_id => aeser_id:create(account, ?RECIPIENT_PUBKEY),
          amount => 40,
          fee => 20000,
          ttl => 100,
          nonce => 11,
          payload => <<"">>},
    {ok, _SpendTx} = aec_spend_tx:new(maps:merge(DefaultOpts, Opts)).

