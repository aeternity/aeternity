%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_spend_tx).

-define(SENDER_PUBKEY, <<"sender_pubkey">>).
-define(RECIPIENT_PUBKEY, <<"recipient_pubkey">>).

check_test_() ->
    [{"Tx fee lower than minimum fee defined in governance",
      fun() ->
              {ok, SpendTx} = spend_tx(#{fee => 0}), %% minimum governance fee = 1
              StateTree = aec_test_utils:create_state_tree(),
              ?assertEqual({error, too_low_fee},
                           aetx:check(SpendTx, StateTree, 10))
      end},
     {"Sender account does not exist in state trees",
      fun() ->
              {ok, SpendTx} = spend_tx(#{fee => 10, sender => <<42>>}),
              StateTree = aec_test_utils:create_state_tree(),
              ?assertEqual({error, account_not_found},
                           aetx:check(SpendTx, StateTree, 10))
      end},
     {"Sender account has insufficient funds to cover tx fee + amount",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_PUBKEY,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12}),

              %% Dispatcher sanity check:
              ?assertEqual(?SENDER_PUBKEY, aetx:origin(SpendTx)),
              ?assertEqual(12, aetx:nonce(SpendTx)),
              ?assertEqual(10, aetx:fee(SpendTx)),

              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 55, nonce => 5, height => 10}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, insufficient_funds},
                           aetx:check(SpendTx, StateTree, 20))
      end},
     {"Sender account has nonce higher than tx nonce",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_PUBKEY,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12}),
              AccountNonce = 15,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => AccountNonce, height => 10}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, account_nonce_too_high},
                           aetx:check(SpendTx, StateTree, 20))
      end},
     {"Sender account has height higher than tx height",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_PUBKEY,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12}),
              AccountHeight = 100,
              BlockHeight = 20,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10, height => AccountHeight}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, sender_account_height_too_big},
                           aetx:check(SpendTx, StateTree, BlockHeight))
      end},
     {"Recipient account has height higher than tx height",
      fun() ->
              {ok, SpendTx} =
                spend_tx(#{sender => ?SENDER_PUBKEY,
                           recipient => ?RECIPIENT_PUBKEY,
                           fee => 10,
                           amount => 50,
                           nonce => 12}),
              SenderAccountHeight = 10,
              RecipientAccountHeight = 100,
              BlockHeight = 20,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10, height => SenderAccountHeight}),
              RecipientAccount = new_account(#{pubkey => ?RECIPIENT_PUBKEY, height => RecipientAccountHeight}),
              StateTree = aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),
              ?assertEqual({error, recipient_account_height_too_big},
                           aetx:check(SpendTx, StateTree, BlockHeight))
      end}].

process_test_() ->
    [{"Check and process valid spend tx",
      fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10, height => 10}),
              RecipientAccount = new_account(#{pubkey => ?RECIPIENT_PUBKEY, balance => 80, nonce => 12, height => 11}),
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_PUBKEY,
                                                 recipient => ?RECIPIENT_PUBKEY,
                                                 amount => 50,
                                                 fee => 10,
                                                 nonce => 11}),
              {ok, StateTree0} = aetx:check(SpendTx, StateTree0, 20),
              {ok, StateTree} = aetx:process(SpendTx, StateTree0, 20),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultSenderAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),
              {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10, aec_accounts:balance(ResultSenderAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultSenderAccount)),
              ?assertEqual(20, aec_accounts:height(ResultSenderAccount)),
              ?assertEqual(80 + 50, aec_accounts:balance(ResultRecipientAccount)),
              ?assertEqual(12, aec_accounts:nonce(ResultRecipientAccount)),
              ?assertEqual(20, aec_accounts:height(ResultRecipientAccount))
      end},
      {"Check spend to oneself",
       fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10, height => 10}),
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_PUBKEY,
                                                 recipient => ?SENDER_PUBKEY,
                                                 amount => 50,
                                                 fee => 10,
                                                 nonce => 11}),
              {ok, StateTree0} = aetx:check(SpendTx, StateTree0, 20),
              {ok, StateTree} = aetx:process(SpendTx, StateTree0, 20),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10 + 50, aec_accounts:balance(ResultAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultAccount)),
              ?assertEqual(20, aec_accounts:height(ResultAccount))
      end}].

spend_tx(Data) ->
    DefaultData = #{sender => ?SENDER_PUBKEY,
                    recipient => ?RECIPIENT_PUBKEY,
                    amount => 0,
                    fee => 0,
                    nonce => 0},
    aec_spend_tx:new(maps:merge(DefaultData, Data)).

new_account(Map) ->
    aec_accounts:set_nonce(
        aec_accounts:new(maps:get(pubkey, Map),
                         maps:get(balance, Map, 0),
                         maps:get(height, Map, 1)), maps:get(nonce, Map, 0)).
