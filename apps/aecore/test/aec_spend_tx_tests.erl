%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("core_txs.hrl").

-define(TEST_MODULE, aec_spend_tx).

-define(SENDER_PUBKEY, <<"sender_pubkey">>).
-define(RECIPIENT_PUBKEY, <<"recipient_pubkey">>).

check_test_() ->
    [{"Tx fee lower than minimum fee defined in governance",
      fun() ->
              SpendTx = #spend_tx{fee = 0}, %% minimum governance fee = 1
              ?assertEqual({error, too_low_fee},
                           ?TEST_MODULE:check(SpendTx, #trees{}, 10))
      end},
     {"Sender account does not exist in state trees",
      fun() ->
              SpendTx = #spend_tx{fee = 10},
              StateTree = aec_test_utils:create_state_tree(),
              ?assertEqual({error, account_not_found},
                           ?TEST_MODULE:check(SpendTx, StateTree, 10))
      end},
     {"Sender account has insufficient funds to cover tx fee + amount",
      fun() ->
              SpendTx = #spend_tx{sender = ?SENDER_PUBKEY,
                                  fee = 10,
                                  amount = 50,
                                  nonce = 12},

              %% Dispatcher sanity check:
              ?assertEqual(?SENDER_PUBKEY, aec_tx:origin(SpendTx)),
              ?assertEqual(12, aec_tx:nonce(SpendTx)),
              ?assertEqual(10, aec_tx:fee(SpendTx)),

              SenderAccount = #account{pubkey = ?SENDER_PUBKEY, balance = 55, nonce = 5, height = 10},
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, insufficient_funds},
                           ?TEST_MODULE:check(SpendTx, StateTree, 20))
      end},
     {"Sender account has nonce higher than tx nonce",
      fun() ->
              SpendTx = #spend_tx{sender = ?SENDER_PUBKEY,
                                  fee = 10,
                                  amount = 50,
                                  nonce = 12},
              AccountNonce = 15,
              SenderAccount = #account{pubkey = ?SENDER_PUBKEY, balance = 100, nonce = AccountNonce, height = 10},
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, account_nonce_too_high},
                           ?TEST_MODULE:check(SpendTx, StateTree, 20))
      end},
     {"Sender account has height higher than tx height",
      fun() ->
              SpendTx = #spend_tx{sender = ?SENDER_PUBKEY,
                                  fee = 10,
                                  amount = 50,
                                  nonce = 12},
              AccountHeight = 100,
              BlockHeight = 20,
              SenderAccount = #account{pubkey = ?SENDER_PUBKEY, balance = 100, nonce = 10, height = AccountHeight},
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, sender_account_height_too_big},
                           ?TEST_MODULE:check(SpendTx, StateTree, BlockHeight))
      end},
     {"Recipient account has height higher than tx height",
      fun() ->
              SpendTx = #spend_tx{sender = ?SENDER_PUBKEY,
                                  recipient = ?RECIPIENT_PUBKEY,
                                  fee = 10,
                                  amount = 50,
                                  nonce = 12},
              SenderAccountHeight = 10,
              RecipientAccountHeight = 100,
              BlockHeight = 20,
              SenderAccount = #account{pubkey = ?SENDER_PUBKEY, balance = 100, nonce = 10, height = SenderAccountHeight},
              RecipientAccount = #account{pubkey = ?RECIPIENT_PUBKEY, height = RecipientAccountHeight},
              StateTree = aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),
              ?assertEqual({error, recipient_account_height_too_big},
                           ?TEST_MODULE:check(SpendTx, StateTree, BlockHeight))
      end}].

process_test_() ->
    [{"Check and process valid spend tx",
      fun() ->
              SenderAccount = #account{pubkey = ?SENDER_PUBKEY,
                                       balance = 100,
                                       nonce = 10,
                                       height = 10},
              RecipientAccount = #account{pubkey = ?RECIPIENT_PUBKEY,
                                          balance = 80,
                                          nonce = 12,
                                          height = 11},
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_PUBKEY,
                                                 recipient => ?RECIPIENT_PUBKEY,
                                                 amount => 50,
                                                 fee => 10,
                                                 nonce => 11}),
              {ok, StateTree0} = ?TEST_MODULE:check(SpendTx, StateTree0, 20),
              {ok, StateTree} = ?TEST_MODULE:process(SpendTx, StateTree0, 20),

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
              SenderAccount = #account{pubkey = ?SENDER_PUBKEY,
                                       balance = 100,
                                       nonce = 10,
                                       height = 10},
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_PUBKEY,
                                                 recipient => ?SENDER_PUBKEY,
                                                 amount => 50,
                                                 fee => 10,
                                                 nonce => 11}),
              {ok, StateTree0} = ?TEST_MODULE:check(SpendTx, StateTree0, 20),
              {ok, StateTree} = ?TEST_MODULE:process(SpendTx, StateTree0, 20),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10 + 50, aec_accounts:balance(ResultAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultAccount)),
              ?assertEqual(20, aec_accounts:height(ResultAccount))
      end}].

