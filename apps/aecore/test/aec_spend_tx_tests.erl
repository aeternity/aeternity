-module(aec_spend_tx_tests).


-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

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
              StateTree = create_state_tree(),
              ?assertEqual({error, sender_account_not_found},
                           ?TEST_MODULE:check(SpendTx, StateTree, 10))
      end},
     {"Sender account has insufficient funds to cover tx fee + amount",
      fun() ->
              SpendTx = #spend_tx{sender = ?SENDER_PUBKEY,
                                  fee = 10,
                                  amount = 50,
                                  nonce = 12},

              SenderAccount = #account{pubkey = ?SENDER_PUBKEY, balance = 55, nonce = 5, height = 10},
              StateTree = create_state_tree_with_accounts([SenderAccount]),
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
              StateTree = create_state_tree_with_accounts([SenderAccount]),
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
              StateTree = create_state_tree_with_accounts([SenderAccount]),
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
              StateTree = create_state_tree_with_accounts([SenderAccount, RecipientAccount]),
              ?assertEqual({error, recipient_account_height_too_big},
                           ?TEST_MODULE:check(SpendTx, StateTree, BlockHeight))
      end}].


%% Internals

create_state_tree() ->
    {ok, AccountsTree} = aec_accounts:empty(),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree).

create_state_tree_with_accounts(Accounts) ->
    {ok, AccountsTree0} = aec_accounts:empty(),
    AccountsTree1 = lists:foldl(
                      fun(Account, Tree0) ->
                              {ok, Tree} = aec_accounts:put(Account, Tree0),
                              Tree
                      end, AccountsTree0, Accounts),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree1).
