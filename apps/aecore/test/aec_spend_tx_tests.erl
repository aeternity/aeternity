%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_spend_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, aec_spend_tx).

-define(SENDER_PUBKEY, <<"_________sender_pubkey__________">>).
-define(SENDER_ID,  aec_id:create(account, ?SENDER_PUBKEY)).
-define(RECIPIENT_PUBKEY, <<"________recipient_pubkey________">>).
-define(RECIPIENT_ID, aec_id:create(account, ?RECIPIENT_PUBKEY)).


check_test_() ->
    [{"Tx fee lower than minimum fee defined in governance",
      fun() ->
              {ok, SpendTx} = spend_tx(#{fee => 0, %% minimum governance fee = 1
                                         payload => <<"">>}),
              StateTree = aec_test_utils:create_state_tree(),
              ?assertEqual({error, too_low_fee},
                           aetx:check(SpendTx, StateTree, 10, ?PROTOCOL_VERSION))
      end},
     {"Sender account does not exist in state trees",
      fun() ->
              BogusSender = aec_id:create(account, <<42:256>>),
              {ok, SpendTx} = spend_tx(#{fee => 10, sender => BogusSender,
                                         payload => <<"">>}),
              StateTree = aec_test_utils:create_state_tree(),
              ?assertEqual({error, account_not_found},
                           aetx:check(SpendTx, StateTree, 10, ?PROTOCOL_VERSION))
      end},
     {"Sender account has insufficient funds to cover tx fee + amount",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_ID,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12,
                                         payload => <<"">>}),

              %% Dispatcher sanity check:
              ?assertEqual(?SENDER_PUBKEY, aetx:origin(SpendTx)),
              ?assertEqual(12, aetx:nonce(SpendTx)),
              ?assertEqual(10, aetx:fee(SpendTx)),

              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 55, nonce => 5}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, insufficient_funds},
                           aetx:check(SpendTx, StateTree, 20, ?PROTOCOL_VERSION))
      end},
     {"Sender account has nonce higher than tx nonce",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_ID,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12,
                                         payload => <<"">>}),
              AccountNonce = 15,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => AccountNonce}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, account_nonce_too_high},
                           aetx:check(SpendTx, StateTree, 20, ?PROTOCOL_VERSION))
      end},
      {"TX TTL is too small",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_ID,
                                         fee => 10,
                                         amount => 50,
                                         ttl => 10,
                                         nonce => 11,
                                         payload => <<"">>}),
              AccountNonce = 10,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => AccountNonce}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, ttl_expired},
                           aetx:check(SpendTx, StateTree, 20, ?PROTOCOL_VERSION))
      end}].

process_test_() ->
    [{"Check and process valid spend tx",
      fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10}),
              RecipientAccount = new_account(#{pubkey => ?RECIPIENT_PUBKEY, balance => 80, nonce => 12}),
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_ID,
                                                 recipient => ?RECIPIENT_ID,
                                                 amount => 50,
                                                 fee => 10,
                                                 nonce => 11,
                                                 payload => <<"foo">>}),
              <<"foo">> = aec_spend_tx:payload(aetx:tx(SpendTx)),
              {ok, StateTree0} = aetx:check(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),
              {ok, StateTree} = aetx:process(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultSenderAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),
              {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10, aec_accounts:balance(ResultSenderAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultSenderAccount)),
              ?assertEqual(80 + 50, aec_accounts:balance(ResultRecipientAccount)),
              ?assertEqual(12, aec_accounts:nonce(ResultRecipientAccount))
      end},
      {"Check spend to oneself",
       fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10}),
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_ID,
                                                 recipient => ?SENDER_ID,
                                                 amount => 50,
                                                 fee => 10,
                                                 nonce => 11,
                                                 payload => <<"foo">>}),
              {ok, StateTree0} = aetx:check(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),
              {ok, StateTree} = aetx:process(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10 + 50, aec_accounts:balance(ResultAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultAccount))
      end}].

spend_tx(Data) ->
    DefaultData = #{sender => ?SENDER_ID,
                    recipient => ?RECIPIENT_ID,
                    amount => 0,
                    fee => 0,
                    nonce => 0},
    aec_spend_tx:new(maps:merge(DefaultData, Data)).

new_account(Map) ->
    aec_accounts:set_nonce(
        aec_accounts:new(maps:get(pubkey, Map),
                         maps:get(balance, Map, 0)), maps:get(nonce, Map, 0)).
