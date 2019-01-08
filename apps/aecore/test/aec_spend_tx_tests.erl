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
              Env = aetx_env:tx_env(10),
              ?assertEqual({error, too_low_fee}, aetx:process(SpendTx, StateTree, Env))
      end},
     {"Sender account does not exist in state trees",
      fun() ->
              BogusSender = aec_id:create(account, <<42:256>>),
              {ok, SpendTx} = spend_tx(#{fee => 20000, sender_id => BogusSender,
                                         payload => <<"">>}),
              StateTree = aec_test_utils:create_state_tree(),
              Env = aetx_env:tx_env(10),
              ?assertEqual({error, account_not_found}, aetx:process(SpendTx, StateTree, Env))
      end},
     {"Sender account has insufficient funds to cover tx fee + amount",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender_id => ?SENDER_ID,
                                         fee => 20000,
                                         amount => 50,
                                         nonce => 12,
                                         payload => <<"">>}),

              %% Dispatcher sanity check:
              ?assertEqual(?SENDER_PUBKEY, aetx:origin(SpendTx)),
              ?assertEqual(12, aetx:nonce(SpendTx)),
              ?assertEqual(20000, aetx:fee(SpendTx)),

              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 55, nonce => 5}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              Env = aetx_env:tx_env(20),
              ?assertEqual({error, insufficient_funds}, aetx:process(SpendTx, StateTree, Env))
      end},
     {"Sender account has nonce higher than tx nonce",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender_id => ?SENDER_ID,
                                         fee => 20000,
                                         amount => 50,
                                         nonce => 12,
                                         payload => <<"">>}),
              AccountNonce = 15,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 1000000, nonce => AccountNonce}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              Env = aetx_env:tx_env(20),
              ?assertEqual({error, account_nonce_too_high},
                           aetx:process(SpendTx, StateTree, Env))
      end},
      {"TX TTL is too small",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender_id => ?SENDER_ID,
                                         fee => 20000,
                                         amount => 50,
                                         ttl => 10,
                                         nonce => 11,
                                         payload => <<"">>}),
              AccountNonce = 10,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 1000000, nonce => AccountNonce}),
              StateTree = aec_test_utils:create_state_tree_with_account(SenderAccount),
              Env = aetx_env:tx_env(20),
              ?assertEqual({error, ttl_expired}, aetx:process(SpendTx, StateTree, Env))
      end}].

process_test_() ->
    [{"Check and process valid spend tx",
      fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 1000000, nonce => 10}),
              RecipientAccount = new_account(#{pubkey => ?RECIPIENT_PUBKEY, balance => 80, nonce => 12}),
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender_id => ?SENDER_ID,
                                                 recipient_id => ?RECIPIENT_ID,
                                                 amount => 50,
                                                 fee => 20000,
                                                 nonce => 11,
                                                 payload => <<"foo">>}),
              <<"foo">> = aec_spend_tx:payload(aetx:tx(SpendTx)),
              Env = aetx_env:tx_env(20),
              {ok, StateTree} = aetx:process(SpendTx, StateTree0, Env),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultSenderAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),
              {value, ResultRecipientAccount} = aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

              ?assertEqual(1000000 - 50 - 20000, aec_accounts:balance(ResultSenderAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultSenderAccount)),
              ?assertEqual(80 + 50, aec_accounts:balance(ResultRecipientAccount)),
              ?assertEqual(12, aec_accounts:nonce(ResultRecipientAccount))
      end},
      {"Check spend to oneself",
       fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 1000000, nonce => 10}),
              StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender_id => ?SENDER_ID,
                                                 recipient_id => ?SENDER_ID,
                                                 amount => 50,
                                                 fee => 20000,
                                                 nonce => 11,
                                                 payload => <<"foo">>}),
              Env = aetx_env:tx_env(20),
              {ok, StateTree} = aetx:process(SpendTx, StateTree0, Env),

              ResultAccountsTree = aec_trees:accounts(StateTree),
              {value, ResultAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

              ?assertEqual(1000000 - 50 - 20000 + 50, aec_accounts:balance(ResultAccount)),
              ?assertEqual(11, aec_accounts:nonce(ResultAccount))
      end},
      {"Check gas is higher with bigger payload",
       fun() ->
              _SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 1000000, nonce => 10}),
              _RecipientAccount = new_account(#{pubkey => ?RECIPIENT_PUBKEY, balance => 80, nonce => 12}),

              {ok, SpendTx1} = ?TEST_MODULE:new(#{sender_id => ?SENDER_ID,
                                                  recipient_id => ?RECIPIENT_ID,
                                                  amount => 50,
                                                  fee => 20000,
                                                  nonce => 11,
                                                  payload => <<"foo">>}),

              {ok, SpendTx2} = ?TEST_MODULE:new(#{sender_id => ?SENDER_ID,
                                                  recipient_id => ?RECIPIENT_ID,
                                                  amount => 50,
                                                  fee => 20000,
                                                  nonce => 11,
                                                  payload => <<"foo bar">>}),

              ?assert(aetx:gas_limit(SpendTx1, 1) < aetx:gas_limit(SpendTx2, 1))
       end}].

spend_tx(Data) ->
    DefaultData = #{sender_id => ?SENDER_ID,
                    recipient_id => ?RECIPIENT_ID,
                    amount => 0,
                    fee => 0,
                    nonce => 0},
    aec_spend_tx:new(maps:merge(DefaultData, Data)).

new_account(Map) ->
    aec_accounts:set_nonce(
        aec_accounts:new(maps:get(pubkey, Map),
                         maps:get(balance, Map, 0)), maps:get(nonce, Map, 0)).
