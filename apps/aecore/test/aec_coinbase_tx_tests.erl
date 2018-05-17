%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_coinbase_tx_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_coinbase_tx).


%%%=============================================================================
%%% Tests
%%%=============================================================================

coinbase_tx_existing_account_test_() ->
    {foreach,
     fun() ->
             PubKey = <<"my_pubkey">>,
             Account0 = aec_accounts:new(PubKey, 23),
             {PubKey, aec_test_utils:create_state_tree_with_account(Account0)}
     end,
     fun(_) ->
             ok
     end,
     [fun({PubKey, Trees0}) ->
              {"Check coinbase trx with existing account: shall not change state",
               fun() ->
                       {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey,
                                                                block_height => 9}),
                       %% Dispatcher sanity check:
                       ?assertEqual(undefined, aetx:origin(CoinbaseTx)),
                       ?assertEqual(undefined, aetx:nonce(CoinbaseTx)),
                       ?assertEqual(0, aetx:fee(CoinbaseTx)),
                       ?assertEqual({ok, Trees0}, aetx:check(CoinbaseTx, Trees0, 9, ?PROTOCOL_VERSION))
               end}
      end,
      fun({PubKey, Trees0}) ->
              {"Check coinbase trx with existing account, but with wrong block height",
               fun() ->
                       Height = 9,
                       {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey,
                                                                block_height => Height}),
                       ?assertEqual({error, wrong_height},
                                    aetx:check(CoinbaseTx, Trees0, Height - 1, ?PROTOCOL_VERSION)),
                       ?assertEqual({error, wrong_height},
                                    aetx:check(CoinbaseTx, Trees0, Height + 1, ?PROTOCOL_VERSION))
               end}
      end,
      fun({PubKey, Trees0}) ->
              {"Check coinbase trx with existing account, but with wrong reward",
               fun() ->
                       Reward = aec_governance:block_mine_reward(),
                       Height = 42,
                       %% no other way of setting a erronous reward
                       CoinbaseTxBiggerReward = {aetx, coinbase_tx, aec_coinbase_tx,
                                      {coinbase_tx, PubKey, Height, Reward + 1}},
                       CoinbaseTxSmallerReward = {aetx, coinbase_tx, aec_coinbase_tx,
                                      {coinbase_tx, PubKey, Height, Reward - 1}},
                       ?assertEqual({error, wrong_reward},
                                    aetx:check(CoinbaseTxBiggerReward, Trees0, Height, ?PROTOCOL_VERSION)),
                       ?assertEqual({error, wrong_reward},
                                    aetx:check(CoinbaseTxSmallerReward, Trees0, Height, ?PROTOCOL_VERSION))
               end}
      end,
      fun({PubKey, Trees0}) ->
              {"Process coinbase trx with existing account",
               fun() ->
                       {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey,
                                                                block_height => 9}),
                       {ok, Trees} = aetx:process(CoinbaseTx, Trees0, 9, ?PROTOCOL_VERSION),

                       AccountsTree = aec_trees:accounts(Trees),
                       {value, Account} = aec_accounts_trees:lookup(PubKey, AccountsTree),
                       ?assertEqual(PubKey, aec_accounts:pubkey(Account)),

                       {coinbase_tx, CbTx} = aetx:specialize_type(CoinbaseTx),
                       Reward = aec_coinbase_tx:reward(CbTx),
                       ?assertEqual(Reward, aec_governance:block_mine_reward()),
                       ?assertEqual(23 + Reward, aec_accounts:balance(Account))
               end}
      end
     ]
    }.

create_coinbase_tx_no_account_test() ->
    {foreach,
     fun() ->
             PubKey = <<"my_pubkey">>,
             Trees0 = aec_test_utils:create_state_tree(),
             {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey,
                                                      block_height => 1}),
             {PubKey, Trees0, CoinbaseTx}
     end,
     fun(_) ->
             ok
     end,
     [fun({PubKey, Trees0, CoinbaseTx}) ->
              {"Check coinbase trx without existing account in state: shall create account",
               fun() ->
                       {Succ, Trees} = aec_coinbase_tx:check(CoinbaseTx, Trees0, 9, ?PROTOCOL_VERSION),
                       ?assertEqual(ok, Succ),
                       AccountsTrees = aec_trees:accounts(Trees),
                       Account = aec_accounts:set_nonce(aec_accounts:new(PubKey, 0), 9),
                       ?assertEqual({value, Account},
                                    aec_accounts_trees:lookup(PubKey, AccountsTrees))
               end}
      end,
      fun({_PubKey, Trees0, CoinbaseTx}) ->
              {"Process coinbase trx without existing account in state: shall fail",
               ?assertEqual({error, account_not_found}, aec_coinbase_tx:process(CoinbaseTx, Trees0, 9, ?PROTOCOL_VERSION))}
      end
     ]
    }.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-endif.
