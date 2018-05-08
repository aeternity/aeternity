%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_trees_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_trees).

ensure_account_at_height_test_() ->
    [{"Not existing account is created with 0 balance",
      fun() ->
              Trees0 = aec_test_utils:create_state_tree(),
              AccountPubkey = <<"account_pubkey">>,
              BlockHeight = 23,

              {ok, Trees} = ?TEST_MODULE:ensure_account_at_height(AccountPubkey, Trees0, BlockHeight),

              AccountsTree = aec_trees:accounts(Trees),
              ExpectedAccount = aec_accounts:new(AccountPubkey, 0, BlockHeight),
              ?assertEqual({value, ExpectedAccount}, aec_accounts_trees:lookup(AccountPubkey, AccountsTree))
      end},
     {"account_height_too_bit is returned on block height lower than current account height",
      fun() ->
              AccountPubkey = <<"account_pubkey">>,
              AccountHeight = 25,
              BlockHeight = 23,
              Account = aec_accounts:new(AccountPubkey, 777, AccountHeight),
              Trees = aec_test_utils:create_state_tree_with_account(Account),

              ?assertEqual({error, account_height_too_big},
                           ?TEST_MODULE:ensure_account_at_height(AccountPubkey, Trees, BlockHeight))
      end},
     {"Same unmodified state tree is returned when account is present",
      fun() ->
              AccountPubkey = <<"account_pubkey">>,
              AccountHeight = 21,
              BlockHeight = 23,
              Account = aec_accounts:new(AccountPubkey, 777, AccountHeight),
              Trees = aec_test_utils:create_state_tree_with_account(Account),

              ?assertEqual({ok, Trees},
                           ?TEST_MODULE:ensure_account_at_height(AccountPubkey, Trees, BlockHeight))
      end}].

signatures_check_test_() ->
    {setup,
     fun() ->
             ok = meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_top_state, 0, {ok, aec_trees:new()}),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             meck:unload(aec_chain),
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [ {"Correctly signed transactions are not rejected",
        fun () ->
            SignedCoinbase = aec_test_utils:signed_coinbase_tx(1),
            SignedTxs = [SignedCoinbase],
            {ok, ApprovedTxs, _Trees} =
                ?TEST_MODULE:apply_signed_txs(SignedTxs, aec_trees:new(), 1,
                                          ?PROTOCOL_VERSION),
            ?assertEqual(SignedTxs, ApprovedTxs),
            ok
        end}
     , {"Transactions with broken signatures are rejected",
        fun () ->
            {ok, BadCoinbaseTx} = aec_coinbase_tx:new(#{ account => <<0:32/unit:8>>,
                                                        block_height => 1}),
            MalformedTxs = [aetx_sign:sign(BadCoinbaseTx, <<0:64/unit:8>>)],
            {ok, ApprovedTxs, _Trees} =
                ?TEST_MODULE:apply_signed_txs(MalformedTxs, aec_trees:new(), 1,
                                          ?PROTOCOL_VERSION),
            ?assertEqual([], ApprovedTxs),
            ok
        end}
     ]}.

