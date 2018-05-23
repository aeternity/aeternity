%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_trees_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_trees).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).

ensure_account_test_() ->
    [{"Not existing account is created with 0 balance",
      fun() ->
              Trees0 = aec_test_utils:create_state_tree(),
              AccountPubkey = <<"account_pubkey">>,

              Trees = ?TEST_MODULE:ensure_account(AccountPubkey, Trees0),

              AccountsTree = aec_trees:accounts(Trees),
              ExpectedAccount = aec_accounts:new(AccountPubkey, 0),
              ?assertEqual({value, ExpectedAccount}, aec_accounts_trees:lookup(AccountPubkey, AccountsTree))
      end},
     {"Same unmodified state tree is returned when account is present",
      fun() ->
              AccountPubkey = <<"account_pubkey">>,
              Account = aec_accounts:new(AccountPubkey, 777),
              Trees = aec_test_utils:create_state_tree_with_account(Account),

              ?assertEqual(Trees,
                           ?TEST_MODULE:ensure_account(AccountPubkey, Trees))
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
            SignedSpend =
                    aec_test_utils:signed_spend_tx(
                      #{recipient => <<1:32/unit:8>>,
                        amount => 1,
                        fee => 1,
                        nonce => 1,
                        payload => <<>>}),
            SignedTxs = [SignedSpend],
            {ok, SenderPubkey} = aec_keys:wait_for_pubkey(),
            Account = aec_accounts:new(SenderPubkey, 1000),
            TreesIn = aec_test_utils:create_state_tree_with_account(Account),
            {ok, ApprovedTxs, _Trees} =
                ?TEST_MODULE:apply_signed_txs(?MINER_PUBKEY, SignedTxs, TreesIn, 1,
                                          ?PROTOCOL_VERSION),
            ?assertEqual(SignedTxs, ApprovedTxs),
            ok
        end}
     , {"Transactions with broken signatures are rejected",
        fun () ->
            Tx = make_a_spend_tx(<<0:32/unit:8>>, <<1:32/unit:8>>),
            MalformedTxs = [aetx_sign:sign(Tx, <<0:64/unit:8>>)],
            {ok, ApprovedTxs, _Trees} =
                ?TEST_MODULE:apply_signed_txs(?MINER_PUBKEY, MalformedTxs, aec_trees:new(), 1,
                                          ?PROTOCOL_VERSION),
            ?assertEqual([], ApprovedTxs),
            ok
        end}
     ]}.

make_a_spend_tx(Sender, Recipient) ->
    {ok, SpendTx} = aec_spend_tx:new(#{sender => Sender,
                                       recipient => Recipient,
                                       amount => 1,
                                       fee => 1,
                                       nonce => 1,
                                       payload => <<>>}),
    SpendTx.
