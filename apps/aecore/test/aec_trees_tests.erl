%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_trees_tests).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, aec_trees).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).

ensure_account_test_() ->
    [{"Not existing account is created with 0 balance",
      fun() ->
              Trees0 = aec_test_utils:create_state_tree(),
              AccountPubkey = <<"_________account_pubkey_________">>,

              Trees = ?TEST_MODULE:ensure_account(AccountPubkey, Trees0),

              AccountsTree = aec_trees:accounts(Trees),
              ExpectedAccount = aec_accounts:new(AccountPubkey, 0),
              ?assertEqual({value, ExpectedAccount}, aec_accounts_trees:lookup(AccountPubkey, AccountsTree))
      end},
     {"Same unmodified state tree is returned when account is present",
      fun() ->
              AccountPubkey = <<"_________account_pubkey_________">>,
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
            {ok, SenderPubkey} = aec_test_utils:wait_for_pubkey(),
            Account = aec_accounts:new(SenderPubkey, 1000),
            TreesIn = aec_test_utils:create_state_tree_with_account(Account),
            {ok, ValidTxs, _InvalidTxs, _Trees} =
                ?TEST_MODULE:apply_txs_on_state_trees(SignedTxs, TreesIn, 1, ?PROTOCOL_VERSION),
            ?assertEqual(SignedTxs, ValidTxs),
            ok
        end}
     , {"Transactions with broken signatures are rejected",
        fun () ->
            Tx = make_spend_tx(<<0:32/unit:8>>, <<1:32/unit:8>>),
            MalformedTxs = [aetx_sign:sign(Tx, <<0:64/unit:8>>)],
            {ok, ValidTxs, _InvalidTxs, _Trees} =
                ?TEST_MODULE:apply_txs_on_state_trees(MalformedTxs, aec_trees:new(), 1, ?PROTOCOL_VERSION),
            ?assertEqual([], ValidTxs),
            ok
        end}
     ]}.

make_spend_tx(Sender, Recipient) ->
    {ok, SpendTx} = aec_spend_tx:new(#{sender => Sender,
                                       recipient => Recipient,
                                       amount => 1,
                                       fee => 1,
                                       nonce => 1,
                                       payload => <<>>}),
    SpendTx.

poi_test_() ->
    [ {"POI for one account",
       fun() ->
               AccountPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,

               %% POI from an empty tree cannot be constructed.
               Trees0 = aec_test_utils:create_state_tree(),
               Poi0 = ?TEST_MODULE:new_poi(Trees0),
               ?assertEqual(?TEST_MODULE:hash(Trees0),
                            ?TEST_MODULE:poi_hash(Poi0)),
               ?assertEqual({error, not_present},
                            ?TEST_MODULE:add_poi(accounts, AccountPubkey,
                                                 Trees0, Poi0)),

               %% Add the account to the tree, and see that
               %% we can construct a POI for the correct account.
               Trees1 = ?TEST_MODULE:ensure_account(AccountPubkey, Trees0),
               Poi1 = ?TEST_MODULE:new_poi(Trees1),
               ?assertEqual(?TEST_MODULE:hash(Trees1),
                            ?TEST_MODULE:poi_hash(Poi1)),

               {ok, SerializedAccount, Poi11} =
                   ?TEST_MODULE:add_poi(accounts, AccountPubkey, Trees1, Poi1),
               ?assertEqual(?TEST_MODULE:hash(Trees1),
                            ?TEST_MODULE:poi_hash(Poi11)),

               %% Check the the stored account in the POI is the correct account
               ATrees = aec_trees:accounts(Trees1),
               {value, Account} = aec_accounts_trees:lookup(AccountPubkey,
                                                            ATrees),
               ?assertEqual(aec_accounts:deserialize(AccountPubkey, SerializedAccount),
                            Account),

               %% Ensure that we can verify the presence of the
               %% account in the POI.
               ?assertEqual(ok,
                            aec_trees:verify_poi(accounts, AccountPubkey,
                                                 SerializedAccount, Poi11)),
               ?assertEqual({ok, Account},
                            aec_trees:lookup_poi(accounts, AccountPubkey, Poi11)),

               %% Ensure that the POI will fail if we change the account.
               {ok, Account1} = aec_accounts:earn(Account, 1),
               SerializedAccount1 = aec_accounts:serialize(Account1),
               ?assertMatch({error, _},
                            aec_trees:verify_poi(accounts, AccountPubkey,
                                                 SerializedAccount1, Poi11))
       end
      },
      {"POI for more than one account"
      , fun() ->
                %% Carefully chosen pubkeys to provoke the intended
                %% behavior.  If not done right, the PoI will contain
                %% more accounts than we want to test ;-)
                Pubkey1 = <<1:4, 2:4, 3:4, 4:4, 123:(?MINER_PUB_BYTES-2)/unit:8>>,
                Pubkey2 = <<1:4, 2:4, 3:4, 5:4, 124:(?MINER_PUB_BYTES-2)/unit:8>>,
                Pubkey3 = <<1:4, 3:4, 4:4, 5:4, 125:(?MINER_PUB_BYTES-2)/unit:8>>,
                Pubkey4 = <<1:4, 3:4, 4:4, 6:4, 126:(?MINER_PUB_BYTES-2)/unit:8>>,

                Account1 = aec_accounts:new(Pubkey1, 0),
                Account2 = aec_accounts:new(Pubkey2, 0),
                Account3 = aec_accounts:new(Pubkey3, 0),
                Account4 = aec_accounts:new(Pubkey4, 0),

                Accounts = [Account1, Account2, Account3, Account4],
                Trees = aec_test_utils:create_state_tree_with_accounts(Accounts),

                Poi0 = ?TEST_MODULE:new_poi(Trees),

                %% Add one account at a time.
                {ok, Serialized1, Poi1} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey1, Trees, Poi0),
                {ok, Serialized2, Poi2} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey2, Trees, Poi1),
                {ok, Serialized3, Poi3} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey3, Trees, Poi2),
                {ok, Serialized4, Poi4} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey4, Trees, Poi3),

                %% Check the reported serializations
                ?assertEqual(Serialized1, aec_accounts:serialize(Account1)),
                ?assertEqual(Serialized2, aec_accounts:serialize(Account2)),
                ?assertEqual(Serialized3, aec_accounts:serialize(Account3)),
                ?assertEqual(Serialized4, aec_accounts:serialize(Account4)),

                %% Check that the reported root hash is the same in all POI.
                ?assertEqual(?TEST_MODULE:hash(Trees),
                             ?TEST_MODULE:poi_hash(Poi1)),
                ?assertEqual(?TEST_MODULE:hash(Trees),
                             ?TEST_MODULE:poi_hash(Poi2)),
                ?assertEqual(?TEST_MODULE:hash(Trees),
                             ?TEST_MODULE:poi_hash(Poi3)),
                ?assertEqual(?TEST_MODULE:hash(Trees),
                             ?TEST_MODULE:poi_hash(Poi4)),

                %% Check that the first account is present in all POI
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Serialized1, Poi1)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi1)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Serialized1, Poi2)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi2)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Serialized1, Poi3)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Serialized1, Poi4)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi4)),

                %% Check that the second account is present in all but the first
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Serialized2, Poi1)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi1)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Serialized2, Poi2)),
                ?assertMatch({ok, Account2},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi2)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Serialized2, Poi3)),
                ?assertMatch({ok, Account2},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Serialized2, Poi4)),
                ?assertMatch({ok, Account2},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi4)),

                %% Check that the third account is present in only the last two
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Serialized3, Poi1)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey3, Poi1)),
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Serialized3, Poi2)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Serialized3, Poi3)),
                ?assertMatch({ok, Account3},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey3, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Serialized3, Poi4)),

                %% Check that the fourth account is present in only the last one
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Serialized4, Poi1)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi1)),
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Serialized4, Poi2)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi2)),
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Serialized4, Poi3)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Serialized4, Poi4)),
                ?assertMatch({ok, Account4},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi4)),

                %% Check serialization/deserialization of PoI
                %% NOTE: The deserialized poi contains a gb_tree,
                %% so it is operation order dependent.
                %% The serialized version is canonical, though.
                [?assertEqual(?TEST_MODULE:serialize_poi(PoI),
                              ?TEST_MODULE:serialize_poi(
                                 ?TEST_MODULE:deserialize_poi(
                                    ?TEST_MODULE:serialize_poi(PoI))))
                 || PoI <- [Poi1, Poi2, Poi2, Poi3, Poi4]]
        end
      }
    ].
