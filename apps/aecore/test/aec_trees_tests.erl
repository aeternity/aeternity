%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_trees_tests).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-include_lib("apps/aecontract/src/aecontract.hrl").

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
                      #{recipient_id => aec_id:create(account, <<1:32/unit:8>>),
                        amount => 1,
                        fee => 20000,
                        nonce => 1,
                        payload => <<>>}),
            SignedTxs = [SignedSpend],
            {ok, SenderPubkey, _} = aec_test_utils:wait_for_pubkey(),
            Account = aec_accounts:new(SenderPubkey, 1000000),
            TreesIn = aec_test_utils:create_state_tree_with_account(Account),
            Env = aetx_env:tx_env(1),
            {ok, ValidTxs, _InvalidTxs, _Trees} =
                ?TEST_MODULE:apply_txs_on_state_trees(SignedTxs, TreesIn, Env),
            ?assertEqual(SignedTxs, ValidTxs),
            ok
        end}
     , {"Transactions with broken signatures are rejected",
        fun () ->
            Tx = make_spend_tx(<<0:32/unit:8>>, <<1:32/unit:8>>),
            MalformedTxs = [aec_test_utils:sign_tx(Tx, <<0:64/unit:8>>)],
            Env = aetx_env:tx_env(1),
            {ok, ValidTxs, _InvalidTxs, _Trees} =
                ?TEST_MODULE:apply_txs_on_state_trees(MalformedTxs, aec_trees:new(), Env),
            ?assertEqual([], ValidTxs),
            ok
        end}
     ]}.

make_spend_tx(Sender, Recipient) ->
    {ok, SpendTx} = aec_spend_tx:new(#{sender_id => aec_id:create(account, Sender),
                                       recipient_id => aec_id:create(account, Recipient),
                                       amount => 1,
                                       fee => 20000,
                                       nonce => 1,
                                       payload => <<>>}),
    SpendTx.

poi_test_() ->
    [ {"PoI constructed from empty state trees enables computation of state trees root hash",
       fun() ->
               Trees0 = aec_test_utils:create_state_tree(),
               Poi0 = ?TEST_MODULE:new_poi(Trees0),
               ?assertEqual(?TEST_MODULE:hash(Trees0),
                            ?TEST_MODULE:poi_hash(Poi0))
       end},
      {"PoI constructed from empty state trees can be serialized/deserialized",
       fun() ->
               Trees0 = aec_test_utils:create_state_tree(),
               Poi0 = ?TEST_MODULE:new_poi(Trees0),
               assert_equal_poi(Poi0,
                                ?TEST_MODULE:deserialize_poi(
                                   ?TEST_MODULE:serialize_poi(Poi0)))
       end},
      {"Non-empty PoI cannot be constructed from empty state trees",
       fun() ->
               AccountPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,

               Trees0 = aec_test_utils:create_state_tree(),
               Poi0 = ?TEST_MODULE:new_poi(Trees0),

               ?assertEqual({error, not_present},
                            ?TEST_MODULE:add_poi(accounts, AccountPubkey,
                                                 Trees0, Poi0)),

               ContractPubkey = aect_contracts:pubkey(make_contract(AccountPubkey)),
               ?assertEqual({error, not_present},
                            ?TEST_MODULE:add_poi(contracts, ContractPubkey,
                                                 Trees0, Poi0))
       end},
      {"Empty PoI constructed from non-empty state trees can be serialized/deserialized",
       fun() ->
               AccountPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,

               Trees0 = aec_test_utils:create_state_tree(),
               Poi0 = ?TEST_MODULE:new_poi(Trees0),

               Trees1 = ?TEST_MODULE:ensure_account(AccountPubkey, Trees0),
               Poi1 = ?TEST_MODULE:new_poi(Trees1),

               assert_equal_poi(Poi1,
                                ?TEST_MODULE:deserialize_poi(
                                   ?TEST_MODULE:serialize_poi(Poi1))),
               ?assertNotEqual(?TEST_MODULE:serialize_poi(Poi0),
                               ?TEST_MODULE:serialize_poi(Poi1))
       end},
      {"Verification of presence of object fails for PoI not including object",
       fun() ->
               AccountPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,
               Account = aec_accounts:new(AccountPubkey, 0),
               Contract = make_contract(AccountPubkey),
               ContractPubkey = aect_contracts:pubkey(Contract),

               %% Check that empty PoI does not prove object.
               Trees0 = aec_test_utils:create_state_tree(),
               Poi0 = ?TEST_MODULE:new_poi(Trees0),
               ?assertMatch({error, _},
                            aec_trees:verify_poi(accounts, AccountPubkey, Account, Poi0)),
               ?assertMatch({error, _},
                            aec_trees:verify_poi(contracts, ContractPubkey, Contract, Poi0)),

               %% Check that non-empty PoI that does not include
               %% object does not prove it.
               Trees1A = aec_trees:set_accounts(
                           Trees0,
                           aec_accounts_trees:enter(Account, aec_trees:accounts(Trees0))),
               Poi1A = ?TEST_MODULE:new_poi(Trees1A),
               {ok, Poi1A1} = ?TEST_MODULE:add_poi(accounts, AccountPubkey, Trees1A, Poi1A),
               ?assertMatch({error, _},
                            aec_trees:verify_poi(contracts, ContractPubkey, Contract, Poi1A1)),
               %%
               Trees1C = aec_trees:set_contracts(
                           Trees0,
                           aect_state_tree:insert_contract(Contract, aec_trees:contracts(Trees0))),
               Poi1C = ?TEST_MODULE:new_poi(Trees1C),
               {ok, Poi1C1} = ?TEST_MODULE:add_poi(contracts, ContractPubkey, Trees1C, Poi1C),
               ?assertMatch({error, _},
                            aec_trees:verify_poi(accounts, AccountPubkey, Account, Poi1C1)),

               ok
       end},
      {"Broken serialized PoI fails verification",
       fun() ->
               OwnerPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,
               Store    = make_store(#{<<1>> => <<"v1">>, <<2>> => <<"v2">>}),
               Contract = aect_contracts:set_state(Store, make_contract(OwnerPubkey)),
               ContractPubkey = aect_contracts:pubkey(Contract),

               Trees0 = aec_test_utils:create_state_tree(),
               Trees1 = aec_trees:set_contracts(
                          Trees0,
                          aect_state_tree:insert_contract(Contract, aec_trees:contracts(Trees0))),
               Poi1 = ?TEST_MODULE:new_poi(Trees1),
               {ok, Poi11} = ?TEST_MODULE:add_poi(contracts, ContractPubkey, Trees1, Poi1),
               Poi11Fields = aec_trees:internal_serialize_poi_fields(Poi11),

               PoiFromFieldsF =
                   fun(Fields) ->
                           aec_trees:deserialize_poi(
                             aec_trees:internal_serialize_poi_from_fields(Fields))
                   end,

               %% The identified PoI fields lead to PoI that proves object.
               ?assertEqual(ok,
                            aec_trees:verify_poi(contracts, ContractPubkey, Contract,
                                                 PoiFromFieldsF(Poi11Fields))),

               %% Check that removing a node from PoI makes object inclusion not proved.
               [{_, Poi11ProofKVs = [_,_|_]}] = %% Hardcoded expectation on test data: at least 2 nodes so able to remove 1 leaving at least a node.
                   poi_fields_get(contracts, Poi11Fields),
               lists:foreach(
                 fun(KV) ->
                         BrokenSerializedPoi =
                             PoiFromFieldsF(
                               poi_fields_update_with(
                                 contracts,
                                 fun([{H, ProofKVs = [_,_|_]}]) ->
                                         NewProofKVs = ProofKVs -- [KV],
                                         ?assertMatch(_ when length(ProofKVs) =:= (1 + length(NewProofKVs)), NewProofKVs),
                                         [{H, NewProofKVs}]
                                 end,
                                 Poi11Fields)),
                         ?assertMatch({error, _},
                                      aec_trees:verify_poi(contracts, ContractPubkey, Contract,
                                                           BrokenSerializedPoi))
                 end,
                 Poi11ProofKVs),

               ok
       end},
      {"POI for one account",
       fun() ->
               AccountKeyF = fun(A) -> aec_accounts:pubkey(A) end,
               ChangeAccountF =
                   fun(A) -> {ok, A1} = aec_accounts:earn(A, 1), A1 end,
               InsertAccountF =
                   fun(Ts, A) ->
                           As = aec_trees:accounts(Ts),
                           aec_trees:set_accounts(
                             Ts, aec_accounts_trees:enter(A, As))
                   end,

               AccountPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,
               Account = aec_accounts:new(AccountPubkey, 0),

               check_poi_for_one_object(
                 accounts,
                 AccountKeyF, ChangeAccountF,
                 InsertAccountF,
                 fun(A) -> A end,
                 Account)
       end},
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
                {ok, Poi1} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey1, Trees, Poi0),
                {ok, Poi2} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey2, Trees, Poi1),
                {ok, Poi3} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey3, Trees, Poi2),
                {ok, Poi4} =
                    ?TEST_MODULE:add_poi(accounts, Pubkey4, Trees, Poi3),

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
                                                     Account1, Poi1)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi1)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Account1, Poi2)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi2)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Account1, Poi3)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey1,
                                                     Account1, Poi4)),
                ?assertMatch({ok, Account1},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey1, Poi4)),

                %% Check that the second account is present in all but the first
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Account2, Poi1)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi1)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Account2, Poi2)),
                ?assertMatch({ok, Account2},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi2)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Account2, Poi3)),
                ?assertMatch({ok, Account2},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey2,
                                                     Account2, Poi4)),
                ?assertMatch({ok, Account2},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey2, Poi4)),

                %% Check that the third account is present in only the last two
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Account3, Poi1)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey3, Poi1)),
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Account3, Poi2)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Account3, Poi3)),
                ?assertMatch({ok, Account3},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey3, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey3,
                                                     Account3, Poi4)),

                %% Check that the fourth account is present in only the last one
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Account4, Poi1)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi1)),
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Account4, Poi2)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi2)),
                ?assertMatch({error, _},
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Account4, Poi3)),
                ?assertMatch({error, not_found},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi3)),
                ?assertMatch(ok,
                             ?TEST_MODULE:verify_poi(accounts, Pubkey4,
                                                     Account4, Poi4)),
                ?assertMatch({ok, Account4},
                             ?TEST_MODULE:lookup_poi(accounts, Pubkey4, Poi4)),

                %% Check serialization/deserialization of PoI
                [assert_equal_poi(PoI,
                                  ?TEST_MODULE:deserialize_poi(
                                     ?TEST_MODULE:serialize_poi(PoI)))
                 || PoI <- [Poi1, Poi2, Poi2, Poi3, Poi4]]
        end
      },
      {"PoI for one contract without store",
       fun() ->
               OwnerPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,
               Contract = make_contract(OwnerPubkey),
               ?assertEqual(make_store(#{}), aect_contracts:state(Contract)), %% Hardcoded expectation on test data.

               check_poi_for_one_contract(
                 Contract,
                 _ChangeContractF =
                     fun(C) ->
                             true = aect_contracts:active(C), %% Assumption for simplicity.
                             aect_contracts:set_active(false, C)
                     end)
       end},
      {"PoI for one contract with store",
       fun() ->
               OwnerPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,
               Contract0 = make_contract(OwnerPubkey),
               Contract1 = aect_contracts:set_state(make_store(#{<<2>> => <<"v">>}),
                                                    Contract0),

               check_poi_for_one_contract(
                 Contract1,
                 _ChangeContractF =
                     fun(C) ->
                             true = aect_contracts:active(C), %% Assumption for simplicity.
                             aect_contracts:set_active(false, C)
                     end)
       end},
      {"PoI for one contract without store that becomes with store",
       fun() ->
               OwnerPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,

               check_poi_for_one_contract(
                 make_contract(OwnerPubkey),
                 fun(C) -> %% Change contract function.
                         ?assertEqual(make_store(#{}), %% Assumption for simplicity.
                                      aect_contracts:state(C)),
                         aect_contracts:set_state(make_store(#{<<1>> => <<"v">>}), C)
                 end)
       end},
      {"PoI for one contract with store that becomes without store",
       fun() ->
               OwnerPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,

               check_poi_for_one_contract(
                 aect_contracts:set_state(make_store(#{<<1>> => <<"v">>}),
                                          make_contract(OwnerPubkey)),
                 fun(C) -> %% Change contract function.
                         ?assertEqual(#{<<1>> => <<"v">>}, %% Assumption for simplicity.
                                      from_store(aect_contracts:state(C))),
                         aect_contracts:set_state(make_store(#{}), C)
                 end)
       end},
      {"PoI for one contract with store in state trees containing multiple contracts with store",
       fun() ->
               Contracts =
                   [aect_contracts:set_state(
                      make_store(#{<<1, X>> => <<"v", X>>}), %% Distinct key per contract.
                      make_contract(<<X:?MINER_PUB_BYTES/unit:8>>))
                    || X <- [1, 2, 3]],
               [check_poi_for_a_contract_among_others(
                  C,
                  [_, _] = Contracts -- [C])
                || C <- Contracts]
       end},
      %% Empty keys are dropped silently, so this doesn't fail verification
      %% {"Serialized contract PoI with empty contract store key fails verification",
      %%  fun() ->
      %%          [check_poi_for_contract_with_invalid_store_with_binary_keys(
      %%             V, #{<<>> => <<"v">>}) || V <- vm_versions()]
      %%  end},
      {"Serialized Solidity contract PoI with invalid contract store key fails verification",
       fun() ->
               IllegalKeys =
                   [<<0, (binary:encode_unsigned(K))/binary>>
                        || K <- lists:seq(0, 3)],
               [check_poi_for_contract_with_invalid_store_with_binary_keys(
                  ?AEVM_01_Solidity_01, #{K => <<"v">>}) || K <- IllegalKeys]
       end},
      {"Serialized Sophia contract PoI with invalid contract store key fails verification",
       fun() ->
               IllegalKeys = [<<3>>, <<4, 17>>, <<16>>],
               [check_poi_for_contract_with_invalid_store_with_binary_keys(
                  ?AEVM_01_Sophia_01,  #{K => <<"v">>}) || K <- IllegalKeys]
       end},
      {"Serialized Sophia contract PoI with invalid contract store key fails verification",
       fun() ->
               IllegalKeys = [<<3>>, <<4, 17>>, <<16>>],
               [check_poi_for_contract_with_invalid_store_with_binary_keys(
                  ?AEVM_02_Sophia_01,  #{K => <<"v">>}) || K <- IllegalKeys]
       end}
    ].

assert_equal_poi(PoIExpect, PoIExpr) ->
    %% The deserialized poi contains a gb_tree, so it is operation
    %% order dependent.  The serialized version is canonical, though.
    ?assertEqual(?TEST_MODULE:serialize_poi(PoIExpect),
                 ?TEST_MODULE:serialize_poi(PoIExpr)).

assert_not_equal_poi(PoIExpect, PoIExpr) ->
    %% The deserialized poi contains a gb_tree, so it is operation
    %% order dependent.  The serialized version is canonical, though.
    ?assertNotEqual(?TEST_MODULE:serialize_poi(PoIExpect),
                    ?TEST_MODULE:serialize_poi(PoIExpr)).

check_poi_for_one_contract(Contract, ChangeContractFun) ->
    ContractKeyF = fun(C) -> aect_contracts:pubkey(C) end,
    InsertContractF =
        fun(Ts, C) ->
                Cs = aec_trees:contracts(Ts),
                aec_trees:set_contracts(
                  Ts, aect_state_tree:insert_contract(C, Cs))
        end,

    check_poi_for_one_object(
      contracts,
      ContractKeyF, ChangeContractFun,
      InsertContractF,
      fun translate_store/1,
      Contract).

check_poi_for_a_contract_among_others(Contract, OtherContracts) ->
    ContractKeyF = fun(C) -> aect_contracts:pubkey(C) end,
    InsertContractF =
        fun(Ts, C) ->
                Cs = aec_trees:contracts(Ts),
                aec_trees:set_contracts(
                  Ts, aect_state_tree:insert_contract(C, Cs))
        end,

    check_poi_for_an_object_among_others(
      contracts,
      ContractKeyF,
      InsertContractF,
      fun translate_store/1,
      Contract, OtherContracts).

check_poi_for_one_object(SubTree,
                         ObjKeyFun, ChangeObjFun,
                         InsertObjFun,
                         EqMeasure,
                         Obj) ->
    Trees0 = aec_test_utils:create_state_tree(),

    %% Add the object to the tree, and see that we can construct a POI
    %% for the correct object.
    Trees1 = InsertObjFun(Trees0, Obj),
    Poi1 = ?TEST_MODULE:new_poi(Trees1),
    ?assertEqual(?TEST_MODULE:hash(Trees1),
                 ?TEST_MODULE:poi_hash(Poi1)),
    ObjKey = ObjKeyFun(Obj),
    {ok, Poi11} = ?TEST_MODULE:add_poi(SubTree, ObjKey, Trees1, Poi1),
    ?assertEqual(?TEST_MODULE:hash(Trees1),
                 ?TEST_MODULE:poi_hash(Poi11)),

    EqMeasureOk = fun({ok, O}) -> {ok, EqMeasure(O)}; (E) -> E end,

    %% Check that the stored object in the POI is the correct one.
    ?assertEqual({ok, EqMeasure(Obj)},
                 EqMeasureOk(aec_trees:lookup_poi(SubTree, ObjKey, Poi11))),

    %% Ensure that we can verify the presence of the object in the
    %% POI.
    ?assertEqual(ok,
                 aec_trees:verify_poi(SubTree, ObjKey, Obj, Poi11)),

    %% Ensure that the POI will fail if we change the object.
    Obj1 = ChangeObjFun(Obj),
    ObjKey = ObjKeyFun(Obj1), %% Hardcoded expectation on function changing object.
    ?assertMatch({error, _},
                 aec_trees:verify_poi(SubTree, ObjKey, Obj1, Poi11)),

    ok.

check_poi_for_an_object_among_others(SubTree,
                                     ObjKeyFun,
                                     InsertObjFun,
                                     EqMeasure, %% Apply before comparing for equality (used for contracts store)
                                     Obj, OtherObjs = [_|_]) ->
    ?assertNot(lists:member(Obj, OtherObjs)), %% Hardcoded expectation on test data.

    ObjKey = ObjKeyFun(Obj),
    Trees0 = aec_test_utils:create_state_tree(),
    Trees1 = InsertObjFun(Trees0, Obj),

    %% Add all other objects to the tree.
    Trees2 =
        lists:foldl(
          fun(OthObj, TreesIn) -> InsertObjFun(TreesIn, OthObj) end,
          Trees1,
          OtherObjs),

    %% Construct a POI for the object.
    Poi2 = ?TEST_MODULE:new_poi(Trees2),
    ?assertEqual(?TEST_MODULE:hash(Trees2),
                 ?TEST_MODULE:poi_hash(Poi2)),
    {ok, Poi21} = ?TEST_MODULE:add_poi(SubTree, ObjKey, Trees2, Poi2),
    ?assertEqual(?TEST_MODULE:hash(Trees2),
                 ?TEST_MODULE:poi_hash(Poi21)),

    EqMeasureOk = fun({ok, O}) -> {ok, EqMeasure(O)}; (E) -> E end,

    %% Check that the stored object in the POI is the correct one.
    ?assertEqual({ok, EqMeasure(Obj)},
                 EqMeasureOk(aec_trees:lookup_poi(SubTree, ObjKey, Poi21))),

    %% Ensure that we can verify the presence of the object in the POI.
    ?assertEqual(ok,
                 aec_trees:verify_poi(SubTree, ObjKey, Obj, Poi21)),

    %% Enrich the POI with the other objects.
    {ok, Poi22} =
        lists:foldl(
          fun(OthObj, {ok, PoiIn}) ->
                  ?TEST_MODULE:add_poi(SubTree, ObjKeyFun(OthObj), Trees2, PoiIn)
          end,
          {ok, Poi21},
          OtherObjs),
    assert_not_equal_poi(Poi21, Poi22),
    ?assertEqual(?TEST_MODULE:hash(Trees2),
                 ?TEST_MODULE:poi_hash(Poi22)),

    %% Check that the stored object in the POI is still the correct one.
    ?assertEqual({ok, EqMeasure(Obj)},
                 EqMeasureOk(aec_trees:lookup_poi(SubTree, ObjKey, Poi22))),

    %% Ensure that we can still verify the presence of the object in the POI.
    ?assertEqual(ok,
                 aec_trees:verify_poi(SubTree, ObjKey, Obj, Poi22)),

    ok.

check_poi_for_contract_with_invalid_store_with_binary_keys(
  VmVersion, InvalidStoreMap) ->
    OwnerPubkey = <<123:?MINER_PUB_BYTES/unit:8>>,

    InvalidStore = make_store(InvalidStoreMap),

    %% Generate contract invalid because of an invalid contract store key.
    ValidContract = make_contract(OwnerPubkey, VmVersion),
    InvalidContract = aect_contracts:internal_set_state(InvalidStore,
                                                        ValidContract),
    ContractPubkey = aect_contracts:pubkey(InvalidContract),
    ?assertNotEqual(ValidContract, InvalidContract), %% Hardcoded expectation on test data.
    ?assertEqual(aect_contracts:pubkey(ValidContract), ContractPubkey), %% Hardcoded expectation on test data.

    %% Include invalid contract in state trees.
    Trees0 = aec_test_utils:create_state_tree(),
    InvalidTrees = aec_trees:set_contracts(
                     Trees0,
                     aect_state_tree:insert_contract(
                       InvalidContract, aec_trees:contracts(Trees0))),

    %% Generate PoI for invalid contract.
    {ok, InvalidPoi0} = ?TEST_MODULE:add_poi(
                           contracts, ContractPubkey,
                           InvalidTrees,
                           ?TEST_MODULE:new_poi(InvalidTrees)),
    InvalidSerializedPoi0 = ?TEST_MODULE:serialize_poi(InvalidPoi0),

    %% Check that PoI verification fails.
    InvalidPoi = ?TEST_MODULE:deserialize_poi(InvalidSerializedPoi0),
    assert_equal_poi(InvalidPoi0, InvalidPoi),
    ValidTrees = aec_trees:set_contracts(
                   Trees0,
                   aect_state_tree:insert_contract(
                     ValidContract, aec_trees:contracts(Trees0))),
    {ok, ValidPoi} = ?TEST_MODULE:add_poi(contracts, ContractPubkey,
                                          ValidTrees,
                                          ?TEST_MODULE:new_poi(ValidTrees)),
    assert_not_equal_poi(ValidPoi, InvalidPoi),
    ?assertEqual({error, bad_proof},
                 aec_trees:verify_poi(contracts, ContractPubkey,
                                      InvalidContract,
                                      InvalidPoi)),
    ?assertEqual({error, bad_proof},
                 aec_trees:verify_poi(contracts, ContractPubkey,
                                      ValidContract,
                                      InvalidPoi)),
    ok.

poi_fields_get(FieldKey, PoiFields) ->
    {_, FieldValue} = lists:keyfind(FieldKey, 1, PoiFields),
    FieldValue.

poi_fields_update_with(FieldKey, Fun, PoiFields) ->
    _ = poi_fields_get(FieldKey, PoiFields), %% Check valid key.
    lists:foldr(
      fun({K, V}, PoiFieldsIn) ->
              NewV = if K =:= FieldKey -> Fun(V); true -> V end,
              [{K, NewV} | PoiFieldsIn]
      end,
      [],
      PoiFields).

make_contract(Owner) ->
    make_contract(Owner, ?AEVM_01_Solidity_01).

make_contract(Owner, VmVersion) ->
    {contract_create_tx, CTx} = aetx:specialize_type(ct_create_tx(Owner, VmVersion)),
    aect_contracts:new(CTx).

make_store(Map) ->
    aect_contracts_store:put_map(Map, aect_contracts_store:new()).

from_store(Store) ->
    aect_contracts_store:contents(Store).

translate_store(C) ->
    Store = aect_contracts:state(C),
    aect_contracts:internal_set_state(from_store(Store), C).

ct_create_tx(Sender, VmVersion) ->
    Spec =
        #{ fee        => 750000
         , owner_id   => aec_id:create(account, Sender)
         , nonce      => 0
         , code       => <<"NOT PROPER BYTE CODE">>
         , vm_version => VmVersion
         , deposit    => 10
         , amount     => 200
         , gas        => 10
         , gas_price  => 1
         , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
         , ttl        => 0
         },
    {ok, Tx} = aect_create_tx:new(Spec),
    Tx.

%% Unused
%% vm_versions() ->
%%     [ ?AEVM_01_Sophia_01
%%     , ?AEVM_01_Solidity_01
%%     ].
