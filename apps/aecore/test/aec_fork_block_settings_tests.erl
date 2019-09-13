-module(aec_fork_block_settings_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_fork_block_settings).
-define(ROOT_DIR, "/tmp").

%%%===================================================================
%%% Test cases
%%%===================================================================

genesis_accounts_test_() ->
    release_based(?ROOT_DIR ++ "/.genesis",
                  fun ?TEST_MODULE:genesis_accounts/0,
                  none,
                  none,
                  genesis_accounts_file_missing).

minerva_accounts_test_() ->
    release_based(?ROOT_DIR ++ "/.minerva",
                  fun ?TEST_MODULE:minerva_accounts/0,
                  none,
                  none,
                  minerva_accounts_file_missing).

fortuna_accounts_test_() ->
    release_based(?ROOT_DIR ++ "/.fortuna",
                  fun ?TEST_MODULE:fortuna_accounts/0,
                  none,
                  none,
                  fortuna_accounts_file_missing).

lima_accounts_test_() ->
    release_based(?ROOT_DIR ++ "/.lima",
                  fun ?TEST_MODULE:lima_accounts/0,
                  fun ?TEST_MODULE:lima_contracts/0,
                  lima_contracts_file_missing,
                  lima_accounts_file_missing).

release_based(Dir, ReadAccountsFun, ReadContractsFun, CMissingErr, AMissingErr) ->
    {foreach,
     fun() ->
         file:make_dir(Dir),
         meck:new(aeu_env, [passthrough]),
         meck:expect(aeu_env, data_dir, fun(aecore) -> ?ROOT_DIR end),
         ok
     end,
     fun(ok) ->
         delete_dir(Dir),
         meck:unload(aeu_env),
         ok
     end,
     [ {"Preset accounts parsing: broken file",
        fun() ->
            Address1 = aeser_api_encoder:encode(account_pubkey, <<42:32/unit:8>>),
            Address2 = aeser_api_encoder:encode(account_pubkey, <<43:32/unit:8>>),
            %% empty file
            expect_accounts(Dir, <<"">>),
            ?assertError(invalid_accounts_json, ReadAccountsFun()),
            %% broken json
            expect_accounts(Dir, <<"{">>),
            ?assertError(invalid_accounts_json, ReadAccountsFun()),
            %% broken json
            expect_accounts(Dir, <<"{\"", Address1/binary, "\":1,\"", Address2/binary, "\":2">>),
            ?assertError(invalid_accounts_json, ReadAccountsFun()),
            %% not json at all
            expect_accounts(Dir, <<"Hejsan svejsan">>),
            ?assertError(invalid_accounts_json, ReadAccountsFun()),
            ok
        end},
       {"Preset accounts parsing: empty object",
        fun() ->
            expect_accounts(Dir, <<"{}">>),
            ?assertEqual([], ReadAccountsFun()),
            expect_accounts(Dir, <<"{ }">>),
            ?assertEqual([], ReadAccountsFun()),
            ok
        end},

       {"Preset accounts parsing: a preset account",
        fun() ->
            Pubkey = <<42:32/unit:8>>,
            expect_accounts(Dir, [{Pubkey, 10}]),
            ?assertEqual([{Pubkey, 10}], ReadAccountsFun()),
            ok
        end},
       {"Preset accounts parsing: deterministic ordering",
        fun() ->
            Accounts =
                [{<<2:32/unit:8>>, 10},
                 {<<1:32/unit:8>>, 20},
                 {<<3:32/unit:8>>, 42}],
            AccountsOrdered = lists:keysort(1, Accounts),
            expect_accounts(Dir, Accounts),
            ?assertEqual(AccountsOrdered, ReadAccountsFun()),
            ok
        end},
       {"Preset accounts parsing: preset accounts file missing",
        fun() ->
            delete_accounts_file(Dir),
            File = accounts_filename(Dir),
            ?assertError({AMissingErr, File}, ReadAccountsFun()),
            ok
        end}]
     ++
       case ReadContractsFun =:= none of
           true -> [];
           false ->
               [{"Preset contracts parsing: broken file",
                 fun() ->
                     %% empty file
                     expect_contracts(Dir, <<"">>),
                     ?assertError(invalid_contracts_json, ReadContractsFun()),
                     %% broken json
                     expect_contracts(Dir, <<"{">>),
                     ?assertError(invalid_contracts_json, ReadContractsFun()),
                     %% not json at all
                     expect_contracts(Dir, <<"Hejsan svejsan">>),
                     ?assertError(invalid_contracts_json, ReadContractsFun()),
                     ok
                 end},
                {"Preset contracts parsing: empty object",
                 fun() ->
                     expect_contracts(Dir, <<"{}">>),
                     ?assertEqual([], ReadContractsFun()),
                     expect_contracts(Dir, <<"{ }">>),
                     ?assertEqual([], ReadContractsFun()),
                     ok
                 end},
                {"Preset contracts parsing: preset contracts file missing",
                 fun() ->
                     delete_contracts_file(Dir),
                     File = contracts_filename(Dir),
                     ?assertError({CMissingErr, File}, ReadContractsFun()),
                     okma
                 end},
                {"Preset contracts parsing: Format check",
                 fun() ->
                     expect_contracts(Dir, well_formed_contract_spec()),
                     ?assertMatch([#{}], ReadContractsFun()),
                     [begin
                          expect_contracts(Dir, S),
                          ?assertError({invalid_spec, _}, ReadContractsFun())
                      end || S <- ill_formed_contract_specs()],
                     ok
                 end}
               ]
       end
    }.

expect_accounts(Dir, B) when is_binary(B) ->
    file:write_file(accounts_filename(Dir), B, [binary]);
expect_accounts(Dir, L0) when is_list(L0) ->
    L =
        lists:map(
            fun({PK, Amt}) ->
                {aeser_api_encoder:encode(account_pubkey, PK), Amt}
            end,
            L0),
    expect_accounts(Dir, jsx:encode(L)).

expect_contracts(Dir, B) when is_binary(B) ->
    file:write_file(contracts_filename(Dir), B, [binary]).


delete_accounts_file(Dir) ->
    case file:delete(accounts_filename(Dir)) of
        ok -> ok;
        {error, enoent} -> ok
    end.

delete_contracts_file(Dir) ->
    case file:delete(contracts_filename(Dir)) of
        ok -> ok;
        {error, enoent} -> ok
    end.

delete_dir(Dir) ->
    ok = delete_accounts_file(Dir),
    ok = delete_contracts_file(Dir),
    ok = file:del_dir(Dir).

accounts_filename(Dir) ->
    Dir ++ "/accounts_test.json".

contracts_filename(Dir) ->
    Dir ++ "/contracts_test.json".

well_formed_contract_spec() ->
    <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>.

ill_formed_contract_specs() ->
    [%% Bad pubkey
     <<"{\"ct_1111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Bad amount
     <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : \"42\",
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Bad vm_version
     <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : \"5\",
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Bad abi_version
    <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : \"3\",
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Bad nonce
    <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : \"1\",
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Bad code
    <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Bad calldata
     <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Missing spec field
     <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>,
     %% Extra spec field
     <<"{\"ct_11111111111111111111111111111115rHyByZ\" :
          { \"amount\" : 42,
            \"vm_version\" : 5,
            \"abi_version\" : 3,
            \"nonce\"       : 1,
            \"nonce_foo\"   : 1,
            \"code\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",
            \"call_data\" : \"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"
         }
      }">>
    ].
