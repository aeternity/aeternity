-module(aec_genesis_block_settings_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_genesis_block_settings).
-define(ROOT_DIR, "/tmp").
-define(DIR, ?ROOT_DIR ++ "/.genesis").
-define(FILENAME, ?DIR ++ "/accounts.json").

%%%===================================================================
%%% Test cases
%%%===================================================================

preset_accounts_test_() ->
    {foreach,
     fun() ->
         file:make_dir(?DIR),
         meck:new(aeu_env, [passthrough]),
         meck:expect(aeu_env, data_dir, fun(aecore) -> ?ROOT_DIR end),
         ok
     end,
     fun(ok) ->
         delete_dir(),
         meck:unload(aeu_env),
         ok
     end,
     [ {"Preset accounts parsing: broken file",
        fun() ->
            Address1 = aec_base58c:encode(account_pubkey, <<42:32/unit:8>>),
            Address2 = aec_base58c:encode(account_pubkey, <<43:32/unit:8>>),
            %% empty file
            expect_accounts(<<"">>),
            ?assertError(invalid_accounts_json, ?TEST_MODULE:preset_accounts()),
            %% broken json
            expect_accounts(<<"{">>),
            ?assertError(invalid_accounts_json, ?TEST_MODULE:preset_accounts()),
            %% broken json
            expect_accounts(<<"{\"", Address1/binary, "\":1,\"", Address2/binary, "\":2">>),
            ?assertError(invalid_accounts_json, ?TEST_MODULE:preset_accounts()),
            %% not json at all
            expect_accounts(<<"Hejsan svejsan">>),
            ?assertError(invalid_accounts_json, ?TEST_MODULE:preset_accounts()),
            ok
        end},
       {"Preset accounts parsing: empty object",
        fun() ->
            expect_accounts(<<"{}">>),
            ?assertEqual([], ?TEST_MODULE:preset_accounts()),
            expect_accounts(<<"{ }">>),
            ?assertEqual([], ?TEST_MODULE:preset_accounts()),
            ok
        end},

       {"Preset accounts parsing: a preset account",
        fun() ->
            Pubkey = <<42:32/unit:8>>,
            expect_accounts([{Pubkey, 10}]),
            ?assertEqual([{Pubkey, 10}], ?TEST_MODULE:preset_accounts()),
            ok
        end},
       {"Preset accounts parsing: deterministic ordering",
        fun() ->
            Accounts =
                [{<<2:32/unit:8>>, 10},
                 {<<1:32/unit:8>>, 20},
                 {<<3:32/unit:8>>, 42}],
            AccountsOrdered = lists:keysort(1, Accounts),
            expect_accounts(Accounts),
            ?assertEqual(AccountsOrdered, ?TEST_MODULE:preset_accounts()),
            ok
        end},
       {"Preset accounts parsing: preset accounts file missing",
        fun() ->
            delete_file(),
            ?assertError({genesis_accounts_file_missing, ?FILENAME}, ?TEST_MODULE:preset_accounts()),
            ok
        end}

     ]}.

expect_accounts(B) when is_binary(B) ->
    file:write_file(?FILENAME, B, [binary]);
expect_accounts(L0) when is_list(L0) ->
    L =
        lists:map(
            fun({PK, Amt}) ->
                {aec_base58c:encode(account_pubkey, PK), Amt}
            end,
            L0),
    expect_accounts(jsx:encode(L)).

delete_file() ->
    case file:delete(?FILENAME) of
        ok -> ok;
        {error, enoent} -> ok
    end.

delete_dir() ->
    ok = delete_file(),
    ok = file:del_dir(?DIR).

