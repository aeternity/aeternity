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
                  genesis_accounts_file_missing).

minerva_accounts_test_() ->
    release_based(?ROOT_DIR ++ "/.minerva",
                  fun ?TEST_MODULE:minerva_accounts/0,
                  minerva_accounts_file_missing).

release_based(Dir, ReadAccountsFun, MissingErr) ->
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
            delete_file(Dir),
            File = filename(Dir),
            ?assertError({MissingErr, File}, ReadAccountsFun()),
            ok
        end}
     ]}.

expect_accounts(Dir, B) when is_binary(B) ->
    file:write_file(filename(Dir), B, [binary]);
expect_accounts(Dir, L0) when is_list(L0) ->
    L =
        lists:map(
            fun({PK, Amt}) ->
                {aeser_api_encoder:encode(account_pubkey, PK), Amt}
            end,
            L0),
    expect_accounts(Dir, jsx:encode(L)).

delete_file(Dir) ->
    case file:delete(filename(Dir)) of
        ok -> ok;
        {error, enoent} -> ok
    end.

delete_dir(Dir) ->
    ok = delete_file(Dir),
    ok = file:del_dir(Dir).

filename(Dir) ->
    Dir ++ "/accounts_test.json".
