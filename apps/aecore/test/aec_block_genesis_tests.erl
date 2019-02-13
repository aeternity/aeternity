-module(aec_block_genesis_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_block_genesis).

%%%===================================================================
%%% Test cases
%%%===================================================================

genesis_block_test_() ->
    {foreach,
     fun() ->
         meck:new(aec_fork_block_settings, [passthrough]),
         ok
     end,
     fun(ok) ->
         meck:unload(aec_fork_block_settings),
         ok
     end,
     [ {"Genesis block is commutative according to the list of preset accounts",
        fun() ->
            [Account1, Account2, Account3] = generate_accounts(3),

            meck_genesis_accounts([Account1, Account2, Account3]),
            Header1 = ?TEST_MODULE:genesis_header(),
            {ok, Header1Hash} = aec_headers:hash_header(Header1),

            % same accounts shuffled
            meck_genesis_accounts([Account3, Account1, Account2]),
            Header2 = ?TEST_MODULE:genesis_header(),
            {ok, Header2Hash} = aec_headers:hash_header(Header2),

            % produced genesis headers are the same
            ?assertEqual(Header1, Header2),
            ?assertEqual(Header1Hash, Header2Hash),
            ok
        end},
       {"Genesis block is idempotent according to the list of preset accounts",
        fun() ->
            [Account1, Account2, Account3] = generate_accounts(3),

            meck_genesis_accounts([Account1, Account2, Account3]),
            Header1 = ?TEST_MODULE:genesis_header(),
            {ok, Header1Hash} = aec_headers:hash_header(Header1),

            % same accounts, one is present twice
            meck_genesis_accounts([Account1, Account2, Account3, Account1]),
            Header2 = ?TEST_MODULE:genesis_header(),
            {ok, Header2Hash} = aec_headers:hash_header(Header2),

            % produced genesis headers are the same
            ?assertEqual(Header1, Header2),
            ?assertEqual(Header1Hash, Header2Hash),
            ok
        end}]}.

preset_trees_test_() ->
    {foreach,
     fun() ->
         meck:new(aec_fork_block_settings, [passthrough]),
         ok
     end,
     fun(ok) ->
         meck:unload(aec_fork_block_settings),
         ok
     end,
     [ {"Trees contain expected accounts",
        fun() ->
            PresetAccounts = generate_accounts(10),

            meck_genesis_accounts(PresetAccounts),
            Trees = ?TEST_MODULE:populated_trees(),
            Accounts = aec_trees:accounts(Trees),
            lists:foreach(
                fun({Pubkey, Balance}) ->
                    Acc = aec_accounts_trees:get(Pubkey, Accounts),
                    ActualBalance = aec_accounts:balance(Acc),
                    ?assertEqual(Balance, ActualBalance)
                end,
                PresetAccounts),
            ok
        end},
       {"Trees do not contain unexpected accounts",
        fun() ->
            PresetAccounts = generate_accounts(10),

            meck_genesis_accounts(PresetAccounts),
            Trees = ?TEST_MODULE:populated_trees(),
            Accounts = aec_trees:accounts(Trees),
            AllAccounts = aec_accounts_trees:get_all_accounts_balances(Accounts),
            UnknownAccounts =
                lists:filter(
                    fun({Pubkey, _Balance}) ->
                        case proplists:get_value(Pubkey, PresetAccounts, not_found) of
                            not_found -> true;
                            _ -> false
                        end
                    end,
                    AllAccounts),
            ?assertEqual(UnknownAccounts, []),
            ok
        end},
       {"Genesis block has expected trees hash",
        fun() ->
            PresetAccounts = generate_accounts(10),

            meck_genesis_accounts(PresetAccounts),
            Header = ?TEST_MODULE:genesis_header(),
            RootHash = aec_headers:root_hash(Header),

            Trees = ?TEST_MODULE:populated_trees(),
            TreesHash = aec_trees:hash(Trees),

            % produced genesis header has expected hash
            ?assertEqual(RootHash, TreesHash),
            ok
        end}]}.

meck_genesis_accounts(AccountsList) ->
    meck:expect(aec_fork_block_settings, genesis_accounts,
                fun() -> AccountsList end).

generate_accounts(Count) ->
    generate_accounts(Count, []).

generate_accounts(Count, Accum) when Count < 1 ->
    Accum;
generate_accounts(Count, Accum) ->
    Pubkey = <<Count:32/unit:8>>,
    generate_accounts(Count - 1 ,[{Pubkey, Count} | Accum]).

