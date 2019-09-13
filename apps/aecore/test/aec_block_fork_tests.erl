-module(aec_block_fork_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/include/aecontract.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

apply_minerva_test_() ->
    [{foreach,
      fun() ->
              meck:new(aec_fork_block_settings, [passthrough]),
              ok
      end,
      fun(ok) ->
              meck:unload(aec_fork_block_settings),
              ok
      end,
      [ {"Empty minerva migration does not change balances",
         fun() ->
                 InitialAccounts =
                     [{Alice, BalA},
                      {Bob,   BalB},
                      {Carol, BalC}] = generate_accounts(3),
                 T0 = make_trees(InitialAccounts), % Alice, Bob and Carol
                 meck_minerva_accounts([]), % no accounts migrated
                 T1 = aec_block_fork:apply_minerva(T0),
                 ?assertEqual(aec_trees:hash(T0), aec_trees:hash(T1)),
                 assert_balance(T1, Alice, BalA),
                 assert_balance(T1, Bob,   BalB),
                 assert_balance(T1, Carol, BalC),
                 ok
         end},
        {"Minerva migration changes balances",
         fun() ->
                 [{Alice,  BalA} = A,
                  {Bob,   BalB} = B,
                  {Carol, BalC},
                  {David, _}] = generate_accounts(4),
                 T0 = make_trees([A, B]), % only Alice and Bob are present pre-minerva
                 meck_minerva_accounts([{Alice, DeltaA = 10}, % Alice had migrated more
                                        {Carol, BalC}, % Carol is new
                                        {David, 0}]), % David has a balance of 0
                 T1 = aec_block_fork:apply_minerva(T0),
                 assert_only_accounts_tree_changed(T0, T1),
                 assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
                 assert_balance(T1, Bob,   BalB), % Bob is unchanged
                 assert_balance(T1, Carol, BalC), % Carol is inserted
                 assert_balance(T1, David, 0),    % David is present
                 ok
         end}]}
     || aect_test_utils:latest_protocol_version() >= ?MINERVA_PROTOCOL_VSN ].

apply_lima_test_() ->
    [{foreach,
      fun() ->
              meck:new(aec_fork_block_settings, [passthrough]),
              ok
      end,
      fun(ok) ->
              meck:unload(aec_fork_block_settings),
              ok
      end,
      [ {"Lima migration changes balances if no contracts are given",
         fun() ->
                 [{Alice,  BalA} = A,
                  {Bob,   BalB} = B,
                  {Carol, BalC},
                  {David, _}] = generate_accounts(4),
                 T0 = make_trees([A, B]), % only Alice and Bob are present pre-minerva
                 meck_lima_accounts_and_contracts([{Alice, DeltaA = 10},%% Alice had migrated more
                                                   {Carol, BalC},       %% Carol is new
                                                   {David, 0}],         %% David has a balance of 0
                                                  []                    %% No contracts
                                                 ),
                 T1 = aec_block_fork:apply_lima(T0, tx_env()),
                 assert_only_accounts_tree_changed(T0, T1),
                 assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
                 assert_balance(T1, Bob,   BalB), % Bob is unchanged
                 assert_balance(T1, Carol, BalC), % Carol is inserted
                 assert_balance(T1, David, 0),    % David is present
                 ok
         end},
        {"Lima migration changes balances when contracts _are_ given",
         fun() ->
                 [{Alice,  BalA} = A,
                  {Bob,   BalB} = B,
                  {Carol, BalC},
                  {David, _}] = generate_accounts(4),
                 T0 = make_trees([A, B]), % only Alice and Bob are present pre-minerva
                 ContractAmount1 = 10000,
                 ContractAmount2 = 40000,
                 ContractSpecs = [lima_contract(2, ContractAmount2),
                                  lima_contract(1, ContractAmount1)
                                 ],
                 meck_lima_accounts_and_contracts([{Alice, DeltaA = 10},%% Alice had migrated more
                                                   {Carol, BalC},       %% Carol is new
                                                   {David, 0}],         %% David has a balance of 0
                                                  ContractSpecs
                                                 ),
                 T1 = aec_block_fork:apply_lima(T0, tx_env()),
                 assert_only_accounts_and_contracts_trees_changed(T0, T1),
                 assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
                 assert_balance(T1, Bob,   BalB), % Bob is unchanged
                 assert_balance(T1, Carol, BalC), % Carol is inserted
                 assert_balance(T1, David, 0),    % David is present
                 %% Make sure the locked account balance didn't change.
                 LockedAccount = aec_governance:locked_coins_holder_account(),
                 assert_balance(T0, LockedAccount, 0),
                 assert_balance(T1, LockedAccount, 0),
                 Contracts0 = aec_trees:contracts(T0),
                 Contracts1 = aec_trees:contracts(T1),
                 %% Make sure the contracts didn't exist before
                 [?assert(not aect_state_tree:is_contract(maps:get(pubkey, S), Contracts0))
                  || S <- ContractSpecs],
                 %% Make sure the contracts now are present and have the right amount
                 [begin
                      PK = maps:get(pubkey, S),
                      ?assert(aect_state_tree:is_contract(PK, Contracts1)),
                      Bal = maps:get(amount, S),
                      assert_balance(T1, PK, Bal)
                  end || S <- ContractSpecs],
                 %% Make sure the total coin amount increased properly.
                 Sums0 = aec_trees:sum_total_coin(T0),
                 Sums1 = aec_trees:sum_total_coin(T1),
                 ExpectedContractDelta = ContractAmount1 + ContractAmount2,
                 ExpectedTotalDelta = ExpectedContractDelta + DeltaA + BalC,
                 Total0 = maps:fold(fun(_, X, Acc) -> Acc + X end, 0, Sums0),
                 Total1 = maps:fold(fun(_, X, Acc) -> Acc + X end, 0, Sums1),
                 ?assertEqual(Total0 + ExpectedTotalDelta,
                              Total1),
                 ?assertEqual(maps:get(contracts, Sums0) + ExpectedContractDelta,
                              maps:get(contracts, Sums1)),
                 ok
         end}]}
     || aect_test_utils:latest_protocol_version() >= ?LIMA_PROTOCOL_VSN ].

tx_env() ->
    aetx_env:tx_env(42).

lima_contract(Nonce, Amount) ->
    {ok, Code} = aect_test_utils:compile_contract(?SOPHIA_LIMA_FATE, identity),
    {ok, Contract} = aect_test_utils:read_contract(?SOPHIA_LIMA_FATE, identity),
    {ok, CallData} = aect_test_utils:encode_call_data(?SOPHIA_LIMA_FATE, Contract, <<"init">>, []),
    Owner = aec_governance:locked_coins_holder_account(),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    #{ amount      => Amount
     , vm_version  => ?VM_FATE_SOPHIA_1
     , abi_version => ?ABI_FATE_SOPHIA_1
     , code        => Code
     , call_data   => CallData
     , pubkey      => Pubkey
     , nonce       => Nonce
     }.

load_files_smoke_test_() ->
    [{foreach,
     fun() ->
         meck:new(aec_fork_block_settings, [passthrough]),
         {ok, WorkDir} = file:get_cwd(),
         DataAecoreDir =  WorkDir ++ "/data/aecore/",
         meck:expect(aec_fork_block_settings, accounts_file_name,
            fun(?ROMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".genesis/" ++ AFile;
                (?MINERVA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".minerva/" ++ AFile;
                (?FORTUNA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".fortuna/" ++ AFile;
                (?LIMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".lima/" ++ AFile
            end),
         meck:expect(aec_fork_block_settings, contracts_file_name,
            fun(?LIMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".lima/" ++ CFile
            end),
         ok
     end,
     fun(ok) ->
         meck:unload(aec_fork_block_settings),
         ok
     end,
     [ {"Load the real account and contract files",
        fun() ->
            T0 = make_trees(aec_fork_block_settings:genesis_accounts()),
            T1 = aec_block_fork:apply_minerva(T0),
            T2 = aec_block_fork:apply_fortuna(T1),
            _T3 = aec_block_fork:apply_lima(T2, tx_env()),
            ok
        end}
     ]} || {AFile, CFile} <- [{"accounts.json", "contracts.json"},
                              {"accounts_uat.json", "contracts_uat.json"},
                              {"accounts_test.json", "contracts_test.json"}]
    ].

meck_minerva_accounts(AccountsList) ->
    meck:expect(aec_fork_block_settings, minerva_accounts,
                fun() -> AccountsList end).

meck_lima_accounts_and_contracts(AccountsList, ContractsList) ->
    meck:expect(aec_fork_block_settings, lima_accounts,
                fun() -> AccountsList end),
    meck:expect(aec_fork_block_settings, lima_contracts,
                fun() -> ContractsList end).



assert_only_accounts_tree_changed(Trees0, Trees1) ->
    ?assertEqual(calls_hash(Trees0)     , calls_hash(Trees1)),
    ?assertEqual(channels_hash(Trees0)  , channels_hash(Trees1)),
    ?assertEqual(contracts_hash(Trees0) , contracts_hash(Trees1)),
    ?assertEqual(names_hash(Trees0)     , names_hash(Trees1)),
    ?assertEqual(oracles_hash(Trees0)   , oracles_hash(Trees1)),

    ?assertNotEqual(accounts_hash(Trees0), accounts_hash(Trees1)),
    ok.

assert_only_accounts_and_contracts_trees_changed(Trees0, Trees1) ->
    ?assertEqual(calls_hash(Trees0)     , calls_hash(Trees1)),
    ?assertEqual(channels_hash(Trees0)  , channels_hash(Trees1)),
    ?assertEqual(names_hash(Trees0)     , names_hash(Trees1)),
    ?assertEqual(oracles_hash(Trees0)   , oracles_hash(Trees1)),

    ?assertNotEqual(accounts_hash(Trees0) , accounts_hash(Trees1)),
    ?assertNotEqual(contracts_hash(Trees0), contracts_hash(Trees1)),
    ok.


names_hash(Trees) ->
    aens_state_tree:root_hash(aec_trees:ns(Trees)).

channels_hash(Trees) ->
    aesc_state_tree:root_hash(aec_trees:channels(Trees)).

oracles_hash(Trees) ->
    aeo_state_tree:root_hash(aec_trees:oracles(Trees)).

calls_hash(Trees) ->
    aect_call_state_tree:root_hash(aec_trees:calls(Trees)).

contracts_hash(Trees) ->
    aect_state_tree:root_hash(aec_trees:contracts(Trees)).

accounts_hash(Trees) ->
    aec_accounts_trees:root_hash(aec_trees:accounts(Trees)).

make_trees(AccountsList) ->
    Trees = aec_trees:new_without_backend(),
    AccTrees =
        lists:foldl(
            fun({Pubkey, Balance}, AccumAccTrees) when Balance >= 0 ->
                Account = aec_accounts:new(Pubkey, Balance),
                aec_accounts_trees:enter(Account, AccumAccTrees)
            end,
            aec_trees:accounts(Trees),
            AccountsList ++ [{aec_governance:locked_coins_holder_account(), 0}]),
    aec_trees:set_accounts(Trees, AccTrees).

generate_accounts(Count) ->
    generate_accounts(Count, []).

generate_accounts(Count, Accum) when Count < 1 ->
    Accum;
generate_accounts(Count, Accum) ->
    Pubkey = <<Count:32/unit:8>>,
    generate_accounts(Count - 1 ,[{Pubkey, Count} | Accum]).


assert_balance(Trees, Pubkey, Balance) ->
    Accounts = aec_trees:accounts(Trees),
    Account = aec_accounts_trees:get(Pubkey, Accounts),
    ?assertEqual(Balance, aec_accounts:balance(Account)). 
