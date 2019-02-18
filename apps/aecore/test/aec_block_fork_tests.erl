-module(aec_block_fork_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

apply_minerva_test_() ->
    {foreach,
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
            assert_only_accounts_tree_changed(T0, T1),
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
            meck_minerva_accounts([{Alice, DeltaA = 10}, % Alice had migrared more
                                   {Carol, BalC}, % Carol is new
                                   {David, 0}]), % David has a balance of 0
            T1 = aec_block_fork:apply_minerva(T0),
            assert_only_accounts_tree_changed(T0, T1),
            assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
            assert_balance(T1, Bob,   BalB), % Bob is unchanged
            assert_balance(T1, Carol, BalC), % Carol is inserted
            assert_balance(T1, David, 0),    % David is present
            ok
        end}
     ]}.

meck_minerva_accounts(AccountsList) ->
    meck:expect(aec_fork_block_settings, minerva_accounts,
                fun() -> AccountsList end).

assert_only_accounts_tree_changed(Trees0, Trees1) ->
    Assert =
        fun(GetTree, GetTreeHash) ->
            Tree0 = GetTree(Trees0),
            Tree1 = GetTree(Trees1),
            ?assertEqual(GetTreeHash(Tree0), GetTreeHash(Tree1))
        end,
    %% ensure channels are unchanged
    Assert(fun aec_trees:channels/1, fun aesc_state_tree:root_hash/1),

    %% ensure name service is unchanged
    Assert(fun aec_trees:ns/1, fun aens_state_tree:root_hash/1),

    %% ensure oracles are unchanged
    Assert(fun aec_trees:oracles/1, fun aeo_state_tree:root_hash/1),

    %% ensure calls are unchanged
    Assert(fun aec_trees:calls/1, fun aect_call_state_tree:root_hash/1),

    %% ensure contracts are unchanged
    Assert(fun aec_trees:contracts/1, fun aect_state_tree:root_hash/1),
    ok.

make_trees(AccountsList) ->
    Trees = aec_trees:new_without_backend(),
    AccTrees =
        lists:foldl(
            fun({Pubkey, Balance}, AccumAccTrees) when Balance > 0 ->
                Account = aec_accounts:new(Pubkey, Balance),
                aec_accounts_trees:enter(Account, AccumAccTrees)
            end,
            aec_trees:accounts(Trees),
            AccountsList),
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
