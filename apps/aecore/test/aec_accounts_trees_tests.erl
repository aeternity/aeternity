-module(aec_accounts_trees_tests).

-include_lib("eunit/include/eunit.hrl").

smoke_test() ->
    T0 = aec_accounts_trees:empty(),
    {error, empty} = aec_accounts_trees:root_hash(T0),

    A1 = aec_accounts:new(<<"_______________k1_______________">>, 10),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A1), T1)),
    {ok, H1} = aec_accounts_trees:root_hash(T1),

    A2 = aec_accounts:new(<<"_______________k2_______________">>, 20),
    T2 = aec_accounts_trees:enter(A2, T1),
    ?assertEqual({value, A1},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A1), T2)),
    ?assertEqual({value, A2},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A2), T2)),
    {ok, H2} = aec_accounts_trees:root_hash(T2),

    %% Assert root hash summarizes content.
    ?assertNotEqual(H1, H2),
    ok.

lookup_test() ->
    K1 = <<"_______________k1_______________">>,
    A1 = aec_accounts:new(K1, 10),
    K2 = <<"_______________k2_______________">>,
    T0 = aec_accounts_trees:empty(),
    ?assertEqual(none, aec_accounts_trees:lookup(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(K1, T1)),
    ?assertEqual(none, aec_accounts_trees:lookup(K2, T1)),
    ok.

get_test() ->
    K1 = <<"_______________k1_______________">>,
    A1 = aec_accounts:new(K1, 10),
    K2 = <<"_______________k2_______________">>,
    T0 = aec_accounts_trees:empty(),
    ?assertException(_, _, aec_accounts_trees:get(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual(A1, aec_accounts_trees:get(K1, T1)),
    ?assertException(_, _, aec_accounts_trees:get(K2, T1)),
    ok.

get_all_accounts_balances_test() ->
    T0 = aec_accounts_trees:empty(),

    A1 = aec_accounts:new(<<"_______________k1_______________">>, 11),
    A2 = aec_accounts:new(<<"_______________k2_______________">>, 13),

    T1 = aec_accounts_trees:enter(A1, T0),
    T2 = aec_accounts_trees:enter(A2, T1),

    Expected = [{<<"_______________k2_______________">>, 13},
                {<<"_______________k1_______________">>, 11}],
    Actual   = aec_accounts_trees:get_all_accounts_balances(T2),
    ?assertEqual(lists:sort(Actual), lists:sort(Expected)).

account_for_locking_test() ->
    HolderPubKey = aec_governance:locked_coins_holder_account(),
    T0 = aec_accounts_trees:empty(),

    %% not present in empty tree
    ?assertException(_, _, aec_accounts_trees:get(HolderPubKey, T0)),

    GetHolderBalance =
        fun(Tree) ->
            Acc = aec_accounts_trees:get(HolderPubKey, Tree),
            _Bal = aec_accounts:balance(Acc)
        end,

    Amt1 = 1,
    Amt2 = 2,
    Amt3 = 3,

    %% adding coins for the first time creates the account
    T1 = aec_accounts_trees:lock_coins(Amt1, T0),
    Bal1 = GetHolderBalance(T1),
    ?assertEqual(Bal1, Amt1),

    %% adding more tokens increments locked coins' amount
    T2 = aec_accounts_trees:lock_coins(Amt2, T1),
    Bal2 = GetHolderBalance(T2),
    ?assertEqual(Bal2, Amt1 + Amt2),

    %% adding more tokens increments locked coins' amount even more
    T3 = aec_accounts_trees:lock_coins(Amt3, T2),
    Bal3 = GetHolderBalance(T3),
    ?assertEqual(Bal3, Amt1 + Amt2 + Amt3),
    ok.
    
% channels' rely on accounts with a dict backend being reproducable with
% only the latest state
trunc_test() ->
    T0 = aec_accounts_trees:empty(),

    K1 = <<"_______________k1_______________">>,
    K2 = <<"_______________k2_______________">>,

    A10 = aec_accounts:new(K1, 11),
    A11 = aec_accounts:new(K1, 5),
    A2  = aec_accounts:new(K2, 13),

    T1  = aec_accounts_trees:enter(A10, T0),
    T2  = aec_accounts_trees:enter(A2, T1),
    T30 = aec_accounts_trees:enter(A11, T2),
    {ok, T3Hash} = aec_accounts_trees:root_hash(T30),

    CleanT1 = aec_accounts_trees:enter(A11, T0),
    CleanT2 = aec_accounts_trees:enter(A2, CleanT1),
    {ok, CleanT2Hash} = aec_accounts_trees:root_hash(CleanT2),
    ?assertEqual(T3Hash, CleanT2Hash),

    T11  = aec_accounts_trees:delete(K2, T2),
    {ok, T11Hash} = aec_accounts_trees:root_hash(T11),
    {ok, T1Hash} = aec_accounts_trees:root_hash(T1),
    ?assertEqual(T1Hash, T11Hash).

