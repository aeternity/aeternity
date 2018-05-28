-module(aec_accounts_trees_tests).

-include_lib("eunit/include/eunit.hrl").

smoke_test() ->
    T0 = aec_accounts_trees:empty(),
    {error, empty} = aec_accounts_trees:root_hash(T0),

    A1 = aec_accounts:new(<<"k1">>, 10),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A1), T1)),
    {ok, H1} = aec_accounts_trees:root_hash(T1),

    A2 = aec_accounts:new(<<"k2">>, 20),
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
    K1 = <<"k1">>,
    A1 = aec_accounts:new(K1, 10),
    K2 = <<"k2">>,
    T0 = aec_accounts_trees:empty(),
    ?assertEqual(none, aec_accounts_trees:lookup(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(K1, T1)),
    ?assertEqual(none, aec_accounts_trees:lookup(K2, T1)),
    ok.

get_test() ->
    K1 = <<"k1">>,
    A1 = aec_accounts:new(K1, 10),
    K2 = <<"k2">>,
    T0 = aec_accounts_trees:empty(),
    ?assertException(_, _, aec_accounts_trees:get(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual(A1, aec_accounts_trees:get(K1, T1)),
    ?assertException(_, _, aec_accounts_trees:get(K2, T1)),
    ok.

get_all_accounts_balances_test() ->
    T0 = aec_accounts_trees:empty(),

    A1 = aec_accounts:new(<<"k1">>, 11),
    A2 = aec_accounts:new(<<"k2">>, 13),

    T1 = aec_accounts_trees:enter(A1, T0),
    T2 = aec_accounts_trees:enter(A2, T1),

    Expected = [{<<"k2">>, 13}, {<<"k1">>, 11}],
    Actual   = aec_accounts_trees:get_all_accounts_balances(T2),
    ?assertEqual(lists:sort(Actual), lists:sort(Expected)).
