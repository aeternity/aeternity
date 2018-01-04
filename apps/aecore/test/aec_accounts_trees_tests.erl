-module(aec_accounts_trees_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("trees.hrl").

smoke_test() ->
    T0 = aec_accounts_trees:empty(),
    {error, empty} = aec_accounts_trees:root_hash(T0),

    A1 = #account{pubkey = <<"k1">>, balance = 10},
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A1), T1)),
    {ok, H1} = aec_accounts_trees:root_hash(T1),

    A2 = #account{pubkey = <<"k2">>, balance = 20},
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
    A1 = #account{pubkey = K1 = <<"k1">>, balance = 10},
    K2 = <<"k2">>,
    T0 = aec_accounts_trees:empty(),
    ?assertEqual(none, aec_accounts_trees:lookup(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(K1, T1)),
    ?assertEqual(none, aec_accounts_trees:lookup(K2, T1)),
    ok.

get_test() ->
    A1 = #account{pubkey = K1 = <<"k1">>, balance = 10},
    K2 = <<"k2">>,
    T0 = aec_accounts_trees:empty(),
    ?assertException(_, _, aec_accounts_trees:get(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual(A1, aec_accounts_trees:get(K1, T1)),
    ?assertException(_, _, aec_accounts_trees:get(K2, T1)),
    ok.

proof_test() ->
    T0 = aec_accounts_trees:empty(),
    A1 = #account{pubkey = <<"k1">>, balance = 10},
    T1 = aec_accounts_trees:enter(A1, T0),
    {ok, H1} = aec_accounts_trees:root_hash(T1),
    {value_and_proof, A1, P1} =
        aec_accounts_trees:lookup_with_proof(aec_accounts:pubkey(A1), T1),
    %% Assert proof starts from leaf and leads to root hash.
    ?assertEqual({ok, verified}, aec_accounts_trees:verify_proof(A1, H1, P1)),
    ok.

get_all_accounts_balances_test() ->
    T0 = aec_accounts_trees:empty(),

    A1 = #account{pubkey = <<"k1">>, balance = 11},
    A2 = #account{pubkey = <<"k2">>, balance = 13},

    T1 = aec_accounts_trees:enter(A1, T0),
    T2 = aec_accounts_trees:enter(A2, T1),

    ?assertEqual([{<<"k2">>, 13}, {<<"k1">>, 11}],
                 aec_accounts_trees:get_all_accounts_balances(T2)).
