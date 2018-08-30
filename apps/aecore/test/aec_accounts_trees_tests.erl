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

