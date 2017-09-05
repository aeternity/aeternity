-module(aec_accounts_tests).

-include_lib("eunit/include/eunit.hrl").
-include("trees.hrl").

smoke_test() ->
    {ok, T0} = aec_accounts:empty(),
    {error, empty} = aec_accounts:root_hash(T0),

    A1 = #account{pubkey = <<"k1">>, balance = 10},
    {ok, T1} = aec_accounts:put(A1, T0),
    ?assertEqual({ok, A1}, aec_accounts:get(A1#account.pubkey, T1)),
    {ok, H1} = aec_accounts:root_hash(T1),

    A2 = #account{pubkey = <<"k2">>, balance = 20},
    {ok, T2} = aec_accounts:put(A2, T1),
    ?assertEqual({ok, A1}, aec_accounts:get(A1#account.pubkey, T2)),
    ?assertEqual({ok, A2}, aec_accounts:get(A2#account.pubkey, T2)),
    {ok, H2} = aec_accounts:root_hash(T2),

    %% Assert root hash summarizes content.
    ?assertNotEqual(H1, H2),
    ok.

proof_test() ->
    {ok, T0} = aec_accounts:empty(),
    A1 = #account{pubkey = <<"k1">>, balance = 10},
    {ok, T1} = aec_accounts:put(A1, T0),
    {ok, H1} = aec_accounts:root_hash(T1),
    {ok, {A1, P1}} = aec_accounts:get_with_proof(A1#account.pubkey, T1),
    %% Assert proof starts from leaf and leads to root hash.
    ?assertEqual({ok, verified}, aec_accounts:verify_proof(A1, H1, P1)),
    ok.
