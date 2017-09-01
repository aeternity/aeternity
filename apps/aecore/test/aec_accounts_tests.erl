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
    ?debugFmt("~p~n", [P1]),
    %% Assert proof leads to root hash.
    ?assertEqual(H1, gb_merkle_trees_merkle_fold(P1)),
    %% TODO Assert proof starts from leaf / serialized value.

    ok.

-define(GB_MERKLE_TREES_HASH(X), crypto:hash(sha256, X)). %% XXX https://github.com/KrzysiekJ/gb_merkle_trees/blob/v0.2.0/src/gb_merkle_trees.erl#L50-L53
%% XXX Why not exported? See https://github.com/KrzysiekJ/gb_merkle_trees/blob/v0.2.0/src/gb_merkle_trees.erl#L415-L421 and https://github.com/KrzysiekJ/gb_merkle_trees/blob/v0.2.0/src/gb_merkle_trees.erl#L415-L421
gb_merkle_trees_merkle_fold({Left, Right}) ->
    LeftHash = gb_merkle_trees_merkle_fold(Left),
    RightHash = gb_merkle_trees_merkle_fold(Right),
    ?GB_MERKLE_TREES_HASH(<<LeftHash/binary, RightHash/binary>>);
gb_merkle_trees_merkle_fold(Hash) ->
    Hash.
