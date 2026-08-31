%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Tests for aect_contracts_store:subtree_within_bytes/3. Accepting, it
%%%    must be indistinguishable from subtree/2 plus summing the result, or
%%%    the gas its callers charge moves. Refusing, it must return nothing.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_contracts_store_tests).

-include_lib("eunit/include/eunit.hrl").

%% Shaped like a real map data key: aefa_stores' ?STORE_MAP_PREFIX and a raw id.
-define(PREFIX, <<1, 0, 0, 0, 7>>).
-define(ENTRIES, 20).
-define(PLENTY, 1000000).

%%%===================================================================
%%% Accepting path: identical to subtree/2
%%%===================================================================

matches_subtree_test() ->
    Store = store_with_entries(?ENTRIES),
    Expect = aect_contracts_store:subtree(?PREFIX, Store),
    ?assertEqual(?ENTRIES, maps:size(Expect)),
    ?assertEqual({ok, Expect, bytes(Expect)},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store, ?PLENTY)).

matches_subtree_through_the_write_cache_test() ->
    Store0 = store_with_entries(?ENTRIES),
    Store1 = aect_contracts_store:put(key(3), <<"short">>, Store0),
    Store2 = aect_contracts_store:put(key(9999), <<"cache-only">>, Store1),
    Store3 = aect_contracts_store:remove(key(4), Store2),
    Expect = aect_contracts_store:subtree(?PREFIX, Store3),
    ?assertEqual(?ENTRIES + 1, maps:size(Expect)),
    ?assertEqual({ok, Expect, bytes(Expect)},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store3, ?PLENTY)).

%% Read-cache hit with a non-empty write cache: the cache must still shadow the
%% cached subtree, and the byte total must still be the merged one.
read_cache_hit_merges_the_write_cache_test() ->
    Store0 = store_with_entries(?ENTRIES),
    {_Subtree, Store1} = aect_contracts_store:subtree_w_cache(?PREFIX, Store0),
    Store2 = aect_contracts_store:put(key(3), <<"short">>, Store1),
    Store3 = aect_contracts_store:put(key(9999), <<"cache-only">>, Store2),
    Expect = aect_contracts_store:subtree(?PREFIX, Store3),
    Bytes = bytes(Expect),
    ?assertEqual(?ENTRIES + 1, maps:size(Expect)),
    ?assertEqual({ok, Expect, Bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store3, Bytes)),
    ?assertEqual({error, too_many_bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store3, Bytes - 1)).

matches_subtree_with_read_cache_populated_test() ->
    Store0 = store_with_entries(?ENTRIES),
    {_Subtree, Store1} = aect_contracts_store:subtree_w_cache(?PREFIX, Store0),
    Expect = aect_contracts_store:subtree(?PREFIX, Store1),
    Bytes = bytes(Expect),
    ?assertEqual({ok, Expect, Bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store1, ?PLENTY)),
    ?assertEqual({error, too_many_bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store1, Bytes - 1)).

ignores_keys_outside_the_prefix_test() ->
    Store = store_with_entries(?ENTRIES),
    Expect = aect_contracts_store:subtree(?PREFIX, Store),
    ?assertEqual({ok, Expect, bytes(Expect)},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store, bytes(Expect))).

%% A prefix with no node of its own is not a subtree; both agree it is empty.
absent_subtree_test() ->
    Store = store_with_entries(?ENTRIES),
    Absent = <<1, 0, 0, 0, 8>>,
    ?assertEqual(#{}, aect_contracts_store:subtree(Absent, Store)),
    ?assertEqual({ok, #{}, 0},
                 aect_contracts_store:subtree_within_bytes(Absent, Store, 0)).

%%%===================================================================
%%% Refusing path
%%%===================================================================

boundary_is_exact_test() ->
    Store = store_with_entries(?ENTRIES),
    Full = aect_contracts_store:subtree(?PREFIX, Store),
    Bytes = bytes(Full),
    ?assertEqual({ok, Full, Bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store, Bytes)),
    ?assertEqual({error, too_many_bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store, Bytes - 1)),
    ?assertEqual({error, too_many_bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store, 0)).

refuses_on_the_write_cache_alone_test() ->
    Store0 = store_with_entries(?ENTRIES),
    Store1 = aect_contracts_store:put(key(9999), binary:copy(<<$c>>, 400), Store0),
    ?assertEqual({error, too_many_bytes},
                 aect_contracts_store:subtree_within_bytes(?PREFIX, Store1, 100)).

%%%===================================================================
%%% Helpers
%%%===================================================================

key(I) -> <<?PREFIX/binary, (integer_to_binary(I))/binary>>.

value(I) -> <<(integer_to_binary(I))/binary, (binary:copy(<<$v>>, 30))/binary>>.

bytes(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc + byte_size(K) + byte_size(V) end, 0, Map).

store_with_entries(N) ->
    Tree0 = lists:foldl(fun(I, T) -> aeu_mtrees:insert(key(I), value(I), T) end,
                        aeu_mtrees:empty(), lists:seq(1, N)),
    %% Marker node as copy_map/5 writes it; without it read_only_subtree/2
    %% finds no subtree at all.
    Tree1 = aeu_mtrees:insert(?PREFIX, <<0>>, Tree0),
    %% A large key outside the prefix, which must not count against the bound.
    Tree2 = aeu_mtrees:insert(<<0, 1>>, binary:copy(<<$z>>, 500), Tree1),
    aect_contracts_store:new(Tree2).
