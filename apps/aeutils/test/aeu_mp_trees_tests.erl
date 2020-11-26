%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    Unit tests for Merkle Patricia Trees
%%% @end
%%%=============================================================================
-module(aeu_mp_trees_tests).

-include_lib("eunit/include/eunit.hrl").

%% For internal functional db
-behavior(aeu_mp_trees_db).
-export([ mpt_db_drop_cache/1
        , mpt_db_list_cache/1
        , mpt_db_get/2
        , mpt_db_put/3
        ]).


basic_test_() ->
    [ {"Put", fun test_put/0}
    , {"Lookup", fun test_lookup/0}
    , {"Delete one", fun test_delete_one_by_one/0}
    , {"Delete all", fun test_delete_all/0}
    , {"Missing lookups", fun test_lookup_missing/0}
    , {"Put rlp values", fun test_put_rlp_vals/0}
    , {"Collapse extension", fun test_collapse_ext/0}
    , {"Iterator test", fun test_iterator/0}
    , {"Iterator depth test", fun test_iterator_depth/0}
    , {"Iterator from non-existing", fun test_iterator_non_existing/0}
    , {"Iterator of subtree", fun test_iterator_prefix/0}
    , {"Create subtrees", fun test_subtrees/0}
    ].

hash_test_() ->
    [ {"Reversed put", fun test_reversed_put/0}
    , {"Put and delete", fun test_put_and_delete/0}
    , {"Merge of ext", fun test_ext_merge_of_paths/0}
    ].

extension_test_() ->
    [ {"One step", fun test_extension_one_step/0}
    , {"Two step", fun test_extension_two_step/0}
    ].

proof_test_() ->
    [ {"Create and verify proofs", fun test_proofs/0}
    , {"Check that bogus proofs don't work", fun test_bogus_proofs/0}
    ].

reachability_test_() ->
    [ {"Visit reachable hashes", fun test_visit_reachable/0}
    , {"Commit reachable cache", {timeout, 30, fun test_commit_reachable/0}}
    , {"GC of cache",            {timeout, 30, fun test_gc_cache/0}}
    ].

%%%=============================================================================
%%% Basic tests

test_put() ->
    {_Tree,_Vals} = gen_mp_tree({23, 123534, 345345}, 1000),
    ok.

test_lookup() ->
    {Tree, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    [?assertEqual(Y, aeu_mp_trees:get(X, Tree)) || {X, Y} <- Vals],
    ok.

test_lookup_missing() ->
    Val = <<0,0,0,0>>,
    K1 = <<0:4, 0:4>>,
    K2 = <<0:4, 1:4, 0:4>>,
    K3 = <<0:4, 0:4, 0:4, 0:4, 0:4>>,
    K4 = <<0:4, 0:4, 0:4, 0:4, 1:4>>,
    Keys = [K1, K2, K3, K4],
    NotPresentLong = [<<K/bits, 15:4>> || K <- Keys],
    NotPresentShort = [ begin S = bit_size(Key) - 4,
                              <<X:S/bits, _:4>> = Key,
                              X
                        end || Key <- Keys],
    T = insert_vals([{K1,Val}, {K2, Val}, {K3, Val}, {K4, Val}],
                    aeu_mp_trees:new()),
    [?assertEqual(<<>>, aeu_mp_trees:get(Key, T))
     || Key <- NotPresentShort ++ NotPresentLong].

test_delete_one_by_one() ->
    {Tree, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    [begin
         T = aeu_mp_trees:delete(X, Tree),
         ?assertEqual(<<>>, aeu_mp_trees:get(X, T))
     end
     || {X,_Y} <- Vals],
    ok.

test_delete_all() ->
    {Tree, Vals} = gen_mp_tree({123, 1234, 1234123}, 1000),
    Tree1 = test_delete_all(Vals, Tree),
    ?assertEqual(aeu_mp_trees:root_hash(aeu_mp_trees:new()),
                 aeu_mp_trees:root_hash(Tree1)).

test_delete_all([{X, Y}|Left], Tree) ->
    ?assertEqual(Y, aeu_mp_trees:get(X, Tree)),
    Tree1 = aeu_mp_trees:delete(X, Tree),
    ?assertEqual(<<>>, aeu_mp_trees:get(X, Tree1)),
    ?assertNotEqual(aeu_mp_trees:root_hash(Tree),
                    aeu_mp_trees:root_hash(Tree1)),
    test_delete_all(Left, Tree1);
test_delete_all([], Tree) ->
    Tree.

test_put_rlp_vals() ->
    apply_ops([{put, <<7:4>>, [<<0>>]},
               {put, <<7:4, 0:4>>, <<0>>}
              ], aeu_mp_trees:new()).

test_collapse_ext() ->
    T = apply_ops([{put, <<15:4>>, <<0>>},
                   {put, <<15:4, 13:4, 0:4>>, <<0>>},
                   {put, <<15:4, 13:4>>, <<0>>}
                  ], aeu_mp_trees:new()),
    aeu_mp_trees:pp(T),
    apply_ops([ {delete, <<15:4>>},
                {delete, <<15:4, 13:4>>}
              ], T).

test_iterator() ->
    {Tree, Vals} = gen_mp_tree({6234, 5234, 9273}, 1000),
    Iterator = aeu_mp_trees:iterator(Tree),
    Sorted = lists:keysort(1, Vals),
    test_iterator(Iterator, Tree, Sorted).

test_iterator(Iterator, Tree, [{Key, Val}|Left]) ->
    {IKey, IVal, Iterator1} = aeu_mp_trees:iterator_next(Iterator),
    ?assertEqual(IKey, Key),
    ?assertEqual(IVal, Val),
    case Left of
        [{NextKey, NextVal}|_] ->
            %% Iterator starting from key skips key.
            NewIterator = aeu_mp_trees:iterator_from(Key, Tree),
            ?assertMatch({NextKey, NextVal, _},
                         aeu_mp_trees:iterator_next(NewIterator));
        [] ->
            ok
    end,
    test_iterator(Iterator1, Tree, Left);
test_iterator(Iterator,_Tree, []) ->
    ?assertEqual('$end_of_table', aeu_mp_trees:iterator_next(Iterator)),
    ok.

test_iterator_depth() ->
    rand:seed(exs1024s, {142343, 132, 4113}),
    Val = <<"Hello">>,
    ShortKeys = [random_hexstring(10) || _ <- lists:seq(1,100)],
    MiddleKeys = [random_hexstring(11) || _ <- lists:seq(1,100)],
    LongKeys = [random_hexstring(12) || _ <- lists:seq(1,100)],
    Vals = [{Key, Val} || Key <- ShortKeys ++ MiddleKeys ++ LongKeys],
    Sorted = lists:sort(Vals),
    ?assertEqual(Sorted, lists:ukeysort(1, Sorted)),
    Tree = insert_vals(Vals, aeu_mp_trees:new()),
    test_iterator_depth(10, Sorted, Tree),
    test_iterator_depth(11, Sorted, Tree),
    test_iterator_depth(12, Sorted, Tree).

test_iterator_depth(Depth, Sorted, Tree) ->
    Iterator = aeu_mp_trees:iterator(Tree, [{max_path_length, Depth}]),
    Sorted1  = [X || {Key, _} = X <- Sorted, (bit_size(Key) div 4) =< Depth],
    test_iterator_depth(Depth, Sorted1, Tree, Iterator).

test_iterator_depth(Depth, [{Key, Val}|Left], Tree, Iterator) ->
    {IKey, IVal, Iterator1} = aeu_mp_trees:iterator_next(Iterator),
    ?assertEqual(IKey, Key),
    ?assertEqual(IVal, Val),
    case Left of
        [{NextKey, NextVal}|_] ->
            Opt = [{max_path_length, Depth}],
            NewIterator = aeu_mp_trees:iterator_from(Key, Tree, Opt),
            ?assertMatch({NextKey, NextVal, _},
                         aeu_mp_trees:iterator_next(NewIterator));
        [] ->
            ok
    end,
    test_iterator_depth(Depth, Left, Tree, Iterator1);
test_iterator_depth(_Depth, [],_Tree, Iterator) ->
    ?assertEqual('$end_of_table', aeu_mp_trees:iterator_next(Iterator)),
    ok.

test_iterator_non_existing() ->
    rand:seed(exs1024s, {1412343, 989132, 44563}),
    Vals = gen_vals(1000),
    Sorted = lists:sort(Vals),
    ?assertEqual(Sorted, lists:ukeysort(1, Sorted)),
    {Vals1, Vals2} = split_every_other(Sorted, [], []),
    NonExistingKeys = [X || {X, _} <- Vals1],
    Tree = insert_vals(Vals2, aeu_mp_trees:new()),
    test_iterator_non_existing(Vals2, NonExistingKeys, Tree).

split_every_other([X, Y|Left], Acc1, Acc2) ->
    split_every_other(Left, [X|Acc1], [Y|Acc2]);
split_every_other([], Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)}.

test_iterator_non_existing([{Key, Val}|Left1], [NonExisting|Left2], Tree) ->
    Iterator = aeu_mp_trees:iterator_from(NonExisting, Tree),
    ?assertMatch({Key, Val, _}, aeu_mp_trees:iterator_next(Iterator)),
    test_iterator_non_existing(Left1, Left2, Tree);
test_iterator_non_existing([], [],_Tree) ->
    ok.

test_iterator_prefix() ->
    rand:seed(exs1024s,{1529,527131,758402}),
    Vals = gen_vals(1000),
    Prefixes = [<<1:4>>, <<2:4, 2:4>>, <<2:4, 3:4>>,
                <<4:4, 2452342462452435:60>>],
    ?assertEqual(lists:usort(Prefixes), Prefixes), %% Hardcoded expectation on test data.
    ValsPrefixed = [ {<<X/bits, Y/bits>>, Val}
                     || X <- Prefixes, {Y, Val} <- Vals],
    Sorted = lists:sort(ValsPrefixed),
    ?assertEqual(Sorted, lists:ukeysort(1, Sorted)),
    Tree = insert_vals(ValsPrefixed, aeu_mp_trees:new()),
    test_iterator_prefix(Prefixes, Sorted, Tree).

test_iterator_prefix([Prefix|Left], Sorted, Tree) ->
    Opts = [{with_prefix, Prefix}],
    Iterator = aeu_mp_trees:iterator_from(Prefix, Tree, Opts),
    SortedLeft = test_iterator_one_prefix(Iterator, Tree, Prefix, Sorted),
    test_iterator_prefix(Left, SortedLeft, Tree);
test_iterator_prefix([], [],_Tree) ->
    ok.

test_iterator_one_prefix(Iterator, Tree, Prefix, [{Key, Val}|Left] = Vals) when is_bitstring(Key) ->
    case aeu_mp_trees:iterator_next(Iterator) of
        '$end_of_table' -> Vals;
        {IKey, IVal, Iterator1} ->
            PrefixS = bit_size(Prefix),
            ?assertMatch(<<Prefix:PrefixS/bitstring, _/bitstring>>, Key),
            ?assertEqual(IKey, Key),
            ?assertEqual(IVal, Val),
            %% Prefix iterator starting from key skips key.
            Opts =  [{with_prefix, Key}],
            NewIterator = aeu_mp_trees:iterator_from(Key, Tree, Opts),
            case Left of
                [] ->
                    ?assertMatch('$end_of_table',
                                 aeu_mp_trees:iterator_next(NewIterator));
                %% Case not triggered by test data.
                %% [{NextKey = <<Key:KeyS/bitstring, _:1/bitstring, _/bitstring>>, NextVal}|_] ->
                %%     ?assertMatch({NextKey, NextVal, _},
                %%                  aeu_mp_trees:iterator_next(NewIterator));
                [{NextKey,_NextVal}|_] when NextKey =/= Key ->
                    ?assertMatch('$end_of_table',
                                 aeu_mp_trees:iterator_next(NewIterator))
            end,
            test_iterator_one_prefix(Iterator1, Tree, Prefix, Left)
    end;
test_iterator_one_prefix(Iterator,_Tree, _, []) ->
    ?assertEqual('$end_of_table', aeu_mp_trees:iterator_next(Iterator)),
    [].

%%%=============================================================================
%%% Hash tests

test_reversed_put() ->
    rand:seed(exs1024s, {143, 14132, 4163}),
    Vals = gen_vals(1000),
    T0 = aeu_mp_trees:new(),
    T1 = insert_vals(Vals, T0),
    T2 = insert_vals(lists:reverse(Vals), T0),
    ?assertEqual(aeu_mp_trees:root_hash(T1),
                 aeu_mp_trees:root_hash(T2)).

test_put_and_delete() ->
    %% From an existing tree, add and delete some nodes and see that
    %% we arrive at the same hash again.
    {T0, _} = gen_mp_tree({345, 2345, 1234}, 1000),
    Vals = gen_vals(1000),
    ?assertEqual([], [X || {X, _} <- Vals, aeu_mp_trees:get(X, T0) =/= <<>>]),
    T1 = insert_vals(Vals, T0),
    T2 = delete_vals(Vals, T1),
    ?assertEqual(aeu_mp_trees:root_hash(T0), aeu_mp_trees:root_hash(T2)),

    T3 = delete_vals(lists:reverse(Vals), T1),
    ?assertEqual(aeu_mp_trees:root_hash(T0), aeu_mp_trees:root_hash(T3)),
    ok.

test_ext_merge_of_paths() ->
    Val = <<0,0,0,0>>,
    Key1 = <<81,0:4>>,
    Key2 = <<81>>,
    Key3 = <<81,0>>,

    T0 = aeu_mp_trees:new(),

    T1 = apply_ops([ {put, Key1, Val}
                   , {put, Key2, Val}
                   , {put, Key3, Val}
                   , {delete, Key2}
                   ], T0),
    T2 = apply_ops([ {put, Key1, Val}
                   , {put, Key3, Val}
                   ], T0),
    ?assertEqual(aeu_mp_trees:root_hash(T1),
                 aeu_mp_trees:root_hash(T2)).

%%%=============================================================================
%%% Extension tests

test_extension_one_step() ->
    rand:seed(exs1024s, {1413, 1432, 413}),
    test_extension_n_step(1).

test_extension_two_step() ->
    rand:seed(exs1024s, {141323, 1423432, 415133}),
    test_extension_n_step(1).

test_extension_n_step(Step) ->
    Key = random_hexstring(65),
    Val = random_hexstring(65*2+2),
    T   = insert_step(Key, Val, 1, aeu_mp_trees:new()),
    lookup_step(Key, Val, Step, T),
    ok.

insert_step(Key, Val, Step, T) ->
    case {Key, Val} of
        { <<_:Step/unit:4, Key1/bitstring>>
        , <<_:Step/unit:8, Val1/binary>>} when Key1 =/= <<>> ->
            T1 = aeu_mp_trees:put(Key1, Val1, T),
            insert_step(Key1, Val1, Step, T1);
        {_, _} -> T
    end.

lookup_step(Key, Val, Step, T) ->
    case {Key, Val} of
        { <<_:Step/unit:4, Key1/bitstring>>
        , <<_:Step/unit:8, Val1/binary>>} when Key1 =/= <<>> ->
            ?assertEqual(Val1, aeu_mp_trees:get(Key1, T)),
            lookup_step(Key1, Val1, Step, T);
        {_, _} -> ok
    end.

%%%=============================================================================
%%% Proof tests

test_proofs() ->
    {T, Vals} = gen_mp_tree({634, 2345, 6987}, 1000),
    test_create_proofs(Vals, T).

test_create_proofs([{Key, Val}|Left], T) ->
    {Val, ProofDB} = aeu_mp_trees:construct_proof(Key, new_dict_db(), T),
    Hash = aeu_mp_trees:root_hash(T),
    ?assertEqual(ok, aeu_mp_trees:verify_proof(Key, Val, Hash, ProofDB)),
    test_create_proofs(Left, T);
test_create_proofs([],_T) ->
    ok.

test_bogus_proofs() ->
    {T, Vals} = gen_mp_tree({634, 2345, 6987}, 1000),
    test_bogus_proofs(Vals, T).

test_bogus_proofs([{Key, Val}|Left], T) ->
    {Val, ProofDB} = aeu_mp_trees:construct_proof(Key, new_dict_db(), T),
    Hash = aeu_mp_trees:root_hash(T),
    BogusHash = case Hash of
                    <<1, Rest/binary>> -> <<2, Rest/binary>>;
                    <<_, Rest/binary>> -> <<1, Rest/binary>>
                end,
    BogusVal = <<0, Val/binary>>,
    {BadHash, BogusDB} = alter_one_hash_value(ProofDB),
    ?assertEqual(bad_proof,
                 aeu_mp_trees:verify_proof(Key, Val, BogusHash, ProofDB)),
    ?assertEqual({bad_value, Val},
                 aeu_mp_trees:verify_proof(Key, BogusVal, Hash, ProofDB)),
    ?assertMatch({bad_hash, BadHash},
                 aeu_mp_trees:verify_proof(Key, Val, Hash, BogusDB)),
    test_bogus_proofs(Left, T);
test_bogus_proofs([],_T) ->
    ok.

alter_one_hash_value(DB) ->
    Dict = aeu_mp_trees_db:get_cache(DB),
    Size = dict:size(Dict),
    Pos  = rand:uniform(Size),
    {Hash, Node} = lists:nth(Pos, dict:to_list(Dict)),
    NewNode =
        case Node of
            [X, Y] ->
                [<<X/binary, 0>>, Y]; %% Leaf or Ext
            Branch ->
                case lists:reverse(Branch) of
                    [<<>>|Rev] -> lists:reverse([<<1>>|Rev]);
                    [_|Rev] -> lists:reverse([<<>>|Rev])
                end
        end,
    BogusDB = aeu_mp_trees_db:put(Hash, NewNode, DB),
    {Hash, BogusDB}.

%%%=============================================================================
%%% Subtree tests

test_subtrees() ->
    {Tree, Vals} = gen_mp_tree({1542,713829,704664}, 1000),
    ValDict = dict:from_list(Vals),
    ?assertEqual(length(Vals), dict:size(ValDict)),
    %% Fenerate nodes for subtree roots
    Roots = lists:usort([R || {<<R:16/binary, _/bits>>, _}  <- Vals]),
    Tree1 = insert_vals([{R, <<1>>} || R <- Roots], Tree),

    %% Check that we can create subtrees from all roots.
    [?assertMatch({ok, _}, aeu_mp_trees:read_only_subtree(Root, Tree1))
     || Root <- Roots],

    %% Check that we cannot create subtrees from a node we know isn't present
    [?assertMatch({error, no_such_subtree},
                  aeu_mp_trees:read_only_subtree(<<1,2,3,Key/bits>>, Tree1))
     || {Key, _} <- Vals],

    %% Check that iterating through all unique subtrees also iterate
    %% through all keys exactly once
    iterate_subtrees(Roots, Tree1, ValDict),
    ok.

iterate_subtrees([Root|Left], Tree, ValDict) ->
    {ok, Subtree} = aeu_mp_trees:read_only_subtree(Root, Tree),
    Iterator = aeu_mp_trees:iterator(Subtree),
    NewValDict = iterate_one_subtree(aeu_mp_trees:iterator_next(Iterator), Root, ValDict),
    iterate_subtrees(Left, Tree, NewValDict);
iterate_subtrees([],_Tree, ValDict) ->
    case dict:size(ValDict) of
        0 -> ok;
        Other -> error({not_traversed, Other})
    end.

iterate_one_subtree('$end_of_table',_Root,ValDict) ->
    ValDict;
iterate_one_subtree({Key0, Val, Iterator}, Root, ValDict) ->
    Key = <<Root/bits, Key0/bits>>,
    case dict:find(Key, ValDict) of
        {ok, Val} ->
            NewValDict = dict:erase(Key, ValDict),
            Next = aeu_mp_trees:iterator_next(Iterator),
            iterate_one_subtree(Next, Root, NewValDict);
        {ok, Other} ->
            error({wrong_value, Key, Val, Other});
        error ->
            error({key_not_found, Key})
    end.

%%%=============================================================================
%%% Reachable hashes tests.

test_visit_reachable() ->
    {Tree, Vals} = gen_mp_tree({1544,700767,24258}, 1000),
    RootHash = aeu_mp_trees:root_hash(Tree),
    VisitFun = fun(Hash, SerializedNode, DB) ->
                       case aeu_mp_trees_db:get(Hash, DB) of
                           none ->
                               {continue,
                                aeu_mp_trees_db:put(Hash, SerializedNode, DB)};
                           {value, _} ->
                               stop
                       end
               end,
    DB = aeu_mp_trees:visit_reachable_hashes(Tree, new_dict_db(), VisitFun),
    NewTree = aeu_mp_trees:new(RootHash, DB),
    Sorted = lists:keysort(1, Vals),
    Iterator = aeu_mp_trees:iterator(NewTree),
    test_iterator(Iterator, NewTree, Sorted),
    OldCache = aeu_mp_trees_db:get_cache(aeu_mp_trees:db(Tree)),
    NewCache = aeu_mp_trees_db:get_cache(aeu_mp_trees:db(NewTree)),
    io:format("OldCache size: ~p\n", [dict:size(OldCache)]),
    io:format("NewCache size: ~p\n", [dict:size(NewCache)]),
    ok.

%%%=============================================================================
%%% Minimal cache commit tests

test_commit_reachable() ->
    %% Generate a random tree and commit the reachable from the cache.
    {Tree, Vals1} = gen_mp_tree({1544,700767,24258}, 1000),
    Tree1 = aeu_mp_trees:commit_reachable_to_db(Tree),

    %% Check that we are not committing the full cache
    CacheSize = dict:size(aeu_mp_trees_db:get_cache(aeu_mp_trees:db(Tree))),
    DBSize1   = dict:size(aeu_mp_trees_db:get_handle(aeu_mp_trees:db(Tree1))),
    ?assert(DBSize1 < CacheSize),

    %% Check that we now have an empty cache.
    CacheSize1 = dict:size(aeu_mp_trees_db:get_cache(aeu_mp_trees:db(Tree1))),
    ?assertEqual(0, CacheSize1),

    %% Check that we didn't lose any keys.
    Sorted1 = lists:ukeysort(1, Vals1),
    Iterator1 = aeu_mp_trees:iterator(Tree1),
    test_iterator(Iterator1, Tree1, Sorted1),

    %% Add another batch of keys and values so we get both a db and a cache
    {Tree2, Vals2} = extend_mp_tree(Tree1, 1000),

    %% The db size should be unchanged.
    DBSize2   = dict:size(aeu_mp_trees_db:get_handle(aeu_mp_trees:db(Tree2))),
    ?assertEqual(DBSize1, DBSize2),

    %% Commit the reachable from the cache.
    Tree3 = aeu_mp_trees:commit_reachable_to_db(Tree2),

    %% The size of the new db should contain all old nodes, plus a
    %% subset of the cache size before commit.
    CacheSize2 = dict:size(aeu_mp_trees_db:get_cache(aeu_mp_trees:db(Tree2))),
    DBSize3   = dict:size(aeu_mp_trees_db:get_handle(aeu_mp_trees:db(Tree3))),
    ?assert(DBSize3 < DBSize2 + CacheSize2),

    %% We should have all values in the final tree
    Sorted3 = lists:ukeysort(1, Vals1 ++ Vals2),
    ?assertEqual(length(Sorted3), length(Vals1) + length(Vals2)),
    Iterator3 = aeu_mp_trees:iterator(Tree3),
    test_iterator(Iterator3, Tree3, Sorted3),

    %% We should also be able to traverse the full tree from the
    %% historical root hash using the final trees DB.
    Root  = aeu_mp_trees:root_hash(Tree),
    DB3   = aeu_mp_trees:db(Tree3),
    Tree4 = aeu_mp_trees:new(Root, DB3),
    Iterator4 = aeu_mp_trees:iterator(Tree4),
    test_iterator(Iterator4, Tree4, Sorted1),

    ok.

%%%=============================================================================
%%% Cache GC tests

test_gc_cache() ->
    {Tree, Vals}  = gen_mp_tree({1545,59644,821050}, 1000),
    GCTree = aeu_mp_trees:gc_cache(Tree),
    CacheSize1 = dict:size(aeu_mp_trees_db:get_cache(aeu_mp_trees:db(Tree))),
    CacheSize2 = dict:size(aeu_mp_trees_db:get_cache(aeu_mp_trees:db(GCTree))),

    %% Assert the GC was effective, but the new tree contains all values.
    ?assert(CacheSize2 < CacheSize1),
    Sorted = lists:ukeysort(1, Vals),
    Iterator = aeu_mp_trees:iterator(GCTree),
    test_iterator(Iterator, GCTree, Sorted),

    %% Traverse all reachable nodes, and make sure the cache contains only these.
    VisitFun = fun(Hash, Val, Acc) -> {continue, [{Hash, Val}|Acc]} end,
    Visited = aeu_mp_trees:visit_reachable_hashes(GCTree, [], VisitFun),
    CacheVals = dict:to_list(aeu_mp_trees_db:get_cache(aeu_mp_trees:db(GCTree))),
    ?assertEqual(lists:sort(Visited), lists:sort(CacheVals)).

%%%=============================================================================
%%% Test utils

gen_mp_tree(Seed, NofNodes) ->
    rand:seed(exs1024s, Seed),
    extend_mp_tree(aeu_mp_trees:new(), NofNodes).

extend_mp_tree(Tree, NofNodes) ->
    Vals = gen_vals(NofNodes),
    ?assertEqual(length(Vals), length(lists:ukeysort(1, Vals))),
    {insert_vals(Vals, Tree), Vals}.

gen_vals(NofNodes) ->
    [{random_hexstring(65), random_hexstring(8)}
     || _ <- lists:seq(1, NofNodes)].

apply_ops([{put, Key, Val}|Left], T) ->
    apply_ops(Left, aeu_mp_trees:put(Key, Val, T));
apply_ops([{delete, Key}|Left], T) ->
    apply_ops(Left, aeu_mp_trees:delete(Key, T));
apply_ops([], T) ->
    T.

insert_vals([{X, Y}|Left], T) ->
    insert_vals(Left, aeu_mp_trees:put(X, Y, T));
insert_vals([], T) ->
    T.

delete_vals([{X,_Y}|Left], T) ->
    delete_vals(Left, aeu_mp_trees:delete(X, T));
delete_vals([], T) ->
    T.

random_hexstring(N) when N >= 1 ->
    << <<(rand:uniform(16) - 1):4>> || _ <- lists:seq(1, N) >>.

new_dict_db() ->
    aeu_mp_trees_db:new(dict_db_spec()).

dict_db_spec() ->
    #{ handle => dict:new()
     , cache  => dict:new()
     , module => ?MODULE
     }.

mpt_db_get(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Val} -> {value, Val};
        error -> none
    end.

mpt_db_put(Key, Val, Dict) ->
    dict:store(Key, Val, Dict).

mpt_db_drop_cache(_Dict) ->
    dict:new().

mpt_db_list_cache(Dict) ->
    dict:to_list(Dict).
