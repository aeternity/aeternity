%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Read-side tests for aeu_mp_trees:get_w_node_cache/2: correctness across
%%%    an intervening write (content-addressed, no staleness), and that it cuts
%%%    DB reads on a read-heavy overlapping-path workload. A counting
%%%    aeu_mp_trees_db behaviour tallies mpt_db_get/2 calls; the tree is
%%%    committed first so reads genuinely hit the counted get, not the
%%%    write cache.
%%% @end
%%%=============================================================================
-module(aeu_mp_trees_node_cache_perf_tests).

-include_lib("eunit/include/eunit.hrl").

-behavior(aeu_mp_trees_db).
-export([ mpt_db_get/2
        , mpt_db_put/3
        , mpt_db_drop_cache/1
        ]).

node_cache_perf_test_() ->
    [ {timeout, 30,
       {"get_w_node_cache/2 stays correct across an intervening write "
        "(content-addressed: no staleness)",
        fun test_no_staleness_after_write/0}}
    , {timeout, 30,
       {"get_w_node_cache/2 returns identical values to get/2 on an "
        "overlapping-path read-heavy workload",
        fun test_values_match_plain_get/0}}
    , {timeout, 30,
       {"get_w_node_cache/2 measurably reduces DB reads vs repeated "
        "plain get/2 on the same overlapping-path workload",
        fun test_reduces_db_reads/0}}
    ].

%%%=============================================================================
%%% Content-addressed safety: no staleness after an intervening write
%%%=============================================================================

%% Overwrite a cache-warmed key, then confirm the rethreaded tree returns
%% the new value: new node hashes make the stale cached nodes unreachable.
test_no_staleness_after_write() ->
    {Tree0, Vals} = gen_overlapping_tree({91, 92, 93}, 4, 60),
    {K, V0} = hd(Vals),

    %% Warm the cache: K plus ~30 prefix-sharing neighbours.
    {V0, TreeWarm} = aeu_mp_trees:get_w_node_cache(K, Tree0),
    TreeWarm2 = lists:foldl(
                  fun({Kx, _}, T) ->
                          {_, T1} = aeu_mp_trees:get_w_node_cache(Kx, T),
                          T1
                  end, TreeWarm, lists:sublist(Vals, 2, 30)),

    V1 = <<"brand-new-value-after-write">>,
    TreeMutated = aeu_mp_trees:put(K, V1, TreeWarm2),

    ?assertEqual(V1, aeu_mp_trees:get(K, TreeMutated)),
    %% Same reads through the stale-cache-carrying tree see the new value too.
    {VGot, _TreeMutated1} = aeu_mp_trees:get_w_node_cache(K, TreeMutated),
    ?assertEqual(V1, VGot),
    ?assertNotEqual(V0, VGot),

    %% Unrelated key still reads correctly through the same carried cache.
    {OtherK, OtherV} = lists:last(Vals),
    {VOther, _} = aeu_mp_trees:get_w_node_cache(OtherK, TreeMutated),
    ?assertEqual(OtherV, VOther).

%%%=============================================================================
%%% Read-heavy overlapping-path workload: correctness + DB-read count
%%%=============================================================================

%% Value-correctness on the same workload, kept separate from the counting
%% path so a counting-DB change can never mask a wrong value.
test_values_match_plain_get() ->
    {Tree, ReadPlan} = build_committed_workload(),
    lists:foreach(
      fun(K) ->
              PlainV = aeu_mp_trees:get(K, Tree),
              {CacheV, _} = aeu_mp_trees:get_w_node_cache(K, Tree),
              ?assertEqual(PlainV, CacheV)
      end, ReadPlan).

%% Many reads over a few overlapping prefix buckets: threading the cache
%% must issue far fewer DB reads than plain get/2, which redecodes shared
%% interior nodes (e.g. the root) on every call.
test_reduces_db_reads() ->
    {Tree, ReadPlan} = build_committed_workload(),

    reset_counter(),
    lists:foreach(fun(K) -> _ = aeu_mp_trees:get(K, Tree) end, ReadPlan),
    PlainCount = read_counter(),

    reset_counter(),
    lists:foldl(
      fun(K, T) -> {_, T1} = aeu_mp_trees:get_w_node_cache(K, T), T1 end,
      Tree, ReadPlan),
    CacheCount = read_counter(),

    ?debugFmt("~nH3b DB-read counts over ~p reads (~p distinct keys, "
              "overlapping-prefix buckets): plain get/2 = ~p, "
              "get_w_node_cache/2 = ~p (~.1f% of plain)~n",
              [length(ReadPlan), length(lists:usort(ReadPlan)),
               PlainCount, CacheCount, 100 * CacheCount / PlainCount]),

    ?assert(CacheCount < PlainCount),
    %% Threshold, not just "<": overlapping reads collapse most upper-node
    %% redecodes, so a flake-proof bound sits well under half.
    ?assert(CacheCount =< PlainCount div 2).

%%%=============================================================================
%%% Workload construction
%%%=============================================================================

%% Fixed prefix buckets (overlapping paths near the trie top), committed to
%% the counting DB, then read across several passes.
build_committed_workload() ->
    {Tree0, Vals} = gen_overlapping_tree({777, 4242, 909090}, 5, 40),
    Tree1 = aeu_mp_trees:commit_reachable_to_db(Tree0),
    Keys = [K || {K, _} <- Vals],
    ReadPlan = lists:append(lists:duplicate(3, Keys)), %% 3 overlapping passes
    {Tree1, ReadPlan}.

%% NBuckets fixed nibble prefixes, PerBucket keys each, so keys in a bucket
%% share the same top branch/extension nodes.
gen_overlapping_tree(Seed, NBuckets, PerBucket) ->
    rand:seed(exs1024s, Seed),
    Prefixes = [random_hexstring(16) || _ <- lists:seq(1, NBuckets)],
    Vals = [ {<<Prefix/bitstring, (random_hexstring(48))/bitstring>>, random_value()}
             || Prefix <- Prefixes, _ <- lists:seq(1, PerBucket) ],
    UVals = lists:ukeysort(1, Vals),
    Tree = lists:foldl(fun({K, V}, T) -> aeu_mp_trees:put(K, V, T) end,
                        aeu_mp_trees:new(new_counting_db()), UVals),
    {Tree, UVals}.

random_value() ->
    random_hexstring(8).

random_hexstring(N) when N >= 1 ->
    << <<(rand:uniform(16) - 1):4>> || _ <- lists:seq(1, N) >>.

%%%=============================================================================
%%% Counting DB: own aeu_mp_trees_db behaviour so its counter side effect
%%% stays isolated from other tests.
%%%=============================================================================

new_counting_db() ->
    aeu_mp_trees_db:new(#{ handle => #{}
                          , cache  => #{}
                          , module => ?MODULE
                          }).

mpt_db_get(Key, Map) ->
    bump_counter(),
    case maps:find(Key, Map) of
        {ok, Val} -> {value, Val};
        error -> none
    end.

mpt_db_put(Key, Val, Map) ->
    Map#{Key => Val}.

mpt_db_drop_cache(_Dict) ->
    #{}.

reset_counter() -> erase(?MODULE), ok.
bump_counter() -> put(?MODULE, read_counter() + 1).
read_counter() -> case get(?MODULE) of undefined -> 0; N -> N end.
