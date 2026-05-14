%%%-------------------------------------------------------------------
%%% @doc
%%%   Global decoded-term cache for Merkle Patricia Tree interior nodes.
%%%
%%%   Nodes are content-addressed: a 32-byte hash uniquely and permanently
%%%   identifies its decoded tree_node() value.  The cache therefore never
%%%   needs invalidation — a cached entry is unconditionally correct for
%%%   the lifetime of the node.
%%%
%%%   Eviction uses the same two-segment generational LRU as
%%%   aec_mpt_cache: entries start in generation 1; when the gen-1
%%%   count reaches max_size(), gen-2 is dropped and gen-1 is demoted
%%%   to gen-2 in two ets:select_* calls (O(n), amortised O(1)).
%%%
%%%   The table is public and named so any process can read/write
%%%   without going through a serialising gen_server.  start/0 is
%%%   called once from aecore_sup before any MPT reader starts.
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_mpt_node_cache).

-export([ start/0
        , get/1
        , put/2
        , clear/0
        , size/0
        ]).

-define(TAB,            aeu_mpt_node_cache).
-define(PT_YOUNG,       {?MODULE, young_counter}).
-define(DEFAULT_MAX,    200000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
    _ = ensure_table(),
    _ = ensure_young_counter(),
    ok.

-spec get(binary()) -> {value, term()} | none.
get(Hash) ->
    try
        case ets:lookup(?TAB, Hash) of
            [{Hash, Node, 1}] ->
                {value, Node};
            [{Hash, Node, 2}] ->
                _ = ets:update_element(?TAB, Hash, {3, 1}),
                note_young_entry(),
                {value, Node};
            [] ->
                none
        end
    catch error:badarg ->
        _ = ensure_table(),
        none
    end.

-spec put(binary(), term()) -> ok.
put(Hash, Node) ->
    try
        case ets:lookup(?TAB, Hash) of
            [{Hash, _, 1}] ->
                true = ets:insert(?TAB, {Hash, Node, 1});
            [{Hash, _, 2}] ->
                true = ets:insert(?TAB, {Hash, Node, 1}),
                note_young_entry();
            [] ->
                true = ets:insert(?TAB, {Hash, Node, 1}),
                note_young_entry()
        end
    catch error:badarg ->
        _ = ensure_table()
    end,
    ok.

-spec clear() -> ok.
clear() ->
    ok = start(),
    true = ets:delete_all_objects(?TAB),
    atomics:put(young_counter(), 1, 0),
    ok.

-spec size() -> non_neg_integer().
size() ->
    case ets:whereis(?TAB) of
        undefined -> 0;
        _         -> ets:info(?TAB, size)
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

ensure_table() ->
    case ets:whereis(?TAB) of
        undefined ->
            try ets:new(?TAB, [ named_table
                              , public
                              , set
                              , {read_concurrency,  true}
                              , {write_concurrency, true}
                              ]) of
                _ -> ok
            catch
                error:badarg -> ok   %% another process won the race
            end;
        _Tid ->
            ok
    end.

ensure_young_counter() ->
    case persistent_term:get(?PT_YOUNG, undefined) of
        undefined ->
            Counter = atomics:new(1, []),
            persistent_term:put(?PT_YOUNG, Counter),
            persistent_term:get(?PT_YOUNG);
        Counter ->
            Counter
    end.

young_counter() ->
    ensure_young_counter().

note_young_entry() ->
    Young = atomics:add_get(young_counter(), 1, 1),
    case Young =:= ?DEFAULT_MAX of
        true  -> rotate();
        false -> ok
    end.

rotate() ->
    %% Drop all gen-2 entries then demote gen-1 to gen-2.
    _Deleted = ets:select_delete(?TAB,
                   [{{'_', '_', 2}, [], [true]}]),
    _ = ets:select_replace(?TAB,
                   [{{'$1', '$2', 1}, [], [{{'$1', '$2', 2}}]}]),
    atomics:put(young_counter(), 1, 0),
    ok.
