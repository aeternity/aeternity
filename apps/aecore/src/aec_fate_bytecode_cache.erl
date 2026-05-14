%%%-------------------------------------------------------------------
%%% @doc
%%%   Cross-transaction immutable bytecode cache for FATE contracts.
%%%
%%%   Contract bytecode is immutable: once a contract is deployed its
%%%   code never changes.  A pubkey therefore permanently identifies a
%%%   (FateCode, VMVersion) pair, making this cache unconditionally safe
%%%   — no invalidation is ever required.
%%%
%%%   INVARIANT: A given contract pubkey always maps to the same
%%%   {Code, VMVersion} pair.  This cache therefore never requires
%%%   invalidation under normal protocol operation.
%%%   If a hard fork must redeploy code at an existing address, this
%%%   cache MUST be cleared at the fork height in
%%%   aec_conductor:preempt_on_new_top/3.  insert/3 logs an error if
%%%   a conflict is detected at runtime.
%%%
%%%   Eviction uses the same two-segment generational LRU as
%%%   aec_mpt_cache (max_size defaults to 10,000 contracts).
%%%   start/0 is called from aecore_sup before any FATE executor starts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_fate_bytecode_cache).

-export([ start/0
        , lookup/1
        , insert/3
        , clear/0
        , size/0
        ]).

-define(TAB,         aec_fate_bytecode_cache).
-define(PT_YOUNG,    {?MODULE, young_counter}).
-define(DEFAULT_MAX, 10000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
    _ = ensure_table(),
    _ = ensure_young_counter(),
    ok.

-spec lookup(binary()) -> {ok, term(), term()} | miss.
lookup(Pubkey) ->
    try
        case ets:lookup(?TAB, Pubkey) of
            [{Pubkey, Code, VMV, 1}] ->
                {ok, Code, VMV};
            [{Pubkey, Code, VMV, 2}] ->
                _ = ets:update_element(?TAB, Pubkey, {4, 1}),
                note_young_entry(),
                {ok, Code, VMV};
            [] ->
                miss
        end
    catch error:badarg ->
        _ = ensure_table(),
        miss
    end.

-spec insert(binary(), term(), term()) -> ok.
insert(Pubkey, Code, VMV) ->
    try
        case ets:insert_new(?TAB, {Pubkey, Code, VMV, 1}) of
            true ->
                note_young_entry();
            false ->
                case ets:lookup(?TAB, Pubkey) of
                    [{Pubkey, Code, VMV, _}] ->
                        ok;
                    [{Pubkey, _OtherCode, _OtherVMV, _}] ->
                        lager:error("aec_fate_bytecode_cache: INVARIANT VIOLATION "
                                    "— bytecode conflict for pubkey ~p; "
                                    "evicting stale entry", [Pubkey]),
                        true = ets:insert(?TAB, {Pubkey, Code, VMV, 1})
                end
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
                              , {write_concurrency, false}
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
    case Young >= ?DEFAULT_MAX of
        true  -> rotate();
        false -> ok
    end.

rotate() ->
    _Deleted = ets:select_delete(?TAB,
                   [{{'_', '_', '_', 2}, [], [true]}]),
    _ = ets:select_replace(?TAB,
                   [{{'$1', '$2', '$3', 1}, [], [{{'$1', '$2', '$3', 2}}]}]),
    atomics:put(young_counter(), 1, 0),
    ok.
