%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    ADT for contract stores in FATE.
%%%
%%%    The code assumes that if a get/put is issued, the contract is
%%%    already in the store. Care must be taken to explicitly check if
%%%    a contract is present, and put it (using put_contract/2) if it is not.
%%%
%%%    Entries are cached in a read/write manner to avoid multiple
%%%    reading/converting and to avoid pushing terms that have not been
%%%    altered. Both things are expensive as it involves going out to the
%%%    underlying merkle trees.
%%%
%%%    Use finalize/3 to push the stores back to the chain when the
%%%    fate execution is done.
%%%
%%%    Arcus/v7+ only; Iris..Ceres and pre-Iris are frozen in
%%%    aefa_stores_ceres/aefa_stores_lima.
%%%    Store reads charge gas for the raw byte count before deserializing.
%%%
%%%  @end
%%%    -------------------------------------------------------------------

-module(aefa_stores).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-export([ finalize/3
        , find_value/3
        , find_value/4
        , has_contract/2
        , initial_contract_store/0
        , new/0
        , put_contract_store/3
        , put_value/4
        , terms_to_finalize/1
        %% Map functions
        , cache_map_metadata/2
        , store_map_lookup/4
        , store_map_lookup/5
        , store_map_member/4
        , store_map_member/5
        , store_map_to_list/3
        , store_map_to_list/4
        , store_map_size/3
        ]).

-record(store, { cache = #{} :: contract_cache()
               }).

-record(cache_entry, { store :: aect_contracts_store:store()
                     , dirty :: boolean()
                     , terms :: fate_terms()
                     }).

-type fate_val() :: aeb_fate_data:fate_type().
-type pubkey() :: <<_:256>>.
-type dirty() :: boolean().
-type fate_terms() :: #{ integer() => {fate_val(), dirty()} }.
-type contract_cache() :: #{pubkey() => #cache_entry{}}.

-type fate_map() :: aeb_fate_data:fate_map() | aeb_fate_data:fate_store_map().

-opaque store() :: #store{}.

-export_type([ store/0
             ]).

-define(MAX_STORE_POS, 16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff).
-define(META_STORE_POS, 0).
-define(VALID_STORE_POS(Pos), Pos > ?META_STORE_POS andalso Pos < ?MAX_STORE_POS).
-define(STORE_KEY_PREFIX, 0).
-define(STORE_MAP_PREFIX, 1).

-ifdef(TEST).
-define(ASSERT(Check, Err),
        case Check of
            true  -> ok;
            false -> error({assertion_failed, ?FILE, ?LINE, Err})
        end).
-else.
-define(ASSERT(Check, Err), ok).
-endif.

-ifdef(DEBUG).
-define(DEBUG_STORE(S), debug_stores(S)).
-define(DEBUG_PRINT(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DEBUG_STORE(S), ok).
-define(DEBUG_PRINT(Fmt, Args), ok).
-endif.

%% Test-only export to drive the reuse-fixpoint fuel mechanism directly.
-ifdef(TEST).
-export([ reuse_fixpoint_loop/7
        , optimistic_reuse_fixpoint/6
        , full_reuse_fixpoint/6
        , compute_reuse_fixpoint/4
        ]).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> store().
new() ->
  #store{}.

-spec initial_contract_store() -> aect_contracts_store:store().
initial_contract_store() ->
    aect_contracts_store:put(store_meta_key(),
                             aeb_fate_encoding:serialize(empty_store_meta_data()),
                             aect_contracts_store:new()).

-spec put_contract_store(pubkey(), aect_contracts_store:store(), store()) -> store().
put_contract_store(Pubkey, Store, #store{cache = Cache} = S) ->
    S#store{cache = Cache#{Pubkey => new_contract_cache_entry(Store)}}.

-spec has_contract(pubkey(), store()) -> boolean().
has_contract(Pubkey, #store{cache = Cache}) ->
    maps:is_key(Pubkey, Cache).

-spec find_value(pubkey(), non_neg_integer(), store()) ->
                    {'ok', fate_val(), store(), non_neg_integer()}
                  | {'ok', fate_val()}
                  | 'error'.
find_value(Pubkey, StorePos, S) when ?VALID_STORE_POS(StorePos) ->
    %% Unmetered variant, used by test helpers and find_meta_data/2.
    case find_value_(Pubkey, StorePos, S, unmetered) of
        {ok, Term, Store1, Bytes, unmetered} -> {ok, Term, Store1, Bytes};
        Other -> Other
    end.

-spec find_value(pubkey(), non_neg_integer(), store(), non_neg_integer()) ->
                    {'ok', fate_val(), store(), non_neg_integer(), non_neg_integer()}
                  | {'ok', fate_val()}
                  | {'error', 'out_of_gas'}
                  | 'error'.
find_value(Pubkey, StorePos, S, GasLeft) when ?VALID_STORE_POS(StorePos), is_integer(GasLeft) ->
    %% Gas-aware variant: charges for a cache-miss read before deserializing.
    %% The only arity aefa_fate:lookup_in_store/2 calls from Arcus onward.
    find_value_(Pubkey, StorePos, S, GasLeft).

-spec put_value(pubkey(), non_neg_integer(), fate_val(), store()) -> store().
put_value(Pubkey, StorePos, FateVal, #store{cache = Cache} = S) ->
    Entry = maps:get(Pubkey, Cache),
    Terms = maps:put(StorePos, {FateVal, true}, Entry#cache_entry.terms),
    Entry1 = Entry#cache_entry{terms = Terms, dirty = true},
    S#store{cache = Cache#{Pubkey => Entry1}}.

%% -- Local functions --------------------------------------------------------

update_ct_store(Pubkey, NewStore, #store{cache = Cache} = S) ->
    E = maps:get(Pubkey, Cache),
    S#store{cache = Cache#{Pubkey => E#cache_entry{store = NewStore}}}.

find_value_(Pubkey, StorePos, #store{cache = Cache} = S, Gas) ->
    case find_term(StorePos, maps:get(Pubkey, Cache), Gas) of
        {ok, Term} ->
            {ok, Term};
        {error, out_of_gas} ->
            {error, out_of_gas};
        {ok, Term, Entry, Bytes, Gas1} ->
            {ok, Term, S#store{cache = Cache#{Pubkey => Entry}}, Bytes, Gas1};
        error ->
            error
    end.

%%%===================================================================
%%% Store maps
%%%
%%% Maps are saved in the store as follows
%%%
%%%     /store_meta_key()               (store register 0) Map meta data: #{ MapId => ?METADATA(RawId, RefCount, Size) }
%%%     /?STORE_MAP_PREFIX/RawId:32      <<0>> - subtree node
%%%     /?STORE_MAP_PREFIX/RawId:32/Key  Value for Key in map RawId
%%%
%%% Storing the metadata in register 0 means we get caching for it, so each
%%% call will only have to deserialize the metadata at most once.
%%%
%%% Disinguishing the MapId from the RawId allows maps to be updated inplace,
%%% when the old copy of the map is not saved.

-define(METADATA(RawId, RefCount, Size), ?FATE_TUPLE({RawId, RefCount, Size})).
-define(RAWID_BITS, 32).

-type map_id()     :: non_neg_integer().
-type raw_id()     :: non_neg_integer().
-type ref_count()  :: non_neg_integer().
-type map_meta()   :: ?METADATA(raw_id(), ref_count(), non_neg_integer()).
-type store_meta() :: #{ map_id() => map_meta() }.

%% -- Store map API ----------------------------------------------------------

-spec cache_map_metadata(pubkey(), store()) -> store().
cache_map_metadata(Pubkey, S) ->
    case find_meta_data(Pubkey, S) of
        {ok, _, S1} -> S1;
        error       -> S
    end.

-spec store_map_lookup(pubkey(), non_neg_integer(), fate_val(), store()) -> {{ok, fate_val()} | error, store()}.
store_map_lookup(Pubkey, MapId, Key, S) ->
    %% Legacy/unmetered variant -- same rationale as find_value/3 above.
    case store_map_lookup_(Pubkey, MapId, Key, S, unmetered) of
        {ok, Val, Store1, unmetered} -> {{ok, Val}, Store1};
        {miss, Store1, unmetered}    -> {error, Store1}
    end.

-spec store_map_lookup(pubkey(), non_neg_integer(), fate_val(), store(), non_neg_integer()) ->
        {ok, fate_val(), store(), non_neg_integer()}
      | {miss, store(), non_neg_integer()}
      | {error, out_of_gas}.
store_map_lookup(Pubkey, MapId, Key, S, GasLeft) when is_integer(GasLeft) ->
    %% Map.lookup priced like a register read; the floor is charged on a
    %% miss too, since a miss here is a normal, non-aborting outcome.
    store_map_lookup_(Pubkey, MapId, Key, S, GasLeft).

store_map_lookup_(Pubkey, MapId, Key, #store{cache = Cache} = S, Gas) ->
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(RawId, _RefCount, _Size) = get_map_meta(MapId, Meta),
    case find_in_store(map_data_key(RawId, Key), Store, Gas) of
        {error, out_of_gas} ->
            {error, out_of_gas};
        error ->
            case charge_read_gas(Gas, 0) of
                out_of_gas -> {error, out_of_gas};
                Gas1       -> {miss, S1, Gas1}
            end;
        {ok, Val, Store1, _Bytes, Gas1} ->
            {ok, Val, update_ct_store(Pubkey, Store1, S1), Gas1}
    end.

-spec store_map_member(pubkey(), non_neg_integer(), fate_val(), store()) -> {boolean(), store()}.
store_map_member(Pubkey, MapId, Key, S) ->
    %% Legacy/unmetered variant -- same rationale as find_value/3 above.
    case store_map_member_(Pubkey, MapId, Key, S, unmetered) of
        {true, Store1, unmetered}  -> {true, Store1};
        {false, Store1, unmetered} -> {false, Store1}
    end.

-spec store_map_member(pubkey(), non_neg_integer(), fate_val(), store(), non_neg_integer()) ->
        {boolean(), store(), non_neg_integer()} | {error, out_of_gas}.
store_map_member(Pubkey, MapId, Key, S, GasLeft) when is_integer(GasLeft) ->
    %% Never deserializes, but still charges the read_gas_cost/1 floor
    %% for the MPT/DB touch, on both the hit and the miss branch.
    store_map_member_(Pubkey, MapId, Key, S, GasLeft).

store_map_member_(Pubkey, MapId, Key, #store{cache = Cache} = S, Gas) ->
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(RawId, _RefCount, _Size) = get_map_meta(MapId, Meta),
    case aect_contracts_store:get_w_cache(map_data_key(RawId, Key), Store) of
        {<<>>, _} ->
            case charge_read_gas(Gas, 0) of
                out_of_gas -> {error, out_of_gas};
                Gas1       -> {false, S1, Gas1}
            end;
        {Val, Store1} ->
            case charge_read_gas(Gas, byte_size(Val)) of
                out_of_gas -> {error, out_of_gas};
                Gas1       -> {true, update_ct_store(Pubkey, Store1, S1), Gas1}
            end
    end.

-spec store_map_to_list(pubkey(), non_neg_integer(), store()) -> {[{fate_val(), fate_val()}], store()}.
store_map_to_list(Pubkey, MapId, S) ->
    %% Legacy/unmetered variant -- same rationale as find_value/3 above.
    {List, Store1, unmetered} = store_map_to_list_(Pubkey, MapId, S, unmetered),
    {List, Store1}.

-spec store_map_to_list(pubkey(), non_neg_integer(), store(), non_neg_integer()) ->
        {[{fate_val(), fate_val()}], store(), non_neg_integer()} | {error, out_of_gas}.
store_map_to_list(Pubkey, MapId, S, GasLeft) when is_integer(GasLeft) ->
    store_map_to_list_(Pubkey, MapId, S, GasLeft).

store_map_to_list_(Pubkey, MapId, #store{cache = Cache} = S, Gas) ->
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(RawId, _, _) = get_map_meta(MapId, Meta),
    {Subtree, Store1} = aect_contracts_store:subtree_w_cache(map_data_key(RawId), Store),
    %% Whole-subtree read charged like any other subtree materialization
    %% (compute_copy_refcounts's copy branch): proportional to total bytes.
    case charge_read_gas(Gas, subtree_bytes(Subtree)) of
        out_of_gas -> {error, out_of_gas};
        Gas1 ->
            List = [ {aeb_fate_encoding:deserialize(K), aeb_fate_encoding:deserialize(V)}
                     || {K, V} <- lists:keysort(1,maps:to_list(Subtree)) ],
            {List, update_ct_store(Pubkey, Store1, S1), Gas1}
    end.

-spec store_map_size(pubkey(), non_neg_integer(), store()) -> {non_neg_integer(), store()}.
store_map_size(Pubkey, MapId, S) ->
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(_, _, Size) = get_map_meta(MapId, Meta),
    {Size, S1}.

%% -- Map metadata -----------------------------------------------------------

-spec find_meta_data(pubkey(), store()) -> {ok, store_meta(), store()} | error.
find_meta_data(Pubkey, S) ->
    %% Left unmetered: small internal bookkeeping term, cached per call.
    case find_value_(Pubkey, ?META_STORE_POS, S, unmetered) of
        {ok, Meta}                        -> {ok, Meta, S};
        {ok, Meta, S1, _Bytes, unmetered} -> {ok, Meta, S1};
        error                              -> error
    end.

empty_store_meta_data() -> #{}.

%% We need to know which map ids are in use when allocating fresh ones. We
%% include RawIds to ensure that we can set MapId == RawId for newly allocated
%% maps.
-spec used_map_ids(store_meta()) -> [map_id()].
used_map_ids(Metadata) ->
    lists:usort(lists:append(
        [ [Id, RawId] || {Id, ?METADATA(RawId, _, _)} <- maps:to_list(Metadata) ])).

-spec put_map_meta(map_id(), map_meta(), store_meta()) -> store_meta().
put_map_meta(MapId, MapMeta, Metadata) ->
    Metadata#{ MapId => MapMeta }.

-spec remove_map_meta(map_id(), store_meta()) -> store_meta().
remove_map_meta(MapId, Metadata) ->
    maps:remove(MapId, Metadata).

-spec get_map_meta(map_id(), store_meta()) -> map_meta().
get_map_meta(MapId, Meta) ->
    maps:get(MapId, Meta).

-spec find_meta_data_no_cache(#cache_entry{}) -> {ok, store_meta()} | error.
find_meta_data_no_cache(CacheEntry) ->
    case find_term(?META_STORE_POS, CacheEntry, unmetered) of
        {ok, Meta}                 -> {ok, Meta};
        {ok, Meta, _, _, unmetered} -> {ok, Meta};
        error                       -> error
    end.

%%%===================================================================
%%% Entry for one contract

new_contract_cache_entry(Store) ->
    #cache_entry{ store = Store
                , terms = #{}
                , dirty = false
                }.

%% Gas is either `unmetered` (no charge) or a gas budget (may return out_of_gas).
find_term(StorePos, #cache_entry{terms = Terms} = E, Gas) ->
    case maps:find(StorePos, Terms) of
        {ok, {FateVal,_Dirty}} ->
            %% Hit in the per-call term cache; nothing to charge.
            {ok, FateVal};
        error ->
            case find_in_store(store_key(StorePos), E#cache_entry.store, Gas) of
                error ->
                    error;
                {error, out_of_gas} ->
                    {error, out_of_gas};
                {ok, FateVal, Store1, Bytes, Gas1} ->
                    {ok, FateVal, E#cache_entry{terms = Terms#{StorePos => {FateVal, false}},
                                                store = Store1}, Bytes, Gas1}
            end
    end.

%% Charge-before-work: Bytes is charged before deserializing, so an
%% under-provisioned read aborts without paying the deserialize cost. A
%% miss is left uncharged here: a register miss always aborts immediately
%% in the caller anyway. Every caller whose miss is non-aborting charges it
%% explicitly instead -- store_map_lookup_/5 and store_map_member_/5 at
%% execution time, compute_reuse_only_refcounts/5 and
%% compute_copy_refcounts/5 at finalize time.
find_in_store(Key, Store, Gas) ->
    case aect_contracts_store:get_w_cache(Key, Store) of
        {<<>>, _Store1} ->
            error;
        {Value, Store1} ->
            Bytes = byte_size(Value),
            case charge_read_gas(Gas, Bytes) of
                out_of_gas ->
                    {error, out_of_gas};
                Gas1 ->
                    FateVal = aeb_fate_encoding:deserialize(Value),
                    {ok, FateVal, Store1, Bytes, Gas1}
            end
    end.

store_key(Int) ->
    <<?STORE_KEY_PREFIX, (binary:encode_unsigned(Int))/binary>>.

store_meta_key() ->
    store_key(?META_STORE_POS).

map_data_key(RawId) ->
    <<?STORE_MAP_PREFIX, RawId:?RAWID_BITS>>.

map_data_key(RawId, Key) ->
    map_raw_key(RawId, aeb_fate_encoding:serialize(Key)).

map_raw_key(RawId, KeyBin) ->
    <<(map_data_key(RawId))/binary, KeyBin/binary>>.

%%%===================================================================
%%% Write through cache to stores

-spec finalize(aefa_chain_api:state(), non_neg_integer(), store()) ->
                  {ok, aefa_chain_api:state(), non_neg_integer()}
                | {error, out_of_gas}.
finalize(API, GasLeft, #store{cache = Cache} = _S) ->
    ?DEBUG_STORE(_S),
    try maps:fold(fun finalize_entry/3, {[], GasLeft}, Cache) of
        {Stores, GasLeft1} ->
            API1 = finalize_stores(Stores, API),
            {ok, API1, GasLeft1}
    catch
        throw:out_of_gas ->
            {error, out_of_gas}
    end.

finalize_stores([{Pubkey, Store}|Left], API) ->
    API1 = aefa_chain_api:set_contract_store(Pubkey, Store, API),
    finalize_stores(Left, API1);
finalize_stores([], API) ->
    API.

finalize_entry(_Pubkey, #cache_entry{dirty = false}, Acc) ->
    Acc;
finalize_entry(Pubkey, Cache = #cache_entry{store = Store}, {Writes, GasLeft}) ->
    {ok, Metadata} = find_meta_data_no_cache(Cache), %% Last access so no need to cache here
    %% Compute which updates need to be performed (see store_update() type
    %% below). This also takes care of updating the metadata with new reference
    %% counts and removing entries for maps to be garbage collected. New maps
    %% get a dummy entry with only the reference count set.
    %% GasLeft/GcCache: subtree reads done while computing refcounts/garbage
    %% are charged here, before perform_store_updates runs; GcCache carries
    %% the fetched subtrees forward so gc_map/3 does not re-fetch them.
    {Metadata1, Updates, GasLeft1, GcCache} = compute_store_updates(Metadata, Cache, GasLeft),
    ?DEBUG_PRINT("Updates\n  ~p\n", [Updates]),

    ?ASSERT(check_store_updates(Updates), {bad_store_updates, Updates}),

    %% Performing the updates writes the necessary changes to the MP trees.
    {Store1, GasLeft2} = perform_store_updates(Metadata, Updates, Metadata1, GasLeft1, Store, GcCache),
    {[{Pubkey, Store1} | Writes], GasLeft2}.

%% These are the terms we need to pay traversal gas for before finalizing.
-spec terms_to_finalize(store()) -> [fate_val()].
terms_to_finalize(#store{cache = Cache}) ->
  [ Term || #cache_entry{dirty = true, terms = Terms} <- maps:values(Cache),
            {Term, true} <- maps:values(Terms) ].

%%%===================================================================
%%% Store updates

-type store_update() :: {push_term,  pos_integer(), fate_val()}                %% Write to store register
                      | {copy_map,   map_id(), fate_map()}                     %% Create a new map (no inplace update)
                      | {update_map, map_id(), aeb_fate_data:fate_store_map()} %% Update an existing map inplace
                      | {gc_map,     map_id()}.                                %% Garbage collect a map removing all entries

-ifdef(TEST).
%% Check that if a map is gc'd it's not also updated or copied.
-spec check_store_updates([store_update()]) -> boolean().
check_store_updates(Updates) ->
    GCd     = [ Id || {gc_map, Id} <- Updates ],
    Copied  = [ Id || {copy_map,   _, ?FATE_STORE_MAP(_, Id)} <- Updates ],
    Updated = [ Id || {update_map, _, ?FATE_STORE_MAP(_, Id)} <- Updates ],
    GCd -- (Copied ++ Updated) == GCd.
-endif.

%% GasLeft is threaded through so subtree reads done while computing
%% reuse/garbage are charged before perform_store_updates runs. Returns
%% GcCache: the RawId -> subtree map fetched while computing garbage, so
%% gc_map/3 can reuse it instead of re-fetching.
-spec compute_store_updates(store_meta(), #cache_entry{}, non_neg_integer()) ->
        {store_meta(), [store_update()], non_neg_integer(), #{raw_id() => #{binary() => binary()}}}.
compute_store_updates(Metadata, #cache_entry{terms = TermCache, store = Store}, GasLeft) ->
    UsedIds = used_map_ids(Metadata),
    {Regs, Terms} = lists:unzip([{Reg, Term} || {Reg, {Term, Dirty}} <- lists:keysort(1,maps:to_list(TermCache)),
                                                Reg > ?META_STORE_POS, Dirty]),

    %% Go through the store register and find all maps that we want to put in
    %% the store. Terms1 is the updated store registry values (containing only
    %% store maps with empty caches), and Maps contains the new store maps that
    %% we should create.
    {Terms1, Maps} = aeb_fate_maps:allocate_store_maps(UsedIds, Terms),
    NewRegs = lists:zip(Regs, Terms1),

    %% Reference counting and garbage collection. Compute which maps can be
    %% updated inplace (Reuse) and which maps can be garbage collected (Garbage).
    RefCounts = compute_refcounts(NewRegs, Maps, Metadata, Store),
    Metadata1 = update_refcounts(RefCounts, Metadata),
    {Unused, Reuse, Metadata1b, GasLeft1} = compute_reuse_fixpoint(Maps, Metadata1, Store, GasLeft),
    {Garbage, Metadata2, GasLeft2, GcCache} = compute_garbage(Unused, Reuse, Metadata1b, Store, GasLeft1),

    CopyOrInplace = fun(MapId, ?FATE_STORE_MAP(_, Id) = Map) ->
                            case maps:get(Id, Reuse, no_reuse) of
                                MapId -> {update_map, MapId, Map};
                                _     -> {copy_map, MapId, Map}
                            end;
                       (MapId, Map) -> {copy_map, MapId, Map} end,

    Updates = [ {push_term, Reg, Term}    || {Reg, Term} <- NewRegs ] ++
              [ CopyOrInplace(MapId, Map) || {MapId, Map} <- lists:keysort(1, maps:to_list(Maps)) ] ++
              [ {gc_map, RawId}           || RawId <- Garbage ],

    %% It's important (very!) that copy_map runs before update_map, since
    %% update map does a destructive update of the store. To make sure this is
    %% the case we sort the updates.
    Order = fun(push_term)  -> 0;
               (copy_map)   -> 1;
               (update_map) -> 2;
               (gc_map)     -> 3 end,
    Compare = fun(A, B) -> Order(element(1, A)) =< Order(element(1, B)) end,
    {Metadata2, lists:sort(Compare, Updates), GasLeft2, GcCache}.

%% Acc is GasLeft on the optimistic (reuse-only) path, and
%% {SubtreeCache, GasLeft} on the full/copy-refcount path.
compute_reuse_fixpoint(Maps, Metadata, Store, GasLeft) ->
    Unused0 = unused_maps(Metadata),
    NumStoreMaps = length([1 || {_, ?FATE_STORE_MAP(_, _)} <- maps:to_list(Maps)]),
    %% Optimistic fast path: run fixpoint using only reuse-branch refcount
    %% adjustments (no expensive MPT subtree reads). If all store maps end up
    %% reused in-place, the copy branch would have contributed zero adjustments
    %% so the optimistic result is exact.
    case optimistic_reuse_fixpoint(Unused0, Maps, Metadata, Store, 100, GasLeft) of
        {out_of_fuel, GasLeftAtFuelOut} ->
            %% Never trust a non-converged optimistic result: fall through to
            %% the full (deterministic, gas-charged) fixpoint below, carrying
            %% forward the gas the optimistic pass already spent on reads so
            %% those reads are not re-run for free.
            full_reuse_fixpoint(Unused0, Maps, Metadata, Store, 100, {#{}, GasLeftAtFuelOut});
        {ok, UnusedOpt, ReuseOpt, _MetaOpt, GasLeftOpt} ->
            case maps:size(ReuseOpt) =:= NumStoreMaps of
                true ->
                    %% All maps reused in-place, so compute_copy_refcounts
                    %% only takes its cheap branch here (no subtree read).
                    {RefCounts1, {_, GasLeft1}} = compute_copy_refcounts(Metadata, ReuseOpt, Maps, Store, {#{}, GasLeftOpt}),
                    Metadata1 = update_refcounts(RefCounts1, Metadata),
                    {UnusedOpt, ReuseOpt, Metadata1, GasLeft1};
                false ->
                    %% Same reason as the out_of_fuel branch: carry GasLeftOpt,
                    %% not the pre-optimistic-pass GasLeft, into the full path.
                    full_reuse_fixpoint(Unused0, Maps, Metadata, Store, 100, {#{}, GasLeftOpt})
            end
    end.

%% Generic reuse fixpoint loop, parameterised on how refcounts are computed.
%% RefCountFun(Metadata, Reuse, Maps, Store, Acc) -> {RefCounts, Acc1}
%% NOTE: Metadata (not the updated Metadata1) is threaded unchanged into
%% recursive calls — each iteration adjusts the original metadata.
reuse_fixpoint_loop(_RefCountFun, Unused, _Maps, _Metadata, _Store, Fuel, Acc) when Fuel =< 0 ->
    {out_of_fuel, Unused, Acc};
reuse_fixpoint_loop(RefCountFun, Unused, Maps, Metadata, Store, Fuel, Acc) ->
    Reuse = compute_inplace_updates(Unused, Maps),
    {RefCounts1, Acc1} = RefCountFun(Metadata, Reuse, Maps, Store, Acc),
    Metadata1 = update_refcounts(RefCounts1, Metadata),
    Unused1 = unused_maps(Metadata1),
    case Unused1 == Unused of
        true  -> {converged, Unused, Reuse, Metadata1, Acc1};
        false -> reuse_fixpoint_loop(RefCountFun, Unused1, Maps, Metadata, Store, Fuel - 1, Acc1)
    end.

%% Optimistic fixpoint: only computes reuse-branch refcount adjustments.
%% Skips the copy branch (no subtree reads from MPT). Returns `out_of_fuel`
%% on non-convergence rather than a best-effort result.
optimistic_reuse_fixpoint(Unused, Maps, Metadata, Store, Fuel, GasLeft) ->
    case reuse_fixpoint_loop(fun compute_reuse_only_refcounts/5, Unused, Maps, Metadata, Store, Fuel, GasLeft) of
        {converged, U, R, M, GasLeft1} -> {ok, U, R, M, GasLeft1};
        {out_of_fuel, _U, Gas}         -> {out_of_fuel, Gas}
    end.

%% Full fixpoint (original algorithm with subtree cache) — used as fallback.
%% Acc is {SubtreeCache, GasLeft}; compute_copy_refcounts charges GasLeft
%% for every subtree it actually reads (cache misses only).
full_reuse_fixpoint(Unused, Maps, Metadata, Store, Fuel, {SubtreeCache, GasLeft}) ->
    Fun = fun(Meta, Reuse, Ms, St, Acc) ->
        compute_copy_refcounts(Meta, Reuse, Ms, St, Acc)
    end,
    case reuse_fixpoint_loop(Fun, Unused, Maps, Metadata, Store, Fuel, {SubtreeCache, GasLeft}) of
        {converged, U, R, M, {_SC, GasLeft1}} ->
            {U, R, M, GasLeft1};
        {out_of_fuel, _U, _Acc} ->
            %% Deterministic: same Fuel/Metadata/Maps/Store on every node,
            %% so throwing here (finalize/3 already catches out_of_gas) is
            %% reproducible, unlike falling back to a best-effort result.
            throw(out_of_gas)
    end.

perform_store_updates(OldMeta, [Update|Left], Meta, GasLeft, Store, GcCache) ->
    ?DEBUG_PRINT("Update: ~p\n", [Update]),
    {Meta1, Bytes, Store1, GasLeft1} = perform_store_update(OldMeta, Update, {Meta, Store}, GasLeft, GcCache),
    GasLeft2 = spend_size_gas(GasLeft1, Bytes),
    perform_store_updates(OldMeta, Left, Meta1, GasLeft2, Store1, GcCache);
perform_store_updates(_OldMeta, [], Meta, GasLeft, Store, _GcCache) ->
    %% Save the updated metadata at the end
    {Store1, Bytes} = push_term(?META_STORE_POS, Meta, Store),
    GasLeft1 = spend_size_gas(GasLeft, Bytes),
    {Store1, GasLeft1}.

spend_size_gas(GasLeft, Bytes) ->
    ?DEBUG_PRINT("GasLeft: ~w Bytes: ~w\n", [GasLeft, Bytes]),
    case GasLeft - Bytes * aec_governance:store_byte_gas() of
        TooLittle when TooLittle < 0 ->
            throw(out_of_gas);
        Enough ->
            Enough
    end.

%% Cost of a single store read: a fixed floor (MPT traversal + DB fetch,
%% paid regardless of size) plus a per-byte term for the deserialize/copy
%% work on top.
read_gas_cost(Bytes) ->
    aec_governance:store_read_base_gas() + Bytes * aec_governance:store_read_byte_gas().

%% Throwing variant of the read-gas charge, used from finalize-time call
%% sites already inside finalize/3's try/catch of throw:out_of_gas.
spend_read_gas(GasLeft, Bytes) ->
    ?DEBUG_PRINT("GasLeft: ~w ReadBytes: ~w\n", [GasLeft, Bytes]),
    case GasLeft - read_gas_cost(Bytes) of
        TooLittle when TooLittle < 0 ->
            throw(out_of_gas);
        Enough ->
            Enough
    end.

%% Non-throwing (sentinel-returning) variant, shared by execution-time
%% callers (which must abort rather than throw) and finalize-time callers
%% (which convert the sentinel to throw(out_of_gas) themselves).
%% `unmetered` always short-circuits to `unmetered`.
charge_read_gas(unmetered, _Bytes) ->
    unmetered;
charge_read_gas(Gas, Bytes) ->
    case Gas - read_gas_cost(Bytes) of
        TooLittle when TooLittle < 0 -> out_of_gas;
        Enough -> Enough
    end.

%% Byte-size of a subtree read: sum of key + value sizes, order-independent.
-spec subtree_bytes(#{binary() => binary()}) -> non_neg_integer().
subtree_bytes(Subtree) ->
    maps:fold(fun(K, V, Acc) -> Acc + byte_size(K) + byte_size(V) end, 0, Subtree).

-spec perform_store_update(store_meta(), store_update(),
                            {store_meta(), aect_contracts_store:store()},
                            non_neg_integer(),
                            #{raw_id() => #{binary() => binary()}}) ->
        {store_meta(), non_neg_integer(), aect_contracts_store:store(), non_neg_integer()}.
perform_store_update(_OldMeta, {push_term, StorePos, FateVal}, {Meta, Store}, GasLeft, _GcCache) ->
    {Store1, Bytes} = push_term(StorePos, FateVal, Store),
    {Meta, Bytes, Store1, GasLeft};
perform_store_update(OldMeta, {copy_map, MapId, Map}, S, GasLeft, _GcCache) ->
    copy_map(OldMeta, MapId, Map, S, GasLeft);
perform_store_update(_OldMeta, {update_map, MapId, Map}, S, GasLeft, _GcCache) ->
    update_map(MapId, Map, S, GasLeft);
perform_store_update(_OldMeta, {gc_map, MapId}, S, GasLeft, GcCache) ->
    {Meta1, Bytes, Store1} = gc_map(MapId, S, GcCache),
    {Meta1, Bytes, Store1, GasLeft}.

%% Write to a store register
push_term(Pos, FateVal, Store) ->
    Val = aeb_fate_encoding:serialize(FateVal),
    Key = store_key(Pos),
    Bytes = byte_size(Key) + byte_size(Val),
    {aect_contracts_store:put(Key, Val, Store), Bytes}.

%% Allocate a new map.
copy_map(_OldMeta, MapId, Map, {Meta, Store}, GasLeft) when ?IS_FATE_MAP(Map) ->
    %% The RefCount was set in compute_store_updates
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta),
    RawId    = MapId,           %% RawId == MapId for fresh maps
    Size     = maps:size(Map),
    %% Update the metadata with RawId and Size
    Meta1    = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size), Meta),
    %% Write the data
    BinData  = cache_to_bin_data(?FATE_MAP_VALUE(Map)),  %% A map value is a special case of a cache (no tombstones)
    {Store1, Bytes} = write_bin_data(RawId, BinData, Store),
    %% and the subtree node that allows us to call aect_contracts_store:subtree
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Bytes, Store2, GasLeft};
copy_map(OldMeta, MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}, GasLeft) ->
    %% In case of a modified store map we need to copy all the entries for the
    %% old map and then update with the new data (Cache).
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta),
    %% We shouldn't get here if the old map is being garbage collected (should
    %% be update_map instead), but the garbage collection analysis is limited
    %% for nested maps currently, so we keep the old metadata around and look
    %% it up there.
    ?METADATA(OldRawId, _RefCount, OldSize) = get_map_meta(OldId, OldMeta),
    RawId   = MapId,
    OldMap  = aect_contracts_store:subtree(map_data_key(OldRawId), Store),
    %% OldMap is a full subtree read (O(size) MPT work); charge it read-side
    %% before using its contents, same formula as every other subtree read.
    GasLeft1 = spend_read_gas(GasLeft, subtree_bytes(OldMap)),
    NewData = cache_to_bin_data(Cache),
    Size    = OldSize + size_delta(OldMap, NewData),
    Meta1   = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size), Meta),
    %% First copy the old data, then update with the new
    {Store1, Bytes} = write_bin_data(RawId, lists:keysort(1, maps:to_list(OldMap)) ++ NewData, Store),
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Bytes, Store2, GasLeft1}.

%% In-place update of an existing store map. This happens, for instance, when
%% you update a map in the state throwing away the old copy of the it.
update_map(MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}, GasLeft) ->
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta), %% Precomputed
    ?METADATA(RawId, _RefCount, OldSize) = get_map_meta(OldId, Meta),
    NewData = cache_to_bin_data(Cache),
    {Delta, GasLeft1} = size_delta(RawId, Store, NewData, GasLeft),
    Size    = OldSize + Delta,
    {Store1, Bytes} = write_bin_data(RawId, NewData, Store),
    Meta1   = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size),
                           remove_map_meta(OldId, Meta)),

    %% We also need to update the refcounts for nested maps. We already added
    %% refcounts for maps in the Cache, now we have to subtract refcounts for
    %% entries overwritten by the cache. Both this fold's lookup and
    %% size_delta/4's membership check above are real single-key store reads,
    %% so both are charged on the same formula as every other store read.
    {RefCounts, GasLeft2} =
        lists:foldl(fun({Key, _}, {Count, Gas}) ->
                        {D, Gas1} = refcount_delta(Key, false, Store, Gas), %% old store
                        {aeb_fate_maps:refcount_union(D, Count), Gas1}
                    end, {aeb_fate_maps:refcount_zero(), GasLeft1}, NewData),
    Meta2     = update_refcounts(RefCounts, Meta1),
    {Meta2, Bytes, Store1, GasLeft2}.

gc_map(RawId, {Meta, Store}, GcCache) ->
    %% Reuse the subtree already fetched (and charged) by gc_refcounts/4
    %% instead of re-fetching. Fallback re-fetch kept in case GcCache is
    %% ever missing an entry, so a future refactor fails safe, not crashes.
    Data = case maps:find(RawId, GcCache) of
               {ok, Cached} -> Cached;
               error        -> aect_contracts_store:subtree(map_data_key(RawId), Store)
           end,
    Store1 = maps:fold(fun(Key, _, S) -> aect_contracts_store:remove(map_raw_key(RawId, Key), S) end,
                       aect_contracts_store:remove(map_data_key(RawId), Store), Data),
    %% Bytes = 0: already charged read-side in gc_refcounts/4 for this Data.
    {Meta, 0, Store1}.

-type bin_data()  :: [{binary(), binary() | ?FATE_MAP_TOMBSTONE}].
-type map_cache() :: #{fate_val() => fate_val() | ?FATE_MAP_TOMBSTONE}.

-spec cache_to_bin_data(map_cache()) -> bin_data().
cache_to_bin_data(Cache) ->
    [ begin
        KeyBin = aeb_fate_encoding:serialize(K),
        ValBin = case V of ?FATE_MAP_TOMBSTONE -> ?FATE_MAP_TOMBSTONE;
                           _                   -> aeb_fate_encoding:serialize(V)
                end,
        {KeyBin, ValBin}
      end || {K, V} <- lists:keysort(1, maps:to_list(Cache)) ].

-spec write_bin_data(raw_id(), bin_data(), aect_contracts_store:store()) ->
                        {aect_contracts_store:store(), non_neg_integer()}.
write_bin_data(RawId, BinData, Store) ->
    lists:foldl(
        fun({K, ?FATE_MAP_TOMBSTONE}, {S, B}) ->
                {aect_contracts_store:remove(map_raw_key(RawId, K), S),
                 B};
           ({K, V}, {S, B}) ->
                {aect_contracts_store:put(map_raw_key(RawId, K), V, S),
                 B + byte_size(K) + byte_size(V)}
        end, {Store, 0}, BinData).

%% Compute the change in size updating an old map with new entries.
-spec size_delta(#{binary() => binary()}, bin_data()) -> integer().
size_delta(OldMap, NewData) ->
    %% Membership comes from a subtree the caller has already read and paid
    %% for, so there is no gas to thread through here.
    {Delta, unmetered} =
        size_delta_(fun(K, Gas) -> {maps:is_key(K, OldMap), Gas} end, NewData, unmetered),
    Delta.

%% Same as size_delta/2 but instead of a binary map we get a raw_id() and the
%% store, so each key costs a real single-key store read rather than a lookup
%% in an already-read subtree. Charged like every other store read: the
%% value's bytes on a hit, the read_gas_cost/1 floor on a miss -- a miss reads
%% back <<>>, so one expression covers both.
-spec size_delta(raw_id(), aect_contracts_store:store(), bin_data(), non_neg_integer()) ->
        {integer(), non_neg_integer()}.
size_delta(RawId, Store, NewData, GasLeft) ->
    size_delta_(fun(K, Gas) ->
                    Bin = aect_contracts_store:get(map_raw_key(RawId, K), Store),
                    {<<>> /= Bin, spend_read_gas(Gas, byte_size(Bin))}
                end, NewData, GasLeft).

size_delta_(IsKey, NewData, Gas0) ->
    Delta = fun({K, ?FATE_MAP_TOMBSTONE}, {N, Gas}) ->
                 case IsKey(K, Gas) of
                     {true,  Gas1} -> {N - 1, Gas1};
                     {false, Gas1} -> {N, Gas1}
                 end;
               ({K, _}, {N, Gas}) ->
                 case IsKey(K, Gas) of
                     {true,  Gas1} -> {N, Gas1};
                     {false, Gas1} -> {N + 1, Gas1}
                 end end,
    lists:foldl(Delta, {0, Gas0}, NewData).

%% -- Reference counting -----------------------------------------------------

%% Compute refcount deltas from updated store registers and updates to store
%% maps.
compute_refcounts(Regs, Maps, Metadata, Store) ->
    TermRefCount = register_refcounts(Regs, Store),
    MapRefCount  = maps_refcounts(Metadata, Maps, Store),
    aeb_fate_maps:refcount_union(TermRefCount, MapRefCount).

%% Refcount delta from updating the store registers.
register_refcounts(Regs, Store) ->
    aeb_fate_maps:refcount_union(
        [ refcount_delta(store_key(Reg), NewVal, Store)
          || {Reg, NewVal} <- Regs ]).

%% Refcount delta from store map updates.
maps_refcounts(Metadata, Maps, Store) ->
    aeb_fate_maps:refcount_union(
        [ map_refcounts(Metadata, Map, Store)
          || {_, Map} <- lists:keysort(1, maps:to_list(Maps)) ]).

map_refcounts(_Meta, Map, _Store) when ?IS_FATE_MAP(Map) ->
    %% Fresh map, only adds new references.
    aeb_fate_maps:refcount(Map);
map_refcounts(_Meta, ?FATE_STORE_MAP(Cache, _Id), _Store) ->
    %% Note that this does not count as a reference to Id
    maps:fold(fun(_Key, Val, Count) ->
                %% We don't know if this map will be copied or updated in place,
                %% so we shouldn't compute a refcount delta. Instead we conservatively
                %% only look at the new value. Once the copy or update happens we'll take
                %% the delta into account.
                aeb_fate_maps:refcount_union(aeb_fate_maps:refcount(Val), Count)
              end, #{}, Cache).

%% Only compute refcount adjustments for reused maps (lightweight individual lookups).
%% Copied maps contribute zero adjustments — their subtrees are NOT read.
%% Acc is GasLeft: the find_in_store/3 lookups below are charged -- a hit for
%% its bytes, a non-aborting miss for the read floor -- and can signal
%% out_of_gas.
compute_reuse_only_refcounts(Meta, Reuse, Maps, Store, GasLeft) ->
    maps:fold(fun(MapId, ?FATE_STORE_MAP(Cache, Id), {Count, Gas}) ->
                      case maps:get(Id, Reuse, no_reuse) of
                          MapId ->
                              ?METADATA(RawId, _RefCount, _Size) = get_map_meta(Id, Meta),
                              {RemovedValues, Gas1} =
                                  lists:foldl(
                                    fun(Key, {Acc, GasA}) ->
                                            case find_in_store(map_data_key(RawId, Key), Store, GasA) of
                                                {error, out_of_gas}         -> throw(out_of_gas);
                                                error                       -> {Acc, spend_read_gas(GasA, 0)};
                                                {ok, Val, _, _Bytes, GasA1} -> {[Val | Acc], GasA1}
                                            end
                                    end, {[], Gas}, maps:keys(Cache)),
                              Removed = aeb_fate_maps:refcount(RemovedValues),
                              {aeb_fate_maps:refcount_diff(Count, Removed), Gas1};
                          _ ->
                              {Count, Gas}
                      end;
                 (_, _, Acc) -> Acc
              end, {#{}, GasLeft}, Maps).

%% We need to increase the refcounts of maps contained in maps that are being
%% copied. Acc is {SubtreeCache, GasLeft}: the copy branch's cache-miss case
%% charges a full subtree read before its values are deserialized; the
%% reuse branch's per-key lookups are charged too -- hit or miss -- and can
%% signal out_of_gas.
compute_copy_refcounts(Meta, Reuse, Maps, Store, {SubtreeCache, GasLeft}) ->
    maps:fold(fun(MapId, ?FATE_STORE_MAP(Cache, Id), {Count, {SC, Gas}}) ->
                      case maps:get(Id, Reuse, no_reuse) of
                          MapId ->
                              ?METADATA(RawId, _RefCount, _Size) = get_map_meta(Id, Meta),
                              {RemovedValues, Gas1} =
                                  lists:foldl(
                                    fun(Key, {Acc, GasA}) ->
                                            case find_in_store(map_data_key(RawId, Key), Store, GasA) of
                                                {error, out_of_gas}         -> throw(out_of_gas);
                                                error                       -> {Acc, spend_read_gas(GasA, 0)};
                                                {ok, Val, _, _Bytes, GasA1} -> {[Val | Acc], GasA1}
                                            end
                                    end, {[], Gas}, maps:keys(Cache)),
                              Removed = aeb_fate_maps:refcount(RemovedValues),
                              {aeb_fate_maps:refcount_diff(Count, Removed), {SC, Gas1}};
                          _ ->
                              ?METADATA(RawId, _RefCount, _Size) = get_map_meta(Id, Meta),
                              NewKeys = [ aeb_fate_encoding:serialize(Key) || Key <- maps:keys(Cache) ],
                              {OldBin, SC1, Gas1} =
                                  case maps:find(RawId, SC) of
                                      {ok, CachedSubtree} ->
                                          {maps:without(NewKeys, CachedSubtree), SC, Gas};
                                      error ->
                                          FullSubtree = aect_contracts_store:subtree(map_data_key(RawId), Store),
                                          GasA = spend_read_gas(Gas, subtree_bytes(FullSubtree)),
                                          {maps:without(NewKeys, FullSubtree), SC#{RawId => FullSubtree}, GasA}
                                  end,
                              Count1 = aeb_fate_maps:refcount([ aeb_fate_encoding:deserialize(Val) || Val <- maps:values(OldBin) ]),
                              {aeb_fate_maps:refcount_union(Count1, Count), {SC1, Gas1}}
                      end;
                 (_, _, Acc) -> Acc
                end, {#{}, {SubtreeCache, GasLeft}}, Maps).

%% Compute the difference in reference counts caused by performing a store
%% update: refcount(Val) - refcount(OldVal).
-spec refcount_delta(binary(), fate_val() | ?FATE_MAP_TOMBSTONE, aect_contracts_store:store()) ->
        aeb_fate_maps:refcount().
refcount_delta(StoreKey, Val, Store) ->
    New = aeb_fate_maps:refcount(Val),
    Old =
        case aect_contracts_store:get(StoreKey, Store) of
            <<>> -> #{};
            Bin  -> aeb_fate_maps:refcount(aeb_fate_encoding:deserialize(Bin))
        end,
    aeb_fate_maps:refcount_diff(New, Old).

%% Same as refcount_delta/3, but charges the lookup. Used from the
%% finalize-time in-place update path, where the read is a real single-key
%% store read rather than a lookup in an already-charged subtree.
%% Charge-before-work: the bytes are paid before the value is deserialized.
-spec refcount_delta(binary(), fate_val() | ?FATE_MAP_TOMBSTONE,
                     aect_contracts_store:store(), non_neg_integer()) ->
        {aeb_fate_maps:refcount(), non_neg_integer()}.
refcount_delta(StoreKey, Val, Store, GasLeft) ->
    New      = aeb_fate_maps:refcount(Val),
    Bin      = aect_contracts_store:get(StoreKey, Store),
    GasLeft1 = spend_read_gas(GasLeft, byte_size(Bin)),
    Old =
        case Bin of
            <<>> -> #{};
            _    -> aeb_fate_maps:refcount(aeb_fate_encoding:deserialize(Bin))
        end,
    {aeb_fate_maps:refcount_diff(New, Old), GasLeft1}.

%% Write new refcounts to the metadata
-spec update_refcounts(aeb_fate_maps:refcount(), store_meta()) -> store_meta().
update_refcounts(Deltas, Meta) ->
    maps:fold(fun(Id, Delta, M) ->
          maps:update_with(Id,
              fun(?METADATA(RawId, RefCount, Size)) ->
                  ?METADATA(RawId, RefCount + Delta, Size)
              end, ?METADATA(undefined, Delta, undefined), M)
        end, Meta, Deltas).

%% Maps with refcount 0 are no longer needed and can be garbage collected or
%% updated in place.
unused_maps(Metadata) ->
    maps:fold(fun(Id, ?METADATA(_, 0, _), Acc) -> Acc#{ Id => true };
                 (_, _, Acc) -> Acc end, #{}, Metadata).

%% Each map can only be reused once (obviously) so we return a map with the old
%% maps (to be reused) as keys.
compute_inplace_updates(Unused, Maps) ->
    maps:fold(fun(MapId, ?FATE_STORE_MAP(_, OldId), Acc) ->
                      case maps:is_key(OldId, Unused) of
                          true  -> Acc#{ OldId => MapId };
                          false -> Acc
                      end;
                 (_, _, Acc) -> Acc end, #{}, Maps).

%% Maps to be garbage collected are the unused maps except those that are being
%% updated in place. Marking a map for garbage collection requires updating the
%% reference counts for maps referenced by it. This may trigger more garbage
%% collection.
%% GasLeft is charged per level for gc_refcounts/4's subtree reads; GcCache
%% is merged across recursion levels so gc_map/3 can reuse the subtrees.
compute_garbage(Unused, Reuse, Metadata, Store, GasLeft) ->
    Garbage   = maps:keys(Unused) -- maps:keys(Reuse),
    case Garbage of
        []  -> {[], Metadata, GasLeft, #{}};
        _   ->
            {Counts, GcCache, GasLeft1} = gc_refcounts(Garbage, Metadata, Store, GasLeft),
            Metadata1 = update_refcounts(aeb_fate_maps:refcount_union(Counts), Metadata),
            Metadata2 = maps:without(Garbage, Metadata1),
            Unused1   = unused_maps(Metadata2),
            {Garbage1, Metadata3, GasLeft2, GcCache1} = compute_garbage(Unused1, Reuse, Metadata2, Store, GasLeft1),
            GetRawId  = fun(Id) -> ?METADATA(RawId, _, _) = get_map_meta(Id, Metadata), RawId end,
            {lists:map(GetRawId, Garbage) ++ Garbage1, Metadata3, GasLeft2, maps:merge(GcCache, GcCache1)}
    end.

%% Refcount deltas arising from garbage collecting Ids, plus the RawId ->
%% subtree cache built along the way, and the updated GasLeft. Each Id's
%% subtree is charged before its entries are deserialized, and before
%% moving to the next Id (incremental, not one lump sum at the end).
gc_refcounts(Ids, Metadata, Store, GasLeft) ->
    lists:foldl(
      fun(Id, {CountsAcc, GcCacheAcc, GasAcc}) ->
              ?METADATA(RawId, _, _) = get_map_meta(Id, Metadata),
              Data = aect_contracts_store:subtree(map_data_key(RawId), Store),
              GasAcc1 = spend_read_gas(GasAcc, subtree_bytes(Data)),
              Count = aeb_fate_maps:refcount_diff(aeb_fate_maps:refcount_zero(),
                        aeb_fate_maps:refcount_union(
                          [ aeb_fate_maps:refcount(aeb_fate_encoding:deserialize(Val))
                            || Val <- maps:values(Data) ])),
              {[Count | CountsAcc], GcCacheAcc#{RawId => Data}, GasAcc1}
      end, {[], #{}, GasLeft}, Ids).

%% -- Debug ------------------------------------------------------------------

-ifdef(DEBUG).
debug_stores(#store{cache = Cache}) ->
    [ begin
          io:format("Contract: ~p\n- Store\n~s", [Pubkey, debug_store(Store)])
      end || {Pubkey, #cache_entry{ store = Store }} <- maps:to_list(Cache) ],
    ok.

debug_store(Store) ->
    Map  = aect_contracts_store:subtree(<<>>, Store),
    Regs = maps:from_list(
             [ {binary:decode_unsigned(Reg), aeb_fate_encoding:deserialize(Val)}
             || {<<?STORE_KEY_PREFIX, Reg/binary>>, Val} <- maps:to_list(Map) ]),
    Maps = maps:from_list(
             [ case Key of
                    <<>> -> {binary:decode_unsigned(RawId), Val};
                    _    -> {{binary:decode_unsigned(RawId), aeb_fate_encoding:deserialize(Key)},
                             aeb_fate_encoding:deserialize(Val)}
               end || {<<?STORE_MAP_PREFIX, RawId:4/binary, Key/binary>>, Val} <- maps:to_list(Map) ]),
    io_lib:format("  Regs: ~p\n  Maps: ~p\n", [Regs, Maps]).
-endif.
