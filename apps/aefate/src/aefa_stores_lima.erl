%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%
%%%    !!! NOTE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%%
%%%    THIS CODE IS KEPT FOR BACKWARDS COMPATIBILITY.
%%%    DO NOT CHANGE.
%%%
%%%    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%%
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
%%%  @end
%%%    -------------------------------------------------------------------

-module(aefa_stores_lima).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-export([ finalize/3
        , find_value/3
        , has_contract/2
        , initial_contract_store/0
        , new/0
        , put_contract_store/3
        , put_value/4
        %% Map functions
        , cache_map_metadata/2
        , store_map_lookup/4
        , store_map_member/4
        , store_map_to_list/3
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

-ifdef(DEBUG).
-define(DEBUG_STORE(S), debug_stores(S)).
-define(DEBUG_PRINT(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DEBUG_STORE(S), ok).
-define(DEBUG_PRINT(Fmt, Args), ok).
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
                    {'ok', fate_val(), store()}
                  | {'ok', fate_val()}
                  | 'error'.
find_value(Pubkey, StorePos, S) when ?VALID_STORE_POS(StorePos) ->
    find_value_(Pubkey, StorePos, S).

-spec put_value(pubkey(), non_neg_integer(), fate_val(), store()) -> store().
put_value(Pubkey, StorePos, FateVal, #store{cache = Cache} = S) ->
    Entry = maps:get(Pubkey, Cache),
    Terms = maps:put(StorePos, {FateVal, true}, Entry#cache_entry.terms),
    Entry1 = Entry#cache_entry{terms = Terms, dirty = true},
    S#store{cache = Cache#{Pubkey => Entry1}}.

%% -- Local functions --------------------------------------------------------

find_value_(Pubkey, StorePos, #store{cache = Cache} = S) ->
    case find_term(StorePos, maps:get(Pubkey, Cache)) of
        {ok, Term} ->
            {ok, Term};
        {ok, Term, Entry} ->
            {ok, Term, S#store{cache = Cache#{Pubkey => Entry}}};
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
store_map_lookup(Pubkey, MapId, Key, #store{cache = Cache} = S) ->
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(RawId, _RefCount, _Size) = get_map_meta(MapId, Meta),
    case find_in_store(map_data_key(RawId, Key), Store) of
        error     -> {error, S1};
        {ok, Val} -> {{ok, Val}, S1}
    end.

-spec store_map_member(pubkey(), non_neg_integer(), fate_val(), store()) -> {boolean(), store()}.
store_map_member(Pubkey, MapId, Key, #store{cache = Cache} = S) ->
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(RawId, _RefCount, _Size) = get_map_meta(MapId, Meta),
    case aect_contracts_store:get(map_data_key(RawId, Key), Store) of
        <<>> -> {false, S1};
        _Val -> {true,  S1}
    end.

-spec store_map_to_list(pubkey(), non_neg_integer(), store()) -> {[{fate_val(), fate_val()}], store()}.
store_map_to_list(Pubkey, MapId, #store{cache = Cache} = S) ->
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(RawId, _, _) = get_map_meta(MapId, Meta),
    Subtree = aect_contracts_store:subtree(map_data_key(RawId), Store),
    {[ {aeb_fate_encoding:deserialize(K), aeb_fate_encoding:deserialize(V)}
      || {K, V} <- maps:to_list(Subtree) ], S1}.

-spec store_map_size(pubkey(), non_neg_integer(), store()) -> {non_neg_integer(), store()}.
store_map_size(Pubkey, MapId, S) ->
    {ok, Meta, S1} = find_meta_data(Pubkey, S),
    ?METADATA(_, _, Size) = get_map_meta(MapId, Meta),
    {Size, S1}.

%% -- Map metadata -----------------------------------------------------------

-spec find_meta_data(pubkey(), store()) -> {ok, store_meta(), store()} | error.
find_meta_data(Pubkey, S) ->
    case find_value_(Pubkey, ?META_STORE_POS, S) of
        {ok, Meta}     -> {ok, Meta, S};
        {ok, Meta, S1} -> {ok, Meta, S1};
        error          -> error
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
    case find_term(?META_STORE_POS, CacheEntry) of
        {ok, Meta}    -> {ok, Meta};
        {ok, Meta, _} -> {ok, Meta};
        error         -> error
    end.

%%%===================================================================
%%% Entry for one contract

new_contract_cache_entry(Store) ->
    #cache_entry{ store = Store
                , terms = #{}
                , dirty = false
                }.

find_term(StorePos, #cache_entry{terms = Terms} = E) ->
    case maps:find(StorePos, Terms) of
        {ok, {FateVal,_Dirty}} ->
            {ok, FateVal};
        error ->
            case find_in_store(store_key(StorePos), E#cache_entry.store) of
                error ->
                    error;
                {ok, FateVal} ->
                    {ok, FateVal, E#cache_entry{terms = Terms#{StorePos => {FateVal, false}}}}
            end
    end.

find_in_store(Key, Store) ->
    case aect_contracts_store:get(Key, Store) of
        <<>> ->
            error;
        Value ->
            FateVal = aeb_fate_encoding:deserialize(Value),
            {ok, FateVal}
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
    {Metadata1, Updates} = compute_store_updates(Metadata, Cache),
    ?DEBUG_PRINT("Updates\n  ~p\n", [Updates]),

    %% Performing the updates writes the necessary changes to the MP trees.
    {Store1, GasLeft1} = perform_store_updates(Metadata, Updates, Metadata1, GasLeft, Store),
    {[{Pubkey, Store1} | Writes], GasLeft1}.

%%%===================================================================
%%% Store updates

-type store_update() :: {push_term,  pos_integer(), fate_val()}                %% Write to store register
                      | {copy_map,   map_id(), fate_map()}                     %% Create a new map (no inplace update)
                      | {update_map, map_id(), aeb_fate_data:fate_store_map()} %% Update an existing map inplace
                      | {gc_map,     map_id()}.                                %% Garbage collect a map removing all entries

-spec compute_store_updates(store_meta(), #cache_entry{}) -> {store_meta(), [store_update()]}.
compute_store_updates(Metadata, #cache_entry{terms = TermCache, store = Store}) ->
    UsedIds = used_map_ids(Metadata),
    {Regs, Terms} = lists:unzip([{Reg, Term} || {Reg, {Term, Dirty}} <- maps:to_list(TermCache),
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
    Unused    = unused_maps(Metadata1),
    Reuse     = compute_inplace_updates(Unused, Maps),
    RefCounts1 = compute_copy_refcounts(Metadata1, Reuse, Maps, Store),
    Metadata1b = update_refcounts(RefCounts1, Metadata1),
    {Garbage, Metadata2} = compute_garbage(Unused, Reuse, Metadata1b, Store),

    CopyOrInplace = fun(MapId, ?FATE_STORE_MAP(_, Id) = Map) ->
                            case maps:get(Id, Reuse, no_reuse) of
                                MapId -> {update_map, MapId, Map};
                                _     -> {copy_map, MapId, Map}
                            end;
                       (MapId, Map) -> {copy_map, MapId, Map} end,

    Updates = [ {push_term, Reg, Term}    || {Reg, Term} <- NewRegs ] ++
              [ CopyOrInplace(MapId, Map) || {MapId, Map} <- maps:to_list(Maps) ] ++
              [ {gc_map, RawId}           || RawId <- Garbage ],

    %% It's important (very!) that copy_map runs before update_map, since
    %% update map does a destructive update of the store. To make sure this is
    %% the case we sort the updates.
    Order = fun(push_term)  -> 0;
               (copy_map)   -> 1;
               (update_map) -> 2;
               (gc_map)     -> 3 end,
    Compare = fun(A, B) -> Order(element(1, A)) =< Order(element(1, B)) end,
    {Metadata2, lists:sort(Compare, Updates)}.

perform_store_updates(OldMeta, [Update|Left], Meta, GasLeft, Store) ->
    ?DEBUG_PRINT("Update: ~p\n", [Update]),
    {Meta1, Bytes, Store1} =  perform_store_update(OldMeta, Update, {Meta, Store}),
    GasLeft1 = spend_size_gas(GasLeft, Bytes),
    perform_store_updates(OldMeta, Left, Meta1, GasLeft1, Store1);
perform_store_updates(_OldMeta, [], Meta, GasLeft, Store) ->
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

-spec perform_store_update(store_meta(), store_update(), {store_meta(), aect_contracts_store:store()}) ->
        {store_meta(), non_neg_integer(), aect_contracts_store:store()}.
perform_store_update(_OldMeta, {push_term, StorePos, FateVal}, {Meta, Store}) ->
    {Store1, Bytes} = push_term(StorePos, FateVal, Store),
    {Meta, Bytes, Store1};
perform_store_update(OldMeta, {copy_map, MapId, Map}, S) ->
    copy_map(OldMeta, MapId, Map, S);
perform_store_update(_OldMeta, {update_map, MapId, Map}, S) ->
    update_map(MapId, Map, S);
perform_store_update(_OldMeta, {gc_map, MapId}, S) ->
    gc_map(MapId, S).

%% Write to a store register
push_term(Pos, FateVal, Store) ->
    Val = aeb_fate_encoding:serialize(FateVal),
    Key = store_key(Pos),
    Bytes = byte_size(Key) + byte_size(Val),
    {aect_contracts_store:put(Key, Val, Store), Bytes}.

%% Allocate a new map.
copy_map(_OldMeta, MapId, Map, {Meta, Store}) when ?IS_FATE_MAP(Map) ->
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
    {Meta1, Bytes, Store2};
copy_map(OldMeta, MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}) ->
    %% In case of a modified store map we need to copy all the entries for the
    %% old map and then update with the new data (Cache).
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta),
    %% We shouldn't get here if the old map is being garbage collected (should
    %% be update_map instead), but the garbage collection analysis is limited
    %% for nested maps currently, so we keep the old metadata around and look
    %% it up there.
    ?METADATA(OldRawId, _RefCount, OldSize) = get_map_meta(OldId, OldMeta),
    RawId    = MapId,
    OldMap   = aect_contracts_store:subtree(map_data_key(OldRawId), Store),
    NewData  = cache_to_bin_data(Cache),
    Size    = OldSize + size_delta(OldMap, NewData),
    Meta1   = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size), Meta),
    %% First copy the old data, then update with the new
    {Store1, Bytes} = write_bin_data(RawId, maps:to_list(OldMap) ++ NewData, Store),
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Bytes, Store2}.

%% In-place update of an existing store map. This happens, for instance, when
%% you update a map in the state throwing away the old copy of the it.
update_map(MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}) ->
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta), %% Precomputed
    ?METADATA(RawId, _RefCount, OldSize) = get_map_meta(OldId, Meta),
    NewData = cache_to_bin_data(Cache),
    Size    = OldSize + size_delta(RawId, Store, NewData),
    {Store1, Bytes} = write_bin_data(RawId, NewData, Store),
    Meta1   = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size),
                           remove_map_meta(OldId, Meta)),

    %% We also need to update the refcounts for nested maps. We already added
    %% refcounts for maps in the Cache, now we have to subtract refcounts for
    %% entries overwritten by the cache.
    RefCounts = lists:foldl(fun({Key, _}, Count) ->
                                aeb_fate_maps:refcount_union(refcount_delta(Key, false, Store), %% old store
                                                             Count)
                            end, aeb_fate_maps:refcount_zero(), NewData),
    Meta2     = update_refcounts(RefCounts, Meta1),
    {Meta2, Bytes, Store1}.

gc_map(RawId, {Meta, Store}) ->
    %% Only the RawId here, we already removed the MapId from the metadata.
    Data   = aect_contracts_store:subtree(map_data_key(RawId), Store),
    Store1 = maps:fold(fun(Key, _, S) -> aect_contracts_store:remove(map_raw_key(RawId, Key), S) end,
                       aect_contracts_store:remove(map_data_key(RawId), Store), Data),

    {Meta, _Bytes = 0, Store1}.

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
      end || {K, V} <- maps:to_list(Cache) ].

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
    size_delta_(fun(K) -> maps:is_key(K, OldMap) end, NewData).

%% Same as size_delta/2 but instead of a binary map we get a raw_id() and the
%% store.
-spec size_delta(raw_id(), aect_contracts_store:store(), bin_data()) -> integer().
size_delta(RawId, Store, NewData) ->
    size_delta_(fun(K) -> <<>> /= aect_contracts_store:get(map_raw_key(RawId, K), Store) end,
                NewData).

size_delta_(IsKey, NewData) ->
    Delta = fun({K, ?FATE_MAP_TOMBSTONE}, N) ->
                 case IsKey(K) of
                     true  -> N - 1;
                     false -> N
                 end;
               ({K, _}, N) ->
                 case IsKey(K) of
                     true  -> N;
                     false -> N + 1
                 end end,
    lists:foldl(Delta, 0, NewData).

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
          || {_, Map} <- maps:to_list(Maps) ]).

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

%% We need to increase the refcounts of maps contained in maps that are being
%% copied.
compute_copy_refcounts(Meta, Reuse, Maps, Store) ->
    maps:fold(fun(MapId, ?FATE_STORE_MAP(Cache, Id), Count) ->
                      case maps:get(Id, Reuse, no_reuse) of
                          MapId -> Count;   %% Inplace update
                          _ ->
                              %% Note that we already added refcounts for the Cache.
                              ?METADATA(RawId, _RefCount, _Size) = get_map_meta(Id, Meta),
                              NewKeys = [ aeb_fate_encoding:serialize(Key) || Key <- maps:keys(Cache) ],
                              OldBin = maps:without(NewKeys, aect_contracts_store:subtree(map_data_key(RawId), Store)),
                              Count1 = aeb_fate_maps:refcount([ aeb_fate_encoding:deserialize(Val) || Val <- maps:values(OldBin) ]),
                              aeb_fate_maps:refcount_union(Count1, Count)
                      end;
                 (_, _, Count) -> Count
                end, #{}, Maps).

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
compute_garbage(Unused, Reuse, Metadata, Store) ->
    Garbage   = maps:keys(Unused) -- maps:keys(Reuse),
    case Garbage of
        []  -> {[], Metadata};
        _   ->
            Refcounts = gc_refcounts(Garbage, Metadata, Store),
            Metadata1 = update_refcounts(Refcounts, Metadata),
            Metadata2 = maps:without(Garbage, Metadata1),
            Unused1   = unused_maps(Metadata2),
            {Garbage1, Metadata3} = compute_garbage(Unused1, Reuse, Metadata2, Store),
            GetRawId  = fun(Id) -> ?METADATA(RawId, _, _) = get_map_meta(Id, Metadata), RawId end,
            {lists:map(GetRawId, Garbage) ++ Garbage1, Metadata3}
    end.

%% Refcount delta arising from garbage collecting a map.
gc_refcounts(Ids, Metadata, Store) ->
    Count = fun(Id) ->
                ?METADATA(RawId, _, _) = get_map_meta(Id, Metadata),
                %% Here we need to go over the entire map to update the
                %% reference counts, so grab the subtree.
                Data = aect_contracts_store:subtree(map_data_key(RawId), Store),
                %% -sum([ refcount(Val) || Val <- Data ])
                aeb_fate_maps:refcount_diff(aeb_fate_maps:refcount_zero(),
                aeb_fate_maps:refcount_union(
                  [ aeb_fate_maps:refcount(aeb_fate_encoding:deserialize(Val))
                    || Val <- maps:values(Data) ]))
            end,
    aeb_fate_maps:refcount_union(lists:map(Count, Ids)).

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

