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
%%%    Use finalize/2 to push the stores back to the chain when the
%%%    fate execution is done.
%%%
%%%  @end
%%%    -------------------------------------------------------------------

-module(aefa_stores).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-export([ finalize/2
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

-spec cache_map_metadata(pubkey(), store()) -> store().
cache_map_metadata(Pubkey, S) ->
    case find_meta_data(Pubkey, S) of
        {ok, _}     -> S;
        {ok, _, S1} -> S1;
        error       -> S
    end.

-spec find_meta_data(pubkey(), store()) -> {ok, map_meta(), store()} | error.
find_meta_data(Pubkey, S) ->
    case find_value_(Pubkey, ?META_STORE_POS, S) of
        {ok, Meta}     -> {ok, Meta, S};
        {ok, Meta, S1} -> {ok, Meta, S1};
        error          -> error
    end.


find_value_(Pubkey, StorePos, #store{cache = Cache} = S) ->
    case find_term(StorePos, maps:get(Pubkey, Cache)) of
        {ok, Term} ->
            {ok, Term};
        {ok, Term, Entry} ->
            {ok, Term, S#store{cache = #{Pubkey => Entry}}};
        error ->
            error
    end.

-spec put_value(pubkey(), non_neg_integer(), fate_val(), store()) -> store().
put_value(Pubkey, StorePos, FateVal, #store{cache = Cache} = S) ->
    Entry = maps:get(Pubkey, Cache),
    Terms = maps:put(StorePos, {FateVal, true}, Entry#cache_entry.terms),
    Entry1 = Entry#cache_entry{terms = Terms, dirty = true},
    S#store{cache = Cache#{Pubkey => Entry1}}.

%%%===================================================================
%%% Write through cache to stores

-spec finalize(aefa_chain_api:state(), store()) -> aefa_chain_api:state().
finalize(API, #store{cache = Cache} = S) ->
    debug_stores(S),
    Stores = maps:fold(fun finalize_entry/3, [], Cache),
    finalize_stores(Stores, API).

finalize_stores([{Pubkey, Store}|Left], API) ->
    API1 = aefa_chain_api:set_contract_store(Pubkey, Store, API),
    finalize_stores(Left, API1);
finalize_stores([], API) ->
    API.

finalize_entry(_Pubkey, #cache_entry{dirty = false}, Acc) ->
    Acc;
finalize_entry(Pubkey, Cache = #cache_entry{store = Store}, Acc) ->
    {ok, Metadata} = find_meta_data_no_cache(Cache), %% Last access so no need to cache here
    {Metadata1, Updates} = compute_store_updates(Metadata, Cache),
    io:format("Updates\n  ~p\n", [Updates]),
    [{Pubkey, perform_store_updates(Updates, Metadata1, Store)} | Acc].

push_term(Pos, FateVal, Store) ->
    Val = aeb_fate_encoding:serialize(FateVal),
    aect_contracts_store:put(store_key(Pos), Val, Store).

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

%% -- Store updates ----------------------------------------------------------

-type store_update() :: {push_term,  pos_integer(), fate_val()}
                      | {copy_map,   map_id(), fate_map()}
                      | {update_map, map_id(), fate_map()}
                      | {gc_map,     map_id()}.

perform_store_updates(Updates, Meta, Store) ->
    {Meta1, Store1} = lists:foldl(fun perform_store_update/2, {Meta, Store}, Updates),
    push_term(?META_STORE_POS, Meta1, Store1).

-spec perform_store_update(store_update(), {store_meta(), aect_contracts_store:store()}) ->
        {store_meta(), aect_contracts_store:store()}.
perform_store_update({push_term, StorePos, FateVal}, {Meta, Store}) ->
    {Meta, push_term(StorePos, FateVal, Store)};
perform_store_update({copy_map, MapId, Map}, S) ->
    copy_map(MapId, Map, S);
perform_store_update({update_map, MapId, Map}, S) ->
    update_map(MapId, Map, S);
perform_store_update({gc_map, MapId}, S) ->
    gc_map(MapId, S).

%%%===================================================================
%%% Store maps
%%%
%%% Maps are saved in the store as follows
%%%
%%%     /store_meta_key()               (store register 0) Map meta data: #{ MapId => ?METADATA(RawId, RefCount, Size) }
%%%     /?STORE_MAP_PREFIX/RawId:32      <<0>> - subtree node
%%%     /?STORE_MAP_PREFIX/RawId:32/Key  Value for Key in map RawId

-define(METADATA(RawId, RefCount, Size), ?FATE_TUPLE({RawId, RefCount, Size})).

-type map_id()     :: non_neg_integer().
-type raw_id()     :: non_neg_integer().
-type ref_count()  :: non_neg_integer().
-type map_meta()   :: ?METADATA(raw_id(), ref_count(), non_neg_integer()).
-type store_meta() :: #{ map_id() => map_meta() }.

empty_store_meta_data() -> #{}.

-spec used_map_ids(store_meta()) -> [map_id()].
used_map_ids(Metadata) ->
    lists:usort(lists:append(
        [ begin
            ?METADATA(RawId, _, _) = Meta,
            [Id, RawId] %% Include RawId to make sure we don't pick a MapId
                        %% clashing with a RawId in use.
          end || {Id, Meta} <- maps:to_list(Metadata) ])).

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

map_data_key(RawId) ->
    <<?STORE_MAP_PREFIX, RawId:32>>.

map_data_key(RawId, Key) ->
    map_raw_key(RawId, aeb_fate_encoding:serialize(Key)).

map_raw_key(RawId, KeyBin) ->
    <<(map_data_key(RawId))/binary, KeyBin/binary>>.

-spec compute_store_updates(store_meta(), aect_contracts_store:store()) -> {store_meta(), [store_update()]}.
compute_store_updates(Metadata, #cache_entry{terms = TermCache, store = Store}) ->
    UsedIds = used_map_ids(Metadata),
    {Regs, Terms} = lists:unzip([{Reg, Term} || {Reg, {Term, Dirty}} <- maps:to_list(TermCache),
                                                Reg > ?META_STORE_POS, Dirty]),
    {_UsedIds1, Terms1, Maps} = aeb_fate_maps:allocate_store_maps(UsedIds, Terms),
    NewRegs = lists:zip(Regs, Terms1),

    %% Reference counting
    TermRefCount = register_refcounts(NewRegs, Store),
    MapRefCount  = maps_refcounts(Metadata, Maps, Store),
    RefCounts    = aeb_fate_maps:refcount_union(TermRefCount, MapRefCount),

    %% Update refcounts
    Metadata1 = update_refcounts(RefCounts, Metadata),
    Unused = unused_maps(Metadata1),
    Reuse  = compute_inplace_updates(Unused, Maps),
    {Garbage, Metadata2} = compute_garbage(Unused, Reuse, Metadata1, Store),

    %% io:format("Refcount delta: ~p\n", [RefCounts]),
    %% io:format("Inplace updates: ~p\n", [Reuse]),
    %% io:format("Garbage: ~p\n", [Garbage]),

    CopyOrInplace = fun(MapId, ?FATE_STORE_MAP(_, Id) = Map) ->
                            case maps:get(Id, Reuse, no_reuse) of
                                MapId -> {update_map, MapId, Map};
                                _     -> {copy_map, MapId, Map}
                            end;
                       (MapId, Map) -> {copy_map, MapId, Map} end,

    Updates = [ {push_term, Reg, Term}    || {Reg, Term} <- NewRegs ] ++
              [ CopyOrInplace(MapId, Map) || {MapId, Map} <- maps:to_list(Maps) ] ++
              [ {gc_map, RawId}           || RawId <- Garbage ],

    {Metadata2, Updates}.

unused_maps(Metadata) ->
    maps:fold(fun(Id, ?METADATA(_, 0, _), Acc) -> Acc#{ Id => true };
                 (_, _, Acc) -> Acc end, #{}, Metadata).

update_refcounts(Deltas, Meta) ->
    maps:fold(fun(Id, Delta, M) ->
          maps:update_with(Id,
              fun(?METADATA(RawId, RefCount, Size)) ->
                  ?METADATA(RawId, RefCount + Delta, Size)
              end, ?METADATA(undefined, Delta, undefined), M)
        end, Meta, Deltas).

compute_inplace_updates(Unused, Maps) ->
    maps:fold(fun(MapId, ?FATE_STORE_MAP(_, OldId), Acc) ->
                      case maps:is_key(OldId, Unused) of
                          true  -> Acc#{ OldId => MapId };
                          false -> Acc
                      end;
                 (_, _, Acc) -> Acc end, #{}, Maps).

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

gc_refcounts(Ids, Metadata, Store) ->
    Count = fun(Id) ->
                ?METADATA(RawId, _, _) = get_map_meta(Id, Metadata),
                Data = aect_contracts_store:subtree(map_data_key(RawId), Store),
                aeb_fate_maps:refcount_union(
                  [ aeb_fate_maps:refcount_diff(
                      aeb_fate_maps:refcount_zero(),
                      aeb_fate_maps:refcount(aeb_fate_encoding:deserialize(Val)))
                    || Val <- maps:values(Data) ])
            end,
    aeb_fate_maps:refcount_union(lists:map(Count, Ids)).

refcount_delta(Val, StoreKey, Store) ->
    New = aeb_fate_maps:refcount(Val),
    Old =
        case aect_contracts_store:get(StoreKey, Store) of
            <<>> -> #{};
            Bin  -> aeb_fate_maps:refcount(aeb_fate_encoding:deserialize(Bin))
        end,
    aeb_fate_maps:refcount_diff(New, Old).

register_refcounts(Regs, Store) ->
    aeb_fate_maps:refcount_union(
        [ refcount_delta(NewVal, store_key(Reg), Store)
          || {Reg, NewVal} <- Regs ]).

maps_refcounts(Metadata, Maps, Store) ->
    aeb_fate_maps:refcount_union(
        [ map_refcounts(Metadata, Map, Store)
          || {_, Map} <- maps:to_list(Maps) ]).

map_refcounts(_Meta, Map, _Store) when ?IS_FATE_MAP(Map) ->
    aeb_fate_maps:refcount(Map);
map_refcounts(Metadata, ?FATE_STORE_MAP(Cache, Id), Store) ->
    %% Note that this does not count as a reference to Id
    maps:fold(fun(Key, Val, Count) ->
                ?METADATA(RawId, _, _) = get_map_meta(Id, Metadata),
                New = refcount_delta(Val, map_data_key(RawId, Key), Store),
                aeb_fate_maps:refcount_union(New, Count)
              end, #{}, Cache).

size_delta(OldMap, NewData) ->
    Delta    = fun({K, ?FATE_MAP_TOMBSTONE}, N) ->
                    case maps:is_key(K, OldMap) of
                        true  -> N - 1;
                        false -> N
                    end;
                  ({K, _}, N) ->
                    case maps:is_key(K, OldMap) of
                        true -> N;
                        false -> N + 1
                    end end,
    lists:foldl(Delta, 0, NewData).

cache_to_bin_data(Cache) ->
    [ begin
        KeyBin = aeb_fate_encoding:serialize(K),
        ValBin = case V of ?FATE_MAP_TOMBSTONE -> ?FATE_MAP_TOMBSTONE;
                           _                   -> aeb_fate_encoding:serialize(V)
                end,
        {KeyBin, ValBin}
      end || {K, V} <- maps:to_list(Cache) ].

write_bin_data(RawId, BinData, Store) ->
    lists:foldl(
        fun({K, ?FATE_MAP_TOMBSTONE}, S) -> aect_contracts_store:remove(map_raw_key(RawId, K), S);
           ({K, V}, S)                   -> aect_contracts_store:put(map_raw_key(RawId, K), V, S)
        end, Store, BinData).

copy_map(MapId, Map, {Meta, Store}) when ?IS_FATE_MAP(Map) ->
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta),
    RawId    = MapId,
    Size     = maps:size(Map),
    Meta1    = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size), Meta),
    Store1   = maps:fold(fun(K, V, S) -> push_map_entry(RawId, K, V, S) end, Store, Map),
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Store2};
copy_map(MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}) ->
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta),
    ?METADATA(OldRawId, _RefCount, OldSize) = get_map_meta(OldId, Meta),
    RawId    = MapId,
    OldMap   = aect_contracts_store:subtree(map_data_key(OldRawId), Store),
    NewData  = cache_to_bin_data(Cache),
    Size    = OldSize + size_delta(OldMap, NewData),
    Meta1   = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size), Meta),
    %% First copy the old data, then update with the new
    Store1   = write_bin_data(RawId, maps:to_list(OldMap) ++ NewData, Store),
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Store2}.

update_map(MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}) ->
    ?METADATA(_, RefCount, _) = get_map_meta(MapId, Meta), %% Precomputed
    ?METADATA(RawId, _RefCount, OldSize) = get_map_meta(OldId, Meta),
    OldMap  = aect_contracts_store:subtree(map_data_key(RawId), Store),
    NewData = cache_to_bin_data(Cache),
    Size    = OldSize + size_delta(OldMap, NewData),
    Store1  = write_bin_data(RawId, NewData, Store),
    Meta1   = put_map_meta(MapId, ?METADATA(RawId, RefCount, Size),
                           remove_map_meta(OldId, Meta)),
    {Meta1, Store1}.

gc_map(RawId, {Meta, Store}) ->
    %% Only the RawId here, we already removed the MapId from the metadata.
    Data   = aect_contracts_store:subtree(map_data_key(RawId), Store),
    Store1 = maps:fold(fun(Key, _, S) -> aect_contracts_store:remove(map_raw_key(RawId, Key), S) end,
                       aect_contracts_store:remove(map_data_key(RawId), Store), Data),

    {Meta, Store1}.

push_map_entry(RawId, Key, ?FATE_MAP_TOMBSTONE, Store) ->
    aect_contracts_store:remove(map_data_key(RawId, Key), Store);
push_map_entry(RawId, Key, Val, Store) ->
    aect_contracts_store:put(map_data_key(RawId, Key),
                             aeb_fate_encoding:serialize(Val), Store).

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

