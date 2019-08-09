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
        , store_map_lookup/4
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
find_value(Pubkey, StorePos, #store{cache = Cache} = S) ->
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
    %% debug_stores(S),
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
    {ok, MetaData} = find_meta_data(Cache),
    Updates = compute_store_updates(MetaData, Cache),
    [{Pubkey, perform_store_updates(Updates, MetaData, Store)} | Acc].

push_term(Pos, FateVal, Store) ->
    Val = aeb_fate_encoding:serialize(FateVal),
    aect_contracts_store:put(store_key(Pos), Val, Store).

%%%===================================================================
%%% Entry for one contract

-define(MAX_STORE_POS, 16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff).
-define(META_STORE_POS, 0).

-define(STORE_KEY_PREFIX, 0).
-define(STORE_MAP_PREFIX, 1).

new_contract_cache_entry(Store) ->
    #cache_entry{ store = Store
                , terms = #{}
                , dirty = false
                }.

find_term(StorePos, E) when StorePos > ?META_STORE_POS, StorePos < ?MAX_STORE_POS ->
    find_term_(StorePos, E).

find_term_(StorePos, #cache_entry{terms = Terms} = E) ->
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
perform_store_update(Other, Store) ->
    error({todo, Other}),
    Store.

%%%===================================================================
%%% Store maps
%%%
%%% Maps are saved in the store as follows
%%%
%%%     /store_meta_key()               (store register 0) Map meta data: #{ MapId => ?META_DATA(RawId, RefCount, Size) }
%%%     /?STORE_MAP_PREFIX/RawId:32      <<0>> - subtree node
%%%     /?STORE_MAP_PREFIX/RawId:32/Key  Value for Key in map RawId

-define(META_DATA(RawId, RefCount, Size), ?FATE_TUPLE({RawId, RefCount, Size})).

-type map_id()     :: non_neg_integer().
-type raw_id()     :: non_neg_integer().
-type ref_count()  :: non_neg_integer().
-type map_meta()   :: ?META_DATA(raw_id(), ref_count(), non_neg_integer()).
-type store_meta() :: #{ map_id() => map_meta() }.

empty_store_meta_data() -> #{}.

-spec used_map_ids(store_meta()) -> [map_id()].
used_map_ids(MetaData) ->
    lists:usort(lists:append(
        [ begin
            ?META_DATA(RawId, _, _) = Meta,
            [Id, RawId] %% Include RawId to make sure we don't pick a MapId
                        %% clashing with a RawId in use.
          end || {Id, Meta} <- maps:to_list(MetaData) ])).

-spec put_map_meta(map_id(), map_meta(), store_meta()) -> store_meta().
put_map_meta(MapId, MapMeta, MetaData) ->
    MetaData#{ MapId => MapMeta }.

-spec get_map_meta(map_id(), store_meta() | #cache_entry{}) -> map_meta().
get_map_meta(MapId, #cache_entry{} = Cache) ->
    {ok, Meta} = find_meta_data(Cache),
    get_map_meta(MapId, Meta);
get_map_meta(MapId, Meta) ->
    maps:get(MapId, Meta).

-spec find_meta_data(#cache_entry{}) -> {ok, store_meta()} | error.
find_meta_data(CacheEntry) ->
    case find_term_(?META_STORE_POS, CacheEntry) of
        {ok, Meta} -> {ok, Meta};
        {ok, Meta, _Cache} -> {ok, Meta}; %% TODO: caching
        error -> error
    end.

map_data_key(RawId) ->
    <<?STORE_MAP_PREFIX, RawId:32>>.

map_data_key(RawId, Key) ->
    map_raw_key(RawId, aeb_fate_encoding:serialize(Key)).

map_raw_key(RawId, KeyBin) ->
    <<(map_data_key(RawId))/binary, KeyBin/binary>>.

-spec compute_store_updates(store_meta(), aect_contracts_store:store()) -> [store_update()].
compute_store_updates(MetaData, #cache_entry{terms = TermCache}) ->
    UsedIds = used_map_ids(MetaData),
    io:format("UsedIds = ~p\n", [UsedIds]),
    {Regs, Terms} = lists:unzip([{Reg, Term} || {Reg, {Term, Dirty}} <- maps:to_list(TermCache),
                                                Reg > ?META_STORE_POS, Dirty]),
    {_UsedIds1, Terms1, Maps} = aeb_fate_maps:allocate_store_maps(UsedIds, Terms),
    [ {push_term, Reg, Term} || {Reg, Term} <- lists:zip(Regs, Terms1) ] ++
    [ {copy_map, MapId, Map} || {MapId, Map} <- maps:to_list(Maps) ].

copy_map(MapId, Map, {Meta, Store}) when ?IS_FATE_MAP(Map) ->
    RawId    = MapId,
    RefCount = 0,     %% Set later (TODO: actually set later)
    Size     = maps:size(Map),
    Meta1    = put_map_meta(MapId, ?META_DATA(RawId, RefCount, Size), Meta),
    Store1   = maps:fold(fun(K, V, S) -> push_map_entry(RawId, K, V, S) end, Store, Map),
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Store2};
copy_map(MapId, ?FATE_STORE_MAP(Cache, OldId), {Meta, Store}) ->
    ?META_DATA(OldRawId, _RefCount, OldSize) = get_map_meta(OldId, Meta),
    RawId    = MapId,
    OldMap   = aect_contracts_store:subtree(map_data_key(OldRawId), Store),
    NewData  = [ begin
                     KeyBin = aeb_fate_encoding:serialize(K),
                     ValBin = case V of ?FATE_MAP_TOMBSTONE -> ?FATE_MAP_TOMBSTONE;
                                        _                   -> aeb_fate_encoding:serialize(V)
                              end,
                     {KeyBin, ValBin}
                 end || {K, V} <- maps:to_list(Cache) ],
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
    Size     = lists:foldl(Delta, OldSize, NewData),
    RefCount = 0,        %% Set later (TODO: actually set later)
    Meta1   = put_map_meta(MapId, ?META_DATA(RawId, RefCount, Size), Meta),
    %% First copy the old data, then update with the new
    Store1  = lists:foldl(
                fun({K, ?FATE_MAP_TOMBSTONE}, S) -> aect_contracts_store:remove(map_raw_key(RawId, K), S);
                   ({K, V}, S)                   -> aect_contracts_store:put(map_raw_key(RawId, K), V, S)
                end, Store, maps:to_list(OldMap) ++ NewData),
    Store2   = aect_contracts_store:put(map_data_key(RawId), <<0>>, Store1),
    {Meta1, Store2}.

push_map_entry(RawId, Key, ?FATE_MAP_TOMBSTONE, Store) ->
    aect_contracts_store:remove(map_data_key(RawId, Key), Store);
push_map_entry(RawId, Key, Val, Store) ->
    aect_contracts_store:put(map_data_key(RawId, Key),
                             aeb_fate_encoding:serialize(Val), Store).

-spec store_map_lookup(pubkey(), non_neg_integer(), fate_val(), store()) -> {ok, fate_val()} | error.
store_map_lookup(Pubkey, MapId, Key, #store{cache = Cache}) ->
    %% No caching of map entries (worth it? doesn't seem like a common
    %% usecase). TODO: Need to cache the meta though!
    CacheEntry = #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    ?META_DATA(RawId, _RefCount, _Size) = get_map_meta(MapId, CacheEntry),
    case find_in_store(map_data_key(RawId, Key), Store) of
        error     -> error;
        {ok, Val} -> {ok, Val}
    end.

store_map_to_list(Pubkey, MapId, #store{cache = Cache}) ->
    CacheEntry = #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    ?META_DATA(RawId, _, _) = get_map_meta(MapId, CacheEntry),
    Subtree = aect_contracts_store:subtree(map_data_key(RawId), Store),
    [ {aeb_fate_encoding:deserialize(K), aeb_fate_encoding:deserialize(V)}
      || {K, V} <- maps:to_list(Subtree) ].

store_map_size(Pubkey, MapId, #store{cache = Cache}) ->
    CacheEntry = maps:get(Pubkey, Cache),
    ?META_DATA(_, _, Size) = get_map_meta(MapId, CacheEntry),
    Size.

debug_stores(#store{cache = Cache}) ->
    [ begin
          NoCache = aect_contracts_store:new(aect_contracts_store:mtree(Store)),
          io:format("Contract: ~p\n- Store\n~s- Without cache\n~s",
                    [Pubkey, debug_store(Store), debug_store(NoCache)])
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

