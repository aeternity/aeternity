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
        , new/0
        , put_contract_store/3
        , put_value/4
        %% Map functions
        , store_map_lookup/4
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

-opaque store() :: #store{}.

-export_type([ store/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> store().
new() ->
  #store{}.

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
finalize(API, #store{cache = Cache}) ->
    Stores = maps:fold(fun finalize_entry/3, [], Cache),
    finalize_stores(Stores, API).

finalize_stores([{Pubkey, Store}|Left], API) ->
    API1 = aefa_chain_api:set_contract_store(Pubkey, Store, API),
    finalize_stores(Left, API1);
finalize_stores([], API) ->
    API.

finalize_entry(_Pubkey, #cache_entry{dirty = false}, Acc) ->
    Acc;
finalize_entry(Pubkey, #cache_entry{terms = Cache, store = Store}, Acc) ->
    UsedIds       = aeb_fate_maps:no_used_ids(),    %% TODO
    {Terms, Maps} = allocate_store_maps(UsedIds, Cache),
    [{Pubkey, push_maps(Maps, lists:foldl(fun push_term/2, Store, Terms))} | Acc].

push_term({StorePos, FateVal}, Store) ->
    Val = aeb_fate_encoding:serialize(FateVal),
    aect_contracts_store:put(store_key(StorePos), Val, Store).

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


-define(MAX_STORE_POS,
        16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff).

-define(STORE_KEY_PREFIX, 0).
-define(STORE_MAP_PREFIX, 1).
-define(STORE_MAP_META_PREFIX, 0).
-define(STORE_MAP_DATA_PREFIX, 1).

store_key(Int) when Int =< ?MAX_STORE_POS, Int > 0 ->
    <<?STORE_KEY_PREFIX, (binary:encode_unsigned(Int))/binary>>.


%%%===================================================================
%%% Store maps
%%%
%%% Maps are saved in the store under the ?STORE_MAP_PREFIX as follows
%%%
%%%     /
%%%     /?STORE_MAP_META_PREFIX/MapId         Meta data for MapId: ?META_DATA(RawId, RefCount, Size)
%%%     /?STORE_MAP_DATA_PREFIX/{RawId, Key}  Value for Key in map RawId

-define(META_DATA(RawId, RefCount, Size), ?FATE_TUPLE({RawId, RefCount, Size})).

store_map_key(MapId) ->
    <<?STORE_MAP_PREFIX, ?STORE_MAP_META_PREFIX,
      (binary:encode_unsigned(MapId))/binary>>.

store_map_entry_key(RawId, Key) ->
    <<?STORE_MAP_PREFIX, ?STORE_MAP_DATA_PREFIX,
      (aeb_fate_encoding:serialize({tuple, {RawId, Key}}))/binary>>.

%% Also discards clean entries.
-spec allocate_store_maps(aeb_fate_maps:used_ids(), fate_terms()) -> {[{integer(), fate_val()}], aeb_fate_maps:maps()}.
allocate_store_maps(UsedIds, Cache) ->
    {Regs, Terms} = lists:unzip([{Reg, Term} || {Reg, {Term, Dirty}} <- maps:to_list(Cache), Dirty]),
    {_UsedIds1, Terms1, Maps} = aeb_fate_maps:allocate_store_maps(UsedIds, Terms),
    {lists:zip(Regs, Terms1), Maps}.
    %% {lists:zip(Regs, Terms), #{}}.

-spec push_maps(aeb_fate_maps:maps(), aect_contracts_store:store()) -> aect_contracts_store:store().
push_maps(Maps, Store) ->
    maps:fold(fun push_map/3, Store, Maps).

push_map(MapId, Map, Store) when ?IS_FATE_MAP(Map) ->
    RawId    = MapId, %% TODO
    RefCount = 0,     %% TODO
    Size     = maps:size(Map),
    Store1   = push_map_meta_data(MapId, ?META_DATA(RawId, RefCount, Size), Store),
    maps:fold(fun(K, V, S) -> push_map_entry(RawId, K, V, S) end, Store1, Map);
push_map(MapId, ?FATE_STORE_MAP(Cache, OldId), Store) ->
    %% io:format("Updating ~p with ~p\n", [OldId, Cache]),
    ?META_DATA(OldRawId, _RefCount, OldSize) = map_meta_data(OldId, Store),
    RawId    = OldRawId, %% TODO
    Size     = OldSize,  %% TODO
    RefCount = 0,        %% TODO
    Store1  = push_map_meta_data(MapId, ?META_DATA(RawId, RefCount, Size), Store),
    maps:fold(fun(K, V, S) -> push_map_entry(RawId, K, V, S) end, Store1, Cache).

push_map_meta_data(MapId, MetaData, Store) ->
    aect_contracts_store:put(store_map_key(MapId), aeb_fate_encoding:serialize(MetaData), Store).

push_map_entry(RawId, Key, ?FATE_MAP_TOMBSTONE, Store) ->
    aect_contracts_store:remove(store_map_entry_key(RawId, Key), Store);
push_map_entry(RawId, Key, Val, Store) ->
    aect_contracts_store:put(store_map_entry_key(RawId, Key),
                             aeb_fate_encoding:serialize(Val), Store).

map_meta_data(MapId, Store) ->
    case find_in_store(store_map_key(MapId), Store) of
        error      -> aefa_fate:abort(bad_store_map_id);
        {ok, Meta} -> Meta
    end.

-spec store_map_lookup(pubkey(), non_neg_integer(), fate_val(), store()) -> {ok, fate_val()} | error.
store_map_lookup(Pubkey, MapId, Key, #store{cache = Cache}) ->
    %% No caching of map entries (yet)
    #cache_entry{ store = Store } = maps:get(Pubkey, Cache),
    ?META_DATA(RawId, _RefCount, _Size) = map_meta_data(MapId, Store),
    case find_in_store(store_map_entry_key(RawId, Key), Store) of
        error     -> error;
        {ok, Val} -> {ok, Val}
    end.

