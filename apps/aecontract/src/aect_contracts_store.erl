%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Interface to the contract store.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts_store).

-record(store, { cache :: #{key() => val()},
                 read_cache :: #{key() => val()},
                 mtree :: aeu_mtrees:mtree() }).

-opaque store() :: #store{}.
-type key() :: binary().
-type val() :: binary().

-export([ contents/1,
          size/1,
          get/2,
          get_w_cache/2,
          mtree/1,
          new/0,
          new/1,
          put/3,
          put_map/2,
          put_map_to_read_cache/2,
          read_cache/1,
          remove/2,
          subtree/2,
          subtree_w_cache/2,
          write_cache/1,
          serialize_for_client/1 ]).

-export_type([store/0, key/0, val/0]).

-spec read_cache(store()) -> #{key() => val()}.
read_cache(#store{read_cache = RCache}) -> RCache.

-spec write_cache(store()) -> #{key() => val()}.
write_cache(#store{ cache = Cache }) -> Cache.

-spec mtree(store()) -> aeu_mtrees:mtree().
mtree(#store{ mtree = Tree }) -> Tree.

-spec new() -> store().
new() ->
    new(aeu_mtrees:empty()).

-spec new(aeu_mtrees:mtree()) -> store().
new(Tree) ->
    #store{ cache = #{}, read_cache = #{}, mtree = Tree }.

%% Returns empty binary if key is not in the store.
-spec get(key(), store()) -> val().
get(Key, Store) ->
    {Val, _NewStore} = get_w_cache(Key, Store),
    Val.

-spec get_w_cache(key(), store()) -> {val(), store()}.
get_w_cache(Key, #store{ cache = Cache, read_cache = RCache, mtree = Tree } = S) ->
    case Cache of
        #{ Key := Val } -> {Val, S};
        _               ->
            case RCache of
                #{ Key := Val } -> {Val, S};
                _               ->
                    {Val, Tree1} = aeu_mp_trees:get_w_node_cache(Key, Tree),
                    RCache1 = RCache#{Key => Val},
                    {Val, S#store{ read_cache = RCache1, mtree = Tree1 }}
            end
    end.

-spec remove(key(), store()) -> store().
remove(Key, Store) ->
    put(Key, <<>>, Store).

-spec put(key(), val(), store()) -> store().
put(Key, Val, Store = #store{ cache = Cache }) ->
    Store#store{ cache = Cache#{ Key => Val } }.

-spec put_map(#{key() => val()}, store()) -> store().
put_map(Map, Store = #store{ cache = Cache }) ->
    Store#store{ cache = maps:merge(Cache, Map) }.

%% Pre-populate read_cache with entries from the microblock store batch.
%% Batch entries (newer, from prior txs in this microblock) override stale MPT reads.
-spec put_map_to_read_cache(#{key() => val()}, store()) -> store().
put_map_to_read_cache(Map, Store = #store{read_cache = RCache}) ->
    Store#store{read_cache = maps:merge(RCache, Map)}.

-spec contents(store()) -> #{key() := val()}.
contents(Store) ->
    Map = subtree(<<>>, Store),
    maps:filter(fun(_K, V) -> V /= <<>> end, Map).

-spec size(store()) -> non_neg_integer().
size(Store) ->
    Contents = contents(Store),
    lists:foldl(fun(V, Acc) -> byte_size(V) + Acc end, 0, maps:values(Contents)).

%% Returns a map of all the key/value pairs with the given key as a strict
%% prefix.
-spec subtree(key(), store()) -> #{key() := val()}.
subtree(Prefix, Store) ->
    {Subtree, _NewStore} = subtree_w_cache(Prefix, Store),
    Subtree.

-spec subtree_w_cache(key(), store()) -> {#{key() := val()}, store()}.
subtree_w_cache(Prefix, #store{ cache = Cache, read_cache = RCache, mtree = Tree } = S) ->
    FromCache = subtree_from_cache(Prefix, Cache),
    {FromTree, RCache1} =
        case maps:get({subtree, Prefix}, RCache, false) of
            true ->
                {subtree_from_cache(Prefix, RCache), RCache};
            false ->
                case aeu_mtrees:read_only_subtree(Prefix, Tree) of
                    {error, no_such_subtree} ->
                        %% Sentinel not yet flushed — batch writes live only in read_cache.
                        FromRCache = subtree_from_cache(Prefix, RCache),
                        {FromRCache, RCache#{{subtree, Prefix} => true}};
                    {ok, Subtree} ->
                        Iterator = aeu_mtrees:iterator(Subtree),
                        Next = aeu_mtrees:iterator_next(Iterator),
                        Map  = find_keys(Next, #{}),
                        %% Overlay read_cache (batch writes) on top of MPT data.
                        FromRCache = subtree_from_cache(Prefix, RCache),
                        MergedTree = maps:merge(Map, FromRCache),
                        CMap = maps:from_list([{<<Prefix/binary, Key/binary>>, Val} ||
                                               {Key, Val} <- maps:to_list(MergedTree)]),
                        {MergedTree, maps:merge(RCache#{{subtree, Prefix} => true}, CMap)}
                end
        end,
    {maps:merge(FromTree, FromCache), S#store{ read_cache = RCache1 }}.

subtree_from_cache(Prefix, Cache) ->
    N = byte_size(Prefix),
    maps:from_list([ {Key, Val}
                     || {<<Pre:N/binary, Key/binary>>, Val} <- maps:to_list(Cache),
                        Pre == Prefix, Key /= <<>> ]).

find_keys('$end_of_table', Map) ->
    Map;
find_keys({<<>>, _Val, Iter}, Map) ->
    find_keys(aeu_mtrees:iterator_next(Iter), Map);
find_keys({Key, Val, Iter}, Map) ->
    find_keys(aeu_mtrees:iterator_next(Iter), Map#{ Key => Val }).

-spec serialize_for_client(store()) -> #{binary() := binary()}.
serialize_for_client(Store) ->
    maps:from_list(
        [ {aeser_api_encoder:encode(contract_store_key, Key),
           aeser_api_encoder:encode(contract_store_value, Val)}
          || {Key, Val} <- lists:keysort(1, maps:to_list(contents(Store))) ]
    ).

