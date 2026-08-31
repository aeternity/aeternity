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
          remove/2,
          subtree/2,
          subtree_w_cache/2,
          subtree_within_bytes/3,
          write_cache/1,
          serialize_for_client/1 ]).

-export_type([store/0, key/0, val/0]).

-spec write_cache(store()) -> #{key() => val()}.
write_cache(#store{ cache = Cache }) -> Cache.

-spec mtree(store()) -> aeu_mtrees:mtree().
mtree(#store{ mtree = Tree }) -> Tree.

-spec new() -> store().
new() ->
    new(aeu_mtrees:empty()).

-spec new(aeu_mtrees:mtree()) -> store().
new(Tree) ->
    #store{ cache = #{}, read_cache = #{}, mtree = aeu_mp_trees:tree_no_cache(Tree) }.

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
                    Val = aeu_mp_trees:get(Key, Tree),
                    RCache1 = RCache#{Key => Val},
                    {Val, S#store{ read_cache = RCache1 }}
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

-spec contents(store()) -> #{key() := val()}.
contents(Store) ->
    subtree(<<>>, Store).

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
                        {#{}, RCache#{{subtree, Prefix} => true}};    %% subtree is only in cache
                    {ok, Subtree} ->
                        Iterator = aeu_mtrees:iterator(Subtree),
                        Next = aeu_mtrees:iterator_next(Iterator),
                        Map  = find_keys(Next, #{}),
                        CMap = maps:from_list([{<<Prefix/binary, Key/binary>>, Val} ||
                                               {Key, Val} <- maps:to_list(Map)]),
                        {Map, maps:merge(RCache#{{subtree, Prefix} => true}, CMap)}
                end
        end,
    {maps:merge(FromTree, FromCache), S#store{ read_cache = RCache1 }}.

%% Same result as subtree/2, but abandons the traversal once the merged byte
%% count would exceed MaxBytes. Returns no store on purpose: an abandoned walk
%% must never leave {subtree, Prefix} => true and memoize a partial subtree.
-spec subtree_within_bytes(key(), store(), non_neg_integer()) ->
        {ok, #{key() := val()}, non_neg_integer()} | {error, too_many_bytes}.
subtree_within_bytes(Prefix, #store{ cache = Cache, read_cache = RCache, mtree = Tree }, MaxBytes) ->
    FromCache = subtree_from_cache(Prefix, Cache),
    case map_bytes(FromCache) of
        Bytes0 when Bytes0 > MaxBytes ->
            {error, too_many_bytes};
        Bytes0 ->
            case maps:get({subtree, Prefix}, RCache, false) of
                true ->
                    %% Already materialized by an earlier read; nothing to abandon.
                    Merged = maps:merge(subtree_from_cache(Prefix, RCache), FromCache),
                    case map_bytes(Merged) of
                        Bytes when Bytes > MaxBytes -> {error, too_many_bytes};
                        Bytes                       -> {ok, Merged, Bytes}
                    end;
                false ->
                    case aeu_mtrees:read_only_subtree(Prefix, Tree) of
                        {error, no_such_subtree} ->
                            {ok, FromCache, Bytes0};      %% subtree is only in cache
                        {ok, Subtree} ->
                            Iterator = aeu_mtrees:iterator(Subtree),
                            walk_within_bytes(aeu_mtrees:iterator_next(Iterator),
                                              FromCache, Bytes0, MaxBytes)
                    end
            end
    end.

walk_within_bytes('$end_of_table', Acc, Bytes, _MaxBytes) ->
    {ok, Acc, Bytes};
walk_within_bytes({<<>>, _Val, Iter}, Acc, Bytes, MaxBytes) ->
    walk_within_bytes(aeu_mtrees:iterator_next(Iter), Acc, Bytes, MaxBytes);
walk_within_bytes({Key, Val, Iter}, Acc, Bytes, MaxBytes) ->
    case add_within_bytes(Key, Val, Acc, Bytes, MaxBytes) of
        {error, _} = Err ->
            Err;
        {ok, Acc1, Bytes1} ->
            walk_within_bytes(aeu_mtrees:iterator_next(Iter), Acc1, Bytes1, MaxBytes)
    end.

%% Write cache wins over the tree, as in subtree_w_cache/2's merge.
add_within_bytes(Key, _Val, Acc, Bytes, _MaxBytes) when is_map_key(Key, Acc) ->
    {ok, Acc, Bytes};
add_within_bytes(Key, Val, Acc, Bytes, MaxBytes) ->
    case Bytes + byte_size(Key) + byte_size(Val) of
        Bytes1 when Bytes1 > MaxBytes -> {error, too_many_bytes};
        Bytes1                        -> {ok, Acc#{ Key => Val }, Bytes1}
    end.

map_bytes(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc + byte_size(K) + byte_size(V) end, 0, Map).

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

