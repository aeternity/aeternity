%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Interface to the contract store.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts_store).

-record(store, { cache :: #{key() => val()},
                 mtree :: aeu_mtrees:mtree() }).

-opaque store() :: #store{}.
-type key() :: binary().
-type val() :: binary().

-export([ contents/1,
          get/2,
          mtree/1,
          new/0,
          new/1,
          put/3,
          put_map/2,
          remove/2,
          subtree/2,
          write_cache/1 ]).

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
    #store{ cache = #{}, mtree = Tree }.

%% Returns empty binary if key is not in the store.
-spec get(key(), store()) -> val().
get(Key, #store{ cache = Cache, mtree = Tree }) ->
    case Cache of
        #{ Key := Val } -> Val;
        _               -> aeu_mp_trees:get(Key, Tree)
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

%% Returns a map of all the key/value pairs with the given key as a strict
%% prefix.
-spec subtree(key(), store()) -> #{key() := val()}.
subtree(Prefix, #store{ cache = Cache, mtree = Tree }) ->
    N = byte_size(Prefix),
    FromCache = maps:from_list(
        [ {Key, Val} || {<<Pre:N/binary, Key/binary>>, Val} <- maps:to_list(Cache),
          Pre == Prefix, Key /= <<>> ]),
    {ok, Subtree} = aeu_mtrees:read_only_subtree(Prefix, Tree),
    Iterator = aeu_mtrees:iterator(Subtree),
    Next = aeu_mtrees:iterator_next(Iterator),
    FromTree = find_keys(Next, #{}),
    maps:merge(FromTree, FromCache).

find_keys('$end_of_table', Map) ->
    Map;
find_keys({<<>>, _Val, Iter}, Map) ->
    find_keys(aeu_mtrees:iterator_next(Iter), Map);
find_keys({Key, Val, Iter}, Map) ->
    find_keys(aeu_mtrees:iterator_next(Iter), Map#{ Key => Val }).

