%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    DB backend for Merkle Patricia Trees
%%%
%%%    The db backend is made to be side effect free for writes, with
%%%    a write cache that collects all new key-value pairs until
%%%    unsafe_write_to_backend/3 is called.
%%% @end
%%%=============================================================================

-module(aeu_mp_trees_db).

-export([ get/2
        , get/3
        , put/3
        , cache_get/2
        , drop_cache/1
        , new/1
        , is_db/1
        , get_cache/1
        , get_handle/1
        , get_module/1
        , unsafe_write_to_backend/3
        ]).

-export([record_fields/1]).

-export([from_db_format/1]).

-export_type([ db/0
             , db_spec/0
             ]).

-record(db, { module :: atom()
            , handle :: handle()
            , cache  :: cache()
            , read_cache = undefined :: read_cache()
            }).

-type mf() :: {module(), atom()}.

-record(old_db, { handle     :: atom()
                , cache      :: cache()
                , drop_cache :: mf()
                , get        :: mf()
                , put        :: mf()
            }).

-opaque db() :: #db{}.

-type db_spec() :: #{ 'module' := atom()
                    , 'cache'  := cache()
                    , 'handle' := handle()
                    , 'read_cache' => read_cache()
                    }.

-type handle() :: term().
-type cache()  :: term().
-type read_cache() :: undefined | term().
-type key()    :: aeu_mp_trees:key().
-type value()  :: aeu_mp_trees:value().

-callback mpt_db_get(key(), cache() | handle()) -> {'value', term()} | 'none'.
-callback mpt_db_put(key(), value(), cache() | handle()) -> cache() | handle().
-callback mpt_db_drop_cache(cache()) -> cache().
-callback mpt_db_read_cache_get(key(), read_cache()) -> {'value', term()} | 'none'.
-callback mpt_db_read_cache_put(key(), value(), read_cache()) -> ok.

-optional_callbacks([ mpt_db_read_cache_get/2
                    , mpt_db_read_cache_put/3
                    ]).

%% ==================================================================
%% Trace support
record_fields(db) -> record_info(fields, db);
record_fields(_ ) -> no.
%% ==================================================================

%%%===================================================================
%%% API
%%%===================================================================

-spec new(db_spec()) -> db().
new(#{ 'module' := Module
     , 'cache'  := Cache
     , 'handle' := Handle
     } = Spec) ->
    #db{ module = Module
       , cache  = Cache
       , handle = Handle
       , read_cache = maps:get(read_cache, Spec, undefined)
       }.

-spec get(key(), db()) -> {'value', value()} | 'none'.
get(Key, DB0) ->
    DB = to_new_db(DB0),
    case int_cache_get(Key, DB) of
        'none' -> read_through(Key, DB);
        {value, _} = Res -> Res
    end.

get(Key, DB0, Map) when is_map(Map) ->
    DB = to_new_db(DB0),
    case int_cache_get(Key, DB) of
        'none' -> read_through(Key, DB, Map);
        {value, _} = Res ->
            Map#{result => Res,
                 source => cache}
    end.

-spec cache_get(key(), db()) -> {'value', value()} | 'none'.
cache_get(Key, DB0) ->
    DB = to_new_db(DB0),
    int_cache_get(Key, DB).

-spec drop_cache(db()) -> db().
drop_cache(DB0) ->
    DB = to_new_db(DB0),
    int_drop_cache(DB).

-spec put(key(), value(), db()) -> db().
put(Key, Val, DB0) ->
    DB = to_new_db(DB0),
    int_cache_put(Key, Val, DB).

-spec unsafe_write_to_backend(key(), value(), db()) -> db().
unsafe_write_to_backend(Key, Val, DB0) ->
    %% NOTE: Disregards the in-memory write cache and does not
    %%       invalidate it. Make sure you know what you are doing!
    %%       This should only be called with the actual cache value.
    %%
    %%       The optional read cache (if attached via `db_spec') is
    %%       populated with the `{Key, Val}' pair we just wrote so the
    %%       very next read of the same MPT node is a cache hit. MPT
    %%       keys are content-addressed (`Key = H(Val)') so this
    %%       write-through can never produce a stale entry.
    DB = to_new_db(DB0),
    DB1 = int_db_put(Key, Val, DB),
    ok = int_read_cache_put(Key, {value, Val}, DB1),
    DB1.

-spec is_db(term()) -> boolean().
is_db(DB) ->
    case to_new_db(DB) of
        #db{} -> true;
        _ -> false
    end.

-spec get_cache(db()) -> cache().
get_cache(DB) ->
    #db{cache = Cache} = to_new_db(DB),
    Cache.

-spec get_handle(db()) -> handle().
get_handle(DB) ->
    #db{handle = Handle} = to_new_db(DB),
    Handle.

-spec get_module(db()) -> atom().
get_module(DB) ->
    #db{module = Module} = to_new_db(DB),
    Module.

-spec from_db_format(db()) -> db().
from_db_format(DB = #db{handle = H, cache = C}) ->
    DB#db{ handle = ensure_map(H), cache = ensure_map(C) };
from_db_format(OldDB) ->
    from_db_format(to_new_db(OldDB)).


%%%===================================================================
%%% Cache
%%%===================================================================

int_cache_get(Key, #db{cache = Cache, module = M}) ->
    M:mpt_db_get(Key, Cache).

int_cache_put(Key, Val, #db{cache = Cache, module = M} = DB) ->
    DB#db{cache = M:mpt_db_put(Key, Val, Cache)}.

int_drop_cache(#db{cache = Cache, module = M} = DB) ->
    DB#db{cache = M:mpt_db_drop_cache(Cache)}.

read_through(Key, DB) ->
    case int_read_cache_get(Key, DB) of
        'none' ->
            Res = int_db_get(Key, DB),
            ok = int_read_cache_put(Key, Res, DB),
            Res;
        {value, _} = Res ->
            Res
    end.

read_through(Key, DB, Map) ->
    case int_read_cache_get(Key, DB) of
        'none' ->
            Map1 = int_db_get(Key, DB, Map),
            ok = int_read_cache_put(Key, maps:get(result, Map1), DB),
            Map1;
        {value, _} = Res ->
            Map#{result => Res,
                 source => read_cache}
    end.

int_read_cache_get(_Key, #db{read_cache = undefined}) ->
    'none';
int_read_cache_get(Key, #db{read_cache = ReadCache, module = M}) ->
    M:mpt_db_read_cache_get(Key, ReadCache).

int_read_cache_put(_Key, 'none', _DB) ->
    ok;
int_read_cache_put(_Key, _Res, #db{read_cache = undefined}) ->
    ok;
int_read_cache_put(Key, {value, Val}, #db{read_cache = ReadCache, module = M}) ->
    M:mpt_db_read_cache_put(Key, Val, ReadCache).

%%%===================================================================
%%% DB
%%%===================================================================

int_db_get(Key, #db{handle = Handle, module = M}) ->
    M:mpt_db_get(Key, Handle).

int_db_get(Key, #db{handle = Handle, module = M}, Map) ->
    M:mpt_db_get(Key, Handle, Map).

int_db_put(Key, Val, #db{handle = Handle, module = M} = DB) ->
    DB#db{handle = M:mpt_db_put(Key, Val, Handle)}.

-spec to_new_db(term()) -> db() | not_db.
to_new_db(#db{} = DB) -> DB;
to_new_db({db, Module, Handle, Cache}) when is_atom(Module) ->
    new(#{ module => Module
         , cache   => Cache
         , handle  => Handle});
to_new_db({db, _, _, _, _, _} = OldDB) ->
    case setelement(1, OldDB, old_db) of
        #old_db{ handle = Handle
               , cache  = Cache
               , drop_cache = {Module, _}
               , get        = {Module, _}
               , put        = {Module, _} } ->
            new(#{ module => Module
                 , cache   => Cache
                 , handle  => Handle});
        _ -> not_db
    end.

-spec ensure_map(map() | dict:dict()) -> map().
ensure_map(Map) when is_map(Map) -> Map;
ensure_map(Dict) ->
    maps:from_list(dict:to_list(Dict)).
