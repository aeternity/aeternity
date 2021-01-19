%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    DB backend for Merkle Patricia Trees
%%%
%%%    The db backend is made to be side effect free for writes, with
%%%    a write cache that collects all new key-value pairs until
%%%    unsafe_write_to_backend/3 is called.
%%%
%%%    TODO: Currently, reads are not cached, only writes.
%%% @end
%%%=============================================================================

-module(aeu_mp_trees_db).

-export([ get/2
        , put/3
        , cache_get/2
        , drop_cache/1
        , new/1
        , is_db/1
        , get_cache/1
        , get_handle/1
        , unsafe_write_to_backend/3
        ]).

-export([record_fields/1]).

-export_type([ db/0
             , db_spec/0
             ]).

-record(db, { module :: atom()
            , handle :: handle()
            , cache  :: cache()
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
                    }.

-type handle() :: term().
-type cache()  :: term().
-type key()    :: aeu_mp_trees:key().
-type value()  :: aeu_mp_trees:value().

-callback mpt_db_get(key(), cache() | handle()) -> {'value', term()} | 'none'.
-callback mpt_db_put(key(), value(), cache() | handle()) -> cache() | handle().
-callback mpt_db_drop_cache(cache()) -> cache().

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
     }) ->
    #db{ module = Module
       , cache  = Cache
       , handle = Handle
       }.

-spec get(key(), db()) -> {'value', value()} | 'none'.
get(Key, DB0) ->
    DB = to_new_db(DB0),
    case int_cache_get(Key, DB) of
        'none' -> int_db_get(Key, DB);
        {value, _} = Res -> Res
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
    %% NOTE: Disregards the actual cache value, and does not invalidate
    %%       the cache. Make sure you know what you are doing!
    %%       This should only be called with the actual cache value.
    DB = to_new_db(DB0),
    int_db_put(Key, Val, DB).

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

%%%===================================================================
%%% Cache
%%%===================================================================

int_cache_get(Key, #db{cache = Cache, module = M}) ->
    M:mpt_db_get(Key, Cache).

int_cache_put(Key, Val, #db{cache = Cache, module = M} = DB) ->
    DB#db{cache = M:mpt_db_put(Key, Val, Cache)}.

int_drop_cache(#db{cache = Cache, module = M} = DB) ->
    DB#db{cache = M:mpt_db_drop_cache(Cache)}.

%%%===================================================================
%%% DB
%%%===================================================================

int_db_get(Key, #db{handle = Handle, module = M}) ->
    M:mpt_db_get(Key, Handle).

int_db_put(Key, Val, #db{handle = Handle, module = M} = DB) ->
    DB#db{handle = M:mpt_db_put(Key, Val, Handle)}.

-spec to_new_db(term()) -> db() | not_db.
to_new_db(#db{} = DB) -> DB;
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
    
