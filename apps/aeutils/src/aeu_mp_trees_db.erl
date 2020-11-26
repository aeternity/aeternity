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
        , get_module/1
        , unsafe_write_to_backend/3
        ]).

-export([ record_fields/1
        , pp_term/1 ]).

-export_type([ db/0
             , db_spec/0
             ]).

-record(db, { module :: atom()
            , handle :: handle()
            , cache  :: cache()
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

pp_term(#db{ module = Mod
           , cache  = Cache
           , handle = Handle } = DB) ->
    case Mod of
        aeu_mp_trees ->
            {yes, DB#db{ handle = {'$db', dict:to_list(Handle) }
                       , cache  = {'$db', int_list_cache(DB)} }};
        _ ->
            {yes, DB#db{ cache = {'$db', Mod:mpt_db_list_cache(Cache)} }}
    end;
pp_term(_) ->
    no.

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
get(Key, DB) ->
    case int_cache_get(Key, DB) of
        'none' -> int_db_get(Key, DB);
        {value, _} = Res -> Res
    end.

-spec cache_get(key(), db()) -> {'value', value()} | 'none'.
cache_get(Key, DB) ->
    int_cache_get(Key, DB).

-spec drop_cache(db()) -> db().
drop_cache(DB) ->
    int_drop_cache(DB).

-spec put(key(), value(), db()) -> db().
put(Key, Val, DB) ->
    int_cache_put(Key, Val, DB).

-spec unsafe_write_to_backend(key(), value(), db()) -> db().
unsafe_write_to_backend(Key, Val, DB) ->
    %% NOTE: Disregards the actual cache value, and does not invalidate
    %%       the cache. Make sure you know what you are doing!
    %%       This should only be called with the actual cache value.
    int_db_put(Key, Val, DB).

-spec is_db(term()) -> boolean().
is_db(#db{}) -> true;
is_db(_) -> false.

-spec get_cache(db()) -> cache().
get_cache(#db{cache = Cache}) ->
    Cache.

-spec get_handle(db()) -> handle().
get_handle(#db{handle = Handle}) ->
    Handle.

-spec get_module(db()) -> atom().
get_module(#db{module = Module}) ->
    Module.

%%%===================================================================
%%% Cache
%%%===================================================================

int_cache_get(Key, #db{cache = Cache, module = M}) ->
    M:mpt_db_get(Key, Cache).

int_cache_put(Key, Val, #db{cache = Cache, module = M} = DB) ->
    DB#db{cache = M:mpt_db_put(Key, Val, Cache)}.

int_drop_cache(#db{cache = Cache, module = M} = DB) ->
    DB#db{cache = M:mpt_db_drop_cache(Cache)}.

int_list_cache(#db{cache = Cache, module = M}) ->
    M:mpt_db_list_cache(Cache).

%%%===================================================================
%%% DB
%%%===================================================================

int_db_get(Key, #db{handle = Handle, module = M}) ->
    M:mpt_db_get(Key, Handle).

int_db_put(Key, Val, #db{handle = Handle, module = M} = DB) ->
    DB#db{handle = M:mpt_db_put(Key, Val, Handle)}.
