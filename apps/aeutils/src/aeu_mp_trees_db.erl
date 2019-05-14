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

-record(db, { handle :: handle()
            , cache  :: cache()
            , drop_cache :: drop_cache_mf()
            , get    :: get_mf()
            , put    :: put_mf()
            }).

-opaque db() :: #db{}.

-type db_spec() :: #{ 'get'    := get_mf()
                    , 'put'    := put_mf()
                    , 'cache'  := cache()
                    , 'drop_cache' := drop_cache_mf()
                    , 'handle' := handle()
                    }.

-type handle() :: term().
-type cache()  :: term().
-type key()    :: aeu_mp_trees:key().
-type value()  :: aeu_mp_trees:value().

%% TODO: This should be a behavior

%% fun((key(), cache()|handle()) -> {'value', term()} | 'none').
-type get_mf() :: {module(), atom()}.

%% fun((key(), value(), cache()) -> cache()).
-type put_mf() :: {module(), atom()}.

%% fun((cache()) -> cache()).
-type drop_cache_mf() :: {module(), atom()}.

%% ==================================================================
%% Trace support
record_fields(db) -> record_info(fields, db);
record_fields(_ ) -> no.
%% ==================================================================

%%%===================================================================
%%% API
%%%===================================================================

-spec new(db_spec()) -> db().
new(#{ 'get'    := GetMF
     , 'put'    := PutMF
     , 'cache'  := Cache
     , 'drop_cache' := DropCacheMF
     , 'handle' := Handle
     }) ->
    validate_exported(put, PutMF, 3),
    validate_exported(get, GetMF, 2),
    validate_exported(drop_cache, DropCacheMF, 1),
    #db{ get    = GetMF
       , put    = PutMF
       , cache  = Cache
       , drop_cache = DropCacheMF
       , handle = Handle
       }.

validate_exported(Type, {M, F}, A) when is_atom(M), is_atom(F) ->
    try lists:member({F, A}, M:module_info(exports)) of
        true -> ok;
        false -> error({invalid, Type, {M, F, A}})
    catch _:_ ->
            error({invalid, Type, {M, F, A}})
    end;
validate_exported(Type, Other,_A) ->
    error({invalid, Type, Other}).


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

%%%===================================================================
%%% Cache
%%%===================================================================

int_cache_get(Key, #db{cache = Cache, get = {M, F}}) ->
    M:F(Key, Cache).

int_cache_put(Key, Val, #db{cache = Cache, put = {M, F}} = DB) ->
    DB#db{cache = M:F(Key, Val, Cache)}.

int_drop_cache(#db{drop_cache = {M, F}, cache = Cache} = DB) ->
    DB#db{cache = M:F(Cache)}.

%%%===================================================================
%%% DB
%%%===================================================================

int_db_get(Key, #db{handle = Handle, get = {M, F}}) ->
    M:F(Key, Handle).

int_db_put(Key, Val, #db{handle = Handle, put = {M, F}} = DB) ->
    DB#db{handle = M:F(Key, Val, Handle)}.

