%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    DB backend for Merkle Patricia Trees
%%%
%%%    The db backend is made to be side effect free for writes, with
%%%    a write cache that collects all new key-value pairs until
%%%    commit/1 is called.
%%%
%%%    TODO: Currently, reads are not cached, only writes.
%%% @end
%%%=============================================================================

-module(aeu_mp_trees_db).

-export([ get/2
        , put/3
        , commit/1
        , new/1
        , is_db/1
        , get_cache/1
        , get_handle/1
        ]).

-export_type([ db/0
             , db_spec/0
             ]).

-record(db, { handle :: handle()
            , cache  :: cache()
            , get    :: get_mf()
            , put    :: put_mf()
            , commit :: commit_mf()
            }).

-opaque db() :: #db{}.

-type db_spec() :: #{ 'get'    := get_mf()
                    , 'put'    := put_mf()
                    , 'cache'  := cache()
                    , 'commit' := commit_mf()
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

%% fun((handle(), cache()) -> {ok, handle(), cache()} | {'error', term()}).
-type commit_mf() :: {module(), atom()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(db_spec()) -> db().
new(#{ 'get'    := GetMF
     , 'put'    := PutMF
     , 'cache'  := Cache
     , 'commit' := CommitMF
     , 'handle' := Handle
     }) ->
    validate_exported(put, PutMF, 3),
    validate_exported(get, GetMF, 2),
    validate_exported(commit, CommitMF, 2),
    #db{ get    = GetMF
       , put    = PutMF
       , cache  = Cache
       , commit = CommitMF
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
    case cache_get(Key, DB) of
        'none' -> db_get(Key, DB);
        {value, _} = Res -> Res
    end.

-spec put(key(), value(), db()) -> db().
put(Key, Val, DB) ->
    cache_put(Key, Val, DB).

-spec commit(db()) -> {'ok', db()} | {'error', term()}.
commit(#db{commit = {M, F}, cache = Cache, handle = Handle} = DB) ->
    case M:F(Handle, Cache) of
        {ok, NewHandle, NewCache} ->
            {ok, DB#db{handle = NewHandle, cache = NewCache}};
        {error, _} = Error ->
            Error
    end.

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

cache_get(Key, #db{cache = Cache, get = {M, F}}) ->
    M:F(Key, Cache).

cache_put(Key, Val, #db{cache = Cache, put = {M, F}} = DB) ->
    DB#db{cache = M:F(Key, Val, Cache)}.

%%%===================================================================
%%% DB
%%%===================================================================

db_get(Key, #db{handle = Handle, get = {M, F}}) ->
    M:F(Key, Handle).
