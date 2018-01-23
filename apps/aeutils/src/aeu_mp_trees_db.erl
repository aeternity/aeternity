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
        ]).

-export_type([ db/0
             , db_spec/0
             ]).

-record(db, { handle :: handle()
            , cache  :: cache()
            , get    :: get_fun()
            , put    :: put_fun()
            , commit :: commit_fun()
            }).

-opaque db() :: #db{}.

-type db_spec() :: #{ 'get'    := get_fun()
                    , 'put'    := put_fun()
                    , 'cache'  := cache()
                    , 'commit' := commit_fun()
                    , 'handle' := handle()
                    }.

-type handle() :: term().
-type cache()  :: term().
-type key()    :: aeu_mp_trees:key().
-type value()  :: aeu_mp_trees:value().

-type get_fun() :: fun((key(), cache()|handle()) -> {'value', term()}
                                                   | 'none').
-type put_fun() :: fun((key(), value(), cache()) -> cache()).
-type commit_fun() :: fun((handle(), cache()) -> {ok, handle(), cache()}
                                               | {'error', term()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(db_spec()) -> db().
new(#{ 'get'    := GetFun
     , 'put'    := PutFun
     , 'cache'  := Cache
     , 'commit' := CommitFun
     , 'handle' := Handle
     }) ->
    #db{ get    = GetFun
       , put    = PutFun
       , cache  = Cache
       , commit = CommitFun
       , handle = Handle
       }.

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
commit(#db{commit = Commit, cache = Cache, handle = Handle} = DB) ->
    case Commit(Handle, Cache) of
        {ok, NewHandle, NewCache} ->
            {ok, DB#db{handle = NewHandle, cache = NewCache}};
        {error, _} = Error ->
            Error
    end.

-spec is_db(term()) -> boolean().
is_db(#db{}) -> true;
is_db(_) -> false.

%%%===================================================================
%%% Cache
%%%===================================================================

cache_get(Key, #db{cache = Cache, get = Get}) ->
    Get(Key, Cache).

cache_put(Key, Val, #db{cache = Cache, put = Put} = DB) ->
    DB#db{cache = Put(Key, Val, Cache)}.

%%%===================================================================
%%% DB
%%%===================================================================

db_get(Key, #db{handle = Handle, get = Get}) ->
    Get(Key, Handle).
