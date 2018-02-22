%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Database backends for the Merkle Patricia Trees.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_db_backends).

-export([ accounts_backend/0
        , contracts_backend/0
        , ns_backend/0
        , ns_cache_backend/0
        , oracles_backend/0
        , oracles_cache_backend/0
        ]).

%% Callbacks for aeu_mp_trees_db
-export([ db_commit/2
        , db_get/2
        , db_put/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec accounts_backend() -> aeu_mp_trees_db:db().
accounts_backend() ->
    aeu_mp_trees_db:new(db_spec(accounts)).

-spec contracts_backend() -> aeu_mp_trees_db:db().
contracts_backend() ->
    aeu_mp_trees_db:new(db_spec(contracts)).

-spec ns_backend() -> aeu_mp_trees_db:db().
ns_backend() ->
    aeu_mp_trees_db:new(db_spec(ns)).

-spec ns_cache_backend() -> aeu_mp_trees_db:db().
ns_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(ns_cache)).

-spec oracles_backend() -> aeu_mp_trees_db:db().
oracles_backend() ->
    aeu_mp_trees_db:new(db_spec(oracles)).

-spec oracles_cache_backend() -> aeu_mp_trees_db:db().
oracles_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(oracles_cache)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

db_spec(Type) ->
    #{ handle => Type
     , cache  => {gb_trees, gb_trees:empty()}
     , get    => {?MODULE, db_get}
     , put    => {?MODULE, db_put}
     , commit => {?MODULE, db_commit}
     }.

db_get(Key, {gb_trees, Tree}) ->
    gb_trees:lookup(Key, Tree);
db_get(Key, accounts) ->
    aec_db:find_accounts_node(Key);
db_get(Key, contracts) ->
    aec_db:find_contracts_node(Key);
db_get(Key, ns) ->
    aec_db:find_ns_node(Key);
db_get(Key, ns_cache) ->
    aec_db:find_ns_cache_node(Key);
db_get(Key, oracles) ->
    aec_db:find_oracles_node(Key);
db_get(Key, oracles_cache) ->
    aec_db:find_oracles_cache_node(Key).

db_put(Key, Val, {gb_trees, Tree}) ->
    {gb_trees, gb_trees:enter(Key, Val, Tree)};
db_put(Key, Val, accounts = Handle) ->
    ok = aec_db:write_accounts_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, ns = Handle) ->
    ok = aec_db:write_ns_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, ns_cache = Handle) ->
    ok = aec_db:write_ns_cache_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, contracts = Handle) ->
    ok = aec_db:write_contracts_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, oracles = Handle) ->
    ok = aec_db:write_oracles_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, oracles_cache = Handle) ->
    ok = aec_db:write_oracles_cache_node(Key, Val),
    {ok, Handle}.

db_commit(Handle, {gb_trees, Cache}) ->
    Iter = gb_trees:iterator(Cache),
    db_commit_1(Handle, gb_trees:next(Iter)).

db_commit_1(Handle, none) -> {ok, Handle, {gb_trees, gb_trees:empty()}};
db_commit_1(Handle, {Key, Val, Iter}) ->
    {ok, Handle} = db_put(Key, Val, Handle),
    db_commit_1(Handle, gb_trees:next(Iter)).
