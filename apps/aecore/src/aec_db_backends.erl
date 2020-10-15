%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Database backends for the Merkle Patricia Trees.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_db_backends).

-export([ accounts_backend/0
        , calls_backend/0
        , channels_backend/0
        , contracts_backend/0
        , ns_backend/0
        , ns_cache_backend/0
        , oracles_backend/0
        , oracles_cache_backend/0
        , dirty_accounts_backend/0
        , dirty_calls_backend/0
        , dirty_channels_backend/0
        , dirty_contracts_backend/0
        , dirty_ns_backend/0
        , dirty_ns_cache_backend/0
        , dirty_oracles_backend/0
        , dirty_oracles_cache_backend/0
        ]).

%% Callbacks for aeu_mp_trees_db
-export([ db_drop_cache/1
        , db_get/2
        , db_put/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec accounts_backend() -> aeu_mp_trees_db:db().
accounts_backend() ->
    aeu_mp_trees_db:new(db_spec(accounts)).

-spec dirty_accounts_backend() -> aeu_mp_trees_db:db().
dirty_accounts_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_accounts)).

-spec calls_backend() -> aeu_mp_trees_db:db().
calls_backend() ->
    aeu_mp_trees_db:new(db_spec(calls)).

-spec dirty_calls_backend() -> aeu_mp_trees_db:db().
dirty_calls_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_calls)).

-spec channels_backend() -> aeu_mp_trees_db:db().
channels_backend() ->
    aeu_mp_trees_db:new(db_spec(channels)).

-spec dirty_channels_backend() -> aeu_mp_trees_db:db().
dirty_channels_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_channels)).

-spec contracts_backend() -> aeu_mp_trees_db:db().
contracts_backend() ->
    aeu_mp_trees_db:new(db_spec(contracts)).

-spec dirty_contracts_backend() -> aeu_mp_trees_db:db().
dirty_contracts_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_contracts)).

-spec ns_backend() -> aeu_mp_trees_db:db().
ns_backend() ->
    aeu_mp_trees_db:new(db_spec(ns)).

-spec dirty_ns_backend() -> aeu_mp_trees_db:db().
dirty_ns_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_ns)).

-spec ns_cache_backend() -> aeu_mp_trees_db:db().
ns_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(ns_cache)).

-spec dirty_ns_cache_backend() -> aeu_mp_trees_db:db().
dirty_ns_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_ns_cache)).

-spec oracles_backend() -> aeu_mp_trees_db:db().
oracles_backend() ->
    aeu_mp_trees_db:new(db_spec(oracles)).

-spec dirty_oracles_backend() -> aeu_mp_trees_db:db().
dirty_oracles_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_oracles)).

-spec oracles_cache_backend() -> aeu_mp_trees_db:db().
oracles_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(oracles_cache)).

-spec dirty_oracles_cache_backend() -> aeu_mp_trees_db:db().
dirty_oracles_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(dirty_oracles_cache)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

db_spec(Type) ->
    #{ handle => Type
     , cache  => {gb_trees, gb_trees:empty()}
     , get    => {?MODULE, db_get}
     , put    => {?MODULE, db_put}
     , drop_cache => {?MODULE, db_drop_cache}
     }.

db_get(Key, {gb_trees, Tree}) ->
    gb_trees:lookup(Key, Tree);
db_get(Key, accounts) ->
    aec_db:find_accounts_node(Key);
db_get(Key, dirty_accounts) ->
    aec_db:dirty_find_accounts_node(Key);
db_get(Key, calls) ->
    aec_db:find_calls_node(Key);
db_get(Key, dirty_calls) ->
    aec_db:dirty_find_calls_node(Key);
db_get(Key, channels) ->
    aec_db:find_channels_node(Key);
db_get(Key, dirty_channels) ->
    aec_db:dirty_find_channels_node(Key);
db_get(Key, contracts) ->
    aec_db:find_contracts_node(Key);
db_get(Key, dirty_contracts) ->
    aec_db:dirty_find_contracts_node(Key);
db_get(Key, ns) ->
    aec_db:find_ns_node(Key);
db_get(Key, dirty_ns) ->
    aec_db:dirty_find_ns_node(Key);
db_get(Key, ns_cache) ->
    aec_db:find_ns_cache_node(Key);
db_get(Key, dirty_ns_cache) ->
    aec_db:dirty_find_ns_cache_node(Key);
db_get(Key, oracles) ->
    aec_db:find_oracles_node(Key);
db_get(Key, dirty_oracles) ->
    aec_db:dirty_find_oracles_node(Key);
db_get(Key, oracles_cache) ->
    aec_db:find_oracles_cache_node(Key);
db_get(Key, dirty_oracles_cache) ->
    aec_db:dirty_find_oracles_cache_node(Key).

db_put(Key, Val, {gb_trees, Tree}) ->
    {gb_trees, gb_trees:enter(Key, Val, Tree)};
db_put(Key, Val, Handle) when Handle =:= accounts; Handle =:= dirty_accounts ->
    ok = aec_db:write_accounts_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= channels; Handle =:= dirty_channels ->
    ok = aec_db:write_channels_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= ns; Handle =:= dirty_ns ->
    ok = aec_db:write_ns_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= ns_cache; Handle =:= dirty_ns_cache ->
    ok = aec_db:write_ns_cache_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= calls; Handle =:= dirty_calls ->
    ok = aec_db:write_calls_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= contracts; Handle =:= dirty_contracts ->
    ok = aec_db:write_contracts_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= oracles; Handle =:= dirty_oracles ->
    ok = aec_db:write_oracles_node(Key, Val),
    Handle;
db_put(Key, Val, Handle) when Handle =:= oracles_cache; Handle =:= dirty_oracles_cache ->
    ok = aec_db:write_oracles_cache_node(Key, Val),
    Handle.

db_drop_cache({gb_trees, _}) ->
    gb_trees:empty().
