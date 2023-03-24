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

-behavior(aeu_mp_trees_db).

%% Callbacks for aeu_mp_trees_db
-export([ mpt_db_drop_cache/1
        , mpt_db_get/2
        , mpt_db_put/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec accounts_backend() -> aeu_mp_trees_db:db().
accounts_backend() ->
    aeu_mp_trees_db:new(db_spec(accounts)).

-spec dirty_accounts_backend() -> aeu_mp_trees_db:db().
dirty_accounts_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, accounts})).

-spec calls_backend() -> aeu_mp_trees_db:db().
calls_backend() ->
    aeu_mp_trees_db:new(db_spec(calls)).

-spec dirty_calls_backend() -> aeu_mp_trees_db:db().
dirty_calls_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, calls})).

-spec channels_backend() -> aeu_mp_trees_db:db().
channels_backend() ->
    aeu_mp_trees_db:new(db_spec(channels)).

-spec dirty_channels_backend() -> aeu_mp_trees_db:db().
dirty_channels_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, channels})).

-spec contracts_backend() -> aeu_mp_trees_db:db().
contracts_backend() ->
    aeu_mp_trees_db:new(db_spec(contracts)).

-spec dirty_contracts_backend() -> aeu_mp_trees_db:db().
dirty_contracts_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, contracts})).

-spec ns_backend() -> aeu_mp_trees_db:db().
ns_backend() ->
    aeu_mp_trees_db:new(db_spec(ns)).

-spec dirty_ns_backend() -> aeu_mp_trees_db:db().
dirty_ns_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, ns})).

-spec ns_cache_backend() -> aeu_mp_trees_db:db().
ns_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(ns_cache)).

-spec dirty_ns_cache_backend() -> aeu_mp_trees_db:db().
dirty_ns_cache_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, ns_cache})).

-spec oracles_backend() -> aeu_mp_trees_db:db().
oracles_backend() ->
    aeu_mp_trees_db:new(db_spec(oracles)).

-spec dirty_oracles_backend() -> aeu_mp_trees_db:db().
dirty_oracles_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, oracles})).

-spec oracles_cache_backend() -> aeu_mp_trees_db:db().
oracles_cache_backend() ->
    aeu_mp_trees_db:new(db_spec(oracles_cache)).

-spec dirty_oracles_cache_backend() -> aeu_mp_trees_db:db().
dirty_oracles_cache_backend() ->
    aeu_mp_trees_db:new(db_spec({dirty, oracles_cache})).

%%%===================================================================
%%% Internal functions
%%%===================================================================

db_spec(Type) ->
    Handle = aec_db:new_tree_context(context(Type), tab_name(Type)),
    #{ handle => Handle
     , cache  => {gb_trees, gb_trees:empty()}
     , module => ?MODULE
     }.

context({Ctxt, _}) when Ctxt == dirty; Ctxt == transaction ->
    Ctxt;
context(_) ->
    transaction.

tab_name({_, T}) -> tab_name(T);
tab_name(T) when is_atom(T) -> T.

mpt_db_get(Key, {gb_trees, Tree}) ->
    gb_trees:lookup(Key, Tree);
mpt_db_get(Key, Handle) ->
    aec_db:lookup_tree_node(Key, Handle).

mpt_db_put(Key, Val, {gb_trees, Tree}) ->
    {gb_trees, gb_trees:enter(Key, Val, Tree)};
mpt_db_put(Key, Val, Handle) ->
    ok = aec_db:enter_tree_node(Key, Val, Handle),
    Handle.

mpt_db_drop_cache({gb_trees, _}) ->
    gb_trees:empty().
