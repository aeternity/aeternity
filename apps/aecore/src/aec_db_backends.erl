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
        , mpt_db_get/3
        , mpt_db_put/3
        ]).

-export([read_cache_tab/1]).

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
    case read_cache_tab(Handle) of
        none ->
            aec_db:lookup_tree_node(Key, Handle);
        {ok, Tab} ->
            case aec_mpt_cache:get(Tab, Key) of
                {value, _} = Res ->
                    Res;
                none ->
                    case aec_db:lookup_tree_node(Key, Handle) of
                        {value, Val} = Res ->
                            ok = aec_mpt_cache:put(Tab, Key, Val),
                            Res;
                        none ->
                            none
                    end
            end
    end.

%% Deliberately never cached: this is the reachability-walk path used by
%% the GC scanner, which must observe real table residency. Adding a cache
%% here would reintroduce the promote-on-read defect (see read_cache_tab/1).
mpt_db_get(Key, {gb_trees, Tree}, Map) when is_map(Map) ->
    Map#{result => gb_trees:lookup(Key, Tree)};
mpt_db_get(Key, Handle, Map) when is_map(Map) ->
    aec_db:lookup_tree_node(Key, Handle, Map).

mpt_db_put(Key, Val, {gb_trees, Tree}) ->
    {gb_trees, gb_trees:enter(Key, Val, Tree)};
mpt_db_put(Key, Val, Handle) ->
    ok = aec_db:enter_tree_node(Key, Val, Handle),
    Handle.

mpt_db_drop_cache({gb_trees, _}) ->
    gb_trees:empty().

%% Decide whether this read may be served from / stored in the shared read
%% cache, and under which table namespace.
%%
%% `aec_db:tree_context_cache_key/1' returns `none' for a tree that has
%% garbage collection enabled. That is load bearing: under GC a read that
%% misses the primary table must reach `aec_db:lookup_tree_node/2' so the
%% node is promoted back into the primary. Serving such a read from cache
%% skips the promotion and the node is lost at the next GC switch.
%%
%% The cache is also bypassed while `aec_db:db_safe_access/0' is on, so
%% every read is re-checksummed in `aec_db:lookup_tree_node_/3'. This
%% covers both the global `chain.db_safe_access' switch and the transient
%% window opened by `aec_db_gc:db_safe_access_scan/1'.
read_cache_tab(Handle) ->
    case aec_mpt_cache:enabled() of
        false ->
            none;
        true ->
            case aec_mpt_cache:gate_safe_access(aec_db:db_safe_access()) of
                false -> none;
                true  -> aec_db:tree_context_cache_key(Handle)
            end
    end.
