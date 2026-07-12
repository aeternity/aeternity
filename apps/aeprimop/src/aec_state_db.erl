%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%% Per-transaction dirty state cache.
%%%
%%% Holds the objects written/deleted by the current transaction and
%%% commits them into the state trees as one tag -> *_state_tree
%%% dispatch. `aeprimop_state' owns the variable environment and the
%%% `#state{}' record and delegates object caching to this module.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_state_db).

-export([ new/0
        , find/3
        , put/3
        , drop/3
        , delete/3
        , commit/2
        ]).

-export_type([state_db/0]).

%% A plain map keyed by {tag, key}; last-writer-wins, and the commit
%% fold is order-independent so the resulting MPT root is identical.
-record(state_db, { cache :: #{ {tag(), term()} => entry() } }).

-opaque state_db() :: #state_db{}.

%% Module-private marker for an object deleted by the current tx, kept
%% in the cache instead of erasing the entry so a same-tx read returns
%% `deleted' rather than falling through to the trees. Objects are
%% records/tuples, never this atom, so it cannot collide with a value.
-define(TOMBSTONE, '$aec_state_db_tombstone').
-type entry() :: object() | ?TOMBSTONE.

-type tag() :: account | auth_call | call | channel | contract
             | oracle | oracle_query | commitment | name_auction | name.
-type object() :: aec_accounts:account()
                 | aect_call:call()
                 | aect_contracts:contract()
                 | aens_auctions:auction()
                 | aens_commitments:commitment()
                 | aens_names:name()
                 | aeo_oracles:oracle()
                 | aeo_query:query()
                 | aesc_channels:channel().

-define(IS_TAG(X), ((X =:= account)
    orelse (X =:= auth_call)
    orelse (X =:= call)
    orelse (X =:= channel)
    orelse (X =:= contract)
    orelse (X =:= oracle)
    orelse (X =:= oracle_query)
    orelse (X =:= commitment)
    orelse (X =:= name_auction)
    orelse (X =:= name))
).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> state_db().
new() ->
    #state_db{cache = #{}}.

%% The key must already be variable-resolved by the caller.
%% `deleted' means the object was tombstoned in this tx: the caller
%% must treat it as absent and must NOT fall through to the trees
%% (which may still hold the pre-delete value).
-spec find(tag(), term(), state_db()) -> {value, object()} | deleted | none.
find(Tag, Key, #state_db{cache = C}) when ?IS_TAG(Tag) ->
    case maps:find({Tag, Key}, C) of
        {ok, ?TOMBSTONE} -> deleted;
        {ok, Val}        -> {value, Val};
        error            -> none
    end.

%% Tombstone an object (key already variable-resolved). Unlike `drop/3'
%% which just forgets a cached value, this records an explicit delete
%% that `find/3' reports as `deleted' and `commit/2' applies as a
%% sub-tree delete.
-spec delete(tag(), term(), state_db()) -> state_db().
delete(Tag, Key, #state_db{cache = C} = S) when ?IS_TAG(Tag) ->
    S#state_db{cache = C#{{Tag, Key} => ?TOMBSTONE}}.

-spec drop(tag(), term(), state_db()) -> state_db().
drop(channel, Hash, #state_db{cache = C} = S) ->
    S#state_db{cache = maps:remove({channel, Hash}, C)};
drop(name_auction, Hash, #state_db{cache = C} = S) ->
    S#state_db{cache = maps:remove({name_auction, Hash}, C)};
drop(contract, PK, #state_db{cache = C} = S) ->
    S#state_db{cache = maps:remove({contract, PK}, C)};
drop(account, PK, #state_db{cache = C} = S) ->
    S#state_db{cache = maps:remove({account, PK}, C)};
drop(commitment, Hash, #state_db{cache = C} = S) ->
    S#state_db{cache = maps:remove({commitment, Hash}, C)}.

-spec put(tag(), object(), state_db()) -> state_db().
put(account, Val, #state_db{cache = C} = S) ->
    Pubkey = aec_accounts:pubkey(Val),
    S#state_db{cache = C#{{account, Pubkey} => Val}};
put(auth_call, Val, #state_db{cache = C} = S) ->
    Id = aect_call:id(Val),
    Pubkey = aect_call:caller_pubkey(Val),
    S#state_db{cache = C#{{auth_call, {Pubkey, Id}} => Val}};
put(call, Val, #state_db{cache = C} = S) ->
    Id = aect_call:id(Val),
    S#state_db{cache = C#{{call, Id} => Val}};
put(channel, Val, #state_db{cache = C} = S) ->
    Pubkey = aesc_channels:pubkey(Val),
    S#state_db{cache = C#{{channel, Pubkey} => Val}};
put(contract, Val, #state_db{cache = C} = S) ->
    Pubkey = aect_contracts:pubkey(Val),
    S#state_db{cache = C#{{contract, Pubkey} => Val}};
put(commitment, Val, #state_db{cache = C} = S) ->
    Hash = aens_commitments:hash(Val),
    S#state_db{cache = C#{{commitment, Hash} => Val}};
put(name_auction, Val, #state_db{cache = C} = S) ->
    Hash = aens_auctions:hash(Val),
    S#state_db{cache = C#{{name_auction, Hash} => Val}};
put(name, Val, #state_db{cache = C} = S) ->
    Hash = aens_names:hash(Val),
    S#state_db{cache = C#{{name, Hash} => Val}};
put(oracle, Val, #state_db{cache = C} = S) ->
    Pubkey = aeo_oracles:pubkey(Val),
    S#state_db{cache = C#{{oracle, Pubkey} => Val}};
put(oracle_query, Val, #state_db{cache = C} = S) ->
    Pubkey = aeo_query:oracle_pubkey(Val),
    QueryId = aeo_query:id(Val),
    S#state_db{cache = C#{{oracle_query, {Pubkey, QueryId}} => Val}}.

%% Commit the dirty cache into the trees in one fold — the single
%% tag -> *_state_tree dispatch.  Keys are distinct and last-writer-wins
%% is already resolved in the map; fold order is irrelevant to the root
%% as long as the per-kind sub-tree lookups stay batch-aware (see the
%% invariant on the contract clause below).
-spec commit(state_db(), aec_trees:trees()) -> aec_trees:trees().
commit(#state_db{cache = C}, Trees) ->
    maps:fold(fun write_through_fun/3, Trees, C).

%% TODO: a dirty flag could avoid re-writing unchanged objects.
%%
%% Tombstone arms come first: ?TOMBSTONE is an atom literal while the
%% object arms bind a variable that would otherwise also match it. A
%% tombstone dispatches to the kind's sub-tree delete (itself batched
%% until `aec_trees:flush_state_batches/1').
-spec write_through_fun({tag(), _}, entry(), aec_trees:trees()) ->
                               aec_trees:trees().
write_through_fun({account, Pubkey}, ?TOMBSTONE, Trees) ->
    ATrees = aec_trees:accounts(Trees),
    ATrees1 = aec_accounts_trees:delete(Pubkey, ATrees),
    aec_trees:set_accounts(Trees, ATrees1);
write_through_fun({contract, Pubkey}, ?TOMBSTONE, Trees) ->
    CTree = aec_trees:contracts(Trees),
    CTree1 = aect_state_tree:delete_contract(Pubkey, CTree),
    aec_trees:set_contracts(Trees, CTree1);
write_through_fun({account, _Pubkey}, Account, Trees) ->
    ATrees = aec_trees:accounts(Trees),
    ATrees1 = aec_accounts_trees:enter(Account, ATrees),
    aec_trees:set_accounts(Trees, ATrees1);
write_through_fun({auth_call, {_Pubkey, _Id}}, Call, Trees) ->
    CTree = aec_trees:calls(Trees),
    CTree1 = aect_call_state_tree:enter_auth_call(Call, CTree),
    aec_trees:set_calls(Trees, CTree1);
write_through_fun({call, _Id}, Call, Trees) ->
    CTree = aec_trees:calls(Trees),
    CTree1 = aect_call_state_tree:insert_call(Call, CTree),
    aec_trees:set_calls(Trees, CTree1);
write_through_fun({channel, _Pubkey}, Channel, Trees) ->
    CTree = aec_trees:channels(Trees),
    CTree1 = aesc_state_tree:enter(Channel, CTree),
    aec_trees:set_channels(Trees, CTree1);
write_through_fun({contract, Pubkey}, Contract, Trees) ->
    %% Insert (new) vs enter (update) is decided by looking Pubkey up in
    %% the trees. INVARIANT: aect_state_tree:lookup_contract/3 MUST stay
    %% batch-aware (consult contract_meta_batch before the MPT). That is
    %% what keeps this fold order-independent and the contract metadata
    %% root deterministic — do not change it to a raw-MPT read.
    CTree = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Pubkey, CTree, [no_store]) of
        {value, _} ->
            CTree1 = aect_state_tree:enter_contract(Contract, CTree),
            aec_trees:set_contracts(Trees, CTree1);
        none ->
            CTree1 = aect_state_tree:insert_contract(Contract, CTree),
            aec_trees:set_contracts(Trees, CTree1)
    end;
write_through_fun({commitment, _Hash}, Commitment, Trees) ->
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_commitment(Commitment, NTree),
    aec_trees:set_ns(Trees, NTree1);
write_through_fun({name_auction, _Hash}, Name, Trees) ->
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_name_auction(Name, NTree),
    aec_trees:set_ns(Trees, NTree1);
write_through_fun({name, _Hash}, Name, Trees) ->
    NTree = aec_trees:ns(Trees),
    NTree1 = aens_state_tree:enter_name(Name, NTree),
    aec_trees:set_ns(Trees, NTree1);
write_through_fun({oracle, _Pubkey}, Oracle, Trees) ->
    OTrees = aec_trees:oracles(Trees),
    OTrees1 = aeo_state_tree:enter_oracle(Oracle, OTrees),
    aec_trees:set_oracles(Trees, OTrees1);
write_through_fun({oracle_query, {_Pubkey, _Id}}, Query, Trees) ->
    OTrees = aec_trees:oracles(Trees),
    OTrees1 = aeo_state_tree:enter_query(Query, OTrees),
    aec_trees:set_oracles(Trees, OTrees1).
