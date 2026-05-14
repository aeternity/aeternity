%%%-------------------------------------------------------------------
%%% @doc
%%%   Block-scoped cache for deserialized contract metadata objects.
%%%
%%%   Holds one deserialized contract record per pubkey for the duration
%%%   of one keyblock epoch.  Cleared at every keyblock boundary and on
%%%   fork switches that change the key epoch.
%%%
%%%   The contract store (key-value state) is NOT cached here — it is
%%%   always rebuilt by aect_state_tree:add_store/3 from the current MPT
%%%   subtree + per-microblock store_batch.  Only the immutable contract
%%%   metadata (owner, code ref, vm version, deposit, active flag) is
%%%   cached, since that is what is serialized in the contracts MPT and
%%%   is expensive to re-deserialize on every lookup.
%%%
%%%   start/0 is called from aecore_sup before any block processing
%%%   begins.  The table is public so aect_state_tree can call get/put
%%%   directly without a round-trip through a gen_server.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_block_contract_cache).

-export([ start/0
        , get/1
        , put/2
        , clear/0
        , size/0
        ]).

-define(TAB, aec_block_contract_cache).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
    case ets:whereis(?TAB) of
        undefined ->
            try ets:new(?TAB, [ named_table
                              , public
                              , set
                              , {read_concurrency,  true}
                              , {write_concurrency, false}
                              ])
            catch
                error:badarg -> ok   %% another process won the race
            end,
            ok;
        _ ->
            ok
    end.

-spec get(binary()) -> {value, aect_contracts:contract()} | none.
get(Pubkey) ->
    try
        case ets:lookup(?TAB, Pubkey) of
            [{Pubkey, Contract}] -> {value, Contract};
            []                   -> none
        end
    catch error:badarg ->
        _ = start(),
        none
    end.

%% Strips the store state before caching so only the small, immutable
%% metadata portion of the contract record occupies ETS memory.
-spec put(binary(), aect_contracts:contract()) -> ok.
put(Pubkey, Contract) ->
    Bare = aect_contracts:set_state(aect_contracts_store:new(), Contract),
    try
        ets:insert(?TAB, {Pubkey, Bare}),
        ok
    catch error:badarg ->
        _ = start(),
        ok
    end.

-spec clear() -> ok.
clear() ->
    ok = start(),
    true = ets:delete_all_objects(?TAB),
    ok.

-spec size() -> non_neg_integer().
size() ->
    case ets:whereis(?TAB) of
        undefined -> 0;
        _         -> ets:info(?TAB, size)
    end.
