%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Unit tests for the per-tx state cache tombstone.
%%%
%%%    Covers `aec_state_db:delete/3' and the `deleted' answer of
%%%    `find/3', and checks that committing a tombstone yields the same
%%%    state as an immediate sub-tree delete.
%%% @end
%%%=============================================================================
-module(aec_state_db_tombstone_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").

state_db_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"find on empty cache is none",            fun find_empty/0}
     , {"put then find returns the value",        fun put_find/0}
     , {"delete then find returns deleted",       fun delete_find/0}
     , {"put then delete: last writer wins",      fun put_then_delete/0}
     , {"delete then put: last writer wins",      fun delete_then_put/0}
     , {"drop only forgets; find falls through",  fun drop_vs_delete/0}
     , {"commit tombstone == immediate delete",   fun commit_tombstone_equiv/0}
     , {"deleted contract: *_without_store absent, no crash",
        fun contract_without_store_after_delete/0}
     , {"present contract: *_without_store still returns it",
        fun contract_without_store_present/0}
     , {"find_x deleted arm: no resurrection from trees",
        fun find_x_deleted_no_resurrection/0}
     , {"commit create then update == immediate insert+enter",
        fun commit_contract_create_then_update_equiv/0}
     , {"commit contract tombstone == immediate delete_contract",
        fun commit_contract_tombstone_equiv/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%%%===================================================================
%%% Per-tx cache state machine
%%%===================================================================

find_empty() ->
    ?assertEqual(none, aec_state_db:find(account, pk(1), aec_state_db:new())).

put_find() ->
    A   = aec_accounts:new(pk(1), 100),
    SDB = aec_state_db:put(account, A, aec_state_db:new()),
    ?assertEqual({value, A}, aec_state_db:find(account, pk(1), SDB)).

delete_find() ->
    SDB = aec_state_db:delete(account, pk(1), aec_state_db:new()),
    %% `deleted' (not `none'): the caller must NOT fall through to the
    %% trees and resurrect a pre-delete value.
    ?assertEqual(deleted, aec_state_db:find(account, pk(1), SDB)).

put_then_delete() ->
    A    = aec_accounts:new(pk(1), 100),
    SDB0 = aec_state_db:put(account, A, aec_state_db:new()),
    SDB1 = aec_state_db:delete(account, pk(1), SDB0),
    ?assertEqual(deleted, aec_state_db:find(account, pk(1), SDB1)).

delete_then_put() ->
    A    = aec_accounts:new(pk(1), 42),
    SDB0 = aec_state_db:delete(account, pk(1), aec_state_db:new()),
    SDB1 = aec_state_db:put(account, A, SDB0),
    ?assertEqual({value, A}, aec_state_db:find(account, pk(1), SDB1)).

%% `drop' merely forgets a cached value (trees stay authoritative) —
%% find is `none' (fall through).  `delete' tombstones — find is
%% `deleted' (do NOT fall through).
drop_vs_delete() ->
    A    = aec_accounts:new(pk(1), 7),
    Base = aec_state_db:put(account, A, aec_state_db:new()),
    ?assertEqual(none,    aec_state_db:find(account, pk(1),
                            aec_state_db:drop(account, pk(1), Base))),
    ?assertEqual(deleted, aec_state_db:find(account, pk(1),
                            aec_state_db:delete(account, pk(1), Base))).

%%%===================================================================
%%% Non-breaking: committing a tombstone == immediate sub-tree delete
%%%===================================================================

commit_tombstone_equiv() ->
    PK = pk(9),
    A  = aec_accounts:new(PK, 1000),
    %% Materialise an account (one microblock), flushed into the MPT.
    T0  = aec_trees:new(),
    T1  = aec_state_db:commit(aec_state_db:put(account, A, aec_state_db:new()), T0),
    T1f = aec_trees:flush_state_batches(T1),
    ?assertMatch({value, _},
                 aec_accounts_trees:lookup(PK, aec_trees:accounts(T1f))),

    %% Reference: delete straight through the sub-tree.
    Ref = aec_trees:flush_state_batches(
            aec_trees:set_accounts(
              T1f, aec_accounts_trees:delete(
                     PK, aec_trees:accounts(T1f)))),

    %% StateDB path: tombstone in the per-tx cache, then commit.
    SDB  = aec_state_db:delete(account, PK, aec_state_db:new()),
    Sdb  = aec_trees:flush_state_batches(aec_state_db:commit(SDB, T1f)),

    ?assertEqual(aec_trees:hash(Ref), aec_trees:hash(Sdb)),
    ?assertEqual(none, aec_accounts_trees:lookup(PK, aec_trees:accounts(Sdb))).

%%%===================================================================
%%% Contract read funnel: a tombstoned contract must be absent through
%%% find/get_contract_without_store (which bypass find_x), not crash.
%%%===================================================================

contract_without_store_after_delete() ->
    PK = pk(11),
    S0 = aeprimop_state:new(aec_trees:new(), aetx_env:tx_env(1)),
    S1 = aeprimop_state:delete_contract(PK, S0),     %% per-tx tombstone
    ?assertEqual(none, aeprimop_state:find_contract_without_store(PK, S1)),
    ?assertException(error, {aeprimop_state, contract_does_not_exist},
                     aeprimop_state:get_contract_without_store(PK, S1)).

contract_without_store_present() ->
    C  = new_contract(aeser_id:create(account, pk(12))),
    PK = aect_contracts:pubkey(C),
    S0 = aeprimop_state:new(aec_trees:new(), aetx_env:tx_env(1)),
    S1 = aeprimop_state:put_contract(C, S0),
    ?assertMatch({value, _}, aeprimop_state:find_contract_without_store(PK, S1)),
    ?assertMatch(_, aeprimop_state:get_contract_without_store(PK, S1)).

%% The account is materialised in the trees; deleting it via the per-tx
%% cache must read as absent and must NOT fall back to the trees.
find_x_deleted_no_resurrection() ->
    PK    = pk(13),
    A     = aec_accounts:new(PK, 1234),
    AT    = aec_accounts_trees:flush_account_batch(
              aec_accounts_trees:enter(A, aec_accounts_trees:empty_with_backend())),
    Trees = aec_trees:set_accounts(aec_trees:new(), AT),
    S0    = aeprimop_state:new(Trees, aetx_env:tx_env(1)),
    ?assertMatch({_, _}, aeprimop_state:find_account(PK, S0)),  %% visible
    S1    = aeprimop_state:delete_account(PK, S0),
    ?assertEqual(none, aeprimop_state:find_account(PK, S1)).     %% no resurrection

%%%===================================================================
%%% commit dispatch: insert/enter decision + contract tombstone
%%%===================================================================

%% Create (tx1) then update (tx2) through aec_state_db:commit must yield
%% the same root as an immediate insert+enter on the sub-tree. Exercises
%% the batch-aware insert/enter decision in write_through_fun.
commit_contract_create_then_update_equiv() ->
    C0 = new_contract(aeser_id:create(account, pk(14))),
    PK = aect_contracts:pubkey(C0),
    St = aect_contracts_store:put_map(#{<<1>> => <<9>>},
                                      aect_contracts_store:new()),
    C1 = aect_contracts:set_state(St, C0),

    T1 = aec_trees:flush_state_batches(
           aec_state_db:commit(aec_state_db:put(contract, C0, aec_state_db:new()),
                               aec_trees:new())),
    T2 = aec_trees:flush_state_batches(
           aec_state_db:commit(aec_state_db:put(contract, C1, aec_state_db:new()),
                               T1)),

    CT0 = aec_trees:contracts(aec_trees:new()),
    CT1 = aect_state_tree:insert_contract(C0, CT0),
    CT2 = aect_state_tree:enter_contract(C1, CT1),
    Ref = aec_trees:flush_state_batches(
            aec_trees:set_contracts(aec_trees:new(), CT2)),

    ?assertEqual(aec_trees:hash(Ref), aec_trees:hash(T2)),
    ?assertMatch({value, _},
                 aect_state_tree:lookup_contract(PK, aec_trees:contracts(T2))).

%% Committing a per-tx contract tombstone == an immediate
%% aect_state_tree:delete_contract on the materialised tree.
commit_contract_tombstone_equiv() ->
    C0 = new_contract(aeser_id:create(account, pk(15))),
    PK = aect_contracts:pubkey(C0),
    T1 = aec_trees:flush_state_batches(
           aec_state_db:commit(aec_state_db:put(contract, C0, aec_state_db:new()),
                               aec_trees:new())),
    ?assertMatch({value, _},
                 aect_state_tree:lookup_contract(PK, aec_trees:contracts(T1))),

    Ref = aec_trees:flush_state_batches(
            aec_trees:set_contracts(
              T1, aect_state_tree:delete_contract(
                    PK, aec_trees:contracts(T1)))),

    Sdb = aec_trees:flush_state_batches(
            aec_state_db:commit(aec_state_db:delete(contract, PK,
                                                    aec_state_db:new()), T1)),

    ?assertEqual(aec_trees:hash(Ref), aec_trees:hash(Sdb)),
    ?assertEqual(none,
                 aect_state_tree:lookup_contract(PK, aec_trees:contracts(Sdb))).

%%%===================================================================
%%% Helpers
%%%===================================================================

pk(N) -> <<N:256>>.

new_contract(OwnerId) ->
    Map = #{ owner_id    => OwnerId
           , nonce       => 42
           , code        => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version  => ?VM_AEVM_SOLIDITY_1
           , abi_version => ?ABI_SOLIDITY_1
           , fee         => 10
           , ttl         => 100
           , deposit     => 100
           , amount      => 50
           , gas         => 100
           , gas_price   => 5
           , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    {ok, Tx} = aect_create_tx:new(Map),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).
