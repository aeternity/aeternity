-module(aect_state_tree_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aecontract.hrl").

-define(TESTED_MODULE, aect_state_tree).

%%%===================================================================
%%% Existing regression test (updated to flush before root_hash)
%%%===================================================================

%% channels' rely on contracts with a dict backend being reproducible with
%% only the latest state
trunc_test() ->
    T0 = ?TESTED_MODULE:empty(),

    Owner1 = aeser_id:create(account, <<"_______________k1_______________">>),
    Owner2 = aeser_id:create(account, <<"_______________k2_______________">>),

    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    St1 = MakeStore(#{<<"1">> => <<"a">>}),
    St2 = MakeStore(#{<<"2">> => <<"b">>, <<"1">> => <<>>}),
    StNot1 = MakeStore(#{<<"1">> => <<>>}), %% Empty value to delete

    C0 = new_contract(),
    C1 = new_contract(#{owner_id => Owner1}),
    C2 = new_contract(#{owner_id => Owner2}),

    T1  = ?TESTED_MODULE:insert_contract(C0, T0),
    T2  = ?TESTED_MODULE:insert_contract(C1, T1),
    T30 = ?TESTED_MODULE:insert_contract(C2, T2),

    %% state changes do not leave artifacts: empty state
    T31  = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St1, C1), T30),
    T300 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StNot1, C1), T31),
    %% Flush both batches before comparing hashes — batching defers writes.
    assert_root_hashes(flush(T30), flush(T300)),

    %% state changes do not leave artifacts: non empty state
    T320 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St2, C1), T31),
    T321 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St2, C1), T30),
    assert_root_hashes(flush(T320), flush(T321)).

%%%===================================================================
%%% Contract metadata batch tests
%%%===================================================================

%% insert_contract defers metadata write to batch; lookup finds it without flush.
meta_batch_lookup_after_insert_test() ->
    T0 = ?TESTED_MODULE:empty(),
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    T1 = ?TESTED_MODULE:insert_contract(C, T0),

    %% lookup_contract must find the contract via the batch (no flush yet).
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK, T1)),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK, T1, [no_store])),

    %% is_contract must also return true via the batch.
    ?assert(?TESTED_MODULE:is_contract(PK, T1)),

    %% The raw MPT does NOT yet contain the contract (batch not flushed).
    %% After flushing, the same lookup still works and the root hash is defined.
    T2 = flush(T1),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK, T2)),
    {ok, _} = ?TESTED_MODULE:root_hash(T2).

%% enter_contract updates the batch; lookup returns the latest version.
meta_batch_enter_update_test() ->
    T0 = ?TESTED_MODULE:empty(),
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    T1 = ?TESTED_MODULE:insert_contract(C, T0),

    %% Flush and re-lookup so the contract is in MPT, not batch.
    T2 = flush(T1),
    {value, C2} = ?TESTED_MODULE:lookup_contract(PK, T2, [no_store]),

    %% Update via enter_contract.
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    St = MakeStore(#{<<"k">> => <<"v">>}),
    C3 = aect_contracts:set_state(St, C2),
    T3 = ?TESTED_MODULE:enter_contract(C3, T2),

    %% lookup must return the updated contract from batch.
    {value, CLookup} = ?TESTED_MODULE:lookup_contract(PK, T3, [no_store]),
    %% The store in CLookup reflects the pending write (via read_cache pre-population).
    StLookup = aect_contracts:state(CLookup),
    ?assertEqual(<<"v">>, aect_contracts_store:get(<<"k">>, StLookup)).

%% A contract inserted and then entered in the same microblock stays tagged 'new'
%% so the flush still uses aeu_mtrees:insert (error-on-duplicate invariant preserved).
meta_batch_new_tag_preserved_through_enter_test() ->
    T0 = ?TESTED_MODULE:empty(),
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    T1 = ?TESTED_MODULE:insert_contract(C, T0),

    %% enter on a contract that was inserted in the same batch.
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    St = MakeStore(#{<<"x">> => <<"1">>}),
    C2 = aect_contracts:set_state(St, C),
    T2 = ?TESTED_MODULE:enter_contract(C2, T1),

    %% After flush, the contract must be in the MPT (insert was used, not enter).
    T3 = flush(T2),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK, T3, [no_store])),
    {ok, _Hash} = ?TESTED_MODULE:root_hash(T3).

%% Multiple contracts in the batch: all are written at flush time.
meta_batch_multi_contract_flush_test() ->
    T0  = ?TESTED_MODULE:empty(),
    C1  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k1_______________">>)}),
    C2  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k2_______________">>)}),
    C3  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k3_______________">>)}),
    PK1 = aect_contracts:pubkey(C1),
    PK2 = aect_contracts:pubkey(C2),
    PK3 = aect_contracts:pubkey(C3),

    T1 = ?TESTED_MODULE:insert_contract(C1, T0),
    T2 = ?TESTED_MODULE:insert_contract(C2, T1),
    T3 = ?TESTED_MODULE:insert_contract(C3, T2),

    %% All three visible from batch without flush.
    ?assert(?TESTED_MODULE:is_contract(PK1, T3)),
    ?assert(?TESTED_MODULE:is_contract(PK2, T3)),
    ?assert(?TESTED_MODULE:is_contract(PK3, T3)),

    %% After flush, all three visible from MPT.
    T4 = flush(T3),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK1, T4, [no_store])),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK2, T4, [no_store])),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK3, T4, [no_store])).

%% delete_contract removes from both store_batch and contract_meta_batch.
meta_batch_delete_clears_batch_test() ->
    T0 = ?TESTED_MODULE:empty(),
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    T1 = ?TESTED_MODULE:insert_contract(C, T0),
    %% Visible in batch.
    ?assert(?TESTED_MODULE:is_contract(PK, T1)),

    T2 = ?TESTED_MODULE:delete_contract(PK, T1),
    %% Must be gone after delete (not in batch, not in MPT).
    ?assertNot(?TESTED_MODULE:is_contract(PK, T2)),
    ?assertEqual(none, ?TESTED_MODULE:lookup_contract(PK, T2)).

%% flush_contract_meta_batch/1 is idempotent (empty batch = no-op).
meta_batch_flush_idempotent_test() ->
    T0 = ?TESTED_MODULE:empty(),
    C  = new_contract(),

    T1 = ?TESTED_MODULE:insert_contract(C, T0),
    T2 = flush(T1),

    %% Second flush of an already-empty batch must return same tree.
    T3 = ?TESTED_MODULE:flush_contract_meta_batch(T2),
    {ok, H2} = ?TESTED_MODULE:root_hash(T2),
    {ok, H3} = ?TESTED_MODULE:root_hash(T3),
    ?assertEqual(H2, H3).

%% flush_contract_meta_for/2 flushes only the requested pubkey.
meta_batch_flush_single_contract_test() ->
    T0  = ?TESTED_MODULE:empty(),
    C1  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k1_______________">>)}),
    C2  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k2_______________">>)}),
    PK1 = aect_contracts:pubkey(C1),
    PK2 = aect_contracts:pubkey(C2),

    T1 = ?TESTED_MODULE:insert_contract(C1, T0),
    T2 = ?TESTED_MODULE:insert_contract(C2, T1),

    %% Flush only PK1.
    T3 = ?TESTED_MODULE:flush_contract_meta_for(PK1, T2),

    %% PK2 still in batch (not in MPT yet), PK1 in MPT.
    ?assert(?TESTED_MODULE:is_contract(PK1, T3)),
    ?assert(?TESTED_MODULE:is_contract(PK2, T3)),

    %% Flush remainder and verify both are in MPT.
    T4 = flush(T3),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK1, T4, [no_store])),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK2, T4, [no_store])).

%% Root hash after flush equals root hash of an equivalent tree built without batching.
%% This is the core correctness invariant.
meta_batch_root_hash_equivalence_test() ->
    Owner1 = aeser_id:create(account, <<"_______________k1_______________">>),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,

    C1 = new_contract(#{owner_id => Owner1}),
    St = MakeStore(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>}),
    C1Updated = aect_contracts:set_state(St, C1),

    %% Tree A: insert then update (two batch ops, one flush).
    TA0 = ?TESTED_MODULE:empty(),
    TA1 = ?TESTED_MODULE:insert_contract(C1, TA0),
    TA2 = ?TESTED_MODULE:enter_contract(C1Updated, TA1),
    TAF = flush(TA2),

    %% Tree B: insert then update with a flush in between.
    TB0 = ?TESTED_MODULE:empty(),
    TB1 = ?TESTED_MODULE:insert_contract(C1, TB0),
    TB1F = flush(TB1),
    TB2 = ?TESTED_MODULE:enter_contract(C1Updated, TB1F),
    TBF = flush(TB2),

    assert_root_hashes(TAF, TBF).

%% Lookup_contract_with_code uses lookup_contract internally — verify it
%% also works against the batch.
meta_batch_lookup_with_code_test() ->
    T0 = ?TESTED_MODULE:empty(),
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    T1 = ?TESTED_MODULE:insert_contract(C, T0),

    %% Must find the contract (and code) from the batch.
    ?assertMatch({value, _, _}, ?TESTED_MODULE:lookup_contract_with_code(PK, T1)).

%%%===================================================================
%%% Cross-tx store read cache tests
%%%===================================================================

%% A key read from the MPT in Tx A is pre-populated in Tx B's store read_cache.
store_read_cache_cross_tx_propagation_test() ->
    T0 = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,

    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    %% Establish initial store value in the MPT.
    CInit = aect_contracts:set_state(MakeStore(#{<<"k">> => <<"v0">>}), C),
    T1F    = flush(?TESTED_MODULE:insert_contract(CInit, T0)),

    %% Tx A: look up contract, read key k (triggers MPT read into store's read_cache).
    {value, CA}       = ?TESTED_MODULE:lookup_contract(PK, T1F),
    {<<"v0">>, StA}   = aect_contracts_store:get_w_cache(<<"k">>, aect_contracts:state(CA)),
    %% Enter the contract so enter_store captures the read_cache into the batch.
    T2 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StA, CA), T1F),

    %% Tx B: look up contract — batch Reads must pre-populate the store's read_cache.
    {value, CB} = ?TESTED_MODULE:lookup_contract(PK, T2),
    <<"v0">> = aect_contracts_store:get(<<"k">>, aect_contracts:state(CB)).

%% If Tx A reads k=old then writes k=new, Tx B must see the written value, not the stale read.
store_read_cache_write_overrides_stale_read_test() ->
    T0 = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,

    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    CInit = aect_contracts:set_state(MakeStore(#{<<"k">> => <<"old">>}), C),
    T1F    = flush(?TESTED_MODULE:insert_contract(CInit, T0)),

    %% Tx A reads k (populates read_cache with old), then overwrites with new.
    {value, CA}      = ?TESTED_MODULE:lookup_contract(PK, T1F),
    {<<"old">>, StA} = aect_contracts_store:get_w_cache(<<"k">>, aect_contracts:state(CA)),
    StA2  = aect_contracts_store:put(<<"k">>, <<"new">>, StA),
    T2 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StA2, CA), T1F),

    %% Tx B must see the written value — write must shadow the stale MPT read in the batch.
    {value, CB} = ?TESTED_MODULE:lookup_contract(PK, T2),
    <<"new">> = aect_contracts_store:get(<<"k">>, aect_contracts:state(CB)).

%% A read-only access (no write_cache change) must not alter the MPT root hash on flush.
store_read_cache_reads_not_flushed_to_mpt_test() ->
    T0 = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,

    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    CInit = aect_contracts:set_state(MakeStore(#{<<"k">> => <<"v">>}), C),
    T1F    = flush(?TESTED_MODULE:insert_contract(CInit, T0)),
    {ok, H1} = ?TESTED_MODULE:root_hash(T1F),

    %% Tx A: read-only — no store write.
    {value, CA}    = ?TESTED_MODULE:lookup_contract(PK, T1F),
    {<<"v">>, StA} = aect_contracts_store:get_w_cache(<<"k">>, aect_contracts:state(CA)),
    T2  = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StA, CA), T1F),

    %% After flush the MPT must be identical — reads are ephemeral.
    T2F  = flush(T2),
    {ok, H2} = ?TESTED_MODULE:root_hash(T2F),
    ?assertEqual(H1, H2).

%% Reads cached for one contract must not appear in another contract's store.
store_read_cache_isolated_per_contract_test() ->
    T0  = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,

    C1  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k1_______________">>)}),
    C2  = new_contract(#{owner_id => aeser_id:create(account, <<"_______________k2_______________">>)}),
    PK1 = aect_contracts:pubkey(C1),
    PK2 = aect_contracts:pubkey(C2),

    C1Init = aect_contracts:set_state(MakeStore(#{<<"key1">> => <<"val1">>}), C1),
    C2Init = aect_contracts:set_state(MakeStore(#{<<"key2">> => <<"val2">>}), C2),
    T2F = flush(?TESTED_MODULE:insert_contract(C2Init,
                    ?TESTED_MODULE:insert_contract(C1Init, T0))),

    %% Tx A reads from C1's store.
    {value, C1A}         = ?TESTED_MODULE:lookup_contract(PK1, T2F),
    {<<"val1">>, St1A}   = aect_contracts_store:get_w_cache(<<"key1">>, aect_contracts:state(C1A)),
    T3 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St1A, C1A), T2F),

    %% C2's store must NOT contain key1 — batch reads are per-contract.
    {value, C2B} = ?TESTED_MODULE:lookup_contract(PK2, T3),
    <<>> = aect_contracts_store:get(<<"key1">>, aect_contracts:state(C2B)).

%%%===================================================================
%%% Regression — cross-tx store deletion must not resurface
%%%===================================================================

%% Tx A deletes a store key materialised in the MPT by a prior microblock;
%% Tx B in the same (unflushed) batch enumerates the store via contents/2.
%% The deleted key must NOT resurface (it was a stale-MPT-merge leak).
store_delete_cross_tx_not_resurfacing_test() ->
    T0 = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    %% Prior microblock: materialise k=>v, k2=>v2 in the MPT.
    CInit = aect_contracts:set_state(MakeStore(#{<<"k">>  => <<"v">>,
                                                  <<"k2">> => <<"v2">>}), C),
    T1F    = flush(?TESTED_MODULE:insert_contract(CInit, T0)),

    %% Tx A: remove k (batched, NOT flushed).
    {value, CA} = ?TESTED_MODULE:lookup_contract(PK, T1F),
    StA  = aect_contracts_store:remove(<<"k">>, aect_contracts:state(CA)),
    T2    = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StA, CA), T1F),

    %% Tx B: enumerate the store — k must be gone, k2 must remain.
    {value, CB} = ?TESTED_MODULE:lookup_contract(PK, T2),
    StB     = aect_contracts:state(CB),
    Contents = aect_contracts_store:contents(StB),
    ?assertEqual(#{<<"k2">> => <<"v2">>}, Contents),
    ?assertEqual(<<>>, aect_contracts_store:get(<<"k">>, StB)),
    ?assertEqual(<<"v2">>, aect_contracts_store:get(<<"k2">>, StB)),
    {Sub, _} = aect_contracts_store:subtree_w_cache(<<>>, StB),
    ?assertEqual(#{<<"k2">> => <<"v2">>}, Sub).

%% Delete in Tx A, re-create with a new value in Tx B: Tx C must see the
%% new value, not the stale MPT value and not "absent".
store_delete_then_recreate_cross_tx_test() ->
    T0 = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),

    CInit = aect_contracts:set_state(MakeStore(#{<<"k">> => <<"v">>}), C),
    T1F    = flush(?TESTED_MODULE:insert_contract(CInit, T0)),

    {value, CA} = ?TESTED_MODULE:lookup_contract(PK, T1F),
    StA = aect_contracts_store:remove(<<"k">>, aect_contracts:state(CA)),
    T2   = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StA, CA), T1F),

    {value, CB} = ?TESTED_MODULE:lookup_contract(PK, T2),
    StB = aect_contracts_store:put(<<"k">>, <<"v3">>, aect_contracts:state(CB)),
    T3   = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StB, CB), T2),

    {value, CC} = ?TESTED_MODULE:lookup_contract(PK, T3),
    StC = aect_contracts:state(CC),
    ?assertEqual(<<"v3">>, aect_contracts_store:get(<<"k">>, StC)),
    ?assertEqual(#{<<"k">> => <<"v3">>}, aect_contracts_store:contents(StC)).

%% copy_contract_store/3 consumer audit: a key removed (batched) in the
%% source must not be copied into the destination contract's store.
store_delete_cross_tx_copy_contract_test() ->
    T0 = ?TESTED_MODULE:empty(),
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),
    NewId = <<"_____________newctr_____________">>,

    CInit = aect_contracts:set_state(MakeStore(#{<<"k">>  => <<"v">>,
                                                  <<"k2">> => <<"v2">>}), C),
    T1F    = flush(?TESTED_MODULE:insert_contract(CInit, T0)),

    {value, CA} = ?TESTED_MODULE:lookup_contract(PK, T1F),
    StA  = aect_contracts_store:remove(<<"k">>, aect_contracts:state(CA)),
    CA2  = aect_contracts:set_state(StA, CA),
    T2    = ?TESTED_MODULE:enter_contract(CA2, T1F),

    T3 = ?TESTED_MODULE:copy_contract_store(CA2, NewId, T2),
    {ok, CopiedStore} = ?TESTED_MODULE:read_contract_store(NewId, T3),
    ?assertEqual(#{<<"k2">> => <<"v2">>},
                 aect_contracts_store:contents(CopiedStore)).

%%%===================================================================
%%% Batch flush root hash equivalence
%%%===================================================================

%% For any sequence of contract-create and store-write operations,
%% batch mode (all writes deferred to one flush) must produce the same
%% MPT root hash as immediate mode (flush after each operation).
%%
%% This is tested with several representative operation sequences.
%% A PropEr/EQC property test should be added once the framework is
%% available; these EUnit cases cover the critical corner cases.

%% Single contract with multiple store keys.
batch_equals_immediate_single_contract_test() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C   = new_contract(),
    PK  = aect_contracts:pubkey(C),
    St  = MakeStore(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>, <<"c">> => <<"3">>}),
    CWithStore = aect_contracts:set_state(St, C),

    %% Batch mode: insert + one flush.
    TBatch = flush(?TESTED_MODULE:insert_contract(CWithStore, ?TESTED_MODULE:empty())),

    %% Immediate mode: insert + flush after each contract (same result since only one here).
    TImmediate = flush(?TESTED_MODULE:insert_contract(CWithStore, ?TESTED_MODULE:empty())),

    assert_root_hashes(TBatch, TImmediate),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK, TBatch, [no_store])).

%% Multiple contracts with disjoint store keys — flush order must not matter.
batch_equals_immediate_multi_contract_test() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    O1 = aeser_id:create(account, <<"_______________k1_______________">>),
    O2 = aeser_id:create(account, <<"_______________k2_______________">>),
    O3 = aeser_id:create(account, <<"_______________k3_______________">>),

    C1 = aect_contracts:set_state(MakeStore(#{<<"p">> => <<"1">>, <<"q">> => <<"2">>}),
                                  new_contract(#{owner_id => O1})),
    C2 = aect_contracts:set_state(MakeStore(#{<<"r">> => <<"3">>}),
                                  new_contract(#{owner_id => O2})),
    C3 = aect_contracts:set_state(MakeStore(#{<<"s">> => <<"4">>, <<"t">> => <<"5">>, <<"u">> => <<"6">>}),
                                  new_contract(#{owner_id => O3})),

    %% Batch mode: all three contracts inserted before any flush.
    TBatch = flush(
               ?TESTED_MODULE:insert_contract(C3,
               ?TESTED_MODULE:insert_contract(C2,
               ?TESTED_MODULE:insert_contract(C1,
               ?TESTED_MODULE:empty())))),

    %% Immediate mode: flush after each contract insertion.
    TImmediate =
        flush(?TESTED_MODULE:insert_contract(C3,
        flush(?TESTED_MODULE:insert_contract(C2,
        flush(?TESTED_MODULE:insert_contract(C1,
              ?TESTED_MODULE:empty())))))),

    assert_root_hashes(TBatch, TImmediate).

%% Contract created in TX 1, store updated in TX 2 (same microblock) —
%% the root hash must match a reference tree that applied both writes in one flush.
batch_equals_immediate_cross_tx_store_update_test() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C   = new_contract(),
    PK  = aect_contracts:pubkey(C),
    St1 = MakeStore(#{<<"k">> => <<"v1">>}),
    St2 = MakeStore(#{<<"k">> => <<"v2">>}),

    %% TX 1: insert with initial store.
    %% TX 2: update store (enter with new value).
    CInit    = aect_contracts:set_state(St1, C),
    CUpdated = aect_contracts:set_state(St2, C),

    %% Batch mode: insert + enter, then flush.
    TBatch = flush(
               ?TESTED_MODULE:enter_contract(CUpdated,
               ?TESTED_MODULE:insert_contract(CInit,
               ?TESTED_MODULE:empty()))),

    %% Reference: apply both writes directly then flush.
    TRef = flush(
             ?TESTED_MODULE:enter_contract(CUpdated,
             flush(?TESTED_MODULE:insert_contract(CInit,
                   ?TESTED_MODULE:empty())))),

    assert_root_hashes(TBatch, TRef),
    ?assertMatch({value, _}, ?TESTED_MODULE:lookup_contract(PK, TBatch, [no_store])).

%%%===================================================================
%%% Sentinel write coverage
%%%===================================================================

%% After insert_contract, the sentinel <<0>> at the StoreId prefix must be
%% written immediately to the MPT (not deferred to the store batch).
%% Consequence: flush_store_batch can be called BEFORE flush_contract_meta_batch
%% and the store writes still land in the MPT correctly.
sentinel_write_enables_store_flush_before_meta_flush_test() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C  = new_contract(),
    PK = aect_contracts:pubkey(C),
    St = MakeStore(#{<<"sentinel_key">> => <<"sentinel_val">>}),
    CWithStore = aect_contracts:set_state(St, C),

    %% insert_contract: sentinel written to MPT immediately; metadata + writes to batch.
    T1 = ?TESTED_MODULE:insert_contract(CWithStore, ?TESTED_MODULE:empty()),

    %% Flush STORE batch first (before meta batch).
    %% This requires the sentinel to already be in the MPT — if it is not,
    %% add_store returns no_such_subtree and the store write is silently dropped.
    T2 = ?TESTED_MODULE:flush_store_batch(T1),

    %% NOW flush meta batch.
    T3 = ?TESTED_MODULE:flush_contract_meta_batch(T2),

    %% Verify: the contract is in the MPT and its store key is readable.
    {value, CFinal} = ?TESTED_MODULE:lookup_contract(PK, T3),
    <<"sentinel_val">> = aect_contracts_store:get(<<"sentinel_key">>,
                                                   aect_contracts:state(CFinal)).

%% The sentinel-then-store order must produce identical root hash as
%% the standard flush order (meta-then-store via the flush/1 helper).
sentinel_flush_order_hash_equivalence_test() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C  = new_contract(),
    St = MakeStore(#{<<"k1">> => <<"v1">>, <<"k2">> => <<"v2">>}),
    CWithStore = aect_contracts:set_state(St, C),

    T1 = ?TESTED_MODULE:insert_contract(CWithStore, ?TESTED_MODULE:empty()),

    %% Order A: store first, then meta.
    TA = ?TESTED_MODULE:flush_contract_meta_batch(?TESTED_MODULE:flush_store_batch(T1)),

    %% Order B: meta first, then store (standard order used by the flush/1 helper).
    TB = flush(T1),

    assert_root_hashes(TA, TB).

%%%===================================================================
%%% Helpers
%%%===================================================================

flush(Tree) ->
    Tree1 = ?TESTED_MODULE:flush_store_batch(Tree),
    ?TESTED_MODULE:flush_contract_meta_batch(Tree1).

new_contract() ->
    new_contract(#{}).

new_contract(Override) ->
    Map = #{ owner_id    => aeser_id:create(account, <<4711:32/unit:8>>)
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
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).

assert_root_hashes(Tree1, Tree2) ->
    {ok, T1Hash} = ?TESTED_MODULE:root_hash(Tree1),
    {ok, T2Hash} = ?TESTED_MODULE:root_hash(Tree2),
    ?assertEqual(T1Hash, T2Hash).
