%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Unit tests for aec_block_contract_cache.
%%%
%%%    Covers:
%%%      C-2 — cache clear on fork switch: verifies that clear/0 evicts all
%%%            cached contracts, so that stale entries from a discarded
%%%            microblock chain are never served to a competing fork.
%%%
%%%    The code fix (aec_conductor calls clear() unconditionally in
%%%    preempt_on_new_top/3) is verified by code inspection.  This test
%%%    confirms the cache module itself behaves correctly when clear() is
%%%    called between two competing fork applications.
%%% @end
%%%=============================================================================
-module(aec_block_contract_cache_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").

block_contract_cache_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"start is idempotent",                    fun start_is_idempotent/0}
     , {"miss on empty cache",                    fun miss_on_empty/0}
     , {"put then get",                           fun put_then_get/0}
     , {"clear removes all entries",              fun clear_removes_entries/0}
     , {"stale entry not served after fork clear",
        fun stale_entry_cleared_on_fork/0}
     , {"size reflects entry count",              fun size_reflects_count/0}
     ]}.

setup() ->
    ok = aec_block_contract_cache:start(),
    ok = aec_block_contract_cache:clear(),
    ok.

teardown(_) ->
    ok = aec_block_contract_cache:clear(),
    ok.

start_is_idempotent() ->
    ?assertEqual(ok, aec_block_contract_cache:start()),
    ?assertEqual(ok, aec_block_contract_cache:start()).

miss_on_empty() ->
    ?assertEqual(none, aec_block_contract_cache:get(sample_pk())).

put_then_get() ->
    PK = sample_pk(),
    C  = sample_contract(PK),
    ok = aec_block_contract_cache:put(PK, C),
    ?assertMatch({value, _}, aec_block_contract_cache:get(PK)).

clear_removes_entries() ->
    PK = sample_pk(),
    C  = sample_contract(PK),
    ok = aec_block_contract_cache:put(PK, C),
    ?assert(aec_block_contract_cache:size() > 0),
    ok = aec_block_contract_cache:clear(),
    ?assertEqual(0, aec_block_contract_cache:size()),
    ?assertEqual(none, aec_block_contract_cache:get(PK)).

%% Simulates the intra-epoch microblock fork scenario (C-2):
%%
%%   Micro1 applied  → contract C cached with state S1
%%   Fork switch     → clear() called (by aec_conductor:preempt_on_new_top)
%%   Micro1' applied → C must be read from MPT (S2), not ETS (S1)
%%
%% After clear(), a get() for any pubkey returns none, forcing the next
%% lookup to read from the MPT — which always holds the correct fork state.
stale_entry_cleared_on_fork() ->
    PK = sample_pk(),

    %% Micro1 application: contract C stored with state S1.
    C_s1 = sample_contract(PK),
    ok = aec_block_contract_cache:put(PK, C_s1),
    ?assertMatch({value, _}, aec_block_contract_cache:get(PK)),

    %% Fork switch: aec_conductor calls clear().
    ok = aec_block_contract_cache:clear(),

    %% Micro1' application: ETS must not serve the stale S1 entry.
    ?assertEqual(none, aec_block_contract_cache:get(PK)).

size_reflects_count() ->
    ?assertEqual(0, aec_block_contract_cache:size()),
    PK1 = <<"__contract_pk_1_________________">>,
    PK2 = <<"__contract_pk_2_________________">>,
    ok  = aec_block_contract_cache:put(PK1, sample_contract(PK1)),
    ?assertEqual(1, aec_block_contract_cache:size()),
    ok  = aec_block_contract_cache:put(PK2, sample_contract(PK2)),
    ?assertEqual(2, aec_block_contract_cache:size()),
    ok  = aec_block_contract_cache:clear(),
    ?assertEqual(0, aec_block_contract_cache:size()).

%%%===================================================================
%%% Helpers
%%%===================================================================

sample_pk() ->
    <<"__sample_contract_pubkey________">>.

%% Returns a minimal contract value accepted by aec_block_contract_cache:put/2.
%% put/2 calls aect_contracts:set_state internally, so we only need a value
%% that satisfies the aect_contracts type.
sample_contract(Pubkey) ->
    OwnerId = aeser_id:create(account, Pubkey),
    Map = #{ owner_id    => OwnerId
           , nonce       => 1
           , code        => <<"PLACEHOLDER_BYTECODE">>
           , vm_version  => ?VM_AEVM_SOLIDITY_1
           , abi_version => ?ABI_SOLIDITY_1
           , fee         => 1
           , ttl         => 100
           , deposit     => 0
           , amount      => 0
           , gas         => 10
           , gas_price   => 1
           , call_data   => <<"PLACEHOLDER_CALL_DATA">>
           },
    {ok, Tx} = aect_create_tx:new(Map),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).
