%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Integration tests for batch hash equivalence with a database
%%%    backend.
%%%
%%%    Verifies that batched writes produce identical MPT root hashes to
%%%    immediate writes when using DB-backed (Mnesia) trees — the code
%%%    path active during real block application, complementing the
%%%    in-memory equivalence tests.
%%% @end
%%%=============================================================================
-module(aec_batch_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").

batch_db_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"account batch hash equals immediate writes (DB backend)",
        fun account_batch_db_hash_equivalence/0}
     , {"contract batch hash equals immediate writes (DB backend)",
        fun contract_batch_db_hash_equivalence/0}
     , {"combined account + contract batch (DB backend)",
        fun combined_batch_db_hash_equivalence/0}
     , {"batch hash survives flush_state_batches on full trees (DB backend)",
        fun full_trees_flush_hash_equivalence/0}
     , {"store delete batch == immediate (DB backend)",
        fun store_delete_db_hash_equivalence/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%%%===================================================================
%%% Account batch (DB backend)
%%%===================================================================

%% Entering N accounts in batch mode and flushing once must produce the same
%% MPT root hash as flushing after each individual enter.
account_batch_db_hash_equivalence() ->
    Accounts = [aec_accounts:new(<<I:256>>, I * 100) || I <- lists:seq(1, 5)],

    %% Batch mode: enter all, then flush once.
    ABatch0 = aec_accounts_trees:empty_with_backend(),
    ABatch1 = lists:foldl(fun aec_accounts_trees:enter/2, ABatch0, Accounts),
    ABatch2 = aec_accounts_trees:flush_account_batch(ABatch1),
    {ok, HashBatch} = aec_accounts_trees:root_hash(ABatch2),

    %% Immediate mode: enter and flush after each account.
    AImm = lists:foldl(
             fun(A, T) ->
                 aec_accounts_trees:flush_account_batch(
                     aec_accounts_trees:enter(A, T))
             end,
             aec_accounts_trees:empty_with_backend(),
             Accounts),
    {ok, HashImm} = aec_accounts_trees:root_hash(AImm),

    ?assertEqual(HashBatch, HashImm).

%%%===================================================================
%%% Contract batch (DB backend)
%%%===================================================================

%% Inserting N contracts with store writes in batch mode and flushing once must
%% produce the same MPT root hash as flushing after each individual insert.
contract_batch_db_hash_equivalence() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    Contracts =
        [aect_contracts:set_state(
             MakeStore(#{<<"k", I>> => <<"v", I>>}),
             new_contract(aeser_id:create(account, <<I:256>>)))
         || I <- lists:seq(1, 3)],

    FlushC = fun(CT) ->
                 CT1 = aect_state_tree:flush_store_batch(CT),
                 aect_state_tree:flush_contract_meta_batch(CT1)
             end,

    %% Batch mode: insert all, then flush.
    CBatch0 = aect_state_tree:empty_with_backend(),
    CBatch1 = lists:foldl(fun aect_state_tree:insert_contract/2, CBatch0, Contracts),
    {ok, HashBatch} = aect_state_tree:root_hash(FlushC(CBatch1)),

    %% Immediate mode: insert and flush after each contract.
    CImm = lists:foldl(
             fun(C, T) -> FlushC(aect_state_tree:insert_contract(C, T)) end,
             aect_state_tree:empty_with_backend(),
             Contracts),
    {ok, HashImm} = aect_state_tree:root_hash(CImm),

    ?assertEqual(HashBatch, HashImm).

%%%===================================================================
%%% Combined (DB backend)
%%%===================================================================

%% Combined account and contract batch in one microblock simulation.
%% Exercises aec_trees:hash/1, which internally calls flush_state_batches.
combined_batch_db_hash_equivalence() ->
    Accounts = [aec_accounts:new(<<I:256>>, I * 100) || I <- lists:seq(1, 3)],
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    Contracts =
        [aect_contracts:set_state(
             MakeStore(#{<<"key">> => <<"val", I>>}),
             new_contract(aeser_id:create(account, <<(I + 100):256>>)))
         || I <- lists:seq(1, 2)],

    HashBatch = trees_hash(Accounts, Contracts, batch),
    HashImm   = trees_hash(Accounts, Contracts, immediate),

    ?assertEqual(HashBatch, HashImm).

%% aec_trees:hash/1 calls flush_state_batches internally.
%% Verify that hash computed via aec_trees:hash equals the hash after
%% an explicit flush on a full DB-backed trees record.
full_trees_flush_hash_equivalence() ->
    Accounts = [aec_accounts:new(<<I:256>>, I * 50) || I <- lists:seq(1, 4)],
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    Contracts =
        [aect_contracts:set_state(
             MakeStore(#{<<"x">> => <<"y", I>>}),
             new_contract(aeser_id:create(account, <<(I + 200):256>>)))
         || I <- lists:seq(1, 2)],

    %% Build batch-mode trees.
    Trees0 = aec_trees:new(),
    AT0 = aec_trees:accounts(Trees0),
    CT0 = aec_trees:contracts(Trees0),

    AT1 = lists:foldl(fun aec_accounts_trees:enter/2, AT0, Accounts),
    CT1 = lists:foldl(fun aect_state_tree:insert_contract/2, CT0, Contracts),

    Trees1 = aec_trees:set_accounts(Trees0, AT1),
    Trees2 = aec_trees:set_contracts(Trees1, CT1),

    %% Hash via aec_trees:hash (flushes internally).
    HashViaHash = aec_trees:hash(Trees2),

    %% Hash via explicit flush then root_hash.
    Trees3 = aec_trees:flush_state_batches(Trees2),
    HashViaFlush = aec_trees:hash(Trees3),

    ?assertEqual(HashViaHash, HashViaFlush).

%%%===================================================================
%%% Store deletion (DB backend) — batch == immediate
%%%===================================================================

%% Inserting a contract with store keys, then removing one key via a
%% later enter_contract, must produce the same DB-backed root hash
%% whether the removal is batched or flushed immediately — and the
%% removed key must be absent from the reloaded store.
store_delete_db_hash_equivalence() ->
    MakeStore = fun(Map) -> aect_contracts_store:put_map(Map, aect_contracts_store:new()) end,
    C0 = new_contract(aeser_id:create(account, <<7:256>>)),
    PK = aect_contracts:pubkey(C0),
    C  = aect_contracts:set_state(
           MakeStore(#{<<"k">> => <<"v">>, <<"k2">> => <<"v2">>}), C0),

    FlushC = fun(CT) ->
                 CT1 = aect_state_tree:flush_store_batch(CT),
                 aect_state_tree:flush_contract_meta_batch(CT1)
             end,
    Remove = fun(CT) ->
                 {value, Cx} = aect_state_tree:lookup_contract(PK, CT),
                 Sx = aect_contracts_store:remove(<<"k">>, aect_contracts:state(Cx)),
                 aect_state_tree:enter_contract(aect_contracts:set_state(Sx, Cx), CT)
             end,

    %% Batch: insert + remove, single flush.
    B1 = aect_state_tree:insert_contract(C, aect_state_tree:empty_with_backend()),
    {ok, HB} = aect_state_tree:root_hash(FlushC(Remove(B1))),

    %% Immediate: insert flush, remove flush.
    I1 = FlushC(aect_state_tree:insert_contract(
                  C, aect_state_tree:empty_with_backend())),
    IFinal = FlushC(Remove(I1)),
    {ok, HI} = aect_state_tree:root_hash(IFinal),

    ?assertEqual(HB, HI),

    {value, Cf} = aect_state_tree:lookup_contract(PK, IFinal),
    ?assertEqual(#{<<"k2">> => <<"v2">>},
                 aect_contracts_store:contents(aect_contracts:state(Cf))).

%%%===================================================================
%%% Helpers
%%%===================================================================

trees_hash(Accounts, Contracts, Mode) ->
    Trees0 = aec_trees:new(),
    AT0    = aec_trees:accounts(Trees0),
    CT0    = aec_trees:contracts(Trees0),

    FlushA = fun(AT) -> aec_accounts_trees:flush_account_batch(AT) end,
    FlushC = fun(CT) ->
                 CT1 = aect_state_tree:flush_store_batch(CT),
                 aect_state_tree:flush_contract_meta_batch(CT1)
             end,

    {AT1, CT1} =
        case Mode of
            batch ->
                A = lists:foldl(fun aec_accounts_trees:enter/2, AT0, Accounts),
                C = lists:foldl(fun aect_state_tree:insert_contract/2, CT0, Contracts),
                {FlushA(A), FlushC(C)};
            immediate ->
                A = lists:foldl(
                      fun(Acc, T) ->
                          FlushA(aec_accounts_trees:enter(Acc, T))
                      end, AT0, Accounts),
                C = lists:foldl(
                      fun(Ctr, T) ->
                          FlushC(aect_state_tree:insert_contract(Ctr, T))
                      end, CT0, Contracts),
                {A, C}
        end,

    Trees1 = aec_trees:set_accounts(Trees0, AT1),
    Trees2 = aec_trees:set_contracts(Trees1, CT1),
    aec_trees:hash(Trees2).

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
