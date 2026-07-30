%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Batch-MPT cross-tx equivalence: two separate aeprimop transactions
%%%    feed the same still-unflushed per-microblock batch (the real
%%%    multi-tx microblock window). `batch' never flushes between tx1 and
%%%    tx2; `immediate' flushes after tx1. Both must yield a byte-identical
%%%    root, exercising the insert/enter/tombstone bookkeeping of each
%%%    state tree across a tx boundary.
%%% @end
%%%=============================================================================
-module(aec_state_batch_crosstx_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").

crosstx_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"contract: tx1 creates C, tx2 (same mb) updates C's store",
        fun contract_create_then_update/0}
     , {"oracle: tx1 registers O, tx2 (same mb) updates O's query fee",
        fun oracle_register_then_update/0}
     , {"name: tx1 claims N, tx2 (same mb) updates N (pointers/ttl)",
        fun name_claim_then_update/0}
     , {"call: tx1 and tx2 (same mb) each insert a distinct call",
        fun two_calls_two_tx/0}
     , {"commitment: tx1 claims (deletes) it, tx2 (same mb) re-preclaims "
        "the identical hash",
        fun commitment_delete_then_recreate_two_tx/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%%%===================================================================
%%% Harness
%%%===================================================================

tx_env() ->
    aetx_env:tx_env(1, aec_hard_forks:protocol_effective_at_height(1)).

%% tx1 commits its dirty per-tx cache (final_trees/1) but leaves the
%% per-microblock batches unflushed; tx2 runs on those same trees.
%% `batch' never flushes between the two; `immediate' flushes after tx1.
two_tx(Trees0, Op1, Op2, Mode) ->
    TxEnv = tx_env(),
    S1  = aeprimop_state:new(Trees0, TxEnv),
    S1a = Op1(S1),
    Trees1 = aeprimop_state:final_trees(S1a),
    Trees1b = case Mode of
                  immediate -> aec_trees:flush_state_batches(Trees1);
                  batch     -> Trees1
              end,
    S2  = aeprimop_state:new(Trees1b, TxEnv),
    S2a = Op2(S2),
    Trees2 = aeprimop_state:final_trees(S2a),
    aec_trees:flush_state_batches(Trees2).

run2(Trees0, Op1, Op2, Mode) ->
    Tf = two_tx(Trees0, Op1, Op2, Mode),
    {aec_trees:hash(Tf), Tf}.

%%%===================================================================
%%% Contract
%%%===================================================================

contract_create_then_update() ->
    Owner = aeser_id:create(account, <<1:256>>),
    C0 = new_contract(Owner),
    PK = aect_contracts:pubkey(C0),
    Op1 = fun(S) -> aeprimop_state:put_contract(C0, S) end,
    Op2 = fun(S) ->
                  {C, S1} = aeprimop_state:get_contract(PK, S),
                  Store1 = aect_contracts_store:put(<<"k">>, <<"v">>, aect_contracts:state(C)),
                  C1 = aect_contracts:set_state(Store1, C),
                  aeprimop_state:put_contract(C1, S1)
          end,
    RunHash = fun(Mode) -> element(1, run2(aec_trees:new(), Op1, Op2, Mode)) end,
    ?assertEqual(RunHash(immediate), RunHash(batch)),
    {_, Tf} = run2(aec_trees:new(), Op1, Op2, batch),
    {value, Cf} = aect_state_tree:lookup_contract(PK, aec_trees:contracts(Tf)),
    ?assertEqual(#{<<"k">> => <<"v">>},
                 aect_contracts_store:contents(aect_contracts:state(Cf))).

%%%===================================================================
%%% Oracle
%%%===================================================================

oracle_register_then_update() ->
    PK = <<42:256>>,
    O0 = aeo_oracles:new(PK, <<"q">>, <<"r">>, 10, 1000000000, 1),
    Op1 = fun(S) -> aeprimop_state:put_oracle(O0, S) end,
    Op2 = fun(S) ->
                  {O, S1} = aeprimop_state:get_oracle(PK, oracle_does_not_exist, S),
                  O1 = aeo_oracles:set_query_fee(777, O),
                  aeprimop_state:put_oracle(O1, S1)
          end,
    RunHash = fun(Mode) -> element(1, run2(aec_trees:new(), Op1, Op2, Mode)) end,
    ?assertEqual(RunHash(immediate), RunHash(batch)),
    {_, Tf} = run2(aec_trees:new(), Op1, Op2, batch),
    {value, Of} = aeo_state_tree:lookup_oracle(PK, aec_trees:oracles(Tf)),
    ?assertEqual(777, aeo_oracles:query_fee(Of)).

%%%===================================================================
%%% Name
%%%===================================================================

name_claim_then_update() ->
    NH = aec_hash:hash(pubkey, <<"crosstx-name">>),
    N0 = aens_names:new(NH, <<7:256>>, 500000),
    Op1 = fun(S) -> aeprimop_state:put_name(N0, S) end,
    Op2 = fun(S) ->
                  {N, S1} = aeprimop_state:get_name(NH, S),
                  N1 = aens_names:update(N, 999999, 3600, []),
                  aeprimop_state:put_name(N1, S1)
          end,
    RunHash = fun(Mode) -> element(1, run2(aec_trees:new(), Op1, Op2, Mode)) end,
    ?assertEqual(RunHash(immediate), RunHash(batch)),
    {_, Tf} = run2(aec_trees:new(), Op1, Op2, batch),
    {value, Nf} = aens_state_tree:lookup_name(NH, aec_trees:ns(Tf)),
    ?assertEqual(999999, aens_names:ttl(Nf)).

%%%===================================================================
%%% Call
%%%===================================================================

two_calls_two_tx() ->
    CtId = aeser_id:create(contract, <<9:256>>),
    Caller1 = aeser_id:create(account, <<10:256>>),
    Caller2 = aeser_id:create(account, <<11:256>>),
    Call1 = aect_call:new(Caller1, 1, CtId, 1, 1000000),
    Call2 = aect_call:new(Caller2, 1, CtId, 1, 1000000),
    Op1 = fun(S) -> aeprimop_state:put_call(Call1, S) end,
    Op2 = fun(S) -> aeprimop_state:put_call(Call2, S) end,
    RunHash = fun(Mode) -> element(1, run2(aec_trees:new(), Op1, Op2, Mode)) end,
    ?assertEqual(RunHash(immediate), RunHash(batch)),
    {_, Tf} = run2(aec_trees:new(), Op1, Op2, batch),
    ?assertEqual(2, length(aect_call_state_tree:to_list(aec_trees:calls(Tf)))).

%%%===================================================================
%%% Commitment delete-then-recreate across a tx boundary in one
%%% unflushed microblock: tx1 claims (deletes) hash H, tx2 re-preclaims
%%% the same H. Legitimate because lookup_commitment/2 is batch-aware and
%%% reports H absent after tx1's tombstone, so the preclaim guard
%%% (aeprimop:assert_not_commitment/2) does not block tx2 — this is
%%% reachable through the real tx pipeline, not a raw-API curiosity.
%%%===================================================================

commitment_delete_then_recreate_two_tx() ->
    CH = aec_hash:hash(pubkey, <<"crosstx-commitment">>),
    C0 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, <<1:256>>), 100, 1),
    %% mb N (already flushed): the original preclaim.
    T0 = aec_trees:new(),
    NT0 = aens_state_tree:enter_commitment(C0, aec_trees:ns(T0)),
    Base = aec_trees:flush_state_batches(aec_trees:set_ns(T0, NT0)),
    ?assertMatch({value, _}, aens_state_tree:lookup_commitment(CH, aec_trees:ns(Base))),

    %% Same microblock, no flush in `batch': tx1 deletes, tx2 re-preclaims.
    Op1 = fun(S) -> aeprimop_state:delete_x(commitment, CH, S) end,
    C1  = aens_commitments:new(aeser_id:create(commitment, CH),
                               aeser_id:create(account, <<2:256>>), 500, 1),
    Op2 = fun(S) ->
                  %% Guard must see tx1's tombstone and treat H as absent
                  %% (batch-aware), like aeprimop:name_preclaim/2.
                  none = aeprimop_state:find_commitment(CH, S),
                  aeprimop_state:put_commitment(C1, S)
          end,

    ImmediateOutcome = try {ok, run2(Base, Op1, Op2, immediate)}
                       catch C:E -> {crashed, C, E} end,
    BatchOutcome = try {ok, run2(Base, Op1, Op2, batch)}
                   catch C2:E2 -> {crashed, C2, E2} end,

    ?debugFmt("~n=== commitment delete-then-recreate, two tx, one mb ===~n"
              "immediate: ~p~nbatch:     ~p~n",
              [outcome_summary(ImmediateOutcome), outcome_summary(BatchOutcome)]),

    case {ImmediateOutcome, BatchOutcome} of
        {{ok, {ImmHash, _}}, {ok, {BatchHash, _}}} ->
            %% No divergence: assert full equivalence.
            ?assertEqual(ImmHash, BatchHash);
        _ ->
            %% A crash or disagreement must fail loudly: surface both
            %% arms' outcomes as a hard failure.
            ?assertEqual({immediate, ImmediateOutcome}, {batch, BatchOutcome})
    end.

outcome_summary({ok, {Hash, _Tf}}) -> {ok, Hash};
outcome_summary({crashed, C, E})   -> {crashed, C, E}.

%%%===================================================================
%%% Helpers
%%%===================================================================

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
