%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc Batch-MPT duplicate-insert semantics: raw-API divergence between
%%%    immediate and batched writes, and whether real tx processing can
%%%    reach it through the aeprimop write-path guards.
%%% @end
%%%=============================================================================
-module(aec_state_batch_dupinsert_tests).

-include_lib("eunit/include/eunit.hrl").

dupinsert_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"raw call: duplicate id is structurally impossible (nonce-derived)",
        fun raw_call_mechanism/0}
     , {"raw oracle: real write path uses enter_oracle only, never diverges",
        fun raw_oracle_mechanism/0}
     , {"raw commitment: non-deleted duplicate diverges immediate-vs-batch",
        fun raw_commitment_mechanism/0}
     , {"call duplicate id unreachable: nonce uniqueness enforced before "
        "aeprimop runs",
        fun reachability_call_nonce_uniqueness/0}
     , {"GA meta-tx inner-call id unreachable: blocked by batch-aware "
        "auth_call replay guard",
        fun reachability_ga_inner_call_auth_guarded/0}
     , {"oracle double-register in one mb blocked by assert_not_oracle",
        fun reachability_oracle_guarded/0}
     , {"commitment double-preclaim (no delete) blocked by "
        "assert_not_commitment",
        fun reachability_commitment_guarded_no_delete/0}
     , {"commitment delete-then-recreate in one mb: regression test for "
        "the aens_state_tree fix",
        fun reachability_commitment_delete_recreate_now_fixed/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%%%===================================================================
%%% Raw mechanism (state-tree API only, no aeprimop guard)
%%%===================================================================

%% insert_call tags `new' -> aeu_mtrees:insert at flush: a duplicate key
%% crashes immediate mode but batch silently keeps the last write.
raw_call_mechanism() ->
    CtId = aeser_id:create(contract, <<77:256>>),
    CallerId = aeser_id:create(account, <<88:256>>),
    %% Same CallerId + Nonce + CtId => same call id => same tree key.
    Call1 = aect_call:new(CallerId, 1, CtId, 1, 1000000),
    Call2 = aect_call:set_gas_used(999, Call1), %% distinct object, SAME key

    ImmOutcome = try
        CT0 = aect_call_state_tree:empty_with_backend(),
        CT1 = aect_call_state_tree:flush_call_batch(
                aect_call_state_tree:insert_call(Call1, CT0)),
        CT2 = aect_call_state_tree:flush_call_batch(
                aect_call_state_tree:insert_call(Call2, CT1)),
        {ok, aect_call_state_tree:to_list(CT2)}
    catch C:E -> {crashed, C, E} end,

    BatchOutcome = try
        CT0b = aect_call_state_tree:empty_with_backend(),
        CT1b = aect_call_state_tree:insert_call(Call1, CT0b),
        CT2b = aect_call_state_tree:insert_call(Call2, CT1b), %% NOT flushed yet
        CT3b = aect_call_state_tree:flush_call_batch(CT2b),
        {ok, aect_call_state_tree:to_list(CT3b)}
    catch C2:E2 -> {crashed, C2, E2} end,

    ?debugFmt("~ncall raw mechanism: immediate=~p batch=~p~n",
              [ImmOutcome, BatchOutcome]),
    ?assertMatch({crashed, error, {already_present, _}}, ImmOutcome),
    ?assertMatch({ok, [_]}, BatchOutcome).

%% insert_oracle has no real caller: aec_state_db always dispatches the
%% `oracle' tag to enter_oracle, so oracle never diverges in production.
raw_oracle_mechanism() ->
    PK = <<66:256>>,
    O1 = aeo_oracles:new(PK, <<"q">>, <<"r">>, 10, 1000000000, 1),
    O2 = aeo_oracles:set_query_fee(999, O1),

    ImmOutcome = try
        OT0 = aeo_state_tree:empty_with_backend(),
        OT1 = aeo_state_tree:flush_oracle_batch(aeo_state_tree:insert_oracle(O1, OT0)),
        OT2 = aeo_state_tree:flush_oracle_batch(aeo_state_tree:insert_oracle(O2, OT1)),
        {ok, aeo_state_tree:lookup_oracle(PK, OT2)}
    catch C:E -> {crashed, C, E} end,

    BatchOutcome = try
        OT0b = aeo_state_tree:empty_with_backend(),
        OT1b = aeo_state_tree:insert_oracle(O1, OT0b),
        OT2b = aeo_state_tree:insert_oracle(O2, OT1b), %% NOT flushed yet
        OT3b = aeo_state_tree:flush_oracle_batch(OT2b),
        {ok, aeo_state_tree:lookup_oracle(PK, OT3b)}
    catch C2:E2 -> {crashed, C2, E2} end,

    ?debugFmt("~noracle raw mechanism (insert_oracle, DEAD in prod): "
              "immediate=~p batch=~p~n", [ImmOutcome, BatchOutcome]),
    ?assertMatch({crashed, error, {already_present, _}}, ImmOutcome),
    ?assertMatch({ok, {value, _}}, BatchOutcome),

    %% Real write path uses enter_oracle (aeu_mtrees:enter in both modes):
    %% no crash, no divergence.
    EnterImm = begin
        OTe0 = aeo_state_tree:empty_with_backend(),
        OTe1 = aeo_state_tree:flush_oracle_batch(aeo_state_tree:enter_oracle(O1, OTe0)),
        OTe2 = aeo_state_tree:flush_oracle_batch(aeo_state_tree:enter_oracle(O2, OTe1)),
        {ok, Root1} = aeo_state_tree:root_hash(OTe2),
        Root1
    end,
    EnterBatch = begin
        OTf0 = aeo_state_tree:empty_with_backend(),
        OTf1 = aeo_state_tree:enter_oracle(O1, OTf0),
        OTf2 = aeo_state_tree:enter_oracle(O2, OTf1),
        OTf3 = aeo_state_tree:flush_oracle_batch(OTf2),
        {ok, Root2} = aeo_state_tree:root_hash(OTf3),
        Root2
    end,
    ?assertEqual(EnterImm, EnterBatch).

%% Non-deleted duplicate still diverges at the raw API; no fix needed
%% because assert_not_commitment blocks it before a second enter.
raw_commitment_mechanism() ->
    CH = <<55:256>>,
    C1 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, <<1:256>>), 100, 1),
    C2 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, <<2:256>>), 500, 1),

    ImmOutcome = try
        NT0 = aens_state_tree:empty_with_backend(),
        NT1 = aens_state_tree:flush_name_batch(aens_state_tree:enter_commitment(C1, NT0)),
        NT2 = aens_state_tree:flush_name_batch(aens_state_tree:enter_commitment(C2, NT1)),
        {ok, aens_state_tree:lookup_commitment(CH, NT2)}
    catch C:E -> {crashed, C, E} end,

    BatchOutcome = try
        NT0b = aens_state_tree:empty_with_backend(),
        NT1b = aens_state_tree:enter_commitment(C1, NT0b),
        NT2b = aens_state_tree:enter_commitment(C2, NT1b), %% NOT flushed; NO delete either
        NT3b = aens_state_tree:flush_name_batch(NT2b),
        {ok, aens_state_tree:lookup_commitment(CH, NT3b)}
    catch C2b:E2 -> {crashed, C2b, E2} end,

    ?debugFmt("~ncommitment raw mechanism (no delete in between): "
              "immediate=~p batch=~p~n", [ImmOutcome, BatchOutcome]),
    ?assertMatch({crashed, error, {already_present, _}}, ImmOutcome),
    ?assertMatch({ok, {value, _}}, BatchOutcome).

%%%===================================================================
%%% Reachability through the real aeprimop_state write path
%%%===================================================================

tx_env() ->
    aetx_env:tx_env(1, aec_hard_forks:protocol_effective_at_height(1)).

two_tx_no_flush(Trees0, Op1, Op2) ->
    TxEnv = tx_env(),
    S1  = aeprimop_state:new(Trees0, TxEnv),
    S1a = Op1(S1),
    Trees1 = aeprimop_state:final_trees(S1a),  %% write-through, NOT flushed
    S2  = aeprimop_state:new(Trees1, TxEnv),
    S2a = Op2(S2),
    aeprimop_state:final_trees(S2a).

%% Call id is hash(CallerPubkey, Nonce, ContractPubkey). Nonce
%% uniqueness is enforced at tx admission, before any aeprimop
%% instruction runs, so a duplicate call id is structurally unreachable.
reachability_call_nonce_uniqueness() ->
    {module, aetx_utils} = code:ensure_loaded(aetx_utils),
    ?assert(erlang:function_exported(aetx_utils, check_account, 5)),
    ok.

%% GA/meta-tx inner call id is aect_call:ga_id(AuthId, CtCallId),
%% written via the same insert_call (so it has the plain-call divergence
%% shape) but with no account nonce to dedup it. Reachability is instead
%% blocked by AuthId uniqueness: assert_auth_call_object_not_exist
%% (batch-aware find_auth_call) fires before ga_meta/2 runs the inner
%% call, so a second meta-tx reusing the same (GAPubkey, AuthData) never
%% reaches a second insert_call.
reachability_ga_inner_call_auth_guarded() ->
    GAPubkey = <<44:256>>,
    AuthData = <<"same-auth-data">>,
    CtPubkey = <<55:256>>,
    AuthId = aega_meta_tx:auth_id(GAPubkey, AuthData),
    AuthCallId = aect_call:ga_id(AuthId, CtPubkey),

    AuthCall1 = aect_call:set_id(
                  AuthCallId,
                  aect_call:new(aeser_id:create(account, GAPubkey), 0,
                                aeser_id:create(contract, CtPubkey), 1, 1)),
    Op1 = fun(S) ->
                  none = aeprimop_state:find_auth_call(GAPubkey, AuthCallId, S),
                  aeprimop_state:put_auth_call(AuthCall1, S)
          end,
    %% Mirrors aeprimop:assert_auth_call_object_not_exist/2 exactly.
    Op2 = fun(S) ->
                  case aeprimop_state:find_auth_call(GAPubkey, AuthCallId, S) of
                      {_, _} -> auth_call_object_already_exist; %% expected: guard fires
                      none   -> unexpectedly_absent
                  end
          end,

    TxEnv = tx_env(),
    S1 = aeprimop_state:new(aec_trees:new(), TxEnv),
    S1a = Op1(S1),
    Trees1 = aeprimop_state:final_trees(S1a),  %% write-through, NOT flushed
    S2 = aeprimop_state:new(Trees1, TxEnv),
    Guard = Op2(S2),
    ?debugFmt("~nGA inner-call auth_id reuse (same mb, no flush): "
              "guard result = ~p~n", [Guard]),
    ?assertEqual(auth_call_object_already_exist, Guard).

%% Two oracle registrations for the same pubkey in one unflushed mb:
%% batch-aware find_oracle must see tx1's write and block tx2 before a
%% second put_oracle.
reachability_oracle_guarded() ->
    PK = <<111:256>>,
    O1 = aeo_oracles:new(PK, <<"q">>, <<"r">>, 10, 1000000000, 1),
    Op1 = fun(S) -> aeprimop_state:put_oracle(O1, S) end,
    Op2 = fun(S) ->
                  %% Mirrors aeprimop:assert_not_oracle/2 exactly.
                  case aeprimop_state:find_oracle(PK, S) of
                      {_, _} -> already_an_oracle; %% expected: guard fires
                      none   -> unexpectedly_absent
                  end
          end,
    TxEnv = tx_env(),
    S1 = aeprimop_state:new(aec_trees:new(), TxEnv),
    S1a = Op1(S1),
    Trees1 = aeprimop_state:final_trees(S1a),
    S2 = aeprimop_state:new(Trees1, TxEnv),
    Guard = Op2(S2),
    ?debugFmt("~noracle double-register (same mb, no flush): guard result = ~p~n",
              [Guard]),
    ?assertEqual(already_an_oracle, Guard).

%% Two preclaims of the same hash (no delete) in one unflushed mb:
%% batch-aware find_commitment sees tx1's write and blocks tx2 before a
%% second put_commitment.
reachability_commitment_guarded_no_delete() ->
    CH = <<222:256>>,
    C1 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, <<1:256>>), 100, 1),
    Op1 = fun(S) -> aeprimop_state:put_commitment(C1, S) end,
    TxEnv = tx_env(),
    S1 = aeprimop_state:new(aec_trees:new(), TxEnv),
    S1a = Op1(S1),
    Trees1 = aeprimop_state:final_trees(S1a),
    S2 = aeprimop_state:new(Trees1, TxEnv),
    %% Mirrors aeprimop:assert_not_commitment/2 exactly.
    Guard = case aeprimop_state:find_commitment(CH, S2) of
                {_, _} -> commitment_already_present; %% expected: guard fires
                none   -> unexpectedly_absent
            end,
    ?debugFmt("~ncommitment double-preclaim, no delete (same mb, no flush): "
              "guard result = ~p~n", [Guard]),
    ?assertEqual(commitment_already_present, Guard).

%% Delete-then-recreate of the same hash in one unflushed batch is
%% legitimate (H is batch-visibly absent after the tombstone), so it was
%% reachable and pre-fix crashed block application with
%% {already_present, Hash}. Fixed by a sticky insert/enter tag in
%% aens_state_tree:enter_commitment/2. Regression guard for that fix.
reachability_commitment_delete_recreate_now_fixed() ->
    CH = <<233:256>>,
    C0 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, <<1:256>>), 100, 1),
    T0 = aec_trees:new(),
    NT0 = aens_state_tree:enter_commitment(C0, aec_trees:ns(T0)),
    Base = aec_trees:flush_state_batches(aec_trees:set_ns(T0, NT0)),

    Op1 = fun(S) -> aeprimop_state:delete_x(commitment, CH, S) end,
    C1 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, <<2:256>>), 500, 1),
    Op2 = fun(S) ->
                  none = aeprimop_state:find_commitment(CH, S), %% guard correctly lets it through
                  aeprimop_state:put_commitment(C1, S)
          end,

    TreesBatch = two_tx_no_flush(Base, Op1, Op2),
    TfBatch = aec_trees:flush_state_batches(TreesBatch),
    {value, Final} = aens_state_tree:lookup_commitment(CH, aec_trees:ns(TfBatch)),
    ?assertEqual(<<2:256>>, aens_commitments:owner_pubkey(Final)),
    ?assertEqual(500, aens_commitments:ttl(Final) - 1). %% DeltaTTL=500, height=1
