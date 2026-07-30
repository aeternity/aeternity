%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Secondary TTL-cache hashes (ns_cache_hash/oracles_cache_hash, read back by
%%%    aec_db_gc) match for the batched vs immediate write path and survive a
%%%    serialize/deserialize round trip.
%%% @end
%%%=============================================================================
-module(aec_state_batch_secondary_hash_tests).

-include_lib("eunit/include/eunit.hrl").

secondary_hash_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"serialize_for_db: batch == immediate, and direct (unflushed) == post-commit_to_db",
        fun serialize_for_db_batch_vs_immediate/0}
     , {"ns_cache_hash/oracles_cache_hash: batch == immediate",
        fun cache_hashes_batch_vs_immediate/0}
     , {"persist-then-reload reproduces the same cache hashes, batch or immediate",
        fun round_trip_stable/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%% Same name+oracle ops either deferred (batch) or flushed after each op
%% (immediate).
build(Mode) ->
    Flush = fun(T) -> case Mode of
                          immediate -> aec_trees:flush_state_batches(T);
                          batch     -> T
                      end
            end,
    T0 = aec_trees:new(),

    NH = aec_hash:hash(pubkey, <<"h4-secondary-hash-name">>),
    Name = aens_names:new(NH, <<11:256>>, 500000),
    NT1 = aens_state_tree:enter_name(Name, aec_trees:ns(T0)),
    T1 = Flush(aec_trees:set_ns(T0, NT1)),

    CH = aec_hash:hash(pubkey, <<"h4-secondary-hash-commitment">>),
    Commitment = aens_commitments:new(aeser_id:create(commitment, CH),
                                      aeser_id:create(account, <<12:256>>), 100, 1),
    NT2 = aens_state_tree:enter_commitment(Commitment, aec_trees:ns(T1)),
    T2 = Flush(aec_trees:set_ns(T1, NT2)),

    OPK = <<13:256>>,
    O = aeo_oracles:new(OPK, <<"q">>, <<"r">>, 10, 1000000000, 1),
    OT1 = aeo_state_tree:enter_oracle(O, aec_trees:oracles(T2)),
    T3 = Flush(aec_trees:set_oracles(T2, OT1)),

    Q = aeo_query:new(OPK, <<14:256>>, 1, <<"q">>, 10, 1000000000, {delta, 100}),
    OT2 = aeo_state_tree:insert_query(Q, aec_trees:oracles(T3)),
    T4 = Flush(aec_trees:set_oracles(T3, OT2)),

    T4.

serialize_for_db_batch_vs_immediate() ->
    TBatch = build(batch),
    TImm   = build(immediate),

    %% Real call sites always commit_to_db first.
    SerBatchDisciplined = aec_trees:serialize_for_db(aec_trees:commit_to_db(TBatch)),
    SerImmDisciplined   = aec_trees:serialize_for_db(aec_trees:commit_to_db(TImm)),
    ?assertEqual(SerImmDisciplined, SerBatchDisciplined),

    %% serialize_for_db/1 directly on the live/unflushed Trees (bypassing
    %% commit_to_db/1) must match the disciplined call.
    SerBatchDirect = aec_trees:serialize_for_db(TBatch),
    SerImmDirect   = aec_trees:serialize_for_db(TImm),
    ?debugFmt("~nserialize_for_db/1 direct-vs-committed — "
              "batch: ~p, immediate: ~p~n",
              [SerBatchDirect =:= SerBatchDisciplined,
               SerImmDirect =:= SerImmDisciplined]),
    ?assertEqual(SerBatchDisciplined, SerBatchDirect),
    ?assertEqual(SerImmDisciplined, SerImmDirect).

cache_hashes_batch_vs_immediate() ->
    TBatch = aec_trees:commit_to_db(build(batch)),
    TImm   = aec_trees:commit_to_db(build(immediate)),

    %% The exact hashes aec_db_gc reads back.
    ?assertEqual(aec_trees:ns_hash(TImm), aec_trees:ns_hash(TBatch)),
    ?assertEqual(aec_trees:ns_cache_hash(TImm), aec_trees:ns_cache_hash(TBatch)),
    ?assertEqual(aec_trees:oracles_hash(TImm), aec_trees:oracles_hash(TBatch)),
    ?assertEqual(aec_trees:oracles_cache_hash(TImm), aec_trees:oracles_cache_hash(TBatch)).

round_trip_stable() ->
    lists:foreach(fun round_trip_stable_for_mode/1, [batch, immediate]).

round_trip_stable_for_mode(Mode) ->
    Committed = aec_trees:commit_to_db(build(Mode)),
    Ser = aec_trees:serialize_for_db(Committed),
    Reloaded = aec_trees:deserialize_from_db(Ser, false),

    ?assertEqual(aec_trees:ns_hash(Committed), aec_trees:ns_hash(Reloaded)),
    ?assertEqual(aec_trees:ns_cache_hash(Committed), aec_trees:ns_cache_hash(Reloaded)),
    ?assertEqual(aec_trees:oracles_hash(Committed), aec_trees:oracles_hash(Reloaded)),
    ?assertEqual(aec_trees:oracles_cache_hash(Committed), aec_trees:oracles_cache_hash(Reloaded)),

    %% Re-serializing must reproduce the same bytes (stable fixed point).
    ?assertEqual(Ser, aec_trees:serialize_for_db(Reloaded)).
