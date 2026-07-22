-module(aec_block_generator_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(GENERATOR, aec_block_generator).
-define(WAIT_MS, 1000).
-define(QUARANTINE_CAP, 1000). %% mirrors aec_block_generator's MAX_QUARANTINE_ENTRIES

block_generator_top_change_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"rebuilds on deferred micro top after worker returns {error, _}",
       fun test_rebuilds_after_failed_worker/0},
      {"rebuilds on deferred micro top after worker crashes",
       fun test_rebuilds_after_worker_down/0},
      {"does not reuse a stale candidate after update worker failure",
       fun test_does_not_reuse_stale_candidate_after_update_failure/0},
      {"does not publish stale candidate after micro top change",
       fun test_does_not_publish_stale_candidate_after_top_change/0}]}.

%% Offending-tx quarantine (livelock breadcrumb + TTL-bounded exclusion).
sec_bb_3_quarantine_test_() ->
    {foreach,
     fun setup/0,
     fun teardown_quarantine_env/1,
     [{"a tx in flight when its worker is hard-killed on preempt is quarantined"
       " and excluded from the very next build (forward progress, not re-applied)",
       fun test_quarantines_offending_tx_after_preempt/0},
      {"a quarantined tx becomes eligible again once its TTL expires",
       fun test_quarantine_ttl_expiry_reallows_tx/0},
      {"a tx retried after its TTL expires is re-quarantined if it is still"
       " slow, with no cap - the TTL alone bounds each exclusion, forever",
       fun test_quarantine_ttl_retry_then_requarantine_if_still_slow/0},
      {"a worker that completes normally (even with an in-flight report) does"
       " not quarantine anything",
       fun test_normal_completion_does_not_quarantine/0},
      {"a tx already applied successfully is never quarantined merely because"
       " its worker is later hard-killed over a DIFFERENT, still in-flight batch"
       " (false-positive-quarantine race)",
       fun test_no_false_positive_quarantine_after_earlier_batch_applied/0},
      {"an administrative stop (not a top_changed preempt) hard-kills the"
       " worker but never quarantines its in-flight batch",
       fun test_admin_stop_does_not_quarantine/0},
      {"candidate_quarantine_ttl_ms defaults to on (cycle-derived TTL): an"
       " offending tx is quarantined after a preempt out of the box",
       fun test_default_ttl_enables_quarantine/0},
      {"an explicit candidate_quarantine_ttl_ms of 0 opts out: quarantine"
       " stays inert even after a preempt",
       fun test_explicit_zero_disables_quarantine/0}]}.

%% Direct unit tests of the cycle-derived clamp (no gen_server needed).
quarantine_ttl_clamp_test_() ->
    {foreach,
     fun() -> application:unset_env(aecore, candidate_quarantine_ttl_ms) end,
     fun(_) -> application:unset_env(aecore, candidate_quarantine_ttl_ms) end,
     [{"unset config defaults to 3x the micro block cycle",
       fun test_quarantine_ttl_default_is_3x_cycle/0},
      {"a value below the floor is clamped up to 1x the cycle",
       fun test_quarantine_ttl_clamps_to_floor/0},
      {"a value above the ceiling is clamped down to 10x the cycle",
       fun test_quarantine_ttl_clamps_to_ceiling/0},
      {"an explicit 0 disables the mechanism, unclamped",
       fun test_quarantine_ttl_zero_stays_zero/0}]}.

%% Direct unit tests of the size cap (no gen_server needed).
quarantine_size_cap_test_() ->
    {foreach,
     fun() -> application:set_env(aecore, candidate_quarantine_ttl_ms, 60000) end,
     fun(_) -> application:unset_env(aecore, candidate_quarantine_ttl_ms) end,
     [{"quarantine_batch bounds the map at the size cap",
       fun test_quarantine_batch_is_size_capped/0},
      {"re-quarantining an existing key refreshes it without growing the map",
       fun test_quarantine_batch_refresh_does_not_grow/0}]}.

%% Before/after proof: does quarantine actually resolve the 0-block issue
%% (a slow tx re-selected on every rebuild stalls the leader forever)?
zero_block_proof_test_() ->
    {foreach,
     fun setup/0,
     fun teardown_quarantine_env/1,
     [{"BEFORE: quarantine off - the offending tx is re-selected on every"
       " rebuild, every build stalls, and no non-empty microblock candidate"
       " is ever published (0-block issue reproduced)",
       fun test_zero_block_before_quarantine_off/0},
      {"AFTER: quarantine on - the offending tx is excluded once quarantined,"
       " every later build succeeds, and progress (publishes) is restored",
       fun test_zero_block_after_quarantine_on/0},
      {"HONESTY: quarantine only fires on a preempt - an uncontested stall"
       " (no top_changed ever arrives) is never captured and makes no"
       " progress either (known efficacy gap, not a regression)",
       fun test_efficacy_gap_uncontested_stall_not_quarantined/0}]}.

setup() ->
    meck:new(aec_events, [non_strict]),
    meck:expect(aec_events, subscribe, fun(_) -> ok end),
    meck:expect(aec_events, publish, fun(_, _) -> ok end),

    meck:new(aec_chain, [non_strict]),
    meck:expect(aec_chain, top_block, fun() -> <<"top-0">> end),

    meck:new(aec_block_micro_candidate, [non_strict]),
    meck:new(aec_blocks, [non_strict]),
    meck:expect(aec_blocks, txs, fun(_) -> [dummy_tx] end),

    {ok, _Pid} = ?GENERATOR:start_link(),
    ok.

teardown(_) ->
    case whereis(?GENERATOR) of
        undefined -> ok;
        _Pid -> catch ?GENERATOR:stop()
    end,
    meck:unload(aec_blocks),
    meck:unload(aec_block_micro_candidate),
    meck:unload(aec_chain),
    meck:unload(aec_events),
    flush_test_mailbox(),
    ok.

test_rebuilds_after_failed_worker() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-1">>,
    ContinueRef = make_ref(),

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, _Opts) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              receive
                  {continue, ContinueRef} -> {error, simulated_failure}
              end;
         (Top, _Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self()},
              receive after infinity -> ok end
      end),

    ?GENERATOR:start_generation(),
    WorkerPid = wait_for_create(InitialTop),
    defer_micro_top(DeferredTop),
    WorkerPid ! {continue, ContinueRef},
    wait_for_create(DeferredTop).

test_rebuilds_after_worker_down() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-2">>,
    CrashRef = make_ref(),

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, _Opts) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              receive
                  {crash, CrashRef} -> exit(simulated_crash)
              end;
         (Top, _Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self()},
              receive after infinity -> ok end
      end),

    ?GENERATOR:start_generation(),
    WorkerPid = wait_for_create(InitialTop),
    defer_micro_top(DeferredTop),
    WorkerPid ! {crash, CrashRef},
    wait_for_create(DeferredTop).

test_does_not_reuse_stale_candidate_after_update_failure() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-1">>,
    Candidate0 = candidate_0,
    Candidate1 = candidate_1,
    State0 = candidate_state_0,
    State1 = candidate_state_1,
    Tx1 = tx_1,
    Tx2 = tx_2,
    ContinueRef = make_ref(),

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, _Opts) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              {ok, Candidate0, State0};
         (Top, _Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self()},
              {ok, Candidate1, State1}
      end),
    meck:expect(
      aec_block_micro_candidate,
      update,
      fun(Block, Txs, BlockInfo) when Block =:= Candidate0,
                                      Txs =:= [Tx1],
                                      BlockInfo =:= State0 ->
              TestPid ! {update_called, Candidate0, [Tx1], State0, self()},
              receive
                  {continue, ContinueRef} -> {error, simulated_failure}
              end;
         (Block, Txs, BlockInfo) when Block =:= Candidate1,
                                      Txs =:= [Tx2],
                                      BlockInfo =:= State1 ->
              TestPid ! {update_called, Candidate1, [Tx2], State1, self()},
              receive after infinity -> ok end;
         (Block, Txs, BlockInfo) ->
              TestPid ! {unexpected_update_called, Block, Txs, BlockInfo, self()},
              receive after infinity -> ok end
      end),

    ?GENERATOR:start_generation(),
    wait_for_create(InitialTop),
    wait_for_candidate(Candidate0),

    send_tx(Tx1),
    UpdateWorkerPid = wait_for_update(Candidate0, [Tx1], State0),
    defer_micro_top(DeferredTop),
    UpdateWorkerPid ! {continue, ContinueRef},

    send_tx(Tx2),
    assert_progress_uses_fresh_top(DeferredTop, Candidate0, Candidate1, State1, Tx2).

test_does_not_publish_stale_candidate_after_top_change() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-1">>,
    Candidate0 = candidate_0,
    Candidate1 = candidate_1,
    State0 = candidate_state_0,
    State1 = candidate_state_1,
    ContinueRef = make_ref(),

    meck:expect(
      aec_events,
      publish,
      fun(candidate_block, new_candidate) ->
              TestPid ! candidate_published,
              ok;
         (_, _) ->
              ok
      end),
    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, _Opts) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              receive
                  {continue, ContinueRef} -> {ok, Candidate0, State0}
              end;
         (Top, _Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self()},
              {ok, Candidate1, State1}
      end),

    ?GENERATOR:start_generation(),
    InitialWorkerPid = wait_for_create(InitialTop),
    defer_micro_top(DeferredTop),
    InitialWorkerPid ! {continue, ContinueRef},

    wait_for_create(DeferredTop),
    wait_for_candidate(Candidate1),
    ?assertEqual(Candidate1, current_candidate()),
    ?assertEqual(1, count_candidate_publishes()).

wait_for_create(ExpectedTop) ->
    receive
        {create_called, ExpectedTop, WorkerPid} ->
            WorkerPid
    after ?WAIT_MS ->
        ?assert(false)
    end.

%% Like wait_for_create/1, but also captures the ignore_tx_hashes the
%% worker was spawned with.
wait_for_create_with_ignore(ExpectedTop) ->
    receive
        {create_called, ExpectedTop, WorkerPid, IgnoreTxHashes} ->
            {WorkerPid, IgnoreTxHashes}
    after ?WAIT_MS ->
        ?assert(false)
    end.

wait_for_update(ExpectedCandidate, ExpectedTxs, ExpectedState) ->
    receive
        {update_called, ExpectedCandidate, ExpectedTxs, ExpectedState, WorkerPid} ->
            WorkerPid
    after ?WAIT_MS ->
        ?assert(false)
    end.

wait_for_candidate(ExpectedCandidate) ->
    wait_for_candidate(ExpectedCandidate, 50).

wait_for_candidate(_ExpectedCandidate, 0) ->
    ?assert(false);
wait_for_candidate(ExpectedCandidate, RetriesLeft) ->
    case ?GENERATOR:get_candidate() of
        {ok, ExpectedCandidate} ->
            ok;
        _ ->
            timer:sleep(20),
            wait_for_candidate(ExpectedCandidate, RetriesLeft - 1)
    end.

current_candidate() ->
    case ?GENERATOR:get_candidate() of
        {ok, Candidate} -> Candidate;
        {error, no_candidate} -> no_candidate
    end.

send_tx(Tx) ->
    ?GENERATOR ! {gproc_ps_event, tx_created, #{info => Tx}},
    ok.

defer_micro_top(DeferredTop) ->
    ?GENERATOR ! {gproc_ps_event, top_changed,
                  #{info => #{block_type => micro, block_hash => DeferredTop}}},
    ok.

assert_progress_uses_fresh_top(DeferredTop, StaleCandidate, FreshCandidate, FreshState, Tx) ->
    receive
        {unexpected_update_called, StaleCandidate, [Tx], _OldState, _WorkerPid} ->
            ?assert(false);
        {update_called, StaleCandidate, [Tx], _OldState, _WorkerPid} ->
            ?assert(false);
        {create_called, DeferredTop, _WorkerPid} ->
            wait_for_candidate(FreshCandidate),
            wait_for_update(FreshCandidate, [Tx], FreshState),
            assert_no_stale_update_after_fresh_progress(StaleCandidate, Tx);
        {update_called, FreshCandidate, [Tx], FreshState, _WorkerPid} ->
            assert_no_stale_update_after_fresh_progress(StaleCandidate, Tx)
    after ?WAIT_MS ->
        ?assert(false)
    end.

assert_no_stale_update_after_fresh_progress(StaleCandidate, Tx) ->
    assert_no_stale_update_after_fresh_progress(StaleCandidate, Tx, 10).

assert_no_stale_update_after_fresh_progress(_StaleCandidate, _Tx, 0) ->
    ok;
assert_no_stale_update_after_fresh_progress(StaleCandidate, Tx, ChecksLeft) ->
    receive
        {unexpected_update_called, StaleCandidate, [Tx], _OldState, _WorkerPid} ->
            ?assert(false);
        {update_called, StaleCandidate, [Tx], _OldState, _WorkerPid} ->
            ?assert(false)
    after 20 ->
        assert_no_stale_update_after_fresh_progress(StaleCandidate, Tx, ChecksLeft - 1)
    end.

count_candidate_publishes() ->
    count_candidate_publishes(0).

count_candidate_publishes(Acc) ->
    receive
        candidate_published ->
            count_candidate_publishes(Acc + 1)
    after 100 ->
        Acc
    end.

flush_test_mailbox() ->
    receive
        _Msg -> flush_test_mailbox()
    after 0 ->
        ok
    end.

%% -- Quarantine tests ----------------------------------------------------

teardown_quarantine_env(Ctx) ->
    application:unset_env(aecore, candidate_quarantine_ttl_ms),
    teardown(Ctx).

%% Tests set a long, explicit TTL so the mechanism doesn't race the TTL
%% expiring mid-test.
test_quarantines_offending_tx_after_preempt() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-10">>,
    SlowTxHash = <<"slow-tx-hash-a">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              %% Simulate the worker's pre-apply breadcrumb, then hang - the
              %% generator's only way to stop this worker is the hard-kill.
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_final, state_final}
      end),

    ?GENERATOR:start_generation(),
    {_WorkerPid, IgnoreTxHashes0} = wait_for_create_with_ignore(InitialTop),
    ?assertEqual([], IgnoreTxHashes0),

    %% Preempt (e.g. the leader's own microblock top_changed): the generator
    %% hard-kills the worker above while SlowTxHash was in flight.
    defer_micro_top(DeferredTop),

    {_WorkerPid2, IgnoreTxHashes1} = wait_for_create_with_ignore(DeferredTop),
    ?assert(lists:member(SlowTxHash, IgnoreTxHashes1)),

    %% Forward progress: the rebuild actually completes and publishes,
    %% instead of looping on the same offending tx forever.
    wait_for_candidate(candidate_final).

%% Unit-tests purge_expired_quarantine/1 directly rather than sleeping past a
%% TTL - the floor clamp (>= 1x micro_block_cycle) makes wall-clock waits slow.
test_quarantine_ttl_expiry_reallows_tx() ->
    SlowTxHash = <<"slow-tx-hash-b">>,
    Now = erlang:monotonic_time(millisecond),
    Expired = #{ SlowTxHash => Now - 1 },

    {Ignore, Quarantine1} = ?GENERATOR:purge_expired_quarantine(Expired),
    ?assertNot(lists:member(SlowTxHash, Ignore)),
    ?assertEqual(#{}, Quarantine1).

test_quarantine_ttl_retry_then_requarantine_if_still_slow() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    SlowTxHash = <<"slow-tx-hash-e">>,
    Now = erlang:monotonic_time(millisecond),

    %% TTL lapsed: purge drops it, so a rebuild would reselect it.
    Expired = #{ SlowTxHash => Now - 1 },
    {IgnoreAfterExpiry, Quarantine1} = ?GENERATOR:purge_expired_quarantine(Expired),
    ?assertNot(lists:member(SlowTxHash, IgnoreAfterExpiry)),

    %% Still slow on retry -> re-quarantined for a fresh span, no cap.
    Quarantine2 = ?GENERATOR:quarantine_batch([SlowTxHash], Quarantine1),
    {IgnoreAfterRequarantine, _Quarantine3} = ?GENERATOR:purge_expired_quarantine(Quarantine2),
    ?assert(lists:member(SlowTxHash, IgnoreAfterRequarantine)).

test_normal_completion_does_not_quarantine() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-40">>,
    SlowTxHash = <<"slow-tx-hash-d">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              %% Completes normally, even though it reported in-flight just before returning.
              {ok, candidate_normal, state_normal};
         (Top, Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_final, state_final}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create_with_ignore(InitialTop),
    wait_for_candidate(candidate_normal),

    %% A later, unrelated top change must not find SlowTxHash quarantined.
    defer_micro_top(DeferredTop),
    {_W, IgnoreTxHashes} = wait_for_create_with_ignore(DeferredTop),
    ?assertNot(lists:member(SlowTxHash, IgnoreTxHashes)).

%% FastTxHash is reported then cleared (already applied); only the still
%% in-flight SlowTxHash may be blamed by a later preempt.
test_no_false_positive_quarantine_after_earlier_batch_applied() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-60">>,
    FastTxHash = <<"fast-tx-hash">>,
    SlowTxHash = <<"slow-tx-hash-g">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              ReportInFlightFun = maps:get(report_in_flight_fun, Opts),
              %% First batch: in flight, then applied successfully and cleared.
              ReportInFlightFun([FastTxHash]),
              ReportInFlightFun([]),
              %% Second batch: in flight, then this worker hangs.
              ReportInFlightFun([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_final, state_final}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create_with_ignore(InitialTop),

    %% Preempt while the SECOND batch is in flight.
    defer_micro_top(DeferredTop),

    {_WorkerPid2, IgnoreTxHashes} = wait_for_create_with_ignore(DeferredTop),
    ?assert(lists:member(SlowTxHash, IgnoreTxHashes)),
    ?assertNot(lists:member(FastTxHash, IgnoreTxHashes)),

    wait_for_candidate(candidate_final).

test_admin_stop_does_not_quarantine() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    SlowTxHash = <<"slow-tx-hash-f">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end
      end),

    ?GENERATOR:start_generation(),
    wait_for_create_with_ignore(InitialTop),

    %% Administrative stop/restart, not a preempt - must not quarantine SlowTxHash.
    ?GENERATOR:stop_generation(),
    ?GENERATOR:start_generation(),

    {_WorkerPid2, IgnoreTxHashes} = wait_for_create_with_ignore(InitialTop),
    ?assertNot(lists:member(SlowTxHash, IgnoreTxHashes)).

%% No env set - relies on the runtime, cycle-derived default (on).
test_default_ttl_enables_quarantine() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-70">>,
    SlowTxHash = <<"slow-tx-hash-h">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_final, state_final}
      end),

    ?GENERATOR:start_generation(),
    {_WorkerPid, IgnoreTxHashes0} = wait_for_create_with_ignore(InitialTop),
    ?assertEqual([], IgnoreTxHashes0),

    defer_micro_top(DeferredTop),

    {_WorkerPid2, IgnoreTxHashes1} = wait_for_create_with_ignore(DeferredTop),
    ?assert(lists:member(SlowTxHash, IgnoreTxHashes1)),

    %% Forward progress: the rebuild completes and publishes.
    wait_for_candidate(candidate_final).

test_explicit_zero_disables_quarantine() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 0),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTop = <<"top-71">>,
    SlowTxHash = <<"slow-tx-hash-i">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_final, state_final}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create_with_ignore(InitialTop),

    defer_micro_top(DeferredTop),

    {_WorkerPid2, IgnoreTxHashes1} = wait_for_create_with_ignore(DeferredTop),
    ?assertEqual([], IgnoreTxHashes1),

    wait_for_candidate(candidate_final).

%% -- Quarantine TTL clamp (direct unit tests) ----------------------------

test_quarantine_ttl_default_is_3x_cycle() ->
    Cycle = aec_governance:micro_block_cycle(),
    ?assertEqual(Cycle * 3, ?GENERATOR:quarantine_ttl_ms()).

test_quarantine_ttl_clamps_to_floor() ->
    Cycle = aec_governance:micro_block_cycle(),
    application:set_env(aecore, candidate_quarantine_ttl_ms, 10),
    ?assertEqual(Cycle, ?GENERATOR:quarantine_ttl_ms()).

test_quarantine_ttl_clamps_to_ceiling() ->
    Cycle = aec_governance:micro_block_cycle(),
    application:set_env(aecore, candidate_quarantine_ttl_ms, 100000000),
    ?assertEqual(Cycle * 10, ?GENERATOR:quarantine_ttl_ms()).

test_quarantine_ttl_zero_stays_zero() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 0),
    ?assertEqual(0, ?GENERATOR:quarantine_ttl_ms()).

%% -- Quarantine size cap (direct unit tests) -----------------------------

test_quarantine_batch_is_size_capped() ->
    TxHashes = [integer_to_binary(N) || N <- lists:seq(1, ?QUARANTINE_CAP + 50)],
    Quarantine = ?GENERATOR:quarantine_batch(TxHashes, #{}),
    ?assertEqual(?QUARANTINE_CAP, map_size(Quarantine)).

test_quarantine_batch_refresh_does_not_grow() ->
    TxHashes = [integer_to_binary(N) || N <- lists:seq(1, ?QUARANTINE_CAP)],
    Quarantine0 = ?GENERATOR:quarantine_batch(TxHashes, #{}),
    ?assertEqual(?QUARANTINE_CAP, map_size(Quarantine0)),

    Quarantine1 = ?GENERATOR:quarantine_batch([hd(TxHashes)], Quarantine0),
    ?assertEqual(?QUARANTINE_CAP, map_size(Quarantine1)).

%% -- Zero-block proof (before/after) -------------------------------------

%% Models the incident's slow tx generically, by ignore_tx_hashes content
%% rather than by Top: hangs (having reported itself in flight) whenever
%% SlowTxHash is still selectable, succeeds once it is excluded. The
%% candidate/state are tagged by Top so callers can wait_for_candidate/1 on
%% a specific rebuild instead of racing on a single shared value.
slow_tx_create_fun(TestPid, SlowTxHash) ->
    fun(Top, Opts) ->
        IgnoreTxHashes = maps:get(ignore_tx_hashes, Opts, []),
        case lists:member(SlowTxHash, IgnoreTxHashes) of
            true ->
                TestPid ! {create_called, Top, self(), IgnoreTxHashes},
                {ok, {candidate_for, Top}, {state_for, Top}};
            false ->
                (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
                TestPid ! {create_called, Top, self(), IgnoreTxHashes},
                receive after infinity -> ok end
        end
    end.

publish_to_test_pid(TestPid) ->
    fun(candidate_block, new_candidate) ->
            TestPid ! candidate_published,
            ok;
       (_, _) ->
            ok
    end.

%% BEFORE: TTL 0 (quarantine off) - the failure loop from the incident.
%% Every preempt re-selects the same slow tx, every build hangs again, and
%% the leader publishes nothing across all K top-changes.
test_zero_block_before_quarantine_off() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 0),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    SlowTxHash = <<"slow-tx-hash-zb">>,
    DeferredTops = [<<"top-zb-1">>, <<"top-zb-2">>, <<"top-zb-3">>,
                    <<"top-zb-4">>, <<"top-zb-5">>],

    meck:expect(aec_events, publish, publish_to_test_pid(TestPid)),
    meck:expect(aec_block_micro_candidate, create, slow_tx_create_fun(TestPid, SlowTxHash)),

    ?GENERATOR:start_generation(),
    {_W0, Ignore0} = wait_for_create_with_ignore(InitialTop),
    ?assertEqual([], Ignore0),

    lists:foreach(
      fun(Top) ->
              defer_micro_top(Top),
              {_W, Ignore} = wait_for_create_with_ignore(Top),
              %% Quarantine disabled: the offending tx is selectable again.
              ?assertEqual([], Ignore)
      end, DeferredTops),

    %% 0-block issue reproduced: not a single non-empty candidate published.
    ?assertEqual(0, count_candidate_publishes()).

%% AFTER: a long TTL (quarantine on) - the same scenario, but the tx gets
%% quarantined on the first preempt and every later build succeeds.
test_zero_block_after_quarantine_on() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    SlowTxHash = <<"slow-tx-hash-za">>,
    DeferredTops = [<<"top-za-1">>, <<"top-za-2">>, <<"top-za-3">>,
                    <<"top-za-4">>, <<"top-za-5">>],

    meck:expect(aec_events, publish, publish_to_test_pid(TestPid)),
    meck:expect(aec_block_micro_candidate, create, slow_tx_create_fun(TestPid, SlowTxHash)),

    ?GENERATOR:start_generation(),
    {_W0, Ignore0} = wait_for_create_with_ignore(InitialTop),
    ?assertEqual([], Ignore0),

    lists:foreach(
      fun(Top) ->
              defer_micro_top(Top),
              {_W, Ignore} = wait_for_create_with_ignore(Top),
              %% Quarantined from the first preempt onward: excluded every time.
              ?assert(lists:member(SlowTxHash, Ignore)),
              wait_for_candidate({candidate_for, Top})
      end, DeferredTops),

    %% Progress restored: one non-empty publish per top-change.
    ?assertEqual(length(DeferredTops), count_candidate_publishes()).

%% HONESTY: quarantine is preempt-triggered only. With the worker stalled
%% and no top_changed ever arriving, nothing gets quarantined and nothing
%% is published either - this is a known efficacy gap, not a bug, and this
%% test is expected to (and does) pass, documenting it rather than hiding it.
test_efficacy_gap_uncontested_stall_not_quarantined() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60000),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    SlowTxHash = <<"slow-tx-hash-gap">>,

    meck:expect(aec_events, publish, publish_to_test_pid(TestPid)),
    meck:expect(aec_block_micro_candidate, create, slow_tx_create_fun(TestPid, SlowTxHash)),

    ?GENERATOR:start_generation(),
    {_WorkerPid, IgnoreTxHashes0} = wait_for_create_with_ignore(InitialTop),
    ?assertEqual([], IgnoreTxHashes0),

    %% No top_changed is ever sent - the worker just stays hung, uncontested.
    ?assertEqual(no_candidate, current_candidate()),
    ?assertEqual(0, count_candidate_publishes()).

-endif.
