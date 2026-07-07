-module(aec_block_generator_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(GENERATOR, aec_block_generator).
-define(WAIT_MS, 1000).

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
      {"candidate_quarantine_ttl_ms defaults to 0 (disabled): quarantine is"
       " inert out of the box, so selection is unaffected",
       fun test_default_ttl_disables_quarantine/0}]}.

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

%% Tests set an explicit, long TTL (disabled by default) to isolate the
%% quarantine mechanism from the TTL expiring mid-test.
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

test_quarantine_ttl_expiry_reallows_tx() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTopA = <<"top-20">>,
    DeferredTopB = <<"top-21">>,
    SlowTxHash = <<"slow-tx-hash-b">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTopA ->
              TestPid ! {create_called, DeferredTopA, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_a, state_a};
         (Top, Opts) when Top =:= DeferredTopB ->
              TestPid ! {create_called, DeferredTopB, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_b, state_b}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create_with_ignore(InitialTop),

    defer_micro_top(DeferredTopA),
    {_W1, IgnoreA} = wait_for_create_with_ignore(DeferredTopA),
    ?assert(lists:member(SlowTxHash, IgnoreA)),
    wait_for_candidate(candidate_a),

    %% Let the (short, test-configured) TTL lapse before the next build.
    timer:sleep(150),

    defer_micro_top(DeferredTopB),
    {_W2, IgnoreB} = wait_for_create_with_ignore(DeferredTopB),
    ?assertNot(lists:member(SlowTxHash, IgnoreB)).

test_quarantine_ttl_retry_then_requarantine_if_still_slow() ->
    application:set_env(aecore, candidate_quarantine_ttl_ms, 60),
    TestPid = self(),
    InitialTop = <<"top-0">>,
    DeferredTopA = <<"top-50">>,
    DeferredTopB = <<"top-51">>,
    DeferredTopC = <<"top-52">>,
    SlowTxHash = <<"slow-tx-hash-e">>,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top, Opts) when Top =:= InitialTop ->
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, InitialTop, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTopA ->
              %% First quarantine cycle: builds without the offending tx.
              TestPid ! {create_called, DeferredTopA, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_a, state_a};
         (Top, Opts) when Top =:= DeferredTopB ->
              %% TTL lapsed - tx reselected and still slow, so this worker hangs again.
              (maps:get(report_in_flight_fun, Opts))([SlowTxHash]),
              TestPid ! {create_called, DeferredTopB, self(), maps:get(ignore_tx_hashes, Opts, [])},
              receive after infinity -> ok end;
         (Top, Opts) when Top =:= DeferredTopC ->
              TestPid ! {create_called, DeferredTopC, self(), maps:get(ignore_tx_hashes, Opts, [])},
              {ok, candidate_final, state_final}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create_with_ignore(InitialTop),

    %% First preempt: quarantined.
    defer_micro_top(DeferredTopA),
    {_W1, IgnoreA} = wait_for_create_with_ignore(DeferredTopA),
    ?assert(lists:member(SlowTxHash, IgnoreA)),
    wait_for_candidate(candidate_a),

    %% Let the TTL lapse before retrying.
    timer:sleep(150),

    defer_micro_top(DeferredTopB),
    {_W2, IgnoreB} = wait_for_create_with_ignore(DeferredTopB),
    ?assertNot(lists:member(SlowTxHash, IgnoreB)),

    %% Still slow -> preempted again -> re-quarantined (no cap released it).
    defer_micro_top(DeferredTopC),
    {_W3, IgnoreC} = wait_for_create_with_ignore(DeferredTopC),
    ?assert(lists:member(SlowTxHash, IgnoreC)),

    %% Forward progress continues regardless.
    wait_for_candidate(candidate_final).

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

test_default_ttl_disables_quarantine() ->
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
    ?assertEqual([], IgnoreTxHashes1),

    wait_for_candidate(candidate_final).

-endif.
