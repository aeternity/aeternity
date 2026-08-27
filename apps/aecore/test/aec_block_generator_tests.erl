-module(aec_block_generator_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(GENERATOR, aec_block_generator).
-define(WAIT_MS, 1000).
%% How long to watch for something that must NOT happen.
-define(SETTLE_MS, 200).
%% Covers the retry taken at once and the module's next three, at 100, 200 and 400 ms,
%% so a backoff that stopped growing shows up as a higher count.
-define(BACKOFF_WINDOW_MS, 1000).
%% A retry taken at once needs no timer so cannot be late; one the backoff has
%% reached is several hundred ms out and cannot be this early.
-define(IMMEDIATE_RETRY_MS, 250).

%% Directly, because reaching the clamps through the gen_server would take ~30 s
retry_delay_test_() ->
    [?_assertEqual(100, ?GENERATOR:retry_delay(1)),
     ?_assertEqual(100, ?GENERATOR:retry_delay(2)),
     ?_assertEqual(200, ?GENERATOR:retry_delay(3)),
     ?_assertEqual(5000, ?GENERATOR:retry_delay(50))].

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

block_generator_unchanged_candidate_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"keeps the candidate when no new tx applies to it",
       fun test_keeps_candidate_on_no_update/0},
      {"keeps the candidate when it is already full",
       fun test_keeps_candidate_on_block_full/0},
      {"adds txs cached while an update that changed nothing was running",
       fun test_drains_cached_txs_after_unchanged_update/0},
      {"rebuilds after an update error that is not routine",
       fun test_rebuilds_after_non_routine_update_error/0},
      {"ignores an unchanged reply from a preempted worker",
       fun test_ignores_unchanged_reply_from_preempted_worker/0}]}.

block_generator_no_candidate_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"survives a new tx arriving with no candidate to extend",
       fun test_caches_tx_with_no_top_block/0}]}.

block_generator_failed_worker_backoff_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"spaces out rebuilds while creating a candidate keeps failing",
       fun test_backs_off_after_repeated_create_failures/0},
      {"rebuilds at once on a top change during backoff",
       fun test_resumes_immediately_on_top_change_during_backoff/0},
      {"goes back to full speed once a candidate is built",
       fun test_recovers_after_transient_failure/0},
      {"survives a new tx arriving while there is no candidate",
       fun test_survives_tx_while_no_candidate/0},
      {"clears the failures of the old run when generation is restarted",
       fun test_restart_after_stop_retries_immediately/0}]}.

setup() ->
    meck:new(aec_events, [non_strict]),
    meck:expect(aec_events, subscribe, fun(_) -> ok end),
    meck:expect(aec_events, publish, fun(_, _) -> ok end),

    meck:new(aec_chain, [non_strict]),
    meck:expect(aec_chain, top_block, fun() -> <<"top-0">> end),

    meck:new(aec_block_micro_candidate, [non_strict]),
    meck:new(aec_blocks, [non_strict]),
    meck:expect(aec_blocks, txs, fun(_) -> [dummy_tx] end),

    %% Unlinked: a generator crash should fail the test that provoked it, not kill the
    %% process eunit shares with every later test module
    {ok, Pid} = ?GENERATOR:start_link(),
    unlink(Pid),
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
      fun(Top) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              receive
                  {continue, ContinueRef} -> {error, simulated_failure}
              end;
         (Top) when Top =:= DeferredTop ->
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
      fun(Top) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              receive
                  {crash, CrashRef} -> exit(simulated_crash)
              end;
         (Top) when Top =:= DeferredTop ->
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
      fun(Top) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              {ok, Candidate0, State0};
         (Top) when Top =:= DeferredTop ->
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
      fun(Top) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              receive
                  {continue, ContinueRef} -> {ok, Candidate0, State0}
              end;
         (Top) when Top =:= DeferredTop ->
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

test_keeps_candidate_on_no_update() ->
    keeps_candidate_on(no_update_to_block_candidate).

test_keeps_candidate_on_block_full() ->
    keeps_candidate_on(block_is_full).

%% Both errors mean nothing applied, so the candidate must survive.
keeps_candidate_on(Reason) ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    Candidate0 = candidate_0,
    State0 = candidate_state_0,
    Tx1 = tx_1,
    Tx2 = tx_2,

    meck:expect(
      aec_events,
      publish,
      fun(candidate_block, new_candidate) ->
              TestPid ! candidate_published,
              ok;
         (_, _) ->
              ok
      end),
    expect_create_once(TestPid, InitialTop, Candidate0, State0),
    meck:expect(
      aec_block_micro_candidate,
      update,
      fun(Block, Txs, BlockInfo) ->
              TestPid ! {update_called, Block, Txs, BlockInfo, self()},
              {error, Reason}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create(InitialTop),
    wait_for_candidate(Candidate0),
    ?assertEqual(1, count_candidate_publishes()),

    send_tx(Tx1),
    wait_for_update(Candidate0, [Tx1], State0),

    assert_no_create(?SETTLE_MS),
    ?assertEqual(Candidate0, current_candidate()),
    ?assertEqual(0, count_candidate_publishes()),

    %% State0 again: the block_info survived, not just the block.
    send_tx(Tx2),
    wait_for_update(Candidate0, [Tx2], State0).

test_drains_cached_txs_after_unchanged_update() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    Candidate0 = candidate_0,
    State0 = candidate_state_0,
    Tx1 = tx_1,
    Tx2 = tx_2,
    Tx3 = tx_3,
    ContinueRef = make_ref(),

    expect_create_once(TestPid, InitialTop, Candidate0, State0),
    meck:expect(
      aec_block_micro_candidate,
      update,
      fun(Block, Txs, BlockInfo) when Txs =:= [Tx1] ->
              TestPid ! {update_called, Block, Txs, BlockInfo, self()},
              receive
                  {continue, ContinueRef} -> {error, block_is_full}
              end;
         (Block, Txs, BlockInfo) ->
              TestPid ! {update_called, Block, Txs, BlockInfo, self()},
              receive after infinity -> ok end
      end),

    ?GENERATOR:start_generation(),
    wait_for_create(InitialTop),
    wait_for_candidate(Candidate0),

    send_tx(Tx1),
    UpdateWorkerPid = wait_for_update(Candidate0, [Tx1], State0),
    send_tx(Tx2),   %% both cached while the first update worker is running
    send_tx(Tx3),
    UpdateWorkerPid ! {continue, ContinueRef},

    %% One update carrying both, newest first: only the cache can batch them.
    wait_for_update(Candidate0, [Tx3, Tx2], State0),
    assert_no_create(?SETTLE_MS).

%% Guards the new clause staying narrow: update/3's spec allows no other error,
%% so this rebuild is the defensive default rather than a reachable path.
test_rebuilds_after_non_routine_update_error() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    Candidate0 = candidate_0,
    State0 = candidate_state_0,
    Tx1 = tx_1,

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              {ok, Candidate0, State0}
      end),
    meck:expect(
      aec_block_micro_candidate,
      update,
      fun(Block, Txs, BlockInfo) ->
              TestPid ! {update_called, Block, Txs, BlockInfo, self()},
              {error, simulated_failure}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create(InitialTop),
    wait_for_candidate(Candidate0),

    send_tx(Tx1),
    wait_for_update(Candidate0, [Tx1], State0),

    %% Rebuilt despite the top not having changed.
    wait_for_create(InitialTop).

%% preempt_generation kills the update worker, but erlang:exit/2 is asynchronous,
%% so the worker can still get its reply out. That reply must not be mistaken for
%% the replacement worker's.
test_ignores_unchanged_reply_from_preempted_worker() ->
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
    CreateRef = make_ref(),

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top) when Top =:= InitialTop ->
              TestPid ! {create_called, InitialTop, self()},
              {ok, Candidate0, State0};
         (Top) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self()},
              receive
                  {continue, CreateRef} -> {ok, Candidate1, State1}
              end
      end),
    meck:expect(
      aec_block_micro_candidate,
      update,
      fun(Block, Txs, BlockInfo) when Block =:= Candidate0,
                                      Txs =:= [Tx1],
                                      BlockInfo =:= State0 ->
              %% Outlive the preempting kill, standing in for a reply that was
              %% already on its way when the exit signal was sent.
              process_flag(trap_exit, true),
              TestPid ! {update_called, Candidate0, [Tx1], State0, self()},
              receive
                  {continue, ContinueRef} -> {error, block_is_full}
              end;
         (Block, Txs, BlockInfo) ->
              TestPid ! {update_called, Block, Txs, BlockInfo, self()},
              {error, no_update_to_block_candidate}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create(InitialTop),
    wait_for_candidate(Candidate0),

    send_tx(Tx1),
    UpdateWorkerPid = wait_for_update(Candidate0, [Tx1], State0),

    defer_micro_top(DeferredTop),
    CreateWorkerPid = wait_for_create(DeferredTop),
    release_and_await_exit(UpdateWorkerPid, {continue, ContinueRef}),

    %% Had the stale reply been accepted it would have cleared the worker slot,
    %% leaving the create worker's own result to be dropped as stale in turn.
    CreateWorkerPid ! {continue, CreateRef},
    wait_for_candidate(Candidate1),

    send_tx(Tx2),
    wait_for_update(Candidate1, [Tx2], State1).

%% start_worker_txs/2 has no clause for an undefined candidate, and the gen_server
%% dying takes aec_conductor with it under one_for_all. With no top block no worker is
%% ever started, so this is the one state where a tx finds the slot idle and nothing
%% to extend.
test_caches_tx_with_no_top_block() ->
    meck:expect(aec_chain, top_block, fun() -> undefined end),
    GenPid = whereis(?GENERATOR),

    ?GENERATOR:start_generation(),
    [ send_tx({tx, N}) || N <- lists:seq(1, 5) ],

    %% A plain call, so a dead server fails here rather than through get_candidate/0,
    %% which answers no_candidate either way
    ?assertEqual(running, ?GENERATOR:get_generation_state()),
    ?assertEqual(GenPid, whereis(?GENERATOR)),
    ?assertEqual({error, no_candidate}, ?GENERATOR:get_candidate()),

    %% Nothing drains a cache built here, so no field may still be holding the txs
    Fields = tuple_to_list(sys:get_state(?GENERATOR)),
    ?assertEqual([], [ F || F <- Fields, is_list(F), F =/= [] ]).

%% A build that keeps failing used to respawn flat out, two debug log lines an iteration
test_backs_off_after_repeated_create_failures() ->
    Calls = counters:new(1, []),
    expect_failing_create(Calls),

    ?GENERATOR:start_generation(),
    timer:sleep(?BACKOFF_WINDOW_MS),

    %% Five while the delays double; one that stopped growing lands above it
    case counters:get(Calls, 1) of
        Made when Made >= 3, Made =< 8 -> ok;
        Made -> erlang:error({unexpected_create_count, Made})
    end.

test_resumes_immediately_on_top_change_during_backoff() ->
    TestPid = self(),
    DeferredTop = <<"top-1">>,
    Calls = counters:new(1, []),

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top) when Top =:= DeferredTop ->
              TestPid ! {create_called, DeferredTop, self()},
              {error, block_state_not_found};
         (_Top) ->
              counters:add(Calls, 1, 1),
              {error, block_state_not_found}
      end),

    ?GENERATOR:start_generation(),
    timer:sleep(?BACKOFF_WINDOW_MS),
    ?assert(counters:get(Calls, 1) >= 2),

    %% Not held back by the pending retry, and not counted against it either
    meck:expect(aec_chain, top_block, fun() -> DeferredTop end),
    defer_micro_top(DeferredTop),
    wait_for_create(DeferredTop),
    wait_for_create(DeferredTop, ?IMMEDIATE_RETRY_MS).

test_recovers_after_transient_failure() ->
    TestPid = self(),
    InitialTop = <<"top-0">>,
    Candidate0 = candidate_0,
    State0 = candidate_state_0,
    Tx1 = tx_1,
    Calls = counters:new(1, []),

    %% Fails long enough to be backing off, then builds
    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(_Top) ->
              N = counters:get(Calls, 1),
              counters:add(Calls, 1, 1),
              case N of
                  3 ->
                      {ok, Candidate0, State0};
                  _ when N < 3 ->
                      {error, block_state_not_found};
                  _ ->
                      TestPid ! {create_called, InitialTop, self()},
                      {error, block_state_not_found}
              end
      end),
    meck:expect(
      aec_block_micro_candidate,
      update,
      fun(_Block, _Txs, _BlockInfo) -> {error, simulated_failure} end),

    ?GENERATOR:start_generation(),
    wait_for_candidate(Candidate0),

    %% A build that got through clears the count, so this is retried at once
    %% rather than at the delay already reached
    send_tx(Tx1),
    wait_for_create(InitialTop, ?IMMEDIATE_RETRY_MS).

%% start_worker_txs/2 has no clause for an undefined candidate, and the gen_server
%% dying takes aec_conductor with it under one_for_all
test_survives_tx_while_no_candidate() ->
    Calls = counters:new(1, []),
    expect_failing_create(Calls),
    GenPid = whereis(?GENERATOR),

    ?GENERATOR:start_generation(),
    %% A failing build returns at once, so these all arrive with the slot idle
    [ begin send_tx({tx, N}), timer:sleep(25) end || N <- lists:seq(1, 12) ],

    ?assertEqual(GenPid, whereis(?GENERATOR)),
    ?assertEqual({error, no_candidate}, ?GENERATOR:get_candidate()),
    ?assert(counters:get(Calls, 1) >= 2).

%% Stopping ends a run of failures, so a restart is not held back by them
test_restart_after_stop_retries_immediately() ->
    TestPid = self(),
    Calls = counters:new(1, []),
    expect_failing_create(Calls),

    ?GENERATOR:start_generation(),
    timer:sleep(?BACKOFF_WINDOW_MS),
    ?assert(counters:get(Calls, 1) >= 2),
    ok = ?GENERATOR:stop_generation(),

    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(Top) ->
              TestPid ! {create_called, Top, self()},
              {error, block_state_not_found}
      end),

    ?GENERATOR:start_generation(),
    wait_for_create(<<"top-0">>),
    %% Taken at once, rather than at the delay the stopped run had reached
    wait_for_create(<<"top-0">>, ?IMMEDIATE_RETRY_MS).

%% Sends nothing, so an unbounded respawn loop fills the counter, not the mailbox
expect_failing_create(Calls) ->
    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(_Top) ->
              counters:add(Calls, 1, 1),
              {error, block_state_not_found}
      end).

wait_for_create(ExpectedTop) ->
    wait_for_create(ExpectedTop, ?WAIT_MS).

wait_for_create(ExpectedTop, TimeoutMs) ->
    receive
        {create_called, ExpectedTop, WorkerPid} ->
            WorkerPid
    after TimeoutMs ->
        erlang:error({timeout, no_create, ExpectedTop})
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

%% Answers Candidate once and a different one after that, so an unwanted rebuild
%% shows up in every later assertion, not only in assert_no_create/1.
expect_create_once(TestPid, Top, Candidate, State) ->
    Calls = counters:new(1, []),
    meck:expect(
      aec_block_micro_candidate,
      create,
      fun(T) when T =:= Top ->
              TestPid ! {create_called, T, self()},
              case counters:get(Calls, 1) of
                  0 ->
                      counters:add(Calls, 1, 1),
                      {ok, Candidate, State};
                  _ ->
                      {ok, rebuilt_candidate, rebuilt_state}
              end
      end).

%% A worker's reply is queued by the time it exits, so waiting for the exit puts
%% that reply ahead of whatever the test sends next.
release_and_await_exit(WorkerPid, Msg) ->
    Mon = erlang:monitor(process, WorkerPid),
    WorkerPid ! Msg,
    receive
        {'DOWN', Mon, process, WorkerPid, _Reason} -> ok
    after ?WAIT_MS ->
        erlang:error({timeout, worker_did_not_exit, WorkerPid})
    end.

assert_no_create(TimeoutMs) ->
    receive
        {create_called, Top, _WorkerPid} ->
            erlang:error({unexpected_rebuild, Top})
    after TimeoutMs ->
        ok
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

-endif.
