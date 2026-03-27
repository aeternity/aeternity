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

wait_for_create(ExpectedTop) ->
    receive
        {create_called, ExpectedTop, WorkerPid} ->
            WorkerPid
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

-endif.
