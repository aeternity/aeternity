-module(aec_dry_run_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(TEST_MODULE, aec_dry_run).

fast_fun_returns_unchanged_test() ->
    ?assertEqual({ok, 42}, ?TEST_MODULE:run_bounded(fun() -> {ok, 42} end, 1000)).

slow_fun_times_out_test() ->
    T0 = erlang:monotonic_time(millisecond),
    Res = ?TEST_MODULE:run_bounded(fun() -> timer:sleep(2000), ok end, 100),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    ?assertEqual({error, <<"dry-run exceeded time limit">>}, Res),
    %% returned close to the bound, well before the 2s sleep would finish
    ?assert(Elapsed >= 100),
    ?assert(Elapsed < 1000).

crashing_fun_returns_error_not_hang_test() ->
    Res = ?TEST_MODULE:run_bounded(fun() -> error(boom) end, 1000),
    ?assertMatch({error, <<"dry-run failed: ", _/binary>>}, Res).

%% On timeout the inner computation must actually be killed, not left running
%% orphaned to completion (which would bound latency but not CPU).
inner_killed_on_timeout_test() ->
    Parent = self(),
    Fun = fun() -> Parent ! {inner_started, self()}, timer:sleep(3000), done end,
    ?assertMatch({error, _}, ?TEST_MODULE:run_bounded(Fun, 100)),
    InnerPid = receive {inner_started, P} -> P after 1000 -> error(inner_never_started) end,
    ?assert(wait_until(fun() -> not is_process_alive(InnerPid) end, 1500)).

wait_until(_Pred, Left) when Left =< 0 -> false;
wait_until(Pred, Left) ->
    case Pred() of
        true  -> true;
        false -> timer:sleep(20), wait_until(Pred, Left - 20)
    end.

no_leftover_messages_or_workers_test() ->
    ProcsBefore = erlang:system_info(process_count),
    _ = ?TEST_MODULE:run_bounded(fun() -> timer:sleep(2000), ok end, 50),
    _ = ?TEST_MODULE:run_bounded(fun() -> ok end, 1000),
    _ = ?TEST_MODULE:run_bounded(fun() -> error(boom) end, 1000),
    %% give the killed/exited worker tree a moment to be fully reaped
    timer:sleep(200),
    ?assertEqual({messages, []}, erlang:process_info(self(), messages)),
    ProcsAfter = erlang:system_info(process_count),
    %% no lingering worker/inner processes from any of the three calls above
    ?assert(ProcsAfter =< ProcsBefore + 1).

%% Only public/replay are time-bounded; internal is unbounded; explicit timeout
%% wins; the dry-run-only opts never leak into the tx-application opts.
timeout_resolution_test() ->
    application:unset_env(aehttp, dry_run),
    ?assertEqual({777, [tx_events]},
                 ?TEST_MODULE:resolve_timeout([tx_events, {dry_run_profile, public}, {timeout_ms, 777}])),
    ?assertEqual({10000, [tx_events]},
                 ?TEST_MODULE:resolve_timeout([tx_events, {dry_run_profile, replay}])),
    {PMs, [tx_events]} = ?TEST_MODULE:resolve_timeout([tx_events, {dry_run_profile, public}]),
    ?assert(is_integer(PMs) andalso PMs < 10000),
    ?assertEqual({infinity, [tx_events]}, ?TEST_MODULE:resolve_timeout([tx_events])).

%%%===================================================================
%%% Public-API wiring: exercise the real dry_run/3,4, not just run_bounded/2.
%%% Mocked one level down, at aetx_env:tx_env_and_trees_from_hash/2 - meck
%%% can't intercept dry_run/4's local call to dry_run_unbounded/4.
%%%===================================================================

wiring_setup() ->
    meck:new(aetx_env, [passthrough]),
    ok.

wiring_teardown(_) ->
    meck:unload(aetx_env),
    application:unset_env(aehttp, dry_run),
    ok.

%% Stand-in {Env, Trees}, same shape aetx_env would hand back; no db needed.
fake_env_and_trees() ->
    Header = aec_headers:raw_key_header(),
    Trees = aec_trees:new_without_backend(),
    Env = aetx_env:tx_env_from_key_header(Header, <<0:32/unit:8>>, 0, <<0:32/unit:8>>),
    {aetx_env:set_context(Env, aetx_transaction), Trees}.

dry_run_wiring_test_() ->
    {setup, fun wiring_setup/0, fun wiring_teardown/1,
     fun() ->
        {EnvT, TreesT} = fake_env_and_trees(),

        %% bound (200ms) < slow inner (2000ms): must time out, not wait.
        meck:expect(aetx_env, tx_env_and_trees_from_hash,
                    fun(_Type, _Hash) -> timer:sleep(2000), {EnvT, TreesT} end),
        application:set_env(aehttp, dry_run, [{timeout_ms, 200}]),
        {TA, ResA} = timer:tc(fun() -> ?TEST_MODULE:dry_run(<<0:32/unit:8>>, [], [], [{dry_run_profile, public}]) end),
        ElapsedA = TA div 1000,
        ?debugFmt("(a) bound=200ms slow_inner=2000ms -> result=~p elapsed=~pms", [ResA, ElapsedA]),

        %% bound (5000ms) > fast inner (50ms): result passes through unchanged.
        meck:expect(aetx_env, tx_env_and_trees_from_hash,
                    fun(_Type, _Hash) -> timer:sleep(50), {EnvT, TreesT} end),
        application:set_env(aehttp, dry_run, [{timeout_ms, 5000}]),
        {TB, ResB} = timer:tc(fun() -> ?TEST_MODULE:dry_run(<<0:32/unit:8>>, [], [], [{dry_run_profile, public}]) end),
        ElapsedB = TB div 1000,
        ?debugFmt("(b) bound=5000ms fast_inner=50ms -> result=~p elapsed=~pms", [ResB, ElapsedB]),

        [ ?_assertEqual({error, <<"dry-run exceeded time limit">>}, ResA)
        , ?_assert(ElapsedA >= 200)
        , ?_assert(ElapsedA < 2000)  %% proves it did NOT wait for the 2s inner
        , ?_assertEqual({ok, {[], []}}, ResB)
        , ?_assert(ElapsedB < 5000)
        ]
     end}.

%%%===================================================================
%%% Always-on Salus dry-run gas metering (the env-forcing mechanism).
%%%
%%% Dry-run meters FATE store reads at the repriced Salus (v8) cost by
%%% default, WITHOUT activating the fork and WITHOUT any response/SDK
%%% change: only the dry-run env's metering protocol is stepped up, and
%%% only from a Ceres-or-later base so nothing but the store repricing
%%% (the sole >= Salus production gate set) is applied.
%%%===================================================================

ceres_dry_run_env() ->
    aetx_env:set_dry_run(aetx_env:tx_env(1, ?CERES_PROTOCOL_VSN), true).

%% Default (no config): a Ceres-tip dry-run is metered at Salus.
salus_metering_default_on_forces_ceres_to_salus_test() ->
    application:unset_env(aehttp, dry_run),
    ?assert(?TEST_MODULE:salus_gas_metering_enabled()),
    Env  = ceres_dry_run_env(),
    Env1 = ?TEST_MODULE:maybe_force_salus_metering(Env),
    ?assertEqual(?SALUS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)),
    %% only the metering protocol moved; it is still a dry-run env
    ?assert(aetx_env:dry_run(Env1)).

%% Operator escape hatch: disabled -> the tip's own (Ceres) metering is kept.
salus_metering_disabled_leaves_env_untouched_test() ->
    application:set_env(aehttp, dry_run, [{salus_gas_metering, false}]),
    try
        ?assertNot(?TEST_MODULE:salus_gas_metering_enabled()),
        Env  = ceres_dry_run_env(),
        Env1 = ?TEST_MODULE:maybe_force_salus_metering(Env),
        ?assertEqual(?CERES_PROTOCOL_VSN, aetx_env:consensus_version(Env1))
    after
        application:unset_env(aehttp, dry_run)
    end.

%% Historical (pre-Ceres) replay is NOT bumped -- forcing Salus there would
%% cross the >= Ceres gates (incl. the contract-vs-name tx-validity gate),
%% changing more than gas. Keep replay on its exact protocol.
salus_metering_not_applied_below_ceres_test() ->
    application:unset_env(aehttp, dry_run),
    Env  = aetx_env:set_dry_run(aetx_env:tx_env(1, ?IRIS_PROTOCOL_VSN), true),
    Env1 = ?TEST_MODULE:maybe_force_salus_metering(Env),
    ?assertEqual(?IRIS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)).

%% Never downgrades: at/above Salus the env is left as-is.
salus_metering_idempotent_at_salus_test() ->
    application:unset_env(aehttp, dry_run),
    Env  = aetx_env:set_dry_run(aetx_env:tx_env(1, ?SALUS_PROTOCOL_VSN), true),
    Env1 = ?TEST_MODULE:maybe_force_salus_metering(Env),
    ?assertEqual(?SALUS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)).

%% Tx-validity is invariant across the bump: a v6-built contract-call tx is
%% equally valid at Ceres and at the forced Salus (both use the >= Ceres
%% clause), so a <= v6 SDK's tx still dry-runs cleanly -- only gas changes.
v6_call_tx_valid_at_ceres_and_forced_salus_test() ->
    {ok, Aetx} = aect_call_tx:new(#{ caller_id   => aeser_id:create(account, <<1:256>>)
                                   , nonce       => 1
                                   , contract_id => aeser_id:create(contract, <<2:256>>)
                                   , abi_version => ?ABI_FATE_SOPHIA_1
                                   , fee         => 1000000
                                   , amount      => 0
                                   , gas         => 1000
                                   , gas_price   => 1000000
                                   , call_data   => <<>> }),
    {aect_call_tx, CTx} = aetx:specialize_callback(Aetx),
    ?assertEqual(aect_call_tx:valid_at_protocol(?CERES_PROTOCOL_VSN, CTx),
                 aect_call_tx:valid_at_protocol(?SALUS_PROTOCOL_VSN, CTx)),
    ?assert(aect_call_tx:valid_at_protocol(?SALUS_PROTOCOL_VSN, CTx)).
