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

%%%===================================================================
%%% Equivalence tripwire: forcing Salus in dry-run changes gas ONLY.
%%%
%%% "Gas amounts only" holds today because Salus/Arcus add no non-repricing
%%% behavior; nothing enforces that. This runs the SAME tx through the real
%%% dry_run/3 apply path at natural Ceres (metering off) and forced Salus
%%% (metering on) and asserts the RESULT is identical:
%%%  - spend + store-free call: byte-identical result AND identical gas_used
%%%    (no store reads -> zero repricing delta);
%%%  - store-heavy call: identical return value/validity, gas_used strictly
%%%    higher (the store-read repricing, and only that).
%%% Red-fails if any future >= Salus/Arcus gate leaks non-gas behavior into
%%% the always-on forcing.
%%%===================================================================

-define(DUMMY_HASH, <<0:32/unit:8>>).

salus_dry_run_equivalence_test_() ->
    {timeout, 120, {setup, fun equiv_setup/0, fun equiv_teardown/1, fun equiv_checks/1}}.

equiv_setup() ->
    Vsn = aect_test_utils:sophia_version(fate, ?CERES_PROTOCOL_VSN),
    S0  = aect_test_utils:new_state(),
    {Owner, S1} = aect_test_utils:setup_new_account(
                    1000000000000000 * aec_test_utils:min_gas_price(), S0),
    Env = ceres_deploy_env(),
    {IdPK, S2} = deploy(Vsn, identity,       Owner, Env, S1),
    {StPK, S3} = deploy(Vsn, storage_tester, Owner, Env, S2),
    Trees = aect_test_utils:trees(S3),
    Nonce = aect_test_utils:next_nonce(Owner, S3),
    {ok, IdData}   = aect_test_utils:encode_call_data(Vsn, src(Vsn, identity), "main_", ["42"]),
    {ok, HashData} = aect_test_utils:encode_call_data(Vsn, src(Vsn, storage_tester), "getHash", []),
    IdCall = aect_test_utils:call_tx(Owner, IdPK,
                #{call_data => IdData, gas => 100000, nonce => Nonce, amount => 0}, S3),
    HashCall = aect_test_utils:call_tx(Owner, StPK,
                #{call_data => HashData, gas => 5000000, nonce => Nonce, amount => 0}, S3),
    {ok, Spend} = aec_spend_tx:new(#{ sender_id    => aeser_id:create(account, Owner)
                                    , recipient_id => aeser_id:create(account, <<9:256>>)
                                    , amount       => 1
                                    , fee          => 20000 * aec_test_utils:min_gas_price()
                                    , nonce        => Nonce
                                    , ttl          => 0
                                    , payload      => <<>> }),
    meck:new(aetx_env, [passthrough]),
    meck:expect(aetx_env, tx_env_and_trees_from_hash, fun(_, _) -> {Env, Trees} end),
    #{id_call => IdCall, hash_call => HashCall, spend => Spend}.

equiv_teardown(_) ->
    meck:unload(aetx_env),
    application:unset_env(aehttp, dry_run),
    ok.

equiv_checks(#{id_call := IdCall, hash_call := HashCall, spend := Spend}) ->
    {CIdR, CIdG} = dry_call(IdCall, false),
    {SIdR, SIdG} = dry_call(IdCall, true),
    {CHR, CHG}   = dry_call(HashCall, false),
    {SHR, SHG}   = dry_call(HashCall, true),
    CSpend = dry_raw(Spend, false),
    SSpend = dry_raw(Spend, true),
    ?debugFmt("~nequivalence tripwire (natural Ceres vs forced Salus):~n"
              "  store-free  main_(42): result_eq=~p, gas ceres=~p salus=~p~n"
              "  store-heavy getHash(): result_eq=~p, gas ceres=~p salus=~p (delta=~p)~n"
              "  spend:                 result_eq=~p~n",
              [CIdR =:= SIdR, CIdG, SIdG,
               CHR =:= SHR, CHG, SHG, SHG - CHG,
               CSpend =:= SSpend]),
    [ ?_assertEqual(CIdR, SIdR)      %% store-free: identical result
    , ?_assertEqual(CIdG, SIdG)      %% store-free: identical gas (zero repricing delta)
    , ?_assertEqual(CHR, SHR)        %% store-heavy: identical return value / validity
    , ?_assert(SHG > CHG)            %% store-heavy: gas strictly higher under Salus
    , ?_assertEqual(CSpend, SSpend)  %% spend: byte-identical result
    ].

ceres_deploy_env() ->
    H0 = aec_headers:raw_key_header(),
    H  = aec_headers:set_version_and_height(H0, ?CERES_PROTOCOL_VSN, 100),
    Env0 = aetx_env:tx_env_from_key_header(H, ?DUMMY_HASH, 0, ?DUMMY_HASH),
    aetx_env:set_context(Env0, aetx_transaction).

src(Vsn, Name) ->
    {ok, S} = aect_test_utils:read_contract(Vsn, Name),
    S.

deploy(Vsn, Name, Owner, Env, S) ->
    {ok, Code}     = aect_test_utils:compile_contract(Vsn, Name),
    {ok, InitData} = aect_test_utils:encode_call_data(Vsn, src(Vsn, Name), "init", []),
    Nonce = aect_test_utils:next_nonce(Owner, S),
    CreateTx = aect_test_utils:create_tx(Owner,
                 #{ code => Code, call_data => InitData
                  , vm_version => ?VM_FATE_SOPHIA_3, abi_version => ?ABI_FATE_SOPHIA_1
                  , gas => 1000000, amount => 0, deposit => 0, nonce => Nonce }, S),
    PK = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    {ok, [_], [], Trees1, _} =
        aec_trees:apply_txs_on_state_trees([dummy_sign(CreateTx)], aect_test_utils:trees(S),
                                           Env, [strict, dont_verify_signature]),
    {PK, aect_test_utils:set_trees(Trees1, S)}.

dry_call(Tx, Metering) ->
    application:set_env(aehttp, dry_run, [{salus_gas_metering, Metering}]),
    {ok, {[{contract_call_tx, {ok, CallObj}}], _}} =
        aec_dry_run:dry_run(?DUMMY_HASH, [], [{tx, Tx}]),
    {{aect_call:return_type(CallObj), aect_call:return_value(CallObj)},
     aect_call:gas_used(CallObj)}.

dry_raw(Tx, Metering) ->
    application:set_env(aehttp, dry_run, [{salus_gas_metering, Metering}]),
    aec_dry_run:dry_run(?DUMMY_HASH, [], [{tx, Tx}]).

dummy_sign(Tx) ->
    aetx_sign:new(Tx, [<<0:64/unit:8>>]).
