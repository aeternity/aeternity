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

%% If the calling process dies mid-run (client disconnect), the worker must
%% not leave the inner computation running orphaned.
caller_death_kills_inner_worker_test() ->
    Parent = self(),
    CallerPid = spawn(fun() ->
        ?TEST_MODULE:run_bounded(
          fun() -> Parent ! {inner_started, self()}, timer:sleep(5000), done end, 10000)
    end),
    InnerPid = receive {inner_started, P} -> P after 1000 -> error(inner_never_started) end,
    exit(CallerPid, kill),
    ?assert(wait_until(fun() -> not is_process_alive(InnerPid) end, 1500)).

%% A single non-yielding allocation past max_heap_words is killed rather than
%% left to grow unbounded; the caller sees an error, not a hang or OOM.
max_heap_size_kill_returns_error_not_hang_test() ->
    application:set_env(aehttp, dry_run, [{max_heap_words, 1000}]),
    try
        Fun = fun() -> _ = lists:seq(1, 10000000), ok end,
        ?assertMatch({error, _}, ?TEST_MODULE:run_bounded(Fun, 5000))
    after
        application:unset_env(aehttp, dry_run)
    end.

wait_until(_Pred, Left) when Left =< 0 -> false;
wait_until(Pred, Left) ->
    case Pred() of
        true  -> true;
        false -> timer:sleep(20), wait_until(Pred, Left - 20)
    end.

%% Tracks the actual worker/inner pids from each run_bounded/2 call rather
%% than the VM-wide process_count, which any unrelated process elsewhere in
%% the (possibly shared) VM can bump during this ~2.3s window and make flaky.
no_leftover_messages_or_workers_test() ->
    Parent = self(),
    Report = fun() -> Parent ! {inner_pid, self()} end,
    _ = ?TEST_MODULE:run_bounded(fun() -> Report(), timer:sleep(2000), ok end, 50),
    Pid1 = receive {inner_pid, P1} -> P1 after 1000 -> error(inner_never_started) end,
    _ = ?TEST_MODULE:run_bounded(fun() -> Report(), ok end, 1000),
    Pid2 = receive {inner_pid, P2} -> P2 after 1000 -> error(inner_never_started) end,
    _ = ?TEST_MODULE:run_bounded(fun() -> Report(), error(boom) end, 1000),
    Pid3 = receive {inner_pid, P3} -> P3 after 1000 -> error(inner_never_started) end,
    ?assertEqual({messages, []}, erlang:process_info(self(), messages)),
    %% no lingering worker/inner processes from any of the three calls above
    ?assert(wait_until(fun() -> lists:all(fun(P) -> not is_process_alive(P) end,
                                          [Pid1, Pid2, Pid3])
                        end, 1500)).

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
     fun(_) ->
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
%%% Always-on Arcus dry-run gas metering (the env-forcing mechanism).
%%%
%%% Dry-run meters FATE store reads at the repriced Arcus (v7) cost by
%%% default, WITHOUT activating the fork and WITHOUT any response/SDK
%%% change: only the dry-run env's metering protocol is stepped up, and
%%% only from a Ceres-or-later base so nothing but the store-read repricing
%%% (Arcus's only production behaviour) is applied. Estimate profiles only;
%%% replay stays historical.
%%%===================================================================

ceres_dry_run_env() ->
    aetx_env:set_dry_run(aetx_env:tx_env(1, ?CERES_PROTOCOL_VSN), true).

%% Estimate profiles force; real-protocol profiles (replay, includability), a
%% missing profile, and unknown profiles do not (fail safe, not open).
force_arcus_for_profile_test() ->
    ?assert(?TEST_MODULE:force_arcus_for_profile([{dry_run_profile, public}])),
    ?assert(?TEST_MODULE:force_arcus_for_profile([{dry_run_profile, internal}])),
    ?assertNot(?TEST_MODULE:force_arcus_for_profile([])),  %% missing profile: pinned
    ?assertNot(?TEST_MODULE:force_arcus_for_profile([{dry_run_profile, replay}])),
    ?assertNot(?TEST_MODULE:force_arcus_for_profile([{dry_run_profile, includability}])),
    ?assertNot(?TEST_MODULE:force_arcus_for_profile([{dry_run_profile, whatever}])).

%% Default (no config), estimate profile: a Ceres-tip dry-run is metered at Arcus.
arcus_metering_default_on_forces_ceres_to_arcus_test() ->
    application:unset_env(aehttp, dry_run),
    ?assert(?TEST_MODULE:store_read_gas_metering_enabled()),
    Env  = ceres_dry_run_env(),
    Env1 = ?TEST_MODULE:maybe_force_arcus_metering(Env, true),
    ?assertEqual(?ARCUS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)),
    %% only the metering protocol moved; it is still a dry-run env
    ?assert(aetx_env:dry_run(Env1)).

%% Replay profile (ForceArcus=false): a Ceres block stays pinned to Ceres, even
%% with metering enabled -- historical re-execution must re-meter at real cost.
arcus_metering_replay_stays_at_ceres_test() ->
    application:unset_env(aehttp, dry_run),
    Env  = ceres_dry_run_env(),
    Env1 = ?TEST_MODULE:maybe_force_arcus_metering(Env, false),
    ?assertEqual(?CERES_PROTOCOL_VSN, aetx_env:consensus_version(Env1)).

%% Operator escape hatch: disabled -> the tip's own (Ceres) metering is kept.
arcus_metering_disabled_leaves_env_untouched_test() ->
    application:set_env(aehttp, dry_run, [{store_read_gas_metering, false}]),
    try
        ?assertNot(?TEST_MODULE:store_read_gas_metering_enabled()),
        Env  = ceres_dry_run_env(),
        Env1 = ?TEST_MODULE:maybe_force_arcus_metering(Env, true),
        ?assertEqual(?CERES_PROTOCOL_VSN, aetx_env:consensus_version(Env1))
    after
        application:unset_env(aehttp, dry_run)
    end.

%% The escape hatch only governs the forward step-up for pre-Arcus heights. Once
%% Arcus is the height's real protocol the repricing is the activated cost, so
%% disabling the switch must not buy an opt-out back to the flat charge.
arcus_metering_cannot_be_disabled_once_active_test() ->
    application:set_env(aehttp, dry_run, [{store_read_gas_metering, false}]),
    try
        ?assertNot(?TEST_MODULE:store_read_gas_metering_enabled()),
        Env  = aetx_env:set_dry_run(aetx_env:tx_env(1, ?ARCUS_PROTOCOL_VSN), true),
        Env1 = ?TEST_MODULE:maybe_force_arcus_metering(Env, true),
        ?assertEqual(?ARCUS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)),
        %% ... and the replay profile, which never forces, lands there too.
        Env2 = ?TEST_MODULE:maybe_force_arcus_metering(Env, false),
        ?assertEqual(?ARCUS_PROTOCOL_VSN, aetx_env:consensus_version(Env2))
    after
        application:unset_env(aehttp, dry_run)
    end.

%% Historical (pre-Ceres) replay is NOT bumped -- forcing Arcus there would
%% cross the >= Ceres gates (incl. the contract-vs-name tx-validity gate),
%% changing more than gas. Keep replay on its exact protocol.
arcus_metering_not_applied_below_ceres_test() ->
    application:unset_env(aehttp, dry_run),
    Env  = aetx_env:set_dry_run(aetx_env:tx_env(1, ?IRIS_PROTOCOL_VSN), true),
    Env1 = ?TEST_MODULE:maybe_force_arcus_metering(Env, true),
    ?assertEqual(?IRIS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)).

%% Never downgrades: at/above Arcus the env is left as-is.
arcus_metering_idempotent_at_arcus_test() ->
    application:unset_env(aehttp, dry_run),
    Env  = aetx_env:set_dry_run(aetx_env:tx_env(1, ?ARCUS_PROTOCOL_VSN), true),
    Env1 = ?TEST_MODULE:maybe_force_arcus_metering(Env, true),
    ?assertEqual(?ARCUS_PROTOCOL_VSN, aetx_env:consensus_version(Env1)).

%% Tx-validity is invariant across the bump: a v6-built contract-call tx is
%% equally valid at Ceres and at the forced Arcus (both use the >= Ceres
%% clause), so a <= v6 SDK's tx still dry-runs cleanly -- only gas changes.
v6_call_tx_valid_at_ceres_and_forced_arcus_test() ->
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
                 aect_call_tx:valid_at_protocol(?ARCUS_PROTOCOL_VSN, CTx)),
    ?assert(aect_call_tx:valid_at_protocol(?ARCUS_PROTOCOL_VSN, CTx)).

%%%===================================================================
%%% Equivalence tripwire: forcing Arcus in dry-run changes gas ONLY.
%%%
%%% "Gas amounts only" holds today only because Arcus/Salus add no non-
%%% repricing behaviour; nothing enforces that. This runs the SAME tx through
%%% the real dry_run/3 apply path at natural Ceres (metering off) and forced
%%% Arcus (metering on) and asserts the result is identical *when adequately
%%% gassed*:
%%%  - spend + store-free call: byte-identical result AND identical gas_used
%%%    (no store reads -> zero repricing delta);
%%%  - store reads at all three gate sites (register read, map lookup, map
%%%    member): identical return value/validity, gas_used strictly higher.
%%% It also proves, end-to-end through dry_run/3, that (a) the repricing turns
%%% a Ceres-adequate call into out_of_gas under a tight budget (the public
%%% endpoint caps at ?DEFAULT_GAS_LIMIT = 6M), and (b) the replay profile is
%%% NOT re-metered -- it keeps the block's real Ceres cost. Red-fails if any
%%% future >= Arcus/Salus gate leaks non-gas behaviour into the forcing.
%%%===================================================================

-define(DUMMY_HASH, <<0:32/unit:8>>).

arcus_dry_run_equivalence_test_() ->
    %% Deploys a Ceres FATE contract, so it only runs on Ceres+ eunit lanes;
    %% skip on pre-Ceres protocols (minerva/fortuna/lima/iris) where the
    %% contract compiler version is illegal.
    case aect_test_utils:latest_protocol_version() >= ?CERES_PROTOCOL_VSN of
        false -> [];
        true  -> {timeout, 120, {setup, fun equiv_setup/0, fun equiv_teardown/1, fun equiv_checks/1}}
    end.

equiv_setup() ->
    Vsn = aect_test_utils:sophia_version(fate, ?CERES_PROTOCOL_VSN),
    S0  = aect_test_utils:new_state(),
    {Owner, S1} = aect_test_utils:setup_new_account(
                    1000000000000000 * aec_test_utils:min_gas_price(), S0),
    Env = ceres_deploy_env(),
    BigStr = "\"" ++ lists:duplicate(300, $a) ++ "\"",  %% ~300B store value
    {IdPK, S2} = deploy(Vsn, identity,        [],        Owner, Env, S1),
    {StPK, S3} = deploy(Vsn, storage_tester,  [],        Owner, Env, S2),
    {MpPK, S4} = deploy(Vsn, arcus_map_probe, [BigStr],  Owner, Env, S3),
    Trees = aect_test_utils:trees(S4),
    {ok, IdData}   = aect_test_utils:encode_call_data(Vsn, src(Vsn, identity), "main_", ["42"]),
    {ok, HashData} = aect_test_utils:encode_call_data(Vsn, src(Vsn, storage_tester), "getHash", []),
    {ok, LookData} = aect_test_utils:encode_call_data(Vsn, src(Vsn, arcus_map_probe), "lookup", ["3"]),
    {ok, MemData}  = aect_test_utils:encode_call_data(Vsn, src(Vsn, arcus_map_probe), "member", ["3"]),
    {ok, Spend} = aec_spend_tx:new(#{ sender_id    => aeser_id:create(account, Owner)
                                    , recipient_id => aeser_id:create(account, <<9:256>>)
                                    , amount       => 1
                                    , fee          => 20000 * aec_test_utils:min_gas_price()
                                    , nonce        => aect_test_utils:next_nonce(Owner, S4)
                                    , ttl          => 0
                                    , payload      => <<>> }),
    meck:new(aetx_env, [passthrough]),
    meck:expect(aetx_env, tx_env_and_trees_from_hash, fun(_, _) -> {Env, Trees} end),
    #{ id_call   => mk_call(Owner, IdPK, IdData,   100000,  S4)
     , hash_call => mk_call(Owner, StPK, HashData, 5000000, S4)
     , look_call => mk_call(Owner, MpPK, LookData, 1000000, S4)
     , mem_call  => mk_call(Owner, MpPK, MemData,  1000000, S4)
     , spend     => Spend
     , owner => Owner, st_pk => StPK, hash_data => HashData, state => S4 }.

equiv_teardown(_) ->
    meck:unload(aetx_env),
    application:unset_env(aehttp, dry_run),
    ok.

equiv_checks(F) ->
    #{ id_call := IdCall, hash_call := HashCall, look_call := LookCall
     , mem_call := MemCall, spend := Spend
     , owner := Owner, st_pk := StPK, hash_data := HashData, state := St } = F,
    %% (1) store-free call + spend: identical result and gas.
    {CIdR, CIdG} = dry_call(IdCall, false),
    {SIdR, SIdG} = dry_call(IdCall, true),
    CSpend = dry_raw(Spend, false),
    SSpend = dry_raw(Spend, true),
    %% (2) store reads at all three gate sites.
    {CHR, CHG} = dry_call(HashCall, false),  {SHR, SHG} = dry_call(HashCall, true),
    {CLR, CLG} = dry_call(LookCall, false),  {SLR, SLG} = dry_call(LookCall, true),
    {CMR, CMG} = dry_call(MemCall,  false),  {SMR, SMG} = dry_call(MemCall,  true),
    %% (3) replay profile is NOT re-metered: metering on, replay -> Ceres cost.
    {_, RHG} = dry_call(HashCall, true, [{dry_run_profile, replay}]),
    %% (4) end-to-end DoS: a budget between the Ceres and Arcus cost makes the
    %%     SAME call succeed at Ceres but abort out_of_gas under forced Arcus.
    Budget = CHG + max(1, (SHG - CHG) div 2),
    Tight  = mk_call(Owner, StPK, HashData, Budget, St),
    {{CTightT, _}, _}       = dry_call(Tight, false),
    {{STightT, STightV}, _} = dry_call(Tight, true),
    ?debugFmt("~nequivalence tripwire (natural Ceres vs forced Arcus):~n"
              "  store-free  main_(42): eq=~p gas ceres=~p arcus=~p~n"
              "  register    getHash(): eq=~p gas ceres=~p arcus=~p (delta=~p)~n"
              "  map-lookup  lookup(3): eq=~p gas ceres=~p arcus=~p~n"
              "  map-member  member(3): eq=~p gas ceres=~p arcus=~p~n"
              "  spend:                 eq=~p~n"
              "  replay getHash gas=~p (ceres=~p arcus=~p)~n"
              "  DoS budget=~p: ceres=~p arcus=~p/~p~n",
              [CIdR =:= SIdR, CIdG, SIdG,
               CHR =:= SHR, CHG, SHG, SHG - CHG,
               CLR =:= SLR, CLG, SLG,
               CMR =:= SMR, CMG, SMG,
               CSpend =:= SSpend,
               RHG, CHG, SHG,
               Budget, CTightT, STightT, STightV]),
    [ %% (1) no store reads -> identical result AND gas
      ?_assertEqual(CIdR, SIdR), ?_assertEqual(CIdG, SIdG)
    , ?_assertEqual(CSpend, SSpend)
      %% (2) each store-read gate -> identical result, strictly higher gas
    , ?_assertEqual(CHR, SHR), ?_assert(SHG > CHG)
    , ?_assertEqual(CLR, SLR), ?_assert(SLG > CLG)
    , ?_assertEqual(CMR, SMR), ?_assert(SMG > CMG)
      %% (3) replay stays at the block's real Ceres cost (not re-metered)
    , ?_assertEqual(CHG, RHG), ?_assert(RHG < SHG)
      %% (4) end-to-end: ok at Ceres, out_of_gas under forced Arcus
    , ?_assertEqual(ok, CTightT)
    , ?_assertEqual(error, STightT)
    , ?_assert(binary:match(STightV, <<"gas">>) =/= nomatch)
    ].

ceres_deploy_env() ->
    H0 = aec_headers:raw_key_header(),
    H  = aec_headers:set_version_and_height(H0, ?CERES_PROTOCOL_VSN, 100),
    Env0 = aetx_env:tx_env_from_key_header(H, ?DUMMY_HASH, 0, ?DUMMY_HASH),
    aetx_env:set_context(Env0, aetx_transaction).

src(Vsn, Name) ->
    {ok, S} = aect_test_utils:read_contract(Vsn, Name),
    S.

deploy(Vsn, Name, InitArgs, Owner, Env, S) ->
    {ok, Code}     = aect_test_utils:compile_contract(Vsn, Name),
    {ok, InitData} = aect_test_utils:encode_call_data(Vsn, src(Vsn, Name), "init", InitArgs),
    Nonce = aect_test_utils:next_nonce(Owner, S),
    CreateTx = aect_test_utils:create_tx(Owner,
                 #{ code => Code, call_data => InitData
                  , vm_version => ?VM_FATE_SOPHIA_3, abi_version => ?ABI_FATE_SOPHIA_1
                  , gas => 5000000, amount => 0, deposit => 0, nonce => Nonce }, S),
    PK = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    {ok, [_], [], Trees1, _} =
        aec_trees:apply_txs_on_state_trees([dummy_sign(CreateTx)], aect_test_utils:trees(S),
                                           Env, [strict, dont_verify_signature]),
    {PK, aect_test_utils:set_trees(Trees1, S)}.

mk_call(Owner, PK, Data, Gas, State) ->
    aect_test_utils:call_tx(Owner, PK,
        #{ call_data => Data, gas => Gas, amount => 0
         , nonce => aect_test_utils:next_nonce(Owner, State) }, State).

dry_call(Tx, Metering) ->
    %% Simulates an internal/estimate caller; a caller that specifies no
    %% profile at all now gets the safe (not forward-metered) default, so
    %% this helper opts in explicitly to keep exercising the forced path.
    dry_call(Tx, Metering, [{dry_run_profile, internal}]).

dry_call(Tx, Metering, Extra) ->
    application:set_env(aehttp, dry_run, [{store_read_gas_metering, Metering}]),
    {ok, {[{contract_call_tx, {ok, CallObj}}], _}} =
        aec_dry_run:dry_run(?DUMMY_HASH, [], [{tx, Tx}], Extra),
    {{aect_call:return_type(CallObj), aect_call:return_value(CallObj)},
     aect_call:gas_used(CallObj)}.

dry_raw(Tx, Metering) ->
    application:set_env(aehttp, dry_run, [{store_read_gas_metering, Metering}]),
    %% Same reason as dry_call/2: opt in explicitly to the forced-metering path.
    aec_dry_run:dry_run(?DUMMY_HASH, [], [{tx, Tx}], [{dry_run_profile, internal}]).

dummy_sign(Tx) ->
    aetx_sign:new(Tx, [<<0:64/unit:8>>]).
