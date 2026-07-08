%%% End-to-end proof that dry_run/4 bounds a genuinely heavy contract call,
%%% not just an injected timer:sleep/1. Slow (~20s) and memory-heavy (runs the
%%% baseline with the heap guard off), so it is opt-in: set AE_HEAVY_DRY_RUN=1.
-module(aec_dry_run_heavy_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_dry_run).
%% storage_tester needs FATE (@compiler >= 4.3); Iris is protocol 5.
-define(MIN_PROTOCOL, 5).

%% Calibrated so the call takes several real seconds (measured ~6.5s),
%% comfortably more than the 1.5s default bound.
-define(DIMS, 60).

dummy_sign(Tx) ->
    aetx_sign:new(Tx, [<<0:64/unit:8>>]).

%% Deploys storage_tester into in-memory trees, builds the slow-entrypoint
%% call tx. Returns {Env, TreesAfterDeploy, CallTx}.
build_fixture() ->
    SophiaVsn = aect_test_utils:sophia_version(),
    Proto = aect_test_utils:latest_protocol_version(),
    VmV = aect_test_utils:vm_version(),
    AbiV = aect_test_utils:abi_version(),

    {ok, Code} = aect_test_utils:compile_contract(SophiaVsn, storage_tester),
    {ok, BinSrc} = aect_test_utils:read_contract(SophiaVsn, storage_tester),

    S0 = aect_test_utils:new_state(),
    {Owner, S1} = aect_test_utils:setup_new_account(
                    1000000000000000 * aec_test_utils:min_gas_price(), S0),

    %% Forced to latest protocol so FATE/ABI versions match the compile.
    Header0 = aec_headers:raw_key_header(),
    Header = aec_headers:set_version_and_height(Header0, Proto, 100),
    Env0 = aetx_env:tx_env_from_key_header(Header, <<0:32/unit:8>>, 0, <<0:32/unit:8>>),
    Env = aetx_env:set_context(Env0, aetx_transaction),

    {ok, InitCallData} = aect_test_utils:encode_call_data(BinSrc, "init", []),
    CreateNonce = aect_test_utils:next_nonce(Owner, S1),
    CreateTx = aect_test_utils:create_tx(Owner, #{ code => Code
                                                  , call_data => InitCallData
                                                  , vm_version => VmV
                                                  , abi_version => AbiV
                                                  , gas => 1000000
                                                  , amount => 0
                                                  , deposit => 0
                                                  , nonce => CreateNonce
                                                  }, S1),
    ContractPK = aect_contracts:compute_contract_pubkey(Owner, CreateNonce),

    Trees1 = aect_test_utils:trees(S1),
    {ok, [_], [], Trees2, _} =
        aec_trees:apply_txs_on_state_trees([dummy_sign(CreateTx)], Trees1, Env,
                                            [strict, dont_verify_signature]),
    S2 = aect_test_utils:set_trees(Trees2, S1),

    DimsStr = integer_to_list(?DIMS),
    {ok, CallCallData} = aect_test_utils:encode_call_data(
                            BinSrc, "inita", [DimsStr, DimsStr, DimsStr]),
    CallNonce = aect_test_utils:next_nonce(Owner, S2),
    CallTx = aect_test_utils:call_tx(Owner, ContractPK,
                                      #{ call_data => CallCallData
                                       , gas => 5000000000
                                       , gas_price => aec_test_utils:min_gas_price()
                                       , amount => 0
                                       , nonce => CallNonce
                                       }, S2),
    {Env, Trees2, CallTx}.

result_tag({ok, _}) -> ok;
result_tag(Other) -> Other.

real_heavy_contract_is_bounded_test_() ->
    Enabled = os:getenv("AE_HEAVY_DRY_RUN") =/= false,
    case Enabled andalso aect_test_utils:latest_protocol_version() >= ?MIN_PROTOCOL of
        false -> [];
        true  -> {timeout, 120, fun run_heavy/0}
    end.

run_heavy() ->
        {Env, Trees, CallTx} = build_fixture(),
        meck:new(aetx_env, [passthrough]),
        try
            meck:expect(aetx_env, tx_env_and_trees_from_hash,
                        fun(_Type, _Hash) -> {Env, Trees} end),

            %% Baseline: no profile (unbounded) and no heap backstop, so this
            %% synthetic 5e9-gas call (there to stress the timer) runs to the
            %% end and shows its true unbounded wall-clock cost T.
            application:set_env(aehttp, dry_run, [{max_heap_words, 0}]),
            {TBaseline, ResBaseline} =
                timer:tc(fun() -> ?TEST_MODULE:dry_run(<<0:32/unit:8>>, [], [{tx, CallTx}]) end),
            BaselineMs = TBaseline div 1000,
            ?debugFmt("dims=~p UNBOUNDED -> ~p, wall-clock T=~pms (~.2fs)",
                       [?DIMS, result_tag(ResBaseline), BaselineMs, BaselineMs / 1000]),

            %% Same call via the public endpoint profile (real 1.5s default bound).
            application:set_env(aehttp, dry_run, [{max_heap_words, 0}]),
            {TBounded, ResBounded} =
                timer:tc(fun() -> ?TEST_MODULE:dry_run(<<0:32/unit:8>>, [], [{tx, CallTx}], [{dry_run_profile, public}]) end),
            BoundedMs = TBounded div 1000,
            ?debugFmt("dims=~p BOUNDED (3s public default) -> ~p, wall-clock=~pms", [?DIMS, ResBounded, BoundedMs]),
            ?debugFmt("MONEY PROOF: unbounded T=~.2fs, bounded -> error at ~pms (~.2fs)",
                       [BaselineMs / 1000, BoundedMs, BoundedMs / 1000]),

            ?assertMatch({ok, _}, ResBaseline),
            ?assert(BaselineMs > 3000),           %% the real call is genuinely slow
            ?assertEqual({error, <<"dry-run exceeded time limit">>}, ResBounded),
            ?assert(BoundedMs < BaselineMs),        %% far less than the unbounded run
            ?assert(BoundedMs < 6000)               %% cut off near the 3s bound
        after
            meck:unload(aetx_env),
            application:unset_env(aehttp, dry_run)
        end.
