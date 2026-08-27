%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc Tests for the caching of the network id, and for the block gas limit
%%%      the network id decides - both arities of it, which is the point:
%%%      block admission and Chain.block_gas_limit are one number per network.
%%%-------------------------------------------------------------------

-module(aec_governance_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(TEST_MODULE, aec_governance).

-define(NW_ID_A, <<"nw_id_for_testing_a">>).
-define(NW_ID_B, <<"nw_id_for_testing_b">>).

-define(LIFECYCLE_MOCKS, [?TEST_MODULE, aec_db, aec_db_gc, aec_jobs_queues]).

%% Resolution is mocked rather than configured - see resolve_network_id/0 in
%% aec_governance.
network_id_cache_test_() ->
    {foreach,
     fun() ->
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:new(?TEST_MODULE, [passthrough]),
             set_resolved_network_id(?NW_ID_A)
     end,
     fun(_) ->
             %% Cleared first: a throw from meck:unload/1 must not leave a
             %% pinned test id behind for the rest of the eunit VM.
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:unload(?TEST_MODULE)
     end,
     [{"An uncached network id keeps following the configuration",
       fun uncached_network_id_follows_resolution/0},
      {"ensure_env/0 pins the network id until the cache is cleared",
       fun cached_network_id_is_pinned/0},
      {"A cache hit does not resolve",
       fun cached_network_id_skips_resolution/0},
      {"Payload prefixing uses the cached network id",
       fun add_network_id_uses_cache/0},
      {"A failed ensure_env/0 leaves the cache empty, not stale",
       fun failed_ensure_env_leaves_cache_empty/0}]}.

uncached_network_id_follows_resolution() ->
    ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()),
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id()).

cached_network_id_is_pinned() ->
    ok = ?TEST_MODULE:ensure_env(),
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()),
    ok = ?TEST_MODULE:clear_network_id_cache(),
    ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id()),
    ok = ?TEST_MODULE:ensure_env(),
    set_resolved_network_id(?NW_ID_A),
    ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id()).

%% Without this a hit could resolve and throw the answer away, and every other
%% test here would still pass.
cached_network_id_skips_resolution() ->
    ok = ?TEST_MODULE:ensure_env(),
    meck:reset(?TEST_MODULE),
    ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()),
    ?assertEqual(0, meck:num_calls(?TEST_MODULE, resolve_network_id, [])).

add_network_id_uses_cache() ->
    ok = ?TEST_MODULE:ensure_env(),
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(<<?NW_ID_A/binary, "payload">>,
                 ?TEST_MODULE:add_network_id(<<"payload">>)),
    ?assertEqual(<<"payload", ?NW_ID_A/binary>>,
                 ?TEST_MODULE:add_network_id_last(<<"payload">>)),
    ?assertEqual(<<?NW_ID_B/binary, "payload">>,
                 ?TEST_MODULE:add_custom_network_id(?NW_ID_B, <<"payload">>)).

failed_ensure_env_leaves_cache_empty() ->
    ok = ?TEST_MODULE:ensure_env(),
    %% The failure resolve_network_id/0 can really produce: a non-binary in the
    %% config falls off its is_binary/1 clause.
    meck:expect(?TEST_MODULE, resolve_network_id, 0,
                meck:raise(error, {case_clause, "ae_uat"})),
    %% The failure has to propagate: a setup hook that swallowed a bad
    %% configuration would boot the node under an id nobody asked for.
    ?assertError({case_clause, "ae_uat"}, ?TEST_MODULE:ensure_env()),
    %% Two resolutions in a row: a cache pinned to either value fails one of them.
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id()),
    set_resolved_network_id(?NW_ID_A),
    ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()).

%% The aecore restart that the setup hook does not cover - see
%% aec_governance:ensure_env/0.
app_lifecycle_test_() ->
    {foreach,
     fun() ->
             %% aecore_app:start/2 and stop/1 both log, so lager has to be up.
             aec_test_utils:ensure_system_init(),
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:new(?LIFECYCLE_MOCKS, [passthrough]),
             %% Mocked: both cleanups erase persistent terms shared by the whole
             %% eunit VM.
             meck:expect(aec_db_gc, cleanup, 0, ok),
             meck:expect(aec_db, cleanup, 0, ok),
             %% Failing the call right after ensure_env/0 aborts start/2 before
             %% mnesia and aecore_sup.
             meck:expect(aec_jobs_queues, start, 0,
                         meck:raise(throw, stop_start)),
             set_resolved_network_id(?NW_ID_A)
     end,
     fun(_) ->
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:unload(?LIFECYCLE_MOCKS)
     end,
     [{"Stopping aecore makes the network id follow the configuration again",
       fun stopping_aecore_unpins_the_network_id/0},
      {"Starting aecore pins the network id again",
       fun starting_aecore_pins_the_network_id/0}]}.

stopping_aecore_unpins_the_network_id() ->
    %% Not that the id changes when aecore stops - that the cache does not
    %% outlive the application, so reads go back to the configuration.
    ok = ?TEST_MODULE:ensure_env(),
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()),
    ok = aecore_app:stop(undefined),
    ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id()).

starting_aecore_pins_the_network_id() ->
    %% The mocked aec_jobs_queues:start/0 aborts start/2 right after the pin.
    ?assertThrow(stop_start, aecore_app:start(normal, [])),
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()).

set_resolved_network_id(NetworkId) ->
    meck:expect(?TEST_MODULE, resolve_network_id, 0, NetworkId).

%% Both failures are silent: a missing hook only costs the lookup again, a hook
%% renumbered ahead of aeutils pins the schema default for the node's lifetime.
setup_hook_test_() ->
    [{"The network id is pinned by a setup hook",
      fun() ->
              ?assertMatch([_], network_id_hook_phases())
      end},
     {"The hook runs after the config is read and before what derives from it",
      fun() ->
              [Phase] = network_id_hook_phases(),
              ?assert(Phase > lists:max(normal_hook_phases(aeutils))),
              ?assert(Phase < lists:min(normal_hook_phases(aecore) -- [Phase]))
      end}].

network_id_hook_phases() ->
    [Phase || {Phase, {?TEST_MODULE, ensure_env, []}}
                  <- normal_setup_hooks(aecore)].

normal_hook_phases(App) ->
    [Phase || {Phase, _MFA} <- normal_setup_hooks(App)].

%% Read from the .app file: loading aecore would leave it loaded for the rest of
%% the eunit VM. Nothing overrides '$setup_hooks' from a sys.config, so the .app
%% file is the only source there is.
normal_setup_hooks(App) ->
    {env, Env} = lists:keyfind(env, 1, app_file(App)),
    {'$setup_hooks', Hooks} = lists:keyfind('$setup_hooks', 1, Env),
    {normal, Normal} = lists:keyfind(normal, 1, Hooks),
    Normal.

app_file(App) ->
    {ok, [{application, App, Props}]} =
        file:consult(code:where_is_file(atom_to_list(App) ++ ".app")),
    Props.

%% On a network whose limit is everybody's, an override is a fork rather than
%% a local knob, so ensure_env/0 refuses to start with one set.
block_gas_limit_override_test_() ->
    {foreach,
     fun() ->
             %% The refusal logs before it raises.
             aec_test_utils:ensure_system_init(),
             ok = application:unset_env(aecore, block_gas_limit),
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:new(?TEST_MODULE, [passthrough]),
             set_resolved_network_id(?NW_ID_A)
     end,
     fun(_) ->
             ok = application:unset_env(aecore, block_gas_limit),
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:unload(?TEST_MODULE)
     end,
     [{"Every network starts when nothing is overriding the limit",
       fun no_override_starts_on_every_network/0},
      {"A network that fixes the limit refuses to start with an override",
       fun override_refuses_to_start_on_fixed_networks/0},
      {"A refused override leaves the network id cache empty, not stale",
       fun refused_override_leaves_cache_empty/0},
      {"Configuring the network's own value is not an override",
       fun override_equal_to_the_network_value_starts/0},
      {"A network that does not fix the limit still takes an override",
       fun override_allowed_on_other_networks/0}]}.

%% ae_dev and the hyperchain ids are deliberately not here: their limit is a
%% property of that deployment, agreed among its own nodes.
fixed_limit_network_ids() ->
    [<<"ae_mainnet">>, <<"ae_uat">>].

no_override_starts_on_every_network() ->
    [ begin
          set_resolved_network_id(NetworkId),
          ok = ?TEST_MODULE:clear_network_id_cache(),
          ?assertEqual(ok, ?TEST_MODULE:ensure_env()),
          ?assertEqual(NetworkId, ?TEST_MODULE:get_network_id())
      end || NetworkId <- [?NW_ID_A | fixed_limit_network_ids()] ].

override_refuses_to_start_on_fixed_networks() ->
    NetworkValue = ?TEST_MODULE:block_gas_limit(),
    [ begin
          set_resolved_network_id(NetworkId),
          ok = ?TEST_MODULE:clear_network_id_cache(),
          ok = application:set_env(aecore, block_gas_limit, NetworkValue * 2),
          ?assertError({block_gas_limit_override_would_fork,
                        #{network_id := NetworkId,
                          configured := _,
                          network    := NetworkValue}},
                       ?TEST_MODULE:ensure_env()),
          %% Lowering it forks just as surely as raising it.
          ok = application:set_env(aecore, block_gas_limit, NetworkValue - 1),
          ?assertError({block_gas_limit_override_would_fork, _},
                       ?TEST_MODULE:ensure_env())
      end || NetworkId <- fixed_limit_network_ids() ].

%% Same fail-closed contract the network id resolution has: a refusal must not
%% leave a pinned id behind for whatever runs next in this VM.
refused_override_leaves_cache_empty() ->
    set_resolved_network_id(<<"ae_mainnet">>),
    ok = ?TEST_MODULE:ensure_env(),
    ok = application:set_env(aecore, block_gas_limit, 1),
    ?assertError({block_gas_limit_override_would_fork, _}, ?TEST_MODULE:ensure_env()),
    ok = application:unset_env(aecore, block_gas_limit),
    set_resolved_network_id(?NW_ID_B),
    ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id()).

override_equal_to_the_network_value_starts() ->
    NetworkValue = ?TEST_MODULE:block_gas_limit(),
    ok = application:set_env(aecore, block_gas_limit, NetworkValue),
    [ begin
          set_resolved_network_id(NetworkId),
          ok = ?TEST_MODULE:clear_network_id_cache(),
          ?assertEqual(ok, ?TEST_MODULE:ensure_env()),
          ?assertEqual(NetworkValue, ?TEST_MODULE:block_gas_limit())
      end || NetworkId <- fixed_limit_network_ids() ].

override_allowed_on_other_networks() ->
    Override = ?TEST_MODULE:block_gas_limit() * 3,
    ok = application:set_env(aecore, block_gas_limit, Override),
    ?assertEqual(ok, ?TEST_MODULE:ensure_env()),
    ?assertEqual(Override, ?TEST_MODULE:block_gas_limit()).
%%%===================================================================
%%% Block gas limit. ?FIXED_BLOCK_GAS_LIMIT_NETWORKS is the read-site test for
%%% both arities: on a network that fixes the limit neither reads the
%%% operator's env, and on one that does not both read it. What the node
%%% admits and what a contract is told are therefore the same number, always.
%%%===================================================================

%% Written out rather than derived from ?BLOCK_GAS_LIMIT (module-local to
%% aec_governance) so the number itself is pinned: this is the value every
%% node has computed for all of history, and a patch that moves it has to
%% restate it here.
-define(HISTORICAL_BLOCK_GAS_LIMIT, 6000000).

%% Networks that leave the limit to their operators. ae_dev is the node's own;
%% the other two stand in for a hyperchain, whose ids this module cannot know.
-define(CONFIGURABLE_NETWORK_IDS, [<<"ae_dev">>, ?NW_ID_A, ?NW_ID_B]).

%% Deliberately not aec_hard_forks:sorted_protocol_versions/0 - that returns
%% only the protocols the eunit VM's network id enables, so a single-protocol
%% lane would shrink the "every protocol version" claim to one row.
-define(ALL_PROTOCOLS, [ ?ROMA_PROTOCOL_VSN
                       , ?MINERVA_PROTOCOL_VSN
                       , ?FORTUNA_PROTOCOL_VSN
                       , ?LIMA_PROTOCOL_VSN
                       , ?IRIS_PROTOCOL_VSN
                       , ?CERES_PROTOCOL_VSN
                       , ?ARCUS_PROTOCOL_VSN
                       , ?SALUS_PROTOCOL_VSN
                       ]).

-define(OVERRIDE_BLOCK_GAS_LIMIT, 1234567).

block_gas_limit_test_() ->
    {foreach,
     fun() ->
             %% the_patch_moves_nothing_a_started_node_reads/0 reaches the
             %% refusal, and the refusal logs before it raises.
             aec_test_utils:ensure_system_init(),
             Saved = application:get_env(aecore, block_gas_limit),
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:new(?TEST_MODULE, [passthrough]),
             Saved
     end,
     fun(Saved) ->
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:unload(?TEST_MODULE),
             restore_env(block_gas_limit, Saved)
     end,
     [{"With nothing configured the limit is the historical value everywhere",
       fun block_gas_limit_is_historical_unconfigured/0},
      {"A network that fixes the limit ignores an override set after boot",
       fun runtime_override_moves_neither_arity_on_a_fixed_network/0},
      {"A network that does not fix it moves both arities together",
       fun both_arities_follow_the_knob_on_a_configurable_network/0},
      {"The two arities agree on every network, configured or not",
       fun both_arities_agree_on_every_network/0},
      {"A node that started reads what it read before this change",
       fun the_patch_moves_nothing_a_started_node_reads/0},
      {"Every protocol version has a clause",
       fun consensus_block_gas_limit_is_total/0}]}.

all_network_ids() ->
    fixed_limit_network_ids() ++ ?CONFIGURABLE_NETWORK_IDS.

%% Resolution is mocked, and the read-back is not decoration: the eunit VM is
%% started with -network_id local_<protocol>_testnet, which is a configurable
%% id, so a mock that failed to take would leave every case below silently
%% exercising the one lane it is trying to tell apart from the other.
with_network_id(NetworkId, Fun) ->
    set_resolved_network_id(NetworkId),
    ok = ?TEST_MODULE:clear_network_id_cache(),
    ?assertEqual(NetworkId, ?TEST_MODULE:get_network_id()),
    Fun().

block_gas_limit_is_historical_unconfigured() ->
    ok = application:unset_env(aecore, block_gas_limit),
    [ with_network_id(
        NetworkId,
        fun() ->
                ?assertEqual({NetworkId, ?HISTORICAL_BLOCK_GAS_LIMIT},
                             {NetworkId, ?TEST_MODULE:block_gas_limit()}),
                [?assertEqual({NetworkId, Protocol, ?HISTORICAL_BLOCK_GAS_LIMIT},
                              {NetworkId, Protocol,
                               ?TEST_MODULE:block_gas_limit(Protocol)})
                 || Protocol <- ?ALL_PROTOCOLS]
        end)
      || NetworkId <- all_network_ids() ].

%% The red witness for the read-site test. check_block_gas_limit/1 runs once, at
%% boot; this override arrives after it, which is exactly what a remote shell
%% does - and what the branch's own aehttp_integration_SUITE helper
%% assert_node_settings_block_gas_limit_live/3 does to a running node. Without
%% the test at the read site, block_gas_limit/0 answers ?OVERRIDE_BLOCK_GAS_LIMIT
%% here and this node admits micro blocks no other ae_mainnet node would.
runtime_override_moves_neither_arity_on_a_fixed_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:unset_env(aecore, block_gas_limit),
                %% The node starts, because at boot nothing was overriding.
                ?assertEqual(ok, ?TEST_MODULE:ensure_env()),
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                ?assertEqual({NetworkId, ?HISTORICAL_BLOCK_GAS_LIMIT},
                             {NetworkId, ?TEST_MODULE:block_gas_limit()}),
                [?assertEqual({NetworkId, Protocol, ?HISTORICAL_BLOCK_GAS_LIMIT},
                              {NetworkId, Protocol,
                               ?TEST_MODULE:block_gas_limit(Protocol)})
                 || Protocol <- ?ALL_PROTOCOLS]
        end)
      || NetworkId <- fixed_limit_network_ids() ].

%% The other red witness. Before the read-site test block_gas_limit/1 answered
%% ?HISTORICAL_BLOCK_GAS_LIMIT here while the node admitted blocks up to
%% ?OVERRIDE_BLOCK_GAS_LIMIT - a deployment whose contracts are told a limit
%% its own nodes do not use.
both_arities_follow_the_knob_on_a_configurable_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = application:set_env(aecore, block_gas_limit,
                                         ?OVERRIDE_BLOCK_GAS_LIMIT),
                ?assertEqual({NetworkId, ?OVERRIDE_BLOCK_GAS_LIMIT},
                             {NetworkId, ?TEST_MODULE:block_gas_limit()}),
                [?assertEqual({NetworkId, Protocol, ?OVERRIDE_BLOCK_GAS_LIMIT},
                              {NetworkId, Protocol,
                               ?TEST_MODULE:block_gas_limit(Protocol)})
                 || Protocol <- ?ALL_PROTOCOLS]
        end)
      || NetworkId <- ?CONFIGURABLE_NETWORK_IDS ].

%% The property itself, stated without naming which side of the test a network
%% falls on: one value per network, whatever is configured and whatever
%% protocol is asking.
both_arities_agree_on_every_network() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = restore_env(block_gas_limit, Configured),
                [?assertEqual({NetworkId, Configured, Protocol,
                               ?TEST_MODULE:block_gas_limit()},
                              {NetworkId, Configured, Protocol,
                               ?TEST_MODULE:block_gas_limit(Protocol)})
                 || Protocol <- ?ALL_PROTOCOLS]
        end)
      || NetworkId  <- all_network_ids(),
         Configured <- configurations() ].

%% Replay identity as a test rather than a claim in a commit message. The
%% pre-change block_gas_limit/0 was application:get_env(aecore, block_gas_limit,
%% ?BLOCK_GAS_LIMIT) and nothing else, so it is evaluated here rather than
%% recalled. On a node that started, both arities now return that number: on the
%% fixed networks because starting at all is what proves the env equals
%% ?BLOCK_GAS_LIMIT, everywhere else because the env is what they read.
the_patch_moves_nothing_a_started_node_reads() ->
    [ with_network_id(
        NetworkId,
        fun() ->
                ok = restore_env(block_gas_limit, Configured),
                Before = application:get_env(aecore, block_gas_limit,
                                             ?HISTORICAL_BLOCK_GAS_LIMIT),
                case node_starts() of
                    true ->
                        ?assertEqual({NetworkId, Configured, Before},
                                     {NetworkId, Configured,
                                      ?TEST_MODULE:block_gas_limit()}),
                        [?assertEqual({NetworkId, Configured, Protocol, Before},
                                      {NetworkId, Configured, Protocol,
                                       ?TEST_MODULE:block_gas_limit(Protocol)})
                         || Protocol <- ?ALL_PROTOCOLS];
                    false ->
                        %% The only node that can be: one on a network that
                        %% fixes the limit, told to run its own. It reads
                        %% nothing, because it does not start.
                        ?assert(lists:member(NetworkId, fixed_limit_network_ids())),
                        ?assertNotEqual(?HISTORICAL_BLOCK_GAS_LIMIT, Before)
                end
        end)
      || NetworkId  <- all_network_ids(),
         Configured <- configurations() ].

configurations() ->
    [undefined,
     {ok, ?HISTORICAL_BLOCK_GAS_LIMIT},
     {ok, ?OVERRIDE_BLOCK_GAS_LIMIT}].

node_starts() ->
    try ?TEST_MODULE:ensure_env() of
        ok -> true
    catch
        error:{block_gas_limit_override_would_fork, _} -> false
    end.

%% A clause set closed at the newest named protocol would crash the node the
%% day the next one is added, which is the failure mode a "pin it at every
%% protocol" change most easily introduces. Read on a network that fixes the
%% limit, with the knob set, so a clause that fell through to the env would be
%% visible here rather than answering the right number for the wrong reason.
consensus_block_gas_limit_is_total() ->
    Unknown = lists:max(?ALL_PROTOCOLS) + 1,
    with_network_id(
      <<"ae_mainnet">>,
      fun() ->
              ok = application:set_env(aecore, block_gas_limit,
                                       ?OVERRIDE_BLOCK_GAS_LIMIT),
              ?assertEqual(?HISTORICAL_BLOCK_GAS_LIMIT,
                           ?TEST_MODULE:block_gas_limit(Unknown))
      end).

%% Structural, off the compiled beam. The cases above can only speak for the
%% network ids they name; this one speaks for every id there will ever be: the
%% two arities are one closure, so no future edit can give one of them a test
%% the other does not have. Runs outside the fixture above on purpose - under
%% meck the module's beam is not the one code:which/1 points at.
both_arities_are_one_closure_test() ->
    NodeLocal = direct_callees({?TEST_MODULE, block_gas_limit, 0}),
    Consensus = direct_callees({?TEST_MODULE, block_gas_limit, 1}),
    %% A read that collapsed to nothing - an unreadable beam, a call form this
    %% module does not recognise - would satisfy the equality while proving
    %% nothing at all.
    ?assertNotEqual([], NodeLocal),
    ?assertEqual(NodeLocal, Consensus).

direct_callees({M, F, A}) ->
    case code:which(M) of
        Path when is_list(Path) ->
            {beam_file, _, _, _, _, Fs} = beam_disasm:file(Path),
            case [Code || {function, Fn, Ar, _, Code} <- Fs, Fn =:= F, Ar =:= A] of
                [Code] -> lists:usort(lists:flatmap(fun call_target/1, Code));
                []     -> erlang:error({no_such_function, {M, F, A}})
            end;
        NoBeam ->
            erlang:error({cannot_disassemble, M, NoBeam})
    end.

call_target({call, _, MFA})                             -> [MFA];
call_target({call_only, _, MFA})                        -> [MFA];
call_target({call_last, _, MFA, _})                     -> [MFA];
call_target({call_ext, _, {extfunc, M, F, A}})          -> [{M, F, A}];
call_target({call_ext_only, _, {extfunc, M, F, A}})     -> [{M, F, A}];
call_target({call_ext_last, _, {extfunc, M, F, A}, _})  -> [{M, F, A}];
call_target(_)                                          -> [].

restore_env(Key, undefined) ->
    application:unset_env(aecore, Key);
restore_env(Key, {ok, Value}) ->
    application:set_env(aecore, Key, Value).
