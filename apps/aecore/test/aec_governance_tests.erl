%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc Tests for the caching of the network id.
%%%-------------------------------------------------------------------

-module(aec_governance_tests).

-include_lib("eunit/include/eunit.hrl").

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
