%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc Tests for the caching of the network id.
%%%-------------------------------------------------------------------

-module(aec_governance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_governance).

-define(NW_ID_A, <<"nw_id_for_testing_a">>).
-define(NW_ID_B, <<"nw_id_for_testing_b">>).

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
       fun add_network_id_uses_cache/0}]}.

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

%% The saving itself. Without this a hit could resolve and throw the answer
%% away, and every other test here would still pass.
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
    %% add_custom_network_id/2 ignores the cache.
    ?assertEqual(<<?NW_ID_B/binary, "payload">>,
                 ?TEST_MODULE:add_custom_network_id(?NW_ID_B, <<"payload">>)).

set_resolved_network_id(NetworkId) ->
    meck:expect(?TEST_MODULE, resolve_network_id, 0, NetworkId).

%% The cache must not outlive the application that owns it - see
%% aec_governance:ensure_env/0.
cache_cleared_on_app_stop_test_() ->
    {setup,
     fun() ->
             %% aecore_app:stop/1 logs, so lager has to be up.
             aec_test_utils:ensure_system_init(),
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:new(?TEST_MODULE, [passthrough]),
             %% Mocked: both cleanups erase persistent terms shared by the whole
             %% eunit VM.
             meck:new([aec_db, aec_db_gc], [passthrough]),
             meck:expect(aec_db_gc, cleanup, 0, ok),
             meck:expect(aec_db, cleanup, 0, ok)
     end,
     fun(_) ->
             ok = ?TEST_MODULE:clear_network_id_cache(),
             meck:unload([aec_db_gc, aec_db]),
             meck:unload(?TEST_MODULE)
     end,
     fun(_) ->
             [{"Stopping aecore drops the pinned network id",
               fun() ->
                       set_resolved_network_id(?NW_ID_A),
                       ok = ?TEST_MODULE:ensure_env(),
                       set_resolved_network_id(?NW_ID_B),
                       ?assertEqual(?NW_ID_A, ?TEST_MODULE:get_network_id()),
                       ok = aecore_app:stop(undefined),
                       ?assertEqual(?NW_ID_B, ?TEST_MODULE:get_network_id())
               end}]
     end}.

%% Both failures are silent: a missing hook only costs the lookup again, a hook
%% renumbered ahead of aeutils pins the schema default for the node's lifetime.
setup_hook_test_() ->
    {setup,
     fun ensure_apps_loaded/0,
     [{"The network id is pinned by a setup hook",
       fun() ->
               ?assertMatch([_], network_id_hook_phases())
       end},
      {"The hook runs after the config is read and before what derives from it",
       fun() ->
               [Phase] = network_id_hook_phases(),
               ?assert(Phase > lists:max(normal_hook_phases(aeutils))),
               ?assert(Phase < lists:min(normal_hook_phases(aecore) -- [Phase]))
       end}]}.

network_id_hook_phases() ->
    [Phase || {Phase, {?TEST_MODULE, ensure_env, []}}
                  <- normal_setup_hooks(aecore)].

normal_hook_phases(App) ->
    [Phase || {Phase, _MFA} <- normal_setup_hooks(App)].

normal_setup_hooks(App) ->
    {ok, Hooks} = application:get_env(App, '$setup_hooks'),
    {normal, Normal} = lists:keyfind(normal, 1, Hooks),
    Normal.

%% Reading an application's env needs it loaded, which a eunit run does not
%% guarantee. Left loaded afterwards: unloading would drop the env that the
%% sys.config of the run has set up for everybody else as well.
ensure_apps_loaded() ->
    lists:foreach(
      fun(App) ->
              case application:load(App) of
                  ok                             -> ok;
                  {error, {already_loaded, App}} -> ok
              end
      end, [aecore, aeutils]).
