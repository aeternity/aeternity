%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the example configuration in doc/examples
%%% @end
%%%=============================================================================
-module(aeu_env_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

schema_test_() ->
    {setup,
     fun() -> setup() end,
     fun(SavedFork) -> teardown(SavedFork) end,
     [{"Example user configuration files pass schema validation",
       [fun() ->
                ?assertMatch({ok, _}, aeu_env:check_config(Config))
        end || Config <- test_data_config_files()]
      }]
    }.

extra_checks_test_() ->
    {setup,
     fun() ->
             ok = meck:new(setup, [passthrough]),
             setup()
     end,
     fun(SavedFork) ->
             teardown(SavedFork),
             ok = meck:unload(setup)
     end,
     [{"User configuration cannot contain both 'mining > cuckoo > edge_bits' and deprecated 'mining > cuckoo > miner'",
       fun deprecated_miner_section_conflicting_with_edge_bits/0},
      {"User configuration cannot contain both 'mining > cuckoo > miners' and deprecated 'mining > cuckoo > miner'",
       fun deprecated_miner_section_conflicting_with_miners/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_start_height' greater or equal to 'fork_management > fork > signalling_end_height'",
       fun invalid_fork_signalling_interval/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_start_height' lower or equal to the last scheduled hard fork height",
       fun invalid_fork_signalling_start_height/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_block_count' greater than signalling interval",
       fun invalid_fork_signalling_block_count/0},
      {"User configuration cannot contain 'fork_management > fork > version' lower or equal to Minerva protocol version (2)",
       fun invalid_fork_signalling_version/0},
      {"User configuration cannot contain 'fork_management > fork > version' lower or equal to the last scheduled hard fork version",
       fun invalid_fork_signalling_version2/0}]
     ++ positive_extra_checks_tests()}.

extra_network_id_checks_test_() ->
    {setup,
     fun() ->
             ok = meck:new(setup, [passthrough]),
             ok = meck:new(aec_governance, [passthrough]),
             setup()
     end,
     fun(SavedFork) ->
             teardown(SavedFork),
             ok = meck:unload(aec_governance),
             ok = meck:unload(setup)
     end,
     [{"User configuration cannot contain more config properties for ae_uat than 'fork_management > fork > enabled'",
       fun() -> invalid_fork_signalling_network_config(<<"ae_uat">>) end},
      {"User configuration cannot contain more config properties for ae_mainnet than 'fork_management > fork > enabled'",
       fun() -> invalid_fork_signalling_network_config(<<"ae_mainnet">>) end}]
    }.

config_cache_test_() ->
    {foreach,
     fun() ->
             %% A config cached by an earlier test in this VM must not feed
             %% into the checks below.
             ok = aeu_env:invalidate_config_cache(),
             setup()
     end,
     fun(SavedFork) ->
             %% Drop the cache before anything that can fail: a leaked config
             %% would be served to every later test module sharing this VM.
             ok = aeu_env:invalidate_config_cache(),
             teardown(SavedFork)
     end,
     [{"setup's expansion is the identity for every example config file",
       fun expansion_is_identity/0},
      {"is_expandable/1 finds a '$' at any depth, in keys as well as values",
       fun is_expandable_covers_nested_terms/0},
      {"a config with nothing to expand is cached; reads skip setup",
       fun plain_config_is_cached/0},
      {"a config with an expandable value is not cached; reads stay live",
       fun expandable_config_is_not_cached/0},
      {"caching an expandable config drops the cache left by a plain one",
       fun expandable_config_drops_stale_cache/0},
      {"a value setup expands later is not frozen by the cache",
       fun expandable_value_follows_setup/0}]}.

%% The cache serves the stored config only when setup's value expansion cannot
%% change it. Assert both halves of that: the example configs have nothing to
%% expand, and for them expansion really is a no-op.
expansion_is_identity() ->
    lists:foreach(
      fun(File) ->
              {ok, {UserMap, _UserConfig}} = aeu_env:check_config(File),
              ?assertEqual(false, aeu_env:is_expandable(UserMap)),
              ok = aeu_env:cache_config(UserMap),
              ?assertEqual(setup:get_env(aeutils, '$user_map', #{}),
                           application:get_env(aeutils, '$user_map', #{})),
              ?assertEqual(setup:get_env(aeutils, '$user_config', []),
                           application:get_env(aeutils, '$user_config', [])),
              %% And the readers really do serve it. Without this the checks
              %% above would hold with the cache never consulted at all: they
              %% compare setup against the app env, not against user_map/0.
              ?assertEqual(setup:get_env(aeutils, '$user_map', #{}),
                           aeu_env:user_map()),
              ?assertEqual(setup:get_env(aeutils, '$user_config', []),
                           aeu_env:user_config())
      end, test_data_config_files()).

is_expandable_covers_nested_terms() ->
    ?assertEqual(false, aeu_env:is_expandable(#{<<"a">> => [1, 2, 3],
                                                <<"b">> => <<"plain">>})),
    ?assertEqual(false, aeu_env:is_expandable([104, 105])),
    ?assertEqual(true, aeu_env:is_expandable(#{<<"a">> => <<"$HOME">>})),
    %% Keys are expanded too, not just values.
    ?assertEqual(true, aeu_env:is_expandable(#{<<"$HOME">> => <<"v">>})),
    ?assertEqual(true, aeu_env:is_expandable([#{<<"a">> => [<<"x">>, <<"$Y">>]}])),
    %% setup's special forms carry the '$' in a string, not a binary.
    ?assertEqual(true, aeu_env:is_expandable({'$value', "$HOME"})),
    ?assertEqual(true, aeu_env:is_expandable("a$b")).

plain_config_is_cached() ->
    Map = #{<<"mempool">> => #{<<"tx_ttl">> => 123}},
    ok = aeu_env:cache_config(Map),
    %% Served from the cache now: reaching for setup must not happen.
    ok = meck:new(setup, [passthrough]),
    try
        ok = meck:expect(setup, get_env,
                         fun(_, _) -> error(unexpected_expansion) end),
        ok = meck:expect(setup, get_env,
                         fun(_, _, _) -> error(unexpected_expansion) end),
        ?assertEqual(Map, aeu_env:user_map()),
        ?assertEqual({ok, 123}, aeu_env:user_map([<<"mempool">>, <<"tx_ttl">>])),
        ?assertEqual({ok, 123}, aeu_env:user_config([<<"mempool">>, <<"tx_ttl">>])),
        %% The binary key clause reads the tree form rather than the map.
        ?assertEqual({ok, [{<<"tx_ttl">>, 123}]}, aeu_env:user_config(<<"mempool">>))
    after
        ok = meck:unload(setup)
    end.

expandable_config_is_not_cached() ->
    Map = #{<<"system">> => #{<<"plugin_path">> => <<"$DATA_DIR/plugins">>}},
    ?assertEqual(true, aeu_env:is_expandable(Map)),
    ok = aeu_env:cache_config(Map),
    ?assertEqual(undefined, cached_user_map()),
    %% Falling back is load bearing here, not merely conservative: for this
    %% config setup really does rewrite the value, so serving the stored term
    %% would return something other than what the old code path returned.
    ?assertNotEqual(application:get_env(aeutils, '$user_map', #{}),
                    setup:get_env(aeutils, '$user_map', #{})),
    %% Uncached, so the read must be exactly what the old code path returned.
    ?assertEqual(setup:get_env(aeutils, '$user_map', #{}), aeu_env:user_map()).

expandable_config_drops_stale_cache() ->
    Plain = #{<<"mempool">> => #{<<"tx_ttl">> => 1}},
    ok = aeu_env:cache_config(Plain),
    ?assertEqual(Plain, cached_user_map()),
    Expandable = Plain#{<<"system">> => #{<<"plugin_path">> => <<"$DATA_DIR/p">>}},
    ok = aeu_env:cache_config(Expandable),
    ?assertEqual(undefined, cached_user_map()),
    ?assertEqual(setup:get_env(aeutils, '$user_map', #{}), aeu_env:user_map()).

%% Why cache_expanded_config/2 refuses to cache an expandable config rather
%% than expanding it once and caching the result. aeu_logging_env rewrites
%% `setup > log_dir' at setup hook 102, after read_config/0 has already stored
%% the config at hook 100, so $LOG_DIR resolves to one directory before that
%% hook and another after it. A read has to follow setup, not a value frozen at
%% cache time - which is exactly what a config with a '$' in it gives up.
expandable_value_follows_setup() ->
    Saved = application:get_env(setup, log_dir),
    try
        ok = application:set_env(setup, log_dir, "/tmp/log-before"),
        ok = aeu_env:cache_config(#{<<"a">> => <<"$LOG_DIR/x">>}),
        ?assertEqual(undefined, cached_user_map()),
        ?assertEqual({ok, <<"/tmp/log-before/x">>}, aeu_env:user_map([<<"a">>])),
        %% The later hook moves it. The next read must see the new directory.
        ok = application:set_env(setup, log_dir, "/tmp/log-after"),
        ?assertEqual({ok, <<"/tmp/log-after/x">>}, aeu_env:user_map([<<"a">>]))
    after
        case Saved of
            {ok, Dir} -> application:set_env(setup, log_dir, Dir);
            undefined -> application:unset_env(setup, log_dir)
        end
    end.

cached_user_map() ->
    persistent_term:get({aeu_env, user_map}, undefined).

positive_extra_checks_tests() ->
    [{"Example user configuration file passes checks further to the schema: " ++ Config, %% For enabling files to be linked from wiki as examples.
      fun() ->
              {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
              ok = mock_user_config(UserMap, UserConfig),
              ?assertEqual(ok, aec_hard_forks:ensure_env()),
              ?assertEqual(ok, aec_mining:check_env())
      end
     } || Config <- test_data_config_files()].

deprecated_miner_section_conflicting_with_edge_bits() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_deprecated_miner_with_edge_bits.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertExit(cuckoo_config_validation_failed, aec_mining:check_env()).

deprecated_miner_section_conflicting_with_miners() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_deprecated_miner_with_miners.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertExit(cuckoo_config_validation_failed, aec_mining:check_env()).

invalid_fork_signalling_interval() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_interval.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError({illegal_fork_signalling_interval, _, _}, aec_hard_forks:ensure_env()).

invalid_fork_signalling_start_height() ->
    case aec_governance:get_network_id() of
        <<"local_roma_testnet">> ->
            %% Roma started with height 0, so signalling height cannot be lower.
            ok;
        _Other ->
            {Dir, DataDir} = get_test_config_base(),
            Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_start_height.yaml"]),
            {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
            ok = mock_user_config(UserMap, UserConfig),
            ?assertError({illegal_fork_signalling_interval, _, _}, aec_hard_forks:ensure_env())
    end.

invalid_fork_signalling_block_count() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_block_count.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError({illegal_fork_signalling_block_count, _}, aec_hard_forks:ensure_env()).

invalid_fork_signalling_version() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_version.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError({illegal_fork_version, _}, aec_hard_forks:ensure_env()).

invalid_fork_signalling_version2() ->
    case aec_governance:get_network_id() of
        <<"local_lima_testnet">> ->
            {Dir, DataDir} = get_test_config_base(),
            Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_version2.yaml"]),
            {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
            ok = mock_user_config(UserMap, UserConfig),
            ?assertError({illegal_fork_version, _}, aec_hard_forks:ensure_env());
        _Other ->
            ok
    end.

invalid_fork_signalling_network_config(NetworkId) ->
    ok = meck:expect(aec_governance, get_network_id, fun() -> NetworkId end),
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_network_config.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError(illegal_fork_signalling_config, aec_hard_forks:ensure_env()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_data_config_files() ->
    {Dir, DataDir} = get_test_config_base(),
    [filename:join([Dir, DataDir, "epoch_full.yaml"]),
     filename:join([Dir, DataDir, "epoch_no_peers.yaml"]),
     filename:join([Dir, DataDir, "epoch_no_newline.yaml"]),
     filename:join([Dir, DataDir, "epoch_prebuilt_miner.yaml"]),
     filename:join([Dir, DataDir, "epoch_deprecated_miner.yaml"]),
     filename:join([Dir, DataDir, "epoch_deprecated_debug_api.yaml"]),
     filename:join([Dir, DataDir, "epoch_monitoring.yaml"]),
     filename:join([Dir, DataDir, "epoch_nice.yaml"])].

get_test_config_base() ->
    %% differentiate between Eunit run in top directory and
    %% common test run in _build/test/logs/...
    %% This should be rebar3 thingy, if only one would know how.
    DataDir = "aeutils/test/data/",
    Dir =
        case filelib:is_dir(filename:join("apps", DataDir)) of
            true -> "apps/";
            false -> "../../lib/"
        end,
    {Dir, DataDir}.

setup() ->
    %% A config installed by an earlier test in this VM must not feed into the
    %% checks below.
    ok = forget_user_config(),
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    %% Several of the checks below run aec_hard_forks:ensure_env/0, which
    %% rewrites `aecore > fork` from whatever config they installed - and
    %% unsets it for a config that does not configure fork signalling, which
    %% every config here is. That key decides which protocol a block falls
    %% under, for every block, so hand it to teardown/1 to put back.
    application:get_env(aecore, fork).

teardown(SavedFork) ->
    %% The checks above store a config and a community fork; they must not leak
    %% into other tests sharing this VM.
    case SavedFork of
        {ok, Fork} -> application:set_env(aecore, fork, Fork);
        undefined  -> application:unset_env(aecore, fork)
    end,
    ok = forget_user_config(),
    application:stop(rfc3339),
    application:stop(jesse),
    application:stop(yamerl),
    application:stop(jsx).

mock_user_config(UserMap, UserConfig) ->
    %% This installs a config without going through aeu_env:cache_config/1, so
    %% a config left by an earlier read_config/0 in this VM would shadow it.
    ok = forget_user_config(),
    %% setup:get_env/2 answers {ok, V} | undefined, setup:get_env/3 answers the
    %% bare value or the default. Both are mocked from one clause set, so that
    %% user_config/0 and user_map/0 - which read the arity 3 form - see the
    %% value itself rather than an {ok, _} wrapper around it.
    F = fun
            (aeutils, '$user_config') -> {ok, UserConfig};
            (aeutils, '$user_map')    -> {ok, UserMap};
            (A, K)                    -> meck:passthrough([A, K])
        end,
    ok = meck:expect(setup, get_env, F),
    ok = meck:expect(setup, get_env,
                     fun(A, K, Default) ->
                             case F(A, K) of
                                 {ok, V}   -> V;
                                 undefined -> Default
                             end
                     end),
    ok.

%% aeu_env:cache_config/1 writes the config to the aeutils app env as well as
%% to the cache, and invalidate_config_cache/0 drops only the cache. Both have
%% to go: a config left in the app env is served to every later test module
%% sharing this VM, and aec_tx_pool reads `mempool > tx_ttl` from it.
forget_user_config() ->
    ok = aeu_env:invalidate_config_cache(),
    ok = application:unset_env(aeutils, '$user_map'),
    ok = application:unset_env(aeutils, '$user_config').

-endif.
