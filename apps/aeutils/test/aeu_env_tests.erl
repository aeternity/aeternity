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
     fun(_) -> teardown() end,
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
     fun(_) ->
             teardown(),
             ok = meck:unload(setup)
     end,
     [{"User configuration cannot contain both 'mining > cuckoo > edge_bits' and deprecated 'mining > cuckoo > miner'",
       fun deprecated_miner_section_conflicting_with_edge_bits/0},
      {"User configuration cannot contain both 'mining > cuckoo > miners' and deprecated 'mining > cuckoo > miner'",
       fun deprecated_miner_section_conflicting_with_miners/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_start_height' greater or equal to 'fork_management > fork > signalling_end_height'",
       fun invalid_fork_signalling_interval/0},
      {"User configuration cannot containt 'fork_management > fork > signalling_start_height' lower or equal to the last scheduled hard fork height",
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
     fun(_) ->
             teardown(),
             ok = meck:unload(aec_governance),
             ok = meck:unload(setup)
     end,
     [{"User configuration cannot contain more config properties for ae_uat than 'fork_management > fork > enabled'",
       fun() -> invalid_fork_signalling_network_config(<<"ae_uat">>) end},
      {"User configuration cannot contain more config properties for ae_mainnet than 'fork_management > fork > enabled'",
       fun() -> invalid_fork_signalling_network_config(<<"ae_mainnet">>) end}]
    }.

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
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    ok.

teardown() ->
    application:stop(rfc3339),
    application:stop(jesse),
    application:stop(yamerl),
    application:stop(jsx).

mock_user_config(UserMap, UserConfig) ->
    F = fun
            (aeutils, '$user_config') -> {ok, UserConfig};
            (aeutils, '$user_map') -> UserMap;
            (A, K) -> meck:passthrough([A, K])
        end,
    ok = meck:expect(setup, get_env, F),
    ok = meck:expect(setup, get_env, fun(A, K, _Default) -> F(A, K) end),
    ok.

-endif.
