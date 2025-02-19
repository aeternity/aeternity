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
      {"User configuration cannot contain 'fork_management > fork > signalling_start_height' lower or equal to the last scheduled hard fork height",
       fun invalid_fork_signalling_start_height/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_block_count' greater than signalling interval",
       fun invalid_fork_signalling_block_count/0},
      {"User configuration cannot contain 'fork_management > fork > version' lower or equal to Minerva protocol version (2)",
       fun invalid_fork_signalling_version/0},
      {"User configuration cannot contain 'fork_management > fork > version' lower or equal to the last scheduled hard fork version",
       fun invalid_fork_signalling_version2/0},
      {"Valid schema for each supported type",
       fun valid_schema_types/0},
      {"Default values from the configuration schema for each type",
       fun schema_defaults/0},
      {"User configuration overwrites schema defaults",
       fun user_config_overwrites_schema_defaults/0},
      {"Environment config overwrites schema defaults",
       fun env_overwrites_schema_defaults/0}]

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

valid_schema_types() ->
    ?assertMatch({ok, #{<<"type">> := <<"boolean">>, <<"default">> := true}},
                 aeu_env:schema([<<"chain">>, <<"persist">>])),
    ?assertMatch({ok, #{<<"type">> := <<"string">>, <<"default">> := <<"data">>}},
                 aeu_env:schema([<<"chain">>, <<"db_path">>])),
    ?assertMatch({ok, #{<<"type">> := <<"integer">>, <<"default">> := 3}},
                 aeu_env:schema([<<"chain">>, <<"db_write_max_retries">>])),
    ?assertMatch({ok, #{<<"type">> := <<"array">>, <<"items">> := #{<<"default">> := []}}},
                 aeu_env:schema([<<"chain">>, <<"protocol_beneficiaries">>])),
    ?assertMatch({ok, #{<<"type">> := <<"object">>, <<"properties">> := _}},
                 aeu_env:schema([<<"chain">>, <<"currency">>])),
    ?assertMatch({ok, #{<<"type">> := <<"object">>, <<"patternProperties">> := _}},
                 aeu_env:schema([<<"chain">>, <<"consensus">>])),
    ?assertMatch({ok, #{<<"type">> := <<"object">>, <<"properties">> := #{<<"type">> := _, <<"config">> := _}}},
                 aeu_env:schema([<<"chain">>, <<"consensus">>, <<"0">>])),
    ?assertMatch({ok, #{<<"type">> := <<"object">>, <<"properties">> := _}},
                 aeu_env:schema([<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>])),
    ok.

schema_defaults() ->
    ?assertEqual({ok, true}, aeu_env:schema_default_values([<<"chain">>, <<"persist">>])),
    ?assertEqual({ok, <<"data">>}, aeu_env:schema_default_values([<<"chain">>, <<"db_path">>])),
    ?assertEqual({ok, 3}, aeu_env:schema_default_values([<<"chain">>, <<"db_write_max_retries">>])),
    ?assertEqual({ok, []}, aeu_env:schema_default_values([<<"chain">>, <<"protocol_beneficiaries">>])),
    ?assertEqual({ok, #{<<"name">> => <<"aeternity">>,
                        <<"subunit">> => <<"aetto">>,
                        <<"subunits_per_unit">> => 1000000000000000000,
                        <<"symbol">> => <<"AE">>}},
                 aeu_env:schema_default_values([<<"chain">>, <<"currency">>])),
    ?assertMatch(undefined, aeu_env:schema_default_values([<<"chain">>, <<"consensus">>])),
    ?assertMatch(undefined, aeu_env:schema_default_values([<<"chain">>, <<"missing_definition">>])),
    ?assertMatch(undefined, aeu_env:schema_default_values([<<"http">>, <<"endpoints">>, <<"chain">>])),
    ?assertMatch(undefined, aeu_env:schema_default_values([<<"http">>, <<"endpoints">>, <<"missing_definition">>])),
    ?assertMatch({ok, [<<"accounts">>, <<"calls">>, <<"channels">>, <<"contracts">>, <<"ns">>, <<"oracles">>]},
                 aeu_env:schema_default_values([<<"chain">>, <<"garbage_collection">>, <<"trees">>])),
    ?assertMatch({ok, #{<<"config">> := #{<<"child_block_time">> := 3000,
                              <<"parent_chain">> := #{<<"consensus">> := #{<<"type">> := <<"AE2AE">>}},
                              <<"stakers">> := []},
                        <<"type">> := <<"hyperchain">>}},
                 aeu_env:schema_default_values([<<"chain">>, <<"consensus">>, <<"0">>])),
    ?assertMatch({ok, #{<<"child_block_time">> := 3000,
                        <<"parent_chain">> := #{<<"consensus">> := #{<<"type">> := <<"AE2AE">>}},
                        <<"stakers">> := []}},
                 aeu_env:schema_default_values([<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>])),
    ok.

user_config_overwrites_schema_defaults() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_defaults.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),

    ?assertEqual({ok, true}, aeu_env:schema_default([<<"chain">>, <<"persist">>])),
    ?assertEqual(false, aeu_env:config_value([<<"chain">>, <<"persist">>], aecore, tests)),

    ?assertEqual({ok, <<"data">>}, aeu_env:schema_default([<<"chain">>, <<"db_path">>])),
    ?assertEqual(<<"test">>, aeu_env:config_value([<<"chain">>, <<"db_path">>], aecore, tests)),

    ?assertEqual({ok, 3}, aeu_env:schema_default([<<"chain">>, <<"db_write_max_retries">>])),
    ?assertEqual(99, aeu_env:config_value([<<"chain">>, <<"db_write_max_retries">>], aecore, tests)),

    ?assertEqual({ok, []}, aeu_env:schema_default_values([<<"chain">>, <<"protocol_beneficiaries">>])),
    ?assertEqual([<<"ak_DummyPubKeyDoNotEverUse999999999999999999999999999:120">>],
                 aeu_env:config_value([<<"chain">>, <<"protocol_beneficiaries">>], aecore, tests)),

    ok.

env_overwrites_schema_defaults() ->
    application:set_env(aecore, consensus, #{<<"0">> => #{<<"type">> => <<"test">>, <<"config">> => a}}),
    ?assertEqual(#{<<"0">> => #{<<"type">> => <<"test">>, <<"config">> => a}},
                 aeu_env:config_value([<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>], aecore, consensus)),
    ?assertEqual(a, aeu_env:config_with_defaults(a, [<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>])),
    ok = application:unset_env(aecore, consensus),

    ok.

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
