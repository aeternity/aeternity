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
     fun setup/0,
     fun teardown/1,
     [{"Example user configuration files pass schema validation",
       [fun() ->
          ?assertMatch({ok, _}, aeu_env:check_config(Config))
        end || Config <- test_data_config_files()]
       }]
    }.

extra_checks_test_() ->
    {setup,
     fun() -> ok = meck:new(setup, [passthrough]), setup() end,
     fun(R) -> teardown(R), ok = meck:unload(setup) end,
     [{"Example user configuration file passes checks further to the schema: " ++ Config, %% For enabling files to be linked from wiki as examples.
       fun() ->
               {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
               ok = mock_user_config(UserMap, UserConfig),
               ?assertEqual(ok, aec_hard_forks:check_env())
       end
      } || Config <- test_data_config_files()]
    }.

test_data_config_files() ->
    %% differentiate between Eunit run in top directory and
    %% common test run in _build/test/logs/...
    %% This should be rebar3 thingy, if only one would know how.
    DataDir = "aeutils/test/data/",
    Dir = 
        case filelib:is_dir(filename:join("apps", DataDir)) of
            true -> "apps/";
            false -> "../../lib/"
        end,
    [filename:join([Dir, DataDir, "epoch_full.yaml"]),
     filename:join([Dir, DataDir, "epoch_no_peers.yaml"]),
     filename:join([Dir, DataDir, "epoch_no_newline.yaml"]),
     filename:join([Dir, DataDir, "epoch_testnet.yaml"]),
     filename:join([Dir, DataDir, "epoch_prebuilt_miner.yaml"])].

setup() ->
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    ok.

teardown(_) ->
    application:stop(rfc3339),
    application:stop(jesse),
    application:stop(yamerl),
    application:stop(jsx).

mock_user_config(UserMap, UserConfig) ->
    F = fun
            (aeutils, '$user_config') -> UserConfig;
            (aeutils, '$user_map') -> UserMap;
            (_A, _K) -> meck:passthrough()
        end,
    ok = meck:expect(setup, get_env, F),
    ok = meck:expect(setup, get_env, fun(A, K, _Default) -> F(A, K) end),
    ok.

-endif.
