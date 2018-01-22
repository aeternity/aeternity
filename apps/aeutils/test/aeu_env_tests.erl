%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the example configuration in doc/examples
%%% @end
%%%=============================================================================
-module(aeu_env_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Example configuration tests",
       [fun() ->
          ?assertMatch({ok, _}, aeu_env:check_config(Config))
        end || Config <- test_data_config_files()]
       }]
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
       filename:join([Dir, DataDir, "epoch_testnet.yaml"])].

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

-endif.
