%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the example configuration in doc/examples
%%% @end
%%%=============================================================================
-module(aeu_example_config_tests).

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
  ["apps/aeutils/doc/examples/epoch_full.yaml",
    "apps/aeutils/doc/examples/epoch_testnet.yaml"].

setup() ->
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    ok.

teardown(_) ->
    application:stop(jesse),
    application:stop(yamerl),
    application:stop(jsx).

-endif.
