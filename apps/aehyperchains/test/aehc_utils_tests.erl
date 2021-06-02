%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Tests for aehc_utils
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_utils_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

is_hc_enabled_test_() ->
    aehc_test_utils:with_mocked_fork_settings(
      [ aec_test_utils:eunit_with_consensus(aehc_test_utils:hc_from_genesis(),
          [{ "Test consensus eunit helper - HC consensus"
           , fun() ->
                  ?assertEqual(true, aehc_utils:hc_enabled())
             end
           }]),
        aec_test_utils:eunit_with_consensus(aehc_test_utils:cuckoo_pow_from_genesis(),
          [{ "Test consensus eunit helper - POW Cuckoo"
           , fun() ->
                  ?assertEqual(false, aehc_utils:hc_enabled())
             end
           }]),
        aec_test_utils:eunit_with_consensus(
            #{ <<"0">>    => aehc_test_utils:hc_consensus()
             , <<"1337">> => aehc_test_utils:cuckoo_pow_consensus()
             },
          [{ "Test consensus eunit helper - Mixed"
           , fun() ->
                  ?assertEqual(true, aehc_utils:hc_enabled())
             end
           }]),
        aec_test_utils:eunit_with_consensus(
            #{ <<"0">>    => aehc_test_utils:cuckoo_pow_consensus()
             , <<"1337">> => aehc_test_utils:hc_consensus()
             },
          [{ "Test consensus eunit helper - Mixed"
           , fun() ->
                  ?assertEqual(true, aehc_utils:hc_enabled())
             end
           }])
     ]).
