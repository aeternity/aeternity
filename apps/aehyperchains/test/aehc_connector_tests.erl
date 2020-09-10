%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Tests for aehc_connector
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_connector_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

hyperchains_simulator_test_() ->
    {foreach,
        fun() ->
            meck:new(aehc_app, [passthrough]),
            meck:expect(aehc_app, get_connector_id, 0, <<"aehc_chain_sim_connector">>),

            application:ensure_started(gproc),
            ok = application:ensure_started(crypto),

            aec_test_utils:mock_genesis_and_forks(),
            Dir = aec_test_utils:aec_keys_setup(),
            aehc_chain_sim_connector:start_link(),
            Dir
        end,
        fun(TmpDir) ->
            aec_test_utils:aec_keys_cleanup(TmpDir),
            aec_test_utils:unmock_genesis_and_forks()
        end,
        [{"Sent payload == Requested payload",
            fun() ->
                %
                Payload = <<"test">>,
                %
                ?assertEqual(ok, aehc_connector:send_tx(Payload)),
                ok
            end}
        ]}.
