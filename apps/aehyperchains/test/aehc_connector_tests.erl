%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Tests for aeconnector
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_connector_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

-define(CONNECTOR, aehc_chain_sim_connector).

hyperchains_simulator_test_() ->
    {foreach,
        fun() ->
            ok = application:ensure_started(gproc),
            aec_test_utils:mock_genesis_and_forks(),
            Dir = aec_test_utils:aec_keys_setup(),
            GenesisState = aec_block_genesis:genesis_block_with_state(),
            aehc_chain_sim_connector:start_link(#{ <<"genesis_state">> => GenesisState }),
            Dir
        end,
        fun(TmpDir) ->
            ok = aehc_chain_sim_connector:stop(),
            aec_test_utils:aec_keys_cleanup(TmpDir),
            aec_test_utils:unmock_genesis_and_forks(),
            ok = application:stop(gproc)
        end,
        [{"Sent payload == Requested payload",
            fun() ->
                {ok, Pub} = aec_keys:pubkey(),
                SenderId = aeser_id:create(account, Pub),
                ?assertEqual(ok, aeconnector:send_tx(?CONNECTOR, SenderId, <<"Hyperchains trace">>)),
                ok
            end}
        ]}.
