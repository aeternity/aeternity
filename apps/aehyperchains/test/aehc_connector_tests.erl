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
             InitialApps = {running_apps(), loaded_apps()},
             meck:new(aehc_app, [passthrough]),
             meck:expect(aehc_app, get_connector_id, 0, <<"aehc_chain_sim_connector">>),

             meck:new(aecore_sup, [passthrough]),
             meck:expect(aecore_sup, start_link, 0, {ok, pid}),
             meck:new(aec_jobs_queues, [passthrough]),
             meck:expect(aec_jobs_queues, start, 0, ok),
             ok = lager:start(),
             InitialApps
     end,
     fun({OldRunningApps, OldLoadedApps}) ->
             ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
end,
     [{"Sent payload == Requested payload",
       fun() ->
            %
            aec_chain_sim:start(),
            Payload = <<"test">>,
            aehc_connector:send_tx(Payload),
            %
               io:format("~nTest is run ~p~n",[?MODULE]),
            ?assertEqual(ok, aehc_connector:send_tx(Payload)),
            ok
       end}
     ]}.
