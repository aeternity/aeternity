-module(aehttp_parent_connector_tests).

%% Test interactions with parent chains using chain simulators for AE and BTC nodes

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

ae_sim_test_() ->
    {foreach,
     fun() ->
            Apps = [gproc, lager, crypto, enacl, cowboy, inets],
            lists:foreach(fun(App) ->
                            {ok, _} = application:ensure_all_started(App)
                          end, Apps),
             aec_test_utils:mock_genesis_and_forks(),
             ParentHosts = ae_parent_http_specs(),
             ParentSims = start_ae_parent_sims(ParentHosts),
             {ok, Connector} = aec_parent_connector:start_link(aehttpc_aeternity, on_demand, ParentHosts),
             {ok, Connector, ParentSims}
     end,
     fun({ok, Connector, ParentSims}) ->
             aec_parent_connector:stop(Connector),
             stop_ae_parent_sims(ParentSims),
             aec_test_utils:unmock_genesis_and_forks(),
             ok = application:stop(inets),
             ok = application:stop(enacl),
             ok = application:stop(crypto),
             ok = application:stop(lager),
             ok = application:stop(gproc),
             ok = application:stop(cowboy),
             ok
     end,
     [fun({ok, Connector, _ParentSims}) ->
        [{"Basic http api operational to each child chain",
            fun() ->
                    aec_parent_connector:trigger_fetch(),
                    ?assertEqual(xx, xx),
                    ok
            end}]
    end]
    }.

 ae_parent_http_specs() ->
    lists:map(fun(Index) ->
                #{host => <<"127.0.0.1">>,
                  port => 3013 + Index,
                  user => "test",
                  password => "Pass"
                 } end, lists:seq(0, 0)).

start_ae_parent_sims(ParentHosts) ->
    lists:map(fun(#{port := Port}) ->
                Name = list_to_atom("ae_sim_" ++ integer_to_list(Port)),
                InitialState = #{accounts => []},
                {ok, Pid} = aehttp_ae_sim:start_link(Name, Port, InitialState),
                Pid
              end, ParentHosts).

stop_ae_parent_sims(ParentSims) ->
    lists:foreach(fun(SimPid) -> aehttp_ae_sim:stop(SimPid) end, ParentSims).