-module(aecore_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         start_phase/3,
         prep_stop/1,
         stop/1]).
-export([set_app_ctrl_mode/0]).
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = lager:info("Starting aecore node"),
    ok = aec_jobs_queues:start(),
    ok = application:ensure_started(mnesia),
    aecore_sup:start_link().

start_phase(create_metrics_probes, _StartType, _PhaseArgs) ->
    lager:debug("start_phase(create_metrics_probes, _, _)", []),
    aec_metrics:create_metrics_probes();
start_phase(start_reporters, _StartType, _PhaseArgs) ->
    lager:debug("start_phase(start_reporters, _, _)", []),
    aec_metrics_rpt_dest:check_config(),
    aec_metrics:start_reporters().

prep_stop(State) ->
    aec_block_generator:prep_stop(),
    aec_metrics:prep_stop(State),
    aec_upnp:prep_stop().

stop(_State) ->
    lager:info("Stopping aecore app", []),
    ok.

set_app_ctrl_mode() ->
    MMode = ok(aeu_env:find_config([<<"system">>, <<"maintenance_mode">>],
                                   [user_config, schema_default])),
    DevMode = is_dev_mode_consensus(),
    Mode = case {MMode, DevMode} of
               {true , _}     -> maintenance;
               {false, true}  -> dev_mode;
               {false, false} -> normal
           end,
    %% This setting will take effect once `app_ctrl` leaves protected mode
    lager:info("Set app_ctrl mode: ~p", [Mode]),
    app_ctrl_config:set_current_mode(Mode).

ok({ok, Value}) ->
    Value.

%% TODO: This should probably be part of some lower-level API
is_dev_mode_consensus() ->
    TopHeight = case aec_chain:top_height() of
                    undefined -> 0;
                    H -> H
                end,
    aec_consensus_on_demand == aec_consensus:get_consensus_module_at_height(TopHeight).
