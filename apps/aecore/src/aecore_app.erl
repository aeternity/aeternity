-module(aecore_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         start_phase/3,
         prep_stop/1,
         stop/1]).
-export([check_env/0]).

%%====================================================================
%% API
%%====================================================================

%% aelog is a dependency of this application so logging is guaranteed
%% to be setup.
start(_StartType, _StartArgs) ->
    ok = lager:info("Starting aecore node"),
    ok = aec_jobs_queues:start(),
    ok = application:ensure_started(mnesia),
    aec_db:load_database(),
    case aec_db:persisted_valid_genesis_block() of
        true ->
            aecore_sup:start_link();
        false ->
            lager:error("Persisted chain has a different genesis block than "
                        ++ "the one being expected. Aborting", []),
            {error, inconsistent_database}
    end.

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
    ok.

%% The user configuration is guaranteed to have been loaded from file
%% to the environment.  This is because the `setup` application runs
%% setup hooks sorted by phase number, and the configured phase number
%% for the hook loading the config is smaller than the phase number of
%% this hook.
%%
%% Checking user-provided configs.
%%
%% Run as setup hook. At this stage, lager is setup with console only - no files.
check_env() ->
    check_env([{[<<"mining">>, <<"autostart">>], {set_env, autostart}},
               {[<<"mining">>, <<"attempt_timeout">>], {set_env, mining_attempt_timeout}},
               {[<<"chain">>, <<"persist">>]   , {set_env, persist}},
               {[<<"chain">>, <<"db_path">>]   , fun set_db_path/1}]).

check_env(Spec) ->
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> set_env(F, V)
              end
      end, Spec).

set_env({set_env, K}, V) when is_atom(K) ->
    io:fwrite("setenv K=~p, V=~p~n", [K, V]),
    application:set_env(aecore, K, V);
set_env(F, V) when is_function(F, 1) ->
    F(V).

set_db_path(Path) ->
    %% TODO: possibly support a new config variable for the mnesia directory,
    %% if we actually want to support keeping the two separate.
    MnesiaDir = filename:join(binary_to_list(Path), "mnesia"),
    ok = filelib:ensure_dir(MnesiaDir),
    application:set_env(mnesia, dir, MnesiaDir),
    application:set_env(aecore, db_path, Path).
