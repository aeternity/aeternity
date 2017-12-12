-module(aecore_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         start_phase/3,
         stop/1]).
-export([check_env/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = lager:info("Starting aecore node"),
    aecore_sup:start_link().

start_phase(start_reporters, _StartType, _PhaseArgs) ->
    aec_metrics:start_reporters().

stop(_State) ->
    ok.

%% Checking user-provided configs. The logic is somewhat complicated
%% by the fact that 'setup' is not guaranteed to start before lager,
%% so we have to be prepared to apply changes to both the lager env
%% and the (possibly) running lager. (This problem is solvable, but not
%% trivially. Basically, the epoch.rel file must be pre-sorted and passed
%% to relx.
check_env() ->
    check_env([{[<<"logging">>, <<"hwm">>]     , fun set_hwm/1},
               {[<<"mining">>, <<"autostart">>], {set_env, autostart}},
               {[<<"mining">>, <<"attempt_timeout">>], {set_env, mining_attempt_timeout}},
               {[<<"chain">>, <<"persist">>]   , {set_env, persist}},
               {[<<"chain">>, <<"db_path">>]   , {set_env, db_path}}]).

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

set_hwm(HWM) when is_integer(HWM) ->
    application:set_env(lager, error_logger_hwm, HWM),
    if_running(lager, fun() -> live_set_hwm(HWM) end).

live_set_hwm(Hwm) ->
    lists:map(
      fun(Sink) ->
              [lager:set_loghwm(Sink, lager_console_backend, Hwm)
               || {lager_console_backend,_}
                      <- gen_event:which_handlers(Sink)]
      end, lager:list_all_sinks()).

if_running(App, F) ->
    case lists:keymember(App, 1, application:which_applications()) of
        true  ->
            F();
        false -> ok
    end.
