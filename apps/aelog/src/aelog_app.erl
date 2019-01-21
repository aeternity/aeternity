-module(aelog_app).

-behaviour(application).

-export([check_env/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

-define(LOGGING_HWM_CFG_KEY, [<<"logging">>, <<"hwm">>]).
-define(LOGGING_LEVEL_CFG_KEY, [<<"logging">>, <<"level">>]).

-define(LOG_FILE_DIR, "log").
-define(LOG_FILE_PREFIX, "epoch").

-define(DEFAULT_LAGER_SINK, lager_event).
-define(LAGER_FILE_BACKEND, lager_file_backend).

-define( MINING_LAGER_SINK, epoch_mining_lager_event).
-define(    POW_LAGER_SINK, epoch_pow_cuckoo_lager_event).
-define(   SYNC_LAGER_SINK, epoch_sync_lager_event).
-define(METRICS_LAGER_SINK, epoch_metrics_lager_event).

-define(ALL_SINKS, [ ?DEFAULT_LAGER_SINK
                   ,  ?MINING_LAGER_SINK
                   ,     ?POW_LAGER_SINK
                   ,    ?SYNC_LAGER_SINK
                   , ?METRICS_LAGER_SINK
                   ]).

%%====================================================================
%% API
%%====================================================================

%% The user configuration is guaranteed to have been loaded from file
%% to the environment.  This is because the `setup` application runs
%% setup hooks sorted by phase number, and the configured phase number
%% for the hook loading the config is smaller than the phase number of
%% this hook.
%%
%% Checking user-provided configs. The logic is somewhat complicated
%% by the fact that 'setup' is not guaranteed to start before lager,
%% so we have to be prepared to apply changes to both the lager env
%% and the (possibly) running lager. (This problem is solvable, but not
%% trivially. Basically, the aeternity.rel file must be pre-sorted and passed
%% to relx.
%% Run as setup hook. At this stage, lager is setup with console only - no files.
check_env() ->
    check_env([{?LOGGING_HWM_CFG_KEY, fun set_hwm/1},
               {?LOGGING_LEVEL_CFG_KEY, fun check_level/1}]).

start(_StartType, _StartArgs) ->
    %% Lager is a dependency of this application so lager is
    %% guaranteed to be running.
    %%
    %% All sinks are configured thanks to lager having read its
    %% portion of system config.  Though only console backends are
    %% started - no file backends yet.
    %%
    %% User configuration (containing items relevant for logging on
    %% file) is loaded and lager is started: lager file handlers can
    %% now be started.
    ok = start_lager_file_handlers(),
    %% All lager backends are started - both console and file.
    ok = lager:info("Logging setup completed"),
    aelog_sup:start_link().


stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

check_env(Spec) ->
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> set_env(F, V)
              end
      end, Spec).

set_env(F, V) when is_function(F, 1) ->
    F(V).

set_hwm(HWM) when is_integer(HWM) ->
    application:set_env(lager, error_logger_hwm, HWM),
    if_running(lager, fun() -> startup_set_hwm(HWM) end).

%% Assumption: no file backends to be considered at this stage.
startup_set_hwm(Hwm) ->
    error_logger_lager_h:set_high_water(Hwm),
    %% Not setting high water mark for console backend as such backend
    %% has not any such configuration.  Compare [console
    %% backend](https://github.com/erlang-lager/lager/blob/3.6.7/src/lager_file_backend.erl#L146)
    %% with [file
    %% backend](https://github.com/erlang-lager/lager/blob/3.6.7/src/lager_console_backend.erl#L171).
    ok.

check_level(L) when is_binary(L) ->
    Level = binary_to_existing_atom(L, latin1),
    case lists:member(Level, levels()) of
        true ->
            %% Assumption: no file backends to be considered at this stage.
            ok;
        false ->
            lager:error("Unknown log level: ~p", [Level]),
            ignore
    end.

levels() ->
    %% copied from lager.hrl
    [debug, info, notice, warning, error, critical, alert, emergency, none].

if_running(App, F) ->
    case is_app_running(App) of
        true  ->
            F();
        false -> ok
    end.

%% This function guarantees the caller that the specified app has not
%% started - not that it has not begun starting.
%%
%% If the caller is synchronous with the [OTP release
%% boot](http://erlang.org/doc/man/init.html#boot-1) then this
%% function guarantees also that the specified app has not begun
%% starting.  This is thanks to the OTP release boot being
%% [sequential](https://github.com/erlang/otp/blob/OTP-20.3.8/erts/preloaded/src/init.erl#L787).
is_app_running(App) ->
    lists:keymember(App, 1, application:which_applications()).

start_lager_file_handlers() ->
    start_lager_file_handlers(?ALL_SINKS).

start_lager_file_handlers(Sinks) ->
    %% All sinks are configured.
    %%
    %% Hardcode expectations that base system config:
    %% * Has all configured sinks.
    %% * Has file handler for each sink.
    FileHandlersBaseSysCfg = lists:map(fun(S) -> {S, get_file_handler_for_sink(S)} end, Sinks),
    %% Build configuration of lager file handlers.
    FileHandlersCfg1 =
        case aeu_env:user_config(?LOGGING_HWM_CFG_KEY) of
            undefined -> FileHandlersBaseSysCfg;
            {ok, Hwm} ->
                lists:map(fun({S, SCfg}) -> {S, overridden_file_handler_config(SCfg, high_water_mark, Hwm)} end, FileHandlersBaseSysCfg)
        end,
    FileHandlersCfg2 =
        case aeu_env:user_config(?LOGGING_LEVEL_CFG_KEY) of
            undefined -> FileHandlersCfg1;
            {ok, Level} ->
                %% Level already validated by `set_level`.
                lists:map(fun({S, SCfg}) -> {S, case S of ?METRICS_LAGER_SINK -> SCfg; _ -> overridden_file_handler_config(SCfg, level, Level) end} end, FileHandlersCfg1)
        end,
    %% Guarantee that the file in each file handler is distinct, by using distinct name suffixes.
    FileHandlersCfg3 = lists:map(fun({S, SCfg}) -> {S, overridden_file_handler_config(SCfg, file, file_for_sink(S))} end, FileHandlersCfg2),
    lists:foreach(fun({S, SCfg}) -> lager_app:start_handler(S, ?LAGER_FILE_BACKEND, SCfg) end, FileHandlersCfg3),
    ok.

get_file_handler_for_sink(Sink) ->
    {ok, Cfg} = aeu_env:get_env(aecore, [lager_file_handlers, sinks, Sink, handlers, ?LAGER_FILE_BACKEND]),
    Cfg.

overridden_file_handler_config(BaseCfg, K, V) ->
    lists:keystore(K, 1, BaseCfg, {K, V}).

file_for_sink(S) ->
    filename:join(?LOG_FILE_DIR, file_for_sink(S, ?LOG_FILE_PREFIX) ++ ".log").

file_for_sink(?DEFAULT_LAGER_SINK, Prefix) ->
    Prefix;
file_for_sink(S, Prefix) ->
    Prefix ++ "_" ++ file_sink_suffix(S).

file_sink_suffix( ?MINING_LAGER_SINK) -> "mining";
file_sink_suffix(    ?POW_LAGER_SINK) -> "pow_cuckoo";
file_sink_suffix(   ?SYNC_LAGER_SINK) -> "sync";
file_sink_suffix(?METRICS_LAGER_SINK) -> "metrics".
