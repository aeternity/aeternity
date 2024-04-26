-module(aeu_logging_env).

-export([adjust_log_levels/0]).

adjust_log_levels() ->
    case init:get_argument(noinput) of
        {ok, _} -> set_lager_console_only();  % if running in foreground mode
        _ -> ok
    end,
    set_lager_log_root(),
    aeu_env:check_env(
      aeutils,
      [{[<<"logging">>, <<"hwm">>]     , fun set_hwm/1},
       {[<<"logging">>, <<"level">>]   , fun set_level/1}]).

%% Rewrite the lager configuration for running in a diskless environment.
%% Use persistent flag to ensure that the settings are not reset by reload
%% of the app config at any later point.
set_lager_console_only() ->
    application:load(lager),  % make sure lager app env has been loaded
    application:set_env(lager, crash_log, false, [{persistent, true}]),
    Handlers = application:get_env(lager, handlers, []),
    application:set_env(lager, handlers, handlers_to_console(Handlers),
                        [{persistent, true}]),
    Sinks = application:get_env(lager, extra_sinks, []),
    NewSinks = lists:map(
                 fun({K, Opts}) ->
                         {_, Hs} = lists:keyfind(handlers, 1, Opts),
                         NewHs = handlers_to_console(Hs),
                         {K, lists:keystore(handlers, 1, Opts, {handlers, NewHs})}
                 end, Sinks),
    application:set_env(lager, extra_sinks, NewSinks, [{persistent, true}]),
    Traces = application:get_env(lager, traces, []),
    NewTraces = lists:map(
                  fun({{lager_file_backend, _}, F, L}) ->
                          {lager_console_backend, F, L};
                     ({{lager_file_backend, _}, F}) ->
                          {lager_console_backend, F};
                     (T) ->
                          T
                  end, Traces),
    application:set_env(lager, traces, NewTraces, [{persistent, true}]).

%% Replace file backends with console backend at the same log level. Drop
%% any previous console backend.
handlers_to_console(Handlers) ->
    lists:filtermap(fun ({lager_file_backend, Opts}) ->
                            {true, {lager_console_backend,
                                    lists:filter(fun ({level,_}) -> true;
                                                     (_) -> false
                                                 end,
                                                 Opts)}};
                        ({lager_console_backend, _}) -> false;
                        (_) -> true
                    end,
                    Handlers).

%% See https://github.com/erlang-lager/lager/issues/557
%% We want to prevent the log directory from moving during runtime. One
%% possible scenario might be that someone pecks around in an aeternity
%% node shell, changes the current working directory and forgets to restore
%% it. If the log_root setting in lager is a relative path (which it is by
%% default), this would effectively move the log root.
%% There are different ways to ensure that the log root is a stable
%% absolute path (once initialized). This could also be done using
%% sys.config.src (e.g. `{log_root, "${ROOTDIR}/log"}`, but at the time
%% of writing, `rebar3 ct` doesn't support `sys_config_src`, and it gets messy
%% to support both types.
%%
%% The setup app must be started before lager, so that this setup hook runs
%% before lager tries to create the log root directory, and thus only
%% setup's log_dir setting will determine the log directory location.

set_lager_log_root() ->
    %% We ignore lager's current log root setting - if it was loaded from
    %% the boot script, it will simply be the default value from the
    %% lager.app.src file. Instead, setup will decide the log directory,
    %% and we make sure that the path is configured to an absolute path
    %% after this point. (If setups log_dir env was not set before, the
    %% call to setup:log_dir() will set it to $HOME/log.<nodename>.)
    case is_app_running(lager) of
        true -> exit(lager_started_before_setup);
        _ -> ok
    end,
    Root = setup:log_dir(),
    case is_tuple(Root) of
        true -> exit(invalid_setup_log_dir);
        _ -> ok
    end,
    AbsRoot = case filename:pathtype(Root) of
                  relative -> filename:absname(Root);
                  absolute -> Root
              end,
    %% also ensure that the setting is not reset by load/reload of the apps
    ok = application:set_env(lager, log_root, AbsRoot, [{persistent, true}]),
    ok = application:set_env(setup, log_dir, AbsRoot, [{persistent, true}]).

set_hwm(HWM) when is_integer(HWM) ->
    application:set_env(lager, error_logger_hwm, HWM),
    if_running(lager, fun() -> live_set_hwm(HWM) end).

live_set_hwm(Hwm) ->
    error_logger_lager_h:set_high_water(Hwm),
    %% Not setting high water mark for console backend as such backend
    %% has not any such configuration.  Compare [console
    %% backend](https://github.com/erlang-lager/lager/blob/3.6.7/src/lager_file_backend.erl#L146)
    %% with [file
    %% backend](https://github.com/erlang-lager/lager/blob/3.6.7/src/lager_console_backend.erl#L171).
    [lager:set_loghwm(lager_event, H, Hwm)
     || H <- gen_event:which_handlers(lager_event),
        element(1, H) =:= lager_file_backend].

set_level(L) when is_binary(L) ->
    Level = binary_to_existing_atom(L, latin1),
    case lists:member(Level, levels()) of
        true ->
            lager_set_level_env(Level),
            logger:set_primary_config(level, Level),
            if_running(lager, fun() -> live_set_level(Level) end),
            set_app_log_level(app_ctrl, Level),
            set_app_log_level(mnesia, Level),
            set_app_log_level(exometer_core, Level);
        false ->
            lager:error("Unknown log level: ~p", [Level]),
            ignore
    end.

lager_set_level_env(L) ->
    Handlers = application:get_env(lager, handlers, []),
    application:set_env(lager, handlers, set_level_opt(L, Handlers)),

    Sinks = application:get_env(lager, extra_sinks, []),
    NewSinks = lists:map(
                 fun({K, Opts}) ->
                         {_, Hs} = lists:keyfind(handlers, 1, Opts),
                         NewHs = set_level_opt(L, Hs),
                         {K, lists:keystore(handlers, 1, Opts, {handlers, NewHs})}
                 end, Sinks),
    application:set_env(lager, extra_sinks, NewSinks).

set_level_opt(Level, Handlers) ->
    [{H, lists:keystore(level, 1, Opts, {level, Level})} || {H, Opts} <- Handlers].

live_set_level(L) ->
    lager:set_loglevel(lager_console_backend, L),
    lager:set_loglevel({lager_file_backend, "aeternity.log"}, L),
    lager:set_loglevel(epoch_mining_lager_event,
                       {lager_file_backend, "aeternity_mining.log"},
                       undefined, L),
    lager:set_loglevel(epoch_metrics_lager_event,
                       {lager_file_backend, "aeternity_metrics.log"},
                       undefined, L),
    lager:set_loglevel(aeminer_lager_event,
                       {lager_file_backend, "aeternity_pow_cuckoo.log"},
                       undefined, L),
    lager:set_loglevel(epoch_sync_lager_event,
                       lager_console_backend, undefined, L),
    lager:set_loglevel(epoch_sync_lager_event,
                       {lager_file_backend, "aeternity_sync.log"},
                       undefined, L),
    lager:set_loglevel(aestratum_lager_event,
                       {lager_file_backend, "aestratum.log"},
                       undefined, L).

levels() ->
    %% copied from lager.hrl
    [debug, info, notice, warning, error, critical, alert, emergency, none].

%% This is to enable silencing of apps that can be very chatty at debug level
set_app_log_level(mnesia, debug) ->
    case os:getenv("AE_MNESIA_TRACE") of
        "true" ->
            mnesia:set_debug_level(trace);
        _ ->
            case os:getenv("AE_MNESIA_DEBUG") of
                "true" -> mnesia:set_debug_level(debug);
                _ -> ok
            end
    end;
set_app_log_level(App, debug) ->
    Key = "AE_" ++ string:to_upper(atom_to_list(App)) ++ "_DEBUG",
    case os:getenv(Key, "false") of
        "true" ->
            ok;
        _ ->
            lager:debug("Setting ~p log level to info", [App]),
            logger:set_application_level(App, info)
    end;
set_app_log_level(_, _) ->
    ok.

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
