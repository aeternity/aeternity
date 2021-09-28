-module(aecore_env).

-export([ check_env/0
        , is_dev_mode/0
        ]).

-export([patron_keypair_for_testing/0]).

%% Checking user-provided configs. The logic is somewhat complicated
%% by the fact that 'setup' is not guaranteed to start before lager,
%% so we have to be prepared to apply changes to both the lager env
%% and the (possibly) running lager. (This problem is solvable, but not
%% trivially. Basically, the aeternity.rel file must be pre-sorted and passed
%% to relx.
%%
check_env() ->
    expand_lager_log_root(),
    check_env([{[<<"logging">>, <<"hwm">>]     , fun set_hwm/1},
               {[<<"logging">>, <<"level">>]   , fun set_level/1},
               {[<<"mining">>, <<"autostart">>], fun set_autostart/2},
               {[<<"mining">>, <<"strictly_follow_top">>], {set_env, strictly_follow_top}},
               {[<<"mining">>, <<"attempt_timeout">>], {set_env, mining_attempt_timeout}},
               {[<<"chain">>, <<"persist">>]   , {set_env, persist}},
               {[<<"chain">>, <<"db_path">>]   , fun set_mnesia_dir/1}]).

is_dev_mode() ->
    case aeu_env:user_config([<<"system">>, <<"dev_mode">>]) of
        {ok, Bool} ->
            Bool;
        undefined ->
            case aeu_env:user_config([<<"fork_management">>, <<"network_id">>]) of
                {ok, <<"ae_dev">>} ->
                    true;
                _ ->
                    false
            end
    end.

%% See https://github.com/erlang-lager/lager/issues/557
%% There are different ways to ensure that the log root is a stable
%% absolute path (once initialized). This could also be done using
%% sys.config.src (e.g. `{log_root, "${ROOTDIR}/log"}`, but at the time
%% of writing, `rebar3 ct` doesn't support `sys_config_src`, and it gets messy
%% to support both types.
%%
%% This setup hook code will likely run after lager has started, but it still
%% fixes the log root for later. One possible scenario might be that someone
%% pecks around in an aeternity node shell, changes the current working directory
%% and forgets to restore it. This would effectively move the logical log_root
%% in lager, if the setting is a relative path (which is the default).
%%
expand_lager_log_root() ->
    case application:get_env(lager, log_root) of
        {ok, Root} when is_list(Root) ->
            case filename:pathtype(Root) of
                relative ->
                    application:set_env(lager, log_root,
                                        filename:absname(Root));
                absolute ->
                    ok
            end;
        _ ->
            application:set_env(lager, log_root, setup:log_dir())
    end.

check_env(Spec) ->
    {ok, MMode} = aeu_env:find_config([<<"system">>, <<"maintenance_mode">>],
                                      [user_config, schema_default]),
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> set_env(F, V, MMode)
              end
      end, Spec).

set_env({set_env, K}, V, _MMode) when is_atom(K) ->
    io:fwrite("setenv K=~p, V=~p~n", [K, V]),
    application:set_env(aecore, K, V);
set_env(F, V, _MMode) when is_function(F, 1) ->
    F(V);
set_env(F, V, MMode) when is_function(F, 2) ->
    F(V, MMode).

set_autostart(V, MMode) ->
    application:set_env(aecore, autostart, V andalso (not MMode)).

set_mnesia_dir(Path) ->
    MnesiaDir = filename:join(binary_to_list(Path), "mnesia"),
    ok = filelib:ensure_dir(MnesiaDir),
    application:set_env(mnesia, dir, MnesiaDir).

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
            if_running(lager, fun() -> live_set_level(Level) end);
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


%% This key pair corresponds to the pubkey
%% ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi
%% defined in data/aecore/.genesis/accounts_test.json
%%
%% The key pair used to be hard-coded in aecore_suite_utils, but in order to
%% make it usable also in dev_mode, it is moved here.
%%
patron_keypair_for_testing() ->
    #{ pubkey  => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,
                    73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
       privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,
                    197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,
                    167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,
                    187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>
     }.
