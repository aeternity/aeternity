%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the logging setup hook
%%% @end
%%%=============================================================================
-module(aeu_logging_env_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% adjust_log_levels/0 starts lager, so every hook after it can log. It cannot
%% run twice in a VM, so pin the ordering here instead; aec_governance_tests
%% pins the aecore hooks above these.
setup_hook_test_() ->
    {"only the config hooks, which report through error_logger, precede lager",
     fun() ->
             [Phase] = [P || {P, {aeu_logging_env, adjust_log_levels, []}}
                                 <- normal_setup_hooks(aeutils)],
             Earlier = [MFA || {P, MFA} <- normal_setup_hooks(aeutils), P < Phase],
             ?assertEqual(lists:sort([{aeu_env, read_config, []},
                                      {aeu_env, apply_os_env, []}]),
                          lists:sort(Earlier))
     end}.

%% Read from the .app file: nothing overrides '$setup_hooks' from a sys.config.
normal_setup_hooks(App) ->
    {ok, [{application, App, Props}]} =
        file:consult(code:where_is_file(atom_to_list(App) ++ ".app")),
    {env, Env} = lists:keyfind(env, 1, Props),
    {'$setup_hooks', Hooks} = lists:keyfind('$setup_hooks', 1, Env),
    {normal, Normal} = lists:keyfind(normal, 1, Hooks),
    Normal.

-endif.
