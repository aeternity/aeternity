-module(aehc_utils).
-export([ hc_enabled/0,
          hc_install/0,
          hc_uninstall/0
]).

-spec hc_enabled() -> boolean().
hc_enabled() ->
    case application:get_env(aehyperchains, enabled) of
        {ok, true} ->
            true;
        _ ->
            false
    end.

-spec hc_install() -> ok | no_return().
hc_install() ->
    ensure_early_boot(),
    aec_plugin:register(#{aec_fork_block_settings => aehc_fork_block_settings}),
    ok.

-spec hc_uninstall() -> ok | no_return().
hc_uninstall() ->
    ensure_early_boot(),
    aec_plugin:register(#{}),
    ok.

-spec ensure_early_boot() -> ok | no_return().
ensure_early_boot() ->
    case proplists:is_defined(aecore, application:which_applications()) of
        true ->
            lager:error("Enabling/Disabling HC in a running node will cause data corruption!"),
            erlang:error(forbidden);
        false ->
            ok
    end.
