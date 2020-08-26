%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Tests for aehc_utils
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_utils_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

install_uninstall_only_in_early_boot_test_() ->
    {foreach,
     fun() ->
             InitialApps = {running_apps(), loaded_apps()},
             ok = lager:start(),
             meck:new(application, [unstick, passthrough]),
             InitialApps
     end,
     fun({OldRunningApps, OldLoadedApps}) ->
             meck:unload(application),
             ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
end,
     [{"HC installs/uninstalls only in early boot",
       fun() ->
            meck:expect(application, which_applications, 0, []),
            ?assertEqual(ok, aehc_utils:hc_install()),
            ?assertEqual(ok, aehc_utils:hc_uninstall()),

            meck:expect(application, which_applications, 0, [{aecore, "Blockchain for aeapps", 20}]),
            ?assertException(error, forbidden, aehc_utils:hc_install()),
            ?assertException(error, forbidden, aehc_utils:hc_uninstall()),

            ok
       end}
     ]}.
