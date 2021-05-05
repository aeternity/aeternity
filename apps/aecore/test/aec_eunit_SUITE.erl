-module(aec_eunit_SUITE).

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).
-export([application_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").
-compile({parse_transform, ct_eunit_xform}).

-define(STARTED_APPS_WHITELIST,
        [ runtime_tools
        , parse_trans
        , folsom
        , bear
        , setup
        , hut
        , hex2bin
        , inet_cidr
        , inet_ext
        , lhttpc
        , rand_compat
        , xmerl
        , aecuckoo
        , aecuckooprebuilt
        , getopt
        , eblake2
        , ecrecover
        , ecrecoverprebuilt
        , emcl
        ]).
-define(TO_BE_STOPPED_APPS_BLACKLIST, []).
-define(REGISTERED_PROCS_WHITELIST,
        [cover_server, timer_server, %% by test framework
         inet_gethost_native_sup, inet_gethost_native, %% by inet
         prfTarg, runtime_tools_sup, %% by eper
         dets_sup, dets,  %% by mnesia
         folsom_sup, folsom_sample_slide_sup, folsom_metrics_histogram_ets, %% by folsom
         folsom_meter_timer_server, %% by folsom
         setup_sup, setup_srv, %% by setup
         lhttpc_sup, lhttpc_manager %% by lhttpc
        ]).

-ifdef(EUNIT_INCLUDED).
all() -> [ {group, eunit}, application_test ]. %%, aec_sync_test ].
-else.
all() -> [ application_test ]. %%, aec_sync_test ].
-endif.

groups() ->
    [].

suite() ->
    [].

init_per_suite(Config) ->
    aec_test_utils:ensure_no_mocks(),
    eunit:start(),
    Config.

end_per_suite(_Config) ->
    aec_test_utils:ensure_no_mocks(),
    ok.

init_per_group(_Grp, Config) ->
    Config.

end_per_group(_Grp, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    lager_common_test_backend:bounce(error),
    Apps = application:which_applications(),
    Names = registered(),
    [{running_apps, Apps},
     {regnames, Names}|Config].

end_per_testcase(_TC, Config) ->
    Apps0 = ?config(running_apps, Config),
    Names0 = ?config(regnames, Config),
    Apps = [ A || A = {ATag, _, _} <- application:which_applications(),
                  not lists:member(ATag, ?STARTED_APPS_WHITELIST) ],
    Names = registered() -- ?REGISTERED_PROCS_WHITELIST,
    case {(Apps -- Apps0), Names -- Names0, lager_common_test_backend:get_logs()} of
        {[], [], []} ->
            ok;
        {_, _, Logs} when Logs =/= []->
            {fail, {errors_in_lager_log, lists:map(fun iolist_to_s/1, Logs)}};
        {NewApps, _, _} when NewApps =/= [] ->
            %% New applications take precedence over new registered processes
            {fail, {started_applications, NewApps}};
        {_, NewReg, _} ->
            await_registered(NewReg, Names0)
    end.

await_registered(Rest, Names0) ->
    receive after 100 ->
                    await_registered(9, Rest, Names0)
            end.

await_registered(N, _, Names0) when N > 0 ->
    case (registered() -- Names0) -- ?REGISTERED_PROCS_WHITELIST of
        [] ->
            ok;
        [_|_] = NewReg ->
            receive after 100 ->
                            await_registered(N-1, NewReg, Names0)
                    end
    end;
await_registered(_, NewReg, _Names0) ->
    {fail, {registered_processes, NewReg}}.

-spec iolist_to_s(iolist()) -> string().
iolist_to_s(L) ->
    lists:flatten(io_lib:format("~s~n", [L])).

%% We must be able to start an application when all applications it
%% depends upon are started.
application_test(Config) ->
    {ok, StartedApps, TempDir} = prepare_app_start(aecore, Config),

    meck:new(aec_fork_block_settings, []),
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, []),
    meck:expect(aec_fork_block_settings, minerva_accounts, 0, []),
    meck:expect(aec_fork_block_settings, fortuna_accounts, 0, []),
    meck:expect(aec_fork_block_settings, lima_accounts, 0, []),
    meck:expect(aec_fork_block_settings, lima_extra_accounts, 0, []),
    meck:expect(aec_fork_block_settings, lima_contracts, 0, []),
    meck:expect(aec_fork_block_settings, block_whitelist, 0, #{}),
    meck:expect(aec_fork_block_settings, pre_iris_map_ordering, 0, #{}),
    meck:new(aeu_info, []),
    meck:expect(aeu_info, block_info, 0, 591),
    {ok,_} = application:ensure_all_started(aecore),
    timer:sleep(100),
    ok = application:stop(aecore),
    meck:unload(aec_fork_block_settings),
    meck:unload(aeu_info),

    app_stop(StartedApps -- ?TO_BE_STOPPED_APPS_BLACKLIST, TempDir).

prepare_app_start(App, Config) ->
    try prepare_app_start_(App, Config)
    ?_catch_(error, Reason, StackTrace)
        error({Reason, StackTrace})
    end.

prepare_app_start_(App, Config) ->
    application:load(App),
    TempDir = aec_test_utils:create_temp_key_dir(),
    application:set_env(aecore, keys_dir, TempDir),
    application:set_env(aecore, peer_password, <<"secret">>),
    application:set_env(aecore, beneficiary, aeser_api_encoder:encode(account_pubkey, <<"_________my_public_key__________">>)),

    {ok, Deps0} = application:get_key(App, applications),
    Deps = maybe_add_mnesia(App, Deps0), % mnesia is started manually in aecore_app
    AlreadyRunning = [ Name || {Name, _,_} <- proplists:get_value(running_apps, Config) ],
    [ application:ensure_started(Dep) || Dep <- Deps ],
    {ok, lists:reverse(Deps -- AlreadyRunning), TempDir}.

app_stop(Apps, TempDir) ->
    aec_test_utils:remove_temp_key_dir(TempDir),
    [ application:stop(App) || App <- Apps ],
    ok.

maybe_add_mnesia(App, Deps) ->
    case lists:member(aecore, [App|Deps]) of
        true  -> Deps ++ [mnesia];
        false -> Deps
    end.
