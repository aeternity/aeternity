-module(aec_eunit_SUITE).

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).
-export([application_test/1]).

-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, ct_eunit_xform}).

-define(STARTED_APPS_WHITELIST, [runtime_tools, parse_trans, folsom, bear, setup, hut]).
-define(TO_BE_STOPPED_APPS_BLACKLIST, []).
-define(REGISTERED_PROCS_WHITELIST,
        [cover_server, timer_server, %% by test framework
         inet_gethost_native_sup, inet_gethost_native, %% by inet
         prfTarg, runtime_tools_sup, %% by eper
         dets_sup, dets,  %% by mnesia
         folsom_sup, folsom_sample_slide_sup, folsom_metrics_histogram_ets, %% by folsom
         folsom_meter_timer_server, %% by folsom
         setup_sup, setup_srv %% by setup
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
    eunit:start(),
    Config.

end_per_suite(_Config) ->
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

    meck:new(aec_genesis_block_settings, []),
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, []),
    {ok,_} = application:ensure_all_started(aecore),
    timer:sleep(100),
    ok = application:stop(aecore),
    meck:unload(aec_genesis_block_settings),

    app_stop(StartedApps -- ?TO_BE_STOPPED_APPS_BLACKLIST, TempDir).

%% aec_sync_test(Config) ->
%%     application:set_env(jobs, queues,
%%                         [{sync_jobs, [passive]},
%%                          {sync_workers, [{regulators, [{counter, [{limit, 4}]}]},
%%                                          {producer, {aec_sync, sync_worker, []}}
%%                                         ]}]),
%%     {ok, StartedApps, TempDir} = prepare_app_start(aecore, Config),
%%     ct:log("jobs running with env: ~p", [application:get_env(jobs, queues)]),
%%     aec_test_utils:fake_start_aehttp(),

%%     ok = application:start(aecore),
%%     SyncPid = whereis(aec_sync),
%%     ct:log("Running aec_sync ~p", [SyncPid]),

%%     ok = aec_sync:connect_peer("http://my.evil_machine:3012/"),
%%     ct:log("trying to connect --> job scheduled"),
%%     %% give it some time to execute the scheduled job
%%     timer:sleep(50),
%%     false = aec_peers:is_blocked("http://my.evil_machine:3012/"),
%%     ct:log("as expected not blocked"),

%%     ok = aec_sync:connect_peer("http://my.evil_machine:3012/"),
%%     ct:log("trying to connect again --> job scheduled"),
%%     timer:sleep(50),
%%     SyncPid = whereis(aec_sync), %% Sync has not been restarted!
%%     Peers = aec_peers:connected_peers(),
%%     ct:log("tried twice and although it failed, it is a peer: ~p", [Peers]),
%%     1 = length(Peers),

%%     ok = application:stop(aecore),
%%     app_stop(StartedApps -- ?TO_BE_STOPPED_APPS_BLACKLIST, TempDir).


prepare_app_start(App, Config) ->
    try prepare_app_start_(App, Config)
    catch
        error:Reason ->
            error({Reason, erlang:get_stacktrace()})
    end.

prepare_app_start_(App, Config) ->
    application:load(App),
    TempDir = aec_test_utils:create_temp_key_dir(),
    application:set_env(aecore, keys_dir, TempDir),
    application:set_env(aecore, peer_password, <<"secret">>),
    application:set_env(aecore, beneficiary, aehttp_api_encoder:encode(account_pubkey, <<"_________my_public_key__________">>)),

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
