%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   The purpose of this integration test suite is to test the application aehttp.
%%%
%%% @end
%%%=============================================================================
-module(aeu_requests_SUITE).

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2
        ]).
-export([application_test/1]).

-include_lib("common_test/include/ct.hrl").

-define(STARTED_APPS_WHITELIST, [{erlexec,"OS Process Manager","1.7.1"}]).
-define(REGISTERED_PROCS_WHITELIST,
        [cover_server, timer_server,
         exec_app, exec, inet_gethost_native_sup, inet_gethost_native]).

all() ->
    [ application_test ].

init_per_suite(Config) ->
    eunit:start(),
    Config.

end_per_suite(_Config) ->
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
    Apps = application:which_applications() -- ?STARTED_APPS_WHITELIST,
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
            {fail, {registered_processes, NewReg}}
    end.

-spec iolist_to_s(iolist()) -> string().
iolist_to_s(L) ->
    lists:flatten(io_lib:format("~s~n", [L])).

application_test(Config) ->
    App = aehttp,
    application:load(App),
    {ok, Deps} = application:get_key(App, applications),
    AlreadyRunning = [ Name || {Name, _, _} <- proplists:get_value(running_apps, Config) ],

    %% Start application it depends on (among which aecore)
    application:set_env(aecore, password, <<"secret">>), 

    {ok, Started} = application:ensure_all_started(aehttp),

    [ ok = application:stop(D) || D <- lists:reverse(Started -- AlreadyRunning) ],
    ok.
  
