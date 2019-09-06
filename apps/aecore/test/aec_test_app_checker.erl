-module(aec_test_app_checker).

-export([ start_link/1
        , init/1
        ]).

start_link(Parent) ->
    {ok, proc_lib:spawn_link(fun() -> init(Parent) end)}.

init(Parent) ->
    io:fwrite(user, "app checker init.~n", []),
    check_and_report(Parent, []).

check_and_report(Parent, KnownApps) ->
    Now = os:timestamp(),
    NewApps = check_new_apps(KnownApps),
    [Parent ! {application_started, Now, A} || A <- NewApps],
    timer:sleep(100),
    check_and_report(Parent, NewApps ++ KnownApps).

check_new_apps(KnownApps) ->
    [A || {A,_,_} <- application:which_applications(), not lists:member(A, KnownApps)].
