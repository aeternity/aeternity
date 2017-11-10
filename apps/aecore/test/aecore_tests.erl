-module(aecore_tests).

-include_lib("eunit/include/eunit.hrl").

%% This test will find out whether there are missing dependencies.
%%
%% We must be able to start an application when all applications it
%% depends upon are started.
application_test() ->
  App = aecore,
  application:load(App),
  application:stop(App),
  application:set_env(aecore, password, "Thisisweird"),
  {ok, Deps} = application:get_key(App, applications), 
  AlreadyRunning = [ Name || {Name, _,_} <- application:which_applications() ],
  [ ?assertEqual(ok, application:ensure_started(Dep)) || Dep <- Deps ],
  ?assertEqual(ok, application:start(App)),
  ok = application:stop(App),
  [ ok = application:stop(D) || D <- lists:reverse(Deps -- AlreadyRunning) ],
  ok.


