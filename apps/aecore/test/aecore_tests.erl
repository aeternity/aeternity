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
  TempDir = aec_test_utils:create_temp_key_dir(),
  application:set_env(aecore, keys_dir, TempDir),
  application:set_env(aecore, password, <<"secret">>),
  {ok, Deps} = application:get_key(App, applications), 
  AlreadyRunning = [ Name || {Name, _,_} <- application:which_applications() ],
  [ ?assertEqual(ok, application:ensure_started(Dep)) || Dep <- Deps ],
  ?assertEqual(ok, application:start(App)),
  ok = application:stop(App),
  aec_test_utils:remove_temp_key_dir(TempDir),
  [ ok = application:stop(D) || D <- lists:reverse(Deps -- AlreadyRunning) ],
  ok.
