-module(aesc_app).
-behavior(application).

-export([
          start/2
        , start_phase/3
        , prep_stop/1
        , stop/1
        ]).

%% aelog is a dependency of this application so logging is guaranteed
%% to be setup.
start(_StartType, _StartArgs) ->
    aesc_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.
