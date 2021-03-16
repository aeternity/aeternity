-module(aeutils_app).
-behaviour(application).

-export([ start/2
        , start_phase/3
        , prep_stop/1
        , stop/1 ]).

start(_StartType, _StartArgs) ->
    aeutils_sup:start_link().

start_phase(_Phase, _Type, _Args) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.
