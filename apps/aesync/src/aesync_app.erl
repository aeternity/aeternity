%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aesync_app).

-behaviour(application).

-export([ start/2
        , start_phase/3
        , prep_stop/1
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    aec_connection_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.
