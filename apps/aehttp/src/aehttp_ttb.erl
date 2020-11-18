-module(aehttp_ttb).
-behavior(tr_ttb).

-export([ on_nodes/2
        , stop/0
        , stop_nofetch/0
        , format/2
        , format/3 ]).

-export([ patterns/0
        , flags/0 ]).

-export([event/1]).

-include_lib("trace_runner/include/trace_runner.hrl").

%% This function is also traced. Can be used to insert markers in the trace log.
event(E) ->
    event(?LINE, E, none).

event(_, _, _) ->
    ok.

on_nodes(Ns, File) ->
    tr_ttb:on_nodes(Ns, File, ?MODULE).

patterns() ->
    sc_ws_api:patterns()
        ++ aehttp_dispatch_int:patterns()
        ++ tr_ttb:default_patterns().

flags() ->
    {all, call}.

stop() ->
    tr_ttb:stop().

stop_nofetch() ->
    tr_ttb:stop_nofetch().

format(Dir, Out) ->
    tr_ttb:format(Dir, Out).

format(Dir, Out, Opts) ->
    tr_ttb:format(Dir, Out, Opts).
