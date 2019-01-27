-module(aestratum_utils).

-export([timestamp/0]).

%% Timestamp in milliseconds.
timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + erlang:trunc(MicroSecs / 1000).
