-module(aeu_time).

%% API
-export([now_in_msecs/0,
         msecs_to_secs/1]).

now_in_msecs() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000.

msecs_to_secs(Msecs) ->
    Msecs div 1000.
