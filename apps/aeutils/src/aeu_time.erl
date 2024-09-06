-module(aeu_time).

%% API
-export([now_in_msecs/0,
         now_in_secs/0,
         msecs_to_secs/1]).

now_in_msecs() ->
    os:system_time(millisecond).


now_in_secs() ->
    os:system_time(second).


msecs_to_secs(Msecs) ->
    Msecs div 1000.
