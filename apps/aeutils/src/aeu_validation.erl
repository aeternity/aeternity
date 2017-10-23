-module(aeu_validation).

-export([run/2]).

run([], _Arg) ->
    ok;
run([F | Rest], Arg) ->
    case F(Arg) of
        ok ->
            run(Rest, Arg);
        {error, _Reason} = Error ->
            Error
    end.
