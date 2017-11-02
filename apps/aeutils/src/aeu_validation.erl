-module(aeu_validation).

-export([run/2]).

run([], _Args) ->
    ok;
run([F | Rest], Args) ->
    case apply(F, Args) of
        ok ->
            run(Rest, Args);
        {error, _Reason} = Error ->
            Error
    end.
