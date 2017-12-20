-module(aeu_validation).

-export([run/1, run/2]).

run(Checks) ->
    run(Checks, []).

run([], _Args) ->
    ok;
run([F | Rest], Args) ->
    case apply(F, Args) of
        ok ->
            run(Rest, Args);
        {error, _Reason} = Error ->
            Error
    end.
