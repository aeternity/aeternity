-module(aer_constants).

-export([string/1]).

string(Str) ->
    case aer_parser:string("let _ = " ++ Str) of
        {ok, [{letval, _, _, _, E}]} -> {ok, E};
        {ok, Other}                  -> error({internal_error, should_be_letval, Other});
        Err                          -> Err
    end.
