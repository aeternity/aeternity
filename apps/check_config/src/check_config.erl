-module(check_config).
-export([main/1]).

main([File, Schema]) ->
    check_config(File, Schema);
main([File]) ->
    Schema = filename:join(
               filename:dirname(
                 filename:dirname(escript:script_name())),
               "data/epoch_config_schema.json"),
    check_config(File, Schema);
main(_) ->
    usage().

check_config(Cfg, Schema) ->
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    case aeu_env:check_config(Cfg, Schema) of
        {error, Reason} ->
            io:fwrite("Configuration error (~p)~n", [Reason]),
            halt(1);
        {ok, _} ->
            io:fwrite("OK~n")
    end.

usage() ->
    io:format("Usage: check_config file [schema]~n", []),
    halt(1).
