%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Entrypoint for stand-alone FATE Virtual Machine
%%% @end
%%%-------------------------------------------------------------------
-module(aefate).
-export([main/1]).


-define(OPT_SPEC,
    [ {obj_file, undefined, undefined, string, "Fate bytecode file"}
    , {call, undefined, undefined, string, "Call to execute: function(arguments)"}
    , {verbose, $v, "verbose", undefined, "Show code and environment"}
    , {help, $h, "help", undefined, "Show this message"}
    ]).

%%%===================================================================
%%% API
%%%===================================================================

main(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            case proplists:get_value(help, Opts, false) of
                false ->
                    run(Opts);
                true ->
                    usage()
            end;

        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

usage() ->
    getopt:usage(?OPT_SPEC, "aefate").

run(Opts) ->
    case proplists:get_value(obj_file, Opts, undefined) of
        undefined ->
            io:format("Error: no input source file\n\n"),
            usage();
        File ->
            load_file(File, Opts)
    end.

load_file(FileName, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, false),
    case proplists:get_value(call, Opts, undefined) of
        undefined ->
            io:format("Error: no call\n\n"),
            usage();
        Call ->
            {ok, File} = file:read_file(FileName),
            Code = aeb_fate_code:deserialize(File),
            SerializedCall = aeb_fate_asm:function_call(Call),
            What = #{ contract => FileName
                    , call => SerializedCall
                    , allow_init => true},
            Chain = #{ contracts =>
                           #{ FileName => Code}},
            case aefa_fate:run(What, Chain) of
                {ok, Env} ->
                    print_after_run(Verbose, Code, Chain, Env),
                    io:format("~0p~n", [aefa_engine_state:accumulator(Env)]);
                {error, Error, Env} ->
                    print_after_run(Verbose, Code, Chain, Env),
                    io:format("~p~n", [Error])
            end
    end.

print_after_run(true, Code, Chain, Env) ->
    io:format("Code: ~0p~n", [Code]),
    io:format("Chain: ~0p~n", [Chain]),
    io:format("Env: ~0p~n", [Env]).
