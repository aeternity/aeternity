-module(aefate).
-export([main/1]).


-define(OPT_SPEC,
    [ {obj_file, undefined, undefined, string, "Fate bytecode file"}
    , {call, undefined, undefined, string, "Call to execute: function(arguments)"}
    , {verbose, $v, "verbose", undefined, "Show code and environment"}
    , {help, $h, "help", undefined, "Show this message"}
    ]).

usage() ->
    getopt:usage(?OPT_SPEC, "aefate").

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
            Code = aeb_fate_asm:bytecode_to_fate_code(File, Opts), 
            SerializedCall = aeb_fate_asm:function_call(Call),
            What = #{ contract => FileName
                    , call => SerializedCall},
            #{functions := Functions} = Code,
            Chain = #{ contracts => 
                           #{ FileName => Functions}},
            Res = aefa_fate:run(What, Chain),
                
            case Verbose of 
                true ->
                    io:format("Code: ~0p~n", [Code]),
                    io:format("Chain: ~0p~n", [Chain]),
                    io:format("Env: ~0p~n", [Res]);
                false -> ok
            end,
            #{accumulator := Acc} = Res,
            io:format("~0p~n", [Acc])
    end.
    

