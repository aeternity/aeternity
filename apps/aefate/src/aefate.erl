%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Entrypoint for stand-alone FATE Virtual Machine
%%% @end
%%%-------------------------------------------------------------------
-module(aefate).
-export([main/1]).

-include_lib("apps/aecontract/include/aecontract.hrl").

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
            {contract_bytearray, CodeArr} = aeser_api_encoder:decode(File),
            #{byte_code := Code} = aeser_contract_code:deserialize(CodeArr),
            SerializedCall = aeb_fate_asm:function_call(Call),
            Spec = #{ contract => <<0:256>>
                    , code => Code
                    , vm_version => ?VM_FATE_SOPHIA_2
                    , call => SerializedCall
                    , allow_init => true
                    , value => 0 % TODO: make a flag
                    , gas => 1000000000000000000000000 % TODO: make a flag
                    , store => aefa_stores:new()
                   },
            Env = #{ contracts =>
                           #{ FileName => Code}
                  , gas_price => 1 % TODO: make a flag
                  , fee => 621
                  , trees => aec_trees:new_without_backend()
                  , caller => <<0:256>>
                  , origin => <<0:256>>
                  , tx_env => aetx_env:tx_env(1)
                  },
            case aefa_fate:run(Spec, Env) of
                {ok, ES} ->
                    print_after_run(Verbose, Code, Env, ES),
                    io:format("~0p~n", [aefa_engine_state:accumulator(ES)]);
                {error, Error, ES} ->
                    print_after_run(Verbose, Code, Env, ES),
                    io:format("~p~n", [Error])
            end
    end.

print_after_run(true, Code, Chain, Env) ->
    io:format("Code: ~0p~n", [Code]),
    io:format("Chain: ~0p~n", [Chain]),
    io:format("Env: ~0p~n", [Env]);
print_after_run(false, _, _, _) ->
    ok.
