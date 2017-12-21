%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Assembler for aevm machine code
%%% @end
%%% Created : 21 Dec 2017
%%%-------------------------------------------------------------------

-module(aeb_asm).

-export([ pp/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").


pp(Asm) ->
    Listing = format(Asm),
    io:format("~p~n", [Listing]).

format(Asm) ->
    Asm.
