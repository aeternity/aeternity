%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%      Memory speifics that compiler and VM need to agree upon
%%% @end
%%% Created : 19 Dec 2018
%%%-------------------------------------------------------------------

-module(aeso_memory).

-export([binary_to_words/1]).

binary_to_words(<<>>) ->
    [];
binary_to_words(<<N:256,Bin/binary>>) ->
    [N|binary_to_words(Bin)];
binary_to_words(Bin) ->
    binary_to_words(<<Bin/binary,0>>).

