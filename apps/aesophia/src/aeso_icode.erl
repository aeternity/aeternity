%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Intermediate Code for Aeterinty Sophia language.
%%% @end
%%% Created : 21 Dec 2017
%%% 
%%%-------------------------------------------------------------------
-module(aeso_icode).

-export([pp/1]).

-include("aeso_icode.hrl").

pp(Icode) ->
    %% TODO: Actually do *Pretty* printing.
    io:format("~p~n", [Icode]).
