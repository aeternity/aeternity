%%%-------------------------------------------------------------------
%%% @author Robert Virding
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Linter for Varna.
%%% @end
%%% Created : 2018-05-01
%%%
%%%-------------------------------------------------------------------

-module(aeva_lint).

-export([contract/1]).

-record(lint, {contract=[],                     %Contract name
               state=[],                        %State definition.
               funcs=[],                        %Defined functions
               errors=[],                       %Error
               warnings={}                      %Warnings
              }).

%% contract(Contract, FileName) -> {ok,Warnings} | {error,Errors,Warnings}.

contract(_Form) ->
    {ok,[]}.
