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

-export([new/1, pp/1, set_name/2, set_functions/2]).
-export_type([icode/0]).

-include("aeso_icode.hrl").

-type bindings() :: any().
-type fun_dec() :: { string()
                   , arg_list()
                   , expr()
                   , aeso_sophia:type()}.
-type icode() :: #{ contract_name => string()
                  , functions => [fun_dec()]
                  , env => [bindings()]
                  , state_type => aeso_sophia:type()
                  , options => [any()]
                  }.


pp(Icode) ->
    %% TODO: Actually do *Pretty* printing.
    io:format("~p~n", [Icode]).

-spec new([any()]) -> icode().
new(Options) ->
    #{ contract_name => ""
     , functions => []
     , env => new_env()
       %% Default to unit type for state
     , state_type => {tuple, []}
     , options => Options}.

new_env() ->
    [].

-spec set_name(string(), icode()) -> icode().
set_name(Name, Icode) ->
    maps:put(contract_name, Name, Icode).

-spec set_functions([fun_dec()], icode()) -> icode().
set_functions(NewFuns, Icode) ->
    maps:put(functions, NewFuns, Icode).
