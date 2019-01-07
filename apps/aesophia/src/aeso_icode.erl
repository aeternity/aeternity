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

-export([new/1, pp/1, set_name/2, set_functions/2, map_typerep/2, option_typerep/1, get_constructor_tag/2]).
-export_type([icode/0]).

-include("aeso_icode.hrl").

-type type_def() :: fun(([aeso_sophia:type()]) -> aeso_sophia:type()).

-type bindings() :: any().
-type fun_dec() :: { string()
                   , [modifier()]
                   , arg_list()
                   , expr()
                   , aeso_sophia:type()}.

-type modifier() :: private | stateful.

-type type_name() :: string() | [string()].

-type icode() :: #{ contract_name => string()
                  , functions => [fun_dec()]
                  , env => [bindings()]
                  , state_type => aeso_sophia:type()
                  , event_type => aeso_sophia:type()
                  , types => #{ type_name() => type_def() }
                  , type_vars => #{ string() => aeso_sophia:type() }
                  , constructors => #{ string() => integer() }  %% name to tag
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
       %% Default to unit type for state and event
     , state_type => {tuple, []}
     , event_type => {tuple, []}
     , types => builtin_types()
     , type_vars => #{}
     , constructors => builtin_constructors()
     , options => Options}.

builtin_types() ->
    Word = fun([]) -> word end,
    #{ "bool"         => Word
     , "int"          => Word
     , "string"       => fun([]) -> string end
     , "address"      => Word
     , "hash"         => Word
     , "signature"    => fun([]) -> {tuple, [word, word]} end
     , "oracle"       => fun([_, _]) -> word end
     , "oracle_query" => fun([_, _]) -> word end
     , "list"         => fun([A]) -> {list, A} end
     , "option"       => fun([A]) -> {variant, [[], [A]]} end
     , "map"          => fun([K, V]) -> map_typerep(K, V) end
     , ["Chain", "ttl"] => fun([]) -> {variant, [[word], [word]]} end
     }.

builtin_constructors() ->
    #{ "RelativeTTL" => 0
     , "FixedTTL"    => 1
     , "None"        => 0
     , "Some"        => 1 }.

map_typerep(K, V) ->
    {map, K, V}.

option_typerep(A) ->
    {variant, [[], [A]]}.

new_env() ->
    [].

-spec set_name(string(), icode()) -> icode().
set_name(Name, Icode) ->
    maps:put(contract_name, Name, Icode).

-spec set_functions([fun_dec()], icode()) -> icode().
set_functions(NewFuns, Icode) ->
    maps:put(functions, NewFuns, Icode).

-spec get_constructor_tag(string(), icode()) -> integer().
get_constructor_tag(Name, #{constructors := Constructors}) ->
    case maps:get(Name, Constructors, undefined) of
        undefined -> error({undefined_constructor, Name});
        Tag       -> Tag
    end.

