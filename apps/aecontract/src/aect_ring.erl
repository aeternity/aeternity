%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Ring contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_ring).

-export([ compile/2
        , create_call/3
        ]).

-spec compile(binary(), aer_compiler:options()) -> binary().

compile(ContractText, Options) ->
    Code = aer_compiler:from_string(ContractText, Options),
    Code.

-spec create_call(binary(), string(), string()) -> {ok, binary()} | {error, string()}.
create_call(Contract, Function, Argument) ->
    case aer_constans:string(Argument) of
        {ok, ParsedArgument} ->
            <<>>;
        {error, _} ->
            {error, argument_syntax_error}
    end.
