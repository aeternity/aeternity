%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Encode and decode data and function calls according to
%%%     Ring-AEVM-ABI.
%%% @end
%%% Created : 25 Jan 2018
%%%
%%%-------------------------------------------------------------------
-module(aer_abi).
-define(HASH_SIZE, 32).

-export([create_calldata/3]).

create_calldata(Contract, Function, Argument) ->
    %% TODO: check that function exists in cotract.
    FunctionHandle = encode_function(Contract, Function),
    Call = {FunctionHandle, Argument},
    {0, Data} = aer_data:to_binary(Call),
    _ArgumentType = get_type(Argument),
    %% TODO: Verify that the type matches the function signature.
    Data.

encode_function(_Contract, Function) ->
     << <<X>> || X <- Function>>.

%% TODO: Handle all ring data types.
get_type(I) when is_integer(I) -> int;
get_type(T) when is_tuple(T) ->
    ListOfTypes = [get_type(E) || E <- tuple_to_list(T)],
    {tuple, list_to_tuple(ListOfTypes)};
get_type(B) when is_binary(B) ->
    string;
get_type([]) -> nil;
get_type([E|_]) ->
    {list, get_type(E)}.
    




