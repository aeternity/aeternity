%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Encode and decode data and function calls according to
%%%     Sophia-AEVM-ABI.
%%% @end
%%% Created : 25 Jan 2018
%%%
%%%-------------------------------------------------------------------
-module(aeso_abi).
-define(HASH_SIZE, 32).

-export([create_calldata/3]).

-spec create_calldata(binary(), string(), string()) -> binary() | {error, argument_syntax_error}.

create_calldata(Contract, Function, Argument) ->
    %% TODO: check that function exists in cotract.
    FunctionHandle = encode_function(Contract, Function),
    case aeso_constants:string(Argument) of
        {ok, ParsedArgument} ->
            encode_call(FunctionHandle, ParsedArgument);
        {error, _} ->
            {error, argument_syntax_error}
    end.

%% Use a tuple to pass multiple arguments. TODO: impossible to pass a single
%% tuple argument.
encode_call(FunctionHandle, {tuple, _, Elems}) ->
    encode_call1(FunctionHandle, Elems);
encode_call(FunctionHandle, Arg) ->
    encode_call1(FunctionHandle, [Arg]).

encode_call1(FunctionHandle, ArgumentAsts) ->
    Arguments = lists:map(fun ast_to_erlang/1, ArgumentAsts),
    Call = list_to_tuple([FunctionHandle | Arguments]),
    Data = aeso_data:to_binary(Call),
    _ArgumentTypes = lists:map(fun get_type/1, Arguments),
    %% TODO: Verify that the type matches the function signature.
    Data.

ast_to_erlang({int, _, N}) -> N;
ast_to_erlang({string, _, Bin}) -> Bin;
ast_to_erlang({unit, _}) -> {};
ast_to_erlang({con, _, "None"}) -> none;
ast_to_erlang({app, _, {con, _, "Some"}, [A]}) -> {some, ast_to_erlang(A)};
ast_to_erlang({tuple, _, Elems}) ->
    list_to_tuple(lists:map(fun ast_to_erlang/1, Elems));
ast_to_erlang({list, _, Elems}) ->
    lists:map(fun ast_to_erlang/1, Elems);
ast_to_erlang({map, _, Elems}) ->
    maps:from_list([ {ast_to_erlang(element(1, Elem)), ast_to_erlang(element(2, Elem))}
                        || Elem <- Elems ]).

encode_function(_Contract, Function) ->
     << <<X>> || X <- Function>>.

%% TODO: Handle all sophia data types.
get_type(I) when is_integer(I) -> int;
get_type(none) -> nil;
get_type({some, X}) -> {option, get_type(X)};
get_type(T) when is_tuple(T) ->
    ListOfTypes = [get_type(E) || E <- tuple_to_list(T)],
    {tuple, list_to_tuple(ListOfTypes)};
get_type(B) when is_binary(B) ->
    string;
get_type([]) -> nil;
get_type([E|_]) -> {list, get_type(E)};
get_type(#{}) -> empty_map;
get_type(M) when is_map(M) ->
    [{K, V} | _] = maps:to_list(M),
    {map, get_type(K), get_type(V)}.





