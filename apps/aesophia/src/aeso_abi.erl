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

-export([ create_calldata/3
        ]).

-ifdef(TEST).
-export([ ast_to_erlang/1]).
-endif.

-spec create_calldata(binary(), string(), string()) ->
                             {ok, aeso_sophia:heap(), aeso_sophia:type()}
                                 | {error, argument_syntax_error}.

create_calldata(ContractCode, Function, Argument) ->
    FunctionHandle = encode_function(Function),
    case aeso_constants:string(Argument) of
        {ok, {tuple, _, _} = Tuple} ->
            encode_call(ContractCode, FunctionHandle, Tuple);
        {ok, {unit, _} = Tuple} ->
            encode_call(ContractCode, FunctionHandle, Tuple);
        {ok, ParsedArgument} ->
            %% The Sophia compiler does not parse a singleton tuple (42) as a tuple,
            %% Wrap it in a tuple.
            encode_call(ContractCode, FunctionHandle, {tuple, [], [ParsedArgument]});
        {error, _} ->
            {error, argument_syntax_error}
    end.

%% Call takes one arument.
%% Use a tuple to pass multiple arguments.
encode_call(ContractCode, FunctionHandle, ArgumentAst) ->
    Argument = ast_to_erlang(ArgumentAst),
    Data = aeso_data:to_binary(list_to_tuple([FunctionHandle, Argument])),
    try aeso_compiler:deserialize(ContractCode) of
        #{type_info := TypeInfo} ->
            case aect_dispatch:check_call_data(Data, TypeInfo) of
                {ok, CallDataType} -> {ok, Data, CallDataType};
                {error,_What} = Err -> Err
            end
    catch _:_ -> {error, bad_contract_code}
    end.

ast_to_erlang({int, _, N}) -> N;
ast_to_erlang({bool, _, true}) -> 1;
ast_to_erlang({bool, _, false}) -> 0;
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

encode_function(Function) ->
     << <<X>> || X <- Function>>.

