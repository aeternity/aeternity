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
        , check_calldata/2
        , function_type_info/3
        , function_type_hash/3
        , arg_typerep_from_function/2
        , type_hash_from_function_name/2
        , typereps_from_type_hash/2
        , function_name_from_type_hash/2
        ]).

-ifdef(TEST).
-export([ ast_to_erlang/1]).
-endif.

-type function_name() :: binary(). %% String
-type typerep() :: aeso_sophia:type().
-type function_type_info() :: { FunctionHash :: aec_hash:hash()
                              , FunctionName :: function_name()
                              , ArgType      :: aeso_sophia:heap() %% binary typerep
                              , OutType      :: aeso_sophia:heap() %% binary typerep
                              }.
-type type_info() :: [function_type_info()].

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-define(DEBUG_LOG(Format, Data), begin lager:debug(Format, Data), ?TEST_LOG(Format, Data) end).
-else.
-define(TEST_LOG(Format, Data), ok).
-define(DEBUG_LOG(Format, Data), lager:debug(Format, Data)).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Handle calldata

-spec create_calldata(binary(), string(), string()) ->
                             {ok, aeso_sophia:heap(), aeso_sophia:type()}
                                 | {error, argument_syntax_error}.

create_calldata(ContractCode, Function, Argument) ->
    case aeso_constants:string(Argument) of
        {ok, {tuple, _, _} = Tuple} ->
            encode_call(ContractCode, Function, Tuple);
        {ok, {unit, _} = Tuple} ->
            encode_call(ContractCode, Function, Tuple);
        {ok, ParsedArgument} ->
            %% The Sophia compiler does not parse a singleton tuple (42) as a tuple,
            %% Wrap it in a tuple.
            encode_call(ContractCode, Function, {tuple, [], [ParsedArgument]});
        {error, _} ->
            {error, argument_syntax_error}
    end.

%% Call takes one arument.
%% Use a tuple to pass multiple arguments.
encode_call(ContractCode, Function, ArgumentAst) ->
    Argument = ast_to_erlang(ArgumentAst),
    try aeso_compiler:deserialize(ContractCode) of
        #{type_info := TypeInfo} ->
            FunBin = list_to_binary(Function),
            case type_hash_from_function_name(FunBin, TypeInfo) of
                {ok, <<TypeHashInt:256>>} ->
                    Data = aeso_data:to_binary({TypeHashInt, Argument}),
                    case check_calldata(Data, TypeInfo) of
                        {ok, CallDataType} ->
                            {ok, Data, CallDataType};
                        {error,_What} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end
    catch _:_ -> {error, bad_contract_code}
    end.

-spec check_calldata(aeso_sophia:heap(), type_info()) ->
                        {'ok', typerep()} | {'error', atom()}.
check_calldata(CallData, TypeInfo) ->
    %% The first element of the CallData should be the function name
    case aeso_data:get_function_hash_from_calldata(CallData) of
        {ok, Hash} ->
            case typereps_from_type_hash(Hash, TypeInfo) of
                {ok, ArgType,_OutType} ->
                    ?TEST_LOG("Found ~p for ~p", [ArgType, Hash]),
                    try aeso_data:from_binary({tuple, [word, ArgType]}, CallData) of
                        {ok, _Something} ->
                            ?TEST_LOG("Whole call data: ~p\n", [_Something]),
                            {ok, {tuple, [word, ArgType]}};
                        {error, _} ->
                            {error, bad_call_data}
                    catch
                        _T:_E ->
                            ?TEST_LOG("Error parsing call data: ~p", [{_T, _E}]),
                            {error, bad_call_data}
                    end;
                {error, _} ->
                    ?TEST_LOG("Unknown function hash ~p", [Hash]),
                    {error, unknown_function}
            end;
        {error, _What} ->
            ?TEST_LOG("Bad call data ~p", [_What]),
            {error, bad_call_data}
    end.

%%%===================================================================
%%% Handle type info from contract meta data

-spec function_type_info(function_name(), [typerep()], typerep()) ->
                            function_type_info().
function_type_info(Name, Args, OutType) ->
    ArgType = {tuple, [T || {_, T} <- Args]},
    { function_type_hash(Name, ArgType, OutType)
    , Name
    , aeso_data:to_binary(ArgType)
    , aeso_data:to_binary(OutType)
    }.

-spec function_type_hash(function_name(), typerep(), typerep()) ->
                            aec_hash:hash().
function_type_hash(Name, ArgType, OutType) ->
    aec_hash:hash(sophia_type_hash,
                  iolist_to_binary([ Name
                                   , aeso_data:to_binary(ArgType)
                                   , aeso_data:to_binary(OutType)
                                   ])).


-spec arg_typerep_from_function(function_name(), type_info()) ->
           {'ok', typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
arg_typerep_from_function(Function, TypeInfo) ->
    case lists:keyfind(Function, 2, TypeInfo) of
        {_TypeHash, Function, ArgTypeBin,_OutTypeBin} ->
            case aeso_data:from_binary(typerep, ArgTypeBin) of
                {ok, ArgType} -> {ok, ArgType};
                {error,_} -> {error, bad_type_data}
            end;
        false ->
            {error, unknown_function}
    end.

-spec typereps_from_type_hash(aec_hash:hash(), type_info()) ->
           {'ok', typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
typereps_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash,_Function, ArgTypeBin, OutTypeBin} ->
            case {aeso_data:from_binary(typerep, ArgTypeBin),
                  aeso_data:from_binary(typerep, OutTypeBin)} of
                {{ok, ArgType}, {ok, OutType}} -> {ok, ArgType, OutType};
                {_, _} -> {error, bad_type_data}
            end;
        false ->
            {error, unknown_function}
    end.

-spec function_name_from_type_hash(aec_hash:hash(), type_info()) ->
                                          {'ok', function_name()}
                                        | {'error', 'unknown_function'}.
function_name_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash, Function,_ArgTypeBin,_OutTypeBin} ->
            {ok, Function};
        false ->
            {error, unknown_function}
    end.

-spec type_hash_from_function_name(function_name(), type_info()) ->
                                          {'ok', aec_hash:hash()}
                                        | {'error', 'unknown_function'}.
type_hash_from_function_name(Name, TypeInfo) ->
    case lists:keyfind(Name, 2, TypeInfo) of
        {TypeHash, Name,_ArgTypeBin,_OutTypeBin} ->
            {ok, TypeHash};
        false ->
            {error, unknown_function}
    end.

%%%===================================================================
%%% Test API
%%%===================================================================

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
