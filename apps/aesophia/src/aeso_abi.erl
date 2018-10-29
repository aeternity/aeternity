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
                             {ok, aeso_sophia:heap(), aeso_sophia:type(), aeso_sophia:type()}
                                 | {error, argument_syntax_error}.
create_calldata(ContractCode, "", CallCode) ->
    case aeso_compiler:check_call(CallCode, []) of
        {ok, FunName, {ArgTypes, RetType}, Args} ->
            try aeso_compiler:deserialize(ContractCode) of
                #{type_info := TypeInfo} ->
                    FunBin = list_to_binary(FunName),
                    case type_hash_from_function_name(FunBin, TypeInfo) of
                        {ok, <<TypeHashInt:256>>} ->
                            Data = aeso_data:to_binary({TypeHashInt, list_to_tuple(Args)}),
                            case check_calldata(Data, TypeInfo) of
                                {ok, CallDataType, OutType} ->
                                    case check_given_type(FunName, ArgTypes, RetType, CallDataType, OutType) of
                                        ok ->
                                            {ok, Data, CallDataType, OutType};
                                        {error, _} = Err ->
                                            Err
                                    end;
                                {error,_What} = Err -> Err
                            end
                    end
            catch _:_ -> {error, bad_contract_code}
            end;
        {error, _} = Err -> Err
    end;
create_calldata(Contract, Function, Argument) ->
    %% Slightly hacky shortcut to let you get away without writing the full
    %% call contract code.
    %% Function should be "foo : type", and
    %% Argument should be "Arg1, Arg2, .., ArgN" (no parens)
    FunName = hd(string:lexemes(Function, ": ")),
    Args    = lists:map(fun($\n) -> 32; (X) -> X end, Argument),    %% newline to space
    CallContract = lists:flatten(
        [ "contract Call =\n"
        , "  function ", Function, "\n"
        , "  function __call() = ", FunName, "(", Args, ")"
        ]),
    create_calldata(Contract, "", CallContract).

%% Check that the given type matches the type from the metadata.
check_given_type(FunName, GivenArgs, GivenRet, CalldataType, ExpectRet) ->
    {tuple, [word, {tuple, ExpectArgs}]} = CalldataType,
    ReturnOk = if FunName == "init" -> true;
                  GivenRet == any   -> true;
                  true              -> GivenRet == ExpectRet
               end,
    ArgsOk   = ExpectArgs == GivenArgs,
    case ReturnOk andalso ArgsOk of
        true -> ok;
        false when FunName == "init" ->
            {error, {init_args_mismatch,
                        {given,    GivenArgs},
                        {expected, ExpectArgs}}};
        false ->
            {error, {call_type_mismatch,
                        {given,    GivenArgs,  '=>', GivenRet},
                        {expected, ExpectArgs, '=>', ExpectRet}}}
    end.

-spec check_calldata(aeso_sophia:heap(), type_info()) ->
                        {'ok', typerep()} | {'error', atom()}.
check_calldata(CallData, TypeInfo) ->
    %% The first element of the CallData should be the function name
    case aeso_data:get_function_hash_from_calldata(CallData) of
        {ok, Hash} ->
            case typereps_from_type_hash(Hash, TypeInfo) of
                {ok, ArgType, OutType} ->
                    ?TEST_LOG("Found ~p for ~p", [ArgType, Hash]),
                    try aeso_data:from_binary({tuple, [word, ArgType]}, CallData) of
                        {ok, _Something} ->
                            ?TEST_LOG("Whole call data: ~p\n", [_Something]),
                            {ok, {tuple, [word, ArgType]}, OutType};
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

