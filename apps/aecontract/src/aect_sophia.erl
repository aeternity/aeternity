%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Sophia contracts and data.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_sophia).

-include("aecontract.hrl").
-include_lib("apps/aecore/include/blocks.hrl").

-export([ compile/2
        , create_call/3
        , decode_data/2
        , encode_call_data/3
        , simple_call/3
        , on_chain_call/3
        ]).

-spec compile(binary(), binary()) -> {ok, binary()} | {error, binary()}.

compile(ContractAsBinString, OptionsAsBinString) ->
    ContractText = binary_to_list(ContractAsBinString),
    Options = parse_options(OptionsAsBinString),
    %% TODO: Handle contract meta data.
    try {ok, aeso_compiler:from_string(ContractText, Options)}
    catch error:Error ->
            {error, list_to_binary(io_lib:format("~p", [Error]))}
    end.

parse_options(OptionsBinString) ->
    parse_options(OptionsBinString, []).

parse_options(<<" ", Rest/binary>>, Acc) ->
    parse_options(Rest, Acc);
parse_options(<<"pp_sophia_code", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_sophia_code | Acc]);
parse_options(<<"pp_ast", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_ast | Acc]);
parse_options(<<"pp_icode", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_icode | Acc]);
parse_options(<<"pp_assembler", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_assembler | Acc]);
parse_options(<<"pp_bytecode", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_bytecode | Acc]);
parse_options(<<_:8, Rest/binary>>, Acc) ->
    %% TODO: give nice error instead of just ignoring stray chars.
    parse_options(Rest, Acc);
parse_options(<<>>, Acc) -> Acc.

-spec on_chain_call(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
on_chain_call(ContractKey, Function, Argument) ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_contract),
    ContractsTree  = aec_trees:contracts(Trees),
    Contract       = aect_state_tree:get_contract(ContractKey, ContractsTree),
    SerializedCode = aect_contracts:code(Contract),
    Store          = aect_contracts:state(Contract),
    %% TODO: Check the type info before calling?
    #{ byte_code := Code} = aeso_compiler:deserialize(SerializedCode),
    case create_call(Code, Function, Argument) of
        {error, E} -> {error, E};
        CallData ->
            VMVersion   = ?AEVM_01_Sophia_01,
            aect_evm:call_common(CallData, ContractKey, Code, Store,
                                 TxEnv, Trees, VMVersion)
    end.

-spec simple_call(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
simple_call(SerializedCode, Function, Argument) ->
    %% TODO: Check the type info before calling?
    #{ byte_code := Code} = aeso_compiler:deserialize(SerializedCode),
    case create_call(Code, Function, Argument) of
        {error, E} -> {error, E};
        CallData ->
            aect_evm:simple_call_common(Code, CallData, ?AEVM_01_Sophia_01)
    end.

-spec encode_call_data(binary(), binary(), binary()) ->
                              {ok, binary()} | {error, binary()}.
encode_call_data(Contract, Function, Argument) ->
    try create_call(Contract, Function, Argument) of
        Data when is_binary(Data) ->
            {ok, Data};
        Error -> Error
    catch _:_ -> {error, <<"bad argument">>}
    end.


decode_data(Type, Data) ->
    case get_type(Type) of
        {ok, SophiaType} ->
            try aeso_data:from_binary(SophiaType, Data) of
                {ok, Term} ->
                    try prepare_for_json(SophiaType, Term) of
                        R -> {ok, R}
                    catch throw:R -> R
                    end;
                {error, _} -> {error, <<"bad type/data">>}
            catch _T:_E ->    {error, <<"bad argument">>}
            end;
        {error, _} = E -> E
    end.

get_type(BinaryString) ->
    String = unicode:characters_to_list(BinaryString, utf8),
    case aeso_data:sophia_type_to_typerep(String) of
        {ok, _Type} = R -> R;
        {error, ErrorAtom} ->
            {error, unicode:characters_to_binary(atom_to_list(ErrorAtom))}
    end.


prepare_for_json(word, Integer) when is_integer(Integer) ->
    #{ <<"type">> => <<"word">>,
       <<"value">> => Integer};
prepare_for_json(string, String) when is_binary(String) ->
    #{ <<"type">> => <<"string">>,
       <<"value">> => String};
prepare_for_json({option, _T}, none) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => <<"None">>};
prepare_for_json({option, T}, {some, E}) ->
    #{ <<"type">> => <<"option">>,
       <<"value">> => prepare_for_json(T,E) };
prepare_for_json({tuple, Ts}, Es) ->
    #{ <<"type">> => <<"tuple">>,
       <<"value">> => [prepare_for_json(T,E)
                       || {T,E} <-
                              lists:zip(Ts, tuple_to_list(Es))] };
prepare_for_json({list, T}, Es) ->
    #{ <<"type">> => <<"list">>,
       <<"value">> => [prepare_for_json(T,E) || E <- Es]};
prepare_for_json(T = {variant, Cons}, R = {variant, Tag, Args}) when is_integer(Tag), Tag < length(Cons) ->
    Ts = lists:nth(Tag + 1, Cons),
    case length(Ts) == length(Args) of
        true ->
            #{ <<"type">> => <<"variant">>
             , <<"value">> => [Tag | [prepare_for_json(ArgT, Arg)
                                      || {ArgT, Arg} <- lists:zip(Ts, Args)]] };
        false ->
            String = io_lib:format("Type: ~p Res:~p", [T,R]),
            Error = << <<B>> || B <- "Invalid Sophia type: " ++ lists:flatten(String) >>,
            throw({error, Error})
    end;
prepare_for_json({map, KeyT, ValT}, Map) when is_map(Map) ->
    #{ <<"type">> => <<"map">>,
       <<"value">> => [ #{ <<"key">> => prepare_for_json(KeyT, K),
                           <<"val">> => prepare_for_json(ValT, V) }
                        || {K, V} <- maps:to_list(Map) ] };
prepare_for_json(T, R) ->
    String = io_lib:format("Type: ~p Res:~p", [T,R]),
    Error = << <<B>> || B <- "Invalid Sophia type: " ++ lists:flatten(String) >>,
    throw({error, Error}).

-spec create_call(binary(), binary(), binary()) -> binary() | {error, binary()}.
create_call(Contract, Function, Argument) ->
    Res = aeso_abi:create_calldata(Contract,
                                   binary_to_list(Function),
                                   binary_to_list(Argument)),
    case Res of
        {error, Error} ->
            {error, list_to_binary(io_lib:format("~p", [Error]))};
        _ -> Res
    end.
