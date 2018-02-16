%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Ring contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_ring).

-export([ compile/2
        , create_call/3
        , encode_call_data/3
        , simple_call/3
        ]).

-spec compile(binary(), binary()) -> {ok, binary()} | {error, binary()}.

compile(ContractAsBinString, OptionsAsBinString) ->
    ContractText = binary_to_list(ContractAsBinString),
    Options = parse_options(OptionsAsBinString),
    try aer_compiler:from_string(ContractText, Options) of
        %% TODO: Handle contract meta data.
        Code -> {ok, aeu_hex:hexstring_encode(Code)}
    catch error:Error ->
            {error, list_to_binary(io_lib:format("~p", [Error]))}
    end.

parse_options(OptionsBinString) ->
    parse_options(OptionsBinString, []).

parse_options(<<" ", Rest/binary>>, Acc) ->
    parse_options(Rest, Acc);
parse_options(<<"pp_ring_code", Rest/binary>>, Acc) ->
    parse_options(Rest, [pp_ring_code | Acc]);
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



-spec simple_call(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
simple_call(Code, Function, Argument) ->
    case create_call(Code, Function, Argument) of
        {error, E} -> {error, E};
        CallData ->
            Spec = #{ code => Code
                    , address => 0
                    , caller => 0
                    , data => CallData
                    , gas => 1000000
                    , gasPrice => 1
                    , origin => 0
                    , value => 0
                    , currentCoinbase => 1
                    , currentDifficulty => 1
                    , currentGasLimit => 1000000
                    , currentNumber => 1
                    , currentTimestamp => 1
                    },
            case aect_evm:execute_call(Spec, false) of
                #{ out := Out } ->
                    {ok, aeu_hex:hexstring_encode(Out)};
                E -> {error, list_to_binary(io_lib:format("~p", [E]))}
            end
    end.

-spec encode_call_data(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
encode_call_data(Contract, Function, Argument) ->
    try create_call(Contract, Function, Argument) of
        Data when is_binary(Data) ->
            {ok, aeu_hex:hexstring_encode(Data)};
        Error -> Error
    catch _:_ -> {error, <<"bad argument">>}
    end.

-spec create_call(binary(), binary(), binary()) -> binary() | {error, binary()}.
create_call(Contract, Function, Argument) ->
    Res = aer_abi:create_calldata(Contract,
                                  binary_to_list(Function),
                                  binary_to_list(Argument)),
    case Res of
        {error, Error} ->
            {error, list_to_binary(io_lib:format("~p", [Error]))};
        _ -> Res
    end.

