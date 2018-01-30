%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Ring contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_ring).

-export([ compile/2
        , create_call/3
        , execute_call/2
        ]).

-spec compile(binary(), binary()) -> {ok, binary()} | {error, binary()}.

compile(ContractAsBinString, OptionsAsBinString) ->
    ContractText = binary_to_list(ContractAsBinString),
    Options = parse_options(OptionsAsBinString),
    try aer_compiler:from_string(ContractText, Options) of
        %% TODO: Handle contract meta data.
        Code -> {ok, hexstring_encode(Code)}
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


hexstring_encode(Code) ->
    CodeAsHexString = 
        << << (hex_nibble(X)):8, (hex_nibble(Y)):8 >>
           || <<X:4, Y:4>> <= Code >>,
    <<"0x", CodeAsHexString/binary >>.
    
hex_nibble(X) ->
    if X < 10 -> X+$0;
       true   -> X+87
    end.

-spec create_call(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
create_call(Contract, Function, Argument) ->
    Res = aer_abi:create_calldata(Contract,
                                  binary_to_list(Function),
                                  binary_to_list(Argument)),
    case Res of
        {error, Error} ->
            {error, list_to_binary(io_lib:format("~p", [Error]))};
        _ -> Res
    end.

-spec execute_call(map(), boolean()) -> {ok, {binary(), binary(), string()}} | {error, term()}.
execute_call(#{ code := CodeAsHexBinString
              , address := Address
              , caller := Caller
              , data := CallData
              , gas := Gas
              , gasPrice := GasPrice
              , origin := Origin
              , value := Value
              , currentCoinbase := CoinBase
              , currentDifficulty := Diffculty
              , currentGasLimit := GasLimit
              , currentNumber := Number
              , currentTimestamp := TS
              }, Trace) ->
    %% TODO: Handle Contract In State.
    Code = binint_to_bin(CodeAsHexBinString),
    Spec =
        #{ exec => #{ code => Code
                    , address => Address
                    , caller => Caller
                    , data => CallData
                    , gas => Gas
                    , gasPrice => GasPrice
                    , origin => Origin
                    , value => Value
                    },
           env => #{ currentCoinbase => CoinBase
                   , currentDifficulty => Diffculty
                   , currentGasLimit => GasLimit
                   , currentNumber => Number
                   , currentTimestamp => TS
                   },
           pre => #{}},
    TraceSpec =
        #{ trace_fun =>
               fun(S,A) -> io_lib:format(S,A) end
         , trace => Trace
         },
    State = aevm_eeevm_state:init(Spec, TraceSpec),
    Result = aevm_eeevm:eval(State),
    %% TODO: Handle Contract Out State.
    case Result of
        #{ out := Res
         , trace := ExecutionTrace} ->
            {ok, {Res, <<>>, ExecutionTrace}};
        E ->
            {error, E}
    end.



binint_to_bin(<<"0x", Bin/binary>>) ->
    << <<(hex_to_int(X)):4>> || <<X:8>> <= Bin>>;
binint_to_bin(<<"0", _/binary>> = Bin) ->
    %% Don't know what to do.
    %% Is this an attempt to pad?
    error({unexpected, Bin});
binint_to_bin(Bin) when is_binary(Bin) ->
    Int = binary_to_integer(Bin),
    binary:encode_unsigned(Int).

hex_to_int(X) when $A =< X, X =< $F -> 10 + X - $A;
hex_to_int(X) when $a =< X, X =< $f -> 10 + X - $a;
hex_to_int(X) when $0 =< X, X =< $9 -> X - $0.
