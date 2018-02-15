%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Ring contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_evm).

-export([ call/2
	, encode_call_data/3
	, execute_call/2
        ]).



-spec encode_call_data(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
encode_call_data(_Contract, Function, Argument) ->
    %% TODO: Check that Function exists in Contract.
    {ok, <<Function/binary, Argument/binary>>}.

-spec call(binary(), binary()) -> {ok, binary()} | {error, binary()}.
call(Code, CallData) ->
    Data = aeu_hex:hexstring_decode(CallData),

    Spec = #{ code => Code
            , address => 0
            , caller => 0
            , data => Data
            , gas => 1000000000000000000000000
            , gasPrice => 1
            , origin => 0
            , value => 0
            , currentCoinbase => 1
            , currentDifficulty => 1
            , currentGasLimit => 10000000000000000000000
            , currentNumber => 1
            , currentTimestamp => 1
            },
    try execute_call(Spec, true) of
        #{ out := Out } -> {ok, aeu_hex:hexstring_encode(Out)};
        E -> {error, list_to_binary(io_lib:format("~p", [E]))}
    catch _T:E ->
	{error, list_to_binary(io_lib:format("~p", [E]))}
    end.


-spec execute_call(map(), boolean()) -> map() | {error, term()}.
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
    Code = aeu_hex:hexstring_decode(CodeAsHexBinString),
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
    Result.

