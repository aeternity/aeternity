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

-spec compile(binary(), aer_compiler:options()) -> binary().

compile(ContractBin, Options) ->
    ContractText = binary_to_list(ContractBin),

    Code = aer_compiler:from_string(ContractText, Options),
    Code.

-spec create_call(binary(), string(), string()) -> {ok, binary()} | {error, string()}.
create_call(Contract, Function, Argument) ->
    aer_abi:create_calldata(Contract, Function, Argument).

-spec execute_call(map(), boolean()) -> {ok, {binary(), binary(), string()}} | {error, term()}.
execute_call(#{ code := Code
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
