%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% API functions for compiling and encoding Solidity contracts.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_evm).

-export([ simple_call_common/5
        , simple_call_common/3
        , call_common/9
        , encode_call_data/3
        , execute_call/2
        ]).

-include("aecontract.hrl").
-include_lib("apps/aecore/include/blocks.hrl").


-spec encode_call_data(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()}.
encode_call_data(_Contract, Function, Argument) ->
    %% TODO: Check that Function exists in Contract.
    {ok, <<Function/binary, Argument/binary>>}.

-spec simple_call_common(binary(), binary(), VMVersion :: integer()) ->
                            {ok, binary()} | {error, binary()}.
simple_call_common(Code, CallData, VMVersion) ->
    simple_call_common(Code, CallData, undefined, undefined, VMVersion).

-spec simple_call_common(binary(), binary(), aeso_sophia:type() | 'undefined',
                         aeso_sophia:type() | 'undefined', VMVersion :: integer()) ->
                            {ok, binary()} | {error, binary()}.
simple_call_common(Code, CallData, CallDataType, OutType, VMVersion) ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Owner          = <<123456:32/unit:8>>,
    Deposit        = 0,
    Contract       = aect_contracts:new(Owner, 1, VMVersion, Code, Deposit),
    Store          = aect_contracts:state(Contract),
    ContractKey    = aect_contracts:pubkey(Contract),
    Trees1         = aect_utils:insert_contract_in_trees(Contract, Trees),
    call_common(CallData, CallDataType, OutType, ContractKey, Code,
                Store, TxEnv, Trees1, VMVersion).

-spec call_common(binary(), aeso_sophia:type() | 'undefined',
                  aeso_sophia:type() | 'undefined', binary(), binary(),
                  aect_contracts:store(), aetx_env:env(),
                  aec_trees:trees(), VMVersion :: integer()) ->
                     {ok, binary()} | {error, binary()}.
call_common(CallData, CallDataType, OutType, ContractKey,
            Code, Store, TxEnv, Trees, VMVersion) ->
    <<Address:256>> = ContractKey,
    GasLimit = aec_governance:block_gas_limit(),
    Amount = 0,
    ChainState = aec_vm_chain:new_state(Trees, TxEnv, ContractKey, VMVersion),
    <<BeneficiaryInt:?BENEFICIARY_PUB_BYTES/unit:8>> = aetx_env:beneficiary(TxEnv),
    Spec = #{ code => Code
            , store => Store
            , address => Address
            , caller => 0
            , data => CallData
            , call_data_type => CallDataType
            , out_type => OutType
            , gas => 100000000000000000
            , gasPrice => 1
            , origin => 0
            , value => Amount
            , currentCoinbase => BeneficiaryInt
            , currentDifficulty => aetx_env:difficulty(TxEnv)
            , currentGasLimit => GasLimit
            , currentNumber => aetx_env:height(TxEnv)
            , currentTimestamp => aetx_env:time_in_msecs(TxEnv)
            , chainAPI => aec_vm_chain
            , chainState => ChainState
            , vm_version => VMVersion
            },
    try execute_call(Spec, true) of
        {ok, #{ out := Out } = _RetState} ->
            {ok, Out};
        E -> {error, list_to_binary(io_lib:format("~p", [E]))}
    catch _T:E ->
            ErrorString = io_lib:format("~p", [{E, erlang:get_stacktrace()}]),
            {error, list_to_binary(ErrorString)}
    end.

-spec execute_call(map(), boolean()) -> {ok, map()} | {error, term()}.
execute_call(#{ code := Code
              , store := Store
              , address := Address
              , caller := Caller
              , data := CallData
              , gas := Gas
              , gasPrice := GasPrice
              , origin := Origin
              , value := Value
              , currentCoinbase := CoinBase
              , currentDifficulty := Difficulty
              , currentGasLimit := GasLimit
              , currentNumber := Number
              , currentTimestamp := TS
              , chainState := ChainState
              , chainAPI := ChainAPI
              , vm_version := VmVersion
              } = CallDef, Trace) ->
    %% TODO: Handle Contract In State.
    Spec =
        #{ exec => #{ code => Code
                    , store => Store
                    , address => Address
                    , caller => Caller
                    , data => CallData
                    , call_data_type => maps:get(call_data_type, CallDef, undefined)
                    , out_type => maps:get(out_type, CallDef, undefined)
                    , gas => Gas
                    , gasPrice => GasPrice
                    , origin => Origin
                    , value => Value
                    },
           env => #{ currentCoinbase => CoinBase
                   , currentDifficulty => Difficulty
                   , currentGasLimit => GasLimit
                   , currentNumber => Number
                   , currentTimestamp => TS
                   , chainState => ChainState
                   , chainAPI => ChainAPI
                   , vm_version => VmVersion
                   },
           pre => #{}},
    TraceSpec =
        #{ trace_fun =>
               fun(S,A) -> lager:debug(S,A) end
         , trace => Trace
         },
    State = aevm_eeevm_state:init(Spec, TraceSpec),
    Result = aevm_eeevm:eval(State),
    Result.
