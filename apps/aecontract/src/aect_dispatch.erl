%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Call dispatcher for running contracts on the right VM.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_dispatch).

-include("../../aecore/include/blocks.hrl").
-include("../include/aecontract.hrl").

%% API
-export([run/2]).

-define(PUB_SIZE, 32).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-define(DEBUG_LOG(Format, Data), begin lager:debug(Format, Data), ?TEST_LOG(Format, Data) end).
-define(ST(), erlang:get_stacktrace()).
-else.
-define(TEST_LOG(Format, Data), ok).
-define(DEBUG_LOG(Format, Data), lager:debug(Format, Data)).
-define(ST(), {}).
-endif.

%% -- Running contract code on chain ---------------------------------------

%% Call the contract and update the call object with the return value and gas
%% used.

-spec run(map(), map()) -> {aect_call:call(), aec_trees:trees(), aetx_env:env()}.
run(#{vm := VM} = Version, #{code := Code0} = CallDef) when ?IS_FATE_SOPHIA(VM) ->
    #{byte_code := Code} = maybe_deserialize_code(Code0),
    case aefa_fate:is_valid_calldata(maps:get(call_data, CallDef)) of
        true ->
            CallDef1 = CallDef#{code => Code},
            run_common(Version, CallDef1);
        false ->
            Gas = maps:get(gas, CallDef),
            Call = maps:get(call, CallDef),
            Trees = maps:get(trees, CallDef),
            Env = maps:get(tx_env, CallDef),
            {create_call(Gas, error, <<"bad_call_data">>, [], Call, VM), Trees, Env}
    end;
run(#{vm := VM} = Version, CallDef) when ?IS_AEVM_SOPHIA(VM) ->
    #{ code      := Code0
     , call_data := CallData
     , amount    := Amount } = CallDef,
    #{ byte_code := Code
     , type_info := TypeInfo } = maybe_deserialize_code(Code0),
    %% TODO: update aeb_aevm_abi and pass Version
    case aeb_aevm_abi:check_calldata(CallData, TypeInfo, Amount > 0) of
        {ok, CallDataType, OutType} ->
            CallDef1 = CallDef#{code => Code,
                                call_data_type => CallDataType,
                                out_type => OutType},
            run_common(Version, CallDef1);
        {error, What} ->
            Gas   = maps:get(gas, CallDef),
            Call  = maps:get(call, CallDef),
            Trees = maps:get(trees, CallDef),
            Env   = maps:get(tx_env, CallDef),
            {create_call(Gas, error, What, [], Call, VM), Trees, Env}
    end;
run(#{vm := ?VM_AEVM_SOLIDITY_1, abi := ?ABI_SOLIDITY_1} = Version, CallDef) ->
    run_common(Version, CallDef);
run(_, #{ call := Call} = _CallDef) ->
    %% TODO:
    %% Wrong VM/ABI version just return an unchanged call.
    Call.

-define(FATE_VM_SPEC_FIELDS, [ trees
                             , tx_env
                             , caller
                             , origin
                             , gas_price
                             , fee
                             ]).

run_common(#{vm := VMVersion, abi := ABIVersion},
           #{ amount      := Value
            , call        := Call
            , call_data   := CallData
            , call_stack  :=_CallStack %% Unused for FATE
            , caller      := <<_:?PUB_SIZE/unit:8>>
            , code        := Code
            , store       := Store
            , contract    := <<_:?PUB_SIZE/unit:8>> = ContractAddress
            , gas         := Gas
            , fee         := Fee
            , gas_price   :=_GasPrice
            , trees       := Trees
            , tx_env      := TxEnv0
            , origin      := <<_:?PUB_SIZE/unit:8>>
            } = CallDef)
  when ?IS_FATE_SOPHIA(VMVersion) ->
    case ABIVersion =:= ?ABI_FATE_SOPHIA_1 of
        false ->
            error({illegal_abi, ?VM_FATE_SOPHIA_1, ABIVersion});
        true ->
            OldContext  = aetx_env:context(TxEnv0),
            Spec = #{ contract   => ContractAddress
                    , call       => CallData
                    , code       => Code
                    , store      => Store
                    , gas        => Gas
                    , fee        => Fee
                    , value      => Value
                    , vm_version => VMVersion
                    , allow_init => maps:get(allow_init, CallDef, false)
                    },
            Env0 = maps:with(?FATE_VM_SPEC_FIELDS, CallDef),
            Env1  = Env0#{ tx_env   => aetx_env:set_context(TxEnv0, aetx_contract)
                        },
            Env2 = case maps:is_key(on_chain_trees, CallDef) of
                       true  -> Env1#{on_chain_trees => maps:get(on_chain_trees, CallDef)};
                       false -> Env1
                   end,

            case aefa_fate:run(Spec, Env2) of
                {ok, ResultState} ->
                    ReturnValue = aefa_fate:return_value(ResultState),
                    Out         = aeb_fate_encoding:serialize(ReturnValue),
                    TxEnvOut0   = aefa_fate:tx_env(ResultState),
                    TxEnvOut    = aetx_env:set_context(TxEnvOut0, OldContext),
                    GasUsed     = Gas - aefa_fate:gas(ResultState),
                    TreesOut    = aefa_fate:final_trees(ResultState),
                    Log         = aefa_fate:logs(ResultState),
                    { create_call(GasUsed, ok, Out, Log, Call, VMVersion)
                    , TreesOut
                    , TxEnvOut
                    };
                {revert, ReturnValue, ResultState} ->
                    Out     = aeb_fate_encoding:serialize(ReturnValue),
                    GasUsed = Gas - aefa_fate:gas(ResultState),
                    { create_call(GasUsed, revert, Out, [], Call, VMVersion)
                    , Trees
                    , TxEnv0
                    };
                {error, Out, ResultState} ->
                    GasUsed = Gas - aefa_fate:gas(ResultState),
                    { create_call(GasUsed, error, Out, [], Call, VMVersion)
                    , Trees
                    , TxEnv0
                    }
            end
    end;
run_common(#{vm := VMVersion, abi := ABIVersion},
           #{  amount      := Value
             , call        := Call
             , call_data   := CallData
             , call_stack  := CallStack
             , caller      := <<CallerAddr:?PUB_SIZE/unit:8>>
             , code        := Code
             , store       := Store
             , contract    := <<Address:?PUB_SIZE/unit:8>>
             , gas         := Gas
             , gas_price   := GasPrice
             , trees       := Trees
             , tx_env      := TxEnv0
             , origin      := <<OriginAddr0:?PUB_SIZE/unit:8>>
             , creator     := <<CreatorAddr:?PUB_SIZE/unit:8>>
             } = CallDef) ->
    OldContext = aetx_env:context(TxEnv0),
    TxEnv = aetx_env:set_context(TxEnv0, aetx_contract),
    ChainState0 = chain_state(CallDef, TxEnv, VMVersion),
    <<BeneficiaryInt:?BENEFICIARY_PUB_BYTES/unit:8>> = aetx_env:beneficiary(TxEnv),
    OriginAddr = get_origin(VMVersion, CallerAddr, OriginAddr0),
    Env = #{currentCoinbase   => BeneficiaryInt,
            currentDifficulty => aetx_env:difficulty(TxEnv),
            currentGasLimit   => aec_governance:block_gas_limit(),
            currentNumber     => aetx_env:height(TxEnv),
            currentTimestamp  => aetx_env:time_in_msecs(TxEnv),
            authTxHash        => aetx_env:ga_tx_hash(TxEnv),
            chainState        => ChainState0,
            chainAPI          => aec_vm_chain,
            protocol_version  => aetx_env:consensus_version(TxEnv),
            vm_version        => VMVersion,
            abi_version       => ABIVersion},
    Exec = #{code           => Code,
             store          => Store,
             address        => Address,
             caller         => CallerAddr,
             call_data_type => maps:get(call_data_type, CallDef, undefined),
             out_type       => maps:get(out_type, CallDef, undefined),
             data           => CallData,
             gas            => Gas,
             gasPrice       => GasPrice,
             origin         => OriginAddr,
             creator        => CreatorAddr,
             value          => Value,
             call_stack     => CallStack
            },
    Spec = #{ env => Env,
              exec => Exec,
              pre => #{}},

    try aevm_eeevm_state:init(Spec, #{trace => false}) of
        InitState ->
            %% TODO: Nicer error handling - do more in check.
            %% Update gas_used depending on exit type.
            case aevm_eeevm:eval(InitState) of
                {ok, ResultState} ->
                    GasLeft = aevm_eeevm_state:gas(ResultState),
                    GasUsed = Gas - GasLeft,
                    Out = aevm_eeevm_state:out(ResultState),
                    ChainState = aevm_eeevm_state:chain_state(ResultState),
                    Log = aevm_eeevm_state:logs(ResultState),
                    {
                      create_call(GasUsed, ok, Out, Log, Call, VMVersion),
                      aec_vm_chain:get_trees(ChainState),
                      aetx_env:set_context(aec_vm_chain:get_tx_env(ChainState), OldContext)
                    };
                {revert, Out, GasLeft} ->
                    GasUsed = Gas - GasLeft,
                    {create_call(GasUsed, revert, Out, [], Call, VMVersion), Trees, TxEnv0};
                {error, What, GasLeft} ->
                    %% If we ran out of gas in a recursive call there
                    %% might still be gas held back by the caller.
                    GasUsed = Gas - GasLeft,
                    {create_call(GasUsed, error, What, [], Call, VMVersion), Trees, TxEnv0}
            end
    catch
        throw:{init_error, What} ->
            ?DEBUG_LOG("Init error ~p", [What]),
            {create_call(Gas, error, What, [], Call, VMVersion), Trees, TxEnv0}
    end.

create_call(GasUsed, Type, Value, Log, Call,_VMVersion) when Type == ok; Type == revert ->
    Call1 = aect_call:set_return_value(Value, Call),
    create_call(GasUsed, Type, Log, Call1);
create_call(GasUsed, error = Type, Value, Log, Call, VMVersion) when ?IS_AEVM_SOPHIA(VMVersion) ->
    Call1 = aect_call:set_return_value(error_to_binary(Value), Call),
    create_call(GasUsed, Type, Log, Call1);
create_call(GasUsed, error = Type, Value, Log, Call, VMVersion) when ?IS_FATE_SOPHIA(VMVersion) ->
    Call1 = aect_call:set_return_value(Value, Call),
    create_call(GasUsed, Type, Log, Call1).

create_call(GasUsed, Type, Log, Call) ->
    Call1 = aect_call:set_log(Log, Call),
    aect_call:set_gas_used(GasUsed, aect_call:set_return_type(Type, Call1)).

%% c.f. aec_vm_chain:binary_to_error/1
error_to_binary(out_of_gas) -> <<"out_of_gas">>;
error_to_binary(out_of_stack) -> <<"out_of_stack">>;
error_to_binary(not_allowed_off_chain) -> <<"not_allowed_off_chain">>;
error_to_binary(bad_call_data) -> <<"bad_call_data">>;
error_to_binary(unknown_error) -> <<"unknown_error">>;
error_to_binary(unknown_contract) -> <<"unknown_contract">>;
error_to_binary(unknown_function) -> <<"unknown_function">>;
error_to_binary(function_is_not_payable) -> <<"function_is_not_payable">>;
error_to_binary(account_is_not_payable) -> <<"account_is_not_payable">>;
error_to_binary(reentrant_call) -> <<"reentrant_call">>;
error_to_binary(arithmetic_error) -> <<"arithmetic_error">>;
error_to_binary({illegal_instruction, OP}) when is_integer(OP), 0 =< OP, OP =< 255 ->
    X = <<_:2/bytes>> = list_to_binary(io_lib:format("~2.16.0B",[OP])),
    <<"illegal_instruction_", X:2/bytes>>;
error_to_binary(E) ->
    ?DEBUG_LOG("Unknown error: ~p\n", [E]),
    <<"unknown_error">>.

chain_state(#{ contract    := ContractPubKey
             , off_chain   := false
             , trees       := Trees}, TxEnv, VMVersion) ->
    aec_vm_chain:new_state(Trees, TxEnv, ContractPubKey, VMVersion);
chain_state(#{ contract    := ContractPubKey
             , off_chain   := true
             , trees       := Trees} = CallDef, TxEnv, VMVersion) ->
            OnChainTrees = maps:get(on_chain_trees, CallDef),
    aec_vm_chain:new_offchain_state(Trees, OnChainTrees, TxEnv,
                                    ContractPubKey, VMVersion).

get_origin(VMVersion, CallerAddr, OriginAddr) ->
    case VMVersion of
        ?VM_AEVM_SOPHIA_1 ->
            %% Backwards compatible bug
            CallerAddr;
        _ ->
            OriginAddr
    end.

maybe_deserialize_code(#{ byte_code := _ } = Code) -> Code;
maybe_deserialize_code(SerializedCode) ->
    aect_sophia:deserialize(SerializedCode).
