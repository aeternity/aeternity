%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Call dispatcher for running contracts on the right VM.
%%% @end
%%%-------------------------------------------------------------------
-module(aect_dispatch).

-include_lib("apps/aecore/include/blocks.hrl").
-include("aecontract.hrl").

%% API
-export([ call/4
        , encode_call_data/4
        , run/2]).

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

%% -- Running contract code off chain ---------------------------------------

%% TODO: replace language string with vm_version number.
call(<<"sophia-address">>, ContractKey, Function, Argument) ->
    aect_sophia:on_chain_call(ContractKey, Function, Argument);
call(<<"evm">>, Code, _, CallData) ->
    aect_evm:simple_call_common(Code, CallData, ?AEVM_01_Solidity_01);
call(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.

%% TODO: replace language string with vm_version number.

encode_call_data(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:encode_call_data(Code, Function, Argument);
encode_call_data(<<"evm">>, Code, Function, Argument) ->
    aect_evm:encode_call_data(Code, Function, Argument);
encode_call_data(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.


%% -- Running contract code on chain ---------------------------------------

%% Call the contract and update the call object with the return value and gas
%% used.

-spec run(byte(), map()) -> {aect_call:call(), aec_trees:trees()}.
run(VMVersion, #{code := SerializedCode} = CallDef) when ?IS_AEVM_SOPHIA(VMVersion) ->
    #{ byte_code := Code
     , type_info := TypeInfo} = aect_sophia:deserialize(SerializedCode),
    case aeso_abi:check_calldata(maps:get(call_data, CallDef), TypeInfo) of
        {ok, CallDataType, OutType} ->
            CallDef1 = CallDef#{code => Code,
                                call_data_type => CallDataType,
                                out_type => OutType},
            run_common(CallDef1, VMVersion);
        {error, What} ->
            Gas = maps:get(gas, CallDef),
            Call = maps:get(call, CallDef),
            Trees = maps:get(trees, CallDef),
            {create_call(Gas, error, What, [], Call), Trees}
    end;
run(?AEVM_01_Solidity_01, CallDef) ->
    run_common(CallDef, ?AEVM_01_Solidity_01);
run(_, #{ call := Call} = _CallDef) ->
    %% TODO:
    %% Wrong VM/ABI version just return an unchanged call.
    Call.

run_common(#{  amount      := Value
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
             } = CallDef, VMVersion) ->
    TxEnv = aetx_env:set_context(TxEnv0, aetx_contract),
    ChainState0 = chain_state(CallDef, TxEnv, VMVersion),
    <<BeneficiaryInt:?BENEFICIARY_PUB_BYTES/unit:8>> = aetx_env:beneficiary(TxEnv),
    Env = #{currentCoinbase   => BeneficiaryInt,
            currentDifficulty => aetx_env:difficulty(TxEnv),
            currentGasLimit   => aec_governance:block_gas_limit(),
            currentNumber     => aetx_env:height(TxEnv),
            currentTimestamp  => aetx_env:time_in_msecs(TxEnv),
            chainState        => ChainState0,
            chainAPI          => aec_vm_chain,
            vm_version        => VMVersion},
    Exec = #{code       => Code,
             store      => Store,
             address    => Address,
             caller     => CallerAddr,
             call_data_type => maps:get(call_data_type, CallDef, undefined),
             out_type   => maps:get(out_type, CallDef, undefined),
             data       => CallData,
             gas        => Gas,
             gasPrice   => GasPrice,
             origin     => CallerAddr,
             value      => Value,
             call_stack => CallStack
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
                      create_call(GasUsed, ok, Out, Log, Call),
                      aec_vm_chain:get_trees(ChainState)
                    };
                {revert, Out, GasLeft} ->
                    GasUsed = Gas - GasLeft,
                    {create_call(GasUsed, revert, Out, [], Call), Trees};
                {error, What, GasLeft} ->
                    %% If we ran out of gas in a recursive call there
                    %% might still be gas held back by the caller.
                    GasUsed = Gas - GasLeft,
                    {create_call(GasUsed, error, What, [], Call), Trees}
            end
    catch
        throw:{init_error, What} ->
            ?DEBUG_LOG("Init error ~p", [What]),
            {create_call(Gas, error, What, [], Call), Trees}
    end.

create_call(GasUsed, Type, Value, Log, Call) when Type == ok; Type == revert ->
    Call1 = aect_call:set_return_value(Value, Call),
    create_call(GasUsed, Type, Log, Call1);
create_call(GasUsed, error = Type, Value, Log, Call)  ->
    Call1 = aect_call:set_return_value(error_to_binary(Value), Call),
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
error_to_binary(reentrant_call) -> <<"reentrant_call">>;
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

