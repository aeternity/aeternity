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
encode_call_data(<<"sophia">>, Code, Function, Argument) ->
    aect_sophia:encode_call_data(Code, Function, Argument);
encode_call_data(<<"evm">>,_Code, Function, Argument) ->
    %% TODO: Check that Function exists in Contract.
    {ok, <<Function/binary, Argument/binary>>};
encode_call_data(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.

%% TODO: replace language string with vm_version number.
call(<<"sophia-address">>, ContractKey, Function, Argument) ->
    sophia_call(ContractKey, Function, Argument);
call(<<"evm">>, Code, _, CallData) ->
    solidity_call(Code, CallData);
call(_, _, _, _) ->
    {error, <<"Unknown call ABI">>}.

solidity_call(Code, CallData) ->
    CTVersion = #{vm => ?VM_AEVM_SOLIDITY_1, abi => ?ABI_SOLIDITY_1},
    CallDataType   = undefined,
    OutType        = undefined,
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Owner          = <<123456:32/unit:8>>,
    Deposit        = 0,
    Contract       = aect_contracts:new(Owner, 1, CTVersion, Code, Deposit),
    Store          = aect_contracts:state(Contract),
    ContractKey    = aect_contracts:pubkey(Contract),
    Trees1         = aect_utils:insert_contract_in_trees(Contract, Trees),
    call_common(CallData, CallDataType, OutType, ContractKey, Code, Store,
                TxEnv, Trees1, CTVersion).

sophia_call(ContractKey, Function, Argument) ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_contract),
    ContractsTree  = aec_trees:contracts(Trees),
    Contract       = aect_state_tree:get_contract(ContractKey, ContractsTree),
    SerializedCode = aect_contracts:code(Contract),
    Store          = aect_contracts:state(Contract),
    CTVersion      = aect_contracts:ct_version(Contract),

    case aehttp_logic:sophia_encode_call_data(SerializedCode, Function, Argument) of
        {error, Error} ->
            {error, list_to_binary(io_lib:format("~p", [Error]))};
        {ok, CallData} ->
            #{type_info := TypeInfo,
              byte_code := Code} = aect_sophia:deserialize(SerializedCode),
            {_Hash, Function, ArgTypeBin, OutTypeBin} = lists:keyfind(Function, 2, TypeInfo),
            {ok, ArgType} = aeso_heap:from_binary(typerep, ArgTypeBin),
            {ok, OutType} = aeso_heap:from_binary(typerep, OutTypeBin),
            CallDataType = {tuple, [word, ArgType]},
            call_common(CallData, CallDataType, OutType, ContractKey, Code,
                        Store, TxEnv, Trees, CTVersion)
    end.

call_common(CallData, CallDataType, OutType, ContractPubkey, Code, Store,
            TxEnv, Trees, CTVersion) ->
    ContractId = aeser_id:create(contract, ContractPubkey),
    Height = aetx_env:height(TxEnv),
    GasPrice = 1,
    Caller = <<123:?PUB_SIZE/unit:8>>,
    CallerId = aeser_id:create(account, Caller),
    CallIn = aect_call:new(CallerId,_Nonce=0, ContractId, Height, GasPrice),
    Spec = #{ code           => Code
            , call           => CallIn
            , store          => Store
            , contract       => ContractPubkey
            , caller         => Caller
            , call_data      => CallData
            , call_data_type => CallDataType
            , call_stack     => []
            , off_chain      => false
            , out_type       => OutType
            , gas            => 100000000000000000
            , gas_price      => GasPrice
            , origin         => <<123:?PUB_SIZE/unit:8>>
            , amount         => 0
            , trees          => Trees
            , tx_env         => TxEnv
            },
    {Call,_TreesOut,Env1} = run_common(CTVersion, Spec),
    ReturnValue = aect_call:return_value(Call),
    case aect_call:return_type(Call) of
        ok     -> {ok, ReturnValue, Env1};
        error  -> {error, ReturnValue};
        revert -> {error, ReturnValue}
    end.


%% -- Running contract code on chain ---------------------------------------

%% Call the contract and update the call object with the return value and gas
%% used.

-spec run(map(), map()) -> {aect_call:call(), aec_trees:trees(), aetx_env:env()}.
run(#{vm := VM} = Version, #{code := SerializedCode} = CallDef) when ?IS_VM_SOPHIA(VM) ->
    #{ byte_code := Code
     , type_info := TypeInfo} = aect_sophia:deserialize(SerializedCode),
    %% TODO: update aeso_abi and pass Version
    case aeso_abi:check_calldata(maps:get(call_data, CallDef), TypeInfo) of
        {ok, CallDataType, OutType} ->
            CallDef1 = CallDef#{code => Code,
                                call_data_type => CallDataType,
                                out_type => OutType},
            run_common(Version, CallDef1);
        {error, What} ->
            Gas = maps:get(gas, CallDef),
            Call = maps:get(call, CallDef),
            Trees = maps:get(trees, CallDef),
            Env = maps:get(tx_env, CallDef),
            {create_call(Gas, error, What, [], Call), Trees, Env}
    end;
run(#{vm := ?VM_AEVM_SOLIDITY_1, abi := ?ABI_SOLIDITY_1} = Version, CallDef) ->
    run_common(Version, CallDef);
run(_, #{ call := Call} = _CallDef) ->
    %% TODO:
    %% Wrong VM/ABI version just return an unchanged call.
    Call.

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
            chainState        => ChainState0,
            chainAPI          => aec_vm_chain,
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
                      create_call(GasUsed, ok, Out, Log, Call),
                      aec_vm_chain:get_trees(ChainState),
                      aetx_env:set_context(aec_vm_chain:get_tx_env(ChainState), OldContext)
                    };
                {revert, Out, GasLeft} ->
                    GasUsed = Gas - GasLeft,
                    {create_call(GasUsed, revert, Out, [], Call), Trees, TxEnv0};
                {error, What, GasLeft} ->
                    %% If we ran out of gas in a recursive call there
                    %% might still be gas held back by the caller.
                    GasUsed = Gas - GasLeft,
                    {create_call(GasUsed, error, What, [], Call), Trees, TxEnv0}
            end
    catch
        throw:{init_error, What} ->
            ?DEBUG_LOG("Init error ~p", [What]),
            {create_call(Gas, error, What, [], Call), Trees, TxEnv0}
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
