-module(aect_channel_contract).
-include("../../aecontract/include/hard_forks.hrl").
-include("../../aecore/include/blocks.hrl").
-include("aecontract.hrl").

-export([new/6,
         run_new/6,
         run/13,
         get_call/4,
         insert_failed_call/6
        ]).

new(Owner, Round, CTVersion, Code, Deposit, Trees0) ->
    %% Create the contract and insert it into the contract state tree
    %%   The public key for the contract is generated from the owners pubkey
    %%   and the nonce, so that no one has the private key.
    Contract        = aect_contracts:new(Owner, Round, CTVersion, Code, Deposit),
    ContractId      = aect_contracts:id(Contract),
    ContractsTree0  = aec_trees:contracts(Trees0),
    ContractsTree1  = aect_state_tree:insert_contract(Contract, ContractsTree0),
    Trees1          = aec_trees:set_contracts(Trees0, ContractsTree1),
    {ContractId, Contract, Trees1}.

-spec run_new(aect_contracts:pubkey(), aect_call:call(), binary(),
              aec_trees:trees(), aec_trees:trees(),
              aetx_env:env()) -> aec_trees:trees().
run_new(ContractPubKey, Call, CallData, Trees0, OnChainTrees,
        OnChainEnv) ->
    ContractsTree  = aec_trees:contracts(Trees0),
    {Contract0, Code} = aect_state_tree:get_contract_with_code(ContractPubKey, ContractsTree),
    OwnerPubKey = aect_contracts:owner_pubkey(Contract0),
    CallStack = [], %% TODO: should we have a call stack for create_tx also
                    %% when creating a contract in a contract.
    VmVersion = aect_contracts:vm_version(Contract0),
    %% Assert VmVersion before running!
    [error({illegal_vm_version, VmVersion}) || not ?IS_AEVM_SOPHIA(VmVersion) andalso
                                               not ?IS_FATE_SOPHIA(VmVersion)],
    assert_init_function(CallData, VmVersion, Code),
    Protocol = aetx_env:consensus_version(OnChainEnv),
    {Contract, Trees1} = prepare_init_call(VmVersion, Protocol, Contract0, Code, Trees0),
    Store = aect_contracts:state(Contract),
    CallDef = make_call_def(OwnerPubKey, OwnerPubKey, ContractPubKey,
                            _Gas = 1000000, _Fee = 0, _GasPrice = 1,
                            _Amount = 0, %TODO: make this configurable
                            CallData, _AllowInit = true, CallStack, Code, Store, Call, OnChainTrees, OnChainEnv, Trees1),
    CTVersion = aect_contracts:ct_version(Contract),
    {CallRes, Trees, _} = aect_dispatch:run(CTVersion, CallDef),
    case aect_call:return_type(CallRes) of
        ok ->
            Trees2 = aect_utils:insert_call_in_trees(CallRes, Trees),
            %% Save the initial state (returned by `init`) in the store.
            InitState = aect_call:return_value(CallRes),
            case CTVersion of
                #{vm := V} when ?IS_AEVM_SOPHIA(V) ->
                    %% TODO: move to/from_sophia_state to make nicer dependencies?
                    case aevm_eeevm_store:from_sophia_state(CTVersion, InitState) of
                        {ok, Store1} ->
                            Contract1 = aect_contracts:set_state(Store1, Contract),
                            ContractsTree0 = aec_trees:contracts(Trees2),
                            ContractsTree1 = aect_state_tree:enter_contract(Contract1, ContractsTree0),
                            aec_trees:set_contracts(Trees2, ContractsTree1);
                        E = {error, _} ->
                            lager:debug("Init error ~w ~w",[E, CallRes]),
                            erlang:error(contract_init_failed)
                    end;
                #{vm := V} when ?IS_FATE_SOPHIA(V) ->
                    %% The store is written by the INIT function
                    Trees2
            end;
        E ->
            lager:debug("Init call error ~w ~w",[E, CallRes]),
            erlang:error(contract_init_failed)
    end.

prepare_init_call(VmVersion, Protocol, Contract, SerializedCode, Trees0) when ?IS_FATE_SOPHIA(VmVersion) ->
    Code = #{ byte_code := ByteCode } = aeser_contract_code:deserialize(SerializedCode),
    ByteCode1 =
        if
            % Before FATE 2 we do not include init function in code put in trees
            VmVersion == ?VM_FATE_SOPHIA_1 ->
                FateCode  = aeb_fate_code:deserialize(ByteCode),
                FateCode1 = aeb_fate_code:strip_init_function(FateCode),
                aeb_fate_code:serialize(FateCode1);
            true -> ByteCode
        end,
    Code1     = Code#{ byte_code := ByteCode1 },
    %% The serialization was broken in the Lima release - setting the
    %% compiler version to "unknown" regardless of the actual value.
    Code2     = case Protocol =< ?LIMA_PROTOCOL_VSN of
                    true  -> Code1#{ compiler_version => <<"unknown">> };
                    false -> Code1
                end,
    SerCode   = aeser_contract_code:serialize(Code2),

    %% Initialize the store before calling INIT
    Store = aefa_stores:initial_contract_store(),
    Contract1 = aect_contracts:set_state(Store, Contract),
    Contract2 = aect_contracts:set_code(SerCode, Contract1),
    ContractsTree0 = aec_trees:contracts(Trees0),
    ContractsTree1 = aect_state_tree:enter_contract(Contract2, ContractsTree0),
    {Contract1, aec_trees:set_contracts(Trees0, ContractsTree1)};
prepare_init_call(VmVersion, Protocol, Contract, SerializedCode, Trees0)
  when ?IS_AEVM_SOPHIA(VmVersion), VmVersion >= ?VM_AEVM_SOPHIA_4 ->
    #{ type_info := TypeInfo } = Code = aeser_contract_code:deserialize(SerializedCode),
    TypeInfo1 = lists:keydelete(<<"init">>, 2, TypeInfo),
    Code1     = Code#{ type_info := TypeInfo1 },
    %% The serialization was broken in the Lima release - setting the
    %% compiler version to "unknown" regardless of the actual value.
    Code2     = case Protocol =< ?LIMA_PROTOCOL_VSN of
                    true  -> Code1#{ compiler_version => <<"unknown">> };
                    false -> Code1
                end,
    SerCode   = aeser_contract_code:serialize(Code2, maps:get(contract_vsn, Code2, 3)),
    Contract1 = aect_contracts:set_code(SerCode, Contract),
    {Contract1, Trees0};
prepare_init_call(_, _, Contract, _Code, Trees0) ->
    {Contract, Trees0}.

assert_init_function(CallData, VMVersion,_SerializedCode) when ?IS_FATE_SOPHIA(VMVersion) ->
    case aefa_fate:verify_init_calldata(CallData) of
        ok ->
            ok;
        error ->
            error(contract_init_failed)
    end;
assert_init_function(CallData, VMVersion, SerializedCode) when ?IS_AEVM_SOPHIA(VMVersion) ->
    try aeser_contract_code:deserialize(SerializedCode) of
        #{type_info := TypeInfo} ->
            case aeb_aevm_abi:get_function_hash_from_calldata(CallData) of
                {ok, Hash} ->
                    case aeb_aevm_abi:function_name_from_type_hash(Hash, TypeInfo) of
                        {ok, <<"init">>} -> ok;
                        _ -> error(contract_init_failed)
                    end;
                _Other -> error(contract_init_failed)
            end
    catch _:_ ->
            error(contract_init_failed)
    end.

-spec run(aect_contracts:pubkey(), aect_contracts:abi_version(), aect_call:call(),
          binary(), [non_neg_integer()], aec_trees:trees(),
          non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(),
          aec_trees:trees(), aetx_env:env(), aec_keys:pubkey()) ->
    {aec_trees:trees(), aect_call:call()}.
run(ContractPubKey, ABIVersion, Call, CallData, CallStack, Trees0,
    Amount, GasPrice, Fee, Gas, OnChainTrees, OnChainEnv, CallerPubkey) ->
    ContractsTree  = aec_trees:contracts(Trees0),
    {Contract, Code} = aect_state_tree:get_contract_with_code(ContractPubKey, ContractsTree),
    OwnerPubkey = aect_contracts:owner_pubkey(Contract),
    Store = aect_contracts:state(Contract),
    VmVersion = aect_contracts:vm_version(Contract),
    case aect_contracts:abi_version(Contract) =:= ABIVersion of
        true when ?IS_AEVM_SOPHIA(VmVersion);
                  ?IS_FATE_SOPHIA(VmVersion) -> ok;
        true                                 -> erlang:error(wrong_vm_version);
        false                                -> erlang:error(wrong_abi_version)
    end,
    CallDef = make_call_def(CallerPubkey, OwnerPubkey, ContractPubKey, Gas, Fee, GasPrice, Amount,
              CallData, _AllowInit = false, CallStack, Code, Store, Call, OnChainTrees, OnChainEnv, Trees0),
    {CallRes, Trees, _} = aect_dispatch:run(#{vm => VmVersion, abi => ABIVersion}, CallDef),
    UpdatedTrees = aect_utils:insert_call_in_trees(CallRes, Trees),
    {aec_trees:gc_cache(UpdatedTrees, [accounts, contracts]), CallRes}.



make_call_def(CallerPubkey, OwnerPubKey, ContractPubKey, GasLimit, Fee, GasPrice, Amount,
              CallData, AllowInit, CallStack, Code, Store, Call, OnChainTrees, OnChainEnv,
              OffChainTrees) ->
    #{caller          => CallerPubkey
    , contract        => ContractPubKey
    , gas             => GasLimit
    , fee             => Fee
    , gas_price       => GasPrice
    , call_data       => CallData
    , amount          => Amount
    , call_stack      => CallStack
    , code            => Code
    , call            => Call
    , store           => Store
    , trees           => OffChainTrees
    , tx_env          => OnChainEnv
    , off_chain       => true
    , on_chain_trees  => OnChainTrees
    , origin          => CallerPubkey
    , creator         => OwnerPubKey
    , allow_init      => AllowInit
    }.


get_call(ContractPubkey, CallerPubkey, Round, CallsTree) ->
    CallId = aect_call:id(CallerPubkey, Round, ContractPubkey),
    case aect_call_state_tree:lookup_call(ContractPubkey, CallId, CallsTree) of
        none -> {error, call_not_found};
        {value, Call} -> {ok, Call}
    end.

-spec insert_failed_call(aect_contracts:pubkey(), aec_keys:pubkey(),
                         non_neg_integer(), non_neg_integer(),
                         non_neg_integer(), aect_call_state_tree:tree()) ->
                         aect_call_state_tree:tree().
insert_failed_call(ContractPubkey, CallerPubkey, Round, GasPrice, GasLimit, CallsTree) ->
    Caller = aeser_id:create(account, CallerPubkey),
    Contract = aeser_id:create(contract, ContractPubkey),
    Call0 = aect_call:new(Caller, Round, Contract, Round, GasPrice),
    Call1 = aect_call:set_gas_used(GasLimit, Call0), % all gas is consumed
    Call2 = aect_call:set_return_type(error, Call1),
    Call = aect_call:set_return_value(<<"invalid_call">>, Call2),
    aect_call_state_tree:insert_call(Call, CallsTree).
