-module(aect_channel_contract).
-include_lib("apps/aecore/include/blocks.hrl").
-include("aecontract.hrl").

-export([new/6,
         run_new/6,
         run/11,
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
    Contract = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    OwnerPubKey = aect_contracts:owner_pubkey(Contract),
    Code = aect_contracts:code(Contract),
    Store = aect_contracts:state(Contract),
    CallStack = [], %% TODO: should we have a call stack for create_tx also
                    %% when creating a contract in a contract.
    VmVersion = aect_contracts:vm_version(Contract),
    %% Assert VmVersion before running!
    [error({illegal_vm_version, VmVersion}) || not ?IS_VM_SOPHIA(VmVersion)],

    CallDef = make_call_def(OwnerPubKey, ContractPubKey,
                            _Gas = 1000000, _GasPrice = 1,
                            _Amount = 0, %TODO: make this configurable
                            CallData, CallStack, Code, Store, Call, OnChainTrees, OnChainEnv, Trees0),
    CTVersion = aect_contracts:ct_version(Contract),
    {CallRes, Trees} = aect_dispatch:run(CTVersion, CallDef),
    case aect_call:return_type(CallRes) of
        ok ->
            Trees1 = aect_utils:insert_call_in_trees(CallRes, Trees),
            %% Save the initial state (returned by `init`) in the store.
            InitState  = aect_call:return_value(CallRes),
            %% TODO: move to/from_sophia_state to make nicer dependencies?
            case aevm_eeevm_store:from_sophia_state(CTVersion, InitState) of
                {ok, Store1} ->
                    Contract1 = aect_contracts:set_state(Store1, Contract),
                    ContractsTree0 = aec_trees:contracts(Trees1),
                    ContractsTree1 = aect_state_tree:enter_contract(Contract1, ContractsTree0),
                    aec_trees:set_contracts(Trees1, ContractsTree1);
                E = {error, _} ->
                    lager:debug("Init error ~w ~w",[E, CallRes]),
                    Trees0
            end;
        E ->
            lager:debug("Init call error ~w ~w",[E, CallRes]),
            Trees0
    end.

-spec run(aect_contracts:pubkey(), aect_contracts:abi_version(), aect_call:call(),
          binary(), [non_neg_integer()], aec_trees:trees(),
          non_neg_integer(), non_neg_integer(), non_neg_integer(),
          aec_trees:trees(), aetx_env:env()) -> aec_trees:trees().
run(ContractPubKey, ABIVersion, Call, CallData, CallStack, Trees0,
    Amount, GasPrice, Gas, OnChainTrees, OnChainEnv) ->
    ContractsTree  = aec_trees:contracts(Trees0),
    Contract = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    OwnerPubKey = aect_contracts:owner_pubkey(Contract),
    Code = aect_contracts:code(Contract),
    Store = aect_contracts:state(Contract),
    VmVersion = aect_contracts:vm_version(Contract),
    case aect_contracts:abi_version(Contract) =:= ABIVersion of
        true when ?IS_VM_SOPHIA(VmVersion) -> ok;
        true                               -> erlang:error(wrong_vm_version);
        false                              -> erlang:error(wrong_abi_version)
    end,
    CallDef = make_call_def(OwnerPubKey, ContractPubKey, Gas, GasPrice, Amount,
              CallData, CallStack, Code, Store, Call, OnChainTrees, OnChainEnv, Trees0),
    {CallRes, Trees} = aect_dispatch:run(#{vm => VmVersion, abi => ABIVersion}, CallDef),
    UpdatedTrees = aect_utils:insert_call_in_trees(CallRes, Trees),
    aec_trees:gc_cache(UpdatedTrees, [accounts, contracts]).



make_call_def(OwnerPubKey, ContractPubKey, GasLimit, GasPrice, Amount,
              CallData, CallStack, Code, Store, Call, OnChainTrees, OnChainEnv,
              OffChainTrees) ->
    #{caller          => OwnerPubKey
    , contract        => ContractPubKey
    , gas             => GasLimit
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
    Caller = aec_id:create(account, CallerPubkey),
    Contract = aec_id:create(contract, ContractPubkey),
    Call0 = aect_call:new(Caller, Round, Contract, Round, GasPrice),
    Call1 = aect_call:set_gas_used(GasLimit, Call0), % all gas is consumed
    Call2 = aect_call:set_return_type(error, Call1),
    Call = aect_call:set_return_value(<<"invalid_call">>, Call2),
    aect_call_state_tree:insert_call(Call, CallsTree).
