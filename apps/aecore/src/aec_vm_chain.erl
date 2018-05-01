%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aec_vm_chain_api.
%%% @end
%%%=============================================================================
-module(aec_vm_chain).

-include_lib("apps/aecore/include/common.hrl").

-behaviour(aec_vm_chain_api).

-export([new_state/3, get_trees/1]).

%% aec_vm_chain_api callbacks
-export([get_balance/2,
         spend/3,
         call_contract/6]).

-record(state, {trees   :: aec_trees:trees(),
                height  :: height(),
                account :: pubkey(),            %% the contract account
                nonce   :: non_neg_integer()
                    %% the nonce of the contract account, cached to avoid having
                    %% to update the tree at each external call
               }).

-type chain_state() :: #state{}.

%% -- API --------------------------------------------------------------------

%% @doc Create a chain state.
-spec new_state(aec_trees:trees(), height(), pubkey()) -> chain_state().
new_state(Trees, Height, ContractAccount) ->
    Contract = aect_state_tree:get_contract(ContractAccount, aec_trees:contracts(Trees)),
    Nonce    = aect_contracts:nonce(Contract),
    #state{ trees   = Trees,
            height  = Height,
            account = ContractAccount,
            nonce   = Nonce }.

%% @doc Get the state trees from a state.
-spec get_trees(chain_state()) -> aec_trees:trees().
get_trees(#state{ trees = Trees, account = Key, nonce = Nonce }) ->
    CTree0    = aec_trees:contracts(Trees),
    Contract0 = aect_state_tree:get_contract(Key, CTree0),
    Contract1 = aect_contracts:set_nonce(Nonce, Contract0),
    CTree1    = aect_state_tree:enter_contract(Contract1, CTree0),
    aec_trees:set_contracts(Trees, CTree1).


%% @doc Get the balance of the contract account.
-spec get_balance(pubkey(), chain_state()) -> non_neg_integer().
get_balance(PubKey, #state{ trees = Trees }) ->
    do_get_balance(PubKey, Trees).

%% @doc Spend money from the contract account.
-spec spend(pubkey(), non_neg_integer(), chain_state()) ->
          {ok, chain_state()} | {error, term()}.
spend(Recipient, Amount, State = #state{ trees   = Trees,
                                         height  = Height,
                                         account = ContractKey }) ->
    case do_spend(Recipient, ContractKey, Amount, Trees, Height) of
        {ok, Trees1}     -> {ok, State#state{ trees = Trees1 }};
        Err = {error, _} -> Err
    end.

%% @doc Call another contract.
-spec call_contract(pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {ok, aec_vm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(Target, Gas, Value, CallData, CallStack,
              State = #state{ trees   = Trees,
                              height  = Height,
                              account = ContractKey,
                              nonce   = Nonce }) ->
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    VmVersion = 1,  %% TODO
    {ok, CallTx} =
        aect_call_tx:new(#{ caller     => ContractKey,
                            nonce      => Nonce,
                            contract   => Target,
                            vm_version => VmVersion,
                            fee        => 0,
                            amount     => Value,
                            gas        => Gas,
                            gas_price  => 0,
                            call_data  => CallData,
                            call_stack => CallStack }),
    case aetx:check_from_contract(CallTx, Trees, Height, ConsensusVersion) of
        Err = {error, _} -> Err;
        {ok, Trees1} ->
            {ok, Trees2} = aetx:process_from_contract(CallTx, Trees1, Height, ConsensusVersion),
            CallId  = aect_call:id(ContractKey, Nonce, Target),
            Call    = aect_state_tree:get_call(Target, CallId, aec_trees:contracts(Trees2)),
            GasUsed = aect_call:gas_used(Call),
            Result  = case aect_call:return_value(Call) of
                          %% TODO: currently we don't set any sensible return value on exceptions
                          <<>> -> aec_vm_chain_api:call_exception(out_of_gas, GasUsed);
                          Bin when is_binary(Bin) ->
                            aec_vm_chain_api:call_result(Bin, GasUsed)
                      end,
            {ok, Result, State#state{ trees = Trees2, nonce = Nonce + 1 }}
    end.


%% -- Internal functions -----------------------------------------------------

do_get_balance(PubKey, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    AccountsTree  = aec_trees:accounts(Trees),
    case aect_state_tree:lookup_contract(PubKey, ContractsTree) of
        {value, Contract} -> aect_contracts:balance(Contract);
        none              ->
            case aec_accounts_trees:lookup(PubKey, AccountsTree) of
                none             -> 0;
                {value, Account} -> aec_accounts:balance(Account)
            end
    end.

%% TODO: can only spend to proper accounts. Not other contracts.
%% Note that we cannot use an aec_spend_tx here, since we are spending from a
%% contract account and not a proper account.
do_spend(Recipient, ContractKey, Amount, Trees, Height) ->
    try
        ContractsTree   = aec_trees:contracts(Trees),
        Contract        = aect_state_tree:get_contract(ContractKey, ContractsTree),
        Balance         = aect_contracts:balance(Contract),
        [ throw(no_funds) || Balance < Amount ],
        Trees1          = ensure_recipient_account(Recipient, Trees, Height),
        AccountsTree    = aec_trees:accounts(Trees1),
        {value, RecipAcc} = aec_accounts_trees:lookup(Recipient, AccountsTree),
        {ok, RecipAcc1} = aec_accounts:earn(RecipAcc, Amount, Height),
        AccountsTree1   = aec_accounts_trees:enter(RecipAcc1, AccountsTree),
        Contract1       = aect_contracts:spend(Amount, Contract),
        ContractsTree1  = aect_state_tree:enter_contract(Contract1, ContractsTree),
        Trees2          = aec_trees:set_accounts(Trees1, AccountsTree1),
        {ok, aec_trees:set_contracts(Trees2, ContractsTree1)}
    catch throw:bad_recip  -> {error, {bad_recipient_account, Recipient}};
          throw:bad_height -> {error, {account_height_too_big, Recipient}};
          throw:no_funds   -> {error, insufficient_funds}
    end.

ensure_recipient_account(Recipient, Trees, Height) when byte_size(Recipient) =:= 65 ->
    case aec_trees:ensure_account_at_height(Recipient, Trees, Height) of
        {ok, Trees1} -> Trees1;
        {error, account_height_too_big} -> throw(bad_height)
    end;
ensure_recipient_account(_,_Trees,_Height) ->
    throw(bad_recip).

