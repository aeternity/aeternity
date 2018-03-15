%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aevm_chain_api.
%%% @end
%%%=============================================================================
-module(aevm_chain).

-include_lib("apps/aecore/include/common.hrl").

-behaviour(aevm_chain_api).

-export([new_state/3]).

%% aevm_chain_api callbacks
-export([get_balance/1, spend/3]).

-record(state, {trees   :: aec_trees:trees(),
                height  :: height(),
                account :: pubkey()     %% the contract account
               }).

-type chain_state() :: #state{}.

%% -- API --------------------------------------------------------------------

%% @doc Create a chain state.
-spec new_state(aec_trees:trees(), height(), pubkey()) -> chain_state().
new_state(Trees, Height, ContractAccount) ->
    #state{ trees   = Trees,
            height  = Height,
            account = ContractAccount }.

%% @doc Get the balance of the contract account.
-spec get_balance(chain_state()) -> non_neg_integer().
get_balance(#state{ trees = Trees, account = PubKey }) ->
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

%% -- Internal functions -----------------------------------------------------

do_get_balance(ContractKey, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    Contract      = aect_state_tree:get_contract(ContractKey, ContractsTree),
    aect_contracts:balance(Contract).

%% TODO: can only spend to proper accounts. Not other contracts.
%% Note that we cannot use an aec_spend_tx here, since we are spending from a
%% contract account and not a proper account.
do_spend(Recipient, ContractKey, Amount, Trees, Height) ->
    try
        ContractsTree = aec_trees:contracts(Trees),
        AccountsTree  = aec_trees:accounts(Trees),
        Contract      = aect_state_tree:get_contract(ContractKey, ContractsTree),
        Balance       = aect_contracts:balance(Contract),
        [ throw(no_funds) || Balance < Amount ],
        RecipAccount  =
            case aec_accounts_trees:lookup(Recipient, AccountsTree) of
                none          ->
                    io:format("~p\n", [AccountsTree]),
                    throw(bad_recip);
                {value, Acct} -> Acct
            end,
        {ok, RecipAccount1} = aec_accounts:earn(RecipAccount, Amount, Height),
        AccountsTree1       = aec_accounts_trees:enter(RecipAccount1, AccountsTree),
        Contract1           = aect_contracts:set_balance(Balance - Amount, Contract),
        ContractsTree1      = aect_state_tree:enter_contract(Contract1, ContractsTree),
        Trees1              = aec_trees:set_contracts(aec_trees:set_accounts(Trees, AccountsTree1),
                                                      ContractsTree1),
        {ok, Trees1}
    catch _:bad_recip -> {error, {bad_recipient_account, Recipient}};
          _:no_funds  -> {error, insufficient_funds}
    end.

