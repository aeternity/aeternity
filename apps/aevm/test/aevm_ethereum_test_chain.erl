%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aec_vm_chain_api. Has a predefined state. To
%%%    use when testing contracts in the ethereum test suite.
%%% @end
%%%=============================================================================

-module(aevm_ethereum_test_chain).

-behaviour(aec_vm_chain_api).

-export([new_state/1]).

%% aec_vm_chain_api callbacks
-export([get_balance/2,
         spend/3,
         call_contract/6]).

new_state(State) -> State.

get_balance(A, #{ pre := Chain} = S) ->
    Account = maps:get(A, Chain, #{}),
    Balance = maps:get(balance, Account, 0),
    Balance.
spend(_Recipient, _Amount, _S)  -> {error, cant_spend_with_dummy_chain}.
call_contract(_, _, _, _, _, _) -> {error, cant_call_contracts_with_dummy_chain}.

