%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Dummy implementation of the aevm_chain_api. Doesn't keep any state. To
%%%    use when testing contracts that don't interact with the chain.
%%% @end
%%%=============================================================================

-module(aevm_dummy_chain).

-behaviour(aevm_chain_api).

-export([new_state/0]).

%% aevm_chain_api callbacks
-export([get_balance/2,
         get_store/1,
         set_store/2,
         spend/3,
         call_contract/7]).

new_state() -> no_state.

get_balance(_, _S)              -> 0.
get_store(_)                    -> #{}.
set_store(_,S)                  -> S.
spend(_Recipient, _Amount, _S)  -> {error, cant_spend_with_dummy_chain}.
call_contract(_, Gas, _, _, _, _, State) ->
    {aevm_chain_api:call_exception(cant_call_contracts_with_dummy_chain, Gas), State}.

