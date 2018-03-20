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
-export([get_balance/1,
         spend/3,
         call_contract/6]).

new_state() -> no_state.

get_balance(_S)                 -> 0.
spend(_Recipient, _Amount, _S)  -> {error, cant_spend_with_dummy_chain}.
call_contract(_, _, _, _, _, _) -> {error, cant_call_contracts_with_dummy_chain}.

