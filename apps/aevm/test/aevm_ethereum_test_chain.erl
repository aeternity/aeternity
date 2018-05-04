%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aec_vm_chain_api. Has a predefined state. To
%%%    use when testing contracts in the ethereum test suite.
%%% @end
%%%=============================================================================

-module(aevm_ethereum_test_chain).

%-behaviour(aec_vm_chain_api).

-export([new_state/1]).

%% aec_vm_chain_api callbacks
-export([get_balance/2,
	 get_store/1,
	 set_store/2,
         spend/3,
         call_contract/6]).

-include_lib("apps/aecore/include/common.hrl").

-type chain_state() :: map().

-spec new_state(map()) -> chain_state().
new_state(State) -> State.

-spec get_balance(pubkey(), chain_state()) -> non_neg_integer().
get_balance(A, #{ pre := Chain} =_S) ->
    Account = maps:get(A, Chain, #{}),
    Balance = maps:get(balance, Account, 0),
    Balance.

-spec get_store(chain_state()) -> map().
get_store(#{ env :=_Env,
	     exec := Exec,
	     pre := Pre }) ->
    Address = maps:get(address, Exec),
    case maps:get(Address, Pre, undefined) of
        undefined -> #{};
        #{storage := _} = S -> aevm_eeevm_store:to_binary(S)
    end.

-spec set_store(Store::binary(), chain_state()) -> chain_state().
set_store(Store, State) ->
    maps:put(storage, Store, State).

-spec spend(pubkey(), non_neg_integer(), chain_state()) ->
          {ok, chain_state()} | {error, term()}.
spend(_Recipient, _Amount, _S)  -> {error, cant_spend_with_dummy_chain}.
-spec call_contract(pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {ok, aec_vm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(_, _, _, _, _, _) -> {error, cant_call_contracts_with_dummy_chain}.

