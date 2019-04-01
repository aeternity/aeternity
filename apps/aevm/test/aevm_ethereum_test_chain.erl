%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aevm_chain_api. Has a predefined state. To
%%%    use when testing contracts in the ethereum test suite.
%%% @end
%%%=============================================================================

-module(aevm_ethereum_test_chain).

%% Commented for avoiding warnings without implementing all dummy callback. %% -behaviour(aevm_chain_api).

-export([new_state/1]).

%% aevm_chain_api callbacks
-export([get_balance/2,
         get_store/1,
         set_store/2,
         call_contract/7]).

-define(MASK160, ((1 bsl 160) -1)).

-type chain_state() :: map().

-spec new_state(map()) -> chain_state().
new_state(State) -> State.

-spec get_balance(aec_keys:pubkey(), chain_state()) -> non_neg_integer().
get_balance(<<A:256>>, #{ pre := Chain} =_S) ->
    Account = maps:get(A band ?MASK160, Chain, #{}),
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

-spec call_contract(aec_keys:pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], aec_keys:pubkey(), chain_state()) ->
        {ok, aevm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(_, Gas, _, _, _, _, State) ->
    {aevm_chain_api:call_exception(cant_call_contracts_with_dummy_chain, Gas), State}.

