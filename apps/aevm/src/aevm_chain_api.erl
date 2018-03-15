%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    This module defines the callbacks required by a VM to interact with the
%%%    chain.
%%% @end
%%%=============================================================================
-module(aevm_chain_api).

-include_lib("apps/aecore/include/common.hrl").

%% @doc The state of the chain. Specific to the API implementation.
-type chain_state() :: any().

%% @doc Execute a spend transaction from the contract account.
-callback spend(Recipient :: pubkey(),
                Amount    :: non_neg_integer(),
                State     :: chain_state()) -> {ok, chain_state()} | {error, term()}.

%% @doc Get the current balance of the contract account.
-callback get_balance(State :: chain_state()) -> non_neg_integer().

