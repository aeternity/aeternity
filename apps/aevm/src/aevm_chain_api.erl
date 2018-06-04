%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    This module defines the callbacks required by the VM to interact with a
%%%    chain.
%%% @end
%%%=============================================================================
-module(aevm_chain_api).

-export_type([call_result/0, exception/0, store/0]).

-export([call_result/2, call_exception/2,
         return_value/1, gas_spent/1]).

-type(pubkey() :: binary()).

%% @doc The state of the chain. Specific to the API implementation.
-type chain_state() :: any().

-type store() :: #{binary() => binary()}.

-type exception() :: out_of_gas.

-record(call_result, { result    :: binary() | exception()
                     , gas_spent :: non_neg_integer() }).

-opaque call_result() :: #call_result{}.

%% -- Callback API -----------------------------------------------------------

%% -- Accounts --

%% Execute a spend transaction from the contract account.
-callback spend(Recipient :: pubkey(),
                Amount    :: non_neg_integer(),
                State     :: chain_state()) -> {ok, chain_state()} | {error, term()}.

%% Get the current balance of an account.
-callback get_balance(Account :: pubkey(), State :: chain_state()) ->
    non_neg_integer().

%% -- Oracles --

-callback oracle_register(Account :: pubkey(),
                          Sign :: binary(),
                          TTL :: non_neg_integer(),
                          DecodedQType :: binary(),
                          DecodedRType :: binary(),
                          ChainState :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.


%% Make a call to another contract.
-callback call_contract(Contract  :: pubkey(),
                        Gas       :: non_neg_integer(),
                        Value     :: non_neg_integer(),
                        CallData  :: binary(),
                        CallStack :: [non_neg_integer()],
                        State     :: chain_state()) ->
                    {ok, call_result(), chain_state()} | {error, term()}.

-callback get_store(chain_state()) -> store().
-callback set_store(store(), chain_state()) -> chain_state().

%% -- Call results -----------------------------------------------------------

-spec call_result(binary(), non_neg_integer()) -> call_result().
call_result(Result, GasSpent) ->
    #call_result{ result = Result, gas_spent = GasSpent }.

-spec call_exception(exception(), non_neg_integer()) -> call_result().
call_exception(Exception, GasSpent) ->
    #call_result{ result = Exception, gas_spent = GasSpent }.

-spec return_value(call_result()) -> {ok, binary()} | {error, exception()}.
return_value(#call_result{ result = Res }) when is_binary(Res) ->
    {ok, Res};
return_value(#call_result{ result = Err }) ->
    {error, Err}.

-spec gas_spent(call_result()) -> non_neg_integer().
gas_spent(#call_result{ gas_spent = Gas }) -> Gas.

