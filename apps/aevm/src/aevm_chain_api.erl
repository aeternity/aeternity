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
-callback spend(Recipient :: aec_id:id(),
                Amount    :: non_neg_integer(),
                State     :: chain_state()) -> {ok, chain_state()} | {error, term()}.

%% Get the current balance of an account.
-callback get_balance(Account :: pubkey(), State :: chain_state()) ->
    non_neg_integer().

%% -- Oracles --

-callback oracle_register(Account :: pubkey(),
                          Sign :: binary(),
                          QueryFee :: non_neg_integer(),
                          TTL :: non_neg_integer(),
                          DecodedQType :: aeso_sophia:type(),
                          DecodedRType :: aeso_sophia:type(),
                          ChainState :: chain_state()) ->
    {ok, OracleKey :: pubkey(), chain_state()} | {error, term()}.

-callback oracle_query(Oracle :: pubkey(),
                       Query :: term(),
                       Value :: non_neg_integer(),
                       QueryTTL :: non_neg_integer(),
                       ResponseTTL :: non_neg_integer(),
                       ChainState :: chain_state()) ->
    {ok, QueryId :: pubkey(), chain_state()} | {error, term()}.

-callback oracle_respond(Oracle :: pubkey(),
                         Query :: pubkey(),
                         Sign :: binary(),
                         Response :: term(),
                         ChainState :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback oracle_extend(Oracle :: pubkey(),
                        Sign :: binary(),
                        TTL :: non_neg_integer(),
                        ChainState :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback oracle_get_answer(Oracle :: pubkey(),
                            Query :: pubkey(),
                            ChainState :: chain_state()) ->
    {ok, none | {some, term()}} | {error, term()}.

-callback oracle_get_question(Oracle :: pubkey(),
                              Query :: pubkey(),
                              ChainState :: chain_state()) ->
    {ok, term()} | {error, term()}.


%% TODO: not here
-type type_spec() :: word | string
                   | {option, type_spec()}
                   | {list, type_spec()}
                   | {tuple, [type_spec()]}.

-callback oracle_query_spec(Oracle :: pubkey(),
                            ChainState :: chain_state()) ->
    {ok, type_spec()} | {error, term()}.

-callback oracle_response_spec(Oracle :: pubkey(),
                               ChainState :: chain_state()) ->
    {ok, type_spec()} | {error, term()}.

-callback oracle_query_fee(Oracle :: pubkey(),
                           ChainState :: chain_state()) ->
    {ok, non_neg_integer()} | {error, term()}.

%% -- Name Services --

-callback aens_resolve(Name :: binary(),
                       Key :: binary(),
                       Type :: aeso_sophia:type(),
                       ChainState :: chain_state()) ->
    {ok, none | {some, term()}} | {error, term()}.

-callback aens_preclaim(Addr  :: pubkey(),
                        CHash :: binary(),
                        Sign  :: binary(),
                        ChainState :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback aens_claim(Addr  :: pubkey(),
                     Name  :: binary(),
                     Salt  :: integer(),
                     Sign  :: binary(),
                     ChainState :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback aens_transfer(FromAddr   :: pubkey(),
                        ToAddr     :: pubkey(),
                        Hash       :: binary(),
                        Sign       :: binary(),
                        ChainState :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback aens_revoke(Addr       :: pubkey(),
                      Hash       :: binary(),
                      Sign       :: binary(),
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

