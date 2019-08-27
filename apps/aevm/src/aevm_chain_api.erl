%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    This module defines the callbacks required by the VM to interact with a
%%%    chain.
%%% @end
%%%=============================================================================
-module(aevm_chain_api).

-export_type([call_result/0, exception/0, store/0]).

-export([call_result/2, call_exception/2, call_revert/2,
         return_value/1, gas_spent/1]).

-type(pubkey() :: binary()).

%% @doc The state of the chain. Specific to the API implementation.
-type chain_state() :: any().

-type store() :: aect_contracts:store().

-type exception() :: atom().

-record(call_result, { result    :: binary() | exception()
                     , gas_spent :: non_neg_integer()
                     , type      :: exception | revert | ok
                     }).

-opaque call_result() :: #call_result{}.

%% -- Callback API -----------------------------------------------------------

%% -- Chain --

-callback get_height(State :: chain_state()) -> aec_blocks:height().

-callback blockhash(N :: non_neg_integer(), State :: chain_state()) ->
  aec_blocks:block_header_hash().

%% -- Accounts --

-callback spend_tx(Recipient :: aeser_id:id(),
                   Amount :: non_neg_integer(),
                   State :: chain_state()) ->
    {ok, aetx:tx()}.

%% Execute a spend transaction from the contract account.
-callback spend(Tx :: aetx:tx(), State :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

%% Get the current balance of an account.
-callback get_balance(Account :: pubkey(), State :: chain_state()) ->
    non_neg_integer().

%% -- Oracles --

-callback oracle_register_tx(Account :: pubkey(),
                            QueryFee :: non_neg_integer(),
                            TTL :: aeo_oracles:ttl(),
                            DecodedQType :: aeb_aevm_data:type(),
                            DecodedRType :: aeb_aevm_data:type(),
                            VMVersion :: pos_integer(),
                            ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback oracle_register(Tx :: aetx:tx(),
                          Signature :: binary(),
                          State :: chain_state()) ->
    {ok, OracleKey :: pubkey(), chain_state()} | {error, term()}.

-callback oracle_query_tx(Oracle :: pubkey(),
                          Query :: term(),
                          Value :: non_neg_integer(),
                          QueryTTL :: aeo_oracles:ttl(),
                          ResponseTTL :: aeo_oracles:ttl(),
                          ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback oracle_query(Tx :: aetx:tx(), State :: chain_state()) ->
    {ok, QueryId :: pubkey(), chain_state()} | {error, term()}.

-callback oracle_respond_tx(Oracle :: pubkey(),
                            Query :: pubkey(),
                            Response :: term(),
                            ResponseTTL :: aeo_oracles:ttl(),
                            ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback oracle_respond(Tx :: aetx:tx(),
                         Signature :: binary(),
                         State :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback oracle_extend_tx(Oracle :: pubkey(),
                           TTL :: aeo_oracles:ttl(),
                           ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback oracle_extend(Tx :: aetx:tx(),
                        Signature :: binary(),
                        State :: chain_state()) ->
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

-callback oracle_query_format(Oracle :: pubkey(),
                              ChainState :: chain_state()) ->
    {ok, type_spec()} | {error, term()}.

-callback oracle_response_format(Oracle :: pubkey(),
                                 ChainState :: chain_state()) ->
    {ok, type_spec()} | {error, term()}.

-callback oracle_query_fee(Oracle :: pubkey(),
                           ChainState :: chain_state()) ->
    {ok, non_neg_integer()} | {error, term()}.

-callback oracle_query_response_ttl(Oracle :: pubkey(),
                                    Query :: pubkey(),
                                    ChainState :: chain_state()) ->
    {ok, aeo_oracles:relative_ttl()} | {error, term()}.

-callback oracle_check(Oracle :: pubkey(),
                       QFormat :: aeb_aevm_data:type(),
                       RFormat :: aeb_aevm_data:type(),
                       ChainState :: chain_state()) ->
    {ok, non_neg_integer()} | {error, term()}.

-callback oracle_check_query(Oracle :: pubkey(),
                             Query :: pubkey(),
                             QFormat :: aeb_aevm_data:type(),
                             RFormat :: aeb_aevm_data:type(),
                             ChainState :: chain_state()) ->
    {ok, non_neg_integer()} | {error, term()}.

%% -- Address --
-callback addr_is_contract(Addr :: pubkey(), ChainState :: chain_state()) ->
    {ok, non_neg_integer()} | {error, term()}.

-callback addr_is_oracle(Addr :: pubkey(), ChainState :: chain_state()) ->
    {ok, non_neg_integer()} | {error, term()}.

%% -- Name Services --

-callback aens_resolve(Name :: binary(),
                       Key :: binary(),
                       Type :: aeb_aevm_data:type(),
                       ChainState :: chain_state()) ->
    {ok, none | {some, term()}} | {error, term()}.

-callback aens_preclaim_tx(Addr :: pubkey(),
                           CHash :: binary(),
                           ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback aens_preclaim(Tx :: aetx:tx(),
                        Signature :: binary(),
                        State :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback aens_claim_tx(Addr :: pubkey(),
                        Name :: binary(),
                        Salt :: integer(),
                        NameFee :: integer() | prelima,
                        ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback aens_claim(Tx :: aetx:tx(),
                     Signature :: binary(),
                     State :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback aens_transfer_tx(FromAddr :: pubkey(),
                           ToAddr :: pubkey(),
                           Hash :: binary(),
                           ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback aens_transfer(Tx :: aetx:tx(),
                        Signature :: binary(),
                        State :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

-callback aens_revoke_tx(Addr :: pubkey(),
                         Hash :: binary(),
                         ChainState :: chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.

-callback aens_revoke(Tx :: aetx:tx(),
                      Signature :: binary(),
                      State :: chain_state()) ->
    {ok, chain_state()} | {error, term()}.

%% Make a call to another contract.
-callback call_contract(Contract :: pubkey(),
                        Gas :: non_neg_integer(),
                        Value :: non_neg_integer(),
                        CallData :: binary(),
                        CallStack :: [non_neg_integer()],
                        Origin :: pubkey(),
                        State :: chain_state()) ->
                    {call_result(), chain_state()}.

-callback get_store(chain_state()) -> store().
-callback set_store(store(), chain_state()) -> chain_state().

-callback get_contract_fun_types(Target :: pubkey(),
                                 VMVersion :: aect_contracts:version(),
                                 TypeHash :: binary(),
                                 State :: chain_state()) ->
    {ok, ArgType::aeb_aevm_data:type(), OutType::aeb_aevm_data:type()} | {error, term()}.

%% -- Call results -----------------------------------------------------------

-spec call_result(binary(), non_neg_integer()) -> call_result().
call_result(Result, GasSpent) when is_binary(Result) ->
    #call_result{ result = Result, gas_spent = GasSpent, type = ok }.

-spec call_exception(exception(), non_neg_integer()) -> call_result().
call_exception(Exception, GasSpent) when is_atom(Exception) ->
    #call_result{ result = Exception, gas_spent = GasSpent, type = exception}.

-spec call_revert(binary(), non_neg_integer()) -> call_result().
call_revert(Slogan, GasSpent) when is_binary(Slogan)->
    #call_result{ result = Slogan, gas_spent = GasSpent, type = revert}.

-spec return_value(call_result()) -> {ok, binary()}
                                   | {exception, exception()}
                                   | {revert, binary()}.
return_value(#call_result{ result = Res, type = Type }) ->
    {Type, Res}.

-spec gas_spent(call_result()) -> non_neg_integer().
gas_spent(#call_result{ gas_spent = Gas }) -> Gas.
