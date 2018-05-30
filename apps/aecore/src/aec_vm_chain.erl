%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aevm_chain_api.
%%% @end
%%%=============================================================================
-module(aec_vm_chain).

-behaviour(aevm_chain_api).

-export([new_state/3, get_trees/1]).

%% aevm_chain_api callbacks
-export([ call_contract/6,
          get_balance/2,
          get_store/1,
          set_store/2,
          oracle_extend/5,
          oracle_get_answer/3,
          oracle_get_question/3,
          oracle_query/6,
          oracle_query_fee/2,
          oracle_query_spec/2,
          oracle_register/7,
          oracle_respond/5,
          oracle_response_spec/2,
          spend/3
        ]).



-record(state, { trees   :: aec_trees:trees()
               , height  :: aec_blocks:height()
               , account :: aec_keys:pubkey()            %% the contract account
               }).

-type chain_state() :: #state{}.

-define(PUB_SIZE, 32).

%% -- API --------------------------------------------------------------------

%% @doc Create a chain state.
-spec new_state(aec_trees:trees(), aec_blocks:height(), aec_keys:pubkey()) -> chain_state().
new_state(Trees, Height, ContractAccount) ->
    #state{ trees   = Trees,
            height  = Height,
            account = ContractAccount
          }.

%% @doc Get the state trees from a state.
-spec get_trees(chain_state()) -> aec_trees:trees().
get_trees(#state{ trees = Trees}) ->
    Trees.

%% @doc Get the balance of the contract account.
-spec get_balance(aec_keys:pubkey(), chain_state()) -> non_neg_integer().
get_balance(PubKey, #state{ trees = Trees }) ->
    do_get_balance(PubKey, Trees).

%% @doc Get the contract state store of the contract account.
-spec get_store(chain_state()) -> aevm_chain_api:store().
get_store(#state{ account = PubKey, trees = Trees }) ->
    Store = do_get_store(PubKey, Trees),
    Store.

%% @doc Set the contract state store of the contract account.
-spec set_store(aevm_chain_api:store(), chain_state()) -> chain_state().
set_store(Store,  #state{ account = PubKey, trees = Trees } = State) ->
    CTree1 = do_set_store(Store, PubKey, Trees),
    Trees1 = aec_trees:set_contracts(Trees, CTree1),
    State#state{ trees = Trees1 }.


%% -- Primops ----------------------------
%%    Account

%% @doc Spend money from the contract account.
-spec spend(aec_keys:pubkey(), non_neg_integer(), chain_state()) ->
          {ok, chain_state()} | {error, term()}.
spend(Recipient, Amount, State = #state{ trees   = Trees,
                                         height  = Height,
                                         account = ContractKey }) ->
    case do_spend(Recipient, ContractKey, Amount, Trees, Height) of
        {ok, Trees1}     -> {ok, State#state{ trees = Trees1 }};
        Err = {error, _} -> Err
    end.

%%    Oracle
-spec oracle_register(pubkey(), binary(), non_neg_integer(),
                      non_neg_integer(), binary(), binary(), chain_state()) ->
    {ok, pubkey(), chain_state()} | {error, term()}.
oracle_register(AccountKey,_Sign, QueryFee, TTL, QuerySpec, ResponseSpec,
                State = #state{ trees   = Trees,
                                height  = Height,
                                account = ContractKey}) ->

    AT = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(AccountKey, AT),
    %% Note: The nonce of the account is incremented.
    %% This means that if you register an oracle for an account other than
    %% the contract account through a contract that contract nonce is incremented
    %% "behind your back".
    Nonce = aec_accounts:nonce(Account) + 1,
    BinaryQuerySpec = aeso_data:to_binary(QuerySpec, 0),
    BinaryResponseSpec = aeso_data:to_binary(ResponseSpec, 0),
    Spec =
        #{account       => AccountKey,
          nonce         => Nonce,
          query_spec    => BinaryQuerySpec,
          response_spec => BinaryResponseSpec,
          query_fee     => QueryFee,
          ttl           => {delta, TTL},
          fee           => 0},

    %% TODO: To register an oracle for another account than the contract
    %%       we need a safe way to sign the register call.
    %%       It should probably do with sign(PubKey+Nonce)
    %%       Then we need to check that signature here.
    %% Registering an oracle on the contract is ok.
    Result =
        if AccountKey =:= ContractKey -> do_oracle_register(Spec, Height, Trees);
           true ->
                %% TODO: Check that Sign is correct for external accounts.
                {error, signature_check_failed}
        end,
    case Result of
        {ok, Trees1}     -> {ok, AccountKey, State#state{ trees = Trees1 }};
        Err = {error, _} -> Err
    end.



oracle_query(Oracle, Q, Value, QTTL, RTTL,
             State = #state{ trees   = Trees,
                             height  = Height,
                             account = ContractKey} = State) ->
    io:format("oracle_query(~p, ~p, ~p, ~p, ~p)\n", [Oracle, Q, Value, QTTL, RTTL]),
    AT = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(ContractKey, AT),
    Nonce = aec_accounts:nonce(Account) + 1,
    {ok, Tx} =
        aeo_query_tx:new(#{sender        => ContractKey,
                           nonce         => Nonce,
                           oracle        => Oracle,
                           query         => Q,
                           query_fee     => Value,
                           query_ttl     => {delta, QTTL},
                           response_ttl  => {delta, RTTL},
                           fee           => 0}),
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    case aetx:check_from_contract(Tx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            {ok, Trees2} =
                aetx:process_from_contract(Tx, Trees1, Height, ConsensusVersion),
            State1 = State#state{ trees = Trees2 },
            {oracle_query_tx, OTx} = aetx:specialize_type(Tx),
            Query = aeo_query:new(OTx, Height),
            Id = aeo_query:id(Query),

            {ok, Id, State1};

        {error, _} = E -> E
    end.

oracle_respond(Oracle, QueryId,_Sign, Response,
               #state{ trees   = Trees,
                       height  = Height,
                       account = ContractKey} = State) ->
    io:format("oracle_respond(~p, ~p, ~p, ~p)\n", [Oracle, QueryId,_Sign, Response]),
    %% TODO: Check signature
    AT = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(ContractKey, AT),
    Nonce = aec_accounts:nonce(Account) + 1,

    {ok, Tx} = aeo_response_tx:new(
                 #{oracle   => Oracle,
                   nonce    => Nonce,
                   query_id => QueryId,
                   response => aeso_data:to_binary(Response, 0),
                   fee      => 0}),

    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),

    case aetx:check_from_contract(Tx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            {ok, Trees2} = aetx:process_from_contract(Tx, Trees1, Height, ConsensusVersion),
            State1 = State#state{ trees = Trees2 },
            {ok, State1};
        {error, _} = E -> E
    end.

oracle_extend(Oracle,_Sign, Fee, TTL,
              State = #state{ trees   = Trees,
                              height  = Height,
                              account = ContractKey} = State) ->
    io:format("oracle_extend(~p, ~p, ~p)\n", [Oracle, _Sign, TTL]),
    AT = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(ContractKey, AT),
    Nonce = aec_accounts:nonce(Account) + 1,
    {ok, Tx} =
        aeo_extend_tx:new(#{oracle => Oracle,
                           nonce  => Nonce,
                           ttl    => {delta, TTL},
                           fee    => Fee}),
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    case aetx:check_from_contract(Tx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            {ok, Trees2} =
                aetx:process_from_contract(Tx, Trees1, Height, ConsensusVersion),
            State1 = State#state{ trees = Trees2 },
            {ok, State1};
        {error, _} = E -> E
    end.

oracle_get_answer(OracleId, QueryId, #state{ trees = Trees } =_State) ->
    case aeo_state_tree:lookup_query(OracleId, QueryId,
                                     aec_trees:oracles(Trees)) of
        {value, Query} ->
            case aeo_query:response(Query) of
                undefined -> {ok, none};
                Answer ->
                    {value, Oracle} = aeo_state_tree:lookup_oracle(OracleId,
                                                                   aec_trees:oracles(Trees)),
                    ResponseFormat = aeo_oracles:response_format(Oracle),
                    {ok, Type} = aeso_data:from_binary(typerep,  ResponseFormat),
                    io:format("RespFormat ~p~nType ~p~n", [ResponseFormat, Type]),
                    io:format("Answer ~p~n", [Answer]),
                    {ok, Result} = aeso_data:from_binary(Type, Answer),
                    {ok, {some, Result}}
            end;
        none ->
            {ok, none}
    end.

oracle_get_question(OracleId, QueryId, #state{trees = Trees} = _State) ->
    case aeo_state_tree:lookup_query(OracleId, QueryId,
                                     aec_trees:oracles(Trees)) of
        {value, Query} ->
            Question = aeo_query:query(Query),
            {ok, Question};
        none ->
            {ok, none}
    end.

oracle_query_fee(Oracle, #state{trees = Trees} =_State) ->
    case aeo_state_tree:lookup_oracle(Oracle, aec_trees:oracles(Trees)) of
        {value, O} ->
            Fee = aeo_oracles:query_fee(O),
            {ok, Fee};
        none  ->
            {ok, none}
    end.

oracle_query_spec(Oracle, #state{ trees   = Trees} =_State) ->
    case aeo_state_tree:lookup_oracle(Oracle, aec_trees:oracles(Trees)) of
        {value, O} ->
            BinaryFormat = aeo_oracles:query_format(O),
            try aeso_data:from_binary(0, typerep, BinaryFormat) of
                {ok, Format} -> {ok, Format};
                {error, _} = Error -> Error
            catch
                _:_ -> {error, bad_typerep}
            end;
        none ->
            {error, no_such_oracle}
    end.




oracle_response_spec(Oracle, #state{ trees   = Trees} =_State) ->
    case aeo_state_tree:lookup_oracle(Oracle, aec_trees:oracles(Trees)) of
        {value, O} ->
            BinaryFormat = aeo_oracles:response_format(O),
            try aeso_data:from_binary(0, typerep, BinaryFormat) of
                {ok, Format} -> {ok, Format};
                {error, _} = Error -> Error
            catch
                _:_ -> {error, bad_typerep}
            end;
        none ->
            {error, no_such_oracle}
    end.



%%    Contracts

%% @doc Call another contract.
-spec call_contract(aec_keys:pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {ok, aevm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(Target, Gas, Value, CallData, CallStack,
              State = #state{ trees   = Trees,
                              height  = Height,
                              account = ContractKey
                            }) ->
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    CT = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Target, CT) of
        {value, Contract} ->
            AT = aec_trees:accounts(Trees),
            {value, ContractAccount} = aec_accounts_trees:lookup(ContractKey, AT),
            Nonce = aec_accounts:nonce(ContractAccount) + 1,
            VmVersion = aect_contracts:vm_version(Contract),
            {ok, CallTx} =
                aect_call_tx:new(#{ caller     => ContractKey,
                                    nonce      => Nonce,
                                    contract   => Target,
                                    vm_version => VmVersion,
                                    fee        => 0,
                                    ttl        => Height,
                                    amount     => Value,
                                    gas        => Gas,
                                    gas_price  => 0,
                                    call_data  => CallData,
                                    call_stack => CallStack }),
            do_call_contract(CallTx, ContractKey, Target, Nonce, Trees, State, Height,
                             ConsensusVersion);
        none -> {error, {no_such_contract, Target}}
    end.

do_call_contract(CallTx, ContractKey, Target, Nonce, Trees,
                 State, Height, ConsensusVersion) ->
    case aetx:check_from_contract(CallTx, Trees, Height, ConsensusVersion) of
        Err = {error, _} -> Err;
        {ok, Trees1} ->
            {ok, Trees2} =
                aetx:process_from_contract(CallTx, Trees1,
                                           Height, ConsensusVersion),
            CallId  = aect_call:id(ContractKey, Nonce, Target),
            Call    = aect_call_state_tree:get_call(Target, CallId,
                                                    aec_trees:calls(Trees2)),
            GasUsed = aect_call:gas_used(Call),
            Result  =
                case aect_call:return_type(Call) of
                    %% TODO: currently we don't set any
                    %%       sensible return value on exceptions
                    error ->
                        aevm_chain_api:call_exception(out_of_gas, GasUsed);
                    ok ->
                        Bin = aect_call:return_value(Call),
                        aevm_chain_api:call_result(Bin, GasUsed)
                end,
            {ok, Result, State#state{ trees = Trees2}}
    end.


%% -- Internal functions -----------------------------------------------------

do_get_balance(PubKey, Trees) ->
    AccountsTree  = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(PubKey, AccountsTree) of
        none             -> 0;
        {value, Account} -> aec_accounts:balance(Account)
    end.

do_get_store(PubKey, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(PubKey, ContractsTree) of
        {value, Contract} -> aect_contracts:state(Contract);
        none              -> #{}
    end.

do_set_store(Store, PubKey, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    NewContract =
	case aect_state_tree:lookup_contract(PubKey, ContractsTree) of
	    {value, Contract} -> aect_contracts:set_state(Store, Contract)
	end,
    aect_state_tree:enter_contract(NewContract, ContractsTree).

do_spend(Recipient, ContractKey, Amount, Trees, Height) ->
    AccountTree = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(ContractKey, AccountTree),
    Nonce = aec_accounts:nonce(Account) + 1,
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    {ok, SpendTx} = aec_spend_tx:new(#{ sender => ContractKey
                                      , recipient => Recipient
                                      , amount => Amount
                                      , fee => 0
                                      , ttl => Height
                                      , nonce => Nonce
                                      , payload => <<>>}),
    case aetx:check_from_contract(SpendTx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            aetx:process_from_contract(SpendTx, Trees1, Height, ConsensusVersion);
        Error -> Error
    end.

do_oracle_register(Spec, Height, Trees) ->
    {ok, Tx} = aeo_register_tx:new(Spec),
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    case aetx:check_from_contract(Tx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            aetx:process_from_contract(Tx, Trees1, Height, ConsensusVersion);
        Error -> Error
    end.
