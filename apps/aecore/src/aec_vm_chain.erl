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
          oracle_extend/4,
          oracle_get_answer/3,
          oracle_get_question/3,
          oracle_query/6,
          oracle_query_fee/2,
          oracle_query_spec/2,
          oracle_register/7,
          oracle_respond/5,
          oracle_response_spec/2,
          aens_resolve/4,
          aens_preclaim/4,
          aens_claim/5,
          aens_transfer/5,
          aens_revoke/4,
          spend/3
        ]).



-record(state, { trees   :: aec_trees:trees()
               , height  :: aec_blocks:height()
               , account :: aec_keys:pubkey()            %% the contract account
               }).

-type chain_state() :: #state{}.

-define(PUB_SIZE, 32).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-define(DEBUG_LOG(Format, Data), begin lager:debug(Format, Data), ?TEST_LOG(Format, Data) end).
-else.
-define(TEST_LOG(Format, Data), ok).
-define(DEBUG_LOG(Format, Data), lager:debug(Format, Data)).
-endif.

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
-spec spend(aec_id:id(), non_neg_integer(), chain_state()) ->
          {ok, chain_state()} | {error, term()}.
spend(Recipient, Amount, State = #state{ account = ContractKey }) ->
    Nonce = next_nonce(State),
    %% Note: The spend is from the contract's account.
    Sender = aec_id:create(account, ContractKey),
    {ok, SpendTx} = aec_spend_tx:new(#{ sender => Sender
                                      , recipient => Recipient
                                      , amount => Amount
                                      , fee => 0
                                      , nonce => Nonce
                                      , payload => <<>>}),
    apply_transaction(SpendTx, State).

%%    Oracle
-spec oracle_register(aec_keys:pubkey(), binary(), non_neg_integer(),
                      non_neg_integer(), aeso_sophia:type(), aeso_sophia:type(), chain_state()) ->
    {ok, aec_keys:pubkey(), chain_state()} | {error, term()}.
oracle_register(AccountKey,_Sign, QueryFee, TTL, QuerySpec, ResponseSpec,
                State = #state{account = ContractKey}) ->
    Nonce = next_nonce(AccountKey, State),
    %% Note: The nonce of the account is incremented.
    %% This means that if you register an oracle for an account other than
    %% the contract account through a contract that contract nonce is incremented
    %% "behind your back".
    BinaryQuerySpec = aeso_data:to_binary(QuerySpec, 0),
    BinaryResponseSpec = aeso_data:to_binary(ResponseSpec, 0),
    Spec =
        #{account       => aec_id:create(account, AccountKey),
          nonce         => Nonce,
          query_spec    => BinaryQuerySpec,
          response_spec => BinaryResponseSpec,
          query_fee     => QueryFee,
          oracle_ttl    => {delta, TTL},
          ttl           => 0, %% Not used.
          fee           => 0},
    {ok, Tx} = aeo_register_tx:new(Spec),

    %% TODO: To register an oracle for another account than the contract
    %%       we need a safe way to sign the register call.
    %%       It should probably do with sign(PubKey+Nonce)
    %%       Then we need to check that signature here.
    %% Registering an oracle on the contract is ok.
    Result =
        if AccountKey =:= ContractKey -> apply_transaction(Tx, State);
           true ->
                %% TODO: Check that Sign is correct for external accounts.
                {error, signature_check_failed}
        end,
    case Result of
        {ok, State1}     -> {ok, AccountKey, State1};
        Err = {error, _} -> Err
    end.



oracle_query(Oracle, Q, Value, QTTL, RTTL,
             State = #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(State),
    QueryData = aeso_data:to_binary(Q, 0),
    {ok, Tx} =
        aeo_query_tx:new(#{sender        => aec_id:create(account, ContractKey),
                           nonce         => Nonce,
                           oracle        => aec_id:create(oracle, Oracle),
                           query         => QueryData,
                           query_fee     => Value,
                           query_ttl     => {delta, QTTL},
                           response_ttl  => {delta, RTTL},
                           fee           => 0,
                           ttl           => 0 %% Not used
                          }),
    case apply_transaction(Tx, State) of
        {ok, State1} ->
            {oracle_query_tx, OTx} = aetx:specialize_type(Tx),
            Id = aeo_query_tx:query_id(OTx),
            {ok, Id, State1};
        {error, _} = E -> E
    end.

oracle_respond(Oracle, QueryId,_Sign, Response, State) ->
    %% TODO: Check signature
    Nonce = next_nonce(Oracle, State),

    {ok, Tx} = aeo_response_tx:new(
                 #{oracle   => aec_id:create(oracle, Oracle),
                   nonce    => Nonce,
                   query_id => QueryId,
                   response => aeso_data:to_binary(Response, 0),
                   fee      => 0,
                   ttl      => 0 %% Not used
                  }),

    apply_transaction(Tx, State).

oracle_extend(Oracle,_Sign, TTL, State) ->
    Nonce = next_nonce(Oracle, State),
    {ok, Tx} =
        aeo_extend_tx:new(#{oracle     => aec_id:create(oracle, Oracle),
                            nonce      => Nonce,
                            oracle_ttl => {delta, TTL},
                            fee        => 0,
                            ttl        => 0 %% Not used
                           }),
    apply_transaction(Tx, State).

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
                    {ok, Result} = aeso_data:from_binary(Type, Answer),
                    {ok, {some, Result}}
            end;
        none ->
            {ok, none}
    end.

oracle_get_question(OracleId, QueryId, #state{trees = Trees} = _State) ->
    OraclesTree = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_query(OracleId, QueryId, OraclesTree) of
        {value, Query} ->
            {ok, QueryType} = get_query_type(OracleId, OraclesTree),
            aeso_data:from_binary(QueryType, aeo_query:query(Query));
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

get_query_type(OracleId, OraclesTree) ->
    {value, Oracle} = aeo_state_tree:lookup_oracle(OracleId, OraclesTree),
    QueryFormat     = aeo_oracles:query_format(Oracle),
    aeso_data:from_binary(typerep, QueryFormat).

%%    AENS

aens_resolve(Name, Key, Type, #state{ trees = Trees } = _State) ->
    case aens:resolve(list_to_atom(binary_to_list(Key)), Name, aec_trees:ns(Trees)) of
        {ok, Val}  -> decode_as(Type, Val);
        {error, _} -> {ok, none}
    end.

decode_as(word, <<N:256>>) -> {ok, {some, N}};
decode_as(string, Bin) when is_binary(Bin) -> {ok, {some, Bin}};
decode_as(Type, Val) ->
    ?DEBUG_LOG("Can't decode ~p as ~p\n", [Val, Type]),
    {error, out_of_gas}.

aens_preclaim(Addr, CHash, _Sign, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(Addr, State),
    {ok, Tx} =
        aens_preclaim_tx:new(#{ account    => aec_id:create(account, Addr),
                                nonce      => Nonce,
                                commitment => aec_id:create(commitment, CHash),
                                fee        => 0 }),
    case Addr =:= ContractKey of
        true  -> apply_transaction(Tx, State);
        false -> %% TODO: check signature for external account
            {error, signature_check_failed}
    end.

aens_claim(Addr, Name, Salt, _Sign, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(Addr, State),
    {ok, Tx} =
        aens_claim_tx:new(#{ account    => aec_id:create(account, Addr),
                             nonce      => Nonce,
                             name       => Name,
                             name_salt  => Salt,
                             fee        => 0 }),
    case Addr =:= ContractKey of
        true  -> apply_transaction(Tx, State);
        false -> %% TODO: check signature for external account
            {error, signature_check_failed}
    end.

aens_transfer(FromAddr, ToAddr, Hash, _Sign, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(FromAddr, State),
    {ok, Tx} =
        aens_transfer_tx:new(#{ account           => aec_id:create(account, FromAddr),
                                nonce             => Nonce,
                                name_hash         => aec_id:create(name, Hash),
                                recipient_account => aec_id:create(account, ToAddr),
                                fee               => 0 }),
    case FromAddr =:= ContractKey of
        true  -> apply_transaction(Tx, State);
        false -> %% TODO: check signature for external account
            {error, signature_check_failed}
    end.

aens_revoke(Addr, Hash, _Sign, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(Addr, State),
    {ok, Tx} =
        aens_revoke_tx:new(#{ account   => aec_id:create(account, Addr),
                              nonce     => Nonce,
                              name_hash => aec_id:create(name, Hash),
                              fee       => 0 }),
    case Addr =:= ContractKey of
        true  -> apply_transaction(Tx, State);
        false -> %% TODO: check signature for external account
            {error, signature_check_failed}
    end.

%%    Contracts

%% @doc Call another contract.
-spec call_contract(aec_keys:pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {ok, aevm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(Target, Gas, Value, CallData, CallStack,
              State = #state{ trees   = Trees,
                              account = ContractKey
                            }) ->
    CT = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Target, CT) of
        {value, Contract} ->
            AT = aec_trees:accounts(Trees),
            {value, ContractAccount} = aec_accounts_trees:lookup(ContractKey, AT),
            Nonce = aec_accounts:nonce(ContractAccount) + 1,
            VmVersion = aect_contracts:vm_version(Contract),
            {ok, CallTx} =
                aect_call_tx:new(#{ caller     => aec_id:create(contract, ContractKey),
                                    nonce      => Nonce,
                                    contract   => aec_id:create(contract, Target),
                                    vm_version => VmVersion,
                                    fee        => 0,
                                    amount     => Value,
                                    gas        => Gas,
                                    gas_price  => 0,
                                    call_data  => CallData,
                                    call_stack => CallStack }),
            case apply_transaction(CallTx, State) of
                {ok, State1 = #state{ trees = Trees1 }} ->
                    CallId  = aect_call:id(ContractKey, Nonce, Target),
                    Call    = aect_call_state_tree:get_call(Target, CallId,
                                                            aec_trees:calls(Trees1)),
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
                    {ok, Result, State1};
                {error, _} = E -> E
            end;
        none -> {error, {no_such_contract, Target}}
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

apply_transaction(Tx, #state{ trees = Trees, height = Height } = State) ->
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    case aetx:check_from_contract(Tx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            {ok, Trees2} =
                aetx:process_from_contract(Tx, Trees1, Height, ConsensusVersion),
            State1 = State#state{ trees = Trees2 },
            {ok, State1};
        {error, _} = E -> E
    end.

next_nonce(State = #state{ account = ContractKey }) ->
    next_nonce(ContractKey, State).

next_nonce(Addr, #state{ trees = Trees }) ->
    AT = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(Addr, AT),
    aec_accounts:nonce(Account) + 1.

