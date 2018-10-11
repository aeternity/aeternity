%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Implementation of the aevm_chain_api.
%%% @end
%%%=============================================================================
-module(aec_vm_chain).

-behaviour(aevm_chain_api).

-export([new_state/3, get_trees/1,
         new_offchain_state/4
         ]).

%% aevm_chain_api callbacks
-export([ get_height/1,
          blockhash/2,
          call_contract/6,
          get_balance/2,
          get_store/1,
          set_store/2,
          oracle_extend/4,
          oracle_get_answer/3,
          oracle_get_question/3,
          oracle_query/6,
          oracle_query_fee/2,
          oracle_query_format/2,
          oracle_query_response_ttl/3,
          oracle_register/7,
          oracle_respond/6,
          oracle_response_format/2,
          aens_resolve/4,
          aens_preclaim/4,
          aens_claim/5,
          aens_transfer/5,
          aens_revoke/4,
          spend/3
        ]).

-include_lib("apps/aecore/include/blocks.hrl").

-define(NO_INNER_TREES, no_inner_trees).

-record(trees, { trees              :: aec_trees:trees()
               , is_onchain = true  :: boolean()
               , inner              :: chain_trees() | ?NO_INNER_TREES
               }).


-record(state, { trees              :: chain_trees()
               , tx_env             :: aetx_env:env()
               , account            :: aec_keys:pubkey() %% the contract account
               }).

-type chain_trees() :: #trees{}.
-type chain_state() :: #state{}.
-type prim_op_result() :: {ok, chain_state()} | {error, term()}.
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

%% @doc Create an on-chain state.
-spec new_state(aec_trees:trees(), aetx_env:env(), aec_keys:pubkey()) -> chain_state().
new_state(Trees, Env, ContractAccount) ->
    #state{ trees       = on_chain_trees(Trees),
            tx_env      = Env,
            account     = ContractAccount
          }.

%% @doc Create an off-chain state.
-spec new_offchain_state(aec_trees:trees(), aec_trees:trees(),
                         aetx_env:env(),
                         aec_keys:pubkey()) -> chain_state().
new_offchain_state(OffChainTrees, OnChainTrees, TxEnv, ContractAccount) ->
    InnerTrees = on_chain_trees(OnChainTrees),
    Trees = push_trees(off_chain_trees(OffChainTrees), InnerTrees),
    #state{ trees       = Trees,
            tx_env      = TxEnv,
            account     = ContractAccount
          }.

%% @doc Get the state trees from a state.
-spec get_trees(chain_state()) -> aec_trees:trees().
get_trees(State) ->
    get_top_trees(State).

%% @doc Get the chain height from a state.
get_height(#state{ tx_env = TxEnv }) ->
    aetx_env:height(TxEnv).

%% @doc Get the key hash at height, in the current fork
%%      NOTE: The check for the valid height is done before calling this
%%            function.
-spec blockhash(non_neg_integer(), chain_state()) -> aec_blocks:block_header_hash().
blockhash(H, #state{ tx_env = TxEnv}) ->
    case aetx_env:key_hash(TxEnv) of
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> = Hash ->
            %% For channels
            Hash;
        KeyHash ->
            {ok, Header} = aec_chain:get_key_header_by_height(H),
            {ok, Hash} = aec_headers:hash_header(Header),
            %% Make sure that this is an ancestor
            case aec_chain:find_common_ancestor(Hash, KeyHash) of
                {ok, Hash} -> Hash;
                {ok, _Other} -> traverse_to_key_hash(H, KeyHash)
            end
    end.

traverse_to_key_hash(H, KeyHash) ->
    {ok, Header} = aec_chain:get_header(KeyHash),
    case aec_headers:height(Header) of
        Height when Height =:= H -> KeyHash;
        Height when Height =:= H + 1 -> aec_headers:prev_key_hash(Header);
        _Height -> traverse_to_key_hash(H, aec_headers:prev_key_hash(Header))
    end.

%% @doc Get the balance of the contract account.
-spec get_balance(aec_keys:pubkey(), chain_state()) -> non_neg_integer().
get_balance(PubKey, #state{} = State) ->
    Get = fun(Trees) -> do_get_balance(PubKey, Trees) end,
    case get_from_state_or_inner(Get, State) of
        {error, not_found} -> 0;
        {ok, B} -> B
    end.

%% @doc Get the contract state store of the contract account.
-spec get_store(chain_state()) -> aevm_chain_api:store().
get_store(#state{ account = PubKey} = State) ->
    Trees = get_top_trees(State),
    Store = do_get_store(PubKey, Trees),
    Store.

%% @doc Set the contract state store of the contract account.
-spec set_store(aevm_chain_api:store(), chain_state()) -> chain_state().
set_store(Store,  #state{ account = PubKey} = State) ->
    Trees = get_top_trees(State),
    CTree1 = do_set_store(Store, PubKey, Trees),
    Trees1 = aec_trees:set_contracts(Trees, CTree1),
    set_top_trees(State, Trees1).


%% -- Primops ----------------------------
%%    Account

%% @doc Spend money from the contract account.
-spec spend(aec_id:id(), non_neg_integer(), chain_state()) ->
          {ok, chain_state()} | {error, term()}.
spend(RecipientId, Amount, State = #state{ account = ContractKey }) ->
    Nonce = next_nonce(State),
    %% Note: The spend is from the contract's account.
    SenderId = aec_id:create(account, ContractKey),
    {ok, SpendTx} = aec_spend_tx:new(#{ sender_id    => SenderId
                                      , recipient_id => RecipientId
                                      , amount       => Amount
                                      , fee          => 0
                                      , nonce        => Nonce
                                      , payload      => <<>>}),
    apply_transaction(SpendTx, State).

%%    Oracle
-spec oracle_register(aec_keys:pubkey(), binary(), non_neg_integer(),
                      aeo_oracles:ttl(), aeso_sophia:type(), aeso_sophia:type(), chain_state()) ->
    {ok, aec_keys:pubkey(), chain_state()} | {error, term()}.
oracle_register(AccountKey, Signature, QueryFee, TTL, QueryFormat, ResponseFormat, State) ->
    on_chain_only(State,
                  fun() ->
                      oracle_register_(AccountKey, Signature, QueryFee, TTL,
                                       QueryFormat, ResponseFormat, State)
                  end).

oracle_register_(AccountKey, Signature, QueryFee, TTL, QueryFormat, ResponseFormat,
                 State = #state{account = ContractKey}) ->
    Nonce = next_nonce(AccountKey, State),
    %% Note: The nonce of the account is incremented.
    %% This means that if you register an oracle for an account other than
    %% the contract account through a contract that contract nonce is incremented
    %% "behind your back".
    BinaryQueryFormat = aeso_data:to_binary(QueryFormat),
    BinaryResponseFormat = aeso_data:to_binary(ResponseFormat),
    Spec =
        #{account_id      => aec_id:create(account, AccountKey),
          nonce           => Nonce,
          query_format    => BinaryQueryFormat,
          response_format => BinaryResponseFormat,
          query_fee       => QueryFee,
          oracle_ttl      => TTL,
          ttl             => 0, %% Not used.
          fee             => 0},
    {ok, Tx} = aeo_register_tx:new(Spec),

    Result =
        case check_account_signature(AccountKey, ContractKey, Signature) of
            ok                -> apply_transaction(Tx, State);
            Err_ = {error, _} -> Err_
        end,
    case Result of
        {ok, State1}     -> {ok, AccountKey, State1};
        Err = {error, _} -> Err
    end.

oracle_query(Oracle, Q, Value, QTTL, RTTL,
             State = #state{ account = ContractKey }) ->
    Nonce = next_nonce(State),
    QueryData = aeso_data:to_binary(Q),
    {ok, Tx} =
        aeo_query_tx:new(#{sender_id     => aec_id:create(account, ContractKey),
                           nonce         => Nonce,
                           oracle_id     => aec_id:create(oracle, Oracle),
                           query         => QueryData,
                           query_fee     => Value,
                           query_ttl     => QTTL,
                           response_ttl  => RTTL,
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

-spec oracle_respond(aec_keys:pubkey(), aeo_query:id(), binary(), binary(),
                     aeo_oracles:relative_ttl(), chain_state()) ->
    prim_op_result().
oracle_respond(Oracle, QueryId, Signature, Response, ResponseTTL, State) ->
    on_chain_only(State,
                  fun() -> oracle_respond_(Oracle, QueryId, Signature,
                                           Response, ResponseTTL, State) end).

oracle_respond_(Oracle, QueryId, Signature, Response, ResponseTTL,
               State = #state{ account = ContractKey }) ->
    Nonce = next_nonce(Oracle, State),

    {ok, Tx} = aeo_response_tx:new(
                 #{oracle_id    => aec_id:create(oracle, Oracle),
                   nonce        => Nonce,
                   query_id     => QueryId,
                   response     => aeso_data:to_binary(Response),
                   response_ttl => ResponseTTL,
                   fee          => 0,
                   ttl          => 0 %% Not used
                  }),

    Bin = <<QueryId/binary, ContractKey/binary>>,
    case check_signature(Oracle, ContractKey, Bin, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec oracle_extend(aec_keys:pubkey(), binary(), aeo_oracles:ttl(), chain_state()) ->
    prim_op_result().
oracle_extend(Oracle, Signature, TTL, State) ->
    on_chain_only(State,
                  fun() -> oracle_extend_(Oracle, Signature, TTL, State) end).

oracle_extend_(Oracle, Signature, TTL, State = #state{ account = ContractKey }) ->
    Nonce = next_nonce(Oracle, State),
    {ok, Tx} =
        aeo_extend_tx:new(#{oracle_id  => aec_id:create(oracle, Oracle),
                            nonce      => Nonce,
                            oracle_ttl => TTL,
                            fee        => 0,
                            ttl        => 0 %% Not used
                           }),
    case check_account_signature(Oracle, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

oracle_get_answer(OracleId, QueryId, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
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

oracle_get_question(OracleId, QueryId, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
    OraclesTree = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_query(OracleId, QueryId, OraclesTree) of
        {value, Query} ->
            {ok, QueryType} = get_query_type(OracleId, OraclesTree),
            aeso_data:from_binary(QueryType, aeo_query:query(Query));
        none ->
            {ok, none}
    end.

oracle_query_fee(Oracle, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
    case aeo_state_tree:lookup_oracle(Oracle, aec_trees:oracles(Trees)) of
        {value, O} ->
            Fee = aeo_oracles:query_fee(O),
            {ok, Fee};
        none  ->
            {ok, none}
    end.

oracle_query_format(Oracle, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
    case aeo_state_tree:lookup_oracle(Oracle, aec_trees:oracles(Trees)) of
        {value, O} ->
            BinaryFormat = aeo_oracles:query_format(O),
            try aeso_data:from_binary(typerep, BinaryFormat) of
                {ok, Format} -> {ok, Format};
                {error, _} = Error -> Error
            catch
                _:_ -> {error, bad_typerep}
            end;
        none ->
            {error, no_such_oracle}
    end.

oracle_query_response_ttl(OracleId, QueryId, #state{trees = ChainTrees} =_State) ->
    Trees = get_on_chain_trees(ChainTrees),
    OraclesTree = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_query(OracleId, QueryId, OraclesTree) of
        {value, Query} ->
            {ok, aeo_query:response_ttl(Query)};
        none ->
            {error, no_such_oracle_query}
    end.

oracle_response_format(Oracle, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
    case aeo_state_tree:lookup_oracle(Oracle, aec_trees:oracles(Trees)) of
        {value, O} ->
            BinaryFormat = aeo_oracles:response_format(O),
            try aeso_data:from_binary(typerep, BinaryFormat) of
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

check_account_signature(AKey, CKey, Signature) ->
    check_signature(AKey, CKey, <<AKey/binary, CKey/binary>>, Signature).

check_name_signature(AKey, Hash, CKey, Signature) ->
    check_signature(AKey, CKey, <<AKey/binary, Hash/binary, CKey/binary>>, Signature).

check_signature(AKey, AKey, _Binary, _Signature) -> ok;
check_signature(AKey, _CKey, Binary, Signature) ->
    case enacl:sign_verify_detached(Signature, Binary, AKey) of
       {ok, _}    -> ok;
       {error, _} -> {error, signature_check_failed}
    end.

%%    AENS

aens_resolve(Name, Key, Type, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
    case aens:resolve(Key, Name, aec_trees:ns(Trees)) of
        {ok, Id}  ->
            {_IdType, IdValue} = aec_id:specialize(Id),
            decode_as(Type, IdValue);
        {error, _} ->
            {ok, none}
    end.

decode_as(word, <<N:256>>) -> {ok, {some, N}};
decode_as(string, Bin) when is_binary(Bin) -> {ok, {some, Bin}};
decode_as(Type, Val) ->
    ?DEBUG_LOG("Can't decode ~p as ~p\n", [Val, Type]),
    {error, out_of_gas}.

-spec aens_preclaim(binary(), binary(), binary(), chain_state()) ->
    prim_op_result().
aens_preclaim(Addr, CHash, Signature, State) ->
    on_chain_only(State,
                  fun() -> aens_preclaim_(Addr, CHash, Signature, State) end).

aens_preclaim_(Addr, CHash, Signature, #state{ account = ContractKey} = State) ->
    Nonce = next_nonce(Addr, State),
    {ok, Tx} =
        aens_preclaim_tx:new(#{ account_id    => aec_id:create(account, Addr),
                                nonce         => Nonce,
                                commitment_id => aec_id:create(commitment, CHash),
                                fee           => 0 }),

    case check_account_signature(Addr, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec aens_claim(aec_keys:pubkey(), binary(), integer(), binary(), chain_state()) ->
    prim_op_result().
aens_claim(Addr, Name, Salt, Signature, State) ->
    on_chain_only(State,
                  fun() -> aens_claim_(Addr, Name, Salt, Signature, State) end).

aens_claim_(Addr, Name, Salt, Signature, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(Addr, State),
    {ok, Tx} =
        aens_claim_tx:new(#{ account_id => aec_id:create(account, Addr),
                             nonce      => Nonce,
                             name       => Name,
                             name_salt  => Salt,
                             fee        => 0 }),

    {ok, Hash} = aens:get_name_hash(Name),
    case check_name_signature(Addr, Hash, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec aens_transfer(aec_keys:pubkey(), aec_keys:pubkey(), binary(), binary(), chain_state()) ->
    prim_op_result().
aens_transfer(FromAddr, ToAddr, Hash, Signature, State) ->
    on_chain_only(State,
                  fun() -> aens_transfer_(FromAddr, ToAddr, Hash, Signature, State) end).

aens_transfer_(FromAddr, ToAddr, Hash, Signature, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(FromAddr, State),
    {ok, Tx} =
        aens_transfer_tx:new(#{ account_id   => aec_id:create(account, FromAddr),
                                nonce        => Nonce,
                                name_id      => aec_id:create(name, Hash),
                                recipient_id => aec_id:create(account, ToAddr),
                                fee          => 0 }),
    case check_name_signature(FromAddr, Hash, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec aens_revoke(aec_keys:pubkey(), binary(), binary(), chain_state()) ->
    prim_op_result().
aens_revoke(Addr, Hash, Signature, State) ->
    on_chain_only(State,
                  fun() -> aens_revoke_(Addr, Hash, Signature, State) end).

aens_revoke_(Addr, Hash, Signature, #state{ account = ContractKey } = State) ->
    Nonce = next_nonce(Addr, State),
    {ok, Tx} =
        aens_revoke_tx:new(#{ account_id => aec_id:create(account, Addr),
                              nonce      => Nonce,
                              name_id    => aec_id:create(name, Hash),
                              fee        => 0 }),
    case check_name_signature(Addr, Hash, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

%%    Contracts

%% @doc Call another contract.
-spec call_contract(aec_keys:pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {ok, aevm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(Target, Gas, Value, CallData, CallStack,
              State = #state{account = ContractKey}) ->
    Trees = get_top_trees(State),
    CT = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Target, CT) of
        {value, Contract} ->
            AT = aec_trees:accounts(Trees),
            {value, ContractAccount} = aec_accounts_trees:lookup(ContractKey, AT),
            Nonce = aec_accounts:nonce(ContractAccount) + 1,
            VmVersion = aect_contracts:vm_version(Contract),
            {ok, CallTx} =
                aect_call_tx:new(#{ caller_id   => aec_id:create(contract, ContractKey),
                                    nonce       => Nonce,
                                    contract_id => aec_id:create(contract, Target),
                                    vm_version  => VmVersion,
                                    fee         => 0,
                                    amount      => Value,
                                    gas         => Gas,
                                    gas_price   => 0,
                                    call_data   => CallData,
                                    call_stack  => CallStack }),
            case apply_transaction(CallTx, State) of
                {ok, State1} ->
                    Trees1 = get_top_trees(State1),
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

-spec do_get_balance(aec_keys:pubkey(), aec_trees:trees()) ->
    {ok, non_neg_integer()} | {error, not_found}.
do_get_balance(PubKey, Trees) ->
    AccountsTree  = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(PubKey, AccountsTree) of
        none             -> {error, not_found};
        {value, Account} -> {ok, aec_accounts:balance(Account)}
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

apply_transaction(Tx, #state{tx_env = Env } = State) ->
    Trees = get_top_trees(State),
    case aetx:check(Tx, Trees, Env) of
        {ok, Trees1} ->
            {ok, Trees2} = aetx:process(Tx, Trees1, Env),
            State1 = set_top_trees(State, Trees2),
            {ok, State1};
        {error, _} = E -> E
    end.

next_nonce(State = #state{ account = ContractKey }) ->
    next_nonce(ContractKey, State).

next_nonce(Addr, State) ->
    Trees = get_top_trees(State),
    AT = aec_trees:accounts(Trees),
    {value, Account} = aec_accounts_trees:lookup(Addr, AT),
    aec_accounts:nonce(Account) + 1.

is_channel_call(#trees{is_onchain = false, inner = ?NO_INNER_TREES}) ->
    error(offchain_missing_onchain);
is_channel_call(#trees{is_onchain = false}) ->
    true;
is_channel_call(#trees{is_onchain = true}) ->
    false.

-spec get_from_state_or_inner(fun((aec_trees:trees()) -> {ok, term()} |{error, not_found}),
                              chain_state()) -> {ok,term()} | {error, not_found}.
get_from_state_or_inner(Fun, #state{trees = ChainTrees}) ->
    get_from_state_or_inner_(Fun, ChainTrees).

get_from_state_or_inner_(Fun, #trees{trees = Trees, inner = Inner} = ChainTrees) ->
    case is_channel_call(ChainTrees) of
        false -> Fun(Trees);
        true ->
            case Fun(Trees) of
                {error, not_found} ->
                    get_from_state_or_inner_(Fun, Inner);
                {ok, _Val} = OK  -> OK
            end
    end.

-spec on_chain_trees(aec_trees:trees()) -> chain_trees().
on_chain_trees(Trees) ->
    #trees{trees = Trees,
           is_onchain = true,
           inner = ?NO_INNER_TREES}.

-spec off_chain_trees(aec_trees:trees()) -> chain_trees().
off_chain_trees(Trees) ->
    #trees{trees = Trees,
           is_onchain = false,
           inner = ?NO_INNER_TREES}.

-spec push_trees(chain_trees(), chain_trees()) -> chain_trees().
push_trees(OuterTrees, InnerTrees) ->
    OuterTrees#trees{inner = InnerTrees}.

-spec get_top_trees(chain_state()) -> aec_trees:trees().
get_top_trees(#state{trees = #trees{trees = Trees}}) ->
    Trees.

set_top_trees(#state{trees = T} = State, Trees) ->
    State#state{trees = T#trees{trees = Trees}}.

get_on_chain_trees(#trees{is_onchain = true, trees = Trees}) ->
    Trees;
get_on_chain_trees(#trees{is_onchain = false, inner = Inner}) ->
    get_on_chain_trees(Inner).

-spec on_chain_only(chain_state(),
                    fun(() -> prim_op_result() | {ok, aec_keys:pubkey(), chain_trees()})) ->
    prim_op_result() | {ok, aec_keys:pubkey(), chain_trees()}.
on_chain_only(State, Fun) ->
    #state{trees = ChainTrees} = State,
    case is_channel_call(ChainTrees) of
        true ->
            {error, not_allowed_off_chain};
        false -> Fun()
    end.
