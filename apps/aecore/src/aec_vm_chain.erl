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
          oracle_extend_tx/3,
          oracle_extend/3,
          oracle_get_answer/3,
          oracle_get_question/3,
          oracle_query_tx/6,
          oracle_query/2,
          oracle_query_fee/2,
          oracle_query_format/2,
          oracle_query_response_ttl/3,
          oracle_register_tx/7,
          oracle_register/3,
          oracle_respond_tx/5,
          oracle_respond/3,
          oracle_response_format/2,
          aens_resolve/4,
          aens_preclaim_tx/3,
          aens_preclaim/3,
          aens_claim_tx/4,
          aens_claim/3,
          aens_transfer_tx/4,
          aens_transfer/3,
          aens_revoke_tx/3,
          aens_revoke/3,
          spend_tx/3,
          spend/2,
          get_contract_fun_types/4
        ]).

-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

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

-spec spend_tx(aec_id:id(), non_neg_integer(), chain_state()) ->
    {ok, aetx:tx()}.
spend_tx(RecipientId, Amount, State = #state{ account = ContractKey }) ->
    Nonce = next_nonce(State),
    %% Note: The spend is from the contract's account.
    SenderId = aec_id:create(account, ContractKey),
    Spec =
        #{ sender_id    => SenderId
         , recipient_id => RecipientId
         , amount       => Amount
         , fee          => 0
         , nonce        => Nonce
         , payload      => <<>> },
    aec_spend_tx:new(Spec).

%% @doc Spend money from the contract account.
-spec spend(aetx:tx(), chain_state()) ->
    {ok, chain_state()} | {error, term()}.
spend(Tx, State) ->
    apply_transaction(Tx, State).

%%    Oracle
-spec oracle_register_tx(aec_keys:pubkey(), non_neg_integer(), aeo_oracles:ttl(),
                         aeso_sophia:type(), aeso_sophia:type(), pos_integer(),
                         chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
oracle_register_tx(AccountKey, QueryFee, TTL, QFormat, RFormat, VMVersion, State) ->
    on_chain_only(State, fun() -> oracle_register_tx_(AccountKey, QueryFee, TTL,
                                                      QFormat, RFormat,
                                                      VMVersion, State)
                         end).

oracle_register_tx_(AccountKey, QueryFee, TTL, QFormat,
                    RFormat, VMVersion, State) ->
    Nonce = next_nonce(AccountKey, State),
    %% Note: The nonce of the account is incremented.
    %% This means that if you register an oracle for an account other than
    %% the contract account through a contract that contract nonce is incremented
    %% "behind your back".
    BinaryQueryFormat = aeso_heap:to_binary(QFormat),
    BinaryResponseFormat = aeso_heap:to_binary(RFormat),
    Spec =
        #{account_id      => aec_id:create(account, AccountKey),
          nonce           => Nonce,
          query_format    => BinaryQueryFormat,
          response_format => BinaryResponseFormat,
          query_fee       => QueryFee,
          oracle_ttl      => TTL,
          vm_version      => VMVersion,
          ttl             => 0, %% Not used.
          fee             => 0},
    aeo_register_tx:new(Spec).

-spec oracle_register(aetx:tx(), binary(), chain_state()) ->
    {ok, aec_keys:pubkey(), chain_state()} | {error, term()}.
oracle_register(Tx, Signature, State) ->
    on_chain_only(State, fun() -> oracle_register_(Tx, Signature, State) end).

oracle_register_(Tx, Signature, State = #state{account = ContractKey}) ->
    {aeo_register_tx, OTx} = aetx:specialize_callback(Tx),
    AccountKey = aeo_register_tx:account_pubkey(OTx),
    Result =
        case check_account_signature(AccountKey, ContractKey, Signature) of
            ok                -> apply_transaction(Tx, State);
            Err_ = {error, _} -> Err_
        end,
    case Result of
        {ok, State1}     -> {ok, AccountKey, State1};
        Err = {error, _} -> Err
    end.

oracle_query_tx(Oracle, Q, Value, QTTL, RTTL, State) ->
    on_chain_only(State, fun() -> oracle_query_tx_(Oracle, Q, Value, QTTL, RTTL, State) end).

oracle_query_tx_(Oracle, Q, Value, QTTL, RTTL,
                State = #state{account = ContractKey}) ->
    Nonce = next_nonce(State),
    QueryData = maybe_convert_oracle_arg(Oracle, Q, State),
    Spec =
        #{sender_id     => aec_id:create(account, ContractKey),
          nonce         => Nonce,
          oracle_id     => aec_id:create(oracle, Oracle),
          query         => QueryData,
          query_fee     => Value,
          query_ttl     => QTTL,
          response_ttl  => RTTL,
          fee           => 0,
          ttl           => 0 %% Not used
         },
    aeo_query_tx:new(Spec).

oracle_query(Tx, State) ->
    on_chain_only(State, fun() -> oracle_query_(Tx, State) end).

oracle_query_(Tx, State) ->
    case apply_transaction(Tx, State) of
        {ok, State1} ->
            {oracle_query_tx, OTx} = aetx:specialize_type(Tx),
            Id = aeo_query_tx:query_id(OTx),
            {ok, Id, State1};
        {error, _} = E -> E
    end.

-spec oracle_respond_tx(aec_keys:pubkey(), aeo_query:id(), binary(),
                        aeo_oracles:relative_ttl(), chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
oracle_respond_tx(Oracle, QueryId, Response, ResponseTTL, State) ->
    on_chain_only(State, fun() -> oracle_respond_tx_(Oracle, QueryId, Response, ResponseTTL, State) end).

oracle_respond_tx_(Oracle, QueryId, Response0, ResponseTTL, State) ->
    Nonce = next_nonce(Oracle, State),
    Response = maybe_convert_oracle_arg(Oracle, Response0, State),
    Spec =
        #{oracle_id    => aec_id:create(oracle, Oracle),
          nonce        => Nonce,
          query_id     => QueryId,
          response     => Response,
          response_ttl => ResponseTTL,
          fee          => 0,
          ttl          => 0 %% Not used
         },
    aeo_response_tx:new(Spec).

%% ABI encode data if the oracle has the sophia vm version.
%% For no vm version, keep the string as is.
maybe_convert_oracle_arg(OracleId, Arg, State) ->
    Trees = get_top_trees(State),
    case aeo_state_tree:lookup_oracle(OracleId, aec_trees:oracles(Trees)) of
        {value, Oracle} ->
            case aeo_oracles:vm_version(Oracle) of
                ?AEVM_NO_VM ->
                    Arg;
                ?AEVM_01_Sophia_01 ->
                    aeso_heap:to_binary(Arg)
            end;
        none ->
            %% Will fail later
            Arg
    end.


-spec oracle_respond(aetx:tx(), binary(), chain_state()) ->
    prim_op_result().
oracle_respond(Tx, Signature, State) ->
    on_chain_only(State, fun() -> oracle_respond_(Tx, Signature, State) end).

oracle_respond_(Tx, Signature, State = #state{ account = ContractKey }) ->
    {aeo_response_tx, OTx} = aetx:specialize_callback(Tx),
    OracleKey = aeo_response_tx:oracle_pubkey(OTx),
    QueryId = aeo_response_tx:query_id(OTx),
    Bin = <<QueryId/binary, ContractKey/binary>>,
    case check_signature(OracleKey, ContractKey, Bin, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec oracle_extend_tx(aec_keys:pubkey(), aeo_oracles:ttl(), chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
oracle_extend_tx(Oracle, TTL, State) ->
    on_chain_only(State, fun() -> oracle_extend_tx_(Oracle, TTL, State) end).

oracle_extend_tx_(Oracle, TTL, State) ->
    Nonce = next_nonce(Oracle, State),
    Spec =
        #{oracle_id  => aec_id:create(oracle, Oracle),
          nonce      => Nonce,
          oracle_ttl => TTL,
          fee        => 0,
          ttl        => 0 %% Not used
         },
    aeo_extend_tx:new(Spec).

-spec oracle_extend(aetx:tx(), binary(), chain_state()) ->
    prim_op_result().
oracle_extend(Tx, Signature, State) ->
    on_chain_only(State, fun() -> oracle_extend_(Tx,Signature, State) end).

oracle_extend_(Tx, Signature, State = #state{ account = ContractKey }) ->
    {aeo_extend_tx, OTx} = aetx:specialize_callback(Tx),
    OracleKey = aeo_extend_tx:oracle_pubkey(OTx),
    case check_account_signature(OracleKey, ContractKey, Signature) of
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
                    VMVersion = aeo_oracles:vm_version(Oracle),
                    case oracle_typerep(VMVersion, ResponseFormat) of
                        {ok, Type} when VMVersion =:= ?AEVM_01_Sophia_01 ->
                            try aeso_heap:from_binary(Type, Answer) of
                                {ok, Result} -> {ok, {some, Result}};
                                {error, _} -> {error, bad_answer}
                            catch _:_ ->
                                    {error, bad_answer}
                            end;
                        {ok, string} when VMVersion =:= ?AEVM_NO_VM ->
                            %% We treat the anwer as a non-sophia string
                            {ok, {some, Answer}};
                        {error, bad_typerep} ->
                            {error, bad_typerep}
                    end
            end;
        none ->
            {ok, none}
    end.

oracle_get_question(OracleId, QueryId, #state{trees = ChainTrees}) ->
    Trees = get_on_chain_trees(ChainTrees),
    OraclesTree = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_query(OracleId, QueryId, OraclesTree) of
        {value, Query} ->
            {value, Oracle} = aeo_state_tree:lookup_oracle(OracleId, OraclesTree),
            QueryFormat     = aeo_oracles:query_format(Oracle),
            VMVersion       = aeo_oracles:vm_version(Oracle),
            case oracle_typerep(VMVersion, QueryFormat) of
                {ok, string} when VMVersion =:= ?AEVM_NO_VM ->
                    %% We treat the question as a non-sophia string
                    {ok, aeo_query:query(Query)};
                {ok, QueryType} ->
                    try aeso_heap:from_binary(QueryType, aeo_query:query(Query)) of
                        {ok, Question} ->
                            {ok, Question};
                        {error, _} ->
                            {error, bad_question}
                    catch _:_ ->
                            {error, bad_question}
                    end;
                {error, bad_typerep} ->
                    {error, bad_typerep}
            end;
        none ->
            {error, no_such_question}
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
            VMVersion = aeo_oracles:vm_version(O),
            oracle_typerep(VMVersion, BinaryFormat);
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
            oracle_typerep(aeo_oracles:vm_version(O),
                           aeo_oracles:response_format(O));
        none ->
            {error, no_such_oracle}
    end.

oracle_typerep(?AEVM_NO_VM,_BinaryFormat) ->
    %% Treat this as a string
    {ok, string};
oracle_typerep(?AEVM_01_Sophia_01, BinaryFormat) ->
    try aeso_heap:from_binary(typerep, BinaryFormat) of
        {ok, Format} -> {ok, Format};
        {error, _} -> {error, bad_typerep}
    catch
        _:_ -> {error, bad_typerep}
    end.

check_account_signature(AKey, CKey, Signature) ->
    check_signature(AKey, CKey, <<AKey/binary, CKey/binary>>, Signature).

check_name_signature(AKey, Hash, CKey, Signature) ->
    check_signature(AKey, CKey, <<AKey/binary, Hash/binary, CKey/binary>>, Signature).

check_signature(AKey, AKey, _Binary, _Signature) -> ok;
check_signature(AKey, _CKey, Binary, Signature) ->
    BinaryForNetwork = aec_governance:add_network_id(Binary),
    case enacl:sign_verify_detached(Signature, BinaryForNetwork, AKey) of
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

-spec aens_preclaim_tx(binary(), binary(), chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
aens_preclaim_tx(Addr, CHash, State) ->
    on_chain_only(State, fun() -> aens_preclaim_tx_(Addr, CHash, State) end).

aens_preclaim_tx_(Addr, CHash, State) ->
    Nonce = next_nonce(Addr, State),
    Spec =
        #{ account_id    => aec_id:create(account, Addr),
           nonce         => Nonce,
           commitment_id => aec_id:create(commitment, CHash),
           fee           => 0 },
    aens_preclaim_tx:new(Spec).

-spec aens_preclaim(aetx:tx(), binary(), chain_state()) ->
    prim_op_result().
aens_preclaim(Tx, Signature, State) ->
    on_chain_only(State, fun() -> aens_preclaim_(Tx, Signature, State) end).

aens_preclaim_(Tx, Signature, #state{ account = ContractKey} = State) ->
    {aens_preclaim_tx, NSTx} = aetx:specialize_callback(Tx),
    Addr = aec_id:specialize(aens_preclaim_tx:account_id(NSTx), account),
    case check_account_signature(Addr, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec aens_claim_tx(aec_keys:pubkey(), binary(), integer(), chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
aens_claim_tx(Addr, Name, Salt, State) ->
    on_chain_only(State, fun() -> aens_claim_tx_(Addr, Name, Salt, State) end).

aens_claim_tx_(Addr, Name, Salt, State) ->
    Nonce = next_nonce(Addr, State),
    Spec =
        #{ account_id => aec_id:create(account, Addr),
           nonce      => Nonce,
           name       => Name,
           name_salt  => Salt,
           fee        => 0
         },
    aens_claim_tx:new(Spec).

-spec aens_claim(aetx:tx(), binary(), chain_state()) ->
    prim_op_result().
aens_claim(Tx, Signature, State) ->
    on_chain_only(State, fun() -> aens_claim_(Tx, Signature, State) end).

aens_claim_(Tx, Signature, #state{ account = ContractKey } = State) ->
    {aens_claim_tx, NSTx} = aetx:specialize_callback(Tx),
    Addr = aec_id:specialize(aens_claim_tx:account_id(NSTx), account),
    Name = aens_claim_tx:name(NSTx),
    {ok, Hash} = aens:get_name_hash(Name),
    case check_name_signature(Addr, Hash, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec aens_transfer_tx(aec_keys:pubkey(), aec_keys:pubkey(), binary(), chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
aens_transfer_tx(FromAddr, ToAddr, Hash, State) ->
    on_chain_only(State, fun() -> aens_transfer_tx_(FromAddr, ToAddr, Hash, State) end).

aens_transfer_tx_(FromAddr, ToAddr, Hash, State) ->
    Nonce = next_nonce(FromAddr, State),
    Spec =
        #{ account_id   => aec_id:create(account, FromAddr),
           nonce        => Nonce,
           name_id      => aec_id:create(name, Hash),
           recipient_id => aec_id:create(account, ToAddr),
           fee          => 0
         },
    aens_transfer_tx:new(Spec).

-spec aens_transfer(aetx:tx(), binary(), chain_state()) ->
    prim_op_result().
aens_transfer(Tx, Signature, State) ->
    on_chain_only(State, fun() -> aens_transfer_(Tx, Signature, State) end).

aens_transfer_(Tx, Signature, #state{ account = ContractKey } = State) ->
    {aens_transfer_tx, NSTx} = aetx:specialize_callback(Tx),
    FromAddr = aec_id:specialize(aens_transfer_tx:account_id(NSTx), account),
    Hash = aec_id:specialize(aens_transfer_tx:name_id(NSTx), name),
    case check_name_signature(FromAddr, Hash, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

-spec aens_revoke_tx(aec_keys:pubkey(), binary(), chain_state()) ->
    {ok, aetx:tx()} | {error, term()}.
aens_revoke_tx(Addr, Hash, State) ->
    on_chain_only(State, fun() -> aens_revoke_tx_(Addr, Hash, State) end).

aens_revoke_tx_(Addr, Hash, State) ->
    Nonce = next_nonce(Addr, State),
    Spec =
        #{ account_id => aec_id:create(account, Addr),
           nonce      => Nonce,
           name_id    => aec_id:create(name, Hash),
           fee        => 0
         },
    aens_revoke_tx:new(Spec).

-spec aens_revoke(aetx:tx(), binary(), chain_state()) ->
    prim_op_result().
aens_revoke(Tx, Signature, State) ->
    on_chain_only(State, fun() -> aens_revoke_(Tx, Signature, State) end).

aens_revoke_(Tx, Signature, #state{ account = ContractKey } = State) ->
    {aens_revoke_tx, NSTx} = aetx:specialize_callback(Tx),
    Addr = aec_id:specialize(aens_revoke_tx:account_id(NSTx), account),
    Hash = aec_id:specialize(aens_revoke_tx:name_id(NSTx), name),
    case check_name_signature(Addr, Hash, ContractKey, Signature) of
        ok               -> apply_transaction(Tx, State);
        Err = {error, _} -> Err
    end.

%%    Contracts

%% @doc Get the type signature of a remote contract
get_contract_fun_types(Target, VMVersion, TypeHash, State) ->
    Trees = get_top_trees(State),
    CT = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Target, CT, [no_store]) of  %% no store
        {value, Contract} ->
            case aect_contracts:vm_version(Contract) of
                VMVersion ->
                    SerializedCode = aect_contracts:code(Contract),
                    #{type_info := TypeInfo} = aect_sophia:deserialize(SerializedCode),
                    aeso_abi:typereps_from_type_hash(TypeHash, TypeInfo);
                Other ->
                    {error, {wrong_vm_version, Other}}
            end;
        none ->
            {error, {no_such_contract, Target}}
    end.


%% @doc Call another contract.
-spec call_contract(aec_keys:pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {aevm_chain_api:call_result(), chain_state()}.
call_contract(Target, Gas, Value, CallData, CallStack,
              State = #state{account = ContractKey}) ->
    Trees = get_top_trees(State),
    CT = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(Target, CT, [no_store]) of  %% skip store, we look it up later
        none ->
            {aevm_chain_api:call_exception(unknown_contract, Gas), State};
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
            apply_call_transaction(CallTx, Gas, State)
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
        case aect_state_tree:lookup_contract(PubKey, ContractsTree, [no_store]) of
            {value, Contract} -> aect_contracts:set_state(Store, Contract)
        end,
    aect_state_tree:enter_contract(NewContract, ContractsTree).

apply_call_transaction(Tx, Gas, #state{tx_env = Env} = State) ->
    Trees = get_top_trees(State),
    case aetx:custom_apply(check_and_process_call, Tx, Trees, Env) of
        {ok, Call, Trees1} ->
            State1 = set_top_trees(State, Trees1),
            GasUsed = aect_call:gas_used(Call),
            case aect_call:return_type(Call) of
                error ->
                    Bin = aect_call:return_value(Call),
                    ReturnAtom = binary_to_error(Bin),
                    {aevm_chain_api:call_exception(ReturnAtom, GasUsed), State1};
                revert ->
                    Bin = aect_call:return_value(Call),
                    {aevm_chain_api:call_revert(Bin, GasUsed), State1};
                ok ->
                    Bin = aect_call:return_value(Call),
                    {aevm_chain_api:call_result(Bin, GasUsed), State1}
            end;
        {error, Atom} when is_atom(Atom) ->
            ReturnAtom = binary_to_error(atom_to_binary(Atom, utf8)),
            {aevm_chain_api:call_exception(ReturnAtom, Gas), State}
    end.

apply_transaction(Tx, #state{tx_env = Env } = State) ->
    Trees = get_top_trees(State),
    case aetx:process(Tx, Trees, Env) of
        {ok, Trees1} ->
            State1 = set_top_trees(State, Trees1),
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
    prim_op_result() | {ok, aec_keys:pubkey(), aetx:tx(), chain_trees()}.
on_chain_only(State, Fun) ->
    #state{trees = ChainTrees} = State,
    case is_channel_call(ChainTrees) of
        true ->
            {error, not_allowed_off_chain};
        false -> Fun()
    end.

%% c.f. aect_dispatch:error_to_binary/1
binary_to_error(<<"bad_call_data">>) -> bad_call_data;
binary_to_error(<<"out_of_gas">>) -> out_of_gas;
binary_to_error(<<"out_of_stack">>) -> out_of_stack;
binary_to_error(<<"not_allowed_off_chain">>) -> not_allowed_off_chain;
binary_to_error(<<"reentrant_call">>) -> reentrant_call;
binary_to_error(<<"unknown_function">>) -> unknown_function;
binary_to_error(<<"unknown_contract">>) -> unknown_contract;
binary_to_error(<<"unknown_error">>) -> unknown_error;
binary_to_error(E) ->
    ?DEBUG_LOG("**** Unknown error: ~p\n", [E]),
    unknown_error.
