%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Chain API for FATE
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_chain_api).

-export([ new/1
        ]).

%% Getters
-export([ account_balance/2
        , next_nonce/2
        , beneficiary/1
        , blockhash/2
        , put_contract/2
        , contract_vm_version/2
        , contract_fate_bytecode/2
        , contract_find_final_ref/2
        , contract_store/2
        , difficulty/1
        , final_trees/1
        , gas_limit/1
        , gas_price/1
        , fee/1
        , generation/1
        , origin/1
        , creator/2
        , set_contract_store/3
        , timestamp_in_msecs/1
        , tx_env/1
        , tx_event_op/3
        ]).

%% Modifiers
-export([ spend/4
        , transfer_value/4
        , remove_contract/2
        , oracle_extend/4
        , oracle_get_answer/5
        , oracle_get_question/5
        , oracle_register/8
        , oracle_query/11
        , oracle_respond/7
        , oracle_expiry/2
        , oracle_query_fee/2
        , oracle_check/4
        , oracle_check_query/5
        , is_oracle/2
        , is_contract/2
        , is_payable/2
        , aens_claim/5
        , aens_preclaim/4
        , aens_resolve/3
        , aens_revoke/3
        , aens_transfer/4
        , aens_update/6
        , aens_lookup/2
        , eval_primops/2
        ]).

-export([ check_delegation_signature/4
        , is_onchain/1
        ]).

-export_type([ state/0
             ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecore/include/blocks.hrl").

%%%-------------------------------------------------------------------
%%% NOTE: We accept that this module causes havoc in the dependency
%%%       graph for now. The state/chain handling will move down to
%%%       a lower level in the dependency graph later.
%%%-------------------------------------------------------------------

-record(state, { primop_state :: aeprimop_state:state()
               , gas_price    :: non_neg_integer()
               , fee    :: non_neg_integer()
               , onchain_primop_state :: {onchain, aeprimop_state:state()} | none
               , origin       :: binary()
               }).

-type state() :: #state{}.
-type pubkey() :: <<_:256>>.

-define(IS_ONCHAIN(S), (S#state.onchain_primop_state =:= none)).

%%===================================================================
%%% API
%%% ===================================================================

%%%-------------------------------------------------------------------
%%% External API for infrastructure

-spec new(map()) -> state().
new(#{ gas_price := GasPrice
     , fee := Fee
     , origin := Origin
     , trees  := Trees
     , tx_env := TxEnv
     } = Env) ->
    State = #state{ primop_state         = aeprimop_state:new(Trees, TxEnv)
                  , gas_price            = GasPrice
                  , fee                  = Fee
                  , origin               = Origin
                  , onchain_primop_state = none
                  },
    %% If we are running in a state channel, we need the onchain trees as well.
    case maps:get(on_chain_trees, Env, none) of
        none ->
            State;
        OnchainTrees ->
            State#state{onchain_primop_state = {onchain, aeprimop_state:new(OnchainTrees, TxEnv)}}
    end.

-spec tx_env(state()) -> aetx_env:env().
tx_env(#state{primop_state = PState}) ->
    aeprimop_state:tx_env(PState).

-spec final_trees(state()) -> aec_trees:trees().
final_trees(#state{primop_state = PState}) ->
    aeprimop_state:final_trees(PState).

-spec is_onchain(state()) -> boolean().
is_onchain(#state{} = S) ->
    ?IS_ONCHAIN(S).

%%%-------------------------------------------------------------------
%%% Basic getters

-spec origin(state()) -> aeb_fate_data:fate_address().
origin(#state{origin = Origin}) ->
    aeb_fate_data:make_address(Origin).

-spec gas_price(state()) -> aeb_fate_data:fate_integer().
gas_price(#state{gas_price = GasPrice}) ->
    aeb_fate_data:make_integer(GasPrice).

-spec fee(state()) -> aeb_fate_data:fate_integer().
fee(#state{fee = Fee}) ->
    aeb_fate_data:make_integer(Fee).

-spec beneficiary(state()) -> aeb_fate_data:fate_address().
beneficiary(#state{} = S) ->
    aeb_fate_data:make_address(aetx_env:beneficiary(tx_env(S))).

-spec generation(state()) -> aeb_fate_data:fate_integer().
generation(#state{} = S) ->
    aeb_fate_data:make_integer(aetx_env:height(tx_env(S))).

-spec difficulty(state()) -> aeb_fate_data:fate_integer().
difficulty(#state{} = S) ->
    aeb_fate_data:make_integer(aetx_env:difficulty(tx_env(S))).

-spec gas_limit(state()) -> aeb_fate_data:fate_integer().
gas_limit(#state{}) ->
    %% Should be tied to height if this is changed.
    aeb_fate_data:make_integer(aec_governance:block_gas_limit()).

-spec timestamp_in_msecs(state()) -> aeb_fate_data:fate_integer().
timestamp_in_msecs(#state{} = S) ->
    aeb_fate_data:make_integer(aetx_env:time_in_msecs(tx_env(S))).

-spec set_contract_store(pubkey(), aect_contracts_store:store(), state()) ->
                                state().
set_contract_store(Pubkey, Store, #state{primop_state = PState} = S) ->
    {value, C} = aeprimop_state:find_contract_without_store(Pubkey, PState),
    C1         = aect_contracts:set_state(Store, C),
    PState1    = aeprimop_state:put_contract(C1, PState),
    S#state{primop_state = PState1}.

creator(Pubkey, #state{primop_state = PState}) ->
    {value, C} = aeprimop_state:find_contract_without_store(Pubkey, PState),
    aect_contracts:owner_pubkey(C).

%%%-------------------------------------------------------------------
%%% Slightly more involved getters with caching

-spec blockhash(non_neg_integer(), #state{}) -> aeb_fate_data:fate_hash().
blockhash(Height, #state{} = S) ->
    TxEnv = tx_env(S),
    case aetx_env:key_hash(TxEnv) of
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> = Hash ->
            %% For channels
            aeb_fate_data:make_hash(Hash);
        KeyHash ->
            {ok, Header} = aec_chain:get_key_header_by_height(Height),
            {ok, Hash} = aec_headers:hash_header(Header),
            %% Make sure that this is an ancestor
            case aec_chain:find_common_ancestor(Hash, KeyHash) of
                {ok, <<_:?BLOCK_HEADER_HASH_BYTES/unit:8 >> = Hash} ->
                    aeb_fate_data:make_hash(Hash);
                {ok, _Other} ->
                    <<_:?BLOCK_HEADER_HASH_BYTES/unit:8 >> = Hash =
                        traverse_to_key_hash(Height, KeyHash),
                    aeb_fate_data:make_hash(Hash)
            end
    end.

traverse_to_key_hash(H, KeyHash) ->
    {ok, Header} = aec_chain:get_header(KeyHash),
    case aec_headers:height(Header) of
        Height when Height =:= H -> KeyHash;
        Height when Height =:= H + 1 -> aec_headers:prev_key_hash(Header);
        _Height -> traverse_to_key_hash(H, aec_headers:prev_key_hash(Header))
    end.

-spec put_contract(aect_contracts:contract(), state()) -> state().
put_contract(Contract, #state{primop_state = PS} = S) ->
    PK = aect_contracts:pubkey(Contract),
    {Payable, PS0} = case aect_contracts:code(Contract) of
                  {code, SerCode} ->
                      #{payable := P} = aeser_contract_code:deserialize(SerCode),
                      {P, PS};
                  {ref, Clonee} ->
                      case aeprimop_state:find_account(aeser_id:specialize(Clonee, contract), PS) of
                          none -> error;
                          {Acc, PS_} -> {aec_accounts:is_payable(Acc), PS_}
                      end
                  end,
    PS1 = aeprimop_state:put_contract(Contract, PS0),
    PS2 = case aeprimop_state:find_account(PK, PS1) of
              none ->
                  Account = aec_accounts:new(PK, 0, [non_payable || not Payable]),
                  aeprimop_state:put_account(Account, PS1);
              {Account0, PS1_0} ->
                  Account1 = aec_accounts:set_payable(Account0, Payable),
                  aeprimop_state:put_account(Account1, PS1_0)
          end,
    S#state{primop_state = PS2}.

-spec contract_vm_version(pubkey(), state()) -> 'error' |
          {'ok', aect_contracts:vm_version()}.
contract_vm_version(Pubkey, #state{primop_state = PState}) ->
    case aeprimop_state:find_contract_without_store(Pubkey, PState) of
        none -> error;
        {value, Contract} -> aect_contracts:vm_version(Contract)
    end.

-spec contract_fate_bytecode(pubkey(), state()) -> 'error' |
                                                   {'ok', term(), aect_contracts:vm_version(), state()}.
contract_fate_bytecode(Pubkey, #state{primop_state = PState} = S0) ->
    case aeprimop_state:find_contract_without_store(Pubkey, PState) of
        none -> error;
        {value, Contract} ->
            case aect_contracts:vm_version(Contract) of
                VMV when ?IS_FATE_SOPHIA(VMV) ->
                    SerCode =
                        case aect_contracts:code(Contract) of
                            {code, Code} -> Code;
                            {ref, Ref} ->
                                RefContractPK = aeser_id:specialize(Ref, contract),
                                {value, RefContract} = aeprimop_state:find_contract_without_store(RefContractPK, PState),
                                case aect_contracts:code(RefContract) of
                                    {ref, RefBad} ->
                                        error({found_deep_reference, RefBad});
                                    {code, Code} -> Code
                                end
                        end,
                    #{ byte_code := ByteCode} = aect_sophia:deserialize(SerCode),
                    try aeb_fate_code:deserialize(ByteCode) of
                        FateCode -> {ok, FateCode, VMV, S0#state{primop_state = PState}}
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end
    end.

-spec contract_find_final_ref(pubkey(), state()) -> 'error' | {'ok', pubkey(), aect_contracts:vm_version()}.
contract_find_final_ref(Pubkey, #state{primop_state = PState} = S0) ->
    case aeprimop_state:find_contract_without_store(Pubkey, PState) of
        none -> error;
        {value, Contract} ->
            case aect_contracts:code(Contract) of
                {code, _} ->
                    {ok, Pubkey, aect_contracts:vm_version(Contract)};
                {ref, Ref} ->
                    RefContractPK = aeser_id:specialize(Ref, contract),
                    contract_find_final_ref(RefContractPK, S0)
            end
    end.

-spec remove_contract(pubkey(), state()) -> state().
remove_contract(Pubkey, #state{primop_state = PState0} = S0) ->
    PState1 = aeprimop_state:delete_contract(Pubkey, PState0),
    %% We don't delete the account as it could have existed before
    %% the contract was created
    S0#state{primop_state = PState1}.

-spec contract_store(pubkey(), state()) -> {aect_contracts_store:store(), state()}.
contract_store(Pubkey, #state{primop_state = PState} = S) ->
    %% If we are looking for the store, we are already running the contract,
    %% so we can boldly assume it exists and have the correct vm version.
    {Contract, PState1} = aeprimop_state:get_contract(Pubkey, PState),
    {aect_contracts:state(Contract), S#state{primop_state = PState1}}.

-spec account_balance(pubkey(), state()) -> 'error' |
                                            {'ok', aeb_fate_data:fate_integer(), state()}.
account_balance(Pubkey, #state{primop_state = PState,
                               onchain_primop_state = Onchain} = S) ->
    case aeprimop_state:find_account(Pubkey, PState) of
        {Account, PState1} ->
            Balance = aeb_fate_data:make_integer(aec_accounts:balance(Account)),
            {ok, Balance, S#state{primop_state = PState1}};
        none when Onchain =:= none ->
            error;
        none ->
            %% Try to find this onchain as well.
            {onchain, OPState} = Onchain,
            case aeprimop_state:find_account(Pubkey, OPState) of
                {Account, OPState1} ->
                    Balance = aeb_fate_data:make_integer(aec_accounts:balance(Account)),
                    {ok, Balance, S#state{onchain_primop_state = {onchain, OPState1}}};
                none ->
                    error
            end
    end.

-spec next_nonce(pubkey(), state()) -> 'error' |
                                       {'ok', integer(), state()}.
next_nonce(Pubkey, #state{primop_state = PState0,
                          onchain_primop_state = Onchain} = S) ->
    case aeprimop_state:find_account(Pubkey, PState0) of
        {Account, PState1} ->
            Nonce = aec_accounts:nonce(Account) + 1,
            case eval_primops(
                   [aeprimop:force_inc_account_nonce_op(Pubkey, Nonce)],
                   S#state{primop_state = PState1}) of
                {ok, S1} -> {ok, Nonce, S1};
                Err -> Err
            end;
        none when Onchain =:= none ->
            error;
        none ->
            %% Try to find this onchain as well.
            {onchain, OPState} = Onchain,
            case aeprimop_state:find_account(Pubkey, OPState) of
                {Account, OPState1} ->
                    Nonce = aec_accounts:nonce(Account) + 1,
                    case eval_primops(
                           [aeprimop:force_inc_account_nonce_op(Pubkey, Nonce)],
                           S#state{onchain_primop_state = {onchain, OPState1}}) of
                        {ok, S1} -> {ok, Nonce, S1};
                        Err -> Err
                    end;
                none ->
                    error
            end
    end.

-spec check_delegation_signature(pubkey(), binary(), binary(), state()) ->
                                        {'ok', state()} | 'error'.
check_delegation_signature(Pubkey, Binary, Signature,
                           #state{ primop_state = PState} = State) ->
    case aeprimop_state:find_account(Pubkey, PState) of
        {Account, PState1} ->
            case aec_accounts:type(Account) of
                generalized ->
                    error;
                basic ->
                    BinaryForNetwork = aec_governance:add_network_id(Binary),
                    case enacl:sign_verify_detached(Signature, BinaryForNetwork, Pubkey) of
                        {ok, _}    -> {ok, State#state{primop_state = PState1}};
                        {error, _} -> error
                    end
            end;
        none ->
            error
    end.

%%%-------------------------------------------------------------------
%%% Operations modifying state

%% GH3283: For FATE we don't build the actual transaction, so some more work
%% is needed - but it should be straightforward.
%%
%% SpendTx = aec_spend_tx:new(#{sender_id => aeser_id:create(account, FromPubkey), ...})
spend(FromPubkey, ToPubkey, Amount, State) ->
    eval_primops([ aeprimop:spend_op(FromPubkey, ToPubkey, Amount)
                 , tx_event_op(spend, {FromPubkey, ToPubkey, Amount}, <<"Chain.spend">>)
                 ], State).

%% GH3283: If the idea is to be able to follow tokens throughout the system, we
%% should probably make this one into a "spend" as well, this is where value is
%% attached to a (remote) contract call. Some care to not include the top level
%% transfer_value might be needed.
transfer_value(FromPubkey, ToPubkey, Amount, State) ->
    eval_primops([ aeprimop:transfer_value_op(FromPubkey, ToPubkey, Amount)
                 , tx_event_op(spend, {FromPubkey, ToPubkey, Amount}, <<"Call.amount">>)
                 ], State).

tx_event_op(Op, Data, Type) ->
    Event = tx_event_data(Op, Data, Type),
    aeprimop:tx_event_op(internal_call_tx, Type, Event).

tx_event_data(spend, {FromPubKey, ToPubKey, Amount}, Payload) when is_binary(Payload) ->
    ok(aec_spend_tx:new(#{ sender_id    => acct_id(FromPubKey)
                         , recipient_id => acct_id(ToPubKey)
                         , amount       => Amount
                         , fee          => 0
                         , nonce        => 0
                         , payload      => Payload }));
tx_event_data(oracle_register, {Pubkey, QFormat, RFormat, QFee, RelTTL, ABIVsn}, _Type) ->
    ok(aeo_register_tx:new(#{ account_id      => acct_id(Pubkey)
                            , nonce           => 0
                            , query_format    => QFormat
                            , abi_version     => ABIVsn
                            , response_format => RFormat
                            , query_fee       => QFee
                            , oracle_ttl      => RelTTL
                            , fee             => 0 }));
tx_event_data(oracle_extend, {Pubkey, RelTTL}, _Type) ->
    ok(aeo_extend_tx:new(#{ oracle_id  => ora_id(Pubkey)
                          , nonce      => 0
                          , oracle_ttl => {delta, RelTTL}
                          , fee        => 0 }));
tx_event_data(oracle_query, {OraclePubkey, SenderPubkey, Query, QFee, QTTL, RTTL}, _Type) ->
    ok(aeo_query_tx:new(#{ sender_id    => acct_id(SenderPubkey)
                         , oracle_id    => ora_id(OraclePubkey)
                         , query        => Query
                         , query_fee    => QFee
                         , query_ttl    => ora_ttl(QTTL)
                         , response_ttl => {delta, RTTL}
                         , nonce        => 0
                         , fee          => 0
                         , ttl          => 0 }));
tx_event_data(oracle_response, {OraclePubkey, QueryId, Response, RTTL}, _Type) ->
    %% The ResponseTTL is not available at this time?
    ok(aeo_response_tx:new(#{ oracle_id => ora_id(OraclePubkey)
                            , nonce     => 0
                            , query_id  => QueryId
                            , response  => Response
                            , response_ttl => RTTL
                            , fee          => 0 }));
tx_event_data(aens_preclaim, {Pubkey, Hash, PreclaimTTL}, _Type) ->
    ok(aens_preclaim_tx:new(#{ account_id    => acct_id(Pubkey)
                             , nonce         => 0
                             , commitment_id => aeser_id:create(commitment, Hash)
                             , fee           => 0
                             , ttl           => val(PreclaimTTL, 0) }));
tx_event_data(aens_claim, {Pubkey, Name, Salt, NameFee, DeltaTTL}, _Type) ->
    ok(aens_claim_tx:new(#{ account_id => acct_id(Pubkey)
                          , name       => Name
                          , name_salt  => Salt
                          , name_fee   => NameFee
                          , ttl        => val(DeltaTTL, 0)
                          , nonce      => 0
                          , fee        => 0 }));
tx_event_data(aens_transfer, {FromPubkey, ToPubkey, NameId}, _Type) ->
    ok(aens_transfer_tx:new(#{ account_id   => acct_id(FromPubkey)
                             , recipient_id => acct_id(ToPubkey)
                             , name_id      => name_id(NameId)
                             , nonce        => 0
                             , fee          => 0 }));
tx_event_data(aens_revoke, {Pubkey, NameId}, _Type) ->
    ok(aens_revoke_tx:new(#{ account_id => acct_id(Pubkey)
                           , name_id    => name_id(NameId)
                           , nonce      => 0
                           , fee        => 0 }));
tx_event_data(aens_update, {Pubkey, NameHash, TTL, ClientTTL, Pointers}, _Type) ->
    ok(aens_update_tx:new(#{ account_id => acct_id(Pubkey)
                           , name_id    => name_id(NameHash)
                           , name_ttl   => val(TTL, 0)
                           , pointers   => val(Pointers, [])
                           , client_ttl => val(ClientTTL, 0)
                           , nonce => 0
                           , fee   => 0 }));
tx_event_data(_, _, _) ->
    error.

acct_id(Pubkey) ->
    aeser_id:create(account, Pubkey).

name_id(Name) ->
    aeser_id:create(name, Name).

ora_id(Pubkey) ->
    aeser_id:create(oracle, Pubkey).

val(undefined, Default) ->
    Default;
val(V, _) ->
    V.

ttl(undefined, _) -> 0;
ttl({fixed_ttl,TTL}, _) ->
    TTL;
ttl({delta, _} = TTL, _) -> TTL;
ttl({block, _} = TTL, _) -> TTL;
ttl({relative_ttl, RelTTL}, S) ->
    Height = aetx_env:height(tx_env(S)),
    Height + RelTTL;
ttl(TTL, _) when is_integer(TTL) ->
    TTL.

ora_ttl({block, T} = TTL) when is_integer(T), T >= 0 -> TTL;
ora_ttl({delta, T} = TTL) when is_integer(T), T >= 0 -> TTL.

ora_ttl(absolute, TTL) ->
    {block, TTL};
ora_ttl(relative, TTL) ->
    {delta, TTL}.


ok({ok, Value}) ->
    Value.

%%%-------------------------------------------------------------------
%%% Oracles

oracle_register(Pubkey, QFee, TTLType, TTLVal,
                QFormat, RFormat, ABIVersion, State) ->
    Height = aetx_env:height(tx_env(State)),
    case to_relative_ttl(TTLType, TTLVal, Height) of
        {ok, RelTTL} ->
            Gas = ttl_gas(oracle_register_tx, RelTTL) +
                size_gas([QFormat, RFormat]),
            eval_primops([ aeprimop:oracle_register_op(Pubkey, QFormat, RFormat,
                                                       QFee, RelTTL, ABIVersion)
                         , tx_event_op(oracle_register, {Pubkey, QFormat, RFormat,
                                                         QFee, ora_ttl(TTLType, TTLVal),
                                                         ABIVersion}, <<"Oracle.register">>)
                         ], State, Gas);
        {error, _} = Err ->
            Err
    end.

oracle_extend(Pubkey, TTLType, TTLVal, State) when ?IS_ONCHAIN(State) ->
    Height = aetx_env:height(tx_env(State)),
    case to_relative_ttl(TTLType, TTLVal, Height) of
        {ok, RelTTL} ->
            Gas = ttl_gas(oracle_extend_tx, RelTTL),
            eval_primops([ aeprimop:oracle_extend_op(Pubkey, RelTTL)
                         , tx_event_op(oracle_extend, {Pubkey, TTLVal}, <<"Oracle.extend">>)
                         ], State, Gas);
        {error, _} = Err ->
            Err
    end.

oracle_query(OraclePubkey, SenderPubkey, Question, QFee, QTTLType, QTTL, RTTL,
             ABIVersion, QType, RType, #state{primop_state = PState} = State) when ?IS_ONCHAIN(State) ->
    Height = aetx_env:height(tx_env(State)),
    case to_relative_ttl(QTTLType, QTTL, Height) of
        {ok, QTTL1} ->
            case abi_encode_oracle_term(OraclePubkey, ABIVersion, QType, RType, Question, PState) of
                {ok, Question1, PState1} ->
                    %% The nonce of the sender is used for creating the query id.
                    %% So, we need to bump it.
                    {SAcc, PState2} = aeprimop_state:get_account(SenderPubkey, PState1),
                    Nonce = aec_accounts:nonce(SAcc) + 1,
                    Gas = ttl_gas(oracle_query_tx, QTTL1) + size_gas([Question1]),
                    Ins   = [ aeprimop:force_inc_account_nonce_op(SenderPubkey, Nonce)
                            , aeprimop:spend_fee_op(SenderPubkey, QFee)
                              %% TODO: Should we add a tx event for spend_fee?
                            , tx_event_op(oracle_query, {OraclePubkey, SenderPubkey,
                                                         Question1, QFee,
                                                         ora_ttl(QTTLType, QTTL),
                                                         RTTL}, <<"Oracle.query">>)
                            , aeprimop:oracle_query_op_with_return(
                                OraclePubkey, SenderPubkey, Nonce,
                                Question1, QFee, QTTL1, RTTL)
                            ],
                    eval_primops(Ins, State#state{ primop_state = PState2},
                                 Gas);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

oracle_respond(OraclePubkey, QueryId, Response, ABIVersion, QType, RType,
               #state{primop_state = PState} = State) when ?IS_ONCHAIN(State) ->
    case abi_encode_oracle_term(OraclePubkey, ABIVersion, QType, RType, Response, PState) of
        {ok, Response1, PState1} ->
            Ins = [ aeprimop:oracle_respond_op(OraclePubkey, QueryId, Response1)
                  , aeprimop:oracle_earn_query_fee_op(OraclePubkey, QueryId)
                  ],
            %% Note: The response TTL is calculated on return when we know the
            %%       query can be found.
            case eval_primops(Ins, State#state{ primop_state = PState1}) of
                {ok, #state{ primop_state = PState2} = State1} ->
                    {Query, _} = aeprimop_state:find_oracle_query(OraclePubkey, QueryId, PState2),
                    {delta, RTTL} = Delta = aeo_query:response_ttl(Query),
                    Gas = size_gas([Response1]) + ttl_gas(oracle_response_tx, RTTL),
                    %% Add the tx_event_op now that we have the response TTL
                    {ok, State2} = eval_primops(
                                     [tx_event_op(oracle_response,
                                                  {OraclePubkey, QueryId, Response1,
                                                   Delta}, <<"Oracle.respond">>)
                                     ], State1),
                    {ok, Gas, State2};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

oracle_query_fee(OraclePubkey, State) ->
    case oracle_get_oracle(OraclePubkey, State) of
        {ok, Oracle, State1} ->
            {ok, aeo_oracles:query_fee(Oracle), State1};
        Err = {error, _} ->
            Err
    end.

oracle_expiry(OraclePubkey, State) ->
    case oracle_get_oracle(OraclePubkey, State) of
        {ok, Oracle, State1} ->
            {ok, aeo_oracles:ttl(Oracle), State1};
        Err = {error, _} ->
            Err
    end.

oracle_get_oracle(OraclePubkey, State) ->
    case aeprimop_state:find_oracle(OraclePubkey, get_pstate(State)) of
        {Oracle, PState1} ->
            {ok, Oracle, set_pstate(PState1, State)};
        none ->
            {error, oracle_does_not_exist}
    end.

oracle_get_question(OraclePubkey, QueryId, QType, RType, State) ->
    case oracle_get_question_from_pstate(OraclePubkey, QueryId, QType, RType, get_pstate(State)) of
        {ok, Resp, PState1} ->
            {ok, Resp, set_pstate(PState1, State)};
        {error, _} = Err ->
            Err
    end.

oracle_get_question_from_pstate(OraclePubkey, QueryId, QType, RType, PState) ->
    ABIVersion = ?ABI_FATE_SOPHIA_1,
    case find_oracle_with_type(OraclePubkey, QType, RType, ABIVersion, PState) of
        {ok, Oracle, PState1} ->
            case aeprimop_state:find_oracle_query(OraclePubkey, QueryId, PState1) of
                {Query, PState2} ->
                    RawQuestion = aeo_query:query(Query),
                    case aeo_oracles:abi_version(Oracle) of
                        ?ABI_NO_VM ->
                            {ok, aeb_fate_data:make_string(RawQuestion), PState2};
                        ?ABI_FATE_SOPHIA_1 ->
                            try
                                {ok, aeb_fate_encoding:deserialize(RawQuestion), PState2}
                            catch _:_ ->
                                    {error, bad_question}
                            end
                    end;
                none ->
                    {error, query_not_found}
            end;
        {error, _} = Err ->
            Err
    end.

oracle_get_answer(OraclePubkey, QueryId, QType, RType, State) ->
    case oracle_get_answer_from_pstate(OraclePubkey, QueryId, QType, RType, get_pstate(State)) of
        {ok, Resp, PState1} ->
            {ok, Resp, set_pstate(PState1, State)};
        {error, _} = Err ->
            Err
    end.

oracle_get_answer_from_pstate(OraclePubkey, QueryId, QType, RType, PState) ->
    ABIVersion = ?ABI_FATE_SOPHIA_1,
    case find_oracle_with_type(OraclePubkey, QType, RType, ABIVersion, PState) of
        {ok, Oracle, PState1} ->
            case aeprimop_state:find_oracle_query(OraclePubkey, QueryId, PState1) of
                {Query, PState2} ->
                    case aeo_query:response(Query) of
                        undefined ->
                            {ok, aeb_fate_data:make_variant([0,1], 0, {}), PState2};
                        RawResponse ->
                            case aeo_oracles:abi_version(Oracle) of
                                ?ABI_NO_VM ->
                                    Answer = aeb_fate_data:make_string(RawResponse),
                                    Resp = aeb_fate_data:make_variant([0,1], 1, {Answer}),
                                    {ok, Resp, PState2};
                                ?ABI_FATE_SOPHIA_1 ->
                                    try aeb_fate_encoding:deserialize(RawResponse) of
                                        Answer ->
                                            Resp = aeb_fate_data:make_variant([0,1], 1, {Answer}),
                                            {ok, Resp, PState2}
                                    catch _:_ ->
                                            {error, bad_response}
                                    end
                            end
                    end;
                none ->
                    {ok, aeb_fate_data:make_variant([0,1], 0, {}), PState}
            end;
        {error, _} = Err ->
            Err
    end.

oracle_check(Pubkey, QType, RType, State) ->
    ABIVersion = ?ABI_FATE_SOPHIA_1,
    case find_oracle_with_type(Pubkey, QType, RType, ABIVersion, get_pstate(State)) of
        {ok, _Oracle, PState1} -> {ok, ?FATE_TRUE,  set_pstate(PState1, State)};
        {error, _}             -> {ok, ?FATE_FALSE, State}
    end.

oracle_check_query(Pubkey, Query, QType, RType, State) ->
    ABIVersion = ?ABI_FATE_SOPHIA_1,
    case find_oracle_with_type(Pubkey, QType, RType, ABIVersion, get_pstate(State)) of
        {ok, _, PState1} ->
            case aeprimop_state:find_oracle_query(Pubkey, Query, PState1) of
                {_, PState2} ->
                    {ok, ?FATE_TRUE, set_pstate(PState2, State)};
                none ->
                    {ok, ?FATE_FALSE, set_pstate(PState1, State)}
            end;
        {error, _} -> {ok, ?FATE_FALSE, State}
    end.

is_oracle(Pubkey, State) ->
    case aeprimop_state:find_oracle(Pubkey, get_pstate(State)) of
        {_Oracle, PState1} ->
            {ok, ?FATE_TRUE, set_pstate(PState1, State)};
        none ->
            {ok, ?FATE_FALSE, State}
    end.

is_contract(Pubkey, State) ->
    case aeprimop_state:find_contract_without_store(Pubkey, get_pstate(State)) of
        {value, _Contract} ->
            {ok, ?FATE_TRUE, State};
        none ->
            {ok, ?FATE_FALSE, State}
    end.

is_payable(Pubkey, State) ->
    case aeprimop_state:find_account(Pubkey, get_pstate(State)) of
        {Account, _} ->
            case aec_accounts:is_payable(Account) of
                true  -> {ok, ?FATE_TRUE, State};
                false -> {ok, ?FATE_FALSE, State}
            end;
        none ->
            {ok, ?FATE_TRUE, State}
    end.

to_relative_ttl(relative, TTL,_Height) ->
    case TTL >= 0 of
        true  -> {ok, TTL};
        false -> {error, negative_ttl}
    end;
to_relative_ttl(absolute, TTL, Height) ->
    case TTL >= Height of
        true  -> {ok, TTL - Height};
        false -> {error, too_low_ttl}
    end.

abi_encode_oracle_term(Pubkey, ABIVersion, QType, RType, Term, PState) ->
    case find_oracle_with_type(Pubkey, QType, RType, ABIVersion, PState) of
        {ok, Oracle, PState1} ->
            case aeo_oracles:abi_version(Oracle) of
                ?ABI_NO_VM ->
                    case ?IS_FATE_STRING(Term) of
                        true ->
                            ?FATE_STRING(String) = Term,
                            {ok, String, PState1};
                        false ->
                            {error, no_vm_oracles_needs_strings}
                    end;
                ABIVersion ->
                    {ok, aeb_fate_encoding:serialize(Term), PState1}
            end;
        {error, _} = Err ->
            Err
    end.

find_oracle_with_type(Pubkey, QType, RType, ABIVersion, PState) ->
    case aeprimop_state:find_oracle(Pubkey, PState) of
        {Oracle, PState1} ->
            case aeo_oracles:abi_version(Oracle) of
                ?ABI_NO_VM when QType =:= ?FATE_TYPEREP(string),
                                RType =:= ?FATE_TYPEREP(string) ->
                    {ok, Oracle, PState1};
                ABIVersion ->
                    QFormat = aeb_fate_encoding:serialize(QType),
                    RFormat = aeb_fate_encoding:serialize(RType),
                    case (aeo_oracles:query_format(Oracle) =:= QFormat andalso
                          aeo_oracles:response_format(Oracle) =:= RFormat) of
                        true ->
                            {ok, Oracle, PState1};
                        false ->
                            {error, wrong_oracle_types}
                    end;
                _Other ->
                    {error, wrong_abi_version}
            end;
        none ->
            {error, oracle_not_found}
    end.

%%%-------------------------------------------------------------------
%%% Naming service

aens_resolve(NameString, Key, S) ->
    case aens_resolve_from_pstate(NameString, Key, get_pstate(S)) of
        {ok, Tag, Pubkey, PState1} ->
            {ok, Tag, Pubkey, set_pstate(PState1, S)};
        none ->
            none;
        {error, _} = Err ->
            Err
    end.

aens_resolve_from_pstate(NameString, Key, PState) ->
    case aens_utils:to_ascii(NameString) of
        {ok, NameAscii} ->
            NameHash = aens_hash:name_hash(NameAscii),
            case aeprimop_state:find_name(NameHash, PState) of
                {Name, PState1} ->
                    case aens:resolve_from_name_object(Key, Name) of
                        {ok, Id} ->
                            {Tag, Pubkey} = aeser_id:specialize(Id),
                            {ok, Tag, Pubkey, PState1};
                        {error, name_revoked} ->
                            none;
                        {error, pointer_id_not_found} ->
                            none
                    end;
                none ->
                    none
            end;
        {error, _} = Err ->
            Err
    end.

aens_preclaim(Pubkey, Hash, #state{} = S, VmVersion) when ?IS_ONCHAIN(S) ->
    PreclaimTTL =
        case VmVersion >= ?VM_FATE_SOPHIA_2 of
            true ->
                aec_governance:name_preclaim_expiration();
            false ->
                0
        end,
    eval_primops([ aeprimop:name_preclaim_op(Pubkey, Hash, PreclaimTTL)
                 , tx_event_op(aens_preclaim, {Pubkey, Hash, ttl(PreclaimTTL, S)},
                               <<"AENS.preclaim">>)
                 ], S).

aens_claim(Pubkey, NameBin, SaltInt, NameFee, #state{} = S) when ?IS_ONCHAIN(S) ->
    PreclaimDelta = aec_governance:name_claim_preclaim_delta(),
    Instructions = [ aeprimop:name_claim_op(Pubkey, NameBin, SaltInt, NameFee, PreclaimDelta)
                   , tx_event_op(aens_claim, {Pubkey, NameBin, SaltInt, NameFee,
                                              ttl(PreclaimDelta, S)}, <<"AENS.claim">>)
                   ],
    eval_primops(Instructions, S, size_gas([NameBin])).

aens_transfer(FromPubkey, HashBin, ToPubkey, #state{} = S) when ?IS_ONCHAIN(S) ->
    Instructions = [ aeprimop:name_transfer_op(FromPubkey, account, ToPubkey, HashBin)
                   , tx_event_op(aens_transfer, {FromPubkey, ToPubkey, HashBin},
                                 <<"AENS.transfer">>)
                   ],
    eval_primops(Instructions, S).

aens_revoke(Pubkey, HashBin, #state{} = S) when ?IS_ONCHAIN(S) ->
    ProtectedDeltaTTL = aec_governance:name_protection_period(),
    Instructions = [ aeprimop:name_revoke_op(Pubkey, HashBin, ProtectedDeltaTTL)
                   , tx_event_op(aens_revoke, {Pubkey, HashBin, ttl(ProtectedDeltaTTL, S)},
                                 <<"AENS.revoke">>)
                   ],
    eval_primops(Instructions, S).

aens_update(Pubkey, HashBin, TTL, ClientTTL, Pointers, #state{} = S)
  when ?IS_ONCHAIN(S) ->
    Instructions = [ aeprimop:name_update_op(Pubkey, HashBin, TTL, ClientTTL, Pointers)
                   , tx_event_op(aens_update, {Pubkey, HashBin, ttl(TTL, S),
                                               ttl(ClientTTL, S), Pointers},
                                 <<"AENS.update">>)
                   ],
    eval_primops(Instructions, S).

aens_lookup(NameString, S) ->
    case aens_lookup_from_pstate(NameString, get_pstate(S)) of
        {ok, NameObj, PState1} ->
            {ok, NameObj, set_pstate(PState1, S)};
        none ->
            none;
        {error, _} = Err ->
            Err
    end.

aens_lookup_from_pstate(NameString, PState) ->
    case aens_utils:to_ascii(NameString) of
        {ok, NameAscii} ->
            NameHash = aens_hash:name_hash(NameAscii),
            case aeprimop_state:find_name(NameHash, PState) of
                {Name, PState1} ->
                    case aens_names:status(Name) of
                        revoked -> none;
                        claimed ->
                            NameObj =
                                #{ owner    => aens_names:owner_pubkey(Name),
                                   ttl      => aens_names:ttl(Name),
                                   pointers => aens_names:pointers(Name) },
                            {ok, NameObj, PState1}
                    end;
                none ->
                    none
            end;
        {error, _} = Err ->
            Err
    end.

%%%-------------------------------------------------------------------
%%% Interface to primop evaluation

eval_primops(Ops, #state{} = S) ->
  eval_primops(Ops, S, none).

eval_primops(Ops, #state{primop_state = PState} = S, OptReturn) ->
    case aeprimop:eval_on_primop_state(Ops, PState) of
        {ok, PState1} when OptReturn =:= none ->
            {ok, S#state{primop_state = PState1}};
        {ok, PState1} when OptReturn =/= none ->
            {ok, OptReturn, S#state{primop_state = PState1}};
        {ok, Return, PState1} when OptReturn =:= none ->
            {ok, Return, S#state{primop_state = PState1}};
        {ok, Return, PState1} when OptReturn =/= none ->
            {ok, Return, OptReturn, S#state{primop_state = PState1}};
        {error, Atom} = Err when is_atom(Atom) ->
            Err
    end.

%%%-------------------------------------------------------------------
%%% Gas costs

ttl_gas(Tag, RelativeTTL) when is_integer(RelativeTTL), RelativeTTL > 0 ->
    aec_governance_utils:state_gas(
      aec_governance:state_gas_per_block(Tag),
      RelativeTTL).

size_gas(List) ->
    lists:sum([byte_size(X) || X <- List]).

%%% --- Helper functions
get_pstate(#state{primop_state = PState} = S) when ?IS_ONCHAIN(S) ->
    PState;
get_pstate(#state{onchain_primop_state = {onchain, PState}}) ->
    PState.

set_pstate(PState, S) when ?IS_ONCHAIN(S) ->
    S#state{primop_state = PState};
set_pstate(PState, S) ->
    S#state{onchain_primop_state = {onchain, PState}}.
