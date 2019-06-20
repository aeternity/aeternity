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
        , beneficiary/1
        , blockhash/2
        , contract_fate_code/2
        , contract_store/2
        , difficulty/1
        , final_trees/1
        , gas_limit/1
        , gas_price/1
        , generation/1
        , origin/1
        , set_contract_store/3
        , timestamp_in_msecs/1
        , tx_env/1
        ]).

%% Modifiers
-export([ spend/4
        , oracle_extend/4
        , oracle_get_answer/5
        , oracle_get_question/5
        , oracle_register/8
        , oracle_query/11
        , oracle_respond/7
        , oracle_query_fee/2
        ]).

-export([ check_delegation_signature/4
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
               , origin       :: binary()
               }).

-type state() :: #state{}.
-type pubkey() :: <<_:256>>.
%%===================================================================
%%% API
%%% ===================================================================

%%%-------------------------------------------------------------------
%%% External API for infrastructure

-spec new(map()) -> state().
new(#{ gas_price := GasPrice
     , origin := Origin
     , trees  := Trees
     , tx_env := TxEnv
     }) ->
    #state{ primop_state = aeprimop_state:new(Trees, TxEnv)
          , gas_price    = GasPrice
          , origin       = Origin
          }.

-spec tx_env(state()) -> aetx_env:env().
tx_env(#state{primop_state = PState}) ->
    aeprimop_state:tx_env(PState).

-spec final_trees(state()) -> aec_trees:trees().
final_trees(#state{primop_state = PState}) ->
    aeprimop_state:final_trees(PState).

%%%-------------------------------------------------------------------
%%% Basic getters

-spec origin(state()) -> aeb_fate_data:fate_address().
origin(#state{origin = Origin}) ->
    aeb_fate_data:make_address(Origin).

-spec gas_price(state()) -> aeb_fate_data:fate_integer().
gas_price(#state{gas_price = GasPrice}) ->
    aeb_fate_data:make_integer(GasPrice).

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

%%%-------------------------------------------------------------------
%%% Slightly more involved getters with caching

-spec blockhash(non_neg_integer(), #state{}) -> aeb_fate_data:fate_integer().
blockhash(Height, #state{} = S) ->
    TxEnv = tx_env(S),
    case aetx_env:key_hash(TxEnv) of
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> ->
            %% For channels
            aeb_fate_data:make_integer(0);
        KeyHash ->
            {ok, Header} = aec_chain:get_key_header_by_height(Height),
            {ok, Hash} = aec_headers:hash_header(Header),
            %% Make sure that this is an ancestor
            case aec_chain:find_common_ancestor(Hash, KeyHash) of
                {ok, <<N:?BLOCK_HEADER_HASH_BYTES/unit:8 >> = Hash} ->
                    aeb_fate_data:make_integer(N);
                {ok, _Other} ->
                    <<N:?BLOCK_HEADER_HASH_BYTES/unit:8 >> =
                        traverse_to_key_hash(Height, KeyHash),
                    aeb_fate_data:make_integer(N)
            end
    end.

traverse_to_key_hash(H, KeyHash) ->
    {ok, Header} = aec_chain:get_header(KeyHash),
    case aec_headers:height(Header) of
        Height when Height =:= H -> KeyHash;
        Height when Height =:= H + 1 -> aec_headers:prev_key_hash(Header);
        _Height -> traverse_to_key_hash(H, aec_headers:prev_key_hash(Header))
    end.

-spec contract_fate_code(pubkey(), state()) -> 'error' |
                                               {'ok', term(), state()}.
contract_fate_code(Pubkey, #state{primop_state = PState} = S) ->
    case aeprimop_state:find_contract_without_store(Pubkey, PState) of
        none -> error;
        {value, Contract} ->
            case aect_contracts:vm_version(Contract) of
                VMV when ?IS_FATE_SOPHIA(VMV) ->
                    SerCode = aect_contracts:code(Contract),
                    #{ byte_code := ByteCode} = aect_sophia:deserialize(SerCode),
                    try aeb_fate_code:deserialize(ByteCode) of
                        FateCode -> {ok, FateCode, S#state{primop_state = PState}}
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end
    end.

-spec contract_store(pubkey(), state()) -> {aect_contracts_store:store(), state()}.
contract_store(Pubkey, #state{primop_state = PState} = S) ->
    %% If we are looking for the store, we are already running the contract,
    %% so we can boldly assume it exists and have the correct vm version.
    {Contract, PState1} = aeprimop_state:get_contract(Pubkey, PState),
    {aect_contracts:state(Contract), S#state{primop_state = PState1}}.

-spec account_balance(pubkey(), state()) -> 'error' |
                                            {'ok', aeb_fate_data:fate_integer(), state()}.
account_balance(Pubkey, #state{primop_state = PState} = S) ->
    case aeprimop_state:find_account(Pubkey, PState) of
        {Account, PState1} ->
            Balance = aeb_fate_data:make_integer(aec_accounts:balance(Account)),
            {ok, Balance, S#state{primop_state = PState1}};
        none ->
            error
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

spend(FromPubkey, ToPubkey, Amount, State) ->
    eval_primops([ aeprimop:spend_op(FromPubkey, ToPubkey, Amount)
                 ], State).

oracle_register(Pubkey, QFee, TTLType, TTLVal,
                QFormat, RFormat, ABIVersion, State) ->
    Height = aetx_env:height(tx_env(State)),
    case to_relative_ttl(TTLType, TTLVal, Height) of
        {ok, RelTTL} ->
            eval_primops([ aeprimop:oracle_register_op(Pubkey, QFormat, RFormat,
                                                       QFee, RelTTL, ABIVersion)
                         ], State);
        {error, _} = Err ->
            Err
    end.

oracle_extend(Pubkey, TTLType, TTLVal, State) ->
    Height = aetx_env:height(tx_env(State)),
    case to_relative_ttl(TTLType, TTLVal, Height) of
        {ok, RelTTL} ->
            eval_primops([ aeprimop:oracle_extend_op(Pubkey, RelTTL)
                         ], State);
        {error, _} = Err ->
            Err
    end.

oracle_query(OraclePubkey, SenderPubkey, Question, QFee, QTTLType, QTTL, RTTL,
             ABIVersion, QType, RType, #state{primop_state = PState} = State) ->
    Height = aetx_env:height(tx_env(State)),
    case to_relative_ttl(QTTLType, QTTL, Height) of
        {ok, QTTL1} ->
            case abi_encode_oracle_term(OraclePubkey, ABIVersion, QType, RType, Question, PState) of
                {ok, Question1, PState1} ->
                    %% The nonce of the sender is used for creating the query id.
                    %% So, we need to bump it.
                    {SAcc, PState2} = aeprimop_state:get_account(SenderPubkey, PState1),
                    Nonce = aec_accounts:nonce(SAcc) + 1,
                    Ins   = [ aeprimop:force_inc_account_nonce_op(SenderPubkey, Nonce)
                            , aeprimop:spend_fee_op(SenderPubkey, QFee)
                            , aeprimop:oracle_query_op_with_return(
                                OraclePubkey, SenderPubkey, Nonce,
                                Question1, QFee, QTTL1, RTTL)
                            ],
                    eval_primops(Ins, State#state{ primop_state = PState2});
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

oracle_respond(OraclePubkey, QueryId, Response, ABIVersion, QType, RType,
               #state{primop_state = PState} = State) ->
    case abi_encode_oracle_term(OraclePubkey, ABIVersion, QType, RType, Response, PState) of
        {ok, Response1, PState1} ->
            Ins = [ aeprimop:oracle_respond_op(OraclePubkey, QueryId, Response1)
                  , aeprimop:oracle_earn_query_fee_op(OraclePubkey, QueryId)
                  ],
            eval_primops(Ins, State#state{ primop_state = PState1});
        {error, _} = Err ->
            Err
    end.

oracle_query_fee(OraclePubkey, #state{primop_state = PState} = State) ->
    case aeprimop_state:find_oracle(OraclePubkey, PState) of
        {Oracle, PState1} ->
            {ok, aeo_oracles:query_fee(Oracle), State#state{primop_state = PState1}};
        none ->
            {error, oracle_does_not_exist}
    end.

oracle_get_question(OraclePubkey, QueryId, QType, RType,
                    #state{primop_state = PState} = State) ->
    ABIVersion = ?ABI_FATE_SOPHIA_1,
    case find_oracle_with_type(OraclePubkey, QType, RType, ABIVersion, PState) of
        {ok, Oracle, PState1} ->
            case aeprimop_state:find_oracle_query(OraclePubkey, QueryId, PState1) of
                {Query, PState2} ->
                    RawQuestion = aeo_query:query(Query),
                    State1 = State#state{primop_state = PState2},
                    case aeo_oracles:abi_version(Oracle) of
                        ?ABI_NO_VM ->
                            {ok, aeb_fate_data:make_string(RawQuestion), State1};
                        ?ABI_FATE_SOPHIA_1 ->
                            try
                                {ok, aeb_fate_encoding:deserialize(RawQuestion), State1}
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

oracle_get_answer(OraclePubkey, QueryId, QType, RType,
                  #state{primop_state = PState} = State) ->
    ABIVersion = ?ABI_FATE_SOPHIA_1,
    case find_oracle_with_type(OraclePubkey, QType, RType, ABIVersion, PState) of
        {ok, Oracle, PState1} ->
            case aeprimop_state:find_oracle_query(OraclePubkey, QueryId, PState1) of
                {Query, PState2} ->
                    State1 = State#state{primop_state = PState2},
                    case aeo_query:response(Query) of
                        undefined ->
                            {ok, aeb_fate_data:make_variant([0,1], 0, {}), State1};
                        RawResponse ->
                            case aeo_oracles:abi_version(Oracle) of
                                ?ABI_NO_VM ->
                                    Answer = aeb_fate_data:make_string(RawResponse),
                                    Resp = aeb_fate_data:make_variant([0,1], 1, {Answer}),
                                    {ok, Resp, State1};
                                ?ABI_FATE_SOPHIA_1 ->
                                    try aeb_fate_encoding:deserialize(RawResponse) of
                                        Answer ->
                                            Resp = aeb_fate_data:make_variant([0,1], 1, {Answer}),
                                            {ok, Resp, State1}
                                    catch _:_ ->
                                            {error, bad_response}
                                    end
                            end
                    end;
                none ->
                    {ok, aeb_fate_data:make_variant([0,1], 0, {}), State}
            end;
        {error, _} = Err ->
            Err
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
%%% Interface to primop evaluation

eval_primops(Ops, #state{primop_state = PState} = S) ->
    case aeprimop:eval_on_primop_state(Ops, PState) of
        {ok, PState1} ->
            {ok, S#state{primop_state = PState1}};
        {ok, Return, PState1} ->
            {ok, Return, S#state{primop_state = PState1}};
        {error, Atom} = Err when is_atom(Atom) ->
            Err
    end.

