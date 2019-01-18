%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
%%% @doc
%%% ADT containing all different transactions
%%% @end
%%%-------------------------------------------------------------------

-module(aetx).

-export([ accounts/1
        , deserialize_from_binary/1
        , fee/1
        , gas_limit/2
        , min_gas/2
        , gas_price/1
        , ttl/1
        , size/1
        , min_fee/2
        , new/2
        , nonce/1
        , origin/1
        , process/3
        , custom_apply/4
        , serialize_for_client/1
        , serialize_to_binary/1
        , signers/2
        , specialize_type/1
        , specialize_callback/1
        , update_tx/2]).

-ifdef(TEST).
-export([tx/1]).
-endif.

-define(IS_CONTRACT_TX(T), ((T =:= contract_create_tx) or (T =:= contract_call_tx) or (T =:= channel_force_progress_tx))).

%%%===================================================================
%%% Types
%%%===================================================================

-record(aetx, { type :: tx_type()
              , cb   :: module()
              , size :: pos_integer()
              , tx   :: tx_instance() }).

-opaque tx() :: #aetx{}.

-type tx_type() :: spend_tx
                 | oracle_register_tx
                 | oracle_extend_tx
                 | oracle_query_tx
                 | oracle_response_tx
                 | name_preclaim_tx
                 | name_claim_tx
                 | name_transfer_tx
                 | name_update_tx
                 | name_revoke_tx
                 | contract_create_tx
                 | contract_call_tx
                 | channel_create_tx
                 | channel_deposit_tx
                 | channel_withdraw_tx
                 | channel_force_progress_tx
                 | channel_close_mutual_tx
                 | channel_close_solo_tx
                 | channel_slash_tx
                 | channel_settle_tx
                 | channel_snapshot_solo_tx
                 | channel_offchain_tx.

-type tx_instance() :: aec_spend_tx:tx()
                     | aeo_register_tx:tx()
                     | aeo_extend_tx:tx()
                     | aeo_query_tx:tx()
                     | aeo_response_tx:tx()
                     | aens_preclaim_tx:tx()
                     | aens_claim_tx:tx()
                     | aens_transfer_tx:tx()
                     | aens_update_tx:tx()
                     | aens_revoke_tx:tx()
                     | aect_create_tx:tx()
                     | aect_call_tx:tx()
                     | aesc_create_tx:tx()
                     | aesc_deposit_tx:tx()
                     | aesc_withdraw_tx:tx()
                     | aesc_force_progress_tx:tx()
                     | aesc_close_mutual_tx:tx()
                     | aesc_close_solo_tx:tx()
                     | aesc_slash_tx:tx()
                     | aesc_settle_tx:tx()
                     | aesc_snapshot_solo_tx:tx()
                     | aesc_offchain_tx:tx().

-type tx_ttl() :: 0 | aec_blocks:height().
%% A transaction TTL is either an absolute block height, or the transaction
%% does not have a TTL. The latter is represented as 0 to get a small
%% serialization. `aetx:ttl/1' returns `max_ttl' in this case.

-export_type([ tx/0
             , tx_instance/0
             , tx_type/0
             , tx_ttl/0 ]).

%%%===================================================================
%%% Behaviour definition
%%%===================================================================

-callback new(Args :: map()) ->
    {ok, Tx :: tx()} | {error, Reason :: term()}.

-callback type() -> atom().

-callback version() -> non_neg_integer().

-callback fee(Tx :: tx_instance()) ->
    Fee :: integer().

-callback gas(Tx :: tx_instance()) ->
    Gas :: non_neg_integer().

-callback gas_price(Tx :: tx_instance()) ->
    GasPrice :: aect_contracts:amount() | undefined.

-callback ttl(Tx :: tx_instance()) ->
    TTL :: aec_blocks:height().

-callback nonce(Tx :: tx_instance()) ->
    Nonce :: non_neg_integer().

-callback origin(Tx :: tx_instance()) ->
    Origin :: aec_keys:pubkey() | undefined.

-callback signers(Tx :: tx_instance(), Trees :: aec_trees:trees()) ->
    {ok, [aec_keys:pubkey()]} | {error, atom()}.

-callback check(Tx :: tx_instance(), aec_trees:trees(), aetx_env:env()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.

-callback process(Tx :: tx_instance(), aec_trees:trees(), aetx_env:env()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.

-callback serialize(Tx :: tx_instance()) ->
    term().

-callback serialization_template(Vsn :: non_neg_integer()) ->
    term().

-callback deserialize(Vsn :: integer(), SerializedTx :: term()) ->
    Tx :: tx_instance().

-callback for_client(Tx :: tx_instance()) ->
    map().

-optional_callbacks([gas_price/1]).

%%%===================================================================
%%% Getters and setters
%%%===================================================================

-spec new(CallbackModule :: module(),  Tx :: tx_instance()) ->
    Tx :: tx().
new(Callback, Tx) ->
    Type = Callback:type(),
    {Vsn, Fields} = Callback:serialize(Tx),
    Template = Callback:serialization_template(Vsn),
    Size = byte_size(aec_object_serialization:serialize(Type, Vsn, Template, Fields)),
    #aetx{ type = Type, cb = Callback, size = Size, tx = Tx }.

-spec fee(Tx :: tx()) -> Fee :: integer().
fee(#aetx{ cb = CB, tx = Tx }) ->
    CB:fee(Tx).

%% In case 0 is returned, the tx will not be included in the micro block
%% candidate by the mempool.
-spec gas_limit(Tx :: tx(), Height :: aec_blocks:height()) -> Gas :: non_neg_integer().
gas_limit(#aetx{type = Type, cb = CB, size = Size, tx = Tx }, Height) when
      Type =:= oracle_register_tx;
      Type =:= oracle_extend_tx ->
    case ttl_delta(Height, CB:oracle_ttl(Tx)) of
        {delta, _D} = TTL ->
            base_gas(Type) + size_gas(Size) + state_gas(Type, TTL);
        {error, _Rsn} ->
            0
    end;
gas_limit(#aetx{type = oracle_query_tx, size = Size, tx = Tx }, Height) ->
    case ttl_delta(Height, aeo_query_tx:query_ttl(Tx)) of
        {delta, _D} = TTL ->
            base_gas(oracle_query_tx) + size_gas(Size) + state_gas(oracle_query_tx, TTL);
        {error, _Rsn} ->
            0
    end;
gas_limit(#aetx{type = oracle_response_tx, size = Size, tx = Tx }, Height) ->
    case ttl_delta(Height, aeo_response_tx:response_ttl(Tx)) of
        {delta, _D} = TTL ->
            base_gas(oracle_response_tx) + size_gas(Size) + state_gas(oracle_response_tx, TTL);
        {error, _Rsn} ->
            0
    end;
gas_limit(#aetx{ type = Type, cb = CB, size = Size, tx = Tx }, _Height) when Type =/= channel_offchain_tx ->
    base_gas(Type) + size_gas(Size) + CB:gas(Tx);
gas_limit(#aetx{ type = channel_offchain_tx }, _Height) ->
    0.

-spec gas_price(Tx :: tx()) -> GasPrice :: non_neg_integer() | undefined.
gas_price(#aetx{ type = Type, cb = CB, tx = Tx }) when ?IS_CONTRACT_TX(Type) ->
    CB:gas_price(Tx);
gas_price(#aetx{}) ->
    undefined.

-spec min_fee(Tx :: tx(), Height :: aec_blocks:height()) -> Fee :: non_neg_integer().
min_fee(#aetx{} = AeTx, Height) ->
    min_gas(AeTx, Height) * aec_governance:minimum_gas_price().

min_gas(#aetx{ type = Type, size = Size }, _Height) when ?IS_CONTRACT_TX(Type) ->
    base_gas(Type) + size_gas(Size);
min_gas(#aetx{} = Tx, Height) ->
    gas_limit(Tx, Height).

-spec nonce(Tx :: tx()) -> Nonce :: non_neg_integer().
nonce(#aetx{ cb = CB, tx = Tx }) ->
    CB:nonce(Tx).

-spec origin(Tx :: tx()) -> Origin :: aec_keys:pubkey() | undefined.
origin(#aetx{ cb = CB, tx = Tx }) ->
    CB:origin(Tx).

-spec accounts(Tx :: tx()) -> [aec_keys:pubkey()].
accounts(#aetx{ cb = CB, tx = Tx }) ->
    CB:accounts(Tx).

-spec signers(Tx :: tx(), Trees :: aec_trees:trees()) ->
    {ok, [aec_keys:pubkey()]} | {error, atom()}.
signers(#aetx{ cb = CB, tx = Tx }, Trees) ->
    CB:signers(Tx, Trees).

%% We let 0 represent no TTL/infinity in order to keep the serialization as
%% short as possible. Since there are no transactions in the genesis block
%% there is little risk for confusion.
-spec ttl(Tx :: tx()) -> max_ttl | aec_blocks:height().
ttl(#aetx{ cb = CB, tx = Tx }) ->
    case CB:ttl(Tx) of
        0 -> max_ttl;
        N -> N
    end.

-spec size(Tx :: tx()) -> pos_integer().
size(#aetx{ size = Size }) ->
    Size.

%%%===================================================================
%%% Checking transactions
%%%===================================================================

check(Tx, Trees, Env) ->
    case aetx_env:context(Env) of
        aetx_transaction -> check_tx(Tx, Trees, Env);
        aetx_contract    -> check_contract(Tx, Trees, Env)
    end.

check_contract(#aetx{ cb = CB, tx = Tx }, Trees, Env) ->
    CB:check(Tx, Trees, Env).

check_tx(#aetx{ cb = CB, tx = Tx } = AeTx, Trees, Env) ->
    Checks =
        [fun() -> check_minimum_fee(AeTx, Env) end,
         fun() -> check_minimum_gas_price(AeTx) end,
         fun() -> check_ttl(AeTx, Env) end],
    case aeu_validation:run(Checks) of
        ok             -> CB:check(Tx, Trees, Env);
        {error, _} = E -> E
    end.

check_minimum_fee(AeTx, Env) ->
    Height = aetx_env:height(Env),
    case min_fee(AeTx, Height) of
        MinFee when MinFee > 0 ->
            case fee(AeTx) >= MinFee of
                true  -> ok;
                false -> {error, too_low_fee}
            end;
        0 ->
            %% Oracle txs can return (minimal) gas of 0 when
            %% the absolute TTL is lower than the current chain
            %% height.
            {error, too_low_abs_ttl}
    end.

check_minimum_gas_price(AeTx) ->
    case gas_price(AeTx) of
        undefined ->
            ok;
        GasPrice when is_integer(GasPrice) ->
            case GasPrice >= aec_governance:minimum_gas_price() of
                true  -> ok;
                false -> {error, too_low_gas_price}
            end
    end.

check_ttl(AeTx, Env) ->
    case ttl(AeTx) >= aetx_env:height(Env) of
        true  -> ok;
        false -> {error, ttl_expired}
    end.

%%%===================================================================
%%% Processing transactions
%%%===================================================================

-spec process(tx(), aec_trees:trees(), aetx_env:env()) ->
                 {ok, NewTrees :: aec_trees:trees()} | {error, term()}.
process(#aetx{ cb = CB, tx = Tx } = AeTx, Trees, Env) ->
    case check(AeTx, Trees, Env) of
        {ok, Trees1} ->
            CB:process(Tx, Trees1, Env);
        {error, _} = Err ->
            Err
    end.

%% Call a custom callback function in the transaction module.
-spec custom_apply(atom(), tx(), aec_trees:trees(), aetx_env:env()) -> any().
custom_apply(Fun, #aetx{ cb = CB, tx = Tx}, Trees, Env) ->
    CB:Fun(Tx, Trees, Env).

%%%===================================================================
%%% Serialize/deserialize
%%%===================================================================

-spec serialize_for_client(Tx :: tx()) -> map().
serialize_for_client(#aetx{ cb = CB, type = Type, tx = Tx }) ->
    Res0 = CB:for_client(Tx),
    Res1 = Res0#{ <<"type">> => type_to_swagger_name(Type), <<"version">> => CB:version() },
    case maps:get(<<"ttl">>, Res1, 0) of
        0 -> maps:remove(<<"ttl">>, Res1);
        _ -> Res1
    end.

-spec serialize_to_binary(Tx :: tx()) -> term().
serialize_to_binary(#aetx{ cb = CB, type = Type, tx = Tx }) ->
    {Vsn, Fields} = CB:serialize(Tx),
    aec_object_serialization:serialize(
      Type,
      Vsn,
      CB:serialization_template(Vsn),
      Fields).

-spec deserialize_from_binary(Bin :: binary()) -> Tx :: tx().
deserialize_from_binary(Bin) ->
    {Type, Vsn, RawFields} =
        aec_object_serialization:deserialize_type_and_vsn(Bin),
    CB = type_to_cb(Type),
    Template = CB:serialization_template(Vsn),
    Fields = aec_serialization:decode_fields(Template, RawFields),
    #aetx{cb = CB, type = Type, size = byte_size(Bin), tx = CB:deserialize(Vsn, Fields)}.

type_to_cb(spend_tx)                  -> aec_spend_tx;
type_to_cb(oracle_register_tx)        -> aeo_register_tx;
type_to_cb(oracle_extend_tx)          -> aeo_extend_tx;
type_to_cb(oracle_query_tx)           -> aeo_query_tx;
type_to_cb(oracle_response_tx)        -> aeo_response_tx;
type_to_cb(name_preclaim_tx)          -> aens_preclaim_tx;
type_to_cb(name_claim_tx)             -> aens_claim_tx;
type_to_cb(name_transfer_tx)          -> aens_transfer_tx;
type_to_cb(name_update_tx)            -> aens_update_tx;
type_to_cb(name_revoke_tx)            -> aens_revoke_tx;
type_to_cb(name_create_tx)            -> aens_create_tx;
type_to_cb(contract_call_tx)          -> aect_call_tx;
type_to_cb(contract_create_tx)        -> aect_create_tx;
type_to_cb(channel_create_tx)         -> aesc_create_tx;
type_to_cb(channel_deposit_tx)        -> aesc_deposit_tx;
type_to_cb(channel_withdraw_tx)       -> aesc_withdraw_tx;
type_to_cb(channel_force_progress_tx) -> aesc_force_progress_tx;
type_to_cb(channel_close_solo_tx)     -> aesc_close_solo_tx;
type_to_cb(channel_close_mutual_tx)   -> aesc_close_mutual_tx;
type_to_cb(channel_slash_tx)          -> aesc_slash_tx;
type_to_cb(channel_settle_tx)         -> aesc_settle_tx;
type_to_cb(channel_snapshot_solo_tx)  -> aesc_snapshot_solo_tx;
type_to_cb(channel_offchain_tx)       -> aesc_offchain_tx.

type_to_swagger_name(spend_tx)                  -> <<"SpendTx">>;
type_to_swagger_name(oracle_register_tx)        -> <<"OracleRegisterTx">>;
type_to_swagger_name(oracle_extend_tx)          -> <<"OracleExtendTx">>;
type_to_swagger_name(oracle_query_tx)           -> <<"OracleQueryTx">>;
type_to_swagger_name(oracle_response_tx)        -> <<"OracleResponseTx">>;
type_to_swagger_name(name_preclaim_tx)          -> <<"NamePreclaimTx">>;
type_to_swagger_name(name_claim_tx)             -> <<"NameClaimTx">>;
type_to_swagger_name(name_transfer_tx)          -> <<"NameTransferTx">>;
type_to_swagger_name(name_update_tx)            -> <<"NameUpdateTx">>;
type_to_swagger_name(name_revoke_tx)            -> <<"NameRevokeTx">>;
type_to_swagger_name(name_create_tx)            -> <<"NameCreateTx">>;
type_to_swagger_name(contract_call_tx)          -> <<"ContractCallTx">>;
type_to_swagger_name(contract_create_tx)        -> <<"ContractCreateTx">>;
type_to_swagger_name(channel_create_tx)         -> <<"ChannelCreateTx">>;
type_to_swagger_name(channel_deposit_tx)        -> <<"ChannelDepositTx">>;
type_to_swagger_name(channel_withdraw_tx)       -> <<"ChannelWithdrawTx">>;
type_to_swagger_name(channel_force_progress_tx) -> <<"ChannelForceProgressTx">>;
type_to_swagger_name(channel_close_solo_tx)     -> <<"ChannelCloseSoloTx">>;
type_to_swagger_name(channel_close_mutual_tx)   -> <<"ChannelCloseMutualTx">>;
type_to_swagger_name(channel_slash_tx)          -> <<"ChannelSlashTx">>;
type_to_swagger_name(channel_settle_tx)         -> <<"ChannelSettleTx">>;
type_to_swagger_name(channel_snapshot_solo_tx)  -> <<"ChannelSnapshotSoloTx">>;
type_to_swagger_name(channel_offchain_tx)       -> <<"ChannelOffchainTx">>.

-spec specialize_type(Tx :: tx()) -> {tx_type(), tx_instance()}.
specialize_type(#aetx{ type = Type, tx = Tx }) -> {Type, Tx}.

-spec specialize_callback(Tx :: tx()) -> {module(), tx_instance()}.
specialize_callback(#aetx{ cb = CB, tx = Tx }) -> {CB, Tx}.

-spec update_tx(tx(), tx_instance()) -> tx().
update_tx(#aetx{} = Tx, NewTxI) ->
    Tx#aetx{tx = NewTxI}.

base_gas(Type) ->
    aec_governance:tx_base_gas(Type).

size_gas(Size) ->
    Size * aec_governance:byte_gas().

state_gas(Tag, {delta, TTL}) ->
    aec_governance_utils:state_gas(
      aec_governance:state_gas_per_block(Tag),
      TTL).

ttl_delta(_Height, {delta, _D} = TTL) ->
    {delta, aeo_utils:ttl_delta(0, TTL)};
ttl_delta(Height, {block, _H} = TTL) ->
    case aeo_utils:ttl_delta(Height, TTL) of
        TTLDelta when is_integer(TTLDelta) ->
            {delta, TTLDelta};
        {error, _Rsn} = Err ->
            Err
    end.

-ifdef(TEST).
tx(Tx) ->
    Tx#aetx.tx.
-endif.
