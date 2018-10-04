%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
%%% @doc
%%% ADT containing all different transactions
%%% @end
%%%-------------------------------------------------------------------

-module(aetx).

-export([ accounts/1
        , check/3
        , deserialize_from_binary/1
        , fee/1
        , gas/1
        , gas_price/1
        , ttl/1
        , new/2
        , nonce/1
        , origin/1
        , process/3
        , serialize_for_client/1
        , serialize_to_binary/1
        , signers/2
        , specialize_type/1
        , specialize_callback/1
        , update_tx/2]).

-ifdef(TEST).
-export([tx/1]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-record(aetx, { type :: tx_type()
              , cb   :: module()
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
    GasPrice :: aect_contracts:amount().

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
    {ok, NewTrees :: aec_trees:trees()}.

-callback serialize(Tx :: tx_instance()) ->
    term().

-callback serialization_template(Vsn :: non_neg_integer()) ->
    term().

-callback deserialize(Vsn :: integer(), SerializedTx :: term()) ->
    Tx :: tx_instance().

-callback for_client(Tx :: tx_instance()) ->
    map().

%%%===================================================================
%%% Getters and setters
%%%===================================================================

-spec new(CallbackModule :: module(),  Tx :: tx_instance()) ->
    Tx :: tx().
new(Callback, Tx) ->
    Type = Callback:type(),
    #aetx{ type = Type, cb = Callback, tx = Tx }.

-spec fee(Tx :: tx()) -> Fee :: integer().
fee(#aetx{ cb = CB, tx = Tx }) ->
    CB:fee(Tx).

-spec gas(Tx :: tx()) -> Gas :: non_neg_integer().
gas(#aetx{ cb = CB, tx = Tx }) ->
    CB:gas(Tx).

-spec gas_price(Tx :: tx()) -> GasPrice :: non_neg_integer().
gas_price(#aetx{ cb = CB, tx = Tx }) ->
    CB:gas_price(Tx).

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

%%%===================================================================
%%% Checking transactions
%%%===================================================================

-spec check(tx(), aec_trees:trees(), aetx_env:env()) ->
               {ok, aec_trees:trees()} | {error, term()}.

check(Tx, Trees, Env) ->
    case aetx_env:context(Env) of
        aetx_transaction -> check_tx(Tx, Trees, Env);
        aetx_contract    -> check_contract(Tx, Trees, Env)
    end.

check_contract(#aetx{ cb = CB, tx = Tx }, Trees, Env) ->
    CB:check(Tx, Trees, Env).

check_tx(#aetx{ cb = CB, tx = Tx } = AeTx, Trees, Env) ->
    case CB:fee(Tx) >= aec_governance:minimum_tx_fee() of
        false ->
            {error, too_low_fee};
        true  ->
            case ttl(AeTx) >= aetx_env:height(Env) of
                false ->
                    {error, ttl_expired};
                true ->
                    CB:check(Tx, Trees, Env)
            end
    end.

%%%===================================================================
%%% Processing transactions
%%%===================================================================

-spec process(tx(), aec_trees:trees(), aetx_env:env()) ->
                 {ok, NewTrees :: aec_trees:trees()}.
process(#aetx{ cb = CB, tx = Tx }, Trees, Env) ->
    CB:process(Tx, Trees, Env).


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
    #aetx{cb = CB, type = Type, tx = CB:deserialize(Vsn, Fields)}.

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

-ifdef(TEST).
tx(Tx) ->
    Tx#aetx.tx.
-endif.
