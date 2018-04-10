%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx).

-include_lib("apps/aecore/include/common.hrl").

-export([ accounts/1
        , check/3
        , deserialize_from_binary/1
        , fee/1
        , hash/1
        , is_coinbase/1
        , is_tx_type/1
        , new/2
        , nonce/1
        , origin/1
        , is_verifiable/1
        , process/3
        , serialize_for_client/1
        , serialize_to_binary/1
        , signers/1
        , specialize_type/1
        , tx_type/1
        , tx_types/0]).

%% -- Types ------------------------------------------------------------------
-record(aetx, { type :: tx_type()
              , cb   :: module()
              , tx   :: tx_instance() }).

-opaque tx() :: #aetx{}.

-type tx_type() :: spend_tx
                 | coinbase_tx
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
                 | channel_close_mutual_tx
                 | channel_close_solo_tx
                 | channel_slash_tx
                 | channel_settle_tx
                 | channel_offchain_tx.

-type tx_instance() :: aec_spend_tx:tx()
                     | aec_coinbase_tx:tx()
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
                     | aesc_close_mutual_tx:tx()
                     | aesc_close_solo_tx:tx()
                     | aesc_slash_tx:tx()
                     | aesc_settle_tx:tx()
                     | aesc_offchain_tx:tx().

-export_type([ tx/0
             , tx_instance/0
             , tx_type/0 ]).

%% -- Behaviour definition ---------------------------------------------------

-callback new(Args :: map()) ->
    {ok, Tx :: tx()} | {error, Reason :: term()}.

-callback type() -> atom().

-callback fee(Tx :: tx_instance()) ->
    Fee :: integer().

-callback nonce(Tx :: tx_instance()) ->
    Nonce :: non_neg_integer() | undefined.

-callback origin(Tx :: tx_instance()) ->
    Origin :: pubkey() | undefined.

-callback accounts(Tx :: tx_instance()) ->
    [pubkey()].

-callback signers(Tx :: tx_instance()) ->
    [pubkey()].

-callback check(Tx :: tx_instance(), Trees :: aec_trees:trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.

-callback process(Tx :: tx_instance(), Trees :: aec_trees:trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()}.

-callback serialize(Tx :: tx_instance()) ->
    term().

-callback serialization_template(Vsn :: non_neg_integer()) ->
    term().

-callback deserialize(Vsn :: integer(), SerializedTx :: term()) ->
    Tx :: tx_instance().

-callback for_client(Tx :: tx_instance()) ->
    map().

-callback is_verifiable(Tx :: tx_instance()) ->
    boolean().

-optional_callbacks([is_verifiable/1]).

%% -- ADT Implementation -----------------------------------------------------

-spec new(CallbackModule :: module(),  Tx :: tx_instance()) ->
    Tx :: tx().
new(Callback, Tx) ->
    Type = Callback:type(),
    #aetx{ type = Type, cb = Callback, tx = Tx }.

-spec hash(Tx :: tx()) -> aec_hash:hash().
hash(Tx) ->
    aec_hash:hash(tx, serialize_to_binary(Tx)).

-spec tx_type(TxOrTxType :: tx_type() | tx()) -> binary().
tx_type(#aetx{ type = TxType }) ->
    tx_type(TxType);
tx_type(TxType) when is_atom(TxType) ->
    erlang:atom_to_binary(TxType, utf8).

-spec fee(Tx :: tx()) -> Fee :: integer().
fee(#aetx{ cb = CB, tx = Tx }) ->
    CB:fee(Tx).

-spec nonce(Tx :: tx()) -> Nonce :: non_neg_integer() | undefined.
nonce(#aetx{ cb = CB, tx = Tx }) ->
    CB:nonce(Tx).

-spec origin(Tx :: tx()) -> Origin :: pubkey() | undefined.
origin(#aetx{ cb = CB, tx = Tx }) ->
    CB:origin(Tx).

-spec accounts(Tx :: tx()) -> [pubkey()].
accounts(#aetx{ cb = CB, tx = Tx }) ->
    CB:accounts(Tx).

-spec signers(Tx :: tx()) -> [pubkey()].
signers(#aetx{ cb = CB, tx = Tx }) ->
    CB:signers(Tx).

-spec check(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.
check(#aetx{ cb = CB, tx = Tx }, Trees, Height) ->
    CB:check(Tx, Trees, Height).

-spec process(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()}.
process(#aetx{ cb = CB, tx = Tx }, Trees, Height) ->
    CB:process(Tx, Trees, Height).

-spec serialize_for_client(Tx :: tx()) -> map().
serialize_for_client(#aetx{ cb = CB, type = Type, tx = Tx }) ->
    Res = CB:for_client(Tx),
    Res#{ <<"type">> => tx_type(Type) }.

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

-spec is_verifiable(Tx :: tx()) -> boolean().
is_verifiable(Tx) ->
    call_optional_callback(Tx, is_verifiable, [], true).


call_optional_callback(#aetx{ cb = CB, tx = Tx }, FunAtom, Params0, Default) ->
    Params = [Tx | Params0],
    Arity = length(Params),
    case erlang:function_exported(CB, FunAtom, Arity) of
        true ->
            apply(CB, FunAtom, Params);
        false ->
            Default
    end.



type_to_cb(spend_tx)           -> aec_spend_tx;
type_to_cb(coinbase_tx)        -> aec_coinbase_tx;
type_to_cb(oracle_register_tx) -> aeo_register_tx;
type_to_cb(oracle_extend_tx)   -> aeo_extend_tx;
type_to_cb(oracle_query_tx)    -> aeo_query_tx;
type_to_cb(oracle_response_tx) -> aeo_response_tx;
type_to_cb(name_preclaim_tx)   -> aens_preclaim_tx;
type_to_cb(name_claim_tx)      -> aens_claim_tx;
type_to_cb(name_transfer_tx)   -> aens_transfer_tx;
type_to_cb(name_update_tx)     -> aens_update_tx;
type_to_cb(name_revoke_tx)     -> aens_revoke_tx;
type_to_cb(name_create_tx)     -> aens_create_tx;
type_to_cb(contract_call_tx)   -> aect_call_tx;
type_to_cb(contract_create_tx) -> aect_create_tx;
type_to_cb(channel_create_tx)       -> aesc_create_tx;
type_to_cb(channel_deposit_tx)      -> aesc_deposit_tx;
type_to_cb(channel_withdraw_tx)     -> aesc_withdraw_tx;
type_to_cb(channel_close_mutual_tx) -> aesc_close_mutual_tx;
type_to_cb(channel_close_solo_tx)   -> aesc_close_solo_tx;
type_to_cb(channel_slash_tx)        -> aesc_slash_tx;
type_to_cb(channel_settle_tx)       -> aesc_settle_tx;
type_to_cb(channel_offchain_tx)     -> aesc_offchain_tx.

-spec is_coinbase(Tx :: tx()) -> boolean().
is_coinbase(#aetx{ type = Type }) ->
    Type == coinbase_tx.

-spec specialize_type(Tx :: tx()) -> {tx_type(), tx_instance()}.
specialize_type(#aetx{ type = Type, tx = Tx }) -> {Type, Tx}.

-spec tx_types() -> list(tx_type()).
tx_types() ->
    [ spend_tx
    , coinbase_tx
    , oracle_register_tx
    , oracle_extend_tx
    , oracle_query_tx
    , oracle_response_tx
    , name_preclaim_tx
    , name_claim_tx
    , name_transfer_tx
    , name_update_tx
    , name_revoke_tx
    , name_create_tx
    , contract_call_tx
    , contract_create_tx
    , channel_deposit_tx
    , channel_withdraw_tx
    , channel_close_mutual_tx
    , channel_close_solo_tx
    , channel_slash_tx
    , channel_settle_tx
    , channel_offchain_tx
    ].

-spec is_tx_type(MaybeTxType :: binary() | atom()) -> boolean().
is_tx_type(X) when is_binary(X) ->
    try
        is_tx_type(erlang:binary_to_existing_atom(X, utf8))
    catch _:_ ->
            false
    end;
is_tx_type(X) when is_atom(X) ->
    lists:member(X, tx_types()).
