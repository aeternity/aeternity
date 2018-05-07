%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx).

-include_lib("apps/aecore/include/common.hrl").

-export([ accounts/1
        , check/4
        , check_from_contract/4
        , deserialize_from_binary/1
        , fee/1
        , hash/1
        , is_coinbase/1
        , is_tx_type/1
        , new/2
        , nonce/1
        , origin/1
        , process/4
        , process_from_contract/4
        , serialize_for_client/1
        , serialize_to_binary/1
        , signers/1
        , specialize_type/1
        , tx_type/1
        , tx_types/0]).

-ifdef(TEST).
-export([tx/1]).
-endif.

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
                 | contract_call_tx.

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
                     | aect_call_tx:tx().

%% @doc Where does this transaction come from? Is it a top level transaction or was it created by
%%      smart contract. In the latter case the fee logic is different.
-type tx_context() :: aetx_transaction | aetx_contract.

-export_type([ tx/0
             , tx_instance/0
             , tx_type/0
             , tx_context/0 ]).

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

-callback check(Tx :: tx_instance(), Context :: tx_context(),
                Trees :: aec_trees:trees(), Height :: non_neg_integer(),
                ConsensusVersion :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.

-callback process(Tx :: tx_instance(), Context :: tx_context(),
                  Trees :: aec_trees:trees(), Height :: non_neg_integer(),
                  ConsensusVersion :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()}.

-callback serialize(Tx :: tx_instance()) ->
    term().

-callback serialization_template(Vsn :: non_neg_integer()) ->
    term().

-callback deserialize(Vsn :: integer(), SerializedTx :: term()) ->
    Tx :: tx_instance().

-callback for_client(Tx :: tx_instance()) ->
    map().

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

-spec check(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer(),
            ConsensusVersion :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.
check(#aetx{ cb = CB, tx = Tx } = O, Trees, Height, ConsensusVersion) ->
    case is_coinbase(O) orelse (CB:fee(Tx) >= aec_governance:minimum_tx_fee()) of
        true ->
            CB:check(Tx, aetx_transaction, Trees, Height, ConsensusVersion);
        false ->
            {error, too_low_fee}
    end.

-spec check_from_contract(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer(),
                          ConsensusVersion :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.
check_from_contract(#aetx{ cb = CB, tx = Tx }, Trees, Height, ConsensusVersion) ->
    CB:check(Tx, aetx_contract, Trees, Height, ConsensusVersion).

-spec process(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer(),
              ConsensusVersion :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()}.
process(#aetx{ cb = CB, tx = Tx }, Trees, Height, ConsensusVersion) ->
    CB:process(Tx, aetx_transaction, Trees, Height, ConsensusVersion).

-spec process_from_contract(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer(),
                            ConsensusVersion :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()}.
process_from_contract(#aetx{ cb = CB, tx = Tx }, Trees, Height, ConsensusVersion) ->
    CB:process(Tx, aetx_contract, Trees, Height, ConsensusVersion).

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
type_to_cb(contract_create_tx) -> aect_create_tx.

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

-ifdef(TEST).
tx(Tx) ->
    Tx#aetx.tx.
-endif.
