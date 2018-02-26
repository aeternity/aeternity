%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx).

-include_lib("apps/aecore/include/common.hrl").

-export([ accounts/1
        , check/3
        , deserialize/1
        , deserialize_from_binary/1
        , fee/1
        , hash/1
        , is_coinbase/1
        , is_tx_type/1
        , new/2
        , nonce/1
        , origin/1
        , process/3
        , serialize/1
        , serialize_for_client/1
        , serialize_to_binary/1
        , signers/1
        , specialize_type/1
        , tx_type/1
        , tx_types/0]).

%% -- Types ------------------------------------------------------------------
-record(aetx, { type :: tx_type()
              , tx   :: tx_instance() }).

-opaque tx() :: #aetx{}.

-type tx_type() :: aec_spend_tx
                 | aec_coinbase_tx
                 | aeo_register_tx
                 | aeo_extend_tx
                 | aeo_query_tx
                 | aeo_response_tx
                 | aens_preclaim_tx
                 | aens_claim_tx
                 | aens_transfer_tx
                 | aens_update_tx
                 | aens_revoke_tx
                 | aect_create_tx
                 | aect_call_tx.

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

-export_type([ tx/0
             , tx_instance/0
             , tx_type/0 ]).

%% -- Behaviour definition ---------------------------------------------------

-callback new(Args :: map()) ->
    {ok, Tx :: tx()} | {error, Reason :: term()}.

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

-callback deserialize(SerializedTx :: term()) ->
    Tx :: tx_instance().

-callback for_client(Tx :: tx_instance()) ->
    map().

%% -- ADT Implementation -----------------------------------------------------

-spec new(Type :: tx_type(),  Tx :: tx_instance()) ->
    Tx :: tx().
new(Type, Tx) ->
    #aetx{ type = Type, tx = Tx }.

-spec hash(Tx :: tx()) -> aec_hash:hash().
hash(Tx) ->
    aec_hash:hash(tx, serialize_to_binary(Tx)).

-spec tx_type(TxOrTxType :: tx_type() | tx()) -> binary().
tx_type(#aetx{ type = TxType }) ->
    tx_type(TxType);
tx_type(TxType) when is_atom(TxType) ->
    erlang:atom_to_binary(TxType, utf8).

-spec fee(Tx :: tx()) -> Fee :: integer().
fee(#aetx{ type = Type, tx = Tx }) ->
    Type:fee(Tx).

-spec nonce(Tx :: tx()) -> Nonce :: non_neg_integer() | undefined.
nonce(#aetx{ type = Type, tx = Tx }) ->
    Type:nonce(Tx).

-spec origin(Tx :: tx()) -> Origin :: pubkey() | undefined.
origin(#aetx{ type = Type, tx = Tx }) ->
    Type:origin(Tx).

-spec accounts(Tx :: tx()) -> [pubkey()].
accounts(#aetx{ type = Type, tx = Tx }) ->
    Type:accounts(Tx).

-spec signers(Tx :: tx()) -> [pubkey()].
signers(#aetx{ type = Type, tx = Tx }) ->
    Type:signers(Tx).

-spec check(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()} | {error, Reason :: term()}.
check(#aetx{ type = Type, tx = Tx }, Trees, Height) ->
    Type:check(Tx, Trees, Height).

-spec process(Tx :: tx(), Trees :: aec_trees:trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: aec_trees:trees()}.
process(#aetx{ type = Type, tx = Tx }, Trees, Height) ->
    Type:process(Tx, Trees, Height).

-spec serialize(Tx :: tx()) -> list(map()).
serialize(#aetx{ type = Type, tx = Tx }) ->
    [#{ <<"type">> => erlang:atom_to_binary(Type, utf8) } | Type:serialize(Tx)].

-spec serialize_for_client(Tx :: tx()) -> map().
serialize_for_client(#aetx{ type = Type, tx = Tx }) ->
    Res = Type:for_client(Tx),
    Res#{ <<"type">> => erlang:atom_to_binary(Type, utf8) }.

-spec serialize_to_binary(Tx :: tx()) -> term().
serialize_to_binary(Tx) ->
    msgpack:pack(serialize(Tx)).

-spec deserialize(list(map())) -> Tx :: tx().
deserialize([#{ <<"type">> := BinType } | SerializedData]) ->
    %% TODO: improve this... try ... catch at least
    Type = erlang:binary_to_existing_atom(BinType, utf8),
    new(Type, Type:deserialize(SerializedData)).

-spec deserialize_from_binary(Bin :: binary()) -> Tx :: tx().
deserialize_from_binary(Bin) ->
    {ok, Unpacked} = msgpack:unpack(Bin),
    deserialize(Unpacked).

-spec is_coinbase(Tx :: tx()) -> boolean().
is_coinbase(#aetx{ type = Type }) ->
    Type == aec_coinbase_tx.

-spec specialize_type(Tx :: tx()) -> {tx_type(), tx_instance()}.
specialize_type(#aetx{ type = Type, tx = Tx }) -> {Type, Tx}.

-spec tx_types() -> list(tx_type()).
tx_types() ->
    [ aec_spend_tx
    , aec_coinbase_tx
    , aeo_register_tx
    , aeo_extend_tx
    , aeo_query_tx
    , aeo_response_tx
    , aens_preclaim_tx
    , aens_claim_tx
    , aens_transfer_tx
    , aens_update_tx
    , aens_revoke_tx
    , aect_create_tx
    , aect_call_tx
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

