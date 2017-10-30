-module(aec_tx_sign).

%% API
-export([data/1,
         signatures/1,
         verify/1]).
-export([serialize/1,
         serialize_to_binary/1,
         deserialize/1,
         deserialize_from_binary/1]).

-include("common.hrl").
-include("txs.hrl").

data(#signed_tx{data = Data}) ->
    Data.

signatures(#signed_tx{signatures = Sigs}) ->
    Sigs.

verify(failed_tx) ->
    {error, verification_failed};
verify(#signed_tx{data = Data, signatures = Sigs}) ->
    case aec_keys:verify(Sigs, Data) of
        true -> ok;
        false ->
            lager:debug("No matching sigs (~p - ~p)", [Sigs]),
            {error, signature_check_failed}
    end.

-define(SIG_TX_TYPE, <<"sig_tx">>).
-define(SIG_TX_VSN, 1).

version() -> ?SIG_TX_VSN.
type()    -> ?SIG_TX_TYPE.

-spec serialize(#signed_tx{}) -> list().
serialize(#signed_tx{data = Tx, signatures = Sigs})  when is_tuple(Tx) ->
    TxSer = aec_tx:serialize(Tx),
    [type(), version(), TxSer, Sigs].

-spec deserialize(list()) -> #signed_tx{}.
deserialize([?SIG_TX_TYPE, ?SIG_TX_VSN, TxSer, Sigs]) ->
    Tx = aec_tx:deserialize(TxSer),
    #signed_tx{data = Tx, signatures = Sigs}.

%% deterministic canonical serialization.
-spec serialize_to_binary(#signed_tx{}) -> binary().
serialize_to_binary(#signed_tx{} = SignedTx) ->
    msgpack:pack(serialize(SignedTx)).

-spec deserialize_from_binary(binary()) -> #signed_tx{}.
deserialize_from_binary(SignedTxBin) when is_binary(SignedTxBin) ->
    {ok, Unpacked} = msgpack:unpack(SignedTxBin),
    lager:debug("unpacked Tx: ~p", [Unpacked]),
    deserialize(Unpacked).
