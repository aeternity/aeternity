-module(aec_tx_sign).

%% API
-export([data/1,
         verify/1]).
-export([serialize/1,
         serialize_to_binary/1,
         deserialize/1,
         deserialize_from_binary/1]).

-include("common.hrl").
-include("txs.hrl").

data(SignedTx) ->
    SignedTx#signed_tx.data.

verify(failed_tx) ->
    %% TODO: Verify signed txs
    {error, verification_failed};
verify(_Tx) ->
    ok.

-spec serialize(#signed_tx{}) -> map().
serialize(#signed_tx{data = Tx, signatures = Sigs})  when is_tuple(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Data = Mod:serialize(Tx),
    Type = Mod:type(),
    #{<<"type">> => Type, 
      <<"data">> => Data,
      <<"signatures">> => lists:map(fun base64:encode/1, Sigs)}.


-spec deserialize(map()) -> #signed_tx{}.
deserialize(#{<<"type">> := Type,
              <<"signatures">> := Sigs,
              <<"data">> := Data}) ->
    Mod = tx_dispatcher:handler_by_type(Type),
    Tx = Mod:deserialize(Data),
    #signed_tx{data = Tx,
               signatures = lists:map(fun base64:decode/1, Sigs)}.

%% deterministic canonical serialization.
-spec serialize_to_binary(#signed_tx{}) -> binary().
serialize_to_binary(#signed_tx{} = SignedTx) ->
    jsx:encode(serialize(SignedTx)).

-spec deserialize_from_binary(binary()) -> #signed_tx{}.
deserialize_from_binary(SignedTxBin) when is_binary(SignedTxBin) ->
    deserialize(jsx:decode(SignedTxBin)).

