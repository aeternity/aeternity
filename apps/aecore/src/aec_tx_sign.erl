-module(aec_tx_sign).

%% API
-export([data/1,
         verify/1]).
-export([serialize/1,
         deserialize/1]).

-include("common.hrl").
-include("txs.hrl").

data(SignedTx) ->
    SignedTx#signed_tx.data.

verify(failed_tx) ->
    %% TODO: Verify signed txs
    {error, verification_failed};
verify(_Tx) ->
    ok.

%% TODO This is meant to be the deterministic canonical serialization.
serialize(SignedTx = #signed_tx{data = Tx, signatures = [_]}) when
      is_tuple(Tx) ->
    term_to_binary(SignedTx).

%% TODO This is meant to be the deserialization of the deterministic
%% canonical serialization.
deserialize(B) ->
    case binary_to_term(B) of
        #signed_tx{signatures = [_]} = SignedTx ->
            SignedTx
    end.
