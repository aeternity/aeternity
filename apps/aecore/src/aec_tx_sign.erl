-module(aec_tx_sign).

%% API
-export([data/1,
         verify/1]).

-include("common.hrl").
-include("txs.hrl").

data(SignedTx) ->
    SignedTx#signed_tx.data.

verify(failed_tx) ->
    %% TODO: Verify signed txs
    {error, verification_failed};
verify(_Tx) ->
    ok.
