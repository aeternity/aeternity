-module(tx_dispatcher).
-export([handler/1,
         handler_by_type/1]).

-include("common.hrl").
-include("txs.hrl").

handler(#coinbase_tx{}) ->
    aec_coinbase_tx;
handler(#spend_tx{}) ->
    aec_spend_tx.

handler_by_type(<<"coinbase">>) ->
    aec_coinbase_tx;
handler_by_type(<<"spend">>) ->
    aec_spend_tx.
