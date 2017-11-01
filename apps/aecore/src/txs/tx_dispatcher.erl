-module(tx_dispatcher).
-export([handler/1,handler_by_type/1]).
-type pubkey() :: binary().
-record(coinbase_tx,{account = <<>> :: pubkey()}).
handler(#coinbase_tx{}) ->
    aec_coinbase_tx.
handler_by_type(<<"coinbase">>) ->
    aec_coinbase_tx.
