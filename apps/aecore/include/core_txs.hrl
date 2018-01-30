%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
-define(TX_HASH_BYTES, 32).
-type(tx_hash() :: <<_:(?TX_HASH_BYTES*8)>>).

-record(coinbase_tx, {
          account   = <<>> :: pubkey()}).
-type(coinbase_tx() :: #coinbase_tx{}).

-record(spend_tx, {
          sender    = <<>> :: pubkey(),
          recipient = <<>> :: pubkey(),
          amount    = 0    :: non_neg_integer(),
          fee       = 0    :: non_neg_integer(),
          nonce     = 0    :: non_neg_integer()}).
-type(spend_tx() :: #spend_tx{}).
