%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

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
