-include("trees.hrl").

-define(GENESIS_HEIGHT, 0).

-define(BLOCK_HEADER_HASH_BYTES, 32).
-type(block_header_hash() :: <<_:(?BLOCK_HEADER_HASH_BYTES*8)>>).

-record(block, {
          height = 0             :: height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash = <<>>       :: binary(), % Hash of all state Merkle trees included in #block.trees
          trees = #trees{}       :: trees(),
          txs = []               :: list(),
          difficulty = 0         :: non_neg_integer(),
          nonce = 0              :: non_neg_integer(),
          time = 0               :: non_neg_integer(),
          version = 0            :: non_neg_integer()}).
-type(block() :: #block{}).

-record(header, {
          height = 0             :: height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash = <<>>       :: binary(),
          difficulty = 0         :: non_neg_integer(),
          nonce = 0              :: non_neg_integer(),
          time = 0               :: non_neg_integer(),
          version = 0            :: non_neg_integer()}).
-type(header() :: #header{}).
