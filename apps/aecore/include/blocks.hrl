-include("trees.hrl").

-record(block, {
          height = 0             :: non_neg_integer(),
          prev_hash = <<>>       :: binary(),
          root_hash = <<>>       :: binary(), % Hash of all state Merkle trees included in #block.trees
          trees = #trees{}       :: trees(),
          txs = []               :: list(),
          difficulty = 0         :: non_neg_integer(),
          nonce = 0              :: non_neg_integer(),
          time = 0               :: non_neg_integer(),
          version = 0            :: non_neg_integer(),
          pow_evidence = []      :: list(non_neg_integer())}).
-type(block() :: #block{}).

-record(header, {
          height = 0             :: non_neg_integer(),
          prev_hash = <<>>       :: binary(),
          root_hash = <<>>       :: binary(),
          difficulty = 0         :: non_neg_integer(),
          nonce = 0              :: non_neg_integer(),
          time = 0               :: non_neg_integer(),
          version = 0            :: non_neg_integer(),
          pow_evidence = []      :: list(non_neg_integer())}).
-type(header() :: #header{}).
