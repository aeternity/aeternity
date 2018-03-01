-include("pow.hrl").

-define(PROTOCOL_VERSION, 7).

-define(GENESIS_VERSION, 7).
-define(GENESIS_HEIGHT, 0).

-define(BLOCK_HEADER_HASH_BYTES, 32).
-define(TXS_HASH_BYTES, 32).
-define(STATE_HASH_BYTES, 32).

-define(ACCEPTED_FUTURE_BLOCK_TIME_SHIFT, 30 * 60 * 1000). %% 30 min

-define(STORAGE_TYPE_BLOCK,  0).
-define(STORAGE_TYPE_HEADER, 1).
-define(STORAGE_TYPE_STATE,  2).

-type(block_header_hash() :: <<_:(?BLOCK_HEADER_HASH_BYTES*8)>>).
-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type(state_hash() :: <<_:(?STATE_HASH_BYTES*8)>>).

-record(block, {
          height = 0              :: height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash = <<0:?STATE_HASH_BYTES/unit:8>> :: state_hash(), % Hash of all state Merkle trees
          txs_hash = <<0:?TXS_HASH_BYTES/unit:8>> :: txs_hash(),
          txs = []                :: list(aetx_sign:signed_tx()),
          target = ?HIGHEST_TARGET_SCI :: aec_pow:sci_int(),
          nonce = 0               :: non_neg_integer(),
          time = 0                :: non_neg_integer(),
          version = ?PROTOCOL_VERSION :: non_neg_integer(),
          pow_evidence = no_value :: aec_pow:pow_evidence()}).

-record(header, {
          height = 0              :: height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          txs_hash = <<0:?TXS_HASH_BYTES/unit:8>> :: txs_hash(),
          root_hash = <<>>        :: state_hash(),
          target = ?HIGHEST_TARGET_SCI :: aec_pow:sci_int(),
          nonce = 0               :: non_neg_integer(),
          time = 0                :: non_neg_integer(),
          version = ?PROTOCOL_VERSION :: non_neg_integer(),
          pow_evidence = no_value :: aec_pow:pow_evidence()}).

-type(header_binary() :: binary()).
-type(deterministic_header_binary() :: binary()).
