-include("pow.hrl").

-define(PROTOCOL_VERSION, 21).
-define(GENESIS_VERSION, ?PROTOCOL_VERSION).
-define(GENESIS_HEIGHT, 0).
-define(GENESIS_TIME, 0).

-define(BLOCK_HEADER_HASH_BYTES, 32).
-define(TXS_HASH_BYTES, 32).
-define(STATE_HASH_BYTES, 32).
-define(MINER_PUB_BYTES, 32).
-define(BENEFICIARY_PUB_BYTES, 32).

-define(STORAGE_TYPE_BLOCK,  0).
-define(STORAGE_TYPE_HEADER, 1).
-define(STORAGE_TYPE_STATE,  2).

-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type(state_hash() :: <<_:(?STATE_HASH_BYTES*8)>>).
-type(miner_pubkey() :: <<_:(?MINER_PUB_BYTES*8)>>).
-type(beneficiary_pubkey() :: <<_:(?BENEFICIARY_PUB_BYTES*8)>>).
-type(block_header_hash() :: <<_:(?BLOCK_HEADER_HASH_BYTES*8)>>).

-type(block_type() :: key | micro).

-record(block, {
          height = 0              :: aec_blocks:height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash = <<0:?STATE_HASH_BYTES/unit:8>> :: state_hash(), % Hash of all state Merkle trees
          txs_hash = <<0:?TXS_HASH_BYTES/unit:8>> :: txs_hash(),
          txs = []                :: list(aetx_sign:signed_tx()),
          target = ?HIGHEST_TARGET_SCI :: aec_pow:sci_int(),
          nonce = 0               :: non_neg_integer(),
          time = ?GENESIS_TIME    :: non_neg_integer(),
          version                 :: non_neg_integer(),
          pow_evidence = no_value :: aec_pow:pow_evidence(),
          miner = <<0:?MINER_PUB_BYTES/unit:8>> :: miner_pubkey(),
          signature = undefined   :: binary() | undefined,
          beneficiary = <<0:?BENEFICIARY_PUB_BYTES/unit:8>> :: beneficiary_pubkey()}).

-record(header, {
          height = 0              :: aec_blocks:height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          txs_hash = <<0:?TXS_HASH_BYTES/unit:8>> :: txs_hash(),
          root_hash = <<>>        :: state_hash(),
          target = ?HIGHEST_TARGET_SCI :: aec_pow:sci_int(),
          nonce = 0               :: non_neg_integer(),
          time = ?GENESIS_TIME    :: non_neg_integer(),
          version                 :: non_neg_integer(),
          pow_evidence = no_value :: aec_pow:pow_evidence(),
          miner = <<0:?MINER_PUB_BYTES/unit:8>> :: miner_pubkey(), %% TODO: remove default, confusing
          beneficiary = <<0:?BENEFICIARY_PUB_BYTES/unit:8>> :: beneficiary_pubkey()}). %% TODO: separate records for key and micro blocks..

-type(header_binary() :: binary()).
-type(deterministic_header_binary() :: binary()).
