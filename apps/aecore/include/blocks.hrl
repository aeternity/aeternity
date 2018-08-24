-include("pow.hrl").

-define(PROTOCOL_VERSION, 22).
-define(GENESIS_VERSION, ?PROTOCOL_VERSION).
-define(GENESIS_HEIGHT, 0).
-define(GENESIS_TIME, 0).

-define(BLOCK_HEADER_HASH_BYTES, 32).
-define(TXS_HASH_BYTES, 32).
-define(STATE_HASH_BYTES, 32).
-define(MINER_PUB_BYTES, 32).
-define(BENEFICIARY_PUB_BYTES, 32).
-define(BLOCK_SIGNATURE_BYTES, 64).

-define(STORAGE_TYPE_BLOCK,  0).
-define(STORAGE_TYPE_HEADER, 1).
-define(STORAGE_TYPE_STATE,  2).

-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type(state_hash() :: <<_:(?STATE_HASH_BYTES*8)>>).
-type(miner_pubkey() :: <<_:(?MINER_PUB_BYTES*8)>>).
-type(beneficiary_pubkey() :: <<_:(?BENEFICIARY_PUB_BYTES*8)>>).
-type(block_header_hash() :: <<_:(?BLOCK_HEADER_HASH_BYTES*8)>>).
-type(block_signature() :: <<_:(?BLOCK_SIGNATURE_BYTES*8)>>).

-type(block_type() :: 'key' | 'micro').

-type(header_binary() :: binary()).
-type(deterministic_header_binary() :: binary()).
