-define(BLOCK_HEADER_HASH_BYTES, 32).
-define(TXS_HASH_BYTES, 32).
-define(STATE_HASH_BYTES, 32).
-define(MINER_PUB_BYTES, 32).
-define(BENEFICIARY_PUB_BYTES, 32).
-define(BLOCK_SIGNATURE_BYTES, 64).
-define(OPTIONAL_INFO_BYTES, 4).

-define(KEY_HEADER_TAG, 1).
-define(MICRO_HEADER_TAG, 0).

-define(KEY_HEADER_MIN_BYTES, 364).
-define(MIC_HEADER_MIN_BYTES, 216).

-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type(state_hash() :: <<_:(?STATE_HASH_BYTES*8)>>).
-type(miner_pubkey() :: <<_:(?MINER_PUB_BYTES*8)>>).
-type(beneficiary_pubkey() :: <<_:(?BENEFICIARY_PUB_BYTES*8)>>).
-type(block_header_hash() :: <<_:(?BLOCK_HEADER_HASH_BYTES*8)>>).
-type(block_signature() :: <<_:(?BLOCK_SIGNATURE_BYTES*8)>>).

-type(block_type() :: 'key' | 'micro').

-type(header_binary() :: binary()).
-type(deterministic_header_binary() :: binary()).

-record(version, {major     :: 0..127,
                  minor     :: 0..127,
                  increment :: 0..127
        }).
-record(info_fields, {
          hole    :: boolean(),
          version :: #version{} | <<_:32>> | default
       }).
%% The info field is a 32 bit field.
%% In ae this is an opaque field (that may contain a node version).
%% In HC thus is a defined bitfield as follows:
%% Bit       31: 1 - A 'Hole' block, 0 - not a 'Hole' block.
%% Bits 21 - 30: Currently undefined/free.
%% Bits  0 - 20: Node version:
%% Bits          14 - 20: Major
%% Bits           7 - 13: Minor
%% Bits           0 -  6: Increment
