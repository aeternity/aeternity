-define(BLOCK_HEADER_HASH_BYTES, 32).
-define(TXS_HASH_BYTES, 32).
-define(STATE_HASH_BYTES, 32).
-define(MINER_PUB_BYTES, 32).
-define(BENEFICIARY_PUB_BYTES, 32).
-define(BLOCK_SIGNATURE_BYTES, 64).
-define(OPTIONAL_INFO_BYTES, 4).
-define(FLAG_BYTES, 4).


-define(KEY_HEADER_TAG, 1).
-define(MICRO_HEADER_TAG, 0).

%% The flag field is a 32 bit (4 bytes) field.
%% With the following meaning:
%% BIT        MEANING
%% 31         Block Type: 1 - key, 0 - micro (KEY_HEADER_TAG/MICRO_HEADER_TAG)
%% 30         For key block: ContainsInfo 1 - true, 0 - false
%%            For micro blocks: PoFFlag: 1 - pof_hash, 0 no pof_hash
%% 29         HC Hole block
%% 0 - 28     Unused shold be 0.

-define(KEY_HEADER_MIN_BYTES, 364).
-define(MIC_HEADER_MIN_BYTES, 216).

-type(txs_hash() :: <<_:(?TXS_HASH_BYTES*8)>>).
-type(state_hash() :: <<_:(?STATE_HASH_BYTES*8)>>).
-type(miner_pubkey() :: <<_:(?MINER_PUB_BYTES*8)>>).
-type(beneficiary_pubkey() :: <<_:(?BENEFICIARY_PUB_BYTES*8)>>).
-type(block_header_hash() :: <<_:(?BLOCK_HEADER_HASH_BYTES*8)>>).
-type(block_signature() :: <<_:(?BLOCK_SIGNATURE_BYTES*8)>>).
-type(block_header_flags() ::  <<_:?FLAG_BYTES*8>>).

-type(block_type() :: 'key' | 'micro').

-type(header_binary() :: binary()).
-type(deterministic_header_binary() :: binary()).
