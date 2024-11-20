-define(BLOCK_HEADER_HASH_BYTES, 32).
-define(TXS_HASH_BYTES, 32).
-define(STATE_HASH_BYTES, 32).
-define(MINER_PUB_BYTES, 32).
-define(BENEFICIARY_PUB_BYTES, 32).
-define(BLOCK_SIGNATURE_BYTES, 64).
-define(OPTIONAL_INFO_BYTES, 4).
-define(FLAG_BYTES, 4).
-define(FLAG_BITS, 32).

-define(SET(X,Y),((X) bor (Y))).
-define(CLR(X,Y),((X) band (bnot (Y)))).

-define(KEY_HEADER_TAG,              1).
-define(KEY_HEADER_FLAG,    (1 bsl 31)).
-define(CONTAINS_INFO_FLAG, (1 bsl 30)).
-define(HOLE_FLAG,          (1 bsl 29)).


-define(MICRO_HEADER_TAG,            0).
-define(MICRO_HEADER_FLAG,           0).
-define(POF_FLAG,           (1 bsl 30)).


%% The flag field is a 32 bit (4 bytes) field, bits 31 to 0.
%% %% With the following meaning:
%%
%% BIT        MEANING
%% 31         Block Type: 1 - key, 0 - micro (KEY_HEADER_TAG/MICRO_HEADER_TAG)
%%
%% Key blocks (bit 31:1)
%% 30         ContainsInfo 1 - true, 0 - false
%% 29         RESERVED (HC Hole block: 1 - a hole block, 0 - ordinary block)
%% 0 - 28     Unused, should be 0.
%%
%% Micro blocks (bit 31:0)
%% 30         PoFFlag: 1 - pof_hash, 0 - no pof_hash
%% 0 - 29     Unused, should be 0.


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
