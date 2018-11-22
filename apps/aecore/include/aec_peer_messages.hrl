-define(REQUEST_TIMEOUT, 30000).

-define(MSG_FRAGMENT, 0).
-define(MSG_P2P_RESPONSE, 100).
-define(MSG_PING, 1).
-define(MSG_PING_RSP, 2).
-define(MSG_GET_HEADER_BY_HASH, 3).
-define(MSG_HEADER, 4).
-define(MSG_GET_N_SUCCESSORS, 5).
-define(MSG_HEADER_HASHES, 6).
-define(MSG_GET_BLOCK_TXS, 7).
-define(MSG_GET_GENERATION, 8).
-define(MSG_TXS, 9).
-define(MSG_BLOCK_TXS, 13).
-define(MSG_KEY_BLOCK, 10).
-define(MSG_MICRO_BLOCK, 11).
-define(MSG_GENERATION, 12).
-define(MSG_GET_HEADER_BY_HEIGHT, 15).
-define(MSG_CLOSE, 127).

-define(MSG_TX_POOL_SYNC_INIT,   20).
-define(MSG_TX_POOL_SYNC_UNFOLD, 21).
-define(MSG_TX_POOL_SYNC_GET,    22).
-define(MSG_TX_POOL_SYNC_FINISH, 23).

-define(RESPONSE_VSN, 1).
-define(PING_VSN, 1).
-define(GET_MEMPOOL_VSN, 1).
-define(MEMPOOL_VSN, 1).
-define(GET_HEADER_BY_HASH_VSN, 1).
-define(GET_HEADER_BY_HEIGHT_VSN, 2).
-define(HEADER_VSN, 1).
-define(GET_N_SUCCESSORS_VSN, 2).
-define(HEADER_HASHES_VSN, 1).
-define(GET_BLOCK_TXS_VSN, 1).
-define(GET_GENERATION_VSN, 1).
-define(KEY_BLOCK_VSN, 1).
-define(MICRO_BLOCK_VSN, 1).
-define(GENERATION_VSN, 1).
-define(TXS_VSN, 1).
-define(BLOCK_TXS_VSN, 1).
-define(PEER_VSN, 1).
-define(TX_POOL_SYNC_INIT_VSN,   1).
-define(TX_POOL_SYNC_UNFOLD_VSN, 1).
-define(TX_POOL_SYNC_GET_VSN,    1).
-define(TX_POOL_SYNC_FINISH_VSN, 1).
-define(CLOSE_VSN, 1).

-define(VSN_1, 1).

