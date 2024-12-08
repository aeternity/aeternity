-type ct_config() :: proplists:proplist().

-define(MAIN_STAKING_CONTRACT, "MainStaking").
-define(HC_CONTRACT, "HCElection").
-define(CONSENSUS, hc).
-define(CHILD_EPOCH_LENGTH, 10).
-define(CHILD_BLOCK_TIME, 600).
-define(CHILD_BLOCK_PRODUCTION_TIME, (?CHILD_BLOCK_TIME div 4)).
-define(PARENT_EPOCH_LENGTH, 3).
-define(PARENT_FINALITY, 2).
-define(REWARD_DELAY, 2).

-define(NODE1, dev1).
-define(NODE1_NAME, aecore_suite_utils:node_name(?NODE1)).

-define(NODE2, dev2).
-define(NODE2_NAME, aecore_suite_utils:node_name(?NODE2)).

-define(NODE3, dev3).
-define(NODE3_NAME, aecore_suite_utils:node_name(?NODE3)).

-define(OWNER_PUBKEY, <<42:32/unit:8>>).

-define(PARENT_CHAIN_NODE, aecore_suite_utils:parent_chain_node(1)).
-define(PARENT_CHAIN_NODE_NAME, aecore_suite_utils:node_name(?PARENT_CHAIN_NODE)).
-define(PARENT_CHAIN_NETWORK_ID, <<"local_testnet">>).

-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(INITIAL_STAKE, 1_000_000_000_000_000_000_000_000).

-define(ALICE, {
    <<177, 181, 119, 188, 211, 39, 203, 57, 229, 94, 108, 2, 107, 214, 167, 74, 27, 53, 222, 108, 6,
        80, 196, 174, 81, 239, 171, 117, 158, 65, 91, 102>>,
    <<145, 69, 14, 254, 5, 22, 194, 68, 118, 57, 0, 134, 66, 96, 8, 20, 124, 253, 238, 207, 230,
        147, 95, 173, 161, 192, 86, 195, 165, 186, 115, 251, 177, 181, 119, 188, 211, 39, 203, 57,
        229, 94, 108, 2, 107, 214, 167, 74, 27, 53, 222, 108, 6, 80, 196, 174, 81, 239, 171, 117,
        158, 65, 91, 102>>,
    "Alice"
}).
%% ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm

-define(BOB, {
    <<103, 28, 85, 70, 70, 73, 69, 117, 178, 180, 148, 246, 81, 104, 33, 113, 6, 99, 216, 72, 147,
        205, 210, 210, 54, 3, 122, 84, 195, 62, 238, 132>>,
    <<59, 130, 10, 50, 47, 94, 36, 188, 50, 163, 253, 39, 81, 120, 89, 219, 72, 88, 68, 154, 183,
        225, 78, 92, 9, 216, 215, 59, 108, 82, 203, 25, 103, 28, 85, 70, 70, 73, 69, 117, 178, 180,
        148, 246, 81, 104, 33, 113, 6, 99, 216, 72, 147, 205, 210, 210, 54, 3, 122, 84, 195, 62,
        238, 132>>,
    "Bob"
}).
%% ak_nQpnNuBPQwibGpSJmjAah6r3ktAB7pG9JHuaGWHgLKxaKqEvC

-define(LISA, {
    <<200, 171, 93, 11, 3, 93, 177, 65, 197, 27, 123, 127, 177, 165, 190, 211, 20, 112, 79, 108, 85,
        78, 88, 181, 26, 207, 191, 211, 40, 225, 138, 154>>,
    <<237, 12, 20, 128, 115, 166, 32, 106, 220, 142, 111, 97, 141, 104, 201, 130, 56, 100, 64, 142,
        139, 163, 87, 166, 185, 94, 4, 159, 217, 243, 160, 169, 200, 171, 93, 11, 3, 93, 177, 65,
        197, 27, 123, 127, 177, 165, 190, 211, 20, 112, 79, 108, 85, 78, 88, 181, 26, 207, 191, 211,
        40, 225, 138, 154>>,
    "Lisa"
}).
%% ak_2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG

-define(DWIGHT, {
    <<8, 137, 159, 99, 139, 175, 27, 58, 77, 11, 191, 52, 198, 199, 7, 50, 133, 195, 184, 219, 148,
        124, 4, 5, 44, 247, 57, 95, 188, 173, 95, 35>>,
    <<107, 251, 189, 176, 92, 221, 4, 46, 56, 231, 137, 117, 181, 8, 124, 14, 212, 150, 167, 53, 95,
        94, 50, 86, 144, 230, 93, 222, 61, 116, 85, 96, 8, 137, 159, 99, 139, 175, 27, 58, 77, 11,
        191, 52, 198, 199, 7, 50, 133, 195, 184, 219, 148, 124, 4, 5, 44, 247, 57, 95, 188, 173, 95,
        35>>,
    %% Parent chain account
    "Dwight"
}).
%% ak_4m5iGyT3AiahzGKCE2fCHVsQYU7FBMDiaMJ1YPxradKsyfCc9

-define(EDWIN, {
    <<212, 212, 169, 78, 149, 148, 138, 221, 156, 80, 4, 156, 9, 139, 144, 114, 243, 122, 20, 103,
        168, 43, 42, 244, 93, 118, 38, 98, 71, 34, 199, 94>>,
    <<81, 177, 15, 108, 16, 183, 128, 229, 4, 114, 166, 227, 47, 125, 145, 21, 68, 196, 185, 115,
        42, 198, 168, 204, 220, 206, 200, 58, 12, 32, 56, 98, 212, 212, 169, 78, 149, 148, 138, 221,
        156, 80, 4, 156, 9, 139, 144, 114, 243, 122, 20, 103, 168, 43, 42, 244, 93, 118, 38, 98, 71,
        34, 199, 94>>,
    %% Parent chain account
    "Edwin"
}).
%% ak_2cjUYDhaKaiyGvuswL6K96ooKZKtFZZEopgxc3hwR2Yqb8SWxd

-define(FORD, {
    <<157, 139, 168, 202, 250, 128, 128, 7, 45, 18, 214, 147, 85, 31, 12, 182, 220, 213, 173, 237,
        6, 147, 239, 41, 183, 214, 34, 113, 100, 122, 208, 14>>,
    <<105, 184, 53, 188, 53, 158, 124, 5, 171, 89, 28, 64, 41, 203, 59, 179, 66, 53, 26, 132, 75,
        116, 139, 24, 228, 4, 200, 223, 25, 224, 76, 127, 157, 139, 168, 202, 250, 128, 128, 7, 45,
        18, 214, 147, 85, 31, 12, 182, 220, 213, 173, 237, 6, 147, 239, 41, 183, 214, 34, 113, 100,
        122, 208, 14>>,
    "Ford"
}).
%% ak_2CPHnpGxYw3T7XdUybxKDFGwtFQY7E5o3wJzbexkzSQ2BQ7caJ

-define(GENESIS_BENEFICIARY,
    <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0>>
).
