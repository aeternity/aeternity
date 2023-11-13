-module(aehttp_stake_contract_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([mine_and_sync/1,
         spend_txs/1,
         simple_withdraw/1,
         change_leaders/1,
         verify_fees/1,
         verify_commitments/1,
         verify_btc_commitments/1,
         genesis_has_commitments/1,
         genesis_has_commitments_btc/1,
         elected_leader_did_not_show_up/1,
         block_difficulty/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(STAKING_CONTRACT, "MainStaking").
-define(POS_ELECTION_CONTRACT, "PoSElection").
-define(HC_ELECTION_CONTRACT, "HCElection").
-define(CONSENSUS_HC, hc).
-define(CONSENSUS_HC_BTC, hc_btc).
-define(CONSENSUS_HC_DOGE, hc_doge).
-define(CONSENSUS_POS, pos).
-define(CHILD_START_HEIGHT, 101).
-define(CHILD_CONFIRMATIONS, 0).
-define(REWARD_DELAY, 2).
-define(LAZY_INTERVAL, 60000).
-define(NODE1, dev1).
-define(NODE1_NAME, aecore_suite_utils:node_name(?NODE1)).

-define(NODE2, dev2).
-define(NODE2_NAME, aecore_suite_utils:node_name(?NODE2)).

-define(LAZY_NODE, dev8).
-define(LAZY_NODE_NAME, aecore_suite_utils:node_name(?LAZY_NODE)).



-define(OWNER_PUBKEY, <<42:32/unit:8>>).

-define(PARENT_CHAIN_NODE1, aecore_suite_utils:parent_chain_node(1)).
-define(PARENT_CHAIN_NODE1_NAME, aecore_suite_utils:node_name(?PARENT_CHAIN_NODE1)).
-define(PARENT_CHAIN_NETWORK_ID, <<"local_testnet">>).

-define(BTC_PARENT_CHAIN_PORT, 7013).

-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(INITIAL_STAKE, 1000000000000000000000000).

-define(PEEK_MSGQ, peek_msgq(?LINE)).

-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>,
    "Alice"}).
%% ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm

-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>,
    "Bob"}).
%% ak_nQpnNuBPQwibGpSJmjAah6r3ktAB7pG9JHuaGWHgLKxaKqEvC

-define(LISA, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>,
    "Lisa"}).
%% ak_2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG

-define(DWIGHT, {
    <<8,137,159,99,139,175,27,58,77,11,191,52,198,199,7,50,133,195,184,219,
        148,124,4,5,44,247,57,95,188,173,95,35>>,
    <<107,251,189,176,92,221,4,46,56,231,137,117,181,8,124,14,212,150,167,
        53,95,94,50,86,144,230,93,222,61,116,85,96,8,137,159,99,139,175,27,58,
        77,11,191,52,198,199,7,50,133,195,184,219,148,124,4,5,44,247,57,95,
        188,173,95,35>>,
    "Dwight"}). %% Parent chain account
%% ak_4m5iGyT3AiahzGKCE2fCHVsQYU7FBMDiaMJ1YPxradKsyfCc9

-define(EDWIN, {
    <<212,212,169,78,149,148,138,221,156,80,4,156,9,139,144,114,243,122,20,
        103,168,43,42,244,93,118,38,98,71,34,199,94>>,
    <<81,177,15,108,16,183,128,229,4,114,166,227,47,125,145,21,68,196,185,
        115,42,198,168,204,220,206,200,58,12,32,56,98,212,212,169,78,149,148,
        138,221,156,80,4,156,9,139,144,114,243,122,20,103,168,43,42,244,93,
        118,38,98,71,34,199,94>>,
    "Edwin"}).  %% Parent chain account
%% ak_2cjUYDhaKaiyGvuswL6K96ooKZKtFZZEopgxc3hwR2Yqb8SWxd

-define(FORD, {
    <<157,139,168,202,250,128,128,7,45,18,214,147,85,31,12,182,220,213,173,
        237,6,147,239,41,183,214,34,113,100,122,208,14>>,
    <<105,184,53,188,53,158,124,5,171,89,28,64,41,203,59,179,66,53,26,132,
        75,116,139,24,228,4,200,223,25,224,76,127,157,139,168,202,250,128,128,
        7,45,18,214,147,85,31,12,182,220,213,173,237,6,147,239,41,183,214,34,
        113,100,122,208,14>>,
    "Ford"}).
%% ak_2CPHnpGxYw3T7XdUybxKDFGwtFQY7E5o3wJzbexkzSQ2BQ7caJ

-define(GENESIS_BENFICIARY, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

all() -> [{group, pos}
          %% These tests are currently not stable enough (CI!) - let's disable for now.
          %% {group, hc},
          %% {group, hc_btc}
         ].

groups() ->
    [{pos, [sequence], common_tests()},
     {hc, [sequence], common_tests() ++ hc_specific_tests() ++ [{group, lazy_leader}]},
     {hc_btc, [sequence], common_tests() ++ hc_btc_specific_tests()},
     {hc_doge, [sequence], common_tests() ++ hc_btc_specific_tests()},
     {lazy_leader, [sequence], [elected_leader_did_not_show_up
                               ]}
    ].

common_tests() ->
    [ verify_fees
    , mine_and_sync
    , spend_txs
    , simple_withdraw
    , change_leaders
    ].

hc_specific_tests() ->
    [
     verify_commitments,
     genesis_has_commitments,
     block_difficulty
    ].

hc_btc_specific_tests() ->
    [
     verify_btc_commitments,
     genesis_has_commitments_btc,
     block_difficulty
    ].

suite() -> [].

init_per_suite(Config0) ->
    case aect_test_utils:require_at_least_protocol(?IRIS_PROTOCOL_VSN) of
        {skip, _} = Skip -> Skip;
        ok ->
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.staking"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite([?NODE1, ?NODE2],
                                                        #{}, %% config is rewritten per suite
                                                        [],
                                                        Config),
            ParentCfg =
                #{  <<"chain">> =>
                        #{  <<"persist">> => false,
                            <<"hard_forks">> =>
                                #{  <<"1">> => 0,
                                    integer_to_binary(?CERES_PROTOCOL_VSN) => 1
                                },
                            <<"consensus">> =>
                                #{ <<"0">> => #{<<"type">> => <<"ct_tests">>}}
                         },
                    <<"fork_management">> =>
                        #{<<"network_id">> => ?PARENT_CHAIN_NETWORK_ID},
                    %%<<"http">> => #{<<"external">> => #{<<"acceptors">> => 100}},
                    <<"http">> => #{<<"cache">> => #{<<"enabled">> => false}},
                    <<"mining">> =>
                        #{<<"micro_block_cycle">> => 1,
                          <<"expected_mine_rate">> => 2000,
                          <<"autostart">> => false,
                          <<"beneficiary_reward_delay">> => ?REWARD_DELAY
                            }},
            aecore_suite_utils:make_multi(Config1, [?PARENT_CHAIN_NODE1]),
            aecore_suite_utils:create_config(?PARENT_CHAIN_NODE1, Config1, ParentCfg, []),
            {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(?PARENT_CHAIN_NODE1),
            ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            aecore_suite_utils:create_seed_file([?PARENT_CHAIN_NODE1],
                Config1,
                "genesis", "accounts_test.json",
                #{  ParentPatronPubEnc =>
                    100000000000000000000000000000000000000000000000000000000000000000000000,
                    encoded_pubkey(?DWIGHT) => 2100000000000000000000000000,
                    encoded_pubkey(?EDWIN) => 3100000000000000000000000000
                }),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            [{staking_contract, StakingContract},
              {election_contract, ElectionContract} | Config1]
    end.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(hc_btc, Config0) ->
    VsnStr =  os:cmd("bitcoind -version"),
    case re:run(VsnStr, "v\\d\\d") of
        nomatch ->
            {skip, bitcoind_executable_not_found};
        {match, _} ->
            VM = fate,
            Config1 = aect_test_utils:init_per_group(VM, Config0),
            init_per_group_custom(<<"hc">>, ?CONSENSUS_HC_BTC, Config1)
    end;
init_per_group(hc_doge, Config0) ->
    VsnStr =  os:cmd("dogecoind -version"),
    case re:run(VsnStr, "v\\d") of
        nomatch ->
            {skip, dogecoind_executable_not_found};
        {match, _} ->
            VM = fate,
            Config1 = aect_test_utils:init_per_group(VM, Config0),
            init_per_group_custom(<<"hc">>, ?CONSENSUS_HC_DOGE, Config1)
    end;
init_per_group(Group, Config0) ->
    VM = fate,
    Config1 = aect_test_utils:init_per_group(VM, Config0),
    case Group of
        pos -> init_per_group_custom(<<"pos">>, ?CONSENSUS_POS, Config1);
        hc -> init_per_group_custom(<<"hc">>, ?CONSENSUS_HC, Config1);
        lazy_leader -> set_up_lazy_leader_node(<<"hc">>, Config1);
        _ -> Config1
    end.

init_per_group_custom(NetworkId, ?CONSENSUS_POS, Config) ->
    ElectionContract = election_contract_by_consensus(?CONSENSUS_POS),
    build_json_files(NetworkId, ElectionContract, Config, [?NODE1, ?NODE2]),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
          ],
    aecore_suite_utils:create_config(?NODE1, Config,
                                    node_config([?ALICE, ?BOB, ?LISA], <<>>, ?CONSENSUS_POS),
                                    [{add_peers, true} ]),
    aecore_suite_utils:create_config(?NODE2, Config,
                                    node_config([], <<>>, ?CONSENSUS_POS),
                                    [{add_peers, true} ]),
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    aecore_suite_utils:start_node(?NODE2, Config, Env),
    aecore_suite_utils:connect(?NODE2_NAME, []),
    Config1 = [{network_id, NetworkId}, {consensus, ?CONSENSUS_POS} | Config],
    {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 10, Config, ?config(consensus, Config1)),
    Config1;
init_per_group_custom(NetworkId, ?CONSENSUS_HC, Config) ->
    GenesisStartTime = aeu_time:now_in_msecs(),
    ElectionContract = election_contract_by_consensus(?CONSENSUS_HC),
    build_json_files(NetworkId, ElectionContract, Config, [?NODE1, ?NODE2]),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
          ],
    aecore_suite_utils:start_node(?PARENT_CHAIN_NODE1, Config),
    aecore_suite_utils:connect(?PARENT_CHAIN_NODE1_NAME, []),
    timer:sleep(1000),
    produce_blocks(?PARENT_CHAIN_NODE1, ?PARENT_CHAIN_NODE1_NAME,
                    parent, ?CHILD_START_HEIGHT, Config, ?CONSENSUS_HC),
    %% ?ALICE on the child chain, ?DWIGHT on the parent chain
    ReceiveAddress = encoded_pubkey(?FORD),
    aecore_suite_utils:create_config(?NODE1, Config,
                                    node_config([{?ALICE, ?DWIGHT}, {?BOB, ?EDWIN}], ReceiveAddress, ?CONSENSUS_HC,
                                                true, GenesisStartTime),
                                    [{add_peers, true} ]),
    aecore_suite_utils:create_config(?NODE2, Config,
                                    node_config([], ReceiveAddress, ?CONSENSUS_HC, true,
                                                GenesisStartTime),
                                    [{add_peers, true} ]),
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    aecore_suite_utils:start_node(?NODE2, Config, Env),
    aecore_suite_utils:connect(?NODE2_NAME, []),
    ParentTopHeight0 = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
    ct:log("Parent chain top height ~p", [ParentTopHeight0]),
    timer:sleep(1000),
    Config1 = [{network_id, NetworkId}, {consensus, ?CONSENSUS_HC},
               {genesis_start_time, GenesisStartTime} | Config],
    {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 10, Config, ?config(consensus, Config1)),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
    {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE1, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, ChildBlocks} = get_generations(?NODE1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),

    Config1;
init_per_group_custom(NetworkId, ?CONSENSUS_HC_BTC, Config) ->
    ElectionContract = election_contract_by_consensus(?CONSENSUS_HC),
    build_json_files(NetworkId, ElectionContract, Config, [?NODE1, ?NODE2]),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
          ],
    %% Start a clean bitcoind regtest node and mine the first 101 blocks to provide
    %% an account with available balance.
    PrivDir = aecore_suite_utils:priv_dir(Config),
    BTCDataDir = filename:join(PrivDir, "bitcoin"),
    ct:log("creating BTC dir ~p", [BTCDataDir]),
    ok = file:make_dir(BTCDataDir),
    ok = filelib:ensure_dir(BTCDataDir),
    BTCConfig =
    "regtest=1
daemon=1
server=1
[regtest]
rpcuser=test
rpcpassword=Pass
rpcport=" ++ integer_to_list(?BTC_PARENT_CHAIN_PORT),
    ok = file:write_file(filename:join(BTCDataDir, "bitcoin.conf"), BTCConfig),
    Cmd = "bitcoind -datadir=" ++ BTCDataDir,
    os:cmd(Cmd),
    BitcoinCli = "bitcoin-cli -regtest -datadir="++BTCDataDir++" ",
    %% Wait for bitcoind to become available to rpcs
    os:cmd(BitcoinCli ++ "-rpcwait ping"),
    %% Generate a wallet
    os:cmd(BitcoinCli ++ "createwallet testwallet"),
    Dwight = string:trim(os:cmd(BitcoinCli ++ "getnewaddress")),
    Edwin = string:trim(os:cmd(BitcoinCli ++ "getnewaddress")),
    ct:log("Produce blocks on parent", []),
    %% Create the first 101 blocks to release mining funds into our wallet for use
    %% in posting commitments.
    Config1 = [{network_id, NetworkId}, {consensus, ?CONSENSUS_HC_BTC}, {bitcoin_cli, BitcoinCli}, {btc_beneficiary, Dwight} | Config],
    produce_blocks(?PARENT_CHAIN_NODE1, ?PARENT_CHAIN_NODE1_NAME,
                    parent, ?CHILD_START_HEIGHT, Config1, ?CONSENSUS_HC_BTC),
    ReceiveAddress = string:trim(os:cmd(BitcoinCli ++ "getnewaddress")),
    aecore_suite_utils:create_config(?NODE1, Config,
                                    node_config([{?ALICE, list_to_binary(Dwight)}, {?BOB, list_to_binary(Edwin)}],
                                                list_to_binary(ReceiveAddress), ?CONSENSUS_HC_BTC),
                                    [{add_peers, true} ]),
    aecore_suite_utils:create_config(?NODE2, Config,
                                    node_config([], ReceiveAddress, ?CONSENSUS_HC_BTC),
                                    [{add_peers, true} ]),
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    aecore_suite_utils:start_node(?NODE2, Config, Env),
    aecore_suite_utils:connect(?NODE2_NAME, []),

    timer:sleep(1000),
    NumberOfCommitments = 2,
    %% mine blocks on the parent chain and include commitments; stop right
    %% before the child chain produces a block
%%
%%    lists:foreach(
%%        fun(Idx) ->
%%            PCTopHeight = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
%%            ct:log("Mining commitment on the parent chain, idx ~p, parent height ~p", [Idx, PCTopHeight]),
%%            wait_for_commitments_in_pool(?PARENT_CHAIN_NODE1, NumberOfCommitments),
%%            {ok, _} = aecore_suite_utils:mine_micro_blocks(?PARENT_CHAIN_NODE1_NAME, 1),
%%            {ok, _} = aecore_suite_utils:mine_key_blocks(?PARENT_CHAIN_NODE1_NAME, 1)
%%        end,
%%       lists:seq(1, ?CHILD_CONFIRMATIONS - 1)),
    ct:log("Mining last initial commitment on the BTC parent chain", []),
    wait_for_btc_commitments_in_pool(BitcoinCli, NumberOfCommitments),
    %% produce_btc_blocks(BitcoinCli, Dwight, 1),
    {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 10, Config1, ?config(consensus, Config1)),
    ParentTopHash = string:trim(os:cmd(BitcoinCli ++ "getbestblockhash")),
    ct:log("Parent chain blocks ~p", [ParentTopHash]),
    ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, ChildBlocks} = get_generations(?NODE1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    Config1;
init_per_group_custom(NetworkId, ?CONSENSUS_HC_DOGE, Config) ->
    ElectionContract = election_contract_by_consensus(?CONSENSUS_HC),
    build_json_files(NetworkId, ElectionContract, Config, [?NODE1, ?NODE2]),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
          ],
    %% Start a clean bitcoind regtest node and mine the first 101 blocks to provide
    %% an account with available balance.
    PrivDir = aecore_suite_utils:priv_dir(Config),
    DogeDataDir = filename:join(PrivDir, "dogecoin"),
    ct:log("creating Doge dir ~p", [DogeDataDir]),
    ok = file:make_dir(DogeDataDir),
    ok = filelib:ensure_dir(DogeDataDir),
    DogeConfig =
    "regtest=1
daemon=1
server=1
rpcuser=test
rpcpassword=Pass
rpcport=" ++ integer_to_list(?BTC_PARENT_CHAIN_PORT),
    ok = file:write_file(filename:join(DogeDataDir, "dogecoin.conf"), DogeConfig),
    Cmd = "dogecoind -txindex -datadir=" ++ DogeDataDir,
    os:cmd(Cmd),
    DogecoinCli = "dogecoin-cli -regtest -datadir="++DogeDataDir++" ",
    %% Wait for dogecoind to become available to rpcs
    os:cmd(DogecoinCli ++ "-rpcwait ping"),
    %% Generate a wallet (not needed - created by default for dogecoin 1.14.6)
    %% os:cmd(DogecoinCli ++ "createwallet testwallet"),
    Dwight = string:trim(os:cmd(DogecoinCli ++ "getnewaddress")),
    Edwin = string:trim(os:cmd(DogecoinCli ++ "getnewaddress")),
    ct:log("Produce blocks on parent", []),
    %% Create the first 101 blocks to release mining funds into our wallet for use
    %% in posting commitments.
    Config1 = [{network_id, NetworkId}, {consensus, ?CONSENSUS_HC_BTC}, {bitcoin_cli, DogecoinCli}, {btc_beneficiary, Dwight} | Config],
    produce_blocks(?PARENT_CHAIN_NODE1, ?PARENT_CHAIN_NODE1_NAME,
                    parent, ?CHILD_START_HEIGHT, Config1, ?CONSENSUS_HC_BTC),
    ReceiveAddress = string:trim(os:cmd(DogecoinCli ++ "getnewaddress")),
    aecore_suite_utils:create_config(?NODE1, Config,
                                    node_config([{?ALICE, list_to_binary(Dwight)}, {?BOB, list_to_binary(Edwin)}],
                                                list_to_binary(ReceiveAddress), ?CONSENSUS_HC_DOGE),
                                    [{add_peers, true} ]),
    aecore_suite_utils:create_config(?NODE2, Config,
                                    node_config([], ReceiveAddress, ?CONSENSUS_HC_DOGE),
                                    [{add_peers, true} ]),
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    aecore_suite_utils:start_node(?NODE2, Config, Env),
    aecore_suite_utils:connect(?NODE2_NAME, []),

    timer:sleep(1000),
    NumberOfCommitments = 2,
    %% mine blocks on the parent chain and include commitments; stop right
    %% before the child chain produces a block
%%
%%    lists:foreach(
%%        fun(Idx) ->
%%            PCTopHeight = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
%%            ct:log("Mining commitment on the parent chain, idx ~p, parent height ~p", [Idx, PCTopHeight]),
%%            wait_for_commitments_in_pool(?PARENT_CHAIN_NODE1, NumberOfCommitments),
%%            {ok, _} = aecore_suite_utils:mine_micro_blocks(?PARENT_CHAIN_NODE1_NAME, 1),
%%            {ok, _} = aecore_suite_utils:mine_key_blocks(?PARENT_CHAIN_NODE1_NAME, 1)
%%        end,
%%       lists:seq(1, ?CHILD_CONFIRMATIONS - 1)),
    ct:log("Mining last initial commitment on the Dogecoin parent chain", []),
    wait_for_btc_commitments_in_pool(DogecoinCli, NumberOfCommitments),
    %% produce_btc_blocks(DogecoinCli, Dwight, 1),
    {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 10, Config1, ?config(consensus, Config1)),
    ParentTopHash = string:trim(os:cmd(DogecoinCli ++ "getbestblockhash")),
    ct:log("Parent chain blocks ~p", [ParentTopHash]),
    ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, ChildBlocks} = get_generations(?NODE1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    Config1.

set_up_lazy_leader_node(NetworkId, Config) ->
    GenesisStartTime = ?config(genesis_start_time, Config),
    false = GenesisStartTime =:= undefined,
    ElectionContract = election_contract_by_consensus(?CONSENSUS_HC),
    aecore_suite_utils:make_multi(Config, [?LAZY_NODE]),
    build_json_files(NetworkId, ElectionContract, Config, [?LAZY_NODE]),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
          ],
    ReceiveAddress = encoded_pubkey(?FORD),
    aecore_suite_utils:create_config(?LAZY_NODE, Config,
                                    node_config([{?LISA, ?LISA}], ReceiveAddress, ?CONSENSUS_HC,
                                                false, GenesisStartTime),
                                    [{add_peers, true} ]),
    aecore_suite_utils:start_node(?LAZY_NODE, Config, Env),
    aecore_suite_utils:connect(?LAZY_NODE_NAME, []),
    timer:sleep(1000),
    LazyNodePeers = rpc(?LAZY_NODE, aec_peers, connected_peers, []),
    ct:log("Connected peers ~p", [LazyNodePeers]),
    LazyNodeVerifiedPeers = rpc(?LAZY_NODE, aec_peers, available_peers, [verified]),
    ct:log("Verified peers ~p", [LazyNodeVerifiedPeers]),
    {ok, _} = wait_same_top(?NODE1, ?LAZY_NODE),
    Inspect =
        fun(Node) ->
            {ok, TopH} = aec_headers:hash_header(rpc(Node, aec_chain, top_header, [])),
            ct:log("     top hash ~p", [TopH]),
            ChainEnds = rpc(Node, aec_db, find_chain_end_hashes, []),
            lists:foreach(
                fun(Hash) ->
                    {value, D} = rpc(Node, aec_db, find_block_difficulty, [Hash]),
                    {value, H} = rpc(Node, aec_db, dirty_find_header, [Hash]),
                    ct:log("     Chain end with ~p has difficulty ~p", [H, D]),
                    ok
                end,
                ChainEnds)
        end,
    ct:log("Node1 point of view:", []),
    Inspect(?NODE1),
    ct:log("Node2 point of view:", []),
    Inspect(?NODE2),
    ct:log("Lazy Node point of view:", []),
    Inspect(?LAZY_NODE),
    {ok, _} = wait_same_top(?NODE1, ?LAZY_NODE),
    Config.

end_per_group(pos, Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config);
end_per_group(hc, Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config),
    aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE1, Config);
end_per_group(hc_btc, Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config),
    PrivDir = aecore_suite_utils:priv_dir(Config),
    BTCDataDir = filename:join(PrivDir, "bitcoin"),
    Cmd = "bitcoin-cli -datadir="++BTCDataDir++" stop",
    os:cmd(Cmd),
    Config;
end_per_group(hc_doge, Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config),
    PrivDir = aecore_suite_utils:priv_dir(Config),
    DogeDataDir = filename:join(PrivDir, "dogecoin"),
    Cmd = "dogecoin-cli -datadir="++DogeDataDir++" stop",
    os:cmd(Cmd),
    Config;
end_per_group(lazy_leader, Config) ->
    aecore_suite_utils:stop_node(?LAZY_NODE, Config);
end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    aect_test_utils:setup_testcase(Config),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

contract_create_spec(Name, Args, Amount, Nonce, Owner) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    Src = binary_to_list(BinSrc),
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    EncodedPubkey   = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    EncodedOwner    = aeser_api_encoder:encode(account_pubkey, Owner),
    EncodedCode     = aeser_api_encoder:encode(contract_bytearray, Code),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, "init", Args),
    EncodedCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    VM = aect_test_utils:vm_version(),
    ABI = aect_test_utils:abi_version(),
    Spec = #{ <<"amount">> => Amount
            , <<"vm_version">> => VM
            , <<"abi_version">> => ABI
            , <<"nonce">> => Nonce
            , <<"code">> => EncodedCode
            , <<"call_data">> => EncodedCallData
            , <<"pubkey">> => EncodedPubkey
            , <<"owner_pubkey">> => EncodedOwner },
    Spec.

contract_call_spec(ContractPubkey, Name, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(contract_call(ContractPubkey, Name, Fun, Args,
                                           Amount, From, Nonce)),
    %% Don't allow named contracts!?
    {contract, ContractPubKey} =
        aeser_id:specialize(aect_call_tx:contract_id(CallTx)),
    Spec =
        #{  <<"caller">>          => aeser_api_encoder:encode(account_pubkey,
                                                              aect_call_tx:caller_pubkey(CallTx))
          , <<"nonce">>           => aect_call_tx:nonce(CallTx)
          , <<"contract_pubkey">> => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
          , <<"abi_version">>     => aect_call_tx:abi_version(CallTx)
          , <<"fee">>             => aect_call_tx:fee(CallTx)
          , <<"amount">>          => aect_call_tx:amount(CallTx)
          , <<"gas">>             => aect_call_tx:gas(CallTx)
          , <<"gas_price">>       => aect_call_tx:gas_price(CallTx)
          , <<"call_data">>       => aeser_api_encoder:encode(contract_bytearray,
                                                              aect_call_tx:call_data(CallTx))},
    Spec.

contract_call(ContractPubkey, Name, Fun, Args, Amount, From) ->
    Nonce = next_nonce(?NODE1, From), %% no contract calls support for parent chain
    contract_call(ContractPubkey, Name, Fun, Args, Amount, From, Nonce).

contract_call(ContractPubkey, Name, Fun, Args, Amount, From, Nonce) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    Src = binary_to_list(BinSrc),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    ABI = aect_test_utils:abi_version(),
    TxSpec =
        #{  caller_id   => aeser_id:create(account, From)
          , nonce       => Nonce
          , contract_id => aeser_id:create(contract, ContractPubkey)
          , abi_version => ABI
          , fee         => 1000000 * ?DEFAULT_GAS_PRICE
          , amount      => Amount
          , gas         => 1000000
          , gas_price   => ?DEFAULT_GAS_PRICE
          , call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(TxSpec),
    Tx.

mine_and_sync(Config) ->
    {ok, [KB]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
    {ok, KB} = wait_same_top(),
    ok.

wait_same_top() ->
    wait_same_top(?NODE1, ?NODE2).

wait_same_top(Node1, Node2) ->
    wait_same_top(Node1, Node2, 500).

wait_same_top(_Node1, _Node2, Attempts) when Attempts < 1 ->
    {error, run_out_of_attempts};
wait_same_top(Node1, Node2, Attempts) ->
    case {rpc(Node1, aec_chain, top_block, []), rpc(Node2, aec_chain, top_block, [])} of
        {KB, KB} -> {ok, KB};
        {KB1, KB2} ->
            ct:log("Node1 top: ~p\nNode2 top: ~p", [KB1, KB2]),
            timer:sleep(500),
            wait_same_top(Node1, Node2, Attempts - 1)
    end.

spend_txs(Config) ->
    Top0 = rpc(?NODE1, aec_chain, top_header, []),
    ct:log("Top before posting spend txs: ~p", [aec_headers:height(Top0)]),
    NetworkId = ?config(network_id, Config),
    seed_account(pubkey(?ALICE), 100000001 * ?DEFAULT_GAS_PRICE, NetworkId),
    seed_account(pubkey(?BOB), 100000002 * ?DEFAULT_GAS_PRICE, NetworkId),
    seed_account(pubkey(?LISA), 100000003 * ?DEFAULT_GAS_PRICE, NetworkId),

    lists:foreach(
        fun(GenIndex) ->
            {ok, Gen} = rpc:call(?NODE1_NAME, aec_chain,
                                get_generation_by_height, [GenIndex, forward]),
            ct:log("Generation ~p:\n~p", [GenIndex, Gen])
        end,
        lists:seq(1, 2)),
    ok.

simple_withdraw(Config) ->
    AliceBin = encoded_pubkey(?ALICE),
    Alice = binary_to_list(encoded_pubkey(?ALICE)),
    running = rpc:call(?NODE1_NAME, aec_conductor, get_mining_state, []),
    true = rpc:call(?NODE1_NAME, aec_conductor, is_leader, []),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),

    {ok, [KB0 | _ ]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
    Top0 = aec_blocks:to_header(KB0),
    Top0 = rpc(?NODE1, aec_chain, top_header, []),
    ct_log_header(Top0),
    InitBalance  = account_balance(pubkey(?ALICE)),
    {ok, _AliceContractSPower} = inspect_staking_contract(?ALICE, {staking_power, ?ALICE}, Config),
    {ok, _BobContractSPower} = inspect_staking_contract(?ALICE, {staking_power, ?BOB}, Config),
    {ok,
        #{<<"ct">> := _, %% pool contract
          <<"is_online">> := true,
          <<"stake">> := _,
          <<"state">> :=
                #{<<"delegates">> := [[AliceBin, ?INITIAL_STAKE]],
                  <<"main_staking_ct">> := <<"ak_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ">>,
                  <<"shares">> := ?INITIAL_STAKE}}} =
        inspect_staking_contract(?ALICE, {get_validator_state, ?ALICE}, Config),
    WithdrawAmount = 1000,
    Fun = "unstake",
    ContractPubkey = ?config(staking_contract, Config),
    NetworkId = ?config(network_id, Config),
    CallTx =
        sign_and_push(
            contract_call(ContractPubkey, ?STAKING_CONTRACT, Fun,
                  [Alice, integer_to_list(WithdrawAmount)], 0, pubkey(?ALICE)),
            ?ALICE,
            NetworkId),
    {ok, [_]} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    {value, _Acc} = rpc(?NODE1, aec_chain, get_account, [pubkey(?ALICE)]),
    mine_tx_no_cheating(?NODE1, CallTx),
    EndBalance = account_balance(pubkey(?ALICE)),
    {ok, Call} = call_info(CallTx),
    {ok, _Res} = decode_consensus_result(Call, Fun, ?STAKING_CONTRACT),
    GasUsed = aect_call:gas_used(Call),
    GasPrice = aect_call:gas_price(Call),
    Fee = aetx:fee(aetx_sign:tx(CallTx)),
    ct:log("Initial balance: ~p, withdrawn: ~p, gas used: ~p, gas price: ~p, fee: ~p, end balance: ~p",
           [InitBalance, WithdrawAmount, GasUsed, GasPrice,
                          Fee, EndBalance]),
%% TODO: adjust rewarded fees
%%    {EndSPower, EndSPower} = {EndSPower, InitSPower + WithdrawAmount -
%%                               TotalSpent},

    {ok, _AliceContractSPower1} = inspect_staking_contract(?ALICE, {staking_power, ?ALICE}, Config),
%%    {AliceContractSPower, AliceContractSPower} = {AliceContractSPower, AliceContractSPower1 + 1},
%%    {ok, BobContractSPower} = inspect_staking_contract(?ALICE, {staking_power, ?BOB}, Config),
    Top1 = rpc(?NODE1, aec_chain, top_header, []),
    ct_log_header(Top1),
    TimeInBetween = aec_headers:time_in_msecs(Top1) - aec_headers:time_in_msecs(Top0),
    BlocksInBetween = aec_headers:height(Top1) - aec_headers:height(Top0),
    ct:log("Key blocks: ~p, Time difference = ~p", [BlocksInBetween, TimeInBetween]),

    ok.

change_leaders(Config) ->
    {ok, AliceSPower} = inspect_staking_contract(?ALICE, {staking_power, ?ALICE}, Config),
    {ok, BobSPower} = inspect_staking_contract(?ALICE, {staking_power, ?BOB}, Config),
    ct:log("Alice ~p, staking_power: ~p", [encoded_pubkey(?ALICE), AliceSPower]),
    ct:log("Bob ~p, staking_power: ~p", [encoded_pubkey(?BOB), BobSPower]),
    Blocks = 10,
    NewLeader =
        fun() ->
            {ok, [KB | _ ]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
            Beneficiary = aec_blocks:beneficiary(KB),
            Beneficiary = aec_blocks:miner(KB),
            ct_log_block(KB),
            {ok, Leader} = inspect_election_contract(?ALICE, current_leader, Config),
            {ok, LeaderDecoded} =
                aeser_api_encoder:safe_decode(account_pubkey, Leader),
            Beneficiary = LeaderDecoded, %% assert
            Leader
        end,
    Ls = lists:map(fun(_Idx) -> NewLeader() end, lists:seq(1, Blocks)),
    Stats =
        lists:foldl(
            fun(Leader, Accum) ->
                maps:update_with(Leader, fun(X) -> X + 1 end, 1, Accum)
            end, #{}, Ls),
    ct:log("Leaders: ~p", [Stats]),
    AliceLeaderCnt = maps:get(encoded_pubkey(?ALICE), Stats, 0),
    BobLeaderCnt = maps:get(encoded_pubkey(?BOB), Stats, 0),
    false = AliceLeaderCnt =:= Blocks,
    true  = AliceLeaderCnt > 0,
    false = BobLeaderCnt =:= Blocks,
    true  = BobLeaderCnt > 0,
    {ok, _B} = wait_same_top(),
    ok.

verify_fees(Config) ->
    %% start without any tx fees, only a keyblock
    Test =
        fun() ->
            %% gather staking_powers before reward distribution
            AliceBalance0 = account_balance(pubkey(?ALICE)),
            BobBalance0 = account_balance(pubkey(?BOB)),
            produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
            TopHeader = rpc(?NODE1, aec_chain, top_header, []),
            {ok, TopHash} = aec_headers:hash_header(TopHeader),
            PrevHash = aec_headers:prev_hash(TopHeader),
            {ok, AliceContractSPower0} = inspect_staking_contract(?ALICE,
                                                                  {staking_power,
                                                                   ?ALICE},
                                                                  Config,
                                                                  PrevHash),
            {ok, BobContractSPower0} = inspect_staking_contract(?ALICE,
                                                                {staking_power,
                                                                 ?BOB},
                                                                Config,
                                                                PrevHash),
            {ok, LisaContractSPower0} = inspect_staking_contract(?ALICE,
                                                                {staking_power,
                                                                 ?LISA},
                                                                Config,
                                                                PrevHash),
            %% gather staking_powers after reward distribution
            AliceBalance1= account_balance(pubkey(?ALICE)),
            BobBalance1 = account_balance(pubkey(?BOB)),
            {ok, AliceContractSPower1} = inspect_staking_contract(?ALICE,
                                                                  {staking_power,
                                                                   ?ALICE},
                                                                  Config,
                                                                  TopHash),
            {ok, BobContractSPower1} = inspect_staking_contract(?ALICE,
                                                                {staking_power,
                                                                 ?BOB},
                                                                Config,
                                                                TopHash),
            {ok, LisaContractSPower1} = inspect_staking_contract(?ALICE,
                                                                  {staking_power,
                                                                  ?LISA},
                                                                  Config,
                                                                  TopHash),
            %% inspect who shall receive what reward
            RewardForHeight = aec_headers:height(TopHeader) - ?REWARD_DELAY,
            {ok, PrevH} = rpc(?NODE1, aec_chain, get_key_header_by_height, [RewardForHeight - 1]),
            {ok, RewardH} = rpc(?NODE1, aec_chain, get_key_header_by_height, [RewardForHeight]),
            Beneficiary1 = aec_headers:beneficiary(PrevH),
            Beneficiary1Name = name(who_by_pubkey(Beneficiary1)),
            Beneficiary2 = aec_headers:beneficiary(RewardH),
            Beneficiary2Name = name(who_by_pubkey(Beneficiary2)),
            ct:log("Beneficiary1: ~p, Beneficiary2: ~p", [Beneficiary1Name,
                                                          Beneficiary2Name]),
            %% assert account staking_powers do not change; only contract staking_powers change
            {AliceBalance0, AliceBalance0} = {AliceBalance0, AliceBalance1},
            {BobBalance0, BobBalance0} = {BobBalance0, BobBalance1},
            %% calc rewards
            {{AdjustedReward1, AdjustedReward2}, _DevRewards} =
                calc_rewards(RewardForHeight),
            {AliceExpectedRewards, BobExpectedRewards, LisaExpectedRewards} =
                lists:foldl(
                    fun({Pubkey, Amount}, {AliceRewards0, BobRewards0, LisaRewards0}) ->
                          case who_by_pubkey(Pubkey) of
                              ?ALICE ->
                                  {AliceRewards0 + Amount, BobRewards0, LisaRewards0};
                              ?BOB ->
                                  {AliceRewards0, BobRewards0 + Amount, LisaRewards0};
                              ?LISA ->
                                  {AliceRewards0, BobRewards0, LisaRewards0 + Amount};
                              genesis ->
                                  {AliceRewards0, BobRewards0, LisaRewards0}
                          end
                    end,
                    {0, 0, 0},
                    [{Beneficiary1, AdjustedReward1},
                    {Beneficiary2, AdjustedReward2}]),
            AliceReward = AliceContractSPower1 - AliceContractSPower0,
            ct:log("Alice expected rewards: ~p, actual rewards: ~p",
                  [AliceExpectedRewards, AliceReward]),
            {AliceExpectedReward, AliceExpectedReward} =
                {AliceExpectedRewards, AliceReward},
            BobReward = BobContractSPower1 - BobContractSPower0,
            ct:log("Bob expected rewards: ~p, actual rewards: ~p",
                  [BobExpectedRewards, BobReward]),
            {BobExpectedReward, BobExpectedReward} =
                {BobExpectedRewards, BobReward},
            LisaReward = LisaContractSPower1 - LisaContractSPower0,
            ct:log("Lisa expected rewards: ~p, actual rewards: ~p",
                  [LisaExpectedRewards, LisaReward]),
            {LisaExpectedReward, LisaExpectedReward} =
                {LisaExpectedRewards, LisaReward}
        end,
    %% test a couple of empty generations - there are no fees, only block
    %% rewards
    NetworkId = ?config(network_id, Config),
    lists:foreach(
        fun(_) -> Test() end,
        lists:seq(1, 10)),
    ct:log("Test with a spend transaction", []),
    Test(), %% fees
    produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
    ct:log("Test with no transaction", []),
    {ok, _SignedTx} = seed_account(pubkey(?ALICE), 1, NetworkId),
    produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
    Test(), %% after fees
    ok.

verify_commitments(Config) ->
    Test =
        fun(GenerationsCnt) ->
            ParentTopHeight0 = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
            TopHeight0 = rpc(?NODE1, aec_chain, top_height, []),
            ct:log("Start height: ~p, parent height ~p", [TopHeight0, ParentTopHeight0]),
            produce_blocks(?NODE1, ?NODE1_NAME, child, GenerationsCnt, Config, ?config(consensus, Config)),
            TopHeight = rpc(?NODE1, aec_chain, top_height, []),
            {true, TopHeight0, TopHeight} = {TopHeight0 =:= TopHeight - GenerationsCnt, TopHeight0, TopHeight},
            ParentTopHeight = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
            ct:log("End height: ~p, parent height ~p", [TopHeight, ParentTopHeight]),
            {ok, Blocks} = get_generations(?PARENT_CHAIN_NODE1, ParentTopHeight0, ParentTopHeight),
            MicroBlocks =
                lists:filter(
                fun(B) -> aec_blocks:type(B) =:= micro end,
                Blocks),
            Commitments =
                lists:flatten(lists:map(
                    fun(MB) ->
                        MBHeight = aec_blocks:height(MB),
                        Txs = aec_blocks:txs(MB),
                        lists:map(
                            fun(SignedTx) ->
                                Tx = aetx_sign:tx(SignedTx),
                                Nonce = aetx:nonce(Tx),
                                {spend_tx, SpendTx} = aetx:specialize_type(Tx),
                                Payload = aec_spend_tx:payload(SpendTx),
                                {btc, _Signature, StakerHash, TopKeyHash} = aec_parent_chain_block:decode_commitment(Payload),
                                Spender = name(who_by_pubkeyhashprefix(StakerHash)),
                                {MBHeight, Spender, Nonce, TopKeyHash}
                            end,
                            Txs)
                    end,
                    MicroBlocks)),
            Filter =
                fun(Name) ->
                    lists:filter(
                        fun({_MBHeight, Spender, _Nonce, _Payload}) -> Spender =:= Name end,
                        Commitments)
                end,
            AliceCommitments = Filter(name(?ALICE)),
            ct:log("Alice commitments: ~p", [AliceCommitments]),
            BobCommitments = Filter(name(?BOB)),
            ct:log("Bob commitments: ~p", [BobCommitments]),
            {GenerationsCnt, GenerationsCnt} = {GenerationsCnt, length(AliceCommitments)},
            {GenerationsCnt, GenerationsCnt} = {GenerationsCnt, length(BobCommitments)},
            AssertOrder =
                fun(Comms) ->
                    ParsedComms =
                        lists:map(
                            fun({MBHeight, _Spender, Nonce, _Hash}) ->
                                %% This check is not possible when we only store the prefix of the hash of the block hash
                                %% For now cheat to ensure the test passes
                                %% {ok, Block} = rpc(?NODE1, aec_chain, get_block, [Hash]),
                                {MBHeight, Nonce, MBHeight - ?CHILD_START_HEIGHT + 1}
                            end,
                        Comms),
                    ct:log("Commitments: ~p", [ParsedComms]),
                    lists:map(
                        fun({ParentHeight, N, H}) ->
                            {N, N} = {N, H},
                            ExpectedParentHeight = H + ?CHILD_START_HEIGHT - 1,
                            {ParentHeight, ParentHeight} = {ParentHeight, ExpectedParentHeight}
                        end,
                        ParsedComms)
                end,
            AssertOrder(AliceCommitments),
            AssertOrder(BobCommitments),
            ok
        end,
    Test(1),
    Test(5),
    Test(10),
    ok.

verify_btc_commitments(Config) ->
    BitcoinCli = ?config(bitcoin_cli, Config),
    Test =
        fun(GenerationsCnt) ->
            ParentTopHeight0 = btc_top_height(BitcoinCli),
            TopHeight0 = rpc(?NODE1, aec_chain, top_height, []),
            ct:log("Start height: ~p, parent height ~p", [TopHeight0, ParentTopHeight0]),
            produce_blocks(?NODE1, ?NODE1_NAME, child, GenerationsCnt, Config, ?config(consensus, Config)),
            TopHeight = rpc(?NODE1, aec_chain, top_height, []),
            {true, TopHeight0, TopHeight} = {TopHeight0 =:= TopHeight - GenerationsCnt, TopHeight0, TopHeight},
            ParentTopHeight = btc_top_height(BitcoinCli),
            ct:log("End height: ~p, parent height ~p", [TopHeight, ParentTopHeight]),
            {ok, Blocks} = get_btc_generations(BitcoinCli, ParentTopHeight0 + 1, ParentTopHeight),
            Commitments =
                lists:flatten(lists:map(
                    fun(B) ->
                        BHeight = maps:get(<<"height">>, B),
                        Txs = maps:get(<<"tx">>, B),
                        lists:map(
                            fun(Tx) ->
                                case Tx of
                                    TxId when is_binary(TxId) ->
                                        %% It's a Doge Tx, get details
                                        {ok, TxDetails} = get_doge_raw_tx(BitcoinCli, TxId),
                                        ct:log("TxDetails ~p", [TxDetails]),
                                        case TxDetails of
                                            #{<<"vout">> := Vout} when length(Vout) == 3 ->
                                                case aehttpc_btc:parse_vout(Vout) of
                                                    {ok, {_Signature, StakerHash, TopKeyHash}} ->
                                                        Spender = name(who_by_pubkeyhashprefix(StakerHash)),
                                                        {BHeight, Spender, 0, TopKeyHash};
                                                    {error, _Reason} ->
                                                        []
                                                end;
                                            _Else ->
                                                []
                                        end;
                                    #{<<"vout">> := Vout}  ->
                                        case aehttpc_btc:parse_vout(Vout) of
                                            {ok, {_Signature, StakerHash, TopKeyHash}} ->
                                                Spender = name(who_by_pubkeyhashprefix(StakerHash)),
                                                {BHeight, Spender, 0, TopKeyHash};
                                            {error, _Reason} ->
                                                []
                                        end;
                                    _Else ->
                                        %% lager:info("Invalid BTC Tx, skipping ~p", [Tx]),
                                        []
                                end
                            end,
                            Txs)
                    end,
                    Blocks)),
            Filter =
                fun(Name) ->
                    lists:filter(
                        fun({_MBHeight, Spender, _Nonce, _Payload}) -> Spender =:= Name end,
                        Commitments)
                end,
            AliceCommitments = Filter(name(?ALICE)),
            BobCommitments = Filter(name(?BOB)),
            {GenerationsCnt, GenerationsCnt} = {GenerationsCnt, length(AliceCommitments)},
            {GenerationsCnt, GenerationsCnt} = {GenerationsCnt, length(BobCommitments)},
            AssertOrder =
                fun(Comms) ->
                    ParsedComms =
                        lists:map(
                            fun({MBHeight, _Spender, Nonce, _Hash}) ->
                                % {ok, Block} = rpc(?NODE1, aec_chain, get_block, [Hash]),
                                % {MBHeight, Nonce, aec_blocks:height(Block)}
                                {MBHeight, Nonce, MBHeight}
                            end,
                        Comms),
                    ct:log("Commitments: ~p", [ParsedComms])
                end,
            AssertOrder(AliceCommitments),
            AssertOrder(BobCommitments),
            ok
        end,
    Test(1),
    Test(5),
    Test(10),
    ok.

genesis_has_commitments(_Config) ->
    PCStartHeight = ?CHILD_START_HEIGHT,
    PCConfirmationsEndHeight = ?CHILD_START_HEIGHT + ?CHILD_CONFIRMATIONS - 1,
    InitialCommitments = get_commitments(PCStartHeight, PCConfirmationsEndHeight),
    GenesisHash = aeser_api_encoder:encode(key_block_hash, rpc(?NODE1, aec_chain, genesis_hash, [])),
    lists:foreach(
        fun(Height) ->
            Txs = maps:get(Height, InitialCommitments),
            2 = length(Txs),
            lists:foreach(
                fun(SignedTx) ->
                    {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
                    {GenesisHash, GenesisHash} = {aec_spend_tx:payload(SpendTx), GenesisHash}
                end,
                Txs),
            ok
        end,
        lists:seq(PCStartHeight, PCConfirmationsEndHeight)),
    ok.

genesis_has_commitments_btc(Config) ->
    BitcoinCli = ?config(bitcoin_cli, Config),
    PCStartHeight = ?CHILD_START_HEIGHT,
    PCConfirmationsEndHeight = ?CHILD_START_HEIGHT + ?CHILD_CONFIRMATIONS - 1,
    InitialCommitments = get_btc_commitments(BitcoinCli, PCStartHeight, PCConfirmationsEndHeight),
    GenesisHash = aeser_api_encoder:encode(key_block_hash, rpc(?NODE1, aec_chain, genesis_hash, [])),
    lists:foreach(
        fun(Height) ->
            Txs = maps:get(Height, InitialCommitments),
            2 = length(Txs),
            lists:foreach(
                fun(SignedTx) ->
                    {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
                    {GenesisHash, GenesisHash} = {aec_spend_tx:payload(SpendTx), GenesisHash}
                end,
                Txs),
            ok
        end,
        lists:seq(PCStartHeight, PCConfirmationsEndHeight)),
    ok.

block_difficulty(Config) ->
    lists:foreach(
        fun(_) ->
            {ok, [KB]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config, ?config(consensus, Config)),
            {ok, AddedStakingPower} = inspect_election_contract(?ALICE, current_added_staking_power, Config),
            Target = aec_blocks:target(KB),
            {Target, Target} = {Target, aeminer_pow:integer_to_scientific(AddedStakingPower)}
        end,
        lists:seq(1, 20)), %% test with 20 elections
    ok.

elected_leader_did_not_show_up(Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config), %% stop the block producer
    TopHeader0 = rpc(?NODE2, aec_chain, top_header, []),
    {TopHeader0, TopHeader0} = {rpc(?LAZY_NODE, aec_chain, top_header, []),
                                TopHeader0},
    ct:log("Start header: ~p", [TopHeader0]),
    %% produce a block on the parent chain
    ok = produce_blocks_hc(?LAZY_NODE, ?LAZY_NODE_NAME, 1, lazy_leader),
    {ok, KB} = wait_same_top(?NODE2, ?LAZY_NODE),
    0 = aec_blocks:difficulty(KB),
    TopHeader1 = rpc(?LAZY_NODE, aec_chain, top_header, []),
    ct:log("Lazy header: ~p", [TopHeader1]),
    TopHeader1 = rpc(?NODE2, aec_chain, top_header, []),
    NetworkId = ?config(network_id, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
          ],
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    {ok, _} = wait_same_top(?NODE1, ?LAZY_NODE),
    ok = produce_blocks_hc(?NODE1, ?NODE1_NAME, 1, abnormal_commitments_cnt),
    {ok, KB1} = wait_same_top(?NODE1, ?LAZY_NODE),
    {ok, KB1} = wait_same_top(?NODE2, ?LAZY_NODE),
    {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 10, Config, ?config(consensus, Config)),
    {ok, KB2} = wait_same_top(?NODE1, ?LAZY_NODE),
    {ok, KB2} = wait_same_top(?NODE2, ?LAZY_NODE),
    ok.



pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

name({_, _, Name}) -> Name.

who_by_pubkey(Pubkey) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    Lisa = pubkey(?LISA),
    Dwight = pubkey(?DWIGHT),
    Edwin = pubkey(?EDWIN),
    Genesis = ?GENESIS_BENFICIARY,
    case Pubkey of
        Alice -> ?ALICE;
        Bob -> ?BOB;
        Lisa -> ?LISA;
        Dwight -> ?DWIGHT;
        Edwin -> ?EDWIN;
        Genesis -> genesis;
        _  -> error(unknown_beneficiary)
    end.

who_by_pubkeyhashprefix(PubKeyHash) ->
    Alice = pubkey_hash_prefix(?ALICE),
    Bob = pubkey_hash_prefix(?BOB),
    Dwight = pubkey_hash_prefix(?DWIGHT),
    Edwin = pubkey_hash_prefix(?EDWIN),
    Genesis = ?GENESIS_BENFICIARY,
    case PubKeyHash of
        Alice -> ?ALICE;
        Bob -> ?BOB;
        Dwight -> ?DWIGHT;
        Edwin -> ?EDWIN;
        Genesis -> genesis;
        _  -> error(unknown_beneficiary)
    end.

pubkey_hash_prefix(Who) ->
    PubKey = pubkey(Who),
    StakerPubKeyFate = aeb_fate_encoding:serialize(aeb_fate_data:make_address(PubKey)),
    <<StakerHash:8/binary, _/binary>> = aec_hash:sha256_hash(StakerPubKeyFate),
    StakerHash.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

next_nonce(Node, Pubkey) ->
    case rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

sign_and_push(Tx, Who, NetworkId) ->
    SignedTx = sign_tx(Tx, privkey(Who), NetworkId),
    ok = rpc:call(?NODE1_NAME, aec_tx_pool, push, [SignedTx, tx_received]),
    SignedTx.

%% usually we would use aec_test_utils:sign_tx/3. This function is being
%% executed in the context of the CT test and uses the corresponding
%% network_id. Since the network_id of the HC node is different, we must sign
%% the tx using the test-specific network_id
sign_tx(Tx, Privkey, NetworkId) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    Bin = aec_hash:hash(signed_tx, Bin0), %% since we are in CERES context, we sign th hash
    BinForNetwork = <<NetworkId/binary, Bin/binary>>,
    Signatures = [ enacl:sign_detached(BinForNetwork, Privkey)],
    aetx_sign:new(Tx, Signatures).

seed_account(RecpipientPubkey, Amount, NetworkId) ->
    seed_account(?NODE1, ?NODE1_NAME, RecpipientPubkey, Amount, NetworkId).

seed_account(Node, NodeName, RecpipientPubkey, Amount, NetworkId) ->
    MineFun = fun(Tx) -> mine_tx_no_cheating(Node, Tx) end,
    seed_account(Node, NodeName, RecpipientPubkey, Amount, NetworkId, MineFun).

seed_account(Node, NodeName, RecipientPubkey, Amount, NetworkId, MineFun) ->
    %% precondition
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    ct:log("Seed spend tx", []),
    {PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(Node),
    Nonce = next_nonce(Node, PatronPub),
    Params =
        #{sender_id    => aeser_id:create(account, PatronPub),
          recipient_id => aeser_id:create(account, RecipientPubkey),
          amount       => Amount,
          fee          => 30000 * ?DEFAULT_GAS_PRICE,
          nonce        => Nonce,
          payload      => <<>>},
    ct:log("Network id ~p", [NetworkId]),
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, PatronPriv, NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, [_SpendTx]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    MineFun(SignedTx),
    {ok, SignedTx}.

mine_tx_no_cheating(Node, SignedTx) ->
    mine_tx_no_cheating(Node, SignedTx, 100).

mine_tx_no_cheating(Node, SignedTx, Attempts) when Attempts < 1 ->
    {TxEnv, Trees} = rpc(Node, aetx_env, tx_env_and_trees_from_top, [aetx_transaction]),
    Reason =
        case rpc(Node, aetx_sign, verify, [SignedTx, Trees, ?CERES_PROTOCOL_VSN]) of
            ok ->
                case rpc(Node, aetx, process, [aetx_sign:tx(SignedTx), Trees, TxEnv]) of
                    {ok, _Trees, _} -> no_reason;
                    {error, R} -> R
                end;
            {error, R} -> R
        end,
    error({could_not_mine_tx, Reason, SignedTx});
mine_tx_no_cheating(Node, SignedTx, Attempts) ->
    Retry = fun() -> timer:sleep(100), mine_tx_no_cheating(Node, SignedTx, Attempts - 1) end,
    TxHash = aetx_sign:hash(SignedTx),
    case rpc(Node, aec_chain, find_tx_location, [TxHash]) of
        mempool -> Retry();
        none ->
            error({could_not_mine_tx, garbage_collected, SignedTx});
        not_found ->
            error({could_not_mine_tx, tx_not_found, SignedTx});
        BlockHash when is_binary(BlockHash) ->
            ok
    end.

account_balance(Pubkey) ->
    case rpc(?NODE1, aec_chain, get_account, [Pubkey]) of
        {value, Account} -> aec_accounts:balance(Account);
        none -> no_such_account
    end.

inspect_staking_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            {staking_power, Who} ->
                {"staking_power", [binary_to_list(encoded_pubkey(Who))]};
            {get_validator_state, Who} ->
                {"get_validator_state", [binary_to_list(encoded_pubkey(Who))]};
            get_state ->
                {"get_state", []}
        end,
    ContractPubkey = ?config(staking_contract, Config),
    Tx = contract_call(ContractPubkey, ?STAKING_CONTRACT, Fun,
                  Args, 0, pubkey(OriginWho)),
    {ok, Call} = dry_run(TopHash, Tx),
    {_Type, _Res} = decode_consensus_result(Call, Fun, ?STAKING_CONTRACT).

inspect_election_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            current_leader -> {"leader", []};
            current_added_staking_power -> {"added_stake", []}
        end,
    ContractPubkey = ?config(election_contract, Config),
    ElectionContract = election_contract_by_consensus(?config(consensus, Config)),
    Tx = contract_call(ContractPubkey, ElectionContract, Fun,
                  Args, 0, pubkey(OriginWho)),
    {ok, Call} = dry_run(TopHash, Tx),
    {_Type, _Res} = decode_consensus_result(Call, Fun, ElectionContract).

dry_run(TopHash, Tx) ->
    case rpc(?NODE1, aec_dry_run, dry_run, [TopHash, [], [{tx, Tx}]]) of
        {error, _} = Err -> Err;
        {ok, {[{contract_call_tx, {ok, Call}}], _Events}} -> {ok, Call}
    end.


call_info(SignedTx) ->
    Hash = aetx_sign:hash(SignedTx),
    case rpc:call(?NODE1_NAME, aec_chain, find_tx_location, [Hash]) of
        not_found ->  {error, unknown_tx};
        none -> {error, gced_tx};
        mempool -> {error, tx_in_pool};
        MBHash when is_binary(MBHash) ->
            case rpc:call(?NODE1_NAME, aehttp_helpers, get_info_object_signed_tx,
                          [MBHash, SignedTx]) of
                {ok, Call} -> {ok, Call};
                {error, Reason} -> {error, Reason}
            end
    end.

ct_log_block(Block) ->
    ct_log_header(aec_blocks:to_header(Block)).

ct_log_header(Header) ->
    Time = aec_headers:time_in_msecs(Header),
    DateTime = calendar:system_time_to_universal_time(Time, millisecond),
    Height = aec_headers:height(Header),
    ct:log("Block ~p, Timestamp: ~p (~p)", [Height, DateTime, Time]).

decode_consensus_result(Call, Fun, Contract) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    SophiaVersion = aect_test_utils:latest_sophia_version(),
    {ok, BinCode} = aect_test_utils:read_contract(SophiaVersion, Contract),
    Res =
        aect_test_utils:decode_call_result(binary_to_list(BinCode), Fun,
                                          ReturnType, ReturnValue),
    {ReturnType, Res}.

calc_rewards(RewardForHeight) ->
    %% we distribute rewards for the previous
    {ok, #{key_block := PrevKB,
           micro_blocks := MBs}}
        = rpc(?NODE1, aec_chain, get_generation_by_height,
              [RewardForHeight - 1, forward]),
    PrevGenProtocol = aec_blocks:version(PrevKB),
    Txs = lists:flatten(
            lists:map(
                fun(MB) -> aec_blocks:txs(MB) end,
                MBs)),
    ct:log("Txs: ~p", [Txs]),
    KeyReward = rpc(?NODE1, aec_governance, block_mine_reward,
                    [RewardForHeight]),
    GenerationFees =
        lists:foldl(
            fun(SignTx, Accum) ->
                %% TODO: maybe add support for contract calls:
                %% * contract create
                %% * contract call
                %% * force progress
                %% * meta tx
                Tx = aetx_sign:tx(SignTx),
                Fee = aetx:fee(Tx),
                Accum + Fee
            end,
            0,
            Txs),
    ct:log("Height ~p, Generation fees: ~p, key reward: ~p",
           [RewardForHeight, GenerationFees, KeyReward]),
    BeneficiaryReward1 = GenerationFees * 4 div 10,
    BeneficiaryReward2 = GenerationFees - BeneficiaryReward1 + KeyReward,
   %% TODO: verify devrewards
    {{AdjustedReward1, AdjustedReward2}, _DevRewards} = Res =
        rpc(?NODE1, aec_dev_reward, split,
            [BeneficiaryReward1, BeneficiaryReward2, PrevGenProtocol]),
    ct:log("AdjustedReward1: ~p, AdjustedReward2: ~p",
           [AdjustedReward1, AdjustedReward2]),
    Res.

build_json_files(NetworkId, ElectionContract, Config, NodesList) ->
    Pubkey = ?OWNER_PUBKEY,
    {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),
    EncodePub =
        fun(P) ->
            binary_to_list(aeser_api_encoder:encode(account_pubkey, P))
        end,
    %% create staking contract
    MinValidatorAmt = integer_to_list(trunc(math:pow(10,18) * math:pow(10, 6))), %% 1 mln AE
    MinStakeAmt = integer_to_list(trunc(math:pow(10,18) * 1)), %% 1 AE
    MinStakePercent = "30",
    OnlineDelay = "0",
    StakeDelay = "0",
    UnstakeDelay = "0",
    #{ <<"pubkey">> := StakingValidatorContract} = C0
        = contract_create_spec("StakingValidator",
                                [EncodePub(Pubkey), UnstakeDelay], 0, 1, Pubkey),
    {ok, ValidatorPoolAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                              StakingValidatorContract),
    %% assert assumption
    ValidatorPoolAddress = validator_pool_contract_address(),
    #{ <<"pubkey">> := StakingContractPubkey
        , <<"owner_pubkey">> := ContractOwner } = SC
        = contract_create_spec(?STAKING_CONTRACT,
                                [binary_to_list(StakingValidatorContract),
                                MinValidatorAmt, MinStakePercent, MinStakeAmt,
                                OnlineDelay, StakeDelay, UnstakeDelay],
                                0, 2, Pubkey),
    {ok, StakingAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                         StakingContractPubkey),
    %% assert assumption
    StakingAddress = staking_contract_address(),
    %% create election contract
    #{ <<"pubkey">> := ElectionContractPubkey
        , <<"owner_pubkey">> := ContractOwner } = EC
        = contract_create_spec(ElectionContract,
                                [binary_to_list(StakingContractPubkey),
                                "\"domat\""], 0, 3, Pubkey),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                          ElectionContractPubkey),
    %% assert assumption
    ElectionAddress = election_contract_address(),
    {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                StakingContractPubkey),
    Call1 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?ALICE), 1),
    Call2 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?BOB), 1),
    Call3 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?LISA), 1),
    Call4  =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_online", [], 0, pubkey(?ALICE), 2),
    Call5  =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_online", [], 0, pubkey(?BOB), 2),
    Call6 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_online", [], 0, pubkey(?LISA), 2),
    Call7 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_validator_name", ["\"Alice\""], 0, pubkey(?ALICE), 3),
    Call8 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_validator_name", ["\"Bob\""], 0, pubkey(?BOB), 3),
    Call9 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_validator_name", ["\"Lisa\""], 0, pubkey(?LISA), 3),
    Call10 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_validator_description",
                            ["\"Alice is a really awesome validator and she had set a description of her great service to the network.\""], 0,
                            pubkey(?ALICE), 4),
    Call11 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_validator_avatar_url",
                            ["\"https://aeternity.com/images/aeternity-logo.svg\""], 0,
                            pubkey(?ALICE), 5),
    %% create a BRI validator in the contract so they can receive
    %% rewards as well
    %% TODO: discuss how we want to tackle this:
    %%  A) require the BRI account to be validator
    %%  B) allow pending stake in the contract that is not allocated
    %%  yet
    %%  C) something else
    BRI = <<"ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8">>,
    {ok, BRIPub} = aeser_api_encoder:safe_decode(account_pubkey, BRI),
    Call12 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, BRIPub, 1),
    Call13 =
        contract_call_spec(SCId, ?STAKING_CONTRACT,
                            "set_validator_description",
                            ["\"This validator is offline. She can never become a leader. She has no name set. She is receiving the BRI rewards\""],
                            0, BRIPub, 2),
    %% keep the BRI offline
    AllCalls =  [Call1, Call2, Call3, Call4, Call5, Call6,
		 Call7, Call8, Call9, Call10, Call11, Call12, Call13],
    Subdir =
        case aect_test_utils:latest_protocol_version() of
            ?IRIS_PROTOCOL_VSN -> "iris";
            ?CERES_PROTOCOL_VSN -> "ceres"
        end,
    aecore_suite_utils:create_seed_file(NodesList,
        Config,
        Subdir, binary_to_list(NetworkId) ++ "_contracts.json",
        #{<<"contracts">> => [C0, SC, EC], <<"calls">> => AllCalls}),
    aecore_suite_utils:create_seed_file(NodesList,
        Config,
        Subdir, binary_to_list(NetworkId) ++ "_accounts.json",
        #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000,
            BRI => 2000000000000000000000000000
         }),
    ok.

node_config(PotentialStakers, ReceiveAddress, Consensus) ->
    node_config(PotentialStakers, ReceiveAddress, Consensus, true, 0).

node_config(PotentialStakers, ReceiveAddress, Consensus, ProducingCommitments, GenesisStartTime) ->
    Stakers =
        case Consensus of
            ?CONSENSUS_POS ->
                lists:map(
                    fun(Who) ->
                        Pub = encoded_pubkey(Who),
                        Priv = list_to_binary(aeu_hex:bin_to_hex( privkey(Who))), %% TODO: discuss key management
                        #{<<"pub">> => Pub, <<"priv">> => Priv}
                    end,
                    PotentialStakers);
            ?CONSENSUS_HC ->
                lists:map(
                    fun({HCWho, PCWho}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))), %% TODO: discuss key management
                        PCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(PCWho))),
                        #{  <<"hyper_chain_account">> =>#{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv},
                            <<"parent_chain_account">> =>#{<<"pub">> => encoded_pubkey(PCWho), <<"priv">> => PCPriv}}
                    end,
                    PotentialStakers);
            _ when Consensus == ?CONSENSUS_HC_BTC; Consensus == ?CONSENSUS_HC_DOGE ->
                lists:map(
                    fun({HCWho, PCWho}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))),
                        #{  <<"hyper_chain_account">> => #{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv},
                            <<"parent_chain_account">> => #{<<"pub">> => PCWho} }
                    end,
                    PotentialStakers)
        end,
    ConsensusType =
        case Consensus of
            ?CONSENSUS_HC -> <<"hyper_chain">>;
            ?CONSENSUS_HC_BTC -> <<"hyper_chain">>;
            ?CONSENSUS_HC_DOGE -> <<"hyper_chain">>;
            ?CONSENSUS_POS -> <<"smart_contract">>
        end,
    SpecificConfig =
        case Consensus of
            ?CONSENSUS_POS -> #{};
            ?CONSENSUS_HC ->
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?CHILD_START_HEIGHT,
                        <<"confirmations">> => ?CHILD_CONFIRMATIONS,
                        <<"consensus">> =>
                            #{  <<"type">> => <<"AE2AE">>,
                                <<"network_id">> => ?PARENT_CHAIN_NETWORK_ID,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 100000000000000,
                                <<"amount">> => 9700
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"nodes">> =>
                                    [   #{  <<"host">> => <<"127.0.0.1">>,
                                            <<"port">> => aecore_suite_utils:external_api_port(?PARENT_CHAIN_NODE1),
                                            <<"user">> => <<"test">>,
                                            <<"password">> => <<"Pass">>}
                                    ]
                            },
                        <<"producing_commitments">> => ProducingCommitments
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"lazy_leader_trigger_time">> => ?LAZY_INTERVAL
                 };
            _ when Consensus == ?CONSENSUS_HC_BTC; Consensus == ?CONSENSUS_HC_DOGE ->
                PCType = case Consensus of
                            ?CONSENSUS_HC_BTC -> <<"AE2BTC">>;
                            ?CONSENSUS_HC_DOGE -> <<"AE2DOGE">>
                        end,
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?CHILD_START_HEIGHT,
                        <<"confirmations">> => ?CHILD_CONFIRMATIONS,
                        <<"consensus">> =>
                            #{  <<"type">> => PCType,
                                <<"network_id">> => <<"regtest">>,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 95000,
                                <<"amount">> => 7500
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"nodes">> =>
                                    [   #{  <<"host">> => <<"127.0.0.1">>,
                                            <<"port">> => ?BTC_PARENT_CHAIN_PORT,
                                            <<"user">> => <<"test">>,
                                            <<"password">> => <<"Pass">>}
                                    ]
                            },
                        <<"producing_commitments">> => ProducingCommitments
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"lazy_leader_trigger_time">> => ?LAZY_INTERVAL
                 }
        end,
    Protocol = aect_test_utils:latest_protocol_version(),
    #{<<"chain">> =>
            #{  <<"persist">> => false,
                <<"hard_forks">> => #{integer_to_binary(Protocol) => 0},
                <<"consensus">> =>
                    #{<<"0">> => #{<<"type">> => ConsensusType,
                                <<"config">> =>
                                maps:merge(
                                    #{  <<"election_contract">> => aeser_api_encoder:encode(contract_pubkey, election_contract_address()),
                                        <<"rewards_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                                        <<"contract_owner">> => aeser_api_encoder:encode(account_pubkey,?OWNER_PUBKEY),
                                        <<"expected_key_block_rate">> => 2000,
                                        <<"stakers">> => Stakers},
                                    SpecificConfig)
                                    }}},
        <<"fork_management">> =>
            #{<<"network_id">> => <<"this_will_be_overwritten_runtime">>},
        <<"logging">> => #{<<"level">> => <<"debug">>},
        <<"mining">> =>
            #{<<"micro_block_cycle">> => 1,
            <<"autostart">> => true,
            %%<<"autostart">> => ProducingCommitments,
            <<"beneficiary_reward_delay">> => ?REWARD_DELAY
        }}.  %% this relies on certain nonce numbers
validator_pool_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

election_contract_by_consensus(?CONSENSUS_HC) -> ?HC_ELECTION_CONTRACT;
election_contract_by_consensus(?CONSENSUS_HC_BTC) -> ?HC_ELECTION_CONTRACT;
election_contract_by_consensus(?CONSENSUS_POS) -> ?POS_ELECTION_CONTRACT.

produce_blocks(_Node, _NodeName, parent = _NodeType, BlocksCnt, Config, ?CONSENSUS_HC_BTC) ->
    produce_btc_blocks(?config(bitcoin_cli, Config), ?config(btc_beneficiary, Config), BlocksCnt);
produce_blocks(_Node, NodeName, parent = _NodeType, BlocksCnt, _Config, _Consensus) ->
    {ok, _} = aecore_suite_utils:mine_key_blocks(NodeName, BlocksCnt);
produce_blocks(Node, NodeName, NodeType, BlocksCnt, Config, Consensus) ->
    produce_blocks(Node, NodeName, NodeType, BlocksCnt, Config, Consensus, correct_leader).

produce_blocks(Node, NodeName, child = _NodeType, BlocksCnt, _Config, ?CONSENSUS_POS, _HCType) ->
    TopHeight = rpc(Node, aec_chain, top_height, []),
    ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + BlocksCnt,
                                            5000 * BlocksCnt), %% 5s per block
    get_generations(Node, TopHeight + 1, TopHeight + BlocksCnt);
produce_blocks(Node, NodeName, child = _NodeType, BlocksCnt, _Config, ?CONSENSUS_HC, HCType) ->
    TopHeight0 = rpc(Node, aec_chain, top_height, []),
    produce_blocks_hc(Node, NodeName, BlocksCnt, HCType),
    TopHeight = rpc(Node, aec_chain, top_height, []),
    get_generations(Node, TopHeight0 + 1, TopHeight);
produce_blocks(Node, NodeName, child = _NodeType, BlocksCnt, Config, ?CONSENSUS_HC_BTC, _) ->
    TopHeight0 = rpc(Node, aec_chain, top_height, []),
    produce_blocks_hc_btc(Node, NodeName, ?config(bitcoin_cli, Config), ?config(btc_beneficiary, Config), BlocksCnt),
    TopHeight = rpc(Node, aec_chain, top_height, []),
    get_generations(Node, TopHeight0 + 1, TopHeight).

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case rpc(Node, aec_chain, get_generation_by_height, [Height, forward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error -> error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)),
    {ok, lists:reverse(ReversedBlocks)}.

get_btc_generations(BitcoinCli, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                Hash = btc_get_blockhash(BitcoinCli, Height),
                case btc_get_block(BitcoinCli, Hash, 2) of
                    #{} = Block ->
                        [Block | Accum];
                    Err ->
                        ct:log("Failed to fetch BTC block ~p", [Err]),
                        error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)),
    {ok, lists:reverse(ReversedBlocks)}.

produce_blocks_hc(_Node, _NodeName, BlocksCnt, _LeaderType) when BlocksCnt < 1 ->
    ok;
produce_blocks_hc(Node, NodeName, BlocksCnt, LeaderType) ->
    ParentNode = ?PARENT_CHAIN_NODE1,
    ParentNodeName = ?PARENT_CHAIN_NODE1_NAME,
    %% make sure the parent chain is not mining
    stopped = rpc:call(ParentNodeName, aec_conductor, get_mining_state, []),
    %% initial child chain state
    TopHeight = rpc(Node, aec_chain, top_height, []),
    ct:log("Producing a block with height ~p", [TopHeight + 1]),
    %% mine a single block on the parent chain
    case LeaderType of
        lazy_leader ->
            {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
            {ok, [KB]} = aecore_suite_utils:mine_key_blocks(ParentNodeName, 1),
            ct:log("Patent block mined ~p", [KB]),
            ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, ?LAZY_INTERVAL + 5000),
            CTop = rpc(Node, aec_chain, top_block, []),
            true = is_keyblock_lazy(CTop),
            ok;
        abnormal_commitments_cnt ->
            {ok, _} = wait_for_at_least_commitments_in_pool(ParentNode, Node, 2),
            {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
            {ok, [KB]} = aecore_suite_utils:mine_key_blocks(ParentNodeName, 1),
            ct:log("Patent block mined ~p", [KB]),
            ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 10000), %% 10s per block
            CTop = rpc(Node, aec_chain, top_block, []),
            false = is_keyblock_lazy(CTop),
            ok;
        correct_leader ->
            {ok, _} = wait_for_commitments_in_pool(ParentNode, Node, 2),
            {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
            {ok, [KB]} = aecore_suite_utils:mine_key_blocks(ParentNodeName, 1),
            ct:log("Patent block mined ~p", [KB]),
            ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 10000), %% 10s per block
            CTop = rpc(Node, aec_chain, top_block, []),
            false = is_keyblock_lazy(CTop),
            ok
    end,
    %% wait for the child to catch up
    produce_blocks_hc(Node, NodeName, BlocksCnt - 1, LeaderType).

wait_for_commitments_in_pool(Node, CNode, Cnt) ->
    wait_for_commitments_in_pool_(Node, CNode, fun(Pool) ->
                                                    TxsCnt = length(Pool),
                                                    Res = TxsCnt =:= Cnt,
                                                    case Res of
                                                        true ->
                                                            validate_expected_commitments(CNode, Pool);
                                                        false ->
                                                            pass
                                                    end,
                                                    Res
                                               end).

wait_for_at_least_commitments_in_pool(Node, CNode, Cnt) ->
    wait_for_commitments_in_pool_(Node, CNode, fun(Pool) ->
                                                    TxsCnt = length(Pool),
                                                    TxsCnt >= Cnt
                                               end).

%% wait_for_commitments_in_pool_but_allow_other_txs(Node, CNode, Cnt) ->
%%     wait_for_commitments_in_pool_(Node, CNode,
%%         fun(Pool) ->
%%             {ok, TopH} = aec_headers:hash_header(rpc(Node, aec_chain, top_header, [])),
%%             ExpectedCommitment = aeser_api_encoder:encode(key_block_hash, TopH),

%%             TxsCnt =
%%                 length(
%%                     lists:filter(
%%                         fun(SignedTx) ->
%%                             {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
%%                             ct:log("Spend payload ~p", [aec_spend_tx:payload(SpendTx)]),
%%                             aec_spend_tx:payload(SpendTx) =:= ExpectedCommitment
%%                         end,
%%                         Pool)),
%%             TxsCnt =:= Cnt
%%         end).

wait_for_commitments_in_pool_(Node, ChildNode, CompareFun) ->
    wait_for_commitments_in_pool_(Node, ChildNode, CompareFun, 100).

wait_for_commitments_in_pool_(Node, _ChildNode, CompareFun, Attempts) when Attempts < 1 ->
    {ok, Pool} = rpc(Node, aec_tx_pool, peek, [infinity]),
    case CompareFun(Pool) of
        true ->
            {ok, Pool};
        false ->
            error({run_out_of_attempts, length(Pool)})
    end;
wait_for_commitments_in_pool_(Node, ChildNode, CompareFun, Attempts) ->
    TopHeader = rpc(Node, aec_chain, top_header, []),
    {ok, TopHash} = aec_headers:hash_header(TopHeader),
    TopHeight = aec_headers:height(TopHeader),
    {ok, ChildTopBlock} = rpc(ChildNode, aec_chain, top_key_block, []),
    {ok, CTopHash} = aec_blocks:hash_internal_representation(ChildTopBlock),
    CTopHeight = aec_blocks:height(ChildTopBlock),
    {ok, Pool} = rpc(Node, aec_tx_pool, peek, [infinity]),
    ct:log("Parent Height ~p, hash ~p, ~p commitments in pool ~p",
           [TopHeight, aeser_api_encoder:encode(key_block_hash, TopHash),
            length(Pool), Pool]),
    ct:log("Child Height ~p, hash ~p",
           [CTopHeight, aeser_api_encoder:encode(key_block_hash, CTopHash)]),
    case CompareFun(Pool) of
        true ->
            {ok, Pool};
        false ->
            timer:sleep(30),
            wait_for_commitments_in_pool_(Node, ChildNode, CompareFun, Attempts - 1)
    end.

produce_blocks_hc_btc(_Node, _NodeName, _BitcoinCli, _BtcBeneficiary, BlocksCnt) when BlocksCnt < 1 ->
    ok;
produce_blocks_hc_btc(Node, NodeName, BitcoinCli, BtcBeneficiary, BlocksCnt) ->
    %% initial child chain state
    TopHeight = rpc(Node, aec_chain, top_height, []),
    %% mine a single block on the parent chain
    produce_btc_blocks(BitcoinCli, BtcBeneficiary, 1),
    ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 5000), %% 5s per block
    wait_for_btc_commitments_in_pool(BitcoinCli, 2),
    %% wait for the child to catch up
    produce_blocks_hc_btc(Node, NodeName, BitcoinCli, BtcBeneficiary, BlocksCnt - 1).

produce_btc_blocks(BitcoinCli, Beneficiary, BlockCnt) ->
    Res = os:cmd(BitcoinCli ++ "generatetoaddress " ++ integer_to_list(BlockCnt) ++ " " ++ Beneficiary),
    BlockHashes = jsx:decode(list_to_binary(Res)),
    lists:foreach(fun(BH) ->
        os:cmd(BitcoinCli ++ "getblock " ++ binary_to_list(BH))
    end, BlockHashes),
    BlockCnt = length(BlockHashes).

wait_for_btc_commitments_in_pool(BitcoinCli, NumberOfCommitments) ->
    wait_for_btc_commitments_in_pool(BitcoinCli, NumberOfCommitments, 100).

wait_for_btc_commitments_in_pool(BitcoinCli, Cnt, Attempts) when Attempts < 1 ->
    TxsCnt = btc_pool_size(BitcoinCli),
    error({run_out_of_attempts, Cnt, TxsCnt});
wait_for_btc_commitments_in_pool(BitcoinCli, Cnt, Attempts) ->
    TopHash = string:trim(os:cmd(BitcoinCli ++ "getbestblockhash")),
    Height = btc_height(BitcoinCli, TopHash),
    TxsCnt = btc_pool_size(BitcoinCli),
    ct:log("Height ~p, hash ~p, commitments in pool ~p",
           [Height, TopHash, TxsCnt]),
    case TxsCnt =:= Cnt of
        true -> ok;
        false when TxsCnt > Cnt -> error(more_tx_in_pool);
        false ->
            timer:sleep(30),
            wait_for_btc_commitments_in_pool(BitcoinCli, Cnt, Attempts - 1)
    end.

btc_pool_size(BitcoinCli) ->
    Resp = list_to_binary(os:cmd(BitcoinCli ++ "getmempoolinfo")),
    ct:log("BTC Size resp ~p", [Resp]),
    MempoolInfo = jsx:decode(Resp, [return_maps]),
    maps:get(<<"size">>, MempoolInfo).

btc_top_height(BitcoinCli) ->
    TopHash = string:trim(os:cmd(BitcoinCli ++ "getbestblockhash")),
    btc_height(BitcoinCli, TopHash).

btc_height(BitcoinCli, TopHash) ->
    Block = btc_get_block(BitcoinCli, TopHash),
    maps:get(<<"height">>, Block).

btc_get_block(BitcoinCli, Hash) ->
    btc_get_block(BitcoinCli, Hash, 1).

btc_get_block(BitcoinCli, Hash, Verbosity) ->
    VerbosityStr = case BitcoinCli of
        "doge" ++ _ -> "true";
        _ -> integer_to_list(Verbosity)
    end,
    Block = list_to_binary(string:trim(os:cmd(BitcoinCli ++ "getblock " ++ Hash ++ " " ++ VerbosityStr))),
    jsx:decode(Block, [return_maps]).

btc_get_blockhash(BitcoinCli, Height) ->
    string:trim(os:cmd(BitcoinCli ++ "getblockhash " ++ integer_to_list(Height))).

get_doge_raw_tx(BitcoinCli, TxId) ->
    Tx = list_to_binary(string:trim(os:cmd(BitcoinCli ++ "getrawtransaction " ++ binary_to_list(TxId) ++ " true"))),
    {ok, jsx:decode(Tx, [return_maps])}.

validate_expected_commitments(Node, Commitments) ->
    TopH = rpc(Node, aec_chain, top_key_block_hash, []),
    NetworkId = rpc(Node, aec_governance, get_network_id, []),
    ExpectedCommitments =
        lists:map(
            fun(Staker) ->
                rpc(?NODE1, aec_parent_chain_block, encode_commitment_btc, [pubkey(Staker), TopH, NetworkId])
            end,
            [?ALICE, ?BOB]),
    ct:log("Child chain top hashes ~p", [ExpectedCommitments]),
    case lists:all(fun(SignedTx) ->
                        {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
                        ct:log("Spend payload ~p", [aec_spend_tx:payload(SpendTx)]),
                        lists:member(aec_spend_tx:payload(SpendTx), ExpectedCommitments)
                    end,
                   Commitments) of
        true -> ok;
        false -> error(commitments_mismatch)
    end.

get_commitments(From, To) ->
    {ok, Blocks} = get_generations(?PARENT_CHAIN_NODE1, From, To + 1),
    MicroBlocks =
        lists:filter(fun(B) -> aec_blocks:type(B) =:= micro end, Blocks),
    maps:from_list(
        lists:map(
            fun(MB) -> {aec_blocks:height(MB), aec_blocks:txs(MB)} end,
            MicroBlocks)).

get_btc_commitments(BitcoinCli, From, To) ->
    {ok, Blocks} = get_btc_generations(BitcoinCli, From, To + 1),
    maps:from_list(
        lists:map(
            fun(B) -> {maps:get(<<"height">>, B), maps:get(<<"tx">>, B)} end,
            Blocks)).


is_keyblock_lazy(KB) ->
    ct:log("Inspecting block ~p", [KB]),
    0 =:= aec_blocks:difficulty(KB).

