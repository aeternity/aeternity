-module(aehttp_hyperchains_SUITE).

-import(aecore_suite_utils, [ http_request/4
                            , internal_address/0
                            , external_address/0
                            , rpc/3
                            , rpc/4
                            ]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([produce_first_epoch/1,
         mine_and_sync/1,
         spend_txs/1,
         simple_withdraw/1,
         change_leaders/1,
         empty_parent_block/1,
         sync_third_node/1,
         verify_fees/1,
         elected_leader_did_not_show_up/1,
         block_difficulty/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(MAIN_STAKING_CONTRACT, "MainStaking").
-define(HC_CONTRACT, "HCElection").
-define(CONSENSUS, hc).
-define(START_HEIGHT, 100).
-define(CHILD_EPOCH_LENGTH, 20).
-define(CHILD_BLOCK_TIME, 1000).
-define(PARENT_EPOCH_LENGTH, 5).
-define(PARENT_FINALITY, 3).
-define(REWARD_DELAY, 2).

-define(NODE1, dev1).
-define(NODE1_NAME, aecore_suite_utils:node_name(?NODE1)).

-define(NODE2, dev2).
-define(NODE2_NAME, aecore_suite_utils:node_name(?NODE2)).

-define(NODE3, dev3).
-define(NODE3_NAME, aecore_suite_utils:node_name(?NODE3)).

%% -define(LAZY_NODE, dev8).
%% -define(LAZY_NODE_NAME, aecore_suite_utils:node_name(?LAZY_NODE)).

-define(OWNER_PUBKEY, <<42:32/unit:8>>).

-define(PARENT_CHAIN_NODE, aecore_suite_utils:parent_chain_node(1)).
-define(PARENT_CHAIN_NODE_NAME, aecore_suite_utils:node_name(?PARENT_CHAIN_NODE)).
-define(PARENT_CHAIN_NETWORK_ID, <<"local_testnet">>).

-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(INITIAL_STAKE, 1_000_000_000_000_000_000_000_000).

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

all() -> [{group, hc}].

groups() ->
    [
     {hc, [sequence], [ produce_first_epoch
                      , verify_fees
                      , mine_and_sync
                      , spend_txs
                      , simple_withdraw
                      , change_leaders
                      , sync_third_node
                      ]}
    ].

suite() -> [].

init_per_suite(Config0) ->
    case aect_test_utils:require_at_least_protocol(?CERES_PROTOCOL_VSN) of
        {skip, _} = Skip -> Skip;
        ok ->
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.hyperchains"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite([?NODE1, ?NODE2],
                                                        #{}, %% config is rewritten per suite
                                                        [],
                                                        Config),
            GenesisProtocol = 1,
            {ok, AccountFileName} =
                aecore_suite_utils:hard_fork_filename(?PARENT_CHAIN_NODE, Config1, integer_to_list(GenesisProtocol), "accounts_test.json"),
            GenesisProtocolBin = integer_to_binary(GenesisProtocol),
            ParentCfg =
                #{  <<"chain">> =>
                        #{  <<"persist">> => false,
                            <<"hard_forks">> =>
                                #{  GenesisProtocolBin => #{<<"height">> => 0, <<"accounts_file">> => AccountFileName},
                                    integer_to_binary(?CERES_PROTOCOL_VSN) => #{<<"height">> => 1}
                                },
                            <<"consensus">> =>
                                #{<<"0">> => #{<<"type">> => <<"ct_tests">>}}
                         },
                    <<"fork_management">> =>
                        #{<<"network_id">> => ?PARENT_CHAIN_NETWORK_ID},
                    %%<<"http">> => #{<<"external">> => #{<<"acceptors">> => 100}},
                    <<"http">> => #{<<"cache">> => #{<<"enabled">> => false}},
                    <<"mempool">> => #{<<"nonce_offset">> => 200},
                    <<"mining">> =>
                        #{<<"micro_block_cycle">> => 1,
                          <<"expected_mine_rate">> => 2000,
                          <<"autostart">> => false,
                          <<"beneficiary_reward_delay">> => ?REWARD_DELAY }
                },
            aecore_suite_utils:make_multi(Config1, [?PARENT_CHAIN_NODE]),
            aecore_suite_utils:create_config(?PARENT_CHAIN_NODE, Config1, ParentCfg, []),
            {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(?PARENT_CHAIN_NODE),
            ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            aecore_suite_utils:create_seed_file(AccountFileName,
                #{  ParentPatronPubEnc => 100000000000000000000000000000000000000000000000000000000000000000000000
                    %%, encoded_pubkey(?DWIGHT) => 2100000000000000000000000000
                    %%, encoded_pubkey(?EDWIN) => 3100000000000000000000000000
                }),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            [{staking_contract, StakingContract}, {election_contract, ElectionContract} | Config1]
    end.

end_per_suite(Config) ->
    catch aecore_suite_utils:stop_node(?NODE1, Config),
    catch aecore_suite_utils:stop_node(?NODE2, Config),
    catch aecore_suite_utils:stop_node(?NODE3, Config),
    catch aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE, Config),
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(hc, Config0) ->
    VM = fate,
    Config = aect_test_utils:init_per_group(VM, Config0),
    NetworkId = <<"hc">>,

    GenesisStartTime = aeu_time:now_in_msecs(),
    ElectionContract = ?HC_CONTRACT,
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)} ],

    aecore_suite_utils:start_node(?PARENT_CHAIN_NODE, Config),
    aecore_suite_utils:connect(?PARENT_CHAIN_NODE_NAME, []),
    timer:sleep(1000),
    produce_blocks(?PARENT_CHAIN_NODE, ?PARENT_CHAIN_NODE_NAME,
                   parent, ?START_HEIGHT + ?PARENT_EPOCH_LENGTH + ?PARENT_FINALITY, Config),

    ReceiveAddress = encoded_pubkey(?FORD),

    NodeConfig1 = node_config(NetworkId,?NODE1, Config, [?ALICE, ?BOB, ?LISA], ReceiveAddress, ?CONSENSUS, false,  GenesisStartTime),
    NodeConfig2 = node_config(NetworkId,?NODE2, Config, [], ReceiveAddress, ?CONSENSUS, false, GenesisStartTime),
    build_json_files(ElectionContract, [NodeConfig1, NodeConfig2]),
    aecore_suite_utils:create_config(?NODE1, Config, NodeConfig1, [{add_peers, true}]),
    aecore_suite_utils:create_config(?NODE2, Config, NodeConfig2, [{add_peers, true}]),
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    aecore_suite_utils:start_node(?NODE2, Config, Env),
    aecore_suite_utils:connect(?NODE2_NAME, []),

    ParentTopHeight0 = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("Parent chain top height ~p", [ParentTopHeight0]),
    timer:sleep(1000),
    Config1 = [{network_id, NetworkId}, {consensus, ?CONSENSUS}, {genesis_start_time, GenesisStartTime},
               {staker_names, [?ALICE, ?BOB, ?LISA]} | Config],
    Config1.

set_up_third_node(Config) ->
    GenesisStartTime = ?config(genesis_start_time, Config),
    false = GenesisStartTime =:= undefined,
    ElectionContract = ?HC_CONTRACT,
    NetworkId = <<"hc">>,
    aecore_suite_utils:make_multi(Config, [?NODE3]),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)} ],
    ReceiveAddress = encoded_pubkey(?FORD),
    NodeConfig = node_config(NetworkId,?NODE3, Config, [?LISA], ReceiveAddress, ?CONSENSUS, false, GenesisStartTime),
    build_json_files(ElectionContract, [NodeConfig]),
    aecore_suite_utils:create_config(?NODE3, Config, NodeConfig, [{add_peers, true}]),
    aecore_suite_utils:start_node(?NODE3, Config, Env),
    aecore_suite_utils:connect(?NODE3_NAME, []),
    timer:sleep(1000),
    Node3Peers = rpc(?NODE3, aec_peers, connected_peers, []),
    ct:log("Connected peers ~p", [Node3Peers]),
    Node3VerifiedPeers = rpc(?NODE3, aec_peers, available_peers, [verified]),
    ct:log("Verified peers ~p", [Node3VerifiedPeers]),
    {ok, _} = wait_same_top(?NODE1, ?NODE3),
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
    ct:log("Node3 point of view:", []),
    Inspect(?NODE3),
    {ok, _} = wait_same_top(?NODE1, ?NODE3),
    Config.

end_per_group(hc, Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config),
    aecore_suite_utils:stop_node(?NODE3, Config),
    aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE, Config);
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
    {ok, [KB]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
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

produce_first_epoch(Config) ->
    {ok, Bs} = produce_blocks(?NODE1, ?NODE1_NAME, child, ?CHILD_EPOCH_LENGTH, Config),
    Producers = [ aec_blocks:miner(B) || B <- Bs ],
    Leaders = leaders_at_height(?NODE1, 1, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    ?assert(true, length(Producers) > 1),  %% should be for larger EPOCHs
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, ChildBlocks} = get_generations(?NODE1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

simple_withdraw(Config) ->
    AliceBin = encoded_pubkey(?ALICE),
    Alice = binary_to_list(encoded_pubkey(?ALICE)),
    running = rpc:call(?NODE1_NAME, aec_conductor, get_mining_state, []),
    true = rpc:call(?NODE1_NAME, aec_conductor, is_leader, []),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),

    {ok, [KB0 | _ ]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
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
            contract_call(ContractPubkey, ?MAIN_STAKING_CONTRACT, Fun,
                  [Alice, integer_to_list(WithdrawAmount)], 0, pubkey(?ALICE)),
            ?ALICE,
            NetworkId),
    {ok, [_]} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    {value, _Acc} = rpc(?NODE1, aec_chain, get_account, [pubkey(?ALICE)]),
    mine_tx_no_cheating(?NODE1, CallTx),
    EndBalance = account_balance(pubkey(?ALICE)),
    {ok, Call} = call_info(CallTx),
    {ok, _Res} = decode_consensus_result(Call, Fun, ?MAIN_STAKING_CONTRACT),
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
            {ok, [KB | _ ]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
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

sync_third_node(Config) ->
    set_up_third_node(Config).

empty_parent_block(_Config) ->
    case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
        true ->
            {skip, lazy_leader_sync_broken_on_iris};
        false ->
            %% empty_parent_block_(Config)
            {skip, todo}
    end.

%% empty_parent_block_(_Config) ->
%%     TopHeight = rpc(?LAZY_NODE, aec_chain, top_height, []),
%%     %% Remove the posted commitments to create a parent generation without commitments
%%     aecore_suite_utils:flush_mempool(2, ?PARENT_CHAIN_NODE1_NAME),
%%     ok = produce_blocks_hc(?LAZY_NODE, ?LAZY_NODE_NAME, 1, lazy_leader),

%%     %% The lazy leader block is either from ?LISA (dev8) or ?ALICE/?BOB (dev1)
%%     ct:log("Checking block with height ~p", [TopHeight + 1]),
%%     CTop = rpc(?LAZY_NODE, aec_chain, top_block, []),
%%     %% true = is_keyblock_lazy(CTop),

%%     %% Produce yet another block (now dev1 should commit to a lazy block and win)
%%     ok = produce_blocks_hc(?LAZY_NODE, ?LAZY_NODE_NAME, 1, abnormal_commitments_cnt),

%%     %% Check that they sync and that the right chain wins
%%     {ok, CTop2} = wait_same_top(?NODE1, ?LAZY_NODE),

%%     case lists:member(aec_blocks:miner(CTop2), [element(1, ?ALICE), element(1, ?BOB)]) of
%%       true -> ok;
%%       _ ->
%%         ct:log("ERROR: Expected ~p or ~200p as the miner:\nTOP  : ~200p",
%%                [element(1, ?ALICE), element(1, ?BOB), CTop2]),
%%         ?assert(false)
%%     end,
%%     ok.

verify_fees(Config) ->
    %% start without any tx fees, only a keyblock
    Test =
        fun() ->
            %% gather staking_powers before reward distribution
            AliceBalance0 = account_balance(pubkey(?ALICE)),
            BobBalance0 = account_balance(pubkey(?BOB)),
            produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
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
    produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
    ct:log("Test with no transaction", []),
    {ok, _SignedTx} = seed_account(pubkey(?ALICE), 1, NetworkId),
    produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
    Test(), %% after fees
    ok.

block_difficulty(Config) ->
    lists:foreach(
        fun(_) ->
            {ok, [KB]} = produce_blocks(?NODE1, ?NODE1_NAME, child, 1, Config),
            {ok, AddedStakingPower} = inspect_election_contract(?ALICE, current_added_staking_power, Config),
            Target = aec_blocks:target(KB),
            {Target, Target} = {Target, aeminer_pow:integer_to_scientific(AddedStakingPower)}
        end,
        lists:seq(1, 20)), %% test with 20 elections
    ok.

elected_leader_did_not_show_up(Config) ->
    case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
        true ->
            {skip, lazy_leader_sync_broken_on_iris};
        false ->
            elected_leader_did_not_show_up_(Config)
    end.

elected_leader_did_not_show_up_(Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config), %% stop the block producer
    TopHeader0 = rpc(?NODE2, aec_chain, top_header, []),
    {TopHeader0, TopHeader0} = {rpc(?NODE3, aec_chain, top_header, []), TopHeader0},
    ct:log("Starting test at (child chain): ~p", [TopHeader0]),
    %% produce a block on the parent chain
    ok = produce_blocks_hc(?NODE3, ?NODE3_NAME, 1, lazy_leader, Config),
    {ok, KB} = wait_same_top(?NODE2, ?NODE3),
    0 = aec_blocks:difficulty(KB),
    TopHeader1 = rpc(?NODE3, aec_chain, top_header, []),
    ct:log("Lazy header: ~p", [TopHeader1]),
    TopHeader1 = rpc(?NODE2, aec_chain, top_header, []),
    NetworkId = ?config(network_id, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)} ],
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    {ok, _} = wait_same_top(?NODE1, ?NODE3),
    timer:sleep(2000), %% Give NODE1 a moment to finalize sync and post commitments
    ok = produce_blocks_hc(?NODE1, ?NODE1_NAME, 1, abnormal_commitments_cnt, Config),
    {ok, KB1} = wait_same_top(?NODE1, ?NODE3),
    {ok, KB1} = wait_same_top(?NODE2, ?NODE3),
    {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 10, Config),
    {ok, KB2} = wait_same_top(?NODE1, ?NODE3),
    {ok, KB2} = wait_same_top(?NODE2, ?NODE3),
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

%% who_by_pubkeyhashprefix(PubKeyHash) ->
%%     Alice = pubkey_hash_prefix(?ALICE),
%%     Bob = pubkey_hash_prefix(?BOB),
%%     Dwight = pubkey_hash_prefix(?DWIGHT),
%%     Edwin = pubkey_hash_prefix(?EDWIN),
%%     Genesis = ?GENESIS_BENFICIARY,
%%     case PubKeyHash of
%%         Alice -> ?ALICE;
%%         Bob -> ?BOB;
%%         Dwight -> ?DWIGHT;
%%         Edwin -> ?EDWIN;
%%         Genesis -> genesis;
%%         _  -> error(unknown_beneficiary)
%%     end.

%% pubkey_hash_prefix(Who) ->
%%     PubKey = pubkey(Who),
%%     StakerPubKeyFate = aeb_fate_encoding:serialize(aeb_fate_data:make_address(PubKey)),
%%     <<StakerHash:8/binary, _/binary>> = aec_hash:sha256_hash(StakerPubKeyFate),
%%     StakerHash.

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
                {"get_state", []};
            leaders ->
                {"sorted_validators", []}
        end,
    ContractPubkey = ?config(staking_contract, Config),
    Tx = contract_call(ContractPubkey, ?MAIN_STAKING_CONTRACT, Fun,
                  Args, 0, pubkey(OriginWho)),
    {ok, Call} = dry_run(TopHash, Tx),
    {_Type, _Res} = decode_consensus_result(Call, Fun, ?MAIN_STAKING_CONTRACT).

inspect_election_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            current_leader -> {"leader", []};
            current_added_staking_power -> {"added_stake", []};
            validators -> {"sorted_validators", []}
        end,
    ContractPubkey = ?config(election_contract, Config),
    ElectionContract = ?HC_CONTRACT,
    Tx = contract_call(ContractPubkey, ElectionContract, Fun, Args, 0, pubkey(OriginWho)),
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
    KeyReward = rpc(?NODE1, aec_governance, block_mine_reward, [RewardForHeight]),
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

build_json_files(ElectionContract, NodeConfigs) ->
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
        = contract_create_spec(?MAIN_STAKING_CONTRACT,
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
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?ALICE), 1),
    Call2 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?BOB), 1),
    Call3 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?LISA), 1),
    Call4  =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_online", [], 0, pubkey(?ALICE), 2),
    Call5  =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_online", [], 0, pubkey(?BOB), 2),
    Call6 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_online", [], 0, pubkey(?LISA), 2),
    Call7 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_validator_name", ["\"Alice\""], 0, pubkey(?ALICE), 3),
    Call8 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_validator_name", ["\"Bob\""], 0, pubkey(?BOB), 3),
    Call9 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_validator_name", ["\"Lisa\""], 0, pubkey(?LISA), 3),
    Call10 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
                            "set_validator_description",
                            ["\"Alice is a really awesome validator and she had set a description of her great service to the §work.\""], 0,
                            pubkey(?ALICE), 4),
    Call11 =
        contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
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
    %% Call12 =
    %%     contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
    %%                         "new_validator", [],
    %%                         ?INITIAL_STAKE, BRIPub, 1),
    %% Call13 =
    %%     contract_call_spec(SCId, ?MAIN_STAKING_CONTRACT,
    %%                         "set_validator_description",
    %%                         ["\"This validator is offline. She can never become a leader. She has no name set. She is receiving the BRI rewards\""],
    %%                         0, BRIPub, 2),
    %% keep the BRI offline
    AllCalls =  [Call1, Call2, Call3, Call4, Call5, Call6,
		 Call7, Call8, Call9, Call10, Call11],
    ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
    ContractsFileNames = [ContractsFileName  || #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"contracts_file">> := ContractsFileName}}}} <- NodeConfigs],
    AccountsFileNames = [AccountsFileName  || #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"accounts_file">> := AccountsFileName}}}} <- NodeConfigs],
    aecore_suite_utils:create_seed_file(ContractsFileNames,
        #{<<"contracts">> => [C0, SC, EC], <<"calls">> => AllCalls}),
    aecore_suite_utils:create_seed_file(AccountsFileNames,
        #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000
         }),
    ok.

%% node_config(NetworkId,Node, CTConfig, PotentialStakers, ReceiveAddress, Consensus) ->
%%     node_config(NetworkId,Node, CTConfig, PotentialStakers, ReceiveAddress, Consensus, false, 0).

node_config(NetworkId,Node, CTConfig, PotentialStakers, ReceiveAddress, _Consensus, ProducingCommitments, GenesisStartTime) ->
    Stakers = lists:map(
                    fun(HCWho) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))), %% TODO: discuss key management
                        #{ <<"hyper_chain_account">> => #{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv} }
                    end,
                    PotentialStakers),
    ConsensusType = <<"hyper_chain">>,
    Port = aecore_suite_utils:external_api_port(?PARENT_CHAIN_NODE),
    SpecificConfig =
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?START_HEIGHT,
                        <<"finality">> => ?PARENT_FINALITY,
                        <<"parent_generation">> => ?PARENT_EPOCH_LENGTH,
                        <<"consensus">> =>
                            #{  <<"type">> => <<"AE2AE">>,
                                <<"network_id">> => ?PARENT_CHAIN_NETWORK_ID,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 100000000000000,
                                <<"amount">> => 9700
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"cache_size">> => 10,
                                <<"nodes">> => [ iolist_to_binary(io_lib:format("http://test:Pass@127.0.0.1:~p", [Port])) ]
                            },
                        <<"producing_commitments">> => ProducingCommitments
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"child_epoch_length">> => ?CHILD_EPOCH_LENGTH,
                    <<"child_block_time">> => ?CHILD_BLOCK_TIME
                 },
    Protocol = aect_test_utils:latest_protocol_version(),
    {ok, ContractFileName} = aecore_suite_utils:hard_fork_filename(Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_contracts.json"),
    {ok, AccountFileName} = aecore_suite_utils:hard_fork_filename(Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_accounts.json"),
    #{<<"chain">> =>
            #{  <<"persist">> => false,
                <<"hard_forks">> => #{integer_to_binary(Protocol) => #{<<"height">> => 0,
                                                                       <<"contracts_file">> => ContractFileName,
                                                                       <<"accounts_file">> => AccountFileName}},
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
        <<"sync">> => #{<<"ping_interval">> => 5000},
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

produce_blocks(_Node, NodeName, parent = _NodeType, BlocksCnt, _Config) ->
    {ok, _} = aecore_suite_utils:mine_key_blocks(NodeName, BlocksCnt);
produce_blocks(Node, NodeName, NodeType, BlocksCnt, Config) ->
    produce_blocks(Node, NodeName, NodeType, BlocksCnt, Config, ?config(consensus, Config), correct_leader).

produce_blocks(Node, NodeName, child = _NodeType, BlocksCnt, Config, ?CONSENSUS, HCType) ->
    TopHeight0 = rpc(Node, aec_chain, top_height, []),
    produce_blocks_hc(Node, NodeName, BlocksCnt, HCType, Config),
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

produce_blocks_hc(_Node, _NodeName, BlocksCnt, _LeaderType, _Config) when BlocksCnt < 1 ->
    ok;
produce_blocks_hc(Node, NodeName, BlocksCnt, LeaderType, Config) ->
    %% ParentNode = ?PARENT_CHAIN_NODE,
    ParentNodeName = ?PARENT_CHAIN_NODE_NAME,
    %% make sure the parent chain is not mining
    stopped = rpc:call(ParentNodeName, aec_conductor, get_mining_state, []),
    %% initial child chain state
    TopHeight = rpc(Node, aec_chain, top_height, []),
    Producer = get_block_producer_name(?config(staker_names, Config), Node, TopHeight),
    ct:log("~p producer for height ~p. Now producing next block", [Producer, TopHeight]),
    %% mine a generation on the parent block
    NumParentBlocks = case (TopHeight + 1) rem ?CHILD_EPOCH_LENGTH == 0 of
                        true -> ?PARENT_EPOCH_LENGTH + ?PARENT_FINALITY;
                        _ -> 0
                      end,
    case LeaderType of
        %% lazy_leader ->
        %%     mine_key_blocks(ParentNodeName, NumParentBlocks),
        %%     ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, ?LAZY_INTERVAL + 5000),
        %%     CTop = rpc(Node, aec_chain, top_block, []),
        %%     %% Choosing leaders will change
        %%     %% true = is_keyblock_lazy(CTop),
        %%     ok;
        %% abnormal_commitments_cnt ->
        %%     {ok, _} = wait_for_at_least_commitments_in_pool(ParentNode, Node, 2),
        %%     mine_key_blocks(ParentNodeName, NumParentBlocks),
        %%     ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 10000), %% 10s per block
        %%     CTop = rpc(Node, aec_chain, top_block, []),
        %%     false = is_keyblock_lazy(CTop),
        %%     ok;
        correct_leader ->
            %% Don't wait for commitments, in the future pinning will be implemented
            %%{ok, _} = wait_for_commitments_in_pool(ParentNode, Node, 2),
            mine_key_blocks(ParentNodeName, NumParentBlocks),
            ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 10000), %% 10s per block
            _CTop = rpc(Node, aec_chain, top_block, []),
            %% Choosing leaders will change
            %% false = is_keyblock_lazy(CTop),
            ok
    end,
    %% wait for the child to catch up
    produce_blocks_hc(Node, NodeName, BlocksCnt - 1, LeaderType, Config).

mine_key_blocks(ParentNodeName, NumParentBlocks) ->
    {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
    {ok, KBs} = aecore_suite_utils:mine_key_blocks(ParentNodeName, NumParentBlocks),
    ct:log("Parent block mined ~p ~p number: ~p", [KBs, ParentNodeName, NumParentBlocks]).

get_block_producer_name(Parties, Node, Height) ->
    Producer = get_block_producer(Node, Height),
    case lists:keyfind(Producer, 1, Parties) of
        false -> Producer;
        {_, _, Name} -> Name
    end.

get_block_producer(Node, Height) ->
    {ok, KeyHeader} = rpc(Node, aec_chain, get_key_header_by_height, [Height]),
    aec_headers:miner(KeyHeader).

leaders_at_height(Node, Height, Config) ->
    {ok, Hash} = rpc(Node, aec_chain_state, get_key_block_hash_at_height, [Height]),
    {ok, Return} = inspect_staking_contract(?ALICE, leaders, Config, Hash),
    [ begin
        {account_pubkey, K} = aeser_api_encoder:decode(LeaderKey), K
      end || [ LeaderKey, _LeaderStake] <- Return ].
