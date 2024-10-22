-module(aehttp_hyperchains_SUITE).

-import(aecore_suite_utils, [ http_request/4
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


%% Test cases
-export([start_two_child_nodes/1,
         produce_first_epoch/1,
         produce_some_epochs/1,
         respect_schedule/1,
         mine_and_sync/1,
         spend_txs/1,
         simple_withdraw/1,
         empty_parent_block/1,
         sync_third_node/1,
         verify_fees/1,
         elected_leader_did_not_show_up/1,
         block_difficulty/1,
         epochs_with_slow_parent/1,
         epochs_with_fast_parent/1,
         check_blocktime/1,
         get_pin/1,
         wallet_post_pin_to_pc/1,
         post_pin_to_pc/1,
         first_leader_next_epoch/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(MAIN_STAKING_CONTRACT, "MainStaking").
-define(HC_CONTRACT, "HCElection").
-define(CONSENSUS, hc).
-define(CHILD_EPOCH_LENGTH, 10).
-define(CHILD_BLOCK_TIME, 200).
-define(PARENT_EPOCH_LENGTH, 3).
-define(PARENT_FINALITY, 2).
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

all() -> [{group, hc}, {group, epochs}, {group, pinning}].

groups() ->
    [
      {hc, [sequence],
          [ start_two_child_nodes
          , produce_first_epoch
          , verify_fees
          , mine_and_sync
          , spend_txs
          , simple_withdraw
          , sync_third_node
          , produce_some_epochs
          , respect_schedule
          , check_blocktime
          ]}
    , {epochs, [sequence],
          [ start_two_child_nodes
          , first_leader_next_epoch
          , epochs_with_slow_parent
          , epochs_with_fast_parent ]}
    , {pinning, [sequence],
          [ start_two_child_nodes,
            produce_first_epoch,
            get_pin,
            wallet_post_pin_to_pc,
            post_pin_to_pc ]}
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
                    , encoded_pubkey(?DWIGHT) => 2100000000000000000000000000
                    , encoded_pubkey(?EDWIN) => 3100000000000000000000000000
                }),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            {ok, SVBinSrc} = aect_test_utils:read_contract("StakingValidator"),
            {ok, MSBinSrc} = aect_test_utils:read_contract(?MAIN_STAKING_CONTRACT),
            {ok, EBinSrc} = aect_test_utils:read_contract(?HC_CONTRACT),
            [{staking_contract, StakingContract}, {election_contract, ElectionContract},
             {contract_src, #{"StakingValidator" => create_stub(binary_to_list(SVBinSrc)),
                              ?MAIN_STAKING_CONTRACT => create_stub(binary_to_list(MSBinSrc)),
                              ?HC_CONTRACT => create_stub(binary_to_list(EBinSrc))
                              }} | Config1]
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

init_per_group(_, Config0) ->
    VM = fate,
    NetworkId = <<"hc">>,
    GenesisStartTime = aeu_time:now_in_msecs(),
    Config = [{network_id, NetworkId}, {genesis_start_time, GenesisStartTime},
              {consensus, ?CONSENSUS} |
              aect_test_utils:init_per_group(VM, Config0)],

    aecore_suite_utils:start_node(?PARENT_CHAIN_NODE, Config),
    aecore_suite_utils:connect(?PARENT_CHAIN_NODE_NAME, []),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    StartHeight = max(ParentTopHeight, ?PARENT_EPOCH_LENGTH),
    ct:log("Parent chain top height ~p start at ~p", [ParentTopHeight, StartHeight]),
    %%TODO mine less than necessary parent height and test chain starts when height reached
    {ok, _} = mine_key_blocks(
            ?PARENT_CHAIN_NODE_NAME,
            (StartHeight - ParentTopHeight) + ?PARENT_FINALITY),
    [ {staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, StartHeight} | Config].

child_node_config(Node, Stakeholders, CTConfig) ->
    ReceiveAddress = encoded_pubkey(?FORD),
    Pinning = false,
    NodeConfig = node_config(Node, CTConfig, Stakeholders, ReceiveAddress, Pinning),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

end_per_group(_Group, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [ aecore_suite_utils:stop_node(Node, Config1)
      || {Node, _, _} <- proplists:get_value(nodes, Config1, []) ],
    Config1.

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [{nodes, [{?NODE1, ?NODE1_NAME, [?ALICE, ?LISA]},
                  {?NODE2, ?NODE2_NAME, [?BOB]}
                 ]}
         | Config],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    Nodes = ?config(nodes, Config1),
    Config2 = lists:keyreplace(nodes, 1, Config1,
                               {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, []}]}),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

with_saved_keys(Keys, Config) ->
    {_TC, SavedConfig} = ?config(saved_config, Config),
    lists:foldl(fun(Key, Conf) ->
                    case proplists:get_value(Key, SavedConfig) of
                        undefined -> Conf;
                        Val -> [{Key, Val} | Conf]
                    end
                end,
                lists:keydelete(saved_config, 1, Config), Keys).

contract_create_spec(Name, Src, Args, Amount, Nonce, Owner) ->
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

contract_call_spec(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(contract_call(ContractPubkey, Src, Fun, Args,
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

contract_call(ContractPubkey, Src, Fun, Args, Amount, From) ->
    Nonce = next_nonce(?NODE1, From), %% no contract calls support for parent chain
    contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce).

contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
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

%% This test is trivially true, since we already check in
%% sync when producting the blocks.
mine_and_sync(Config) ->
    {ok, _KBs} = produce_cc_blocks(Config, 3),
    {ok, _KB} = wait_same_top([ Node || {Node, _, _} <- ?config(nodes, Config)]),
    ok.

wait_same_top(Nodes) ->
    wait_same_top(Nodes, 3).

wait_same_top(_Nodes, Attempts) when Attempts < 1 ->
    %% {error, run_out_of_attempts};
    throw({error, run_out_of_attempts});
wait_same_top(Nodes, Attempts) ->
    KBs = [ rpc(Node, aec_chain, top_block, []) || Node <- Nodes ],
    case lists:usort(KBs) of
        [KB] -> {ok, KB};
        Diffs ->
            ct:log("Nodes differ: ~p", [Diffs]),
            timer:sleep(?CHILD_BLOCK_TIME div 2),
            wait_same_top(Nodes, Attempts - 1)
    end.

spend_txs(Config) ->
    produce_cc_blocks(Config, 1),
    Top0 = rpc(?NODE1, aec_chain, top_header, []),
    ct:log("Top before posting spend txs: ~p", [aec_headers:height(Top0)]),
    NetworkId = ?config(network_id, Config),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    seed_account(pubkey(?ALICE), 100000001 * ?DEFAULT_GAS_PRICE, NetworkId),
    seed_account(pubkey(?BOB), 100000002 * ?DEFAULT_GAS_PRICE, NetworkId),
    seed_account(pubkey(?LISA), 100000003 * ?DEFAULT_GAS_PRICE, NetworkId),

    produce_cc_blocks(Config, 1),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    %% TODO check that the actors got their share
    ok.

check_blocktime(_Config) ->
    {ok, TopBlock} = rpc(?NODE1, aec_chain, top_key_block, []),
    check_blocktime_(TopBlock).

check_blocktime_(Block) ->
    case aec_blocks:height(Block) >= 1 of
        true ->
            {ok, PrevBlock} = rpc(?NODE1, aec_chain, get_block, [aec_blocks:prev_key_hash(Block)]),
            Time1 = aec_blocks:time_in_msecs(Block),
            Time2 = aec_blocks:time_in_msecs(PrevBlock),
            [ ct:pal("Blocktime not respected KB(~p) at ~p and KB(~p) at ~p",
                     [aec_blocks:height(Block), Time1, aec_blocks:height(PrevBlock), Time2])
              || Time1 - Time2 < ?CHILD_BLOCK_TIME ],
            ?assertMatch(Diff when Diff >= ?CHILD_BLOCK_TIME, Time1 - Time2),
            check_blocktime_(PrevBlock);
        false ->
            ok
    end.

start_two_child_nodes(Config) ->
    [{Node1, NodeName1, Stakers1}, {Node2, NodeName2, Stakers2} | _] = ?config(nodes, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))} ],
    child_node_config(Node1, Stakers1, Config),
    aecore_suite_utils:start_node(Node1, Config, Env),
    aecore_suite_utils:connect(NodeName1, []),
    child_node_config(Node2, Stakers2, Config),
    aecore_suite_utils:start_node(Node2, Config, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

produce_first_epoch(Config) ->
    produce_n_epochs(Config, 1).

produce_some_epochs(Config) ->
    produce_n_epochs(Config, 5).

produce_n_epochs(Config, N) ->
    [{Node1, _, _}|_] = ?config(nodes, Config),
    %% produce blocks
    {ok, Bs} = produce_cc_blocks(Config, N * ?CHILD_EPOCH_LENGTH),
    %% check producers
    Producers = [ aec_blocks:miner(B) || B <- Bs ],
    ChildTopHeight = rpc(Node1, aec_chain, top_height, []),
    Leaders = leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

respect_schedule(Config) ->
    [{Node1, _, _}|_] = ?config(nodes, Config),
    ChildHeight = rpc(?NODE1, aec_chain, top_height, []),

    %% Validate one epoch at a time
    respect_schedule(Node1, 1, 1, ChildHeight).


respect_schedule(_Node, EpochStart, _Epoch, TopHeight) when TopHeight < EpochStart ->
    ok;
respect_schedule(Node, EpochStart, Epoch, TopHeight) ->
    {ok, #{first := StartHeight} = EI} =
        rpc(?NODE1, aec_chain_hc, epoch_info, [EpochStart]),

    #{ seed := EISeed, validators := EIValidators, length := EILength, last := EILast } = EI,

    ct:log("Checking epoch ~p info: ~p at height ~p", [Epoch, EI, EpochStart]),

    %% We buffer the seed two epochs, entropy height already looks at previous Epoch,
    %% hence Epoch - 1
    ParentHeight = rpc(?NODE1, aec_consensus_hc, entropy_height, [Epoch - 1]),
    {ok, PHdr}   = rpc(?PARENT_CHAIN_NODE, aec_chain, get_key_header_by_height, [ParentHeight]),
    {ok, PHash0} = aec_headers:hash_header(PHdr),
    PHash = aeser_api_encoder:encode(key_block_hash, PHash0),

    ct:log("ParentHash at height ~p: ~p", [ParentHeight, PHash]),
    ?assertMatch(Hash when Hash == undefined; Hash == PHash, EISeed),

    %% Check the API functions in aec_chain_hc
    {ok, Schedule} = rpc(?NODE1, aec_chain_hc, validator_schedule, [EpochStart, PHash, EIValidators, EILength]),
    ct:log("Validating schedule ~p for Epoch ~p", [Schedule, Epoch]),

    lists:foreach(fun({Height, ExpectedProducer}) when Height =< TopHeight ->
                              Producer = get_block_producer(Node, Height),
                              ct:log("Check producer of block ~p: ~p =?= ~p", [Height, Producer, ExpectedProducer]),
                              ?assertEqual(Producer, ExpectedProducer);
                     (_) -> ok
                  end, lists:zip(lists:seq(StartHeight, StartHeight + EILength - 1), Schedule)),

    respect_schedule(Node, EILast + 1, Epoch + 1, TopHeight).

simple_withdraw(Config) ->
    produce_cc_blocks(Config, 3), %% Make sure there are no lingering TxFees in the reward
    [{_, NodeName, _}|_] = ?config(nodes, Config),
    AliceBin = encoded_pubkey(?ALICE),
    Alice = binary_to_list(encoded_pubkey(?ALICE)),
    {ok, []} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),

    InitBalance  = account_balance(pubkey(?ALICE)),
    {ok, AliceContractSPower} = inspect_staking_contract(?ALICE, {staking_power, ?ALICE}, Config),
    {ok, BobContractSPower} = inspect_staking_contract(?ALICE, {staking_power, ?BOB}, Config),
    {ok,
        #{<<"ct">> := _, %% pool contract
          <<"is_online">> := true,
          <<"stake">> := _,
          <<"state">> :=
                #{<<"delegates">> := [[AliceBin, ?INITIAL_STAKE]],
                  <<"main_staking_ct">> := CPubkey,
                  <<"shares">> := ?INITIAL_STAKE}
         }} =
        inspect_staking_contract(?ALICE, {get_validator_state, ?ALICE}, Config),

    %% The results translation somehow makes a contract key into an account key!
    ?assertEqual(aeser_api_encoder:encode(account_pubkey, ?config(staking_contract, Config)), CPubkey),
    WithdrawAmount = 1000,
    NetworkId = ?config(network_id, Config),
    CallTx =
        sign_and_push(
            contract_call(?config(staking_contract, Config), src(?MAIN_STAKING_CONTRACT, Config), "unstake",
                [Alice, integer_to_list(WithdrawAmount)], 0, pubkey(?ALICE)),
            ?ALICE,
            NetworkId),
    {ok, [_]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    StakeWithdrawDelay = 1,
    produce_cc_blocks(Config, StakeWithdrawDelay),
    EndBalance = account_balance(pubkey(?ALICE)),
    {ok, Call} = call_info(CallTx),
    {ok, _Res} = decode_consensus_result(Call, "unstake", src(?MAIN_STAKING_CONTRACT, Config)),
    GasUsed = aect_call:gas_used(Call),
    GasPrice = aect_call:gas_price(Call),
    Fee = aetx:fee(aetx_sign:tx(CallTx)),
    {Producer, KeyReward} = key_reward_provided(),
    ct:log("Initial balance: ~p, withdrawn: ~p, gas used: ~p, gas price: ~p, fee: ~p, end balance: ~p",
           [InitBalance, WithdrawAmount, GasUsed, GasPrice, Fee, EndBalance]),
    {ok, AliceContractSPower1} = inspect_staking_contract(?ALICE, {staking_power, ?ALICE}, Config),
    {ok, BobContractSPower1} = inspect_staking_contract(?ALICE, {staking_power, ?BOB}, Config),
    ?assert(BobContractSPower == BobContractSPower1 orelse
            (Producer == pubkey(?BOB) andalso BobContractSPower + KeyReward == BobContractSPower1)),
    ct:log("Staking power before: ~p and after ~p", [AliceContractSPower, AliceContractSPower1]),
    ?assert(AliceContractSPower - 1000 == AliceContractSPower1 orelse
            (Producer == pubkey(?ALICE) andalso AliceContractSPower + KeyReward - 1000 == AliceContractSPower1)),
    ok.

set_up_third_node(Config) ->
    {Node3, NodeName, Stakers} = lists:keyfind(?NODE3, 1, ?config(nodes, Config)),
    Nodes = [ Node || {Node, _, _} <- ?config(nodes, Config)],
    aecore_suite_utils:make_multi(Config, [Node3]),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))} ],
    child_node_config(Node3, Stakers, Config),
    aecore_suite_utils:start_node(Node3, Config, Env),
    aecore_suite_utils:connect(NodeName, []),
    timer:sleep(1000),
    Node3Peers = rpc(Node3, aec_peers, connected_peers, []),
    ct:log("Connected peers ~p", [Node3Peers]),
    Node3VerifiedPeers = rpc(Node3, aec_peers, available_peers, [verified]),
    ct:log("Verified peers ~p", [Node3VerifiedPeers]),
    {ok, _} = wait_same_top(Nodes, 300),
    %% What on earth are we testing here??
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

verify_fees(Config) ->
    %% start without any tx fees, only a keyblock
    GetSPower = fun(Who1, Who2, When) ->
                    inspect_staking_contract(Who1, {staking_power, Who2}, Config, When)
                end,
    Test =
        fun() ->
            %% gather staking_powers before reward distribution
            AliceBalance0 = account_balance(pubkey(?ALICE)),
            BobBalance0 = account_balance(pubkey(?BOB)),
            produce_cc_blocks(Config, 1),
            {ok, TopKeyBlock} = rpc(?NODE1, aec_chain, top_key_block, []),
            TopKeyHeader = aec_blocks:to_header(TopKeyBlock),
            {ok, TopHash} = aec_headers:hash_header(TopKeyHeader),
            PrevHash = aec_headers:prev_key_hash(TopKeyHeader),
            {ok, AliceContractSPower0} = GetSPower(?ALICE, ?ALICE, PrevHash),
            {ok, BobContractSPower0}   = GetSPower(?ALICE, ?BOB, PrevHash),
            {ok, LisaContractSPower0}  = GetSPower(?ALICE, ?LISA, PrevHash),

            %% gather staking_powers after reward distribution
            AliceBalance1 = account_balance(pubkey(?ALICE)),
            BobBalance1   = account_balance(pubkey(?BOB)),
            {ok, AliceContractSPower1} = GetSPower(?ALICE, ?ALICE, TopHash),
            {ok, BobContractSPower1}   = GetSPower(?ALICE, ?BOB, TopHash),
            {ok, LisaContractSPower1}  = GetSPower(?ALICE, ?LISA, TopHash),

            %% inspect who shall receive what reward
            RewardForHeight = aec_headers:height(TopKeyHeader) - ?REWARD_DELAY,
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
    Test(),

    ct:log("Test with a spend transaction", []),
    {_, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    {ok, _SignedTx} = seed_account(PatronPub, 1, NetworkId),
    Test(), %% fees are generated

    %% key blocks are in sync, but give gossip time to sync micro block
    %% This won't be needed if micro blocks come before key blocks
    timer:sleep(?CHILD_BLOCK_TIME div 2),

    ct:log("Test with no transaction", []),
    [ Test() || _ <- lists:seq(0, ?REWARD_DELAY) ],
    ok.

block_difficulty(Config) ->
    lists:foreach(
        fun(_) ->
            {ok, [KB]} = produce_cc_blocks(Config, 1),
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
    produce_cc_blocks(Config, 1),
    {ok, KB} = wait_same_top([?NODE2, ?NODE3]),
    0 = aec_blocks:difficulty(KB),
    TopHeader1 = rpc(?NODE3, aec_chain, top_header, []),
    ct:log("Lazy header: ~p", [TopHeader1]),
    TopHeader1 = rpc(?NODE2, aec_chain, top_header, []),
    NetworkId = ?config(network_id, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)} ],
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    produce_cc_blocks(Config, 1),
    {ok, _} = wait_same_top([?NODE1, ?NODE3]),
    timer:sleep(2000), %% Give NODE1 a moment to finalize sync and post commitments
    produce_cc_blocks(Config, 1),
    {ok, _KB1} = wait_same_top([ Node || {Node, _, _} <- ?config(nodes, Config)]),
    {ok, _} = produce_cc_blocks(Config, 10),
    {ok, _KB2} = wait_same_top([ Node || {Node, _, _} <- ?config(nodes, Config)]),
    ok.

first_leader_next_epoch(Config) ->
    [{Node, _, _} | _] = ?config(nodes, Config),
    StartHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last, epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, [StartHeight]),
    ct:log("Checking leader for first block next epoch ~p (height ~p)", [Epoch+1, Last+1]),
    ?assertMatch({ok, _}, rpc(Node, aec_consensus_hc, leader_for_height, [Last + 1])).


%% Demonstrate that child chain start signalling epoch length adjustment upward
%% When parent blocks are produced too slowly, we need to lengthen child epoch
epochs_with_slow_parent(Config) ->
    ct:log("Parent start height = ~p", [?config(parent_start_height, Config)]),
    %% ensure start at a new epoch boundary
    StartHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{last := Last}} = rpc(?NODE1, aec_chain_hc, epoch_info, [StartHeight]),
    BlocksLeftToBoundary = Last - StartHeight,
    ct:log("Starting at CC height ~p: producing ~p cc blocks", [StartHeight, BlocksLeftToBoundary]),
    %% some block production including parent blocks
    produce_cc_blocks(Config, BlocksLeftToBoundary),

    ParentHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("Child continues while parent stuck at: ~p", [ParentHeight]),
    ParentEpoch = (ParentHeight - ?config(parent_start_height, Config) +
                      (?PARENT_EPOCH_LENGTH - 1)) div ?PARENT_EPOCH_LENGTH,
    ChildEpoch = rpc(?NODE1, aec_chain, top_height, []) div ?CHILD_EPOCH_LENGTH,
    ct:log("Child epoch ~p while parent epoch ~p (parent should be in next epoch)", [ChildEpoch, ParentEpoch]),
    ?assertEqual(1, ParentEpoch - ChildEpoch),

    Resilience = 2, %% Child can cope with missing Resilience epochs in parent chain
    %% Produce no parent block in the next Resilience child epochs
    %% the child chain should get to a halt or
    %% at least one should be able to observe signalling that the length should be adjusted upward
    {ok, _} = produce_cc_blocks(Config, ?CHILD_EPOCH_LENGTH*Resilience, []),

    ct:log("Mined almost ~p additional child epochs without parent progress", [Resilience]),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ?assertEqual(ParentHeight, ParentTopHeight),
    ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{epoch := EndEpoch} = EpochInfo} = rpc(?NODE1, aec_chain_hc, epoch_info, [ChildTopHeight]),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight, ChildTopHeight, EndEpoch ]),

    %% Here we should have observed some signalling for increased child epoch length

    %% Parent hash grabbed in last block child epoch, so here we can start, but not finish next epoch
    {ok, _} = produce_cc_blocks(Config, maps:get(length, EpochInfo) - 1, []),
    ?assertException(error, timeout_waiting_for_block, produce_cc_blocks(Config, 1, [])),

    ?assertEqual([{ok, (N-1) * ?CHILD_EPOCH_LENGTH + 1} || N <- lists:seq(1, EndEpoch)],
                 [rpc(?NODE1, aec_chain_hc, epoch_start_height, [N]) || N <- lists:seq(1, EndEpoch)]),
    ok.

%% Demonstrate that child chain start signalling epoch length adjustment downward
%% When parent blocks are produced too quickly, we need to shorten child epoch
epochs_with_fast_parent(Config) ->
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{epoch := ChildEpoch}} = rpc(?NODE1, aec_chain_hc, epoch_info, []),

    %% Quickly produce parent blocks to be in sync again
    ParentBlocksNeeded =
        ChildEpoch * ?PARENT_EPOCH_LENGTH + ?config(parent_start_height, Config) + ?PARENT_FINALITY - ParentTopHeight,

    %% Produce ?PARENT_EPOCH_LENGTH parent blocks quickly (very artificial)
    {ok, _} = produce_cc_blocks(Config, 1, [{ChildTopHeight + 1, ParentBlocksNeeded}]),
    %% and finish a child epoch
    %% ensure start at a new epoch boundary
    StartHeight = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{last := Last, length := Len} = EpochInfo1} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    ct:log("Info ~p", [EpochInfo1]),
    BlocksLeftToBoundary = Last - StartHeight,
    %% some block production including parent blocks
    {ok, _} = produce_cc_blocks(Config, BlocksLeftToBoundary),

    %% Produce twice as many parent blocks as needed in an epoch
    Height0 = rpc(?NODE1, aec_chain, top_height, []),
    ParentTopHeight0 = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, _} = produce_cc_blocks(Config, ?CHILD_EPOCH_LENGTH,
                                spread(2*?PARENT_EPOCH_LENGTH, Height0,
                                       [ {CH, 0} || CH <- lists:seq(Height0 + 1, Height0 + Len)])),

    ParentTopHeight1 = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    Height1 = rpc(?NODE1, aec_chain, top_height, []),
    {ok, EpochInfo2} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight1, Height1, EpochInfo2 ]),
    ?assertEqual(2*?PARENT_EPOCH_LENGTH, ParentTopHeight1 - ParentTopHeight0),

    {ok, _} = produce_cc_blocks(Config, ?CHILD_EPOCH_LENGTH,
                                spread(2*?PARENT_EPOCH_LENGTH, Height1,
                                       [ {CH, 0} || CH <- lists:seq(Height1 + 1, Height1 + Len)])),

    %% Here we should be able to observe signalling that epoch should be shorter
    ok.

%%%=============================================================================
%%% Pinning
%%%=============================================================================

get_pin(Config) ->
    [{Node, _, _} | _] = ?config(nodes, Config),
    %% Verify that endpoint is available
    {ok, IsChildChain} = rpc(Node, aeu_env, find_config,
                             [[<<"http">>, <<"endpoints">>, <<"hyperchain">>], [user_config, schema_default]]),
    ?assert(IsChildChain),

    %% Mine one block and derive which epoch we are in
    {ok, _} = produce_cc_blocks(Config, 1),
    {ok, #{epoch := Epoch} = EpochInfo1} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% note: the pins are for the last block in previous epoch
    {ok, 200, Repl1} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/pin-tx", []),
    #{<<"epoch">> := PrevEpoch,
      <<"height">> := Height1,
      <<"block_hash">> := BH1,
      <<"parent_payload">> := Payload} = Repl1,
    {ok, BH1Dec} = aeser_api_encoder:safe_decode(key_block_hash, BH1),
    ?assertEqual({epoch, Epoch - 1}, {epoch, PrevEpoch}),
    ?assertEqual(maps:get(first, EpochInfo1) - 1, Height1),
    {ok, IBH1} = rpc(?NODE1, aec_chain_state, get_key_block_hash_at_height, [Height1]),
    ?assertEqual(BH1Dec, IBH1),

    %% Verify that decoding function works on encoded payload:
    ?assertEqual(#{epoch => PrevEpoch, height => Height1, block_hash => BH1Dec},
                 rpc(Node, aec_pinning_agent, decode_pin_payload, [Payload])),

    %% produce some more child blocks if we stay in same epoch, then pins should be the same
    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, 200, Repl2} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/pin-tx", []),
    {ok, EpochInfo2} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    %% Get response from being in next Epoch
    Repl3 =
        if EpochInfo1 == EpochInfo2 ->
             ?assertEqual(Repl1, Repl2),
             {ok, _} = produce_cc_blocks(Config, maps:get(length, EpochInfo2) - 1),
             {ok, 200, Repl} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/pin-tx", []),
             Repl;
           true -> Repl2
        end,
    %% Verfify for the next epoch as well
    #{<<"epoch">> := NextEpoch, <<"height">> := Height2, <<"block_hash">> := BH2} = Repl3,
    {ok, BH2Dec} = aeser_api_encoder:safe_decode(key_block_hash, BH2),
    %% Now the epoch we started with is the one we take the pin from
    ?assertEqual({epoch, Epoch}, {epoch, NextEpoch}),
    ?assertEqual(maps:get(last, EpochInfo1), Height2),
    {ok, IBH2} = rpc(?NODE1, aec_chain_state, get_key_block_hash_at_height, [Height2]),
    ?assertEqual(BH2Dec, IBH2),
    ok.

post_pin_to_pc(Config) ->
    [{Node, _, _} | _] = ?config(nodes, Config),

    %% Get to first block in new epoch
    Height1 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last1}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, _} = produce_cc_blocks(Config, Last1 - Height1 + 1),
    {ok, Pin} = rpc(Node, aec_pinning_agent, get_pinning_data, []),
    PinPayloadBin = rpc(Node, aec_pinning_agent, encode_pin_payload, [Pin]),
    {ok, _} = produce_cc_blocks(Config, 5),

    DwightPub = pubkey(?DWIGHT), % PC chain account
    DwightEnc = aeser_api_encoder:encode(account_pubkey, DwightPub),
    ParentNodeSpec = #{scheme => "http", host => "127.0.0.1", port => aecore_suite_utils:external_api_port(?PARENT_CHAIN_NODE)},
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % no pending transactions
    PinTx = aec_pinning_agent:create_pin_tx(ParentNodeSpec, DwightEnc, DwightPub, 1, 30000 * ?DEFAULT_GAS_PRICE, PinPayloadBin),
    SignedPinTx = sign_tx(PinTx, privkey(?DWIGHT),?PARENT_CHAIN_NETWORK_ID),
    {ok, #{<<"tx_hash">> := TxHash}} = aec_pinning_agent:post_pin_tx(SignedPinTx, ParentNodeSpec),
    {ok, [_]} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % one transaction pending now.
    {ok, _} = produce_cc_blocks(Config, 5),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted

    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, []),

    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),

    NetworkId = ?config(network_id, Config), % TODO not 100% sure about this one...
    Nonce = next_nonce(Node, pubkey(?ALICE)),
    Params = #{ sender_id    => aeser_id:create(account, pubkey(?ALICE)),
                recipient_id => aeser_id:create(account, LastLeader),
                amount       => 1,
                fee          => 30000 * ?DEFAULT_GAS_PRICE,
                nonce        => Nonce,
                payload      => TxHash},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, privkey(?ALICE), NetworkId),
    ok = rpc(Node, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, [_]} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool
    {ok, _} = produce_cc_blocks(Config, 1),
    CH = rpc(Node, aec_chain, top_height, []),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted
    DistToBeforeLast = Last - CH - 1,
    {ok, _} = produce_cc_blocks(Config, DistToBeforeLast), % produce blocks until last
    BL = Last - 1,
    BL = rpc(Node, aec_chain, top_height, []), % we're producing in last black
    ok.

%% A wallet posting a pin transaction by only using HTTP API towards Child and Parent
wallet_post_pin_to_pc(Config) ->
    [{Node, _, _} | _] = ?config(nodes, Config),

    %% Progress to first block of next epoch
    Height1 = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{last := Last1, length := Len, epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, Bs} = produce_cc_blocks(Config, Last1 - Height1 + 1),
    HashLastInEpoch = aec_blocks:prev_hash(lists:last(Bs)),
    ct:log("Block last epoch: ~p", [aeser_api_encoder:encode(key_block_hash, HashLastInEpoch)]),

    DwightPub = pubkey(?DWIGHT),
    DwightEnc = aeser_api_encoder:encode(account_pubkey, DwightPub),
    %% Get the block hash of the last block of previous epoch wrapped in a specified payload
    {ok, 200, #{<<"parent_payload">> := Payload,
                <<"epoch">> := E, <<"height">> := H,
                <<"block_hash">> := BH,
                <<"last_leader">> := LastLeader}} =
        aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/pin-tx", []),
    ?assertEqual(E, Epoch),
    ?assertEqual(H, Last1),
    ?assertEqual(BH, aeser_api_encoder:encode(key_block_hash, HashLastInEpoch)),

    %% The wallet talks to "its own version" of the parent chain
    %% Here typically the only node
    ParentHost = external_address(?PARENT_CHAIN_NODE),
    ct:log("Parent address ~p", [ParentHost]),
    {ok, 200, DwightInfo} = aecore_suite_utils:http_request(ParentHost, get, <<"accounts/", DwightEnc/binary>>, []),
    Nonce = maps:get(<<"nonce">>, DwightInfo) + 1,
    {ok, PinTx} = create_ae_spend_tx(DwightPub, DwightPub, Nonce, Payload),
    ct:log("Unsigned Spend on parent chain ~p", [PinTx]),

    SignedPinTx = sign_tx(PinTx, privkey(?DWIGHT), ?PARENT_CHAIN_NETWORK_ID),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedPinTx)),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = aecore_suite_utils:http_request(ParentHost, post, <<"transactions">>, #{tx => Transaction}),
    {ok, [_]} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % one transaction pending now.
    {ok, _} = produce_cc_blocks(Config, Len div 2),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted

    %% Don't wait and check for the height of acceptance, because due to parent fork micro forks,
    %% this may change in a while... the last leader will do the work needed on the hashes
    %% it receives

    %% Now just inform the last leader of this epoch about the transaction hash
    %% via a spend on child chain... the leader will have machinery to pick up tx hash
    %% and to find out at which parent height the hash is accepted at
    ProofHash = list_to_binary("PIN"++TxHash),
    {_, LeaderPubkey} = aeser_api_encoder:decode(LastLeader),
    NonceAlice = next_nonce(Node, pubkey(?ALICE)),
    Params = #{ sender_id    => aeser_id:create(account, pubkey(?ALICE)),
                recipient_id => aeser_id:create(account, LeaderPubkey),
                amount       => 1,
                fee          => 30000 * ?DEFAULT_GAS_PRICE,
                nonce        => NonceAlice,
                payload      => ProofHash},
    ct:log("Preparing a spend tx for child chain: ~p", [Params]),
    {ok, ProofTx} = aec_spend_tx:new(Params),
    SignedProofTx = sign_tx(ProofTx, privkey(?ALICE), ?config(network_id, Config)),
    ProofTransaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedProofTx)),
    {ok, 200, _} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), post, <<"transactions">>, #{tx => ProofTransaction}),

    {ok, [_]} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool
    {ok, _} = produce_cc_blocks(Config, 1),
    {ok, []} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool

    Height2 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := CollectHeight}} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    %% mine to CollectHeight and TODO: see that indeed the proof has been used
    {ok, _} = produce_cc_blocks(Config, CollectHeight - Height2),
    ok.


%%% --------- helper functions

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
    seed_account(?NODE1, RecpipientPubkey, Amount, NetworkId).

seed_account(Node, RecipientPubkey, Amount, NetworkId) ->
    NodeName = aecore_suite_utils:node_name(Node),
    {PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(Node),
    Nonce = next_nonce(Node, PatronPub),
    Params =
        #{sender_id    => aeser_id:create(account, PatronPub),
          recipient_id => aeser_id:create(account, RecipientPubkey),
          amount       => Amount,
          fee          => 30000 * ?DEFAULT_GAS_PRICE,
          nonce        => Nonce,
          payload      => <<>>},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, PatronPriv, NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, SignedTx}.

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
    do_contract_call(ContractPubkey, src(?MAIN_STAKING_CONTRACT, Config), Fun, Args, OriginWho, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            current_added_staking_power -> {"added_stake", []}
        end,
    ContractPubkey = ?config(election_contract, Config),
    do_contract_call(ContractPubkey, src(?HC_CONTRACT, Config), Fun, Args, OriginWho, TopHash).

do_contract_call(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    F = fun() -> do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) end,
    {T, Res} = timer:tc(F),
    ct:log("Calling contract took ~.2f ms", [T / 1000]),
    Res.

do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    Tx = contract_call(CtPubkey, CtSrc, Fun, Args, 0, pubkey(Who)),
    {ok, Call} = dry_run(TopHash, Tx),
    decode_consensus_result(Call, Fun, CtSrc).

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

create_ae_spend_tx(SenderId, RecipientId, Nonce, Payload) ->
    Params = #{sender_id => aeser_id:create(account, SenderId),
               recipient_id => aeser_id:create(account, RecipientId),
               amount => 1,
               nonce => Nonce,
               fee => 40000 * ?DEFAULT_GAS_PRICE,
               payload => Payload},
    ct:log("Preparing a spend tx: ~p", [Params]),
    aec_spend_tx:new(Params).

external_address(Node) ->
    {ok, Port} = rpc(Node, aeu_env, user_config_or_env,
                     [[<<"http">>, <<"external">>, <<"port">>], aehttp, [external, port]]),
   "http://127.0.0.1:" ++ integer_to_list(Port).


decode_consensus_result(Call, Fun, Src) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    Res = aect_test_utils:decode_call_result(Src, Fun, ReturnType, ReturnValue),
    {ReturnType, Res}.

calc_rewards(RewardForHeight) ->
    %% we distribute rewards for the previous
    {ok, #{key_block := PrevKB,
           micro_blocks := MBs}}
        = rpc(?NODE1, aec_chain, get_generation_by_height,
              [RewardForHeight, backward]),
    PrevGenProtocol = aec_blocks:version(PrevKB),
    Txs = lists:flatten(
            lists:map(
                fun(MB) -> aec_blocks:txs(MB) end,
                MBs)),
    ct:log("Txs: ~p", [Txs]),
    {_, KeyReward} = key_reward_provided(RewardForHeight),
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

src(ContractName, Config) ->
    Srcs = ?config(contract_src, Config),
    maps:get(ContractName, Srcs).

build_json_files(ElectionContract, NodeConfig, CTConfig) ->
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
        = contract_create_spec("StakingValidator", src("StakingValidator", CTConfig),
                                [EncodePub(Pubkey), UnstakeDelay], 0, 1, Pubkey),
    {ok, ValidatorPoolAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                              StakingValidatorContract),
    %% assert assumption
    ValidatorPoolAddress = validator_pool_contract_address(),
    MSSrc = src(?MAIN_STAKING_CONTRACT, CTConfig),
    #{ <<"pubkey">> := StakingContractPubkey
        , <<"owner_pubkey">> := ContractOwner } = SC
        = contract_create_spec(?MAIN_STAKING_CONTRACT, MSSrc,
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
        = contract_create_spec(ElectionContract, src(ElectionContract, CTConfig),
                               [binary_to_list(StakingContractPubkey)], 0, 3, Pubkey),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                          ElectionContractPubkey),
    %% assert assumption
    ElectionAddress = election_contract_address(),
    {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                StakingContractPubkey),
    Call1 =
        contract_call_spec(SCId, MSSrc,
                           "new_validator", [],
                           ?INITIAL_STAKE, pubkey(?ALICE), 1),
    Call2 =
        contract_call_spec(SCId, MSSrc,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?BOB), 1),
    Call3 =
        contract_call_spec(SCId, MSSrc,
                            "new_validator", [],
                            ?INITIAL_STAKE, pubkey(?LISA), 1),
    Call4  =
        contract_call_spec(SCId, MSSrc,
                            "set_online", [], 0, pubkey(?ALICE), 2),
    Call5  =
        contract_call_spec(SCId, MSSrc,
                            "set_online", [], 0, pubkey(?BOB), 2),
    Call6 =
        contract_call_spec(SCId, MSSrc,
                            "set_online", [], 0, pubkey(?LISA), 2),
    Call7 =
        contract_call_spec(SCId, MSSrc,
                            "set_validator_name", ["\"Alice\""], 0, pubkey(?ALICE), 3),
    Call8 =
        contract_call_spec(SCId, MSSrc,
                            "set_validator_name", ["\"Bob\""], 0, pubkey(?BOB), 3),
    Call9 =
        contract_call_spec(SCId, MSSrc,
                            "set_validator_name", ["\"Lisa\""], 0, pubkey(?LISA), 3),
    Call10 =
        contract_call_spec(SCId, MSSrc,
                            "set_validator_description",
                            ["\"Alice is a really awesome validator and she had set a description of her great service to the work.\""], 0,
                            pubkey(?ALICE), 4),
    Call11 =
        contract_call_spec(SCId, MSSrc,
                            "set_validator_avatar_url",
                            ["\"https://aeternity.com/images/aeternity-logo.svg\""], 0,
                            pubkey(?ALICE), 5),

    Call12 =
        contract_call_spec(ElectionAddress, src(ElectionContract, CTConfig),
                           "init_epochs",
                           [integer_to_list(?CHILD_EPOCH_LENGTH)], 0, ?OWNER_PUBKEY, 4),
    %% create a BRI validator in the contract so they can receive
    %% rewards as well
    %% TODO: discuss how we want to tackle this:
    %%  A) require the BRI account to be validator
    %%  B) allow pending stake in the contract that is not allocated
    %%  yet
    %%  C) something else
    %% Call12 =
    %%     contract_call_spec(SCId, MSSrc,
    %%                         "new_validator", [],
    %%                         ?INITIAL_STAKE, BRIPub, 1),
    %% Call13 =
    %%     contract_call_spec(SCId, MSSrc,
    %%                         "set_validator_description",
    %%                         ["\"This validator is offline. She can never become a leader. She has no name set. She is receiving the BRI rewards\""],
    %%                         0, BRIPub, 2),
    %% keep the BRI offline
    AllCalls =  [Call1, Call2, Call3, Call4, Call5, Call6,
		 Call7, Call8, Call9, Call10, Call11, Call12],
    ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
    #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"contracts_file">> := ContractsFileName,
                                                              <<"accounts_file">> := AccountsFileName}}}} = NodeConfig,
    aecore_suite_utils:create_seed_file(ContractsFileName,
        #{<<"contracts">> => [C0, SC, EC], <<"calls">> => AllCalls}),
    aecore_suite_utils:create_seed_file(AccountsFileName,
        #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000
         }),
    ok.

node_config(Node, CTConfig, PotentialStakers, ReceiveAddress, ProducingCommitments) ->
    NetworkId = ?config(network_id, CTConfig),
    GenesisStartTime = ?config(genesis_start_time, CTConfig),
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
                    #{  <<"start_height">> => ?config(parent_start_height, CTConfig),
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
        <<"http">> => #{<<"endpoints">> => #{<<"hyperchain">> => true}},
        <<"mining">> =>
            #{<<"micro_block_cycle">> => 1,
            <<"autostart">> => false,
            %%<<"autostart">> => ProducingCommitments,
            <<"beneficiary_reward_delay">> => ?REWARD_DELAY
        }}.  %% this relies on certain nonce numbers

validator_pool_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

%% Increase the child chain with a number of key blocks
%% Automatically add key blocks on parent chain and
%% if there are Txs, put them in a micro block
produce_cc_blocks(Config, BlocksCnt) ->
    [{Node, _, _} | _] = ?config(nodes, Config),
    TopHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := Epoch, first := First, last := Last, length := L} = Info} =
        rpc(?NODE1, aec_chain_hc, epoch_info, [TopHeight]),
    ct:log("EpochInfo ~p", [Info]),
    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (Last - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div L),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("P@~p C@~p for next ~p child blocks", [ParentTopHeight, TopHeight,  BlocksCnt]),
    %% Spread parent blocks over BlocksCnt
    ParentProduce =
        lists:append([ spread(?PARENT_EPOCH_LENGTH, TopHeight,
                              [ {CH, 0} || CH <- lists:seq(First + E * L, Last + E * L)]) ||
                       E <- lists:seq(0, ScheduleUpto - Epoch) ]),
    %% Last parameter steers where in Child epoch parent block is produced
    produce_cc_blocks(Config, BlocksCnt, ParentProduce).

produce_cc_blocks(Config, BlocksCnt, ParentProduce) ->
    [{Node1, _, _} | _] = ?config(nodes, Config),
    %% The previous production ended with wait_same_top, so asking first node is sufficient
    TopHeight = rpc(Node1, aec_chain, top_height, []),
    %% assert that the parent chain is not mining
    ?assertEqual(stopped, rpc:call(?PARENT_CHAIN_NODE_NAME, aec_conductor, get_mining_state, [])),
    ct:log("parent produce ~p", [ParentProduce]),
    NewTopHeight = produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce),
    wait_same_top([ Node || {Node, _, _} <- ?config(nodes, Config)]),
    get_generations(Node1, TopHeight + 1, NewTopHeight).

%% It seems we automatically produce child chain blocks in the background
produce_to_cc_height(Config, TopHeight, GoalHeight, ParentProduce) ->
    NodeNames = [ Name || {_, Name, _} <- ?config(nodes, Config) ],
    BlocksNeeded = GoalHeight - TopHeight,
    case  BlocksNeeded > 0 of
        false ->
            TopHeight;
        true ->
            NewParentProduce =
                case ParentProduce of
                    [{CH, PBs} | PRest ]  when CH == TopHeight+1 ->
                        mine_key_blocks(?PARENT_CHAIN_NODE_NAME, PBs),
                        PRest;
                    PP -> PP
                end,
            KeyBlock =
                case rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]) of
                    {ok, []} ->
                         {ok, [{N, Block}]} = mine_cc_blocks(NodeNames, 1),
                         ct:log("CC ~p mined block: ~p", [N, Block]),
                         Block;
                    {ok, _Txs} ->
                         {ok, [{N1, MB}, {N2, KB}]} = mine_cc_blocks(NodeNames, 2),
                         ?assertEqual(key, aec_blocks:type(KB)),
                         ?assertEqual(micro, aec_blocks:type(MB)),
                         ct:log("CC ~p mined micro block: ~p", [N1, MB]),
                         ct:log("CC ~p mined key block:   ~p", [N2, KB]),
                         KB
                end,
            Producer = get_block_producer_name(?config(staker_names, Config), KeyBlock),
            ct:log("~p produced CC block at height ~p", [Producer, aec_blocks:height(KeyBlock)]),
            produce_to_cc_height(Config, TopHeight + 1, GoalHeight, NewParentProduce)
      end.

mine_cc_blocks(NodeNames, N) ->
    aecore_suite_utils:hc_mine_blocks(NodeNames, N).

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case rpc(Node, aec_chain, get_generation_by_height, [Height, backward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error -> error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)),
    {ok, lists:reverse(ReversedBlocks)}.

mine_key_blocks(ParentNodeName, NumParentBlocks) ->
    {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
    {ok, KBs} = aecore_suite_utils:mine_key_blocks(ParentNodeName, NumParentBlocks),
    ct:log("Parent block mined ~p ~p number: ~p", [KBs, ParentNodeName, NumParentBlocks]),
    {ok, KBs}.

%get_block_producer_name(Parties, Node, Height) ->
%    Producer = get_block_producer(Node, Height),
%    case lists:keyfind(Producer, 1, Parties) of
%        false -> Producer;
%        {_, _, Name} -> Name
%    end.

get_block_producer_name(Parties, Block) ->
    Producer = aec_blocks:miner(Block),
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

key_reward_provided() ->
    TopHeight = rpc(?NODE1, aec_chain, top_height, []),
    RewardHeight = TopHeight - ?REWARD_DELAY,
    key_reward_provided(RewardHeight).

key_reward_provided(RewardHeight) ->
  {get_block_producer(?NODE1, RewardHeight),
   rpc(?NODE1, aec_governance, block_mine_reward, [RewardHeight])}.

create_stub(ContractFile) ->
    create_stub(ContractFile, []).

create_stub(ContractFile, Opts) ->
    {ok, Enc}  = aeso_aci:contract_interface(json, ContractFile, Opts ++ [{no_code, true}]),
    {ok, Stub} = aeso_aci:render_aci_json(Enc),
    binary_to_list(Stub).

spread(_, _, []) ->
    [];
spread(0, TopHeight, Spread) ->
    [ {CH, N} || {CH, N} <- Spread, N /= 0, CH > TopHeight ];
%spread(N, TopHeight, [{CH, K} | Spread]) when length(Spread) < N ->
%    %% Take speed first (not realistic), then fill rest
%    spread(0, TopHeight, [{CH, K + N - length(Spread)} | [ {CH2, X+1} || {CH2, X} <- Spread]]);
spread(N, TopHeight, Spread) when N rem 2 == 0 ->
    {Left, Right} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ spread(N div 2, TopHeight, Right);
spread(N, TopHeight, Spread) when N rem 2 == 1 ->
    {Left, [{Middle, K} | Right]} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ [{Middle, K+1} || Middle > TopHeight] ++ spread(N div 2, TopHeight, Right).

