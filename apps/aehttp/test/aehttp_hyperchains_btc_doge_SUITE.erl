-module(aehttp_hyperchains_btc_doge_SUITE).

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
         check_default_pin/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(STAKING_VALIDATOR_CONTRACT, "StakingValidator").
-define(MAIN_STAKING_CONTRACT, "MainStaking").
-define(HC_CONTRACT, "HCElection").
-define(CONSENSUS, hc).
-define(CHILD_EPOCH_LENGTH, 20).
-define(CHILD_BLOCK_TIME, 4000).
-define(CHILD_BLOCK_PRODUCTION_TIME, 1500).
-define(PARENT_EPOCH_LENGTH, 4).
-define(PARENT_FINALITY, 2).
-define(REWARD_DELAY, 2).
-define(BLOCK_REWARD, 100000000000000000000).
-define(FEE_REWARD, 30000 * ?DEFAULT_GAS_PRICE).

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

-define(BOB_SIGN, {
    <<211,171,126,224,112,125,255,130,213,51,158,2,198,188,30,
      130,227,205,11,191,122,121,237,227,129,67,65,170,117,35,
      131,190>>,
    <<245,228,166,6,138,54,196,135,180,68,180,161,153,228,97,
      127,100,77,122,20,169,108,224,29,51,209,182,55,106,223,
      24,219,211,171,126,224,112,125,255,130,213,51,158,2,
      198,188,30,130,227,205,11,191,122,121,237,227,129,67,
      65,170,117,35,131,190>>,
    "Bob"}).
%% ak_2cDpmgCXN4nTu2hYsa5KEVTgPJo2cu2SreCDPhjh6VuXH37Z7Y

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

all() -> [{group, doge}].

% groups() ->
%     [
%       {hc, [sequence],
%           [ start_two_child_nodes
%           , produce_first_epoch
%           , verify_rewards
%           , spend_txs
%           , simple_withdraw
%           , correct_leader_in_micro_block
%           , sync_third_node
%           , produce_some_epochs
%           , respect_schedule
%           , entropy_impact_schedule
%           , check_blocktime
%           , get_contract_pubkeys
%           , sanity_check_vote_tx
%           ]}
%     , {epochs, [sequence],
%           [ start_two_child_nodes
%           , first_leader_next_epoch
%           , epochs_with_slow_parent
%           , epochs_with_fast_parent ]}
%     , {pinning, [sequence],
%           [ start_two_child_nodes,
%             produce_first_epoch,
%             get_pin,
%             wallet_post_pin_to_pc,
%             post_pin_to_pc,
%             last_leader_validates_pin_and_post_to_contract]}
%     , {default_pin, [sequence],
%           [ start_two_child_nodes,
%             produce_first_epoch,
%             check_default_pin]}
%     ].

groups() ->
    [
        {doge, [sequence],
            [ start_two_child_nodes
            , produce_first_epoch
            , produce_some_epochs
            %, check_default_pin
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
            % GenesisProtocol = 1,
            % {ok, AccountFileName} =
            %     aecore_suite_utils:hard_fork_filename(?PARENT_CHAIN_NODE, Config1, integer_to_list(GenesisProtocol), "accounts_test.json"),
            % GenesisProtocolBin = integer_to_binary(GenesisProtocol),
            % ParentCfg =
            %     #{  <<"chain">> =>
            %             #{  <<"persist">> => false,
            %                 <<"hard_forks">> =>
            %                     #{  GenesisProtocolBin => #{<<"height">> => 0, <<"accounts_file">> => AccountFileName},
            %                         integer_to_binary(?CERES_PROTOCOL_VSN) => #{<<"height">> => 1}
            %                     },
            %                 <<"consensus">> =>
            %                     #{<<"0">> => #{<<"type">> => <<"ct_tests">>}}
            %              },
            %         <<"fork_management">> =>
            %             #{<<"network_id">> => ?PARENT_CHAIN_NETWORK_ID},
            %         <<"mempool">> => #{<<"nonce_offset">> => 200},
            %         <<"mining">> =>
            %             #{<<"micro_block_cycle">> => 1,
            %               <<"expected_mine_rate">> => 2000,
            %               <<"autostart">> => false,
            %               <<"beneficiary_reward_delay">> => ?REWARD_DELAY }
            %     },
            % aecore_suite_utils:make_multi(Config1, [?PARENT_CHAIN_NODE]),
            % aecore_suite_utils:create_config(?PARENT_CHAIN_NODE, Config1, ParentCfg, []),
            % {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(?PARENT_CHAIN_NODE),
            % ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            % aecore_suite_utils:create_seed_file(AccountFileName,
            %     #{  ParentPatronPubEnc => 100000000000000000000000000000000000000000000000000000000000000000000000
            %         , encoded_pubkey(?DWIGHT) => 2100000000000000000000000000
            %         , encoded_pubkey(?EDWIN) => 3100000000000000000000000000
            %     }),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            CtSrcMap = maps:from_list([{C, create_stub(C)}
                                       || C <- [?MAIN_STAKING_CONTRACT, ?STAKING_VALIDATOR_CONTRACT, ?HC_CONTRACT]]),
            [{staking_contract, StakingContract}, {election_contract, ElectionContract}, {contract_src, CtSrcMap} | Config1]
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

init_per_group(Group, ConfigPre) ->
    Config0 = [ {default_pinning_behavior, false} | ConfigPre ],
        % case Group of
        %     default_pin -> [ {default_pinning_behavior, true} | ConfigPre ];
        %     _ -> [ {default_pinning_behavior, false} | ConfigPre ]
        % end,
    VM = fate,
    NetworkId = <<"hc">>,
    GenesisStartTime = aeu_time:now_in_msecs(),
    Config = [{network_id, NetworkId}, {genesis_start_time, GenesisStartTime},
              {consensus, ?CONSENSUS} |
              aect_test_utils:init_per_group(VM, Config0)],

    % aecore_suite_utils:start_node(?PARENT_CHAIN_NODE, Config),
    % aecore_suite_utils:connect(?PARENT_CHAIN_NODE_NAME, []),
    % ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    % StartHeight = max(ParentTopHeight, ?PARENT_EPOCH_LENGTH),
    % ct:log("Parent chain top height ~p start at ~p", [ParentTopHeight, StartHeight]),
    % %%TODO mine less than necessary parent height and test chain starts when height reached
    % {ok, _} = mine_key_blocks(
    %         ?PARENT_CHAIN_NODE_NAME,
    %         (StartHeight - ParentTopHeight) + ?PARENT_FINALITY),
    [ {staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, 6778619} | Config].

child_node_config(Node, Stakeholders, Pinners, CTConfig) ->
    ReceiveAddress = encoded_pubkey(?FORD),
    NodeConfig = node_config(Node, CTConfig, Stakeholders, Pinners, ReceiveAddress),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

end_per_group(_Group, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [ aecore_suite_utils:stop_node(Node, Config1)
      || {Node, _, _, _} <- proplists:get_value(nodes, Config1, []) ],

    aecore_suite_utils:assert_no_errors_in_logs(Config1, ["{handled_abort,parent_chain_not_synced}"]),

    Config1.

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [{nodes, [{?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [{?ALICE, ?DWIGHT}, {?LISA, ?EDWIN}]},
                  {?NODE2, ?NODE2_NAME, [?BOB_SIGN], [{?BOB_SIGN, ?EDWIN}]}
                 ]}
         | Config],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    Nodes = ?config(nodes, Config1),
    Config2 = lists:keyreplace(nodes, 1, Config1,
                               {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, [], []}]}),
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
    [{Node1, NodeName1, Stakers1, Pinners1}, {Node2, NodeName2, Stakers2, Pinners2} | _] = ?config(nodes, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))} ],
    child_node_config(Node1, Stakers1, Pinners1, Config),
    aecore_suite_utils:start_node(Node1, Config, Env),
    aecore_suite_utils:connect(NodeName1, []),
    child_node_config(Node2, Stakers2, Pinners2, Config),
    aecore_suite_utils:start_node(Node2, Config, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

produce_first_epoch(Config) ->
    produce_n_epochs(Config, 1).

produce_some_epochs(Config) ->
    produce_n_epochs(Config, 5).

produce_n_epochs(Config, N) ->
    [{Node1, _, _, _}|_] = ?config(nodes, Config),
    %% produce blocks
    {ok, Bs} = produce_cc_blocks(Config, N * ?CHILD_EPOCH_LENGTH),
    %% check producers
    Producers = [ aec_blocks:miner(B) || B <- Bs, aec_blocks:is_key_block(B) ],
    ChildTopHeight = rpc(Node1, aec_chain, top_height, []),
    Leaders = leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    % ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    % {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    % ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.


%% Demonstrate that child chain start signalling epoch length adjustment upward
%% When parent blocks are produced too slowly, we need to lengthen child epoch
epochs_with_slow_parent(Config) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),
    ct:log("Parent start height = ~p", [?config(parent_start_height, Config)]),
    %% ensure start at a new epoch boundary
    StartHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, [StartHeight]),
    BlocksLeftToBoundary = Last - StartHeight,
    ct:log("Starting at CC height ~p: producing ~p cc blocks", [StartHeight, BlocksLeftToBoundary]),
    %% some block production including parent blocks
    produce_cc_blocks(Config, BlocksLeftToBoundary),

    ParentHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("Child continues while parent stuck at: ~p", [ParentHeight]),
    ParentEpoch = (ParentHeight - ?config(parent_start_height, Config) +
                      (?PARENT_EPOCH_LENGTH - 1)) div ?PARENT_EPOCH_LENGTH,
    ChildEpoch = rpc(Node, aec_chain, top_height, []) div ?CHILD_EPOCH_LENGTH,
    ct:log("Child epoch ~p while parent epoch ~p (parent should be in next epoch)", [ChildEpoch, ParentEpoch]),
    ?assertEqual(1, ParentEpoch - ChildEpoch),

    Resilience = 1, %% Child can cope with missing Resilience epochs in parent chain
    %% Produce no parent block in the next Resilience child epochs
    %% the child chain should get to a halt or
    %% at least one should be able to observe signalling that the length should be adjusted upward
    {ok, _} = produce_cc_blocks(Config, ?CHILD_EPOCH_LENGTH*Resilience, []),

    ct:log("Mined almost ~p additional child epochs without parent progress", [Resilience]),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ?assertEqual(ParentHeight, ParentTopHeight),
    ChildTopHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := EndEpoch} = EpochInfo} = rpc(Node, aec_chain_hc, epoch_info, [ChildTopHeight]),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight, ChildTopHeight, EndEpoch]),

    %% Here we should have observed some signalling for increased child epoch length

    %% Parent hash grabbed in last block child epoch, so here we can start, but not finish next epoch
    {ok, _} = produce_cc_blocks(Config, maps:get(length, EpochInfo) - 1, []),
    ?assertException(error, timeout_waiting_for_block, produce_cc_blocks(Config, 1, [])),

    ?assertEqual([{ok, (N-1) * ?CHILD_EPOCH_LENGTH + 1} || N <- lists:seq(1, EndEpoch)],
                 [rpc(Node, aec_chain_hc, epoch_start_height, [N]) || N <- lists:seq(1, EndEpoch)]),
    ok.


post_pin_to_pc(Config) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),

    %% Get to first block in new epoch
    Height1 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last1}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, _} = produce_cc_blocks(Config, Last1 - Height1 + 1),
    {ok, Pin} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    {ok, _} = produce_cc_blocks(Config, 5),

    DwightPub = pubkey(?DWIGHT), % PC chain account
    %DwightEnc = aeser_api_encoder:encode(account_pubkey, DwightPub),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % no pending transactions
    ct:log("DWIGHT: ~p ",[DwightPub]),
    PinTx = rpc(Node, aec_parent_connector, create_pin_tx, [DwightPub, DwightPub, 1, 30000 * ?DEFAULT_GAS_PRICE, Pin]),
    ct:log("PinTX: ~p", [PinTx]),
    SignedPinTx = sign_tx(PinTx, privkey(?DWIGHT),?PARENT_CHAIN_NETWORK_ID),
    EncTxHash = rpc(Node, aec_parent_connector, post_pin_tx, [SignedPinTx]),
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
                payload      => EncTxHash},
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
    [{Node, _, _, _} | _] = ?config(nodes, Config),

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
    {ok, 200, #{<<"tx_hash">> := ProofHash}} = aecore_suite_utils:http_request(ParentHost, post, <<"transactions">>, #{tx => Transaction}),
    {ok, [_]} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % one transaction pending now.
    {ok, _} = produce_cc_blocks(Config, Len div 2),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted

    %% Don't wait and check for the height of acceptance, because due to parent fork micro forks,
    %% this may change in a while... the last leader will do the work needed on the hashes
    %% it receives

    %% Now just inform the last leader of this epoch about the transaction hash
    %% via a spend on child chain... the leader will have machinery to pick up tx hash
    %% and to find out at which parent height the hash is accepted at
    % ProofHash = list_to_binary("PIN"++TxHash), % the hash comes encoded already
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
    {ok, #{last := CollectHeight}} = rpc(Node, aec_chain_hc, epoch_info, []),
    %% mine to CollectHeight and TODO: see that indeed the proof has been used
    {ok, _} = produce_cc_blocks(Config, CollectHeight - Height2),
    ok.

last_leader_validates_pin_and_post_to_contract(Config) ->
    [{Node, NodeName, _, _} | _] = ?config(nodes, Config),
    %% 1. Correct pin is posted in the contract

    #{cur_pin_reward := _Reward} = rpc(Node, aec_chain_hc , pin_reward_info, []),

    %% move into next epoch
    mine_to_next_epoch(Node, Config),
    %% post pin to PC
    {ok, PinningData} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    ct:log("Pinning data ~p", [PinningData]),
    TxHash = pin_to_parent(Node, PinningData, pubkey(?DWIGHT)),
    %% post parent spend tx hash to CC
    {ok, #{epoch  := _Epoch,
           last   := Last,
           length := _Length}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    tx_hash_to_child(Node, TxHash, ?ALICE, LastLeader, Config),
    %% move forward to last block

    mine_to_last_block_in_epoch(Node, Config),
    % produce blocks until last

    aecore_suite_utils:subscribe(NodeName, pin),
    %% TODO test to see that LastLeader actually is leader now?

    %% Find the first spend
    [FirstSpend|_] = rpc(Node, aec_parent_connector, find_spends_to, [LastLeader]),
    ct:log("First Spend: ~p", [FirstSpend]),

    %% call contract with PC pin tx hash
    ok = pin_contract_call_tx(Config, FirstSpend, LastLeader),

    {value, Account} = rpc(?NODE1, aec_chain, get_account, [LastLeader]),
    ct:log("Leader Account: ~p", [Account]),

    LeaderBalance1A = account_balance(LastLeader),
    %% use get_pin_by_tx_hash to get the posted hash back and compare with actual keyblock (to test encoding decoding etc)
    {ok, #{epoch := _PinEpoch, height := PinHeight, block_hash := PinHash}} =
        rpc(Node, aec_parent_connector, get_pin_by_tx_hash, [FirstSpend]),
    ?assertEqual({ok, PinHash}, rpc(Node, aec_chain_state, get_key_block_hash_at_height, [PinHeight])),

    %% move into next epoch - trigger leader validation?
    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, #{info := {pin_accepted, _}}} = wait_for_ps(pin),
    LeaderBalance1B = account_balance(LastLeader),

    ct:log("Account balance for leader was: ~p, is now: ~p", [LeaderBalance1A, LeaderBalance1B]),
    % Any Reasonable way to do this test? Likely a bunch of rewards/fees etc have been awarded, although
    % the above log clearly shows that 4711 (and a bunch more coin) was added.
    % LeaderBalance0 = LeaderBalance1 - 4711,

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 2. No pin is posted

    % to end of (next) epoch
    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % In last generation, but we don't post pin

    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, #{info := {no_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 3. Incorrect pin posted to contract a) bad tx hash

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % post bad hash to contract

    {ok, #{last := Last3}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader3} = rpc(Node, aec_consensus_hc, leader_for_height, [Last3]),
    ok = pin_contract_call_tx(Config, <<"THIS IS A BAD TX HASH">>, LastLeader3),

    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, #{info := {incorrect_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 4. Incorrect hash stored on PC

    {ok, PD4} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    EncTxHash4 = pin_to_parent(Node, PD4#{block_hash := <<"VERYINCORRECTBLOCKHASH">>}, pubkey(?DWIGHT)),
    %% post parent spend tx hash to CC
    {ok, #{last := Last4}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader4} = rpc(Node, aec_consensus_hc, leader_for_height, [Last4]),

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % post bad hash to contract
    LeaderBalance4A = account_balance(LastLeader4),
    ok = pin_contract_call_tx(Config, EncTxHash4, LastLeader4),

    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, #{info := {incorrect_proof_posted}}} = wait_for_ps(pin),

    LeaderBalance4B = account_balance(LastLeader4),

    ct:log("Account balance for leader was: ~p, is now: ~p", [LeaderBalance4A, LeaderBalance4B]),
    % See above for when a reward for pinning actually was given... Same problem here.
    % LeaderBalance4A = LeaderBalance4B, % nothing was rewarded

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 4. Bad height and then bad leader

    {ok, PD5} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    EncTxHash5 = pin_to_parent(Node, PD5, pubkey(?DWIGHT)),

    {ok, #{last := Last5}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader5} = rpc(Node, aec_consensus_hc, leader_for_height, [Last5]),

    {ok, _} = produce_cc_blocks(Config, 1),

    %% at the wrong height
    ok = pin_contract_call_tx(Config, EncTxHash5, LastLeader5),

    {ok, _} = produce_cc_blocks(Config, 1),
    {ok, []} = rpc(Node, aec_tx_pool, peek, [infinity]), % transaction not in pool
    %% check that no pin info was stored.
    undefined = rpc(Node, aec_chain_hc, pin_info, []),

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % post by wrong leader
    NotLeader = hd([pubkey(?ALICE), pubkey(?BOB)] -- [LastLeader5]),
    ok = pin_contract_call_tx(Config, EncTxHash5, NotLeader),

    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, #{info := {no_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    ok.

check_default_pin(Config) ->
    [{Node, NodeName, _, _} | _] = ?config(nodes, Config),

    {ok, _} = produce_cc_blocks(Config, 12),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    ct:log("Last Leader: ~p", [LastLeader]),

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    {ok, _} = produce_cc_blocks(Config, 2),
    %% with current test setup, all validators have a pc account, so pins will always happen(?)
    {ok, #{info := {pin_accepted, _}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% TODO test when not all validators have PC account, but how ensure
    %% that any given validator will be last leader within the run of the test???

    ok.

%%% --------- pinning helpers


wait_for_ps(Event) ->
    receive
        {gproc_ps_event, Event, Info} -> {ok, Info};
        Other -> error({wrong_signal, Other})
    end.

mine_to_last_block_in_epoch(Node, Config) ->
    {ok, #{epoch  := _Epoch,
           first  := _First,
           last   := Last,
           length := _Length}} = rpc(Node, aec_chain_hc, epoch_info, []),
    CH = rpc(Node, aec_chain, top_height, []),
    DistToBeforeLast = Last - CH - 1,
    {ok, _} = produce_cc_blocks(Config, DistToBeforeLast).

bytes_literal(Bin) ->
    [_, _ | PinLit] = binary_to_list(aeu_hex:hexstring_encode(Bin)),
    "#" ++ PinLit.

% PINREFAC
pin_contract_call_tx(Config, PinProof, FromPubKey) ->
    Tx = contract_call(?config(election_contract, Config), src(?HC_CONTRACT, Config),
                       "pin", [bytes_literal(PinProof)], 0, FromPubKey),

    NetworkId = ?config(network_id, Config),
    SignedTx = sign_tx(Tx, privkey(who_by_pubkey(FromPubKey)), NetworkId),
    rpc:call(?NODE1_NAME, aec_tx_pool, push, [SignedTx, tx_received]),
    ok.

% PINREFAC aec_parent_connector??
pin_to_parent(Node, PinningData, AccountPK) ->
    %AccPKEncEnc = aeser_api_encoder:encode(account_pubkey, AccountPK),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % no pending transactions
    PinTx = rpc(Node, aec_parent_connector, create_pin_tx, [AccountPK, AccountPK, 1, 30000 * ?DEFAULT_GAS_PRICE, PinningData]),
    SignedPinTx = sign_tx(PinTx, privkey(?DWIGHT),?PARENT_CHAIN_NETWORK_ID),
    rpc(Node, aec_parent_connector, post_pin_tx, [SignedPinTx]).

% PINREFAC
tx_hash_to_child(Node, EncTxHash, SendAccount, Leader, Config) ->
    NodeName = aecore_suite_utils:node_name(Node),
    NetworkId = ?config(network_id, Config),
    Nonce = next_nonce(Node, pubkey(SendAccount)),
    Params = #{ sender_id    => aeser_id:create(account, pubkey(SendAccount)),
                recipient_id => aeser_id:create(account, Leader),
                amount       => 1,
                fee          => 30000 * ?DEFAULT_GAS_PRICE,
                nonce        => Nonce,
                payload      => EncTxHash},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, privkey(SendAccount), NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    Hash = rpc:call(NodeName, aetx_sign, hash, [SignedTx]),
    Hash.

mine_to_next_epoch(Node, Config) ->
    Height1 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last1, length := _Len}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, Bs} = produce_cc_blocks(Config, Last1 - Height1 + 1),
    ct:log("Block last epoch: ~p", [Bs]).

%%% --------- helper functions

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

who_by_pubkey(Pubkey) ->
    Alice = pubkey(?ALICE),
    Bob = pubkey(?BOB),
    BobSign = pubkey(?BOB_SIGN),
    Lisa = pubkey(?LISA),
    Dwight = pubkey(?DWIGHT),
    Edwin = pubkey(?EDWIN),
    Genesis = ?GENESIS_BENFICIARY,
    case Pubkey of
        Alice -> ?ALICE;
        Bob -> ?BOB;
        BobSign -> ?BOB_SIGN;
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

sign_and_push(NodeName, Tx, Who, NetworkId) ->
    SignedTx = sign_tx(Tx, privkey(Who), NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
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

src(ContractName, Config) ->
    Srcs = ?config(contract_src, Config),
    maps:get(ContractName, Srcs).

build_json_files(ElectionContract, NodeConfig, CTConfig) ->
    Pubkey = ?OWNER_PUBKEY,
    {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),

    %% create staking contract
    MinStakeAmt = integer_to_list(trunc(math:pow(10,18) * 1)), %% 1 AE
    MSSrc = src(?MAIN_STAKING_CONTRACT, CTConfig),
    #{ <<"pubkey">> := StakingContractPubkey
     , <<"owner_pubkey">> := ContractOwner } = SC
        = contract_create_spec(?MAIN_STAKING_CONTRACT, MSSrc, [MinStakeAmt], 0, 1, Pubkey),
    {ok, StakingAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                         StakingContractPubkey),
    %% assert assumption
    StakingAddress = staking_contract_address(),

    %% create election contract
    #{ <<"pubkey">> := ElectionContractPubkey
     , <<"owner_pubkey">> := ContractOwner } = EC
        = contract_create_spec(ElectionContract, src(ElectionContract, CTConfig),
                               [binary_to_list(StakingContractPubkey)], 0, 2, Pubkey),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                          ElectionContractPubkey),
    %% assert assumption
    ElectionAddress = election_contract_address(),
    {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey, StakingContractPubkey),

    APub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?ALICE))),
    Call1 =
        contract_call_spec(SCId, MSSrc, "new_validator", [APub, APub, "true"],
                           ?INITIAL_STAKE, pubkey(?ALICE), 1),

    BPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?BOB))),
    BPubSign = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?BOB_SIGN))),
    Call2 =
        contract_call_spec(SCId, MSSrc, "new_validator", [BPub, BPubSign, "true"],
                           ?INITIAL_STAKE, pubkey(?BOB), 1),

    LPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?LISA))),
    Call3 =
        contract_call_spec(SCId, MSSrc, "new_validator", [LPub, LPub, "true"],
                           ?INITIAL_STAKE, pubkey(?LISA), 1),

    AllCalls =  [Call1, Call2, Call3],
    ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
    #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"contracts_file">> := ContractsFileName,
                                                              <<"accounts_file">> := AccountsFileName}}}} = NodeConfig,
    aecore_suite_utils:create_seed_file(ContractsFileName,
        #{<<"contracts">> => [SC, EC], <<"calls">> => AllCalls}),
    aecore_suite_utils:create_seed_file(AccountsFileName,
        #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?BOB_SIGN) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000
         }),
    ok.

node_config(Node, CTConfig, PotentialStakers, PotentialPinners, ReceiveAddress) ->
    NetworkId = ?config(network_id, CTConfig),
    GenesisStartTime = ?config(genesis_start_time, CTConfig),
    Stakers = lists:map(
                    fun(HCWho) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))), %% TODO: discuss key management
                        #{ <<"hyper_chain_account">> => #{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv} }
                    end,
                    PotentialStakers),
    Pinners = lists:map(
                    fun({Owner, Pinner}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(Pinner))), %% TODO: discuss key management
                        #{ <<"parent_chain_account">> => #{<<"pub">> => encoded_pubkey(Pinner), <<"priv">> => HCPriv, <<"owner">> => encoded_pubkey(Owner)} }
                    end,
                    PotentialPinners),
    ct:log("Stakers: ~p", [Stakers]),
    ct:log("Pinners: ~p", [Pinners]),
    ConsensusType = <<"hyperchain">>,
    Port = 44555,
    SpecificConfig =
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?config(parent_start_height, CTConfig),
                        <<"finality">> => ?PARENT_FINALITY,
                        <<"parent_epoch_length">> => ?PARENT_EPOCH_LENGTH,
                        <<"consensus">> =>
                            #{  <<"type">> => <<"AE2DOGE">>,
                                <<"network_id">> => <<"testnet">>,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 100000,
                                <<"amount">> => 9700
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"cache_size">> => 10,
                                <<"nodes">> => [ iolist_to_binary(io_lib:format("http://test:Pass@127.0.0.1:~p", [Port])) ]
                            }
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"child_epoch_length">> => ?CHILD_EPOCH_LENGTH,
                    <<"child_block_time">> => ?CHILD_BLOCK_TIME,
                    <<"child_block_production_time">> => ?CHILD_BLOCK_PRODUCTION_TIME
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
                                        <<"staking_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                                        <<"contract_owner">> => aeser_api_encoder:encode(account_pubkey,?OWNER_PUBKEY),
                                        <<"expected_key_block_rate">> => 2000,
                                        <<"stakers">> => Stakers,
                                        <<"pinners">> => Pinners,
                                        <<"pinning_reward_value">> => 4711,
                                        <<"fixed_coinbase">> => ?BLOCK_REWARD,
                                        <<"default_pinning_behavior">> => ?config(default_pinning_behavior, CTConfig)},
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

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

%% Increase the child chain with a number of key blocks
%% Automatically add key blocks on parent chain and
%% if there are Txs, put them in a micro block
produce_cc_blocks(Config, BlocksCnt) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),
    TopHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := Epoch, first := First, last := Last, length := L} = Info} =
        rpc(Node, aec_chain_hc, epoch_info, [TopHeight]),
    ct:log("EpochInfo ~p", [Info]),
    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (Last - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div L),
    %ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    %ct:log("P@~p C@~p for next ~p child blocks", [ParentTopHeight, TopHeight,  BlocksCnt]),
    %% Spread parent blocks over BlocksCnt
    ParentProduce =
        lists:append([ spread(?PARENT_EPOCH_LENGTH, TopHeight,
                              [ {CH, 0} || CH <- lists:seq(First + E * L, Last + E * L)]) ||
                       E <- lists:seq(0, ScheduleUpto - Epoch) ]),
    %% Last parameter steers where in Child epoch parent block is produced
    produce_cc_blocks(Config, BlocksCnt, ParentProduce).

produce_cc_blocks(Config, BlocksCnt, ParentProduce) ->
    [{Node1, _, _, _} | _] = ?config(nodes, Config),
    %% The previous production ended with wait_same_top, so asking first node is sufficient
    TopHeight = rpc(Node1, aec_chain, top_height, []),
    %% assert that the parent chain is not mining
    %?assertEqual(stopped, rpc:call(?PARENT_CHAIN_NODE_NAME, aec_conductor, get_mining_state, [])),
    %ct:log("parent produce ~p", [ParentProduce]),
    NewTopHeight = produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce),
    wait_same_top([ Node || {Node, _, _, _} <- ?config(nodes, Config)]),
    get_generations(Node1, TopHeight + 1, NewTopHeight).

%% It seems we automatically produce child chain blocks in the background
produce_to_cc_height(Config, TopHeight, GoalHeight, ParentProduce) ->
    NodeNames = [ Name || {_, Name, _, _} <- ?config(nodes, Config) ],
    BlocksNeeded = GoalHeight - TopHeight,
    case BlocksNeeded > 0 of
        false ->
            TopHeight;
        true ->
            NewParentProduce =
                case ParentProduce of
                    [{CH, PBs} | PRest ] when CH == TopHeight+1 ->
                        mine_key_blocks(?PARENT_CHAIN_NODE_NAME, PBs),
                        PRest;
                    PP -> PP
                end,

            %% TODO: add some assertions when we expect an MB (and not)!
            {ok, _Txs} = rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]),

            %% This will mine 1 key-block (and 0 or 1 micro-blocks)
            {ok, Blocks} = mine_cc_blocks(NodeNames, 1),

            {Node, KeyBlock} = lists:last(Blocks),
            case Blocks of
                [{Node, MB}, _] ->
                    ?assertEqual(micro, aec_blocks:type(MB)),
                    ct:log("CC ~p produced micro-block: ~p", [Node, MB]);
                [_] ->
                    ok
            end,
            ?assertEqual(key, aec_blocks:type(KeyBlock)),
            ct:log("CC ~p produced key-block: ~p", [Node, KeyBlock]),

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
    {ok, []}.
    % {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
    % {ok, KBs} = aecore_suite_utils:mine_key_blocks(ParentNodeName, NumParentBlocks),
    % ct:log("Parent block mined ~p ~p number: ~p", [KBs, ParentNodeName, NumParentBlocks]),
    % {ok, KBs}.

%get_block_producer_name(Parties, Node, Height) ->
%    Producer = get_block_producer(Node, Height),
%    case lists:keyfind(Producer, 1, Parties) of
%        false -> Producer;
%        {_, _, Name} -> Name
%    end.

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
            {get_validator_contract, Who} ->
                {"get_validator_contract", [binary_to_list(encoded_pubkey(Who))]};
            get_current_epoch ->
                {"get_current_epoch", []};
            get_state ->
                {"get_state", []};
            leaders ->
                {"sorted_validators", []}

        end,
    ContractPubkey = ?config(staking_contract, Config),
    do_contract_call(ContractPubkey, src(?MAIN_STAKING_CONTRACT, Config), Fun, Args, OriginWho, TopHash).

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

create_stub(Contract) ->
    create_stub(Contract, []).

create_stub(Contract, Opts0) ->
    File = aect_test_utils:contract_filename(Contract),
    Opts = Opts0 ++ [{no_code, true}] ++ aect_test_utils:copts({file, File}),
    {ok, SrcBin} = aect_test_utils:read_contract(Contract),
    {ok, Enc}  = aeso_aci:contract_interface(json, binary_to_list(SrcBin), Opts),
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

get_entropy(Node, Epoch) ->
    ParentHeight = rpc(Node, aec_consensus_hc, entropy_height, [Epoch]),
    {ok, WPHdr}  = rpc(?PARENT_CHAIN_NODE, aec_chain, get_key_header_by_height, [ParentHeight]),
    {ok, WPHash0} = aec_headers:hash_header(WPHdr),
    {ParentHeight, aeser_api_encoder:encode(key_block_hash, WPHash0)}.
