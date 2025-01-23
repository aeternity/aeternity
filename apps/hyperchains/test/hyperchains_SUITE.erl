-module(hyperchains_SUITE).

-import(aecore_suite_utils, [http_request/4, external_address/0, rpc/3, rpc/4]).

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
         entropy_impact_schedule/1,
         spend_txs/1,
         simple_withdraw/1,
         empty_parent_block/1,
         sync_third_node/1,
         verify_rewards/1,
         elected_leader_did_not_show_up/1,
         block_difficulty/1,
         epochs_with_slow_parent/1,
         epochs_with_fast_parent/1,
         check_blocktime/1,
         get_pin/1,
         wallet_post_pin_to_pc/1,
         post_pin_to_pc/1,
         last_leader_validates_pin_and_post_to_contract/1,
         get_contract_pubkeys/1,
         correct_leader_in_micro_block/1,
         first_leader_next_epoch/1,
         check_default_pin/1,
         check_finalize_info/1,
         sanity_check_vote_tx/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("./test_defaults.hrl").

all() -> [{group, hc}, {group, epochs}, {group, pinning}, {group, default_pin}].

groups() ->
    [
      {hc, [sequence],
          [ start_two_child_nodes
          , produce_first_epoch
          , verify_rewards
          , check_finalize_info
          , spend_txs
          , simple_withdraw
          , correct_leader_in_micro_block
          , sync_third_node
          , produce_some_epochs
          , respect_schedule
          , entropy_impact_schedule
          , check_blocktime
          , get_contract_pubkeys
          , sanity_check_vote_tx
          ]}
    , {epochs, [sequence],
          [ start_two_child_nodes
          , first_leader_next_epoch
          , epochs_with_slow_parent
          , epochs_with_fast_parent ]}
    ].

suite() -> [].

init_per_suite(Config0) ->
    hctest_ct_shared:init_per_suite(Config0, _ParentSettings = #{
        owner_pubkey => ?OWNER_PUBKEY,
        parent_chain_node => ?PARENT_CHAIN_NODE,
        parent_chain_network_id => ?PARENT_CHAIN_NETWORK_ID,
        parent_finality => ?PARENT_FINALITY,
        reward_delay => ?REWARD_DELAY,
        parent_account_seeds => #{ % Parent patron pubkey is added in the shared implementation
            hctest:encoded_pubkey(?DWIGHT) => 2_100000000_000000000_000000000,
            hctest:encoded_pubkey(?EDWIN) => 3_100000000_000000000_000000000
        },
        main_staking_contract => ?MAIN_STAKING_CONTRACT,
        staking_validator_contract => ?STAKING_VALIDATOR_CONTRACT, 
        hc_contract => ?HC_CONTRACT
    }, _Settings = #{
        nodes_list => [?NODE1, ?NODE2]
    }).

end_per_suite(Config) ->
    hctest_ct_shared:end_per_suite(Config, [?NODE1, ?NODE2, ?NODE3, ?PARENT_CHAIN_NODE]).

init_per_group(Group, ConfigPre) ->
    hctest_ct_shared:init_per_group(Group, ConfigPre, #{
        consensus => ?CONSENSUS,
        parent_chain_node => ?PARENT_CHAIN_NODE,
        parent_chain_node_name => ?PARENT_CHAIN_NODE_NAME,
        parent_epoch_length => ?PARENT_EPOCH_LENGTH,
        parent_finality => ?PARENT_FINALITY,
        stakers => [?ALICE, ?BOB, ?LISA]
    }).

end_per_group(Group, Config) ->
    hctest_ct_shared:end_per_group(Group, Config).

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
    Config1 = hctest:with_saved_keys([nodes], Config),
    Nodes = proplists:get_value(nodes, Config1),
    Config2 = lists:keyreplace(nodes, 1, Config1,
                               {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, [], []}]}),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = hctest:with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

spend_txs(CTConfig) ->
    hctest:produce_cc_blocks(CTConfig, 1),

    %% First, seed ALICE, BOB and LISA, they need tokens in later tests
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    NetworkId = proplists:get_value(network_id, CTConfig),
    hctest:seed_account(
        hctest:pubkey(?ALICE), 100000001 * ?DEFAULT_GAS_PRICE, NetworkId),
    hctest:seed_account(
        hctest:pubkey(?BOB), 100000002 * ?DEFAULT_GAS_PRICE, NetworkId),
    hctest:seed_account(
        hctest:pubkey(?LISA), 100000003 * ?DEFAULT_GAS_PRICE, NetworkId),

    hctest:produce_cc_blocks(CTConfig, 1),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),

    %% Make spends until we've passed an epoch boundary
    spend_txs_(CTConfig).


spend_txs_(Config) ->
    {ok, #{first := First}} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    Top = rpc(?NODE1, aec_chain, top_header, []),

    case aec_headers:height(Top) == First of
        true  -> ok;
        false ->
            NetworkId = proplists:get_value(network_id, Config),
            hctest:seed_account(
                hctest:pubkey(?EDWIN), 1, NetworkId),
            hctest:produce_cc_blocks(Config, 1),
            {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
            spend_txs_(Config)
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

start_two_child_nodes(CTConfig) ->
    [{Node1, NodeName1, Stakers1, Pinners1}, {Node2, NodeName2, Stakers2, Pinners2} | _] = proplists:get_value(nodes, CTConfig),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(proplists:get_value(network_id, CTConfig))} ],
    
    hctest_ct_shared:child_node_config(CTConfig, #{
        node => Node1,
        stakeholders => Stakers1,
        pinners => Pinners1,
        receive_address => ?FORD,
        hc_contract => ?HC_CONTRACT
    }),
    aecore_suite_utils:start_node(Node1, CTConfig, Env),
    aecore_suite_utils:connect(NodeName1, []),
    
    hctest_ct_shared:child_node_config(#{
        node => Node2,
        stakeholders => Stakers2,
        pinners => Pinners2,
        receive_address => ?FORD,
        hc_contract => ?HC_CONTRACT
    }, CTConfig),
    aecore_suite_utils:start_node(Node2, CTConfig, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

produce_first_epoch(Config) ->
    produce_n_epochs(Config, 1).

produce_some_epochs(Config) ->
    produce_n_epochs(Config, 5).

produce_n_epochs(Config, N) ->
    [{Node1, _, _, _}|_] = proplists:get_value(nodes, Config),
    %% produce blocks
    {ok, Bs} = hctest:produce_cc_blocks(Config, N * ?CHILD_EPOCH_LENGTH),
    %% check producers
    Producers = [ aec_blocks:miner(B) || B <- Bs, aec_blocks:is_key_block(B) ],
    ChildTopHeight = rpc(Node1, aec_chain, top_height, []),
    Leaders = hctest:leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, ParentBlocks} = hctest:get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = hctest:get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

respect_schedule(Config) ->
    [{Node, _, _, _}|_] = proplists:get_value(nodes, Config),
    ChildHeight = rpc(Node, aec_chain, top_height, []),
    %% Validate one epoch at a time
    respect_schedule(Node, 1, 1, ChildHeight).

respect_schedule(_Node, EpochStart, _Epoch, TopHeight) when TopHeight < EpochStart ->
    ok;
respect_schedule(Node, EpochStart, Epoch, TopHeight) ->
    {ok, #{first := StartHeight} = EI} =
        rpc(Node, aec_chain_hc, epoch_info, [EpochStart]),

    #{ seed := EISeed, validators := EIValidators, length := EILength, last := EILast } = EI,

    ct:log("Checking epoch ~p info: ~p at height ~p", [Epoch, EI, EpochStart]),

    {ParentHeight, PHash} = hctest:get_entropy(Node, Epoch),
    ct:log("ParentHash at height ~p: ~p", [ParentHeight, PHash]),
    ?assert(case EISeed of Hash when Hash == undefined; Hash == PHash -> true; _ -> false end, 
        hctest:format("The (epoch_info)EISeed=~w must either be undefined or equal to the parent_hash=~w",
            [EISeed, PHash])),

    %% Check the API functions in aec_chain_hc
    {ok, Schedule} = rpc(Node, aec_chain_hc, validator_schedule, [EpochStart, PHash, EIValidators, EILength]),
    ct:log("Validating schedule ~p for Epoch ~p", [Schedule, Epoch]),

    lists:foreach(fun({Height, ExpectedProducer}) when Height =< TopHeight ->
                              Producer = hctest:get_block_producer(Node, Height),
                              ct:log("Check producer of block ~p: ~p =?= ~p", [Height, Producer, ExpectedProducer]),
                              ?assertEqual(Producer, ExpectedProducer);
                     (_) -> ok
                  end, lists:zip(lists:seq(StartHeight, StartHeight + EILength - 1), Schedule)),

    respect_schedule(Node, EILast + 1, Epoch + 1, TopHeight).

%% For different Epoch's we have different schedules
%% (Provided we past 4 epochs)
entropy_impact_schedule(Config) ->
    Nodes = [ N || {N, _, _, _} <- proplists:get_value(nodes, Config)],
    Node = hd(Nodes),
    %% Sync nodes
    ChildHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := Epoch0, length := Length0}} = rpc(Node, aec_chain_hc, epoch_info, []),
    %% Make sure chain is long enough
    case Epoch0 =< 5 of
      true ->
        %% Chain to short to have meaningful test, e.g. when ran in isolation
        hctest:produce_cc_blocks(Config, 5 * Length0 - ChildHeight);
      false ->
        ok
    end,
    {ok, #{seed := Seed,
           first := First,
           length := Length,
           validators := Validators,
           epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, []),

    {_, Entropy0} = hctest:get_entropy(Node, Epoch - 1),
    {_, Entropy1} = hctest:get_entropy(Node, Epoch),

    {ok, Schedule} = rpc(Node, aec_chain_hc, validator_schedule, [First, Seed, Validators, Length]),
    {ok, RightSchedule} = rpc(Node, aec_chain_hc, validator_schedule, [First, Entropy1, Validators, Length]),
    {ok, WrongSchedule} = rpc(Node, aec_chain_hc, validator_schedule, [First, Entropy0, Validators, Length]),
    ct:log("Schedules:\nepoch ~p\nwrong ~p\nright ~p", [Schedule, WrongSchedule, RightSchedule]),
    %% There is a tiny possibility that the two against all odds are the same
    ?assertEqual(RightSchedule, Schedule),
    ?assertNotEqual(WrongSchedule, Schedule).

simple_withdraw(Config) ->
    [{_Node, NodeName, _, _} | _] = proplists:get_value(nodes, Config),
    NetworkId = proplists:get_value(network_id, Config),

    %% Not at the start of the epoch
    hctest:produce_cc_blocks(Config, 2),

    %% grab Alice's and Bob's staking validator contract
    {ok, AliceCtEnc} = hctest:inspect_staking_contract(?ALICE, {get_validator_contract, ?ALICE}, Config),
    {ok, AliceCt} = aeser_api_encoder:safe_decode(contract_pubkey, AliceCtEnc),
    {ok, BobCtEnc} = hctest:inspect_staking_contract(?ALICE, {get_validator_contract, ?BOB}, Config),
    {ok, BobCt} = aeser_api_encoder:safe_decode(contract_pubkey, BobCtEnc),
    ValidatorStub = hctest:src(?STAKING_VALIDATOR_CONTRACT, Config),

    %% Get the initial state
    InitBalance  = hctest:account_balance(hctest:pubkey(?ALICE)),

    %% Auto-stake is on, so available balance should be 0
    {ok, 0} = hctest:inspect_validator(AliceCt, ?ALICE, get_available_balance, Config),

    %% Adjust stake to prepare for a withdrawal...
    WithdrawAmount = 1000,
    Call1 = hctest:contract_call(
        AliceCt, ValidatorStub, "adjust_stake",
         [integer_to_list(-WithdrawAmount)], 0, 
         hctest:pubkey(?ALICE)),
    CallTx1 = hctest:sign_and_push(NodeName, Call1, ?ALICE, NetworkId),
    {ok, [_]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    hctest:produce_cc_blocks(Config, 1),
    {ok, CallRes1} = hctest:call_info(CallTx1),
    {ok, _Res1} = hctest:decode_consensus_result(CallRes1, "adjust_stake", ValidatorStub),

    %% Ok, should still be 0
    hctest:produce_cc_blocks(Config, 1),
    {ok, 0} = hctest:inspect_validator(AliceCt, ?ALICE, get_available_balance, Config),

    %% Let's advance 5 epochs...
    produce_n_epochs(Config, 5),

    %% Now test the withdrawal
    {ok, WithdrawAmount} = hctest:inspect_validator(AliceCt, ?ALICE, get_available_balance, Config),

    {ok, AliceStake} = hctest:inspect_validator(AliceCt, ?ALICE, get_total_balance, Config),
    {ok, BobStake} = hctest:inspect_validator(BobCt, ?BOB, get_total_balance, Config),

    Call2 = hctest:contract_call(
        AliceCt, ValidatorStub, "withdraw",
         [integer_to_list(WithdrawAmount)], 0,
         hctest:pubkey(?ALICE)),
    CallTx2 = hctest:sign_and_push( NodeName, Call2, ?ALICE, NetworkId),
    {ok, [_]} = rpc:call(NodeName, aec_tx_pool, peek, [infinity]),
    StakeWithdrawDelay = 1,
    hctest:produce_cc_blocks(Config, StakeWithdrawDelay),
    EndBalance = hctest:account_balance(hctest:pubkey(?ALICE)),
    {ok, CallRes2} = hctest:call_info(CallTx2),
    {ok, _Res2} = hctest:decode_consensus_result(CallRes2, "withdraw", ValidatorStub),
    GasUsed = aect_call:gas_used(CallRes2),
    GasPrice = aect_call:gas_price(CallRes2),
    Fee = aetx:fee(aetx_sign:tx(CallTx2)),
    {Producer, KeyReward} = hctest:key_reward_provided(),
    ct:log("Initial balance: ~p, withdrawn: ~p, gas used: ~p, gas price: ~p, fee: ~p, end balance: ~p",
           [InitBalance, WithdrawAmount, GasUsed, GasPrice, Fee, EndBalance]),
    {ok, AliceStake1} = hctest:inspect_validator(AliceCt, ?ALICE, get_total_balance, Config),
    {ok, BobStake1} = hctest:inspect_validator(BobCt, ?BOB, get_total_balance, Config),
    ?assert(BobStake == BobStake1 orelse
            (Producer == hctest:pubkey(?BOB)
             andalso BobStake + KeyReward == BobStake1)),
    ct:log("Staking power before: ~p and after ~p", [AliceStake, AliceStake1]),
    ?assert(AliceStake - 1000 == AliceStake1 orelse
            (Producer == hctest:pubkey(?ALICE) 
            andalso AliceStake + KeyReward - 1000 == AliceStake1)),
    ok.

correct_leader_in_micro_block(Config) ->
    [{_Node, NodeName, _, _} | _] = proplists:get_value(nodes, Config),
    %% Call the contract in a transaction, asking for "leader"
    {ok, [_]} = hctest:produce_cc_blocks(Config, 1),
    CallTx = hctest:sign_and_push(
        NodeName,
        hctest:contract_call(
            proplists:get_value(election_contract, Config), 
                hctest:src(?HC_CONTRACT, Config),
            "leader", [], 0, hctest:pubkey(?ALICE)),
        ?ALICE, proplists:get_value(network_id, Config)),

    {ok, [KeyBlock, _MicroBlock]} = hctest:produce_cc_blocks(Config, 1),
    %% Microblock contains the contract call, find out what it returned on that call
    {ok, Call} = hctest:call_info(CallTx),
    {ok, Res} = hctest:decode_consensus_result(Call, "leader", 
        hctest:src(?HC_CONTRACT, Config)),
    %% The actual leader did produce the keyblock (and micro block)
    Producer = aeser_api_encoder:encode(account_pubkey, aec_blocks:miner(KeyBlock)),
    ?assertEqual(Producer, Res).

set_up_third_node(Config) ->
    {Node3, NodeName, Stakers, _Pinners} = lists:keyfind(?NODE3, 1, proplists:get_value(nodes, Config)),
    Nodes = [ Node || {Node, _, _, _} <- proplists:get_value(nodes, Config)],
    aecore_suite_utils:make_multi(Config, [Node3]),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(proplists:get_value(network_id, Config))} ],
    hctest_ct_shared:child_node_config(#{
        node => Node3,
        stakeholders => Stakers,
        pinners => []}, Config), % no pinners here FTM
    aecore_suite_utils:start_node(Node3, Config, Env),
    aecore_suite_utils:connect(NodeName, []),
    timer:sleep(1000),
    Node3Peers = rpc(Node3, aec_peers, connected_peers, []),
    ct:log("Connected peers ~p", [Node3Peers]),
    Node3VerifiedPeers = rpc(Node3, aec_peers, available_peers, [verified]),
    ct:log("Verified peers ~p", [Node3VerifiedPeers]),
    {ok, _} = hctest:wait_same_top(Nodes, 300),
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

sanity_check_vote_tx(Config) ->
    [{Node1, _, _, _}, {Node2, _, _, _} | _] = proplists:get_value(nodes, Config),

    {ok, #{epoch := Epoch}} = rpc(Node1, aec_chain_hc, epoch_info, []),
    %% Push a vote tx onto node1 - then read on node2
    {ok, VoteTx1} = aec_hc_vote_tx:new(#{voter_id => aeser_id:create(account, hctest:pubkey(?ALICE)),
                                         epoch    => Epoch,
                                         type     => 4,
                                         data     => #{<<"key1">> => <<"value1">>,
                                                       <<"key2">> => <<"value2">>}}),
    {_, HCVoteTx1} = aetx:specialize_type(VoteTx1),

    NetworkId = rpc(Node1, aec_governance, get_network_id, []),
    SVoteTx1 = hctest:sign_tx(VoteTx1, hctest:privkey(?ALICE), NetworkId),

    ok = rpc(Node1, aec_hc_vote_pool, push, [SVoteTx1]),
    timer:sleep(10),
    {ok, [HCVoteTx1]} = rpc(Node2, aec_hc_vote_pool, peek, [Epoch]),

    %% Test GC
    hctest:mine_to_next_epoch(Node2, Config),

    timer:sleep(10),
    {ok, []} = rpc(Node1, aec_hc_vote_pool, peek, [Epoch]),
    {ok, []} = rpc(Node2, aec_hc_vote_pool, peek, [Epoch]),

    ok.

fetch_validator_contract(Who, Config) ->
    {ok, CtEnc} = hctest:inspect_staking_contract(Who, {get_validator_contract, Who}, Config),
    {ok, Ct} = aeser_api_encoder:safe_decode(contract_pubkey, CtEnc),
    Ct.

verify_rewards(Config) ->
    [{Node, _NodeName, _, _} | _ ] = proplists:get_value(nodes, Config),
    NetworkId = proplists:get_value(network_id, Config),
    {_, PatronPub} = aecore_suite_utils:sign_keys(Node),

    Height = rpc(Node, aec_chain, top_height, []),
    {ok, #{first := First0}} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% grab Alice's and Bob's staking validator contract
    AliceCt = fetch_validator_contract(?ALICE, Config),
    BobCt = fetch_validator_contract(?BOB, Config),
    LisaCt = fetch_validator_contract(?LISA, Config),

    %% Assert we are at the beginning of an epoch
    [ hctest:mine_to_next_epoch(Node, Config) || Height + 1 /= First0 ],
    {ok, EpochInfo} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% Now fill this generation with known stuff
    {ok, _} = hctest:produce_cc_blocks(Config, 2),

    {ok, SignedTx1} = hctest:seed_account(PatronPub, 1, NetworkId),
    {ok, _} = hctest:produce_cc_blocks(Config, 1),
    ?assertEqual(aetx:fee(aetx_sign:tx(SignedTx1)), ?FEE_REWARD),

    {ok, _SignedTx2} = hctest:seed_account(PatronPub, 1, NetworkId),
    {ok, _} = hctest:produce_cc_blocks(Config, 1),

    %% Now skip to the next-next epoch where rewards should have been payed
    hctest:mine_to_next_epoch(Node, Config),
    mine_to_last_block_in_epoch(Node, Config),

    %% Record the state
    {ok, AliceTot0} = hctest:inspect_validator(AliceCt, ?ALICE, get_total_balance, Config),
    {ok, BobTot0} = hctest:inspect_validator(BobCt, ?BOB, get_total_balance, Config),
    {ok, LisaTot0} = hctest:inspect_validator(LisaCt, ?LISA, get_total_balance, Config),

    %% Produce final block to distribute rewards.
    {ok, _} = hctest:produce_cc_blocks(Config, 1),

    %% Record the new state
    {ok, AliceTot1} = hctest:inspect_validator(AliceCt, ?ALICE, get_total_balance, Config),
    {ok, BobTot1} = hctest:inspect_validator(BobCt, ?BOB, get_total_balance, Config),
    {ok, LisaTot1} = hctest:inspect_validator(LisaCt, ?LISA, get_total_balance, Config),

    #{first := First, last := Last} = EpochInfo,
    {ok, BlocksInGen} = hctest:get_generations(Node, First, Last),

    Rewards = calc_rewards(BlocksInGen, Node),

    ct:log("Alice ~p -> ~p expected reward ~p", [AliceTot0, AliceTot1, maps:get(hctest:pubkey(?ALICE), Rewards)]),
    ct:log("Bob ~p -> ~p expected reward ~p", [BobTot0, BobTot1, maps:get(hctest:pubkey(?BOB_SIGN), Rewards)]),
    ct:log("Lisa ~p -> ~p expected reward ~p", [LisaTot0, LisaTot1, maps:get(hctest:pubkey(?LISA), Rewards)]),
    ?assertEqual(AliceTot0 + maps:get(hctest:pubkey(?ALICE), Rewards, 0), AliceTot1),
    ?assertEqual(BobTot0 + maps:get(hctest:pubkey(?BOB_SIGN), Rewards, 0), BobTot1),
    ?assertEqual(LisaTot0 + maps:get(hctest:pubkey(?LISA), Rewards, 0), LisaTot1),

    %% Check that MainStaking knows the right epoch.
    {ok, #{epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, Epoch} = hctest:inspect_staking_contract(?ALICE, get_current_epoch, Config),

    ok.

calc_rewards(Blocks, Node) ->
    calc_rewards(Blocks, Node, undefined, #{}).

calc_rewards([], _Node, _Prev, Rs) -> Rs;
calc_rewards([B | Bs], Node, Prev, Rs) ->
    case aec_blocks:type(B) of
        micro -> calc_rewards(Bs, Node, Prev, Rs);
        key ->
            M = aec_blocks:miner(B),
            {R1, R2} =
                case aec_blocks:prev_key_hash(B) == aec_blocks:prev_hash(B) of
                    true  -> {?BLOCK_REWARD, 0};
                    false ->
                        [{H, _}] = rpc(Node, aec_db, find_key_headers_and_hash_at_height, [aec_blocks:height(B)]),
                        {value, Fees} = rpc(Node, aec_db, find_block_fees, [H]),
                        {?BLOCK_REWARD + 6 * (Fees div 10), 4 * (Fees div 10)}
                end,
            Rs1 = Rs#{M => R1 + maps:get(M, Rs, 0)},
            Rs2 = Rs1#{Prev => R2 + maps:get(Prev, Rs1, 0)},
            calc_rewards(Bs, Node, M, Rs2)
    end.


block_difficulty(Config) ->
    lists:foreach(
        fun(_) ->
            {ok, [KB]} = hctest:produce_cc_blocks(Config, 1),
            {ok, AddedStakingPower} = hctest:inspect_election_contract(?ALICE, current_added_staking_power, Config),
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
    hctest:produce_cc_blocks(Config, 1),
    {ok, KB} = hctest:wait_same_top([?NODE2, ?NODE3]),
    0 = aec_blocks:difficulty(KB),
    TopHeader1 = rpc(?NODE3, aec_chain, top_header, []),
    ct:log("Lazy header: ~p", [TopHeader1]),
    TopHeader1 = rpc(?NODE2, aec_chain, top_header, []),
    NetworkId = proplists:get_value(network_id, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)} ],
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    hctest:produce_cc_blocks(Config, 1),
    {ok, _} = hctest:wait_same_top([?NODE1, ?NODE3]),
    timer:sleep(2000), %% Give NODE1 a moment to finalize sync and post commitments
    hctest:produce_cc_blocks(Config, 1),
    {ok, _KB1} = hctest:wait_same_top([ Node || {Node, _, _, _} <- proplists:get_value(nodes, Config)]),
    {ok, _} = hctest:produce_cc_blocks(Config, 10),
    {ok, _KB2} = hctest:wait_same_top([ Node || {Node, _, _, _} <- proplists:get_value(nodes, Config)]),
    ok.

first_leader_next_epoch(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),
    hctest:produce_cc_blocks(Config, 1),
    StartHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last, epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, [StartHeight]),
    ct:log("Checking leader for first block next epoch ~p (height ~p)", [Epoch+1, Last+1]),
    ?assertMatch({ok, _}, rpc(Node, aec_consensus_hc, leader_for_height, [Last + 1])).

%% Demonstrate that child chain start signalling epoch length adjustment upward
%% When parent blocks are produced too slowly, we need to lengthen child epoch
epochs_with_slow_parent(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),
    ct:log("Parent start height = ~p", [proplists:get_value(parent_start_height, Config)]),
    %% ensure start at a new epoch boundary
    StartHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, [StartHeight]),
    BlocksLeftToBoundary = Last - StartHeight,
    ct:log("Starting at CC height ~p: producing ~p cc blocks", [StartHeight, BlocksLeftToBoundary]),
    %% some block production including parent blocks
    hctest:produce_cc_blocks(Config, BlocksLeftToBoundary),

    ParentHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("Child continues while parent stuck at: ~p", [ParentHeight]),
    ParentEpoch = (ParentHeight - proplists:get_value(parent_start_height, Config) +
                      (?PARENT_EPOCH_LENGTH - 1)) div ?PARENT_EPOCH_LENGTH,
    ChildEpoch = rpc(Node, aec_chain, top_height, []) div ?CHILD_EPOCH_LENGTH,
    ct:log("Child epoch ~p while parent epoch ~p (parent should be in next epoch)", [ChildEpoch, ParentEpoch]),
    ?assertEqual(1, ParentEpoch - ChildEpoch),

    Resilience = 1, %% Child can cope with missing Resilience epochs in parent chain
    %% Produce no parent block in the next Resilience child epochs
    %% the child chain should get to a halt or
    %% at least one should be able to observe signalling that the length should be adjusted upward
    {ok, _} = hctest:produce_cc_blocks(Config, ?CHILD_EPOCH_LENGTH*Resilience, []),

    ct:log("Mined almost ~p additional child epochs without parent progress", [Resilience]),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ?assertEqual(ParentHeight, ParentTopHeight),
    ChildTopHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := EndEpoch} = EpochInfo} = rpc(Node, aec_chain_hc, epoch_info, [ChildTopHeight]),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight, ChildTopHeight, EndEpoch]),

    %% Here we should have observed some signalling for increased child epoch length

    %% Parent hash grabbed in last block child epoch, so here we can start, but not finish next epoch
    {ok, _} = hctest:produce_cc_blocks(Config, maps:get(length, EpochInfo) - 1, []),
    ?assertException(error, timeout_waiting_for_block, hctest:produce_cc_blocks(Config, 1, [])),

    ?assertEqual([{ok, (N-1) * ?CHILD_EPOCH_LENGTH + 1} || N <- lists:seq(1, EndEpoch)],
                 [rpc(Node, aec_chain_hc, epoch_start_height, [N]) || N <- lists:seq(1, EndEpoch)]),
    ok.

%% Demonstrate that child chain start signalling epoch length adjustment downward
%% When parent blocks are produced too quickly, we need to shorten child epoch
epochs_with_fast_parent(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ChildTopHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := ChildEpoch}} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% Quickly produce parent blocks to be in sync again
    ParentBlocksNeeded =
        ChildEpoch * ?PARENT_EPOCH_LENGTH + proplists:get_value(parent_start_height, Config) + ?PARENT_FINALITY - ParentTopHeight,

    %% Produce ?PARENT_EPOCH_LENGTH parent blocks quickly (very artificial)
    {ok, _} = hctest:produce_cc_blocks(Config, 1, [{ChildTopHeight + 1, ParentBlocksNeeded}]),
    %% and finish a child epoch
    %% ensure start at a new epoch boundary
    StartHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last, length := Len} = EpochInfo1} = rpc(Node, aec_chain_hc, epoch_info, []),
    ct:log("Info ~p", [EpochInfo1]),
    BlocksLeftToBoundary = Last - StartHeight,
    %% some block production including parent blocks
    {ok, _} = hctest:produce_cc_blocks(Config, BlocksLeftToBoundary),

    %% Produce twice as many parent blocks as needed in an epoch
    Height0 = rpc(Node, aec_chain, top_height, []),
    ParentTopHeight0 = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, _} = hctest:produce_cc_blocks(
        Config, ?CHILD_EPOCH_LENGTH,
        hctest:spread(2*?PARENT_EPOCH_LENGTH, Height0,
            [ {CH, 0} || CH <- lists:seq(Height0 + 1, Height0 + Len)])),

    ParentTopHeight1 = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    Height1 = rpc(Node, aec_chain, top_height, []),
    {ok, EpochInfo2} = rpc(Node, aec_chain_hc, epoch_info, []),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight1, Height1, EpochInfo2 ]),
    ?assertEqual(2*?PARENT_EPOCH_LENGTH, ParentTopHeight1 - ParentTopHeight0),

    {ok, _} = hctest:produce_cc_blocks(
        Config, ?CHILD_EPOCH_LENGTH,
        hctest:spread(2*?PARENT_EPOCH_LENGTH, Height1,
            [ {CH, 0} || CH <- lists:seq(Height1 + 1, Height1 + Len)])),

    %% Here we should be able to observe signalling that epoch should be shorter
    ok.

%%%=============================================================================
%%% HC Endpoints
%%%=============================================================================

get_contract_pubkeys(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),
    %% Verify that endpoint is available
    {ok, IsChildChain} = rpc(Node, aeu_env, find_config,
                             [[<<"http">>, <<"endpoints">>, <<"hyperchain">>], [user_config, schema_default]]),
    ?assert(IsChildChain),
    StakingContractPK = rpc(Node, aec_consensus_hc, get_contract_pubkey, [staking]),
    ElectionContractPK = rpc(Node, aec_consensus_hc, get_contract_pubkey, [election]),
    RewardsContractPK = rpc(Node, aec_consensus_hc, get_contract_pubkey, [rewards]),
    ct:log("Calling hyperchain/contracts at ~p", [aecore_suite_utils:external_address()]),
    {ok, 200, Repl1} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/contracts", []),
    #{<<"staking">> := Staking,
      <<"election">> := Election,
      <<"rewards">> := Rewards
    } = Repl1,
    ?assertEqual({ok, StakingContractPK}, aeser_api_encoder:safe_decode(contract_pubkey, Staking)),
    ?assertEqual({ok, ElectionContractPK}, aeser_api_encoder:safe_decode(contract_pubkey, Election)),
    ?assertEqual({ok, RewardsContractPK}, aeser_api_encoder:safe_decode(contract_pubkey, Rewards)),

    ok.


%%%=============================================================================
%%% Pinning
%%%=============================================================================

get_pin(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),
    %% Verify that endpoint is available
    {ok, IsChildChain} = rpc(Node, aeu_env, find_config,
                             [[<<"http">>, <<"endpoints">>, <<"hyperchain">>], [user_config, schema_default]]),
    ?assert(IsChildChain),

    %% Mine one block and derive which epoch we are in
    {ok, _} = hctest:produce_cc_blocks(Config, 1),
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
    {ok, DecodedPin} = rpc(Node, aec_parent_connector, decode_parent_pin_payload, [Payload]),
    ?assertEqual(#{epoch => PrevEpoch, height => Height1, block_hash => BH1Dec},
                 DecodedPin),

    %% produce some more child blocks if we stay in same epoch, then pins should be the same
    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    {ok, 200, Repl2} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/pin-tx", []),
    {ok, EpochInfo2} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    %% Get response from being in next Epoch
    Repl3 =
        if EpochInfo1 == EpochInfo2 ->
             ?assertEqual(Repl1, Repl2),
             {ok, _} = hctest:produce_cc_blocks(Config, maps:get(length, EpochInfo2) - 1),
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
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),

    %% Get to first block in new epoch
    Height1 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last1}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, _} = hctest:produce_cc_blocks(Config, Last1 - Height1 + 1),
    {ok, Pin} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    {ok, _} = hctest:produce_cc_blocks(Config, 5),

    DwightPub = hctest:pubkey(?DWIGHT), % PC chain account
    %DwightEnc = aeser_api_encoder:encode(account_pubkey, DwightPub),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % no pending transactions
    ct:log("DWIGHT: ~p ",[DwightPub]),
    PinTx = rpc(Node, aec_parent_connector, create_pin_tx, [DwightPub, DwightPub, 1, 30000 * ?DEFAULT_GAS_PRICE, Pin]),
    ct:log("PinTX: ~p", [PinTx]),
    SignedPinTx = hctest:sign_tx(PinTx, hctest:privkey(?DWIGHT),?PARENT_CHAIN_NETWORK_ID),
    EncTxHash = rpc(Node, aec_parent_connector, post_pin_tx, [SignedPinTx]),
    {ok, [_]} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % one transaction pending now.
    {ok, _} = hctest:produce_cc_blocks(Config, 5),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted

    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, []),

    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),

    NetworkId = proplists:get_value(network_id, Config), % TODO not 100% sure about this one...
    Nonce = hctest:next_nonce(Node, hctest:pubkey(?ALICE)),
    Params = #{ sender_id    => aeser_id:create(account, hctest:pubkey(?ALICE)),
                recipient_id => aeser_id:create(account, LastLeader),
                amount       => 1,
                fee          => 30000 * ?DEFAULT_GAS_PRICE,
                nonce        => Nonce,
                payload      => EncTxHash},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = hctest:sign_tx(Tx, hctest:privkey(?ALICE), NetworkId),
    ok = rpc(Node, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, [_]} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool
    {ok, _} = hctest:produce_cc_blocks(Config, 1),
    CH = rpc(Node, aec_chain, top_height, []),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted
    DistToBeforeLast = Last - CH - 1,
    {ok, _} = hctest:produce_cc_blocks(Config, DistToBeforeLast), % produce blocks until last
    BL = Last - 1,
    BL = rpc(Node, aec_chain, top_height, []), % we're producing in last black
    ok.

%% A wallet posting a pin transaction by only using HTTP API towards Child and Parent
wallet_post_pin_to_pc(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),

    %% Progress to first block of next epoch
    Height1 = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{last := Last1, length := Len, epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, Bs} = hctest:produce_cc_blocks(Config, Last1 - Height1 + 1),
    HashLastInEpoch = aec_blocks:prev_hash(lists:last(Bs)),
    ct:log("Block last epoch: ~p", [aeser_api_encoder:encode(key_block_hash, HashLastInEpoch)]),

    DwightPub = hctest:pubkey(?DWIGHT),
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
    ParentHost = hctest:external_address(?PARENT_CHAIN_NODE),
    ct:log("Parent address ~p", [ParentHost]),
    {ok, 200, DwightInfo} = aecore_suite_utils:http_request(ParentHost, get, <<"accounts/", DwightEnc/binary>>, []),
    Nonce = maps:get(<<"nonce">>, DwightInfo) + 1,
    {ok, PinTx} = hctest:create_ae_spend_tx(DwightPub, DwightPub, Nonce, Payload),
    ct:log("Unsigned Spend on parent chain ~p", [PinTx]),

    SignedPinTx = hctest:sign_tx(PinTx, hctest:privkey(?DWIGHT), ?PARENT_CHAIN_NETWORK_ID),
    Transaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedPinTx)),
    {ok, 200, #{<<"tx_hash">> := ProofHash}} = aecore_suite_utils:http_request(ParentHost, post, <<"transactions">>, #{tx => Transaction}),
    {ok, [_]} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % one transaction pending now.
    {ok, _} = hctest:produce_cc_blocks(Config, Len div 2),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted

    %% Don't wait and check for the height of acceptance, because due to parent fork micro forks,
    %% this may change in a while... the last leader will do the work needed on the hashes
    %% it receives

    %% Now just inform the last leader of this epoch about the transaction hash
    %% via a spend on child chain... the leader will have machinery to pick up tx hash
    %% and to find out at which parent height the hash is accepted at
    % ProofHash = list_to_binary("PIN"++TxHash), % the hash comes encoded already
    {_, LeaderPubkey} = aeser_api_encoder:decode(LastLeader),
    NonceAlice = hctest:next_nonce(Node, hctest:pubkey(?ALICE)),
    Params = #{ sender_id    => aeser_id:create(account, hctest:pubkey(?ALICE)),
                recipient_id => aeser_id:create(account, LeaderPubkey),
                amount       => 1,
                fee          => 30000 * ?DEFAULT_GAS_PRICE,
                nonce        => NonceAlice,
                payload      => ProofHash},
    ct:log("Preparing a spend tx for child chain: ~p", [Params]),
    {ok, ProofTx} = aec_spend_tx:new(Params),
    SignedProofTx = hctest:sign_tx(ProofTx, hctest:privkey(?ALICE), proplists:get_value(network_id, Config)),
    ProofTransaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedProofTx)),
    {ok, 200, _} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), post, <<"transactions">>, #{tx => ProofTransaction}),

    {ok, [_]} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool
    {ok, _} = hctest:produce_cc_blocks(Config, 1),
    {ok, []} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool

    Height2 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := CollectHeight}} = rpc(Node, aec_chain_hc, epoch_info, []),
    %% mine to CollectHeight and TODO: see that indeed the proof has been used
    {ok, _} = hctest:produce_cc_blocks(Config, CollectHeight - Height2),
    ok.

last_leader_validates_pin_and_post_to_contract(Config) ->
    [{Node, NodeName, _, _} | _] = proplists:get_value(nodes, Config),
    %% 1. Correct pin is posted in the contract

    #{cur_pin_reward := _Reward} = rpc(Node, aec_chain_hc , pin_reward_info, []),

    %% move into next epoch
    hctest:mine_to_next_epoch(Node, Config),

    %% post pin to PC
    {ok, PinningData} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    ct:log("Pinning data ~p", [PinningData]),
    TxHash = pin_to_parent(Node, PinningData, hctest:pubkey(?DWIGHT)),
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

    LeaderBalance1A = hctest:account_balance(LastLeader),
    %% use get_pin_by_tx_hash to get the posted hash back and compare with actual keyblock (to test encoding decoding etc)
    {ok, #{epoch := _PinEpoch, height := PinHeight, block_hash := PinHash}} =
        rpc(Node, aec_parent_connector, get_pin_by_tx_hash, [FirstSpend]),
    ?assertEqual({ok, PinHash}, rpc(Node, aec_chain_state, get_key_block_hash_at_height, [PinHeight])),

    %% move into next epoch - trigger leader validation?
    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    {ok, #{info := {pin_accepted, _}}} = wait_for_ps(pin),
    LeaderBalance1B = hctest:account_balance(LastLeader),

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

    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    {ok, #{info := {no_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 3. Incorrect pin posted to contract a) bad tx hash

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % post bad hash to contract

    {ok, #{last := Last3}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader3} = rpc(Node, aec_consensus_hc, leader_for_height, [Last3]),
    ok = pin_contract_call_tx(Config, <<"THIS IS A BAD TX HASH">>, LastLeader3),

    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    {ok, #{info := {incorrect_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 4. Incorrect hash stored on PC

    {ok, PD4} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    EncTxHash4 = pin_to_parent(Node, PD4#{block_hash := <<"VERYINCORRECTBLOCKHASH">>}, hctest:pubkey(?DWIGHT)),
    %% post parent spend tx hash to CC
    {ok, #{last := Last4}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader4} = rpc(Node, aec_consensus_hc, leader_for_height, [Last4]),

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % post bad hash to contract
    LeaderBalance4A = hctest:account_balance(LastLeader4),
    ok = pin_contract_call_tx(Config, EncTxHash4, LastLeader4),

    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    {ok, #{info := {incorrect_proof_posted}}} = wait_for_ps(pin),

    LeaderBalance4B = hctest:account_balance(LastLeader4),

    ct:log("Account balance for leader was: ~p, is now: ~p", [LeaderBalance4A, LeaderBalance4B]),
    % See above for when a reward for pinning actually was given... Same problem here.
    % LeaderBalance4A = LeaderBalance4B, % nothing was rewarded

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 4. Bad height and then bad leader

    {ok, PD5} = rpc(Node, aec_parent_connector, get_pinning_data, []),
    EncTxHash5 = pin_to_parent(Node, PD5, hctest:pubkey(?DWIGHT)),

    {ok, #{last := Last5}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader5} = rpc(Node, aec_consensus_hc, leader_for_height, [Last5]),

    {ok, _} = hctest:produce_cc_blocks(Config, 1),

    %% at the wrong height
    ok = pin_contract_call_tx(Config, EncTxHash5, LastLeader5),

    {ok, _} = hctest:produce_cc_blocks(Config, 1),
    {ok, []} = rpc(Node, aec_tx_pool, peek, [infinity]), % transaction not in pool
    %% check that no pin info was stored.
    undefined = rpc(Node, aec_chain_hc, pin_info, []),

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % post by wrong leader
    NotLeader = hd([hctest:pubkey(?ALICE), hctest:pubkey(?BOB)] -- [LastLeader5]),
    ok = pin_contract_call_tx(Config, EncTxHash5, NotLeader),

    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    {ok, #{info := {no_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    ok.

check_default_pin(Config) ->
    [{Node, NodeName, _, _} | _] = proplists:get_value(nodes, Config),

    {ok, _} = hctest:produce_cc_blocks(Config, 12),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    ct:log("Last Leader: ~p", [LastLeader]),

    mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    %% with current test setup, all validators have a pc account, so pins will always happen(?)
    {ok, #{info := {pin_accepted, _}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% TODO test when not all validators have PC account, but how ensure
    %% that any given validator will be last leader within the run of the test???

    ok.

%%%=============================================================================
%%% Elections
%%%=============================================================================

check_finalize_info(Config) ->
    [{Node, _, _, _} | _] = proplists:get_value(nodes, Config),
    hctest:mine_to_next_epoch(Node, Config),
    {ok, #{epoch  := Epoch,
           last   := Last,
           validators := Validators,
           length := _Length}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    mine_to_last_block_in_epoch(Node, Config),
    {ok, _} = hctest:produce_cc_blocks(Config, 2),
    #{producer := Producer, epoch := FEpoch, votes := Votes} = rpc(Node, aec_chain_hc , finalize_info, []),
    FVoters = lists:map(fun(#{producer := Voter}) -> Voter end, Votes),
    TotalStake = lists:foldl(fun({_, Stake}, Accum) -> Stake + Accum end, 0, Validators),
    VotersStake = lists:foldl(fun(Voter, Accum) -> proplists:get_value(Voter, Validators) + Accum end, 0, FVoters),
    TotalVotersStake = proplists:get_value(LastLeader, Validators) + VotersStake,
    ?assertEqual(Epoch, FEpoch),
    ?assertEqual(Producer, LastLeader),
    ?assert(TotalVotersStake >= trunc(math:ceil((2 * TotalStake) / 3))).

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
    {ok, _} = hctest:produce_cc_blocks(Config, DistToBeforeLast).

bytes_literal(Bin) ->
    [_, _ | PinLit] = binary_to_list(aeu_hex:hexstring_encode(Bin)),
    "#" ++ PinLit.

% PINREFAC
pin_contract_call_tx(Config, PinProof, FromPubKey) ->
    Tx = hctest:contract_call(proplists:get_value(election_contract, Config), 
        hctest:src(?HC_CONTRACT, Config),
        "pin", [bytes_literal(PinProof)], 0, FromPubKey),

    NetworkId = proplists:get_value(network_id, Config),
    SignedTx = hctest:sign_tx(Tx, 
        hctest:privkey(hctest:who_by_pubkey(FromPubKey)), 
        NetworkId),
    rpc:call(?NODE1_NAME, aec_tx_pool, push, [SignedTx, tx_received]),
    ok.

% PINREFAC aec_parent_connector??
pin_to_parent(Node, PinningData, AccountPK) ->
    %AccPKEncEnc = aeser_api_encoder:encode(account_pubkey, AccountPK),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % no pending transactions
    PinTx = rpc(Node, aec_parent_connector, create_pin_tx, [AccountPK, AccountPK, 1, 30000 * ?DEFAULT_GAS_PRICE, PinningData]),
    SignedPinTx = hctest:sign_tx(PinTx, hctest:privkey(?DWIGHT),?PARENT_CHAIN_NETWORK_ID),
    rpc(Node, aec_parent_connector, post_pin_tx, [SignedPinTx]).

% PINREFAC
tx_hash_to_child(Node, EncTxHash, SendAccount, Leader, Config) ->
    NodeName = aecore_suite_utils:node_name(Node),
    NetworkId = proplists:get_value(network_id, Config),
    Nonce = hctest:next_nonce(Node, hctest:pubkey(SendAccount)),
    Params = #{ sender_id    => aeser_id:create(account, hctest:pubkey(SendAccount)),
                recipient_id => aeser_id:create(account, Leader),
                amount       => 1,
                fee          => 30000 * ?DEFAULT_GAS_PRICE,
                nonce        => Nonce,
                payload      => EncTxHash},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = hctest:sign_tx(Tx, hctest:privkey(SendAccount), NetworkId),
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    Hash = rpc:call(NodeName, aetx_sign, hash, [SignedTx]),
    Hash.

