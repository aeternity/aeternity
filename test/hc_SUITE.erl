-module(hc_SUITE).

-import(aecore_suite_utils, [http_request/4, external_address/0, rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).


%% Test cases
-export([
    check_blocktime/1,
    check_finalize_info/1,
    correct_leader_in_micro_block/1,
    entropy_impact_schedule/1,
    epochs_with_fast_parent/1,
    epochs_with_slow_parent/1,
    first_leader_next_epoch/1,
    get_contract_pubkeys/1,
    produce_first_epoch/1,
    produce_some_epochs/1,
    respect_schedule/1,
    sanity_check_vote_tx/1,
    simple_withdraw/1,
    spend_txs/1,
    start_two_child_nodes/1,
    sync_third_node/1,
    verify_rewards/1
]).

-include_lib("stdlib/include/assert.hrl").
-include("./hctest_defaults.hrl").

all() -> [{group, hc}, {group, epochs}].

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
    hctest_ct_shared:init_per_suite(Config0, #{
        owner_pubkey => ?OWNER_PUBKEY,
        parent_chain_node => ?PARENT_CHAIN_NODE,
        parent_chain_network_id => ?PARENT_CHAIN_NETWORK_ID,
        parent_finality => ?PARENT_FINALITY,
        parent_epoch_length => ?PARENT_EPOCH_LENGTH,
        reward_delay => ?REWARD_DELAY,
        parent_account_seeds => #{ % Parent patron pubkey is added in the shared implementation
            hctest:encoded_pubkey(?DWIGHT) => 2_100000000_000000000_000000000,
            hctest:encoded_pubkey(?EDWIN) => 3_100000000_000000000_000000000
        },
        main_staking_contract => ?MAIN_STAKING_CONTRACT,
        staking_validator_contract => ?STAKING_VALIDATOR_CONTRACT,
        hc_contract => ?HC_CONTRACT,
        nodes_list => [?NODE1, ?NODE2]
    }).

end_per_suite(Config) ->
    hctest_ct_shared:end_per_suite(Config, [?NODE1, ?NODE2, ?NODE3, ?PARENT_CHAIN_NODE]).

init_per_group(Group, Config) ->
    hctest_ct_shared:init_per_group(Group, Config, #{
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
    Nodes = hctest:get_nodes(Config1),
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
    hctest:produce_cc_blocks(CTConfig, #{count => 1}),

    %% First, seed ALICE, BOB and LISA, they need tokens in later tests
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    NetworkId = proplists:get_value(network_id, CTConfig),
    hctest:seed_account(
        hctest:pubkey(?ALICE), 100000001 * ?DEFAULT_GAS_PRICE, NetworkId),
    hctest:seed_account(
        hctest:pubkey(?BOB), 100000002 * ?DEFAULT_GAS_PRICE, NetworkId),
    hctest:seed_account(
        hctest:pubkey(?LISA), 100000003 * ?DEFAULT_GAS_PRICE, NetworkId),

    hctest:produce_cc_blocks(CTConfig, #{count => 1}),
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
            hctest:produce_cc_blocks(Config, #{count => 1}),
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
    [{Node1, NodeName1, Stakers1, Pinners1}, {Node2, NodeName2, Stakers2, Pinners2} | _] = hctest:get_nodes(CTConfig),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(proplists:get_value(network_id, CTConfig))} ],

    hctest_ct_shared:child_node_config(#{
        node => Node1,
        stakeholders => Stakers1,
        pinners => Pinners1,
        receive_address_pub => ?FORD,
        hc_contract => ?HC_CONTRACT,
        child_epoch_length => ?CHILD_EPOCH_LENGTH,
        child_block_time => ?CHILD_BLOCK_TIME,
        child_block_production_time => ?CHILD_BLOCK_PRODUCTION_TIME,
        block_reward => ?BLOCK_REWARD,
        reward_delay => ?REWARD_DELAY
    }, CTConfig),
    aecore_suite_utils:start_node(Node1, CTConfig, Env),
    aecore_suite_utils:connect(NodeName1, []),
    rpc(Node1, aec_conductor, start_mining, []),

    hctest_ct_shared:child_node_config(#{
        node => Node2,
        stakeholders => Stakers2,
        pinners => Pinners2,
        receive_address_pub => ?FORD,
        hc_contract => ?HC_CONTRACT,
        child_epoch_length => ?CHILD_EPOCH_LENGTH,
        child_block_time => ?CHILD_BLOCK_TIME,
        child_block_production_time => ?CHILD_BLOCK_PRODUCTION_TIME,
        block_reward => ?BLOCK_REWARD,
        reward_delay => ?REWARD_DELAY
    }, CTConfig),
    aecore_suite_utils:start_node(Node2, CTConfig, Env),
    aecore_suite_utils:connect(NodeName2, []),
    rpc(Node2, aec_conductor, start_mining, []),
    ok.

produce_first_epoch(Config) ->
    %% Produce on parent chain before we begin on child chain
    hctest:produce_pc_blocks(12),
    hctest:produce_n_epochs(Config, 1).

produce_some_epochs(Config) ->
    hctest:produce_n_epochs(Config, 5).

respect_schedule(Config) ->
    [{Node, _, _, _}|_] = hctest:get_nodes(Config),
    ChildHeight = hctest:get_height(Node),
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
    Nodes = [ N || {N, _, _, _} <- hctest:get_nodes(Config)],
    Node = hd(Nodes),
    %% Sync nodes
    ChildHeight = hctest:get_height(Node),
    {ok, #{epoch := Epoch0, length := Length0}} = rpc(Node, aec_chain_hc, epoch_info, []),
    %% Make sure chain is long enough
    case Epoch0 =< 5 of
      true ->
        %% Chain to short to have meaningful test, e.g. when ran in isolation
        hctest:produce_cc_blocks(Config, #{count => 5 * Length0 - ChildHeight});
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
    [{_Node, NodeName, _, _} | _] = hctest:get_nodes(Config),
    NetworkId = proplists:get_value(network_id, Config),

    %% Not at the start of the epoch
    hctest:produce_cc_blocks(Config, #{count => 2}),

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
    hctest:produce_cc_blocks(Config, #{count => 1}),
    {ok, CallRes1} = hctest:call_info(CallTx1),
    {ok, _Res1} = hctest:decode_consensus_result(CallRes1, "adjust_stake", ValidatorStub),

    %% Ok, should still be 0
    hctest:produce_cc_blocks(Config, #{count => 1}),
    {ok, 0} = hctest:inspect_validator(AliceCt, ?ALICE, get_available_balance, Config),

    %% Let's advance 5 epochs...
    hctest:produce_n_epochs(Config, 5),

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
    hctest:produce_cc_blocks(Config, #{count => StakeWithdrawDelay}),
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
    [{_Node, NodeName, _, _} | _] = hctest:get_nodes(Config),
    %% Call the contract in a transaction, asking for "leader"
    {ok, [_]} = hctest:produce_cc_blocks(Config, #{count => 1}),
    CallTx = hctest:sign_and_push(
        NodeName,
        hctest:contract_call(
            proplists:get_value(election_contract, Config),
                hctest:src(?HC_CONTRACT, Config),
            "leader", [], 0, hctest:pubkey(?ALICE)),
        ?ALICE, proplists:get_value(network_id, Config)),

    {ok, [KeyBlock, _MicroBlock]} = hctest:produce_cc_blocks(Config, #{count => 1}),
    %% Microblock contains the contract call, find out what it returned on that call
    {ok, Call} = hctest:call_info(CallTx),
    {ok, Res} = hctest:decode_consensus_result(Call, "leader",
        hctest:src(?HC_CONTRACT, Config)),
    %% The actual leader did produce the keyblock (and micro block)
    Producer = aeser_api_encoder:encode(account_pubkey, aec_blocks:miner(KeyBlock)),
    ?assertEqual(Producer, Res).

set_up_third_node(Config) ->
    {Node3, NodeName, Stakers, _Pinners} = lists:keyfind(?NODE3, 1, hctest:get_nodes(Config)),
    Nodes = [ Node || {Node, _, _, _} <- hctest:get_nodes(Config)],
    aecore_suite_utils:make_multi(Config, [Node3]),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(proplists:get_value(network_id, Config))} ],
    hctest_ct_shared:child_node_config(#{
        node => Node3,
        stakeholders => Stakers,
        pinners => [],
        receive_address_pub => ?FORD,
        hc_contract => ?HC_CONTRACT,
        child_epoch_length => ?CHILD_EPOCH_LENGTH,
        child_block_time => ?CHILD_BLOCK_TIME,
        child_block_production_time => ?CHILD_BLOCK_PRODUCTION_TIME,
        block_reward => ?BLOCK_REWARD,
        reward_delay => ?REWARD_DELAY
    }, Config), % no pinners here FTM
    aecore_suite_utils:start_node(Node3, Config, Env),
    aecore_suite_utils:connect(NodeName, []),
    rpc(Node3, aec_conductor, start_mining, []),

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

sanity_check_vote_tx(Config) ->
    [{Node1, _, _, _}, {Node2, _, _, _} | _] = hctest:get_nodes(Config),

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
    [{Node, _NodeName, _, _} | _ ] = hctest:get_nodes(Config),
    NetworkId = proplists:get_value(network_id, Config),
    {_, PatronPub} = aecore_suite_utils:sign_keys(Node),

    Height = hctest:get_height(Node),
    {ok, #{first := First0}} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% grab Alice's and Bob's staking validator contract
    AliceCt = fetch_validator_contract(?ALICE, Config),
    BobCt = fetch_validator_contract(?BOB, Config),
    LisaCt = fetch_validator_contract(?LISA, Config),

    %% Assert we are at the beginning of an epoch
    [ hctest:mine_to_next_epoch(Node, Config) || Height + 1 /= First0 ],
    {ok, EpochInfo} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% Now fill this generation with known stuff
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 2}),

    {ok, SignedTx1} = hctest:seed_account(PatronPub, 1, NetworkId),
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 1}),
    ?assertEqual(aetx:fee(aetx_sign:tx(SignedTx1)), ?FEE_REWARD),

    {ok, _SignedTx2} = hctest:seed_account(PatronPub, 1, NetworkId),
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 1}),

    %% Now skip to the next-next epoch where rewards should have been payed
    hctest:mine_to_next_epoch(Node, Config),
    hctest:mine_to_last_block_in_epoch(Node, Config),

    %% Record the state
    {ok, AliceTot0} = hctest:inspect_validator(AliceCt, ?ALICE, get_total_balance, Config),
    {ok, BobTot0} = hctest:inspect_validator(BobCt, ?BOB, get_total_balance, Config),
    {ok, LisaTot0} = hctest:inspect_validator(LisaCt, ?LISA, get_total_balance, Config),

    %% Produce final block to distribute rewards.
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 1}),

    %% Record the new state
    {ok, AliceTot1} = hctest:inspect_validator(AliceCt, ?ALICE, get_total_balance, Config),
    {ok, BobTot1} = hctest:inspect_validator(BobCt, ?BOB, get_total_balance, Config),
    {ok, LisaTot1} = hctest:inspect_validator(LisaCt, ?LISA, get_total_balance, Config),

    #{first := First, last := Last} = EpochInfo,
    {ok, BlocksInGen} = hctest:get_generations(Node, First, Last),

    Rewards = calc_rewards(BlocksInGen, Node),

    ct:log("Alice ~p -> ~p expected reward ~p", [AliceTot0, AliceTot1, maps:get(hctest:pubkey(?ALICE), Rewards, 0)]),
    ct:log("Bob ~p -> ~p expected reward ~p", [BobTot0, BobTot1, maps:get(hctest:pubkey(?BOB_SIGN), Rewards, 0)]),
    ct:log("Lisa ~p -> ~p expected reward ~p", [LisaTot0, LisaTot1, maps:get(hctest:pubkey(?LISA), Rewards, 0)]),
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

% block_difficulty(Config) ->
%     lists:foreach(
%         fun(_) ->
%             {ok, [KB]} = hctest:produce_cc_blocks(Config, #{count => 1}),
%             {ok, AddedStakingPower} = hctest:inspect_election_contract(?ALICE, current_added_staking_power, Config),
%             Target = aec_blocks:target(KB),
%             {Target, Target} = {Target, aeminer_pow:integer_to_scientific(AddedStakingPower)}
%         end,
%         lists:seq(1, 20)), %% test with 20 elections
%     ok.

% elected_leader_did_not_show_up(Config) ->
%     case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
%         true ->
%             {skip, lazy_leader_sync_broken_on_iris};
%         false ->
%             elected_leader_did_not_show_up_(Config)
%     end.

% elected_leader_did_not_show_up_(Config) ->
%     aecore_suite_utils:stop_node(?NODE1, Config), %% stop the block producer
%     TopHeader0 = rpc(?NODE2, aec_chain, top_header, []),
%     {TopHeader0, TopHeader0} = {rpc(?NODE3, aec_chain, top_header, []), TopHeader0},
%     ct:log("Starting test at (child chain): ~p", [TopHeader0]),
%     %% produce a block on the parent chain
%     hctest:produce_cc_blocks(Config, 1),
%     {ok, KB} = hctest:wait_same_top([?NODE2, ?NODE3]),
%     0 = aec_blocks:difficulty(KB),
%     TopHeader1 = rpc(?NODE3, aec_chain, top_header, []),
%     ct:log("Lazy header: ~p", [TopHeader1]),
%     TopHeader1 = rpc(?NODE2, aec_chain, top_header, []),
%     NetworkId = proplists:get_value(network_id, Config),
%     Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)} ],
%     aecore_suite_utils:start_node(?NODE1, Config, Env),
%     aecore_suite_utils:connect(?NODE1_NAME, []),
%     hctest:produce_cc_blocks(Config, #{count => 1}),
%     {ok, _} = hctest:wait_same_top([?NODE1, ?NODE3]),
%     timer:sleep(2000), %% Give NODE1 a moment to finalize sync and post commitments
%     hctest:produce_cc_blocks(Config, #{count => 1}),
%     {ok, _KB1} = hctest:wait_same_top([ Node || {Node, _, _, _} <- hctest:get_nodes(Config)]),
%     {ok, _} = hctest:produce_cc_blocks(Config, #{count => 10}),
%     {ok, _KB2} = hctest:wait_same_top([ Node || {Node, _, _, _} <- hctest:get_nodes(Config)]),
%     ok.

first_leader_next_epoch(Config) ->
    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
    hctest:produce_cc_blocks(Config, #{count => 1}),
    StartHeight = hctest:get_height(Node),
    {ok, #{last := Last, epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, [StartHeight]),
    ct:log("Checking leader for first block next epoch ~p (height ~p)", [Epoch+1, Last+1]),
    ?assertMatch({ok, _}, rpc(Node, aec_consensus_hc, leader_for_height, [Last + 1])).

%% Demonstrate that child chain start signalling epoch length adjustment upward
%% When parent blocks are produced too slowly, we need to lengthen child epoch
epochs_with_slow_parent(Config) ->
    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
    ct:log("Parent start height = ~p", [proplists:get_value(parent_start_height, Config)]),
    %% ensure start at a new epoch boundary
    StartHeight = hctest:get_height(Node),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, [StartHeight]),
    BlocksLeftToBoundary = Last - StartHeight,
    ct:log("Starting at CC height ~p: producing ~p cc blocks", [StartHeight, BlocksLeftToBoundary]),
    %% some block production including parent blocks
    hctest:produce_cc_blocks(Config, #{count => BlocksLeftToBoundary}),

    ParentHeight = hctest:get_height(?PARENT_CHAIN_NODE),
    ct:log("Child continues while parent stuck at: ~p", [ParentHeight]),
    ParentEpoch = (ParentHeight - proplists:get_value(parent_start_height, Config) +
                      (?PARENT_EPOCH_LENGTH - 1)) div ?PARENT_EPOCH_LENGTH,
    ChildEpoch = hctest:get_height(Node) div ?CHILD_EPOCH_LENGTH,
    ct:log("Child epoch ~p while parent epoch ~p (parent should be in next epoch)", [ChildEpoch, ParentEpoch]),
    ?assertEqual(1, ParentEpoch - ChildEpoch),

    Resilience = 1, %% Child can cope with missing Resilience epochs in parent chain
    %% Produce no parent block in the next Resilience child epochs
    %% the child chain should get to a halt or
    %% at least one should be able to observe signalling that the length should be adjusted upward
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => ?CHILD_EPOCH_LENGTH*Resilience}),

    ct:log("Mined almost ~p additional child epochs without parent progress", [Resilience]),
    ActualParentHeight = hctest:get_height(?PARENT_CHAIN_NODE),
    ?assert(ActualParentHeight > ParentHeight,
        hctest:format("Parent must have reached at least height of ~w (got ~w)",
        [ParentHeight, ActualParentHeight])),

    ChildTopHeight = hctest:get_height(Node),
    {ok, #{epoch := EndEpoch} = EpochInfo} = rpc(Node, aec_chain_hc, epoch_info, [ChildTopHeight]),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ActualParentHeight, ChildTopHeight, EndEpoch]),

    %% Here we should have observed some signalling for increased child epoch length

    %% Parent hash grabbed in last block child epoch, so here we can start, but not finish next epoch
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => maps:get(length, EpochInfo) - 1}),
    ?assertException(error, timeout_waiting_for_block, hctest:produce_cc_blocks(Config, #{count => 1})),

    ?assertEqual([{ok, (N-1) * ?CHILD_EPOCH_LENGTH + 1} || N <- lists:seq(1, EndEpoch)],
                 [rpc(Node, aec_chain_hc, epoch_start_height, [N]) || N <- lists:seq(1, EndEpoch)]),
    ok.

%% Demonstrate that child chain start signalling epoch length adjustment downward
%% When parent blocks are produced too quickly, we need to shorten child epoch
epochs_with_fast_parent(Config) ->
    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
    ParentTopHeight = hctest:get_height(?PARENT_CHAIN_NODE),
    ChildTopHeight = hctest:get_height(Node),
    {ok, #{epoch := ChildEpoch}} = rpc(Node, aec_chain_hc, epoch_info, []),

    %% Quickly produce parent blocks to be in sync again
    ParentBlocksNeeded =
        ChildEpoch * ?PARENT_EPOCH_LENGTH + proplists:get_value(parent_start_height, Config) + ?PARENT_FINALITY - ParentTopHeight,

    %% Produce ?PARENT_EPOCH_LENGTH parent blocks quickly (very artificial)
    {ok, _} = hctest:produce_cc_blocks(Config, 1, [{ChildTopHeight + 1, ParentBlocksNeeded}]),
    %% and finish a child epoch
    %% ensure start at a new epoch boundary
    StartHeight = hctest:get_height(Node),
    {ok, #{last := Last, length := Len} = EpochInfo1} = rpc(Node, aec_chain_hc, epoch_info, []),
    ct:log("Info ~p", [EpochInfo1]),
    BlocksLeftToBoundary = Last - StartHeight,
    %% some block production including parent blocks
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => BlocksLeftToBoundary}),

    %% Produce twice as many parent blocks as needed in an epoch
    Height0 = hctest:get_height(Node),
    ParentTopHeight0 = hctest:get_height(?PARENT_CHAIN_NODE),
    {ok, _} = hctest:produce_cc_blocks(
        Config, #{
            count => ?CHILD_EPOCH_LENGTH,
            expected_schedule => hctest:spread(
                2*?PARENT_EPOCH_LENGTH, Height0,
                [ {CH, 0} || CH <- lists:seq(Height0 + 1, Height0 + Len)]
            )
        }),

    ParentTopHeight1 = hctest:get_height(?PARENT_CHAIN_NODE),
    Height1 = hctest:get_height(Node),
    {ok, EpochInfo2} = rpc(Node, aec_chain_hc, epoch_info, []),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight1, Height1, EpochInfo2 ]),
    ?assertEqual(2*?PARENT_EPOCH_LENGTH, ParentTopHeight1 - ParentTopHeight0),

    {ok, _} = hctest:produce_cc_blocks(
        Config, #{
            count => ?CHILD_EPOCH_LENGTH,
            expected_schedule => hctest:spread(
                2*?PARENT_EPOCH_LENGTH, Height1,
                [ {CH, 0} || CH <- lists:seq(Height1 + 1, Height1 + Len)]
            )
        }),

    %% Here we should be able to observe signalling that epoch should be shorter
    ok.

%%%=============================================================================
%%% HC Endpoints
%%%=============================================================================

get_contract_pubkeys(Config) ->
    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
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
%%% Elections
%%%=============================================================================

check_finalize_info(Config) ->
    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
    hctest:mine_to_next_epoch(Node, Config),
    {ok, #{epoch  := Epoch,
           last   := Last,
           validators := Validators,
           length := _Length}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    hctest:mine_to_last_block_in_epoch(Node, Config),
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 2}),

    #{ producer := Producer, epoch := FEpoch, votes := Votes
    } = case rpc(Node, aec_chain_hc, finalize_info, []) of
        undefined -> ?assert(false, "aec_chain_hc:finalize_info returned undefined");
        Result -> Result
    end,

    FVoters = lists:map(fun(#{producer := Voter}) -> Voter end, Votes),
    TotalStake = lists:foldl(fun({_, Stake}, Accum) -> Stake + Accum end, 0, Validators),
    VotersStake = lists:foldl(fun(Voter, Accum) -> proplists:get_value(Voter, Validators) + Accum end, 0, FVoters),
    TotalVotersStake = proplists:get_value(LastLeader, Validators) + VotersStake,
    ?assertEqual(Epoch, FEpoch),
    ?assertEqual(Producer, LastLeader),
    ?assert(TotalVotersStake >= trunc(math:ceil((2 * TotalStake) / 3))).
