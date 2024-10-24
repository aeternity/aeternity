-module(hctest_forks_SUITE).

-export([
    all/0,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    start_two_child_nodes/1,
    produce_first_epoch/1,
    produce_1_cc_block/1,
    produce_1_cc_block_late/1,
    produce_3_cc_blocks/1,
    spend_txs_late_producing/1,
    verify_consensus_solution_late_block/1,
    verify_consensus_solution_netsplit/1
]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("../../aecontract/include/hard_forks.hrl").
% -include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include("../include/hc_test.hrl").

all() ->
    [
        {group, late_producing},
        {group, producing_two_sequential_blocks},
        {group, producing_a_fork},
        {group, netsplit},
        {group, combinatoric_explosion},
        {group, eoe_netsplit},
        {group, eoe_double_vote},
        {group, eoe_inactive_producer},
        {group, eoe_ignored_vote},
        {group, eoe_finalizing_txn},
        {group, bad_block}
    ].

groups() ->
    [
        {producing_a_fork, [sequence], [
            %% - Two (key) blocks at the same height signed by the same producer
            %% Same as double_mining - do we penalize or ignore?
        ]},
        {netsplit, [sequence], [
            %% Create a network split in the middle of a block
            start_two_child_nodes,
            produce_first_epoch,
            start_netsplit,
            produce_3_cc_blocks,
            finish_netsplit,
            verify_consensus_solution_netsplit
        ]},
        {combinatoric_explosion, [sequence], [
            %% Worst case scenario when each validator creates a fork on every block, making it V^B count of forks
        ]}
    ].

%% Test suite TO DO:x
%%
%% Malicious (and disfunctional) nodes
%% - Disappearing leader
%% - 51% attack?
%% - Producing multiple blocks in a time slot is not allowed
%%
%% Chain Attacks and Penalties
%%
%% Which activities are to be detected and penalized? Ignore cases which should be detected by the gossiping protocol.
%% - Double spending: Attempting to spend the same funds multiple times
%% - Sybil attack: Creating multiple fake identities to gain disproportionate influence
%%   - Allowed in PoS (or does the random node distribution math say otherwise? TODO: the math)
%% - Eclipse attack: Isolating a node from the rest of the network
%%   - No penalty, should be resolved by the consensus protocol
%% - Long-range attack: Rewriting a long history of the blockchain
%% - Spam transactions: Flooding the network with useless transactions
%%   - There's a cost to posting a txn?]
%% - Invalid block propagation: Spreading blocks that don't follow consensus rules
%%   - Gossiping and consensus protocol should prevent this?
%% - Timejacking: Manipulating a node's time to affect block acceptance
%%   - There should be some sort of time and timeslot verification in the gossiping and consensus protocol
%% - Txn manipulation (address substitution)
%% - Routing attacks: Bad actors can intercept data over the group of network nodes, preventing the chain from reaching consensus.
%%
%% Halting
%% - The chain will stop when there aren't enough online nodes to serve as producers
%%
%% To Implement:
%% - TODO: A way to configure the gossip connections so that the test will have a guaranteeed initial setup
%% - Sync connections?
%% - IMPORTANT: When testing always assume all nodes are on the same height (apart from the last blocks might be different or empty?)
%%
%% Penalizable Offences
%% - Two (key) blocks at the same height signed by the same producer
%% - Double voting (two different votes in the voting process), double proposal, double anything else
%% - [explanation?] Attesting to a block that surrounds another one (changing the history)?
%% - Missing vote (even if offline)
%% - Producing a bad block that does not pass the checks
%%
suite() -> [].

init_per_suite(Config0) ->
    hctest_shared_test_steps:init_per_suite(Config0, [?NODE1, ?NODE2]).

end_per_suite(Config) ->
    hctest_shared_test_steps:end_per_suite(Config, [?NODE1, ?NODE2, ?NODE3]).

init_per_group(_GroupName, Config0) ->
    hctest_shared_test_steps:init_per_group(Config0).

end_per_group(_Group, Config) ->
    hctest_shared_test_steps:end_per_group(Config).

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [
            {nodes, [
                {?NODE1, ?NODE1_NAME, [?ALICE, ?LISA]},
                {?NODE2, ?NODE2_NAME, [?BOB]}
            ]}
            | Config
        ],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = hctest_setup:with_saved_keys([nodes], Config),
    Nodes = ?config(nodes, Config1),
    Config2 = lists:keyreplace(
        nodes,
        1,
        Config1,
        {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, []}]}
    ),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = hctest_setup:with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

start_netsplit(Config) ->
    %% Use aec_peers:block_peer and maybe need to disconnect?
    %% See if the block is not propagated to other nodes, and is strictly local. It is currently propagated via gproc
    ok.

finish_netsplit(Config) ->
    %% Use aec_peers:unblock_peer
    ok.

verify_consensus_solution_netsplit(Config) ->
    %% TODO: verify that the chain split is detected and the correct chain is continued
    %% TODO: Check with the whitepaper the correct behaviour
    %% Use aec_chain to read last block and check if it is the correct one
    %% Some of the elected leaders will be in the other split, so there will be empty blocks
    ok.

%% Test step: Produce 1 block in the child chain, sync nodes
produce_1_cc_block(Config) ->
    {ok, _KBs} = hctest_utils:produce_cc_blocks(Config, 1),
    {ok, _KB} = hctest_utils:wait_same_top([Node || {Node, _, _} <- ?config(nodes, Config)]),
    ok.

%% Test step: Produce 3 blocks in the child chain, sync nodes
produce_3_cc_blocks(Config) ->
    {ok, _KBs} = hctest_utils:produce_cc_blocks(Config, 3),
    {ok, _KB} = hctest_utils:wait_same_top([Node || {Node, _, _} <- ?config(nodes, Config)]),
    ok.

%% Test step: Produce 1 block, but late out of their time slot, sync the child chain
produce_1_cc_block_late(Config) ->
    {ok, _KBs} = hctest_utils:produce_cc_blocks(Config, 1),
    {ok, _KB} = hctest_utils:wait_same_top([Node || {Node, _, _} <- ?config(nodes, Config)]),
    ok.

%% Add one transaction to create a micro block
%% The current leader is requested to mine late outside of their time slot
%% The expected result: The late block is not accepted, and the other leader has produced one
spend_txs_late_producing(Config) ->
    Top0 = aecore_suite_utils:rpc(?NODE1, aec_chain, top_header, []),
    ct:log("Top before posting spend txs: ~p", [aec_headers:height(Top0)]),
    NetworkId = ?config(network_id, Config),
    % elp:ignore W0014 (cross_node_eval)
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    seed_account(hctest_utils:pubkey(?ALICE), 100000001 * ?DEFAULT_GAS_PRICE, NetworkId),
    % seed_account(pubkey(?BOB), 100000002 * ?DEFAULT_GAS_PRICE, NetworkId),
    % seed_account(pubkey(?LISA), 100000003 * ?DEFAULT_GAS_PRICE, NetworkId),

    hctest_utils:produce_cc_blocks(Config, 1),
    % elp:ignore W0014 (cross_node_eval)
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    %% TODO check that the actors got their share
    ok.

start_two_child_nodes(Config) ->
    [{Node1, NodeName1, Stakers1}, {Node2, NodeName2, Stakers2} | _] = ?config(nodes, Config),
    Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))}],
    hctest_utils:child_node_config(Node1, Stakers1, Config),
    aecore_suite_utils:start_node(Node1, Config, Env),
    aecore_suite_utils:connect(NodeName1, []),
    hctest_utils:child_node_config(Node2, Stakers2, Config),
    aecore_suite_utils:start_node(Node2, Config, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

empty_parent_block(_Config) ->
    case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
        true ->
            {skip, lazy_leader_sync_broken_on_iris};
        false ->
            %% empty_parent_block_(Config)
            {skip, todo}
    end.

produce_first_epoch(Config) ->
    produce_n_epochs(Config, 1).

produce_n_epochs(Config, N) ->
    [{Node1, _, _} | _] = ?config(nodes, Config),
    %% produce blocks
    {ok, Bs} = hctest_utils:produce_cc_blocks(Config, N * ?CHILD_EPOCH_LENGTH, undefined),
    %% check producers
    Producers = [aec_blocks:miner(B) || B <- Bs],
    ChildTopHeight = aecore_suite_utils:rpc(Node1, aec_chain, top_height, []),
    Leaders = leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    ParentTopHeight = aecore_suite_utils:rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, ParentBlocks} = hctest_utils:get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = hctest_utils:get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

leaders_at_height(Node, Height, Config) ->
    {ok, Hash} = aecore_suite_utils:rpc(Node, aec_chain_state, get_key_block_hash_at_height, [
        Height
    ]),
    {ok, Return} = inspect_staking_contract(?ALICE, leaders, Config, Hash),
    [
        begin
            {account_pubkey, K} = aeser_api_encoder:decode(LeaderKey),
            K
        end
     || [LeaderKey, _LeaderStake] <- Return
    ].

inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            {staking_power, Who} ->
                {"staking_power", [binary_to_list(hctest_utils:encoded_pubkey(Who))]};
            {get_validator_state, Who} ->
                {"get_validator_state", [binary_to_list(hctest_utils:encoded_pubkey(Who))]};
            get_state ->
                {"get_state", []};
            leaders ->
                {"sorted_validators", []}
        end,
    ContractPubkey = ?config(staking_contract, Config),
    do_contract_call(
        ContractPubkey,
        hctest_utils:src(?MAIN_STAKING_CONTRACT, Config),
        Fun,
        Args,
        OriginWho,
        TopHash
    ).

do_contract_call(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    F = fun() -> do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) end,
    {T, Res} = timer:tc(F),
    ct:log("Calling contract took ~.2f ms", [T / 1000]),
    Res.

do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    Tx = hctest_utils:contract_call(CtPubkey, CtSrc, Fun, Args, 0, hctest_utils:pubkey(Who)),
    {ok, Call} = dry_run(TopHash, Tx),
    hctest_utils:decode_consensus_result(Call, Fun, CtSrc).

verify_consensus_solution_late_block(Config) ->
    %% TODO: verify that the late block is not accepted and some other leader stepped in
    %% Use aec_chain to read
    ok.

%%%--------------------------------------------------------------------
%%% Helper functions
%%%--------------------------------------------------------------------

elected_leader_did_not_show_up(Config) ->
    case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
        true ->
            {skip, lazy_leader_sync_broken_on_iris};
        false ->
            elected_leader_did_not_show_up_(Config)
    end.

elected_leader_did_not_show_up_(Config) ->
    %% stop the block producer
    aecore_suite_utils:stop_node(?NODE1, Config),
    TopHeader0 = aecore_suite_utils:rpc(?NODE2, aec_chain, top_header, []),
    {TopHeader0, TopHeader0} = {
        aecore_suite_utils:rpc(?NODE3, aec_chain, top_header, []), TopHeader0
    },
    ct:log("Starting test at (child chain): ~p", [TopHeader0]),
    %% produce a block on the parent chain
    hctest_utils:produce_cc_blocks(Config, 1),
    {ok, KB} = hctest_utils:wait_same_top([?NODE2, ?NODE3]),
    0 = aec_blocks:difficulty(KB),
    TopHeader1 = aecore_suite_utils:rpc(?NODE3, aec_chain, top_header, []),
    ct:log("Lazy header: ~p", [TopHeader1]),
    TopHeader1 = aecore_suite_utils:rpc(?NODE2, aec_chain, top_header, []),
    NetworkId = ?config(network_id, Config),
    Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}],
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    hctest_utils:produce_cc_blocks(Config, 1),
    {ok, _} = hctest_utils:wait_same_top([?NODE1, ?NODE3]),
    %% Give NODE1 a moment to finalize sync and post commitments
    timer:sleep(2000),
    hctest_utils:produce_cc_blocks(Config, 1),
    {ok, _KB1} = hctest_utils:wait_same_top([Node || {Node, _, _} <- ?config(nodes, Config)]),
    {ok, _} = hctest_utils:produce_cc_blocks(Config, 10),
    {ok, _KB2} = hctest_utils:wait_same_top([Node || {Node, _, _} <- ?config(nodes, Config)]),
    ok.

dry_run(TopHash, Tx) ->
    case aecore_suite_utils:rpc(?NODE1, aec_dry_run, dry_run, [TopHash, [], [{tx, Tx}]]) of
        {error, _} = Err -> Err;
        {ok, {[{contract_call_tx, {ok, Call}}], _Events}} -> {ok, Call}
    end.

seed_account(RecipientPubkey, Amount, NetworkId) ->
    seed_account(?NODE1, RecipientPubkey, Amount, NetworkId).

seed_account(Node, RecipientPubkey, Amount, NetworkId) ->
    NodeName = aecore_suite_utils:node_name(Node),
    {PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(Node),
    Nonce = hctest_utils:next_nonce(Node, PatronPub),
    Params =
        #{
            sender_id => aeser_id:create(account, PatronPub),
            recipient_id => aeser_id:create(account, RecipientPubkey),
            amount => Amount,
            fee => 30000 * ?DEFAULT_GAS_PRICE,
            nonce => Nonce,
            payload => <<>>
        },
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, PatronPriv, NetworkId),
    % elp:ignore W0014 (cross_node_eval)
    ok = rpc:call(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, SignedTx}.

%% usually we would use aec_test_utils:sign_tx/3. This function is being
%% executed in the context of the CT test and uses the corresponding
%% network_id. Since the network_id of the HC node is different, we must sign
%% the tx using the test-specific network_id
sign_tx(Tx, Privkey, NetworkId) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    %% since we are in CERES context, we sign th hash
    Bin = aec_hash:hash(signed_tx, Bin0),
    BinForNetwork = <<NetworkId/binary, Bin/binary>>,
    Signatures = [enacl:sign_detached(BinForNetwork, Privkey)],
    aetx_sign:new(Tx, Signatures).
