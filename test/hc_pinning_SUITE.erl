-module(hc_pinning_SUITE).

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
-export([
    check_default_pin/1,
    get_pin/1,
    last_leader_validates_pin_and_post_to_contract/1,
    post_pin_to_pc/1,
    produce_first_epoch/1,
    start_two_child_nodes/1,
    wallet_post_pin_to_pc/1
]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("./hctest_defaults.hrl").

all() -> [{group, pinning}, {group, default_pin}].

groups() ->
    [
      {pinning, [sequence],
          [ start_two_child_nodes,
            produce_first_epoch,
            get_pin,
            wallet_post_pin_to_pc,
            post_pin_to_pc,
            last_leader_validates_pin_and_post_to_contract]}
    , {default_pin, [sequence],
          [ start_two_child_nodes,
            produce_first_epoch,
            check_default_pin]}
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

start_two_child_nodes(Config) ->
    [{Node1, NodeName1, Stakers1, Pinners1}, {Node2, NodeName2, Stakers2, Pinners2} | _] = ?config(nodes, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))} ],
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
    }, Config),
    aecore_suite_utils:start_node(Node1, Config, Env),
    aecore_suite_utils:connect(NodeName1, []),
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
    }, Config),
    aecore_suite_utils:start_node(Node2, Config, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

produce_first_epoch(Config) ->
    hctest:produce_n_epochs(Config, 1).

%%%=============================================================================
%%% Pinning
%%%=============================================================================

get_pin(Config) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),
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
    {ok, DecodedPin} = rpc(Node, aec_parent_connector, decode_parent_pin_payload, [Payload]),
    ?assertEqual(#{epoch => PrevEpoch, height => Height1, block_hash => BH1Dec},
                 DecodedPin),

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
    [{Node, _, _, _} | _] = ?config(nodes, Config),

    %% Get to first block in new epoch
    Height1 = hctest:get_height(Node),
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
    CH = hctest:get_height(Node),
    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]), % all transactions comitted
    DistToBeforeLast = Last - CH - 1,
    {ok, _} = produce_cc_blocks(Config, DistToBeforeLast), % produce blocks until last
    BL = Last - 1,
    BL = hctest:get_height(Node), % we're producing in last black
    ok.

%% A wallet posting a pin transaction by only using HTTP API towards Child and Parent
wallet_post_pin_to_pc(Config) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),

    %% Progress to first block of next epoch
    Height1 = hctest:get_height(?NODE1),
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

    Height2 = hctest:get_height(Node),
    {ok, #{last := CollectHeight}} = rpc(Node, aec_chain_hc, epoch_info, []),
    %% mine to CollectHeight and TODO: see that indeed the proof has been used
    {ok, _} = produce_cc_blocks(Config, CollectHeight - Height2),
    ok.

last_leader_validates_pin_and_post_to_contract(Config) ->
    [{Node, NodeName, _, _} | _] = ?config(nodes, Config),
    %% 1. Correct pin is posted in the contract

    #{cur_pin_reward := _Reward} = rpc(Node, aec_chain_hc , pin_reward_info, []),

    %% move into next epoch
    hctest:mine_to_next_epoch(Node, Config),
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

    hctest:mine_to_last_block_in_epoch(Node, Config),
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
    hctest:mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    % In last generation, but we don't post pin

    {ok, _} = produce_cc_blocks(Config, 2),
    {ok, #{info := {no_proof_posted}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% 3. Incorrect pin posted to contract a) bad tx hash

    hctest:mine_to_last_block_in_epoch(Node, Config),

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

    hctest:mine_to_last_block_in_epoch(Node, Config),

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

    hctest:mine_to_last_block_in_epoch(Node, Config),

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

    hctest:mine_to_last_block_in_epoch(Node, Config),

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

bytes_literal(Bin) ->
    [_, _ | PinLit] = binary_to_list(aeu_hex:hexstring_encode(Bin)),
    "#" ++ PinLit.

% PINREFAC
pin_contract_call_tx(Config, PinProof, FromPubKey) ->
    Tx = contract_call(?config(election_contract, Config), hctest:src(?HC_CONTRACT, Config),
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

next_nonce(Node, Pubkey) ->
    case rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

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

account_balance(Pubkey) ->
    case rpc(?NODE1, aec_chain, get_account, [Pubkey]) of
        {value, Account} -> aec_accounts:balance(Account);
        none -> no_such_account
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

%% Increase the child chain with a number of key blocks
%% Automatically add key blocks on parent chain and
%% if there are Txs, put them in a micro block
produce_cc_blocks(Config, BlocksCnt) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),
    TopHeight = hctest:get_height(Node),
    {ok, #{epoch := Epoch, first := First, last := Last, length := L} = Info} =
        rpc(Node, aec_chain_hc, epoch_info, [TopHeight]),
    ct:log("EpochInfo ~p", [Info]),
    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (Last - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div L),
    ParentTopHeight = hctest:get_height(?PARENT_CHAIN_NODE),
    ct:log("P@~p C@~p for next ~p child blocks", [ParentTopHeight, TopHeight,  BlocksCnt]),
    %% Spread parent blocks over BlocksCnt
    ParentProduce =
        lists:append([ hctest:spread(?PARENT_EPOCH_LENGTH, TopHeight,
                              [ {CH, 0} || CH <- lists:seq(First + E * L, Last + E * L)]) ||
                       E <- lists:seq(0, ScheduleUpto - Epoch) ]),
    %% Last parameter steers where in Child epoch parent block is produced
    produce_cc_blocks(Config, BlocksCnt, ParentProduce).

produce_cc_blocks(Config, BlocksCnt, ParentProduce) ->
    [{Node1, _, _, _} | _] = ?config(nodes, Config),
    %% The previous production ended with wait_same_top, so asking first node is sufficient
    TopHeight = hctest:get_height(Node1),
    %% assert that the parent chain is not mining
    ?assertEqual(stopped, rpc:call(?PARENT_CHAIN_NODE_NAME, aec_conductor, get_mining_state, [])),
    ct:log("parent produce ~p", [ParentProduce]),
    NewTopHeight = produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce),
    wait_same_top([ Node || {Node, _, _, _} <- ?config(nodes, Config)]),
    hctest:get_generations(Node1, TopHeight + 1, NewTopHeight).

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
                        hctest:mine_key_blocks(?PARENT_CHAIN_NODE_NAME, PBs),
                        PRest;
                    PP -> PP
                end,

            %% TODO: add some assertions when we expect an MB (and not)!
            {ok, _Txs} = rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]),

            %% This will mine 1 key-block (and 0 or 1 micro-blocks)
            {ok, Blocks} = hctest:mine_cc_blocks(NodeNames, 1),

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

            Producer = hctest:get_block_producer_name(?config(staker_names, Config), KeyBlock),
            ct:log("~p produced CC block at height ~p", [Producer, aec_blocks:height(KeyBlock)]),
            produce_to_cc_height(Config, TopHeight + 1, GoalHeight, NewParentProduce)
      end.
