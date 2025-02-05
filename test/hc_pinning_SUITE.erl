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
    produce_first_epoch/1,
    start_two_child_nodes/1,
    wallet_post_pin_to_pc/1
]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("./hctest_defaults.hrl").

all() -> [{group, pinning}, {group, default_pin}].

groups() -> [
    {pinning, [sequence], [
        start_two_child_nodes,
        produce_first_epoch,
        get_pin,
        wallet_post_pin_to_pc
    ]}
    , {default_pin, [sequence], [
        start_two_child_nodes,
        produce_first_epoch,
        check_default_pin
    ]}
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
    Nonce = hctest:next_nonce(?NODE1, From), %% no contract calls support for parent chain
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

base_child_config() ->
    #{
        receive_address_pub => ?FORD,
        hc_contract => ?HC_CONTRACT,
        child_epoch_length => ?CHILD_EPOCH_LENGTH,
        child_block_time => ?CHILD_BLOCK_TIME,
        child_block_production_time => ?CHILD_BLOCK_PRODUCTION_TIME,
        block_reward => ?BLOCK_REWARD,
        reward_delay => ?REWARD_DELAY
    }.

start_two_child_nodes(Config) ->
    hctest_ct_shared:start_child_nodes([?NODE1, ?NODE2], base_child_config(), Config).

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
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 1}),
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
    {ok, DecodedPin} = rpc(Node, aeser_hc, decode_parent_pin_payload, [Payload]),
    ?assertEqual(#{epoch => PrevEpoch, height => Height1, block_hash => BH1Dec},
                 DecodedPin),

    %% produce some more child blocks if we stay in same epoch, then pins should be the same
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 2}),
    {ok, 200, Repl2} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), get, "hyperchain/pin-tx", []),
    {ok, EpochInfo2} = rpc(?NODE1, aec_chain_hc, epoch_info, []),
    %% Get response from being in next Epoch
    Repl3 =
        if EpochInfo1 == EpochInfo2 ->
             ?assertEqual(Repl1, Repl2),
             {ok, _} = hctest:produce_cc_blocks(Config, #{count => maps:get(length, EpochInfo2) - 1}),
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

%% A wallet posting a pin transaction by only using HTTP API towards Child and Parent
wallet_post_pin_to_pc(Config) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),

    %% Progress to first block of next epoch
    Height1 = rpc(?NODE1, aec_chain, top_height, []),
    {ok, #{last := Last1, length := Len, epoch := Epoch}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, Bs} = hctest:produce_cc_blocks(Config, #{count => Last1 - Height1 + 1}),
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
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => Len div 2}),
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
    SignedProofTx = hctest:sign_tx(ProofTx, hctest:privkey(?ALICE), ?config(network_id, Config)),
    ProofTransaction = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedProofTx)),
    {ok, 200, _} = aecore_suite_utils:http_request(aecore_suite_utils:external_address(), post, <<"transactions">>, #{tx => ProofTransaction}),

    {ok, [_]} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 1}),
    {ok, []} = rpc(Node, aec_tx_pool, peek, [infinity]), % transactions in pool

    Height2 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := CollectHeight}} = rpc(Node, aec_chain_hc, epoch_info, []),
    %% mine to CollectHeight and TODO: see that indeed the proof has been used
    {ok, _} = hctest:produce_cc_blocks(Config, #{count => CollectHeight - Height2}),
    ok.

check_default_pin(Config) ->
    [{Node, NodeName, _, _} | _] = ?config(nodes, Config),

    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 12}),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    ct:log("Last Leader: ~p", [LastLeader]),

    hctest:mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 2}),
    %% with current test setup, all validators have a pc account, so pins will always happen(?)
    {ok, #{info := {pin_accepted, _}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% TODO test when not all validators have PC account, but how ensure
    %% that any given validator will be last leader within the run of the test???

    ok.

%%%=============================================================================
%%% Elections
%%%=============================================================================

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
    SignedTx = hctest:sign_tx(Tx, hctest:privkey(hctest:who_by_pubkey(FromPubKey)), NetworkId),
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
    NetworkId = ?config(network_id, Config),
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

mine_to_next_epoch(Node, Config) ->
    Height1 = rpc(Node, aec_chain, top_height, []),
    {ok, #{last := Last1, length := _Len}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, Bs} = hctest:produce_cc_blocks(Config, #{count => Last1 - Height1 + 1}),
    ct:log("Block last epoch: ~p", [Bs]).
