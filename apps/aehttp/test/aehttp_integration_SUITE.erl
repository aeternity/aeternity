-module(aehttp_integration_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").
%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export(
   [
    get_top_block/1
   ]).

-export(
   [
    get_current_key_block/1,
    get_current_key_block_hash/1,
    get_current_key_block_height/1,
    get_pending_key_block/1,
    get_key_block_by_hash/1,
    get_key_block_by_height/1
   ]).

-export(
   [
    get_micro_block_header_by_hash/1,
    get_micro_block_transactions_by_hash/1,
    get_micro_block_transactions_count_by_hash/1,
    get_micro_block_transaction_by_hash_and_index/1
   ]).

-export(
    [
     get_generation_current/1,
     get_generation_by_hash/1,
     get_generation_by_height/1
    ]).

-export(
   [
    get_account_by_pubkey/1,
    get_pending_account_transactions_by_pubkey/1
   ]).

-export(
   [
    get_transaction_by_hash/1,
    get_transaction_info_by_hash/1,
    post_spend_tx/1,
    post_contract_and_call_tx/1
   ]).

-export(
   [
    get_contract/1
   ]).

-export(
   [
    post_oracle_register/1,
    get_oracle_by_pubkey/1,
    post_oracle_extend/1,
    post_oracle_query/1,
    post_oracle_response/1
   ]).

-export(
   [
    get_name_entry_by_name/1
   ]).

-export(
   [
    get_channel_by_pubkey/1
   ]).

-export(
   [
    get_peer_pubkey/1
   ]).

-export(
   [
    get_status/1
   ]).

%% off chain endpoints
-export(
   [test_decode_sophia_data/1,
    test_decode_sophia_data2/1,
    broken_decode_sophia_data/1
   ]).

%% test case exports
%% external endpoints
-export(
   [
    % non signed txs
    contract_transactions/1,
    contract_create_compute_transaction/1,
    contract_create_transaction_init_error/1,
    oracle_transactions/1,
    nameservice_transactions/1,
    spend_transaction/1,
    state_channels_onchain_transactions/1,
    unknown_atom_in_spend_tx/1,

    get_transaction/1,

    % sync gossip
    pending_transactions/1,
    post_correct_tx/1,
    post_broken_tx/1,
    post_broken_base58_tx/1,

    % infos
    peer_pub_key/1
   ]).

%%
%% test case exports
%% internal endpoints
-export(
   [
    broken_spend_tx/1,
    node_pubkey/1,

    %% requested Endpoints
    naming_system_manage_name/1,
    naming_system_broken_txs/1,

    peers/1
   ]).

%% test case exports
%% for swagger validation errors
-export([
    swagger_validation_body/1,
    %% swagger_validation_enum/1,
    %%swagger_validation_required/1,
    swagger_validation_schema/1
    %% TODO: validate that API expects some type but gets
    %% a different type
    %%swagger_validation_types/1

    ]).

%%
%% test case exports
%% wrong http method for all endpoints
-export([
    wrong_http_method_top/1,
    wrong_http_method_contract_create/1,
    wrong_http_method_contract_create_compute/1,
    wrong_http_method_contract_call/1,
    wrong_http_method_contract_call_compute/1,
    wrong_http_method_spend/1,
    wrong_http_method_oracle_register/1,
    wrong_http_method_oracle_extend/1,
    wrong_http_method_oracle_query/1,
    wrong_http_method_oracle_response/1,
    wrong_http_method_name_preclaim/1,
    wrong_http_method_name_claim/1,
    wrong_http_method_name_transfer/1,
    wrong_http_method_name_revoke/1,
    wrong_http_method_pending_transactions/1,
    wrong_http_method_tx_id/1,
    wrong_http_method_commitment_hash/1,
    wrong_http_method_name/1,
    wrong_http_method_tx/1,
    wrong_http_method_node_pubkey/1,
    wrong_http_method_peers/1
    ]).

%%
%% test case exports
%% websocket endpoints
-export(
   [ws_get_genesis/1,
    ws_request_tag/1,
    ws_block_mined/1,
    ws_micro_block_added/1,
    ws_refused_on_limit_reached/1,
    ws_tx_on_chain/1
   ]).

%% channel websocket endpoints
-export(
   [sc_ws_timeout_open/1,
    sc_ws_open/1,
    sc_ws_update/1,
    sc_ws_update_fails_and_close/1,
    sc_ws_send_messages_and_close/1,
    sc_ws_conflict_and_close/1,
    sc_ws_close/1,
    sc_ws_close_mutual_initiator/1,
    sc_ws_close_mutual_responder/1,
    sc_ws_leave/1,
    sc_ws_reestablish/1,
    sc_ws_deposit_initiator_and_close/1,
    sc_ws_deposit_responder_and_close/1,
    sc_ws_withdraw_initiator_and_close/1,
    sc_ws_withdraw_responder_and_close/1,
    sc_ws_contracts/1
   ]).


-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [sequence],
      [
       %% /key-blocks/* /micro-blocks/* /generations/*
       {group, block_endpoints},
       %% /accounts/*
       {group, account_endpoints},
       %% /transactions/*
       {group, transaction_endpoints},
       %% /contracts/*
       {group, contract_endpoints},
       %% /oracles/*
       {group, oracle_endpoints},
       %% /names/*
       {group, name_endpoints},
       %% /channels/*
       {group, channel_endpoints},
       %% /peers/*
       {group, peer_endpoints},
       %% /status/*
       {group, status_endpoints},

       {group, off_chain_endpoints},
       {group, external_endpoints},
       {group, internal_endpoints},
       {group, swagger_validation},
       {group, wrong_http_method_endpoints},
       {group, websocket},
       {group, naming},
       {group, channel_websocket}
      ]},

     %% /key-blocks/* /micro-blocks/* /generations/*
     {block_endpoints, [sequence],
      [
       {group, on_genesis_block}, %% standalone
       {group, on_key_block},     %% standalone
       {group, on_micro_block}    %% standalone
      ]},
     {on_genesis_block, [sequence],
      [
       {group, block_info},
       {group, chain_with_pending_key_block}
      ]},
     {on_key_block, [sequence],
      [
       {group, block_info},
       {group, chain_with_pending_key_block}
      ]},
     {on_micro_block, [sequence],
      [
       {group, block_info},
       {group, chain_with_pending_key_block}
      ]},
     {chain_with_pending_key_block, [],
      [
       get_pending_key_block
      ]},
     {block_info, [sequence],
      [
       get_top_block,
       get_current_key_block,
       get_current_key_block_hash,
       get_current_key_block_height,
       get_pending_key_block,
       get_key_block_by_hash,
       get_key_block_by_height,
       get_micro_block_header_by_hash,
       get_micro_block_transactions_by_hash,
       get_micro_block_transactions_count_by_hash,
       get_micro_block_transaction_by_hash_and_index,
       get_generation_current,
       get_generation_by_hash,
       get_generation_by_height
      ]},

     %% /accounts/*
     {account_endpoints, [sequence],
      [
       {group, nonexistent_account}, %% standalone
       {group, account_with_balance} %% standalone
      ]},
     {nonexistent_account, [],
      [
       {group, account_info}
      ]},
     {account_with_balance, [sequence],
      [
       {group, account_info},
       {group, account_with_pending_tx}
      ]},
     {account_with_pending_tx, [],
      [
       {group, account_info}
      ]},
     {account_info, [sequence],
      [
       get_account_by_pubkey,
       get_pending_account_transactions_by_pubkey
      ]},

     %% /transactions/*
     {transaction_endpoints, [sequence],
      [
       {group, nonexistent_tx},    %% standalone
       {group, tx_is_pending},     %% standalone
       {group, tx_is_on_chain},    %% standalone
       {group, post_tx_to_mempool},%% standalone
       {group, contract_txs}
      ]},
     {nonexistent_tx, [],
      [
       {group, tx_info}
      ]},
     {tx_is_pending, [],
      [
       {group, tx_info}
      ]},
     {tx_is_on_chain, [],
      [
       {group, tx_info}
      ]},
     {post_tx_to_mempool, [],
      [
       post_spend_tx
      ]},
     {tx_info, [sequence],
      [
       get_transaction_by_hash,
       get_transaction_info_by_hash
      ]},
     {contract_txs, [sequence],
      [
       post_contract_and_call_tx
      ]},
     %% /contracts/*
     {contract_endpoints, [sequence],
      [
       get_contract
      ]},

     %% /oracles/*
     {oracle_endpoints, [sequence],
      [
       {group, nonexistent_oracle}, %% standalone
       {group, oracle_txs}          %% standalone
      ]},
     {nonexistent_oracle, [],
      [
       get_oracle_by_pubkey
      ]},
     {oracle_txs, [sequence],
      [
       post_oracle_register,
       post_oracle_extend,
       post_oracle_query,
       post_oracle_response
      ]},

     %% /names/*
     {name_endpoints, [sequence],
      [
       {group, nonexistent_name}, %% standalone
       {group, name_txs}          %% standalone
      ]},
     {nonexistent_name, [],
      [
       get_name_entry_by_name
      ]},
     {name_txs, [sequence],
      [
       %% TODO
      ]},
     %% /channels/*
     {channel_endpoints, [],
      [
       get_channel_by_pubkey
      ]},

     %% /peers/*
     {peer_endpoints, [],
      [
       get_peer_pubkey
      ]},

     %% /status/*
     {status_endpoints, [],
      [
       get_status
      ]},

     {off_chain_endpoints, [],
      [
       test_decode_sophia_data,
       test_decode_sophia_data2,
       broken_decode_sophia_data
      ]},
     {external_endpoints, [sequence],
      [
        % non signed txs
        contract_transactions,
        contract_create_compute_transaction,
        contract_create_transaction_init_error,
        oracle_transactions,
        nameservice_transactions,
        spend_transaction,
        state_channels_onchain_transactions,
        unknown_atom_in_spend_tx,

        get_transaction,

        % sync gossip
        pending_transactions,
        post_correct_tx,
        post_broken_tx,
        post_broken_base58_tx,

        % infos
        peer_pub_key
      ]},
     {internal_endpoints, [sequence],
      [
        broken_spend_tx,
        naming_system_broken_txs,
        node_pubkey,

        % requested Endpoints
        peers
      ]},
     {swagger_validation, [], [
        swagger_validation_body,
        %% swagger_validation_enum,
        %%swagger_validation_required,
        swagger_validation_schema
        %%swagger_validation_types
      ]},
     {wrong_http_method_endpoints, [], [
        wrong_http_method_top,
        wrong_http_method_contract_create,
        wrong_http_method_contract_create_compute,
        wrong_http_method_contract_call,
        wrong_http_method_contract_call_compute,
        wrong_http_method_spend,
        wrong_http_method_oracle_register,
        wrong_http_method_oracle_extend,
        wrong_http_method_oracle_query,
        wrong_http_method_oracle_response,
        wrong_http_method_name_preclaim,
        wrong_http_method_name_claim,
        wrong_http_method_name_transfer,
        wrong_http_method_name_revoke,
        wrong_http_method_pending_transactions,
        wrong_http_method_tx_id,
        wrong_http_method_commitment_hash,
        wrong_http_method_name,
        wrong_http_method_tx,
        wrong_http_method_node_pubkey,
        wrong_http_method_peers
     ]},
     {naming, [sequence],
      [naming_system_manage_name
      ]},
     {websocket, [sequence],
      [ws_get_genesis,
       ws_request_tag,
       ws_block_mined,
       ws_micro_block_added,
       ws_refused_on_limit_reached,
       ws_tx_on_chain
      ]},
     {channel_websocket, [sequence],
      [sc_ws_timeout_open,
       sc_ws_open,
       sc_ws_update,
       sc_ws_close,
      % ensure port is reusable
       sc_ws_open,
       sc_ws_update_fails_and_close,
       sc_ws_open,
       sc_ws_send_messages_and_close,
       sc_ws_open,
       sc_ws_conflict_and_close,
      % initiator can start close mutual
       sc_ws_open,
       sc_ws_update,
       sc_ws_close_mutual_initiator,
       % responder can start close mutual
       sc_ws_open,
       sc_ws_update,
       sc_ws_close_mutual_responder,
       % possible to leave and reestablish channel
       sc_ws_open,
       sc_ws_leave,
       sc_ws_reestablish,
       sc_ws_update,
       sc_ws_close_mutual_initiator,
       % initiator can make a deposit
       sc_ws_open,
       sc_ws_update,
       sc_ws_deposit_initiator_and_close,
       % responder can make a deposit
       sc_ws_open,
       sc_ws_update,
       sc_ws_deposit_responder_and_close,
       % initiator can make a withdrawal
       sc_ws_open,
       sc_ws_update,
       sc_ws_withdraw_initiator_and_close,
       % responder can make a withdrawal
       sc_ws_open,
       sc_ws_update,
       sc_ws_withdraw_responder_and_close,
       % responder can make a withdrawal
       sc_ws_open,
       sc_ws_update,
       sc_ws_contracts
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.http_endpoints"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    Forks = aecore_suite_utils:forks(),

    aecore_suite_utils:create_configs(Config1, #{<<"chain">> =>
                                                 #{<<"persist">> => true,
                                                   <<"hard_forks">> => Forks}}),
    aecore_suite_utils:make_multi(Config1, [?NODE]),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(all, Config) ->
    Config;
init_per_group(Group, Config) when
      Group =:= block_endpoints;
      Group =:= account_endpoints;
      Group =:= transaction_endpoints;
      %%Group =:= contract_endpoint;
      Group =:= oracle_endpoints;
      Group =:= name_endpoints;
      %%Group =:= channel_endpoints;
      Group =:= peer_endpoints;
      Group =:= status_endpoints ->
    start_node(Group, Config);
%% block_endpoints
init_per_group(on_genesis_block = Group, Config) ->
    Config1 = start_node(Group, Config),
    GenesisBlock = rpc(aec_chain, genesis_block, []),
    [{current_block, GenesisBlock},
     {current_block_hash, hash(key, GenesisBlock)},
     {current_block_hash_wrong_type, hash(micro, GenesisBlock)},
     {current_block_height, 0},
     {current_block_type, genesis_block} | Config1];
init_per_group(on_key_block = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    %% Mine at least 1 key block (fork height may be 0).
    aecore_suite_utils:mine_key_blocks(Node, aecore_suite_utils:latest_fork_height()),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{current_block, KeyBlock},
     {current_block_hash, hash(key, KeyBlock)},
     {current_block_hash_wrong_type, hash(micro, KeyBlock)},
     {current_block_height, aec_blocks:height(KeyBlock)},
     {current_block_type, key_block} | Config1];
init_per_group(on_micro_block = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    %% Mine at least 1 key block (fork height may be 0).
    aecore_suite_utils:mine_key_blocks(Node, aecore_suite_utils:latest_fork_height()),
    {ok, [_KeyBlock0]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    %% Send spend tx so it gets included into micro block.
    {ok, Pub} = rpc(aec_keys, pubkey, []),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    {ok, [KeyBlock, MicroBlock]} = aecore_suite_utils:mine_micro_blocks(Node, 1),
    {ok, []} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    true = aec_blocks:is_key_block(KeyBlock),
    false = aec_blocks:is_key_block(MicroBlock),
    [{prev_key_block, KeyBlock},
     {prev_key_block_hash, hash(key, KeyBlock)},
     {prev_key_block_height, aec_blocks:height(KeyBlock)},
     {current_block, MicroBlock},
     {current_block_hash, hash(micro, MicroBlock)},
     {current_block_height, aec_blocks:height(KeyBlock)},
     {current_block_txs, [Tx]},
     {current_block_type, micro_block} | Config1];
init_per_group(chain_with_pending_key_block, Config) ->
    %% Expect a key block each hour.
    MineRate = 60 * 60 * 1000,
    ok = rpc(application, set_env, [aecore, expected_mine_rate, MineRate]),
    ok = rpc(aec_conductor, start_mining, []),
    {ok, PendingKeyBlock} = wait_for_key_block_candidate(),
    [{expected_mine_rate, MineRate},
     {pending_key_block, PendingKeyBlock},
     {pending_key_block_hash, hash(key, PendingKeyBlock)} | Config];
init_per_group(block_info, Config) ->
    Config;
%% account_endpoints
init_per_group(nonexistent_account = Group, Config) ->
    Config1 = start_node(Group, Config),
    {ok, Pubkey} = rpc(aec_keys, pubkey, []),
    [{account_id, aec_base58c:encode(account_pubkey, Pubkey)},
     {account_exists, false} | Config1];
init_per_group(account_with_balance = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    {ok, Pubkey} = rpc(aec_keys, pubkey, []),
    aecore_suite_utils:mine_key_blocks(Node, aecore_suite_utils:latest_fork_height()),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{account_id, aec_base58c:encode(account_pubkey, Pubkey)},
     {account_exists, true} | Config1];
init_per_group(account_with_pending_tx, Config) ->
    Node = ?config(node, Config),
    {ok, Pubkey} = rpc(aec_keys, pubkey, []),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pubkey, Pubkey, 1),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    [{pending_txs, [{aec_base58c:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
     {block_with_txs, undefined},
     {block_with_txs_hash, <<"none">>},
     {block_with_txs_height, -1} | Config];
init_per_group(account_info, Config) ->
    Config;
%% transaction_endpoints
init_per_group(nonexistent_tx = Group, Config) ->
    start_node(Group, Config);
init_per_group(tx_is_pending = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    {ok, Pubkey} = rpc(aec_keys, pubkey, []),
    aecore_suite_utils:mine_key_blocks(Node, aecore_suite_utils:latest_fork_height()),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pubkey, Pubkey, 1),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    [{pending_txs, [{aec_base58c:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
     {block_with_txs, undefined},
     {block_with_txs_hash, <<"none">>},
     {block_with_txs_height, -1} | Config];
init_per_group(tx_is_on_chain = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    {ok, [KeyBlock, MicroBlock]} = aecore_suite_utils:mine_micro_blocks(Node, 1),
    {ok, []} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    true = aec_blocks:is_key_block(KeyBlock),
    false = aec_blocks:is_key_block(MicroBlock),
    [Tx] = aec_blocks:txs(MicroBlock),
    [{on_chain_txs, [{aec_base58c:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
     {block_with_txs, MicroBlock},
     {block_with_txs_hash, hash(micro, MicroBlock)},
     {block_with_txs_height, aec_blocks:height(KeyBlock)} | Config1];
init_per_group(post_tx_to_mempool = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    {ok, Pubkey} = rpc(aec_keys, pubkey, []),
    aecore_suite_utils:mine_key_blocks(Node, aecore_suite_utils:latest_fork_height()),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{sender_id, aec_base58c:encode(account_pubkey, Pubkey)},
     {recipient_id, aec_base58c:encode(account_pubkey, random_hash())},
     {amount, 1},
     {fee, 1},
     {payload, <<"foo">>} | Config1];
init_per_group(tx_info, Config) ->
    Config;
%% contract_endpoints
%% oracle_endpoints
init_per_group(nonexistent_oracle = Group, Config) ->
    start_node(Group, Config);
init_per_group(oracle_txs = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    aecore_suite_utils:mine_key_blocks(Node, aecore_suite_utils:latest_fork_height()),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    Config1;
%% name_endpoints
init_per_group(nonexistent_name = Group, Config) ->
    start_node(Group, Config);
init_per_group(name_txs, _Config) ->
    {skip, not_implemented};

init_per_group(channel_websocket = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    {ok, 404, _} = get_balance_at_top(),
    %% prepare participants
    {IPubkey, IPrivkey} = generate_key_pair(),
    {RPubkey, RPrivkey} = generate_key_pair(),
    IStartAmt = 10000,
    RStartAmt = 10000,
    Fee = 1,
    BlocksToMine = 1,

    aecore_suite_utils:mine_key_blocks(Node, BlocksToMine),

    {ok, 200, #{<<"tx">> := SpendTx1}} =
        post_spend_tx(aec_base58c:encode(account_pubkey, IPubkey), IStartAmt, Fee),
    sign_and_post_tx(SpendTx1),
    {ok, 200, #{<<"tx">> := SpendTx2}} =
        post_spend_tx(aec_base58c:encode(account_pubkey, RPubkey), RStartAmt, Fee),
    sign_and_post_tx(SpendTx2),
    {ok, [_KeyBlock, MicroBlock]} = aecore_suite_utils:mine_blocks(Node, 2),
    [_Spend1, _Spend2] = aec_blocks:txs(MicroBlock),
    assert_balance(IPubkey, IStartAmt),
    assert_balance(RPubkey, RStartAmt),
    Participants = #{initiator => #{pub_key => IPubkey,
                                    priv_key => IPrivkey,
                                    start_amt => IStartAmt},
                     responder => #{pub_key => RPubkey,
                                    priv_key => RPrivkey,
                                    start_amt => RStartAmt}},
    [{participants, Participants} | Config1];
init_per_group(Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    BlocksToMine = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(Node, BlocksToMine),
    Config1.

end_per_group(Group, _Config) when
      Group =:= all;
      Group =:= block_info;
      Group =:= account_info;
      Group =:= tx_info ->
    ok;
end_per_group(chain_with_pending_key_block, _Config) ->
    ok = rpc(aec_conductor, stop_mining, []);
end_per_group(account_with_pending_tx, _Config) ->
    ok;
end_per_group(oracle_txs, _Config) ->
    ok;
end_per_group(Group, Config) ->
    ok = stop_node(Group, Config).

init_per_testcase(post_oracle_register, Config) ->
    %% TODO: assert there is enought balance
    {ok, Pubkey} = rpc(aec_keys, pubkey, []),
    [{account_id, aec_base58c:encode(account_pubkey, Pubkey)},
     {oracle_id, aec_base58c:encode(oracle_pubkey, Pubkey)},
     {query_format, <<"something">>},
     {response_format, <<"something else">>},
     {query_fee, 1},
     {fee, 10},
     {oracle_ttl_type, <<"block">>},
     {oracle_ttl_value, 2000} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_extend, Config) ->
    {post_oracle_register, SavedConfig} = ?config(saved_config, Config),
    OracleTtlDelta = 500,
    [{account_id, ?config(account_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {fee, 10},
     {oracle_ttl_value_final, ?config(oracle_ttl_value, SavedConfig) + OracleTtlDelta},
     {oracle_ttl_type, <<"delta">>},
     {oracle_ttl_value, OracleTtlDelta} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_query, Config) ->
    {post_oracle_extend, SavedConfig} = ?config(saved_config, Config),
    [{sender_id, ?config(account_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {query, <<"Hejsan Svejsan">>},
     {query_fee, 2},
     {fee, 30},
     {query_ttl_type, <<"block">>},
     {query_ttl_value, 20},
     {response_ttl_type, <<"delta">>},
     {response_ttl_value, 20} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_response, Config) ->
    {post_oracle_query, SavedConfig} = ?config(saved_config, Config),
    [{sender_id, ?config(sender_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {query, ?config(query, SavedConfig)},
     {query_id, ?config(query_id, SavedConfig)},
     {fee, 10},
     {response, <<"Hejsan">>} | init_per_testcase_all(Config)];
init_per_testcase(_Case, Config) ->
    init_per_testcase_all(Config).

init_per_testcase_all(Config) ->
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(_Case, Config) ->
    end_per_testcase_all(Config).

end_per_testcase_all(Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p",
           [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
             || {_,N} <- ?config(nodes, Config)]]),
    ok.

start_node(Group, Config) ->
    start_node(proplists:is_defined(node, Config), Group, Config).

start_node(true, _Group, Config) ->
    Config;
start_node(false, Group, Config) ->
    %% TODO: consider reinint_chain to speed up tests
    aecore_suite_utils:start_node(?NODE, Config),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node),
    [{node, Node}, {node_start_group, Group} | Config].

stop_node(Group, Config) ->
    stop_node(proplists:is_defined(node, Config), Group, Config).

stop_node(true, Group, Config) ->
    NodeStartGroup = ?config(node_start_group, Config),
    case Group =:= NodeStartGroup of
        true ->
            RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
            {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
            aecore_suite_utils:stop_node(?NODE, Config),
            aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
            ok;
        false ->
            ok
    end;
stop_node(false, _Group, _Config) ->
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% off chain endpoints
test_decode_sophia_data(_Config) ->
    CallEncodedInteger42 = to_binary({<<"foo">>, {42, 2}}),
    Decoded = decode_data(<<"(string, (int, int))">>,
                          CallEncodedInteger42),
    #{<<"data">> :=
          #{<<"type">> := <<"tuple">>,
            <<"value">> :=
                [#{<<"type">>  := <<"string">>,
                   <<"value">> := <<"foo">>},
                 #{<<"type">>  := <<"tuple">>,
                   <<"value">> :=
                       [#{<<"type">> := <<"word">>,
                          <<"value">> := 42},
                        _]}
                ]
           }
     }
        = Decoded,

    ok.

test_decode_sophia_data2(_Config) ->
    CD = {<<"foo">>, {<<"Hello">>, [1, 2, 3], {some, 1}}},
    Type = <<"(string, (string, list(int), option(bool)))">>,
    CallEncoded = to_binary(CD),
    Decoded = decode_data(Type, CallEncoded),
    #{<<"data">> :=
          #{ <<"type">> := <<"tuple">>
           , <<"value">> :=
                 [#{<<"type">>   := <<"string">>
                   , <<"value">> := <<"foo">> }
                 , #{ <<"type">>  := <<"tuple">>
                    , <<"value">> :=
                          [ #{ <<"type">> := <<"string">>
                             , <<"value">> := <<"Hello">> }
                          , #{ <<"type">> := <<"list">>
                             , <<"value">> :=
                                   [_,_,_]}
                          , #{ <<"type">> := <<"option">>
                             , <<"value">> :=
                                   #{ <<"type">> := <<"word">>
                                    , <<"value">> := 1}}]}
                  ]
           }
     } = Decoded,

    ok.

broken_decode_sophia_data(_Config) ->
    D = to_binary({<<"bar">>, 42}),
    T = <<"(string, int)">>,
    %% Happy path.
    {ok, 200,
     #{<<"data">> :=
           #{<<"type">> := <<"tuple">>,
             <<"value">> :=
                 [#{<<"type">> := <<"string">>, <<"value">> := <<"bar">>},
                  #{<<"type">> := <<"word">>  , <<"value">> := 42} ]}}
    } = get_contract_decode_data(#{'sophia-type' => T, data => D}),
    %% Missing field.
    lists:foreach(
      fun({Req, ExpMissingField}) ->
              {ok, 400, Body} = get_contract_decode_data(Req),
              ?assertMatch(
                 #{<<"reason">> := <<"validation_error">>,
                   <<"parameter">> := <<"body">>,
                   <<"info">> := #{<<"error">> := <<"missing_required_property">>,
                                   <<"data">> := ActMissingField}
                  } when ActMissingField =:= ExpMissingField,
                 Body)
      end,
      [ { #{data => D}, <<"sophia-type">>}
      , { #{'sophia-type' => T}, <<"data">>}
      , { #{type => T, data => D}, <<"sophia-type">>}
      ]),
    %% Field invalid according to schema.
    lists:foreach(
      fun({Req, ExpWrongField, ExpWrongValue}) ->
              {ok, 400, Body} = get_contract_decode_data(Req),
              ?assertMatch(
                 #{<<"reason">> := <<"validation_error">>,
                   <<"parameter">> := <<"body">>,
                   <<"info">> := #{<<"error">> := <<"wrong_type">>,
                                   <<"path">> := [ActWrongField],
                                   <<"data">> := ActWrongValue}
                  } when (ActWrongField =:= ExpWrongField) andalso
                         (ActWrongValue =:= ExpWrongValue),
                 Body)
      end,
      [ { #{'sophia-type' => 42, data => D}, <<"sophia-type">>, 42}
      , { #{'sophia-type' => T, data => 42}, <<"data">>, 42}
      ]),
    %% Field valid according to schema but invalid for handler.
    {ok, 400, #{<<"reason">> := <<"bad_type">>}} = get_contract_decode_data(#{'sophia-type' => <<"foo">>, data => D}),
    {ok, 400, #{<<"reason">> := <<"bad argument">>}} = get_contract_decode_data(#{'sophia-type' => T, data => <<"foo">>}),
    %% Field valid for both schema and handler, though data
    %% interpreted in a different way than the specified type spec.
    {ok, 200,
     #{<<"data">> :=
           #{<<"type">> := <<"tuple">>,
             <<"value">> :=
                 [#{<<"type">> := <<"string">>, <<"value">> := <<"bar">>},
                  #{<<"type">> := <<"word">>, <<"value">> := 160} ]}}
    } = get_contract_decode_data(#{'sophia-type' => T, data => to_binary({<<"bar">>, {42}})}),
    ok.

%% Used in contract-decode endpoint tests.
to_binary(Term) ->
    BaseAddr = 0, %% Decode expects base address 0 (since return values have base address 0)
    aeu_hex:hexstring_encode(aeso_data:to_binary(Term, BaseAddr)).

decode_data(Type, EncodedData) ->
    {ok, 200, Data} =
        get_contract_decode_data(#{ 'sophia-type' => Type,
                                    data => EncodedData}),
    Data.

%% /blocks/top

get_top_block(Config) ->
    get_top_block(?config(current_block_type, Config), Config).

get_top_block(_CurrentBlockType, Config) ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, Block} = get_top_sut(),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, Block)),
    ok.

get_top_sut() ->
    Host = external_address(),
    http_request(Host, get, "blocks/top", []).

%% /key-blocks/*

get_current_key_block(Config) ->
    get_current_key_block(?config(current_block_type, Config), Config).

get_current_key_block(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHeight = ?config(current_block_height, Config),
    {ok, 200, Block} = get_key_blocks_current_sut(),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, Block)),
    ?assertEqual(CurrentBlockHeight, maps:get(<<"height">>, Block)),
    ok;
get_current_key_block(micro_block, Config) ->
    PrevKeyBlockHash = ?config(prev_key_block_hash, Config),
    PrevKeyBlockHeight = ?config(prev_key_block_height, Config),
    {ok, 200, Block} = get_key_blocks_current_sut(),
    ?assertEqual(PrevKeyBlockHash, maps:get(<<"hash">>, Block)),
    ?assertEqual(PrevKeyBlockHeight, maps:get(<<"height">>, Block)),
    ok.

get_current_key_block_hash(Config) ->
    get_current_key_block_hash(?config(current_block_type, Config), Config).

get_current_key_block_hash(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, #{<<"hash">> := Hash}} = get_key_blocks_current_hash_sut(),
    ?assertEqual(CurrentBlockHash, Hash),
    ok;
get_current_key_block_hash(micro_block, Config) ->
    PrevKeyBlockHash = ?config(prev_key_block_hash, Config),
    {ok, 200, #{<<"hash">> := Hash}} = get_key_blocks_current_hash_sut(),
    ?assertEqual(PrevKeyBlockHash, Hash),
    ok.

get_current_key_block_height(Config) ->
    get_current_key_block_height(?config(current_block_type, Config), Config).

get_current_key_block_height(_CurrentBlockType, Config) ->
    CurrentBlockHeight = ?config(current_block_height, Config),
    {ok, 200, #{<<"height">> := Height}} = get_key_blocks_current_height_sut(),
    ?assertEqual(CurrentBlockHeight, Height),
    ok.

get_pending_key_block(Config) ->
    CurrentBlockType = ?config(current_block_type, Config),
    get_pending_key_block(proplists:is_defined(pending_key_block, Config), CurrentBlockType, Config).

get_pending_key_block(false, _CurrentBlockType, _Config) ->
    {ok, 404, Error} = get_key_blocks_pending_sut(),
    ?assertEqual(<<"Block not found">>, maps:get(<<"reason">>, Error)),
    ok;
get_pending_key_block(true, _CurrentBlockType, Config) ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHeight = ?config(current_block_height, Config),
    Pred =
        fun({ok, 200, _}) ->
                true;
           ({ok, 404,
             #{<<"reason">> :=
                   <<"Starting mining, pending block not available yet">>}}) ->
                false
        end,
    {ok, {ok, 200, Block}} = aec_test_utils:wait_for_pred_or_timeout(
                               fun get_key_blocks_pending_sut/0, Pred, 20000),
    case maps:get(<<"height">>, Block) of
        H when H =:= (CurrentBlockHeight + 1) ->
            ?assertEqual(CurrentBlockHash, maps:get(<<"prev_hash">>, Block));
        H when is_integer(H), H > CurrentBlockHeight ->
            ?assertNotEqual(CurrentBlockHash, maps:get(<<"prev_hash">>, Block))
    end,
    ok.

get_key_block_by_hash(Config) ->
    get_key_block_by_hash(?config(current_block_type, Config), Config).

get_key_block_by_hash(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, Block} = get_key_blocks_by_hash_sut(CurrentBlockHash),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, Block)),
    ok;
get_key_block_by_hash(micro_block, Config) ->
    PrevKeyBlockHash = ?config(prev_key_block_hash, Config),
    {ok, 200, Block} = get_key_blocks_by_hash_sut(PrevKeyBlockHash),
    ?assertEqual(PrevKeyBlockHash, maps:get(<<"hash">>, Block)),
    ok.

get_key_block_by_height(Config) ->
    get_key_block_by_height(?config(current_block_type, Config), Config).

get_key_block_by_height(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHeight = ?config(current_block_height, Config),
    {ok, 200, Block} = get_key_blocks_by_height_sut(CurrentBlockHeight),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, Block)),
    ?assertEqual(CurrentBlockHeight, maps:get(<<"height">>, Block)),
    ok;
get_key_block_by_height(micro_block, Config) ->
    PrevKeyBlockHash = ?config(prev_key_block_hash, Config),
    PrevKeyBlockHeight = ?config(prev_key_block_height, Config),
    {ok, 200, Block} = get_key_blocks_by_height_sut(PrevKeyBlockHeight),
    ?assertEqual(PrevKeyBlockHash, maps:get(<<"hash">>, Block)),
    ?assertEqual(PrevKeyBlockHeight, maps:get(<<"height">>, Block)),
    ok.

get_key_blocks_current_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current", []).

get_key_blocks_current_hash_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current/hash", []).

get_key_blocks_current_height_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current/height", []).

get_key_blocks_pending_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/pending", []).

get_key_blocks_by_height_sut(Height) ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/height/" ++ integer_to_list(Height), []).

get_key_blocks_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/hash/" ++ http_uri:encode(Hash), []).

%% /micro-blocks/*

get_micro_block_header_by_hash(Config) ->
    get_micro_block_header_by_hash(?config(current_block_type, Config), Config).

get_micro_block_header_by_hash(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHashWrongType = ?config(current_block_hash_wrong_type, Config),
    {ok, 400, Error} = get_micro_blocks_header_by_hash_sut(CurrentBlockHash),
    ?assertEqual(<<"Invalid hash">>, maps:get(<<"reason">>, Error)),
    {ok, 404, Error1} = get_micro_blocks_header_by_hash_sut(CurrentBlockHashWrongType),
    ?assertEqual(<<"Block not found">>, maps:get(<<"reason">>, Error1)),
    ok;
get_micro_block_header_by_hash(micro_block, Config) ->
    PrevKeyBlockHash = ?config(prev_key_block_hash, Config),
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, Header} = get_micro_blocks_header_by_hash_sut(CurrentBlockHash),
    ?assertEqual(PrevKeyBlockHash, maps:get(<<"prev_hash">>, Header)),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, Header)),
    ok.

get_micro_block_transactions_by_hash(Config) ->
    get_micro_block_transactions_by_hash(?config(current_block_type, Config), Config).

get_micro_block_transactions_by_hash(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHashWrongType = ?config(current_block_hash_wrong_type, Config),
    {ok, 400, Error} = get_micro_blocks_transactions_by_hash_sut(CurrentBlockHash),
    ?assertEqual(<<"Invalid hash">>, maps:get(<<"reason">>, Error)),
    {ok, 404, Error1} = get_micro_blocks_header_by_hash_sut(CurrentBlockHashWrongType),
    ?assertEqual(<<"Block not found">>, maps:get(<<"reason">>, Error1)),
    ok;
get_micro_block_transactions_by_hash(micro_block, Config) ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockTxs = ?config(current_block_txs, Config),
    CurrentBlockTxsCount = length(CurrentBlockTxs),
    {ok, 200, #{<<"transactions">> := Txs}} = get_micro_blocks_transactions_by_hash_sut(CurrentBlockHash),
    %% TODO: check Txs is the same as CurrentBlockTxs
    ?assertMatch([_], Txs),
    ?assertMatch(CurrentBlockTxsCount, length(Txs)),
    ok.

get_micro_block_transactions_count_by_hash(Config) ->
    get_micro_block_transactions_count_by_hash(?config(current_block_type, Config), Config).

get_micro_block_transactions_count_by_hash(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHashWrongType = ?config(current_block_hash_wrong_type, Config),
    {ok, 400, Error} = get_micro_blocks_transactions_count_by_hash_sut(CurrentBlockHash),
    ?assertEqual(<<"Invalid hash">>, maps:get(<<"reason">>, Error)),
    {ok, 404, Error1} = get_micro_blocks_header_by_hash_sut(CurrentBlockHashWrongType),
    ?assertEqual(<<"Block not found">>, maps:get(<<"reason">>, Error1)),
    ok;
get_micro_block_transactions_count_by_hash(micro_block, Config) ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockTxs = ?config(current_block_txs, Config),
    CurrentBlockTxsCount = length(CurrentBlockTxs),
    {ok, 200, Count} = get_micro_blocks_transactions_count_by_hash_sut(CurrentBlockHash),
    ?assertEqual(CurrentBlockTxsCount, maps:get(<<"count">>, Count)),
    ok.

get_micro_block_transaction_by_hash_and_index(Config) ->
    get_micro_block_transaction_by_hash_and_index(?config(current_block_type, Config), Config).

get_micro_block_transaction_by_hash_and_index(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHashWrongType = ?config(current_block_hash_wrong_type, Config),
    {ok, 400, Error} = get_micro_blocks_transactions_count_by_hash_sut(CurrentBlockHash),
    ?assertEqual(<<"Invalid hash">>, maps:get(<<"reason">>, Error)),
    {ok, 404, Error1} = get_micro_blocks_header_by_hash_sut(CurrentBlockHashWrongType),
    ?assertEqual(<<"Block not found">>, maps:get(<<"reason">>, Error1)),
    ok;
get_micro_block_transaction_by_hash_and_index(micro_block, Config) ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockTxs = ?config(current_block_txs, Config),
    CurrentBlockTxIndex = 1,
    WrongCurrentBlockTxIndex = length(CurrentBlockTxs) + 1,
    {ok, 200, Tx} = get_micro_blocks_transactions_by_hash_by_index_sut(CurrentBlockHash, CurrentBlockTxIndex),
    ?assertEqual(CurrentBlockHash, maps:get(<<"block_hash">>, Tx)),
    %% TODO: check tx hashes
    {ok, 400, Error} = get_micro_blocks_transactions_by_hash_by_index_sut(CurrentBlockHash, WrongCurrentBlockTxIndex),
    ?assertEqual(<<"Invalid hash or index">>, maps:get(<<"reason">>, Error)),
    ok.

get_micro_blocks_header_by_hash_sut(Hash) ->
    Host = external_address(),
    Hash1 = binary_to_list(Hash),
    http_request(Host, get, "micro-blocks/hash/" ++ http_uri:encode(Hash1) ++ "/header", []).

get_micro_blocks_transactions_by_hash_sut(Hash) ->
    Host = external_address(),
    Hash1 = binary_to_list(Hash),
    http_request(Host, get, "micro-blocks/hash/" ++ http_uri:encode(Hash1) ++ "/transactions", []).

get_micro_blocks_transactions_count_by_hash_sut(Hash) ->
    Host = external_address(),
    Hash1 = binary_to_list(Hash),
    http_request(Host, get, "micro-blocks/hash/" ++ http_uri:encode(Hash1) ++ "/transactions/count", []).

get_micro_blocks_transactions_by_hash_by_index_sut(Hash, Index) ->
    Host = external_address(),
    Hash1 = binary_to_list(Hash),
    Index1 = integer_to_list(Index),
    Path = "micro-blocks/hash/" ++ http_uri:encode(Hash1) ++ "/transactions/index/" ++ Index1,
    http_request(Host, get, Path, []).

%% /generations/*

get_generation_current(Config) ->
    get_generation_current(?config(current_block_type, Config), Config).

get_generation_current(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, #{<<"key_block">> := KeyBlock,
                <<"micro_blocks">> := []}} = get_generation_current_sut(),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, KeyBlock)),
    ok;
get_generation_current(micro_block, Config) ->
    {ok, 200, #{<<"key_block">> := KeyBlock,
                <<"micro_blocks">> := [MicroBlockHash]}} = get_generation_current_sut(),

    ?assertEqual(?config(prev_key_block_hash, Config), maps:get(<<"hash">>, KeyBlock)),
    ?assertEqual(?config(current_block_hash, Config), MicroBlockHash),
    ok.

get_generation_by_hash(Config) ->
    get_generation_by_hash(?config(current_block_type, Config), Config).

get_generation_by_hash(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, #{<<"key_block">> := KeyBlock,
                <<"micro_blocks">> := []}} = get_generation_by_hash_sut(CurrentBlockHash),

    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, KeyBlock)),
    ?assertEqual({ok, 400, #{<<"reason">> => <<"Invalid hash">>}},
                 get_generation_by_hash_sut(<<"random">>)),
    ok;
get_generation_by_hash(micro_block, Config) ->
    PrevKeyBlockHash = ?config(prev_key_block_hash, Config),
    CurrentBlockHash = ?config(current_block_hash, Config),
    {ok, 200, #{<<"key_block">> := KeyBlock,
                <<"micro_blocks">> := [MicroBlockHash]}} = get_generation_by_hash_sut(PrevKeyBlockHash),

    ?assertEqual(PrevKeyBlockHash, maps:get(<<"hash">>, KeyBlock)),
    ?assertEqual(CurrentBlockHash, MicroBlockHash),
    ?assertEqual({ok, 400, #{<<"reason">> => <<"Invalid hash">>}},
                 get_generation_by_hash_sut(CurrentBlockHash)),
    ?assertEqual({ok, 400, #{<<"reason">> => <<"Invalid hash">>}},
                 get_generation_by_hash_sut(<<"random">>)),
    ok.

get_generation_by_height(Config) ->
    get_generation_by_height(?config(current_block_type, Config), Config).

get_generation_by_height(CurrentBlockType, Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    CurrentBlockHash = ?config(current_block_hash, Config),
    CurrentBlockHeight = ?config(current_block_height, Config),
    {ok, 200, #{<<"key_block">> := KeyBlock,
                <<"micro_blocks">> := []}} = get_generation_by_height_sut(CurrentBlockHeight),

    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, KeyBlock)),
    ?assertEqual({ok,404,#{<<"reason">> => <<"Chain too short">>}},
                 get_generation_by_height_sut(CurrentBlockHeight+1)),
    ok;
get_generation_by_height(micro_block, Config) ->
    CurrentBlockHeight = ?config(current_block_height, Config),
    {ok, 200, #{<<"key_block">> := KeyBlock,
                <<"micro_blocks">> := [MicroBlockHash]}} = get_generation_by_height_sut(CurrentBlockHeight),

    ?assertEqual(?config(prev_key_block_hash, Config), maps:get(<<"hash">>, KeyBlock)),
    ?assertEqual(?config(current_block_hash, Config), MicroBlockHash),
    ?assertEqual({ok,404,#{<<"reason">> => <<"Chain too short">>}},
                 get_generation_by_height_sut(CurrentBlockHeight+1)),
    ok.

get_generation_current_sut() ->
    Host = external_address(),
    http_request(Host, get, "generations/current", []).

get_generation_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "generations/hash/" ++ http_uri:encode(Hash), []).

get_generation_by_height_sut(Height) ->
    Host = external_address(),
    http_request(Host, get, "generations/height/" ++ integer_to_list(Height), []).

%% /accounts/*

get_account_by_pubkey(Config) ->
    get_account_by_pubkey(?config(account_exists, Config), Config).

get_account_by_pubkey(false, Config) ->
    AccountId = ?config(account_id, Config),
    {ok, 404, Error} = get_accounts_by_pubkey_sut(AccountId),
    ?assertEqual(<<"Account not found">>, maps:get(<<"reason">>, Error)),
    ok;
get_account_by_pubkey(true, Config) ->
    AccountId = ?config(account_id, Config),
    {ok, 200, Account} = get_accounts_by_pubkey_sut(AccountId),
    ?assertEqual(AccountId, maps:get(<<"id">>, Account)),
    ?assert(maps:get(<<"balance">>, Account) > 0),
    %% TODO: check nonce?
    ok.

get_pending_account_transactions_by_pubkey(Config) ->
    get_pending_account_transactions_by_pubkey(?config(account_exists, Config), Config).

get_pending_account_transactions_by_pubkey(false, Config) ->
    AccountId = ?config(account_id, Config),
    {ok, 404, Error} = get_accounts_transactions_pending_by_pubkey_sut(AccountId),
    ?assertEqual(<<"Account not found">>, maps:get(<<"reason">>, Error)),
    ok;
get_pending_account_transactions_by_pubkey(true, Config) ->
    AccountId = ?config(account_id, Config),
    PendingTxs = proplists:get_value(pending_txs, Config, []),
    {ok, 200, Txs} = get_accounts_transactions_pending_by_pubkey_sut(AccountId),
    %% TODO: check txs hashes
    ?assertEqual(length(PendingTxs), length(maps:get(<<"transactions">>, Txs))),
    ok.

get_accounts_by_pubkey_sut(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

get_accounts_transactions_pending_by_pubkey_sut(Id) ->
    Host = external_address(),
    Id1 = binary_to_list(Id),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id1) ++ "/transactions/pending", []).

%% /transactions/*

get_transaction_by_hash(Config) ->
    PendingTxs = proplists:get_value(pending_txs, Config, []),
    OnChainTxs = proplists:get_value(on_chain_txs, Config, []),
    case {PendingTxs, OnChainTxs} of
        {[], []} -> get_transaction_by_hash([], Config);
        {[_], []} -> get_transaction_by_hash(PendingTxs, Config);
        {[], [_]} -> get_transaction_by_hash(OnChainTxs, Config)
    end.

get_transaction_by_hash([], _Config) ->
    RandomTxHash = aec_base58c:encode(tx_hash, random_hash()),
    {ok, 404, Error} = get_transactions_by_hash_sut(RandomTxHash),
    ?assertEqual(<<"Transaction not found">>, maps:get(<<"reason">>, Error)),
    ok;
get_transaction_by_hash([{TxHash, _ExpectedTx}], Config) ->
    BlockWithTxsHash = ?config(block_with_txs_hash, Config),
    BlockWithTxsHeight = ?config(block_with_txs_height, Config),
    {ok, 200, Tx} = get_transactions_by_hash_sut(TxHash),
    ?assertEqual(TxHash, maps:get(<<"hash">>, Tx)),
    ?assertEqual(BlockWithTxsHash, maps:get(<<"block_hash">>, Tx)),
    ?assertEqual(BlockWithTxsHeight, maps:get(<<"block_height">>, Tx)),
    ok.

get_transaction_info_by_hash(_Config) ->
    {skip, not_implemented}.

post_spend_tx(Config) ->
    TxArgs =
        #{sender_id    => ?config(sender_id, Config),
          recipient_id => ?config(recipient_id, Config),
          amount       => ?config(amount, Config),
          fee          => ?config(fee, Config),
          payload      => ?config(payload, Config)},
    {TxHash, Tx} = prepare_tx(spend_tx, TxArgs),
    ok = post_tx(TxHash, Tx),
    ok.

post_contract_and_call_tx(_Config) ->
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),

    {ok, EncodedInitCallData} = aect_sophia:encode_call_data(Code, <<"init">>, <<"()">>),
    ValidEncoded = #{ owner_id   => MinerAddress,
                      code       => Code,
                      vm_version => 1,
                      deposit    => 2,
                      amount     => 1,
                      gas        => 300,
                      gas_price  => 1,
                      fee        => 1,
                      call_data  => EncodedInitCallData},

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    %%%% {ok, ContractPubKey} = aec_base58c:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Tx not mined">>}}, get_transactions_info_by_hash_sut(ContractCreateTxHash)),

    % mine
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertMatch({ok, 200, _}, get_transactions_info_by_hash_sut(ContractCreateTxHash)),

    {ok, EncodedCallData} = aect_sophia:encode_call_data(Code, <<"main">>, <<"42">>),
    ContractCallEncoded = #{ caller_id   => MinerAddress,
                             contract_id => EncodedContractPubKey,
                             vm_version  => 1,
                             amount      => 1,
                             gas         => 1000,
                             gas_price   => 1,
                             fee         => 1,
                             call_data   => EncodedCallData},
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallTx}} = get_contract_call(ContractCallEncoded),
    ContractCallTxHash = sign_and_post_tx(EncodedUnsignedContractCallTx),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCallTxHash)),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Tx not mined">>}}, get_transactions_info_by_hash_sut(ContractCallTxHash)),

    % mine
    ok = wait_for_tx_hash_on_chain(ContractCallTxHash),
    ?assert(tx_in_chain(ContractCallTxHash)),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCallTxHash)),
    ?assertMatch({ok, 200, _}, get_transactions_info_by_hash_sut(ContractCallTxHash)),
    ok.

get_transactions_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ http_uri:encode(Hash), []).

get_transactions_info_by_hash_sut(Hash) ->
    Host = external_address(),
    Hash1 = http_uri:encode(Hash),
    http_request(Host, get, "transactions/" ++ binary_to_list(Hash1) ++ "/info", []).

post_transactions_sut(Tx) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => Tx}).

%% /contracts/*

get_contract(_Config) ->
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 1),

    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),

    % contract_create_tx positive test
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aect_sophia:encode_call_data(Code,
                                     InitFunction,
                                     InitArgument),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id   => MinerAddress,
                      code       => Code,
                      vm_version => 1,
                      deposit    => 2,
                      amount     => ContractInitBalance,
                      gas        => 300,
                      gas_price  => 1,
                      fee        => 1,
                      call_data  => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner_id  => aec_id:create(account, MinerPubkey),
                                code      => aeu_hex:hexstring_decode(Code),
                                call_data => aeu_hex:hexstring_decode(EncodedInitCallData)}),

    unsigned_tx_positive_test(ValidDecoded, ValidEncoded, fun get_contract_create/1,
                               fun aect_create_tx:new/1, MinerPubkey),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} = get_contract_create(ValidEncoded),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    %% Try to get the contract init call object while in mempool
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} = get_contract_call_object(ContractCreateTxHash),

    {ok, 404, #{<<"reason">> := <<"Proof for contract not found">>}} = get_contract_poi(EncodedContractPubKey),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Account not found">>}},
                 get_accounts_by_pubkey_sut(EncodedContractPubKey)),

    % mine a block
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    {ok, 200, #{<<"return_value">> := ReturnValue}} = get_contract_call_object(ContractCreateTxHash),

    ?assertMatch({ok, 200, #{<<"id">>          := EncodedContractPubKey,
                             <<"owner_id">>    := MinerAddress,
                             <<"active">>      := true,
                             <<"deposit">>     := 2,
                             <<"vm_version">>  := 1,
                             <<"referrer_ids">> := [],
                             <<"log">>         := <<>>}},
                 get_contract_sut(EncodedContractPubKey)),
    ?assertEqual({ok, 200, #{<<"bytecode">> => Code}}, get_contract_code_sut(EncodedContractPubKey)),
    ?assertMatch({ok, 200, #{<<"store">> := [
        #{<<"key">> := <<"0x00">>, <<"value">> := ReturnValue}
        ]}}, get_contract_store_sut(EncodedContractPubKey)),
    ok.

get_contract_sut(PubKey) ->
    Host = external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(PubKey), []).

get_contract_code_sut(PubKey) ->
    Host = external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(PubKey) ++ "/code", []).

get_contract_store_sut(PubKey) ->
    Host = external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(PubKey) ++ "/store", []).

%% /oracles/*

get_oracle_by_pubkey(_Config) ->
    RandomOraclePubkey = aec_base58c:encode(oracle_pubkey, random_hash()),
    {ok, 404, Error} = get_oracles_by_pubkey_sut(RandomOraclePubkey),
    ?assertEqual(<<"Oracle not found">>, maps:get(<<"reason">>, Error)),
    ok.

post_oracle_register(Config) ->
    OracleId = ?config(oracle_id, Config),
    TxArgs =
        #{account_id      => ?config(account_id, Config),
          query_format    => ?config(query_format, Config),
          response_format => ?config(response_format, Config),
          query_fee       => ?config(query_fee, Config),
          fee             => ?config(fee, Config),
          oracle_ttl      => #{type  => ?config(oracle_ttl_type, Config),
                               value => ?config(oracle_ttl_value, Config)}},
    {TxHash, Tx} = prepare_tx(oracle_register_tx, TxArgs),
    ok = post_tx(TxHash, Tx),
    ok = wait_for_tx_hash_on_chain(TxHash),
    {ok, 200, Resp} = get_oracles_by_pubkey_sut(OracleId),
    ?assertEqual(OracleId, maps:get(<<"id">>, Resp)),
    {save_config, save_config([account_id, oracle_id, oracle_ttl_value], Config)}.

post_oracle_extend(Config) ->
    OracleId = ?config(oracle_id, Config),
    TxArgs =
        #{oracle_id  => OracleId,
          fee        => ?config(fee, Config),
          oracle_ttl => #{type  => ?config(oracle_ttl_type, Config),
                          value => ?config(oracle_ttl_value, Config)}},
    {TxHash, Tx} = prepare_tx(oracle_extend_tx, TxArgs),
    ok = post_tx(TxHash, Tx),
    ok = wait_for_tx_hash_on_chain(TxHash),
    {ok, 200, Resp} = get_oracles_by_pubkey_sut(OracleId),
    ?assertEqual(OracleId, maps:get(<<"id">>, Resp)),
    ?assertEqual(?config(oracle_ttl_value_final, Config), maps:get(<<"ttl">>, Resp)),
    {ok, 200, Resp1} = get_oracles_queries_by_pubkey_sut(OracleId, #{type => "all"}),
    ?assertEqual([], maps:get(<<"oracle_queries">>, Resp1)),
    {save_config, save_config([account_id, oracle_id], Config)}.

post_oracle_query(Config) ->
    SenderId = ?config(sender_id, Config),
    OracleId = ?config(oracle_id, Config),
    TxArgs =
        #{sender_id    => SenderId,
          oracle_id    => OracleId,
          query        => ?config(query, Config),
          query_fee    => ?config(query_fee, Config),
          fee          => ?config(fee, Config),
          query_ttl    => #{type  => ?config(query_ttl_type, Config),
                            value => ?config(query_ttl_value, Config)},
          response_ttl => #{type  => ?config(response_ttl_type, Config),
                            value => ?config(response_ttl_value, Config)}},
    {TxHash, Tx} = prepare_tx(oracle_query_tx, TxArgs),
    ok = post_tx(TxHash, Tx),
    ok = wait_for_tx_hash_on_chain(TxHash),
    {ok, 200, Resp} = get_oracles_queries_by_pubkey_sut(OracleId, #{type => "closed"}),
    ?assertEqual([], maps:get(<<"oracle_queries">>, Resp)),
    {ok, 200, Resp1} = get_oracles_queries_by_pubkey_sut(OracleId, #{type => "all"}),
    ?assertEqual(1, length(maps:get(<<"oracle_queries">>, Resp1))),
    [Query] = maps:get(<<"oracle_queries">>, Resp1),
    ?assertEqual(SenderId, maps:get(<<"sender_id">>, Query)),
    ?assertEqual(OracleId, maps:get(<<"oracle_id">>, Query)),
    QueryId = maps:get(<<"id">>, Query),
    Config1 = [{query, ?config(query, Config)}, {query_id, QueryId} | Config],
    {save_config, save_config([sender_id, oracle_id, query, query_id], Config1)}.

post_oracle_response(Config) ->
    OracleId = ?config(oracle_id, Config),
    Query = ?config(query, Config),
    QueryId = ?config(query_id, Config),
    Response = ?config(response, Config),
    TxArgs =
        #{oracle_id => OracleId,
          query_id  => QueryId,
          response  => Response,
          fee       => ?config(fee, Config)},
    {TxHash, Tx} = prepare_tx(oracle_response_tx, TxArgs),
    ok = post_tx(TxHash, Tx),
    ok = wait_for_tx_hash_on_chain(TxHash),
    {ok, 200, Resp} = get_oracles_queries_by_pubkey_sut(OracleId, #{type => "open"}),
    ?assertEqual([], maps:get(<<"oracle_queries">>, Resp)),
    {ok, 200, Resp1} = get_oracles_query_by_pubkey_and_query_id(OracleId, QueryId),
    ?assertEqual(QueryId, maps:get(<<"id">>, Resp1)),
    ?assertEqual(OracleId, maps:get(<<"oracle_id">>, Resp1)),
    ?assertEqual({ok, Query}, aec_base58c:safe_decode(oracle_query, maps:get(<<"query">>, Resp1))),
    ?assertEqual({ok, Response}, aec_base58c:safe_decode(oracle_response, maps:get(<<"response">>, Resp1))),
    ok.

get_oracles_by_pubkey_sut(Pubkey) ->
    Host = external_address(),
    http_request(Host, get, "oracles/" ++ http_uri:encode(Pubkey), []).

%% TODO: add test for 'limit' and 'from' in HTTP query
get_oracles_queries_by_pubkey_sut(Pubkey, Params) ->
    Host = external_address(),
    Pubkey1 = binary_to_list(Pubkey),
    http_request(Host, get, "oracles/" ++ http_uri:encode(Pubkey1) ++ "/queries", Params).

get_oracles_query_by_pubkey_and_query_id(Pubkey, Id) ->
    Host = external_address(),
    Pubkey1 = binary_to_list(Pubkey),
    Id1 = binary_to_list(Id),
    http_request(Host, get, "oracles/" ++ http_uri:encode(Pubkey1) ++ "/queries/" ++ http_uri:encode(Id1), []).

%% /names/*

get_name_entry_by_name(_Config) ->
    NonexistentName = <<"Nonexistent_name">>,
    {ok, 400, _Error} = get_names_entry_by_name_sut(NonexistentName),
    %%?assertEqual(<<"Name not found">>, maps:get(<<"reason">>, Error)),
    ok.

get_names_entry_by_name_sut(Name) ->
    Host = external_address(),
    http_request(Host, get, "names/" ++ Name, []).

%% /channels/*

get_channel_by_pubkey(_Config) ->
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 1),
    ?assertEqual({ok, 400, #{<<"reason">> => <<"Invalid public key">>}},
                 get_channel_by_pubkey_sut(<<"InvalidKey">>)),

    #{i := #{channel_id := ChannelId0, pub := IPub},
      r := #{pub := RPub}
     } = aesc_fsm_SUITE:create_channel_on_port(9311),
    ChannelId = aec_base58c:encode(channel, ChannelId0),

    {ok, 200, #{
        <<"id">> := ChannelId,
        <<"initiator_id">> := InitiatorId,
        <<"responder_id">> := ResponderId,
        <<"delegate_ids">> := [],         %% Update needed
        <<"state_hash">> := StateHash
      }} = get_channel_by_pubkey_sut(ChannelId),

    ?assertEqual({ok, IPub}, aec_base58c:safe_decode(account_pubkey, InitiatorId)),
    ?assertEqual({ok, RPub}, aec_base58c:safe_decode(account_pubkey, ResponderId)),
    ?assertMatch({ok, _}, aec_base58c:safe_decode(state, StateHash)),
    ok.

get_channel_by_pubkey_sut(PubKey) ->
    Host = external_address(),
    PubKey1 = binary_to_list(PubKey),
    http_request(Host, get, "channels/" ++ http_uri:encode(PubKey1), []).

%% /peers/*

get_peer_pubkey(_Config) ->
    {ok, 200, _PeerPubkey} = get_peers_pubkey_sut(),
    ok.

get_peers_pubkey_sut() ->
    Host = external_address(),
    http_request(Host, get, "peers/pubkey", []).

%% /status/*

get_status(_Config) ->
    {ok, 200, #{
       <<"genesis-key-block-hash">>     := _,
       <<"solutions">>                  := _,
       <<"difficulty">>                 := _,
       <<"syncing">>                    := _,
       <<"listening">>                  := _,
       <<"protocols">>                  := _,
       <<"node-version">>               := _,
       <<"node-revision">>              := _,
       <<"peer-count">>                 := _,
       <<"pending-transactions-count">> := _
      }} = get_status_sut(),
    ok.

get_status_sut() ->
    Host = external_address(),
    http_request(Host, get, "status", []).

prepare_tx(TxType, Args) ->
    %assert_required_tx_fields(TxType, Args),
    {Host, Path} = tx_object_http_path(TxType),
    {ok, 200, #{<<"tx">> := EncodedSerializedUnsignedTx}} = http_request(Host, post, Path, Args),
    {ok, SerializedUnsignedTx} = aec_base58c:safe_decode(transaction, EncodedSerializedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} = rpc(aec_keys, sign_tx, [UnsignedTx]),
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx = aec_base58c:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    {TxHash, EncodedSerializedSignedTx}.

post_tx(TxHash, Tx) ->
    {ok, 200, Resp} = post_transactions_sut(Tx),
    ?assertEqual(TxHash, maps:get(<<"tx_hash">>, Resp)),
    Fun = fun() -> tx_in_mempool(TxHash) end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    ok.

%assert_required_tx_fields(TxType, Args) ->
%    lists:foreach(fun(Key) -> true = maps:is_key(Key, Args) end, required_tx_fields(TxType)).

%required_tx_fields(spend_tx) ->
%    [sender, recipient_pubkey, amount, fee, payload];
%required_tx_fields(oracle_register_tx) ->
%    [account, query_format, response_format, query_fee, fee, oracle_ttl].

%% TODO: use /debug/* when available
tx_object_http_path(spend_tx) -> {internal_address(), "debug/transactions/spend"};
tx_object_http_path(oracle_register_tx) -> {internal_address(), "debug/oracles/register"};
tx_object_http_path(oracle_extend_tx) -> {internal_address(), "debug/oracles/extend"};
tx_object_http_path(oracle_query_tx) -> {internal_address(), "debug/oracles/query"};
tx_object_http_path(oracle_response_tx) -> {internal_address(), "debug/oracles/respond"}.

hash(key, Block) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(Block),
    aec_base58c:encode(key_block_hash, Hash0);
hash(micro, Block) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(Block),
    aec_base58c:encode(micro_block_hash, Hash0).

wait_for_key_block_candidate() -> wait_for_key_block_candidate(10).

wait_for_key_block_candidate(0) -> {error, miner_starting};
wait_for_key_block_candidate(N) ->
    case rpc(aec_conductor, get_key_block_candidate, []) of
        {ok, Block} -> {ok, Block};
        {error, not_mining} -> {error, not_mining};
        {error, miner_starting} ->
            timer:sleep(10),
            wait_for_key_block_candidate(N)
    end.

save_config(Keys, Config) ->
    save_config(Keys, Config, []).

save_config([Key | Rest], Config, Acc) ->
    save_config(Rest, Config, [{Key, ?config(Key, Config)} | Acc]);
save_config([], _Config, Acc) ->
    Acc.


%% enpoints

%% tests the following
%% GET contract_create_tx unsigned transaction
%% GET contract_call_tx unsigned transaction
%% due to complexity of contract_call_tx (needs a contract in the state tree)
%% both positive and negative cases are tested in this test
contract_transactions(_Config) ->    % miner has an account
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),

    % contract_create_tx positive test
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aect_sophia:encode_call_data(Code,
                                     InitFunction,
                                     InitArgument),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id => MinerAddress,
                      code => Code,
                      vm_version => 1,
                      deposit => 2,
                      amount => ContractInitBalance,
                      gas => 300,
                      gas_price => 1,
                      fee => 1,
                      call_data => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner_id => aec_id:create(account, MinerPubkey),
                                code => aeu_hex:hexstring_decode(Code),
                                call_data => aeu_hex:hexstring_decode(EncodedInitCallData)}),

    unsigned_tx_positive_test(ValidDecoded, ValidEncoded, fun get_contract_create/1,
                               fun aect_create_tx:new/1, MinerPubkey),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aec_base58c:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    %% Try to get the contract init call object while in mempool
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCreateTxHash),

    {ok, 404, #{<<"reason">> := <<"Proof for contract not found">>}} = get_contract_poi(EncodedContractPubKey),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
        get_accounts_by_pubkey_sut(EncodedContractPubKey),

    % mine a block
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Get the contract init call object
    {ok, 200, InitCallObject} = get_contract_call_object(ContractCreateTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, InitCallObject)),
    ?assertEqual(get_tx_nonce(ContractCreateTxHash), maps:get(<<"caller_nonce">>, InitCallObject)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
        maps:get(<<"contract_id">>, InitCallObject)),
    ?assertEqual(maps:get(gas_price, ValidDecoded), maps:get(<<"gas_price">>, InitCallObject)),
    ?assertMatch({Used, Limit} when
        is_integer(Used) andalso
            is_integer(Limit) andalso
            Limit > 0 andalso
            Used =< Limit,
        {maps:get(<<"gas_used">>, InitCallObject), maps:get(gas, ValidDecoded)}
    ),
    ?assertEqual(<<"ok">>, maps:get(<<"return_type">>, InitCallObject)),
    ?assertMatch(_, maps:get(<<"return_value">>, InitCallObject)),

    {ok, 200, #{<<"poi">> := EncPoI}} = get_contract_poi(EncodedContractPubKey),
    {ok, PoIBin} = aec_base58c:safe_decode(poi, EncPoI),
    PoI = aec_trees:deserialize_poi(PoIBin),
    {ok, ContractInPoI} = aec_trees:lookup_poi(contracts, ContractPubKey, PoI),
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ContractInPoI = rpc(aect_state_tree, get_contract, [ContractPubKey,
                                                         aec_trees:contracts(Trees)]),
    {ok, ContractAccInPoI} = aec_trees:lookup_poi(accounts, ContractPubKey, PoI),
    ContractAccInPoI = rpc(aec_accounts_trees, get, [ContractPubKey,
                                                     aec_trees:accounts(Trees)]),

    %% Assert the balance is the one which we created the contract with
    {ok, 200, #{<<"balance">> := ContractInitBalance}} =
        get_accounts_by_pubkey_sut(EncodedContractPubKey),
    Function = <<"main">>,
    Argument = <<"42">>,
    {ok, EncodedCallData} =
        aect_sophia:encode_call_data(Code,
                                     Function,
                                     Argument),


    ContractCallEncoded = #{ caller_id => MinerAddress,
                             contract_id => EncodedContractPubKey,
                             vm_version => 1,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 1,
                             call_data => EncodedCallData},

    ContractCallDecoded = maps:merge(ContractCallEncoded,
                              #{caller_id => aec_id:create(account, MinerPubkey),
                                contract_id => aec_id:create(contract, ContractPubKey),
                                call_data => aeu_hex:hexstring_decode(EncodedCallData)}),

    unsigned_tx_positive_test(ContractCallDecoded, ContractCallEncoded,
                               fun get_contract_call/1,
                               fun aect_call_tx:new/1, MinerPubkey),

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallTx}} = get_contract_call(ContractCallEncoded),
    ContractCallTxHash = sign_and_post_tx(EncodedUnsignedContractCallTx),

    %% Try to get the call object while in mempool
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCallTxHash),

    % mine blocks
    ok = wait_for_tx_hash_on_chain(ContractCallTxHash),
    ?assert(tx_in_chain(ContractCallTxHash)),

    %% Get the call object
    {ok, 200, CallObject} = get_contract_call_object(ContractCallTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, CallObject, <<>>)),
    ?assertEqual(get_tx_nonce(ContractCallTxHash), maps:get(<<"caller_nonce">>, CallObject)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
                 maps:get(<<"contract_id">>, CallObject, <<>>)),
    ?assertEqual(maps:get(gas_price, ContractCallDecoded), maps:get(<<"gas_price">>, CallObject)),
    ?assertMatch({Used, Limit} when
      is_integer(Used) andalso
      is_integer(Limit) andalso
      Limit > 0 andalso
      Used =< Limit,
      {maps:get(<<"gas_used">>, CallObject), maps:get(gas, ContractCallDecoded)}
      ),
    ?assertEqual(<<"ok">>, maps:get(<<"return_type">>, CallObject)),

    %% Test to call the contract without a transaction.
    {ok, 200, #{<<"out">> := DirectCallResult}} =
        call_contract_directly(#{<<"abi">> => <<"sophia-address">>,
                                 <<"code">> => EncodedContractPubKey,
                                 <<"function">> => Function,
                                 <<"arg">> => Argument}),
    ReturnBin = maps:get(<<"return_value">>, CallObject),
    {ok, DecodedReturnValue} = aehttp_logic:contract_decode_data(<<"int">>, ReturnBin),
    {ok, 200, #{<<"data">> := DecodedCallResult}} =
        get_contract_decode_data(
          #{ 'sophia-type' => <<"int">>,
             data => DirectCallResult}),
    ?assertEqual(DecodedReturnValue, DecodedCallResult),
    #{<<"value">> := 42} = DecodedReturnValue,

    ComputeCCallEncoded = #{ caller_id => MinerAddress,
                             contract_id => EncodedContractPubKey,
                             vm_version => 1,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 1,
                             function => Function,
                             arguments => Argument},

    {ok, EncodedCallData} = aect_sophia:encode_call_data(Code, Function,
                                                         Argument),
    ComputeCCallDecoded = maps:merge(ComputeCCallEncoded,
                              #{caller_id => aec_id:create(account, MinerPubkey),
                                contract_id => aec_id:create(contract, ContractPubKey),
                                call_data => aeu_hex:hexstring_decode(EncodedCallData)}),

    unsigned_tx_positive_test(ComputeCCallDecoded, ComputeCCallEncoded,
                               fun get_contract_call_compute/1,
                               fun aect_call_tx:new/1, MinerPubkey),

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallComputeTx}} =
        get_contract_call_compute(ComputeCCallEncoded),
    ContractCallComputeTxHash =
        sign_and_post_tx(EncodedUnsignedContractCallComputeTx),

    % mine a block
    ok = wait_for_tx_hash_on_chain(ContractCallComputeTxHash),
    ?assert(tx_in_chain(ContractCallComputeTxHash)),

    %% Get the call object
    {ok, 200, CallObject1} = get_contract_call_object(ContractCallComputeTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, CallObject1, <<>>)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
                 maps:get(<<"contract_id">>, CallObject1, <<>>)),

    {ok, 200, #{<<"data">> := DecodedCallReturnValue}} =
        get_contract_decode_data(
          #{ 'sophia-type' => <<"int">>,
             data => maps:get(<<"return_value">>, CallObject1)}),
    %% Check that it is also the same as the direct call result
    ?assertEqual(DecodedCallReturnValue, DecodedCallResult),

    %% negative tests
    %% Invalid hashes
    %% invalid owner hash
    <<_, InvalidHash/binary>> = MinerAddress,
    {ok, 400, #{<<"reason">> := <<"Invalid hash: owner_id">>}} =
        get_contract_create(maps:put(owner_id, InvalidHash, ValidEncoded)),
    % invalid caller hash
    {ok, 400, #{<<"reason">> := <<"Invalid hash: caller_id">>}} =
        get_contract_call(maps:put(caller_id, InvalidHash, ContractCallEncoded)),
    % invalid caller hash
    {ok, 400, #{<<"reason">> := <<"Invalid hash: caller_id">>}} =
        get_contract_call_compute(maps:put(caller_id, InvalidHash,
                                           ComputeCCallEncoded)),
    %% account not found
    RandAddress = aec_base58c:encode(account_pubkey, random_hash()),
    RandContractAddress =aec_base58c:encode(contract_pubkey, random_hash()),
    %% owner not found
    {ok, 404, #{<<"reason">> := <<"Account of owner_id not found">>}} =
        get_contract_create(maps:put(owner_id, RandAddress, ValidEncoded)),
    %% caller not found
    {ok, 404, #{<<"reason">> := <<"Account of caller_id not found">>}} =
        get_contract_call(maps:put(caller_id, RandAddress, ContractCallEncoded)),
    %% contract not found
    {ok, 404, #{<<"reason">> := <<"Contract address for key contract_id not found">>}} =
        get_contract_call(maps:put(contract_id, RandContractAddress,
                                   ContractCallEncoded)),
    %% caller not found
    {ok, 404, #{<<"reason">> := <<"Account of caller_id not found">>}} =
        get_contract_call_compute(maps:put(caller_id, RandAddress,
                                           ComputeCCallEncoded)),
    %% contract not found
    {ok, 404, #{<<"reason">> := <<"Contract address for key contract_id not found">>}} =
        get_contract_call_compute(maps:put(contract_id, RandContractAddress,
                                           ComputeCCallEncoded)),

    %% Invalid hexstrings
    InvalidHex1 = <<"1234">>,
    InvalidHex2 = <<"0xXYZ">>,

    % invalid code
    {ok, 400, #{<<"reason">> := <<"Not hex string: code">>}} =
        get_contract_create(maps:put(code, InvalidHex1, ValidEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not hex string: code">>}} =
        get_contract_create(maps:put(code, InvalidHex2, ValidEncoded)),
    % invalid call data
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_create(maps:put(call_data, InvalidHex1, ValidEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_create(maps:put(call_data, InvalidHex2, ValidEncoded)),
    % invalid call data
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_call(maps:put(call_data, InvalidHex1, ContractCallEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_call(maps:put(call_data, InvalidHex2, ContractCallEncoded)),

    {ok, 400, #{<<"reason">> := <<"Failed to compute call_data, reason: bad argument">>}} =
        get_contract_call_compute(maps:put(arguments, <<"garbadge">>,
                                           ComputeCCallEncoded)),

    %% Call objects
    {ok, 200, #{<<"tx">> := SpendTx}} = post_spend_tx(MinerAddress, 1, 1),
    SpendTxHash = sign_and_post_tx(SpendTx),
    ok = wait_for_tx_hash_on_chain(SpendTxHash),
    {ok, 400, #{<<"reason">> := <<"Tx is not a create or call">>}} =
        get_contract_call_object(SpendTxHash),

    ok.

%% Tests the following
%% GET contract_create_compute_tx unsigned transaction
%% GET contract_call_compute_tx unsigned transaction
%% No testing of negative cases as these are same as for "normal" create.
contract_create_compute_transaction(_Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),

    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id => MinerAddress,
                      code => Code,
                      vm_version => 1,
                      deposit => 2,
                      amount => ContractInitBalance,
                      gas => 300,
                      gas_price => 1,
                      fee => 1,
                      arguments => <<"()">> },

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create_compute(ValidEncoded),

    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    % mine a block
    wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    Function = <<"main">>,
    Argument = <<"(42)">>,
    ComputeCCallEncoded = #{ caller_id => MinerAddress,
                             contract_id => EncodedContractPubKey,
                             vm_version => 1,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 1,
                             function => Function,
                             arguments => Argument},

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallComputeTx}} =
        get_contract_call_compute(ComputeCCallEncoded),
    ContractCallComputeTxHash =
        sign_and_post_tx(EncodedUnsignedContractCallComputeTx),

    % mine a block
    wait_for_tx_hash_on_chain(ContractCallComputeTxHash),
    ?assert(tx_in_chain(ContractCallComputeTxHash)),


    %% Get the call object
    {ok, 200, CallObject} = get_contract_call_object(ContractCallComputeTxHash),

    {ok, 200, #{<<"data">> := DecodedCallReturnValue}} =
        get_contract_decode_data(
          #{ 'sophia-type' => <<"int">>,
             data => maps:get(<<"return_value">>, CallObject)}),
    #{ <<"type">> := <<"word">>, <<"value">> := 42 } = DecodedCallReturnValue,

    ok.

contract_create_transaction_init_error(_Config) ->
    % miner has an account
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),

    % contract_create_tx positive test
    Code = aeu_hex:hexstring_encode(<<"NOT PROPER BYTE CODE">>),

    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aect_sophia:encode_call_data(Code,
            InitFunction,
            InitArgument),
    ValidEncoded = #{ owner_id   => MinerAddress,
                      code       => Code,
                      vm_version => 1,
                      deposit    => 2,
                      amount     => 1,
                      gas        => 30,
                      gas_price  => 1,
                      fee        => 1,
                      call_data  => EncodedInitCallData},
    ValidDecoded = maps:merge(ValidEncoded,
        #{owner => MinerPubkey,
            code => aeu_hex:hexstring_decode(Code),
            call_data => aeu_hex:hexstring_decode(EncodedInitCallData)}),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
        <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aec_base58c:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    %% Try to get the contract init call object while in mempool
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCreateTxHash),

    % mine blocks
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Get the contract init call object
    {ok, 200, InitCallObject} = get_contract_call_object(ContractCreateTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, InitCallObject)),
    ?assertEqual(get_tx_nonce(ContractCreateTxHash), maps:get(<<"caller_nonce">>, InitCallObject)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
        maps:get(<<"contract_id">>, InitCallObject)),
    ?assertEqual(maps:get(gas_price, ValidDecoded), maps:get(<<"gas_price">>, InitCallObject)),
    ?assertMatch({Used, Limit} when
        is_integer(Used) andalso
            is_integer(Limit) andalso
            Limit > 0 andalso
            Used =< Limit,
        {maps:get(<<"gas_used">>, InitCallObject), maps:get(gas, ValidDecoded)}
    ),
    ?assertEqual(<<"error">>, maps:get(<<"return_type">>, InitCallObject)),
    ?assertMatch(_, maps:get(<<"return_value">>, InitCallObject)),

    ok.

oracle_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    OracleAddress = aec_base58c:encode(oracle_pubkey, MinerPubkey),

    % oracle_register_tx positive test
    RegEncoded = #{account_id => MinerAddress,
                   query_format => <<"something">>,
                   response_format => <<"something else">>,
                   query_fee => 1,
                   fee => 6,
                   oracle_ttl => #{type => <<"block">>, value => 2000}},
    RegDecoded = maps:merge(RegEncoded,
                            #{account_id => aec_id:create(account, MinerPubkey),
                              query_format => <<"something">>,
                              response_format => <<"something else">>,
                              oracle_ttl => {block, 2000}}),
    unsigned_tx_positive_test(RegDecoded, RegEncoded,
                               fun get_oracle_register/1,
                               fun aeo_register_tx:new/1, MinerPubkey),

    % in order to test a positive case for oracle_extend_tx and
    % oracle_query_tx we first need an actual Oracle on the chain

    {ok, 200, #{<<"tx">> := RegisterTx}} = get_oracle_register(RegEncoded),
    RegisterTxHash = sign_and_post_tx(RegisterTx),

    % mine blocks to include it
    ct:log("Before oracle registered nonce is ~p", [rpc(aec_next_nonce, pick_for_account, [MinerPubkey])]),
    ok = wait_for_tx_hash_on_chain(RegisterTxHash),
    ct:log("Oracle registered nonce is ~p", [rpc(aec_next_nonce, pick_for_account, [MinerPubkey])]),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty

    % oracle_extend_tx positive test
    ExtEncoded = #{oracle_id => aec_base58c:encode(oracle_pubkey, MinerPubkey),
                   fee => 2,
                   oracle_ttl => #{type => <<"delta">>, value => 500}},
    ExtDecoded = maps:merge(ExtEncoded,
                            #{oracle_id => aec_id:create(oracle, MinerPubkey),
                              oracle_ttl => {delta, 500}}),
    unsigned_tx_positive_test(ExtDecoded, ExtEncoded,
                               fun get_oracle_extend/1,
                               fun aeo_extend_tx:new/1, MinerPubkey),

    % oracle_query_tx positive test
    QueryEncoded = #{sender_id => MinerAddress,
                     oracle_id => aec_base58c:encode(oracle_pubkey, MinerPubkey),
                     query => <<"Hejsan Svejsan">>,
                     query_fee => 2,
                     fee => 30,
                     query_ttl => #{type => <<"block">>, value => 30},
                     response_ttl => #{type => <<"delta">>, value => 20}},
    QueryDecoded = maps:merge(QueryEncoded,
                              #{sender_id => aec_id:create(account, MinerPubkey),
                                oracle_id => aec_id:create(oracle, MinerPubkey),
                                query_ttl => {block, 30},
                                response_ttl => {delta, 20}}),
    unsigned_tx_positive_test(QueryDecoded, QueryEncoded,
                               fun get_oracle_query/1,
                               fun aeo_query_tx:new/1, MinerPubkey),

    % in order to test a positive case for oracle_response_tx we first need an
    % actual Oracle query on the chain

    {ok, 200, _} = get_balance_at_top(),
    {ok, QueryNonce} = rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
    ct:log("Nonce is ~p", [QueryNonce]),
    QueryId = aeo_query:id(MinerPubkey, QueryNonce, MinerPubkey),
    {ok, 200, #{<<"tx">> := QueryTx}} = get_oracle_query(QueryEncoded),
    QueryTxHash = sign_and_post_tx(QueryTx),

    % mine blocks to include it
    ok = wait_for_tx_hash_on_chain(QueryTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty

    ResponseEncoded = #{oracle_id => OracleAddress,
                        query_id => aec_base58c:encode(oracle_query_id,
                                                       QueryId),
                        response => <<"Hejsan">>,
                        fee => 3},
    ResponseDecoded = maps:merge(ResponseEncoded,
                              #{oracle_id => aec_id:create(oracle, MinerPubkey),
                                query_id => QueryId}),
    unsigned_tx_positive_test(ResponseDecoded, ResponseEncoded,
                               fun get_oracle_response/1,
                               fun aeo_response_tx:new/1, MinerPubkey),
    {ok, 200, #{<<"tx">> := ResponseTx}} = get_oracle_response(ResponseEncoded),
    ResponseTxHash = sign_and_post_tx(ResponseTx),
    % mine a block to include it
    ok = wait_for_tx_hash_on_chain(ResponseTxHash),

    %% negative tests

    % broken hash
    <<_, InvalidHash/binary>> = MinerAddress,
    {ok, 400, #{<<"reason">> := <<"Invalid hash: account_id">>}} =
        get_oracle_register(maps:put(account_id, InvalidHash, RegEncoded)),

    {ok, 400, #{<<"reason">> := <<"Invalid hash: sender_id">>}} =
        get_oracle_query(maps:put(sender_id, InvalidHash, QueryEncoded)),
    {ok, 400, #{<<"reason">> := <<"Invalid hash: oracle_id">>}} =
        get_oracle_query(maps:put(oracle_id, InvalidHash, QueryEncoded)),

    {ok, 400, #{<<"reason">> := <<"Invalid hash: oracle_id">>}} =
        get_oracle_response(maps:put(oracle_id, InvalidHash, ResponseEncoded)),

    %% account not found
    RandAddress = aec_base58c:encode(account_pubkey, random_hash()),
    RandOracleAddress = aec_base58c:encode(oracle_pubkey, random_hash()),
    RandQueryID = aec_base58c:encode(oracle_query_id, random_hash()),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_oracle_register(maps:put(account_id, RandAddress, RegEncoded)),

    {ok, 404, #{<<"reason">> := <<"Account of sender_id not found">>}} =
        get_oracle_query(maps:put(sender_id, RandAddress, QueryEncoded)),

    {ok, 404, #{<<"reason">> := <<"Account of oracle_id not found">>}} =
        get_oracle_response(maps:put(oracle_id, RandOracleAddress, ResponseEncoded)),

    {ok, 404, #{<<"reason">> := <<"Oracle address for key oracle_id not found">>}} =
        get_oracle_query(maps:put(oracle_id, RandOracleAddress, QueryEncoded)),

    {ok, 404, #{<<"reason">> := <<"Oracle query for key query_id not found">>}} =
        get_oracle_response(maps:put(query_id, RandQueryID, ResponseEncoded)),

    %% broken ttl
    BrokenTTL1 = #{<<"invalid">> => <<"structure">>},
    BrokenTTL2 = #{<<"type">> => <<"hejsan">>, <<"value">> => 20},

    {ok, 400, _} =
        get_oracle_register(maps:put(ttl, BrokenTTL1, RegEncoded)),
    {ok, 400, _} =
        get_oracle_register(maps:put(ttl, BrokenTTL2, RegEncoded)),

    {ok, 400, _} =
        get_oracle_query(maps:put(query_ttl, BrokenTTL1, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(query_ttl, BrokenTTL2, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL1, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL2, QueryEncoded)),
    % test non-relative ttl
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, #{type => <<"block">>,
                                                  value => 2}, QueryEncoded)),
    ok.

%% tests the following
%% GET preclaim_tx unsigned transaction
%% GET claim_tx unsigned transaction
%% GET update_tx unsigned transaction
%% GET transfer_tx unsigned transaction
%% GET revoke_tx unsigned transaction
nameservice_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    nameservice_transaction_preclaim(MinerAddress, MinerPubkey),
    nameservice_transaction_claim(MinerAddress, MinerPubkey),
    nameservice_transaction_update(MinerAddress, MinerPubkey),
    nameservice_transaction_transfer(MinerAddress, MinerPubkey),
    nameservice_transaction_revoke(MinerAddress, MinerPubkey),
    ok.

nameservice_transaction_preclaim(MinerAddress, MinerPubkey) ->
    Commitment = random_hash(),
    Encoded = #{account_id => MinerAddress,
                commitment_id => aec_base58c:encode(commitment, Commitment),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account_id => aec_id:create(account, MinerPubkey),
                          commitment_id => aec_id:create(commitment, Commitment)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_preclaim/1,
                               fun aens_preclaim_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, account_id, Encoded, fun get_name_preclaim/1),
    test_invalid_hash({commitment, MinerPubkey}, commitment_id, Encoded, fun get_name_preclaim/1),
    test_missing_address(account_id, Encoded, fun get_name_preclaim/1),
    ok.

test_invalid_hash({PubKeyType, PubKey}, MapKey0, Encoded, APIFun) when is_atom(PubKeyType) ->
    {MapKey, Name} =
        case MapKey0 of
            {_, _} = Pair -> Pair;
            K -> {K, K}
        end,
    CorrectAddress = aec_base58c:encode(PubKeyType, PubKey),
    Msg = list_to_binary("Invalid hash: " ++ atom_to_list(Name)),
    <<_, HashWithBrokenPrefix/binary>> = CorrectAddress,
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HashWithBrokenPrefix, Encoded)),

    <<_Prefix:3/binary, HashWithNoPrefix/binary>> = CorrectAddress,
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HashWithNoPrefix, Encoded)),

    case aec_base58c:byte_size_for_type(PubKeyType) of
        not_applicable -> pass;
        _ ->
            <<ShortHash:10/binary, _Rest/binary>> = CorrectAddress,
            {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, ShortHash, Encoded)),

            BS = byte_size(PubKey),
            HalfSize = BS div 2,
            <<FirstHalfKey:HalfSize/binary, _SecondHalfKey/binary>> = PubKey,
            HalfHash = aec_base58c:encode(PubKeyType, FirstHalfKey),
            {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HalfHash, Encoded))
    end,
    ok.

test_missing_address(Key, Encoded, APIFun) ->
    Msg = list_to_binary("Account of " ++ atom_to_list(Key) ++ " not found"),
    RandAddress = aec_base58c:encode(account_pubkey, random_hash()),
    {ok, 404, #{<<"reason">> := Msg}} =
        APIFun(maps:put(Key, RandAddress, Encoded)),
    ok.

nameservice_transaction_claim(MinerAddress, MinerPubkey) ->
    Name = <<"name.test">>,
    Salt = 1234,

    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = get_commitment_id(Name, Salt),
    {ok, CHash} = aec_base58c:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => 1,
                     account_id    => MinerAddress},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = get_name_preclaim(PreclaimData),
    PreclaimTxHash = sign_and_post_tx(PreclaimTxEnc),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    {ok, BS1} = aecore_suite_utils:mine_blocks_until_tx_on_chain(
                    aecore_suite_utils:node_name(?NODE), PreclaimTxHash, 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    Encoded = #{account_id => MinerAddress,
                name => aec_base58c:encode(name, Name),
                name_salt => Salt,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account_id => aec_id:create(account, MinerPubkey),
                          name => Name}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_claim/1,
                               fun aens_claim_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, account_id, Encoded, fun get_name_claim/1),
    test_invalid_hash({name, MinerPubkey}, name, Encoded, fun get_name_claim/1),
    test_missing_address(account_id, Encoded, fun get_name_claim/1),

    %% missing registar
    Missing = aec_base58c:encode(name, <<"missing">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: no_registrar">>}} =
        get_name_claim(maps:put(name, Missing, Encoded)),
    MissingReg = aec_base58c:encode(name, <<"missing.reg">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_name_claim(maps:put(name, MissingReg, Encoded)),
    ok.

nameservice_transaction_update(MinerAddress, MinerPubkey) ->
    NameHash = random_hash(),
    Pointers = [],
    Encoded = #{account_id => MinerAddress,
                name_id => aec_base58c:encode(name, NameHash),
                name_ttl => 3,
                client_ttl => 2,
                pointers => Pointers,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account_id => aec_id:create(account, MinerPubkey),
                          pointers => Pointers,
                          name_id => aec_id:create(name, NameHash)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_update/1,
                               fun aens_update_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, account_id, Encoded, fun get_name_update/1),
    test_invalid_hash({name, MinerPubkey}, name_id, Encoded, fun get_name_update/1),
    test_missing_address(account_id, Encoded, fun get_name_update/1),
    %% test broken pointers
    TestBrokenPointers =
        fun(P) ->
            {ok, 400, #{<<"reason">> := <<"Invalid pointers">>}} =
                get_name_update(maps:put(pointers, P, Encoded))
        end,
    TestBrokenPointers([#{<<"key">> => <<"k2">>, <<"id">> => <<"not a valid pointer">>}]),
    TestBrokenPointers([#{<<"invalid_key">> => <<"k2">>, <<"id">> => <<"not a valid pointer">>}]),
    ok.

nameservice_transaction_transfer(MinerAddress, MinerPubkey) ->
    RandAddress = random_hash(),
    NameHash = random_hash(),
    Encoded = #{account_id => MinerAddress,
                name_id => aec_base58c:encode(name, NameHash),
                recipient_id => aec_base58c:encode(account_pubkey, RandAddress),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account_id => aec_id:create(account, MinerPubkey),
                          recipient_id => aec_id:create(account, RandAddress),
                          name_id => aec_id:create(name, NameHash)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_transfer/1,
                               fun aens_transfer_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, account_id, Encoded, fun get_name_transfer/1),
    test_invalid_hash({account_pubkey, MinerPubkey}, recipient_id, Encoded, fun get_name_transfer/1),
    test_invalid_hash({name, MinerPubkey}, name_id, Encoded, fun get_name_transfer/1),
    test_missing_address(account_id, Encoded, fun get_name_transfer/1),
    ok.

nameservice_transaction_revoke(MinerAddress, MinerPubkey) ->
    NameHash = random_hash(),
    Encoded = #{account_id => MinerAddress,
                name_id => aec_base58c:encode(name, NameHash),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account_id => aec_id:create(account, MinerPubkey),
                          name_id => aec_id:create(name, NameHash)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_revoke/1,
                               fun aens_revoke_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, account_id, Encoded, fun get_name_revoke/1),
    test_invalid_hash({account_pubkey, MinerPubkey}, name_id, Encoded, fun get_name_revoke/1),
    test_missing_address(account_id, Encoded, fun get_name_revoke/1),
    ok.

%% tests the following
%% GET channel_create_tx unsigned transaction
%% GET channel_deposit_tx unsigned transaction
%% GET channel_withdraw_tx unsigned transaction
%% GET channel_close_mutual_tx unsigned transaction
%% GET channel_close_solo unsigned transaction
%% GET channel_slash_tx unsigned transaction
%% GET channel_settle_tx unsigned transaction
state_channels_onchain_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    ParticipantPubkey = random_hash(),
    ok = give_tokens(ParticipantPubkey, 100),
    {ok, AeTx} = state_channels_create(MinerPubkey, ParticipantPubkey),
    ChannelPubKey = state_channel_pubkey(AeTx),
    state_channels_deposit(ChannelPubKey, MinerPubkey),
    state_channels_withdrawal(ChannelPubKey, MinerPubkey),
    state_channels_snapshot_solo(ChannelPubKey, MinerPubkey),
    state_channels_close_mutual(ChannelPubKey, MinerPubkey),
    state_channels_close_solo(ChannelPubKey, MinerPubkey),
    state_channels_slash(ChannelPubKey, MinerPubkey),
    state_channels_settle(ChannelPubKey, MinerPubkey),
    ok.

state_channel_pubkey(Tx) ->
    {channel_create_tx, ChCTx} = aetx:specialize_type(Tx),
    Initiator = aesc_create_tx:initiator_pubkey(ChCTx),
    Nonce = aesc_create_tx:nonce(ChCTx),
    Responder = aesc_create_tx:responder_pubkey(ChCTx),
    aesc_channels:pubkey(Initiator, Nonce, Responder).

state_channels_create(MinerPubkey, ResponderPubkey) ->
    Encoded = #{initiator_id => aec_base58c:encode(account_pubkey, MinerPubkey),
                initiator_amount => 2,
                responder_id => aec_base58c:encode(account_pubkey, ResponderPubkey),
                responder_amount => 3,
                channel_reserve => 5,
                lock_period => 20,
                state_hash => aec_base58c:encode(state, ?BOGUS_STATE_HASH),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{initiator_id => aec_id:create(account, MinerPubkey),
                          responder_id => aec_id:create(account, ResponderPubkey),
                          state_hash => ?BOGUS_STATE_HASH}),
    {ok, Tx} = unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_create/1,
                               fun aesc_create_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, initiator_id, Encoded, fun get_channel_create/1),
    test_invalid_hash({account_pubkey, ResponderPubkey}, responder_id, Encoded, fun get_channel_create/1),
    test_missing_address(initiator_id, Encoded, fun get_channel_create/1),
    {ok, Tx}.

state_channels_deposit(ChannelId, MinerPubkey) ->
    MinerAddress = aec_base58c:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from_id => MinerAddress,
                amount => 2,
                state_hash => aec_base58c:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{channel_id => aec_id:create(channel, ChannelId),
                          from_id => aec_id:create(account, MinerPubkey),
                          state_hash => ?BOGUS_STATE_HASH}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_deposit/1,
                               fun aesc_deposit_tx:new/1, MinerPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
                            MinerPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded1, fun get_channel_deposit/1),
    ok.

state_channels_withdrawal(ChannelId, MinerPubkey) ->
    MinerAddress = aec_base58c:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                to_id => MinerAddress,
                amount => 2,
                state_hash => aec_base58c:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{channel_id => aec_id:create(channel, ChannelId),
                          to_id => aec_id:create(account, MinerPubkey),
                          state_hash => ?BOGUS_STATE_HASH}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_withdrawal/1,
                               fun aesc_withdraw_tx:new/1, MinerPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
                            MinerPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash({account_pubkey, MinerPubkey}, to_id, Encoded1, fun get_channel_withdrawal/1),
    ok.

state_channels_snapshot_solo(ChannelId, MinerPubkey) ->
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from_id => aec_base58c:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from_id => aec_id:create(account, MinerPubkey),
                          channel_id => aec_id:create(channel, ChannelId)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_snapshot_solo/1,
                               fun aesc_snapshot_solo_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_snapshot_solo/1),
    ok.

state_channels_close_mutual(ChannelId, InitiatorPubkey) ->
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                initiator_amount_final => 4,
                responder_amount_final => 3,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{channel_id => aec_id:create(channel, ChannelId)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_close_mutual/1,
                               fun aesc_close_mutual_tx:new/1, InitiatorPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [InitiatorPubkey]),
                            InitiatorPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash({channel, ChannelId}, channel_id, Encoded1, fun get_channel_close_mutual/1),
    ok.

state_channels_close_solo(ChannelId, MinerPubkey) ->
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from_id => aec_base58c:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                poi => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from_id => aec_id:create(account, MinerPubkey),
                          channel_id => aec_id:create(channel, ChannelId),
                          poi => PoI}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_close_solo/1,
                               fun aesc_close_solo_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_close_solo/1),
    BrokenPoIs = [<<>>, <<"hejsan svejsan">>],
    lists:foreach(
        fun(BrokenPoI) ->
            EncBrokenPoI =  aec_base58c:encode(poi, BrokenPoI),
            {ok, 400, #{<<"reason">> := <<"Invalid proof of inclusion">>}}
                = get_channel_close_solo(maps:put(poi, EncBrokenPoI, Encoded))
        end,
        BrokenPoIs),
    ok.

state_channels_slash(ChannelId, MinerPubkey) ->
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from_id => aec_base58c:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                poi => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from_id => aec_id:create(account, MinerPubkey),
                          channel_id => aec_id:create(channel, ChannelId),
                          poi => PoI}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_slash/1,
                               fun aesc_slash_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_slash/1),

    BrokenPoIs = [<<>>, <<"hejsan svejsan">>],
    lists:foreach(
        fun(BrokenPoI) ->
            EncBrokenPoI =  aec_base58c:encode(poi, BrokenPoI),
            {ok, 400, #{<<"reason">> := <<"Invalid proof of inclusion">>}}
                = get_channel_slash(maps:put(poi, EncBrokenPoI, Encoded))
        end,
        BrokenPoIs),
    ok.

state_channels_settle(ChannelId, MinerPubkey) ->
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from_id => aec_base58c:encode(account_pubkey, MinerPubkey),
                initiator_amount_final => 4,
                responder_amount_final => 3,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from_id => aec_id:create(account, MinerPubkey),
                          channel_id => aec_id:create(channel, ChannelId)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_settle/1,
                               fun aesc_settle_tx:new/1, MinerPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
                            MinerPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded1, fun get_channel_settle/1),
    ok.

%% tests the following
%% GET spend_tx unsigned transaction
spend_transaction(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    RandAddress = random_hash(),
    Encoded = #{sender_id => MinerAddress,
                recipient_id => aec_base58c:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 1,
                ttl => 43,
                payload => <<"hejsan svejsan">>},
    Decoded = maps:merge(Encoded,
                        #{sender_id => aec_id:create(account, MinerPubkey),
                          recipient_id => aec_id:create(account, RandAddress)}),
    {ok, T} = unsigned_tx_positive_test(Decoded, Encoded,
                                  fun get_spend/1,
                                  fun aec_spend_tx:new/1, MinerPubkey),
    {spend_tx, SpendTx} = aetx:specialize_type(T),
    <<"hejsan svejsan">> = aec_spend_tx:payload(SpendTx),

    test_invalid_hash({account_pubkey, MinerPubkey}, sender_id, Encoded, fun get_spend/1),
    test_invalid_hash({account_pubkey, MinerPubkey}, {recipient_id, recipient_id}, Encoded, fun get_spend/1),
    test_missing_address(sender_id, Encoded, fun get_spend/1),
    ok.

%% tests the following
%% GET spend_tx unsigned transaction with an non-present key in request
unknown_atom_in_spend_tx(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_node_pubkey(),
    RandAddress = random_hash(),
    Encoded = #{sender_id => MinerAddress,
                recipient_id => aec_base58c:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 1,
                %% this tests relies on this being an atom unknown to the VM
                %% if someone adds this atom in particular, please modify the
                %% binary below accordingly
                <<"hejsan_svejsan_atom">> => 10,
                payload => <<"hejsan svejsan">>},
    {ok, 400, #{<<"reason">> := <<"Invalid parameter: hejsan_svejsan_atom">>}} = get_spend(Encoded),
    ok.

unsigned_tx_positive_test(Data, Params0, HTTPCallFun, NewFun, Pubkey) ->
    unsigned_tx_positive_test(Data, Params0, HTTPCallFun, NewFun, Pubkey,
                              false).

unsigned_tx_positive_test(Data, Params0, HTTPCallFun, NewFun, Pubkey,
                          NonceRequred) ->
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [Pubkey]),
                            Pubkey},
    Test =
        fun(Nonce, P) ->
            ct:log("PARAMS ~p", [P]),
            {ok, ExpectedTx} = NewFun(maps:put(nonce, Nonce, Data)),
            {ok, 200, #{<<"tx">> := ActualTx}} = HTTPCallFun(P),
            {ok, SerializedTx} = aec_base58c:safe_decode(transaction, ActualTx),
            Tx = aetx:deserialize_from_binary(SerializedTx),
            ct:log("Expected ~p~nActual ~p", [ExpectedTx, Tx]),
            ExpectedTx = Tx,
            Tx
        end,
    Params =
        case NonceRequred of
            true -> maps:put(nonce, NextNonce, Params0);
            false -> Params0
        end,
    Tx = Test(NextNonce, Params),
    RandomNonce = rand:uniform(999) + 1,
    Test(RandomNonce, maps:put(nonce, RandomNonce, Params)),
    {ok, Tx}.

get_transaction(_Config) ->
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_node_pubkey(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    TxHashes = add_spend_txs(),
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 2),
    lists:foreach(
        fun(TxHash) ->
                {ok, 200, #{<<"hash">> := TxHash1}} =
                    get_transactions_by_hash_sut(TxHash),
                ?assertEqual(TxHash, TxHash1)
        end,
      TxHashes),

    %% test in mempool
    RandAddress = random_hash(),
    Encoded = #{sender_id => EncodedPubKey,
                recipient_id => aec_base58c:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 1,
                payload => <<"foo">>},
    {ok, 200, #{<<"tx">> := EncodedSpendTx}} = get_spend(Encoded),
    {ok, SpendTxBin} = aec_base58c:safe_decode(transaction, EncodedSpendTx),
    SpendTx = aetx:deserialize_from_binary(SpendTxBin),
    {ok, SignedSpendTx} = rpc(aec_keys, sign_tx, [SpendTx]),
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedSpendTx)),

    SerializedSpendTx = aetx_sign:serialize_to_binary(SignedSpendTx),
    {ok, 200, _} = post_transactions_sut(aec_base58c:encode(transaction, SerializedSpendTx)),
    {ok, 200, PendingTx} = get_transactions_by_hash_sut(TxHash),
    Expected = aetx_sign:serialize_for_client_pending(SignedSpendTx),
    Expected = PendingTx,

    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ok.

%% Maybe this test should be broken into a couple of smaller tests
%% it currently tests the positive cases for
%% GET externalAPI/transactions
%% POST internalAPI/debug/transactions/spend
%% GET externalAPI/account/balance
pending_transactions(_Config) ->
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),
    InitialBalance =
        case get_balance_at_top() of
            {ok, 404, #{<<"reason">> := <<"Account not found">>}} -> 0;
            {ok, 200, #{<<"balance">> := Bal00}} -> Bal00
        end,
    AmountToSpent = 3,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(AmountToSpent, 1),
    MineReward = rpc(aec_governance, block_mine_reward, []),
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), BlocksToMine),
    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),

    ct:log("Bal0: ~p, Initial Balance: ~p, Blocks to mine: ~p, Mine reward: ~p",
           [Bal0, InitialBalance, BlocksToMine, MineReward]),
    {Bal0, _, _} = {InitialBalance + BlocksToMine * MineReward, Bal0,
                    {InitialBalance, BlocksToMine, MineReward}},
    true = (is_integer(Bal0) andalso Bal0 > AmountToSpent + Fee),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    %{ok, SenderPubKey} = rpc:call(?NODE, aec_keys, pubkey, [], 5000),
    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, ReceiverPubKey)),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aec_base58c:encode(account_pubkey, ReceiverPubKey), AmountToSpent, Fee),
    sign_and_post_tx(SpendTx),
    {ok, NodeTxs} = rpc(aec_tx_pool, peek, [infinity]),
    true = length(NodeTxs) =:= 1, % not empty anymore
    {ok, 200, #{<<"transactions">> := ReturnedTxs}} = get_pending_transactions(),
    ExpectedTxs = [aetx_sign:serialize_for_client_pending(T) || T <- NodeTxs],
    true = length(ExpectedTxs) =:= length(ReturnedTxs),
    true = lists:all(fun(Tx) -> lists:member(Tx, ExpectedTxs) end, ReturnedTxs),

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, ReceiverPubKey)),


    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 3),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    {ok, 200, #{<<"balance">> := Bal1}} = get_balance_at_top(),
    ct:log("Bal1: ~p, Bal0: ~p, Mine reward: ~p, Fee: ~p, Amount to spend: ~p",
           [Bal1, Bal0, 3 * MineReward, Fee, AmountToSpent]),
    {Bal1, _} = {Bal0 + 3 * MineReward + Fee - Fee - AmountToSpent, Bal1},
    {ok, 200, #{<<"balance">> := AmountToSpent}} =
                 get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, ReceiverPubKey)),
    ok.

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(_Config) ->
    Amount = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender_id => aec_id:create(account, PubKey),
            recipient_id => aec_id:create(account, random_hash()),
            amount => Amount,
            fee => Fee,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = rpc(aec_keys, sign_tx, [SpendTx]),
    ExpectedHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, 200, #{<<"tx_hash">> := ExpectedHash}} =
        post_transactions_sut(aec_base58c:encode(transaction, aetx_sign:serialize_to_binary(SignedTx))),
    {ok, [SignedTx]} = rpc(aec_tx_pool, peek, [infinity]), % same tx
    ok.

post_broken_tx(_Config) ->
    Amount = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender_id => aec_id:create(account, PubKey),
            recipient_id => aec_id:create(account, random_hash()),
            amount => Amount,
            fee => Fee,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = rpc(aec_keys, sign_tx, [SpendTx]),
    SignedTxBin = aetx_sign:serialize_to_binary(SignedTx),
    BrokenTxBin = case SignedTxBin of
                    <<1:1, Rest/bits>> -> <<0:1, Rest/bits>>;
                    <<0:1, Rest/bits>> -> <<1:1, Rest/bits>>
                  end,
    EncodedBrokenTx = aec_base58c:encode(transaction, BrokenTxBin),
    EncodedSignedTx = aec_base58c:encode(transaction, SignedTxBin),
    {ok, 400, #{<<"reason">> := <<"Invalid tx">>}} = post_transactions_sut(EncodedBrokenTx),
    {ok, 200, _} = post_transactions_sut(EncodedSignedTx),
    ok.

post_broken_base58_tx(_Config) ->
    Amount = 1,
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, NumberOfChecks),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    lists:foreach(
        fun(_) ->
            {ok, SpendTx} =
                aec_spend_tx:new(
                  #{sender_id => aec_id:create(account, PubKey),
                    recipient_id => aec_id:create(account, random_hash()),
                    amount => Amount,
                    fee => Fee,
                    nonce => Nonce,
                    payload => <<"foo">>}),
            {ok, SignedTx} = rpc(aec_keys, sign_tx, [SpendTx]),
            <<_, BrokenHash/binary>> =
                aec_base58c:encode(transaction,
                                   aetx_sign:serialize_to_binary(SignedTx)),
            {ok, 400, #{<<"reason">> := <<"Invalid base58Check encoding">>}} = post_transactions_sut(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

%% positive test of spend_tx is handled in pending_transactions test
broken_spend_tx(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance_at_top(),
    ReceiverPubKey = random_hash(),
    {ok, 404, _} = post_spend_tx(aec_base58c:encode(account_pubkey, ReceiverPubKey), 42, 2),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

node_pubkey(_Config) ->
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_node_pubkey(),
    ct:log("MinerPubkey = ~p~nEncodedPubKey = ~p", [MinerPubKey,
                                                    EncodedPubKey]),
    {account_pubkey, MinerPubKey} = aec_base58c:decode(EncodedPubKey),
    ok.

peer_pub_key(_Config) ->
    {ok, PeerPubKey} = rpc(aec_keys, peer_pubkey, []),
    {ok, 200, #{<<"pubkey">> := EncodedPubKey}} = get_peer_pub_key(),
    ct:log("PeerPubkey = ~p~nEncodedPubKey = ~p", [PeerPubKey,
                                                    EncodedPubKey]),
    {ok, PeerPubKey} = aec_base58c:safe_decode(peer_pubkey, EncodedPubKey),
    ok.

naming_system_manage_name(_Config) ->
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    PubKeyEnc   = aec_base58c:encode(account_pubkey, PubKey),
    %% TODO: find out how to craete HTTP path with unicode chars
    %%Name        = <<"詹姆斯詹姆斯.test"/utf8>>,
    Name        = <<"without_unicode.test">>,
    NameSalt    = 12345,
    NameTTL     = 20000,
    Pointers    = [#{<<"key">> => <<"account_pubkey">>, <<"id">> => PubKeyEnc}],
    TTL         = 10,
    {ok, NHash} = aens:get_name_hash(Name),
    Fee         = 2,
    MineReward  = rpc(aec_governance, block_mine_reward, []),
    Node        = aecore_suite_utils:node_name(?NODE),

    %% Mine a block to get some funds
    aecore_suite_utils:mine_key_blocks(Node, 1),
    Height0 = 1,
    {ok, 200, #{<<"balance">> := Balance}} = get_balance_at_top(),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Get commitment hash to preclaim a name
    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = get_commitment_id(Name, NameSalt),
    {ok, CHash} = aec_base58c:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => Fee,
                     account_id    => PubKeyEnc},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = get_name_preclaim(PreclaimData),
    PreclaimTxHash = sign_and_post_tx(PreclaimTxEnc),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    {ok, BS1} = aecore_suite_utils:mine_blocks_until_tx_on_chain(Node, PreclaimTxHash, 10),
    Height1 = Height0 + length(BS1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check fee taken from account, then mine reward and fee added to account
    {ok, 200, #{<<"balance">> := Balance1}} = get_balance_at_top(),
    ?assertEqual(Balance1, Balance - Fee + (Height1 - Height0) * MineReward + Fee),

    %% Submit name claim tx and check it is in mempool
    ClaimData = #{account_id => PubKeyEnc,
                  name       => aec_base58c:encode(name, Name),
                  name_salt  => NameSalt,
                  fee        => Fee},
    {ok, 200, #{<<"tx">> := ClaimTxEnc}} = get_name_claim(ClaimData),
    ClaimTxHash = sign_and_post_tx(ClaimTxEnc),

    %% Mine a block and check mempool empty again
    {ok, BS2} = aecore_suite_utils:mine_blocks_until_tx_on_chain(Node, ClaimTxHash, 10),
    Height2 = Height1 + length(BS2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check tx fee taken from account, claim fee burned,
    %% then mine reward and fee added to account
    ClaimBurnedFee = rpc(aec_governance, name_claim_burned_fee, []),
    {ok, 200, #{<<"balance">> := Balance2}} = get_balance_at_top(),
    ?assertEqual(Balance2, Balance1 - Fee + (Height2 - Height1) * MineReward + Fee - ClaimBurnedFee),

    %% Check that name entry is present
    EncodedNHash = aec_base58c:encode(name, NHash),
    ExpectedTTL1 = (Height2 - 1) + aec_governance:name_claim_max_expiration(),
    {ok, 200, #{<<"id">>       := EncodedNHash,
                <<"ttl">>      := ExpectedTTL1,
                <<"pointers">> := []}} = get_names_entry_by_name_sut(Name),

    %% Submit name updated tx and check it is in mempool
    NameUpdateData = #{account_id => PubKeyEnc,
                       name_id    => aec_base58c:encode(name, NHash),
                       client_ttl => TTL,
                       pointers   => Pointers,
                       name_ttl   => NameTTL,
                       fee        => Fee},
    {ok, 200, #{<<"tx">> := UpdateEnc}} = get_name_update(NameUpdateData),
    UpdateTxHash = sign_and_post_tx(UpdateEnc),

    %% Mine a block and check mempool empty again
    {ok, BS3} = aecore_suite_utils:mine_blocks_until_tx_on_chain(Node, UpdateTxHash, 10),
    Height3 = Height2 + length(BS3),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check that TTL and pointers got updated in name entry
    ExpectedTTL2 = (Height3 - 1) + NameTTL,
    {ok, 200, #{<<"ttl">>      := ExpectedTTL2,
                <<"pointers">> := Pointers}} = get_names_entry_by_name_sut(Name),

    %% Check mine reward
    {ok, 200, #{<<"balance">> := Balance3}} = get_balance_at_top(),
    ?assertEqual(Balance3, Balance2 - Fee + (Height3 - Height2) * MineReward + Fee),

    {ok, 200, #{<<"tx">> := EncodedSpendTx}} =
        get_spend(#{recipient_id => EncodedNHash, amount => 77, fee => 50,
                    payload => <<"foo">>, sender_id => PubKeyEnc}),
    SpendTxHash = sign_and_post_tx(EncodedSpendTx),

    {ok, BS4} = aecore_suite_utils:mine_blocks_until_tx_on_chain(Node, SpendTxHash, 10),
    Height4 = Height3 + length(BS4),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Nothing gets lost as recipient = sender = miner
    %% This tests 'resolve_name' because recipient is expressed by name label
    %% This tests passes with 1 block confirmation due to lack of miner's reward delay
    {ok, 200, #{<<"balance">> := Balance4}} = get_balance_at_top(),
    ?assertEqual(Balance4, Balance3 + (Height4 - Height3) * MineReward),

    %% Submit name transfer tx and check it is in mempool
    TransferData = #{account_id   => PubKeyEnc,
                     recipient_id => PubKeyEnc,
                     name_id      => aec_base58c:encode(name, NHash),
                     fee          => Fee},
    {ok, 200, #{<<"tx">> := TransferEnc}} = get_name_transfer(TransferData),
    TransferTxHash = sign_and_post_tx(TransferEnc),

    %% Mine a block and check mempool empty again
    {ok, BS5} = aecore_suite_utils:mine_blocks_until_tx_on_chain(Node, TransferTxHash, 10),
    Height5 = Height4 + length(BS5),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check balance
    {ok, 200, #{<<"balance">> := Balance5}} = get_balance_at_top(),
    ?assertEqual(Balance5, Balance4 + (Height5 - Height4) * MineReward),

    %% Submit name revoke tx and check it is in mempool
    RevokeData = #{account_id => PubKeyEnc,
                   name_id => aec_base58c:encode(name, NHash),
                   fee => Fee},
    {ok, 200, #{<<"tx">> := RevokeEnc}} = get_name_revoke(RevokeData),
    RevokeTxHash = sign_and_post_tx(RevokeEnc),

    %% Mine a block and check mempool empty again
    {ok, BS6} = aecore_suite_utils:mine_blocks_until_tx_on_chain(Node, RevokeTxHash, 10),
    Height6 = Height5 + length(BS6),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check balance
    {ok, 200, #{<<"balance">> := Balance6}} = get_balance_at_top(),
    ?assertEqual(Balance6, Balance5 + (Height6 - Height5) * MineReward),

    %% Check the name got expired
    {ok, 404, #{<<"reason">> := <<"Name revoked">>}} = get_names_entry_by_name_sut(Name),
    ok.

naming_system_broken_txs(_Config) ->
    Name        = <<"fooo.test">>,
    NameSalt    = 12345,
    {ok, NHash} = aens:get_name_hash(Name),
    CHash       = aens_hash:commitment_hash(Name, NameSalt),
    Fee         = 2,

    % these tests require that no accounts are present
    ok = rpc(aec_conductor, reinit_chain, []),
    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Try to submit txs with empty account

    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_commitment_id(<<"abcd.badregistrar">>, 123),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_names_entry_by_name_sut(<<"abcd.badregistrar">>),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_preclaim(#{commitment_id => aec_base58c:encode(commitment, CHash),
                            fee => Fee,
                            account_id => aec_base58c:encode(account_pubkey, random_hash())}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_claim(#{name => aec_base58c:encode(name, Name),
                         name_salt => NameSalt,
                         account_id => aec_base58c:encode(account_pubkey, random_hash()),
                         fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_update(#{account_id => aec_base58c:encode(account_pubkey, random_hash()),
                          name_id => aec_base58c:encode(name, NHash),
                          name_ttl => 5,
                          pointers => [],
                          client_ttl => 5,
                          fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_transfer(#{account_id => aec_base58c:encode(account_pubkey, random_hash()),
                            recipient_id => aec_base58c:encode(account_pubkey, random_hash()),
                            name_id => aec_base58c:encode(name, NHash),
                            fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_revoke(#{account_id => aec_base58c:encode(account_pubkey, random_hash()),
                          name_id => aec_base58c:encode(name, NHash),
                          fee => Fee}),
    %% Check mempool still empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

%% ============================================================
%% Websocket tests
%% ============================================================

ws_get_genesis(_Config) ->
    {ok, ConnPid} = ws_start_link(),
    {_Tag, #{ <<"block">> := Block }} =
        ws_chain_get(ConnPid, #{height => 0, type => block}),
    {ok, 200, BlockMap} = get_key_blocks_by_height_sut(0),
    ?assertEqual(BlockMap, Block),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

ws_request_tag(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Test with tag
    {<<"supersecret_tag">>, #{<<"block">> := _Block}} =
        ws_chain_get(ConnPid, supersecret_tag, #{height => 0, type => block}),

    %% And test without tag
    {<<"untagged">>, #{<<"block">> := _Block}} =
        ws_chain_get(ConnPid, #{height => 0, type => block}),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.


ws_block_mined(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Register for mined_block events
    ws_subscribe(ConnPid, #{ type => mined_block }),

    {Height, Hash} = ws_mine_key_block(ConnPid, ?NODE, 1),

    {_Tag, #{<<"block">> := Block}} = ws_chain_get(ConnPid, #{height => Height, type => block}),
    {_Tag, #{<<"block">> := Block}} = ws_chain_get(ConnPid, #{hash => Hash, type => block}),
    {_Tag, #{<<"header">> := Header}} = ws_chain_get(ConnPid, #{height => Height, type => header}),
    {_Tag, #{<<"header">> := Header}} = ws_chain_get(ConnPid, #{hash => Hash, type => header}),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

ws_micro_block_added(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Mine 1 key block to get a reward.
    ws_subscribe(ConnPid, #{ type => mined_block }),
    {_Height0, _KeyBlockHash0} = ws_mine_key_block(ConnPid, ?NODE, 1),

    %% 1 tx in the mempool, so micro block will be generated.
    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aec_base58c:encode(account_pubkey, random_hash()), 1, 1),
    sign_and_post_tx(SpendTx),

    %% Register for added_micro_block events
    ws_subscribe(ConnPid, #{ type => added_micro_block }),
    {Height, KeyBlockHash, MicroBlockHash} = ws_mine_key_and_micro_block(ConnPid, ?NODE),

    {_Tag, #{<<"block">> := #{<<"height">> := Height, <<"prev_hash">> := PrevHash}}} =
        ws_chain_get(ConnPid, #{hash => KeyBlockHash, type => block}),
    {_Tag, #{<<"header">> := #{<<"height">> := Height, <<"prev_hash">> := PrevHash}}} =
        ws_chain_get(ConnPid, #{hash => KeyBlockHash, type => header}),
    {_Tag, #{<<"block">> := #{<<"height">> := Height, <<"prev_hash">> := PrevHash1, <<"txs_hash">> := TxsHash}}} =
        ws_chain_get(ConnPid, #{hash => MicroBlockHash, type => block}),
    {_Tag, #{<<"header">> := #{<<"height">> := Height, <<"prev_hash">> := PrevHash1, <<"txs_hash">> := TxsHash}}} =
        ws_chain_get(ConnPid, #{hash => MicroBlockHash, type => header}),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.
%% Currently the websockets are a queue: they have a maximim amount of
%% acceptors. Every WS trying to connect after all acceptoprs are used
%% goes into a queue. When the queue is full - the node starts rejecting
%% any new incoming WS connections
ws_refused_on_limit_reached(_Config) ->
    %% maximum amount of acceptors
    MaxWsCount = rpc(aeu_env, user_config_or_env,
                          [[<<"websocket">>, <<"internal">>, <<"acceptors">>],
                          aehttp, [internal, websocket, handlers], 10]),
    %% Maximum WS connections hanging in the queue
    WSQueueSize = rpc(aehttp_app, ws_handlers_queue_max_size, []),
    WSDieTimeout = 1000,
    ct:log("Websocket acceptors: ~p, websocket acceptor's queue size ~p",
           [MaxWsCount, WSQueueSize]),
    %% assert no WS running on the node
    0 = open_websockets_count(),
    %% start as many WS as needed to consume all acceptors
    WSPids =
        lists:map(
            fun(_) ->
              {ok, ConnPid} = ws_start_link(),
              ConnPid
            end,
            lists:seq(1, MaxWsCount)),
    %% assert expectation for amount of connected WSs
    MaxWsCount = open_websockets_count(),
    WaitingPids =
        lists:map(
            fun(_) ->
                %% try to connect a WS client; assert it does not connect and
                %% is waiting
                {error, {still_connecting, WaitingPid}} = ws_start_link(),
                MaxWsCount = open_websockets_count(),
                WaitingPid
            end,
            lists:seq(1, WSQueueSize)),

    %% Now both the acceptopr pool and queue are full. Try to connect a new WS
    %% client and assert it fails
    {error, rejected} = ws_start_link(),

    %% split currently connected WS clients into two groups: first with as
    %% many as there are WS clients waiting in the queue and all the rest in a
    %% seperate list. The first group would be used for stopping WS clients
    %% one by one while validating that one of the waiting WS clients connects
    %% for each one stopped
    {FirstWSsPids, OtherWSsPids} = lists:split(WSQueueSize, WSPids),
    lists:foreach(
        fun(Pid) ->
            %% stop one
            ?WS:stop(Pid),
            %% another one connects in its place and it doesn't matter which one
            {ok, _SomePid} = ?WS:wait_for_connect_any(),
            %% total amount of connected WS clients is still the maximum
            MaxWsCount = open_websockets_count()
        end,
        FirstWSsPids),
    %% cleanup
    lists:foreach(fun ?WS:stop/1, OtherWSsPids),
    timer:sleep(100), % wait for all of them to die out
    {ok, WSQueueSize} =
        aec_test_utils:wait_for_it_or_timeout(fun open_websockets_count/0,
                                              WSQueueSize, WSDieTimeout),
    lists:foreach(fun ?WS:stop/1, WaitingPids),
    timer:sleep(100), % wait for all of them to die out
    {ok, 0} =
        aec_test_utils:wait_for_it_or_timeout(fun open_websockets_count/0,
                                              0, WSDieTimeout),
    ok.

ws_tx_on_chain(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Register for events when a block is mined!
    ws_subscribe(ConnPid, #{ type => mined_block }),

    %% Mine a block to make sure the Pubkey has some funds!
    ws_mine_key_block(ConnPid, ?NODE, 1),

    %% Fetch the pubkey via HTTP
    {ok, 200, #{ <<"pub_key">> := PK }} = get_node_pubkey(),

    %% Post spend tx
    {ok, 200, #{<<"tx">> := Tx}} =
        post_spend_tx(aec_base58c:encode(account_pubkey, random_hash()), 3, 1),
    TxHash = sign_and_post_tx(Tx),

    %% Subscribe for an event once the Tx goes onto the chain...
    ws_subscribe(ConnPid, #{ type => tx, tx_hash => TxHash }),
    ok = ?WS:register_test_for_event(ConnPid, chain, tx_chain),

    %% Mine a block and check that an event is receieved corresponding to
    %% the Tx.
    ws_mine_key_block(ConnPid, ?NODE, 2),
    {ok, #{<<"tx_hash">> := TxHash }} = ?WS:wait_for_event(ConnPid, chain, tx_chain),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

%%
%% Channels
%%
assert_balance(Pubkey, ExpectedBalance) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := ExpectedBalance}} =
        get_accounts_by_pubkey_sut(Address).

channel_sign_tx(ConnPid, Privkey, Tag) ->
    {ok, Tag, #{<<"tx">> := EncCreateTx}} = ?WS:wait_for_channel_event(ConnPid, sign),
    {ok, CreateBinTx} = aec_base58c:safe_decode(transaction, EncCreateTx),
    Tx = aetx:deserialize_from_binary(CreateBinTx),
    SignedCreateTx = aec_test_utils:sign_tx(Tx, Privkey),
    EncSignedCreateTx = aec_base58c:encode(transaction,
                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
    ?WS:send(ConnPid, Tag, #{tx => EncSignedCreateTx}),
    Tx.

sc_ws_open(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    {ok, 200, #{<<"balance">> := IStartAmt}} =
                 get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := RStartAmt}} =
                 get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, RPubkey)),
    IAmt = 700,
    RAmt = 400,

    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt),
    {ok, IConnPid} = channel_ws_start(initiator,
                                           maps:put(host, <<"localhost">>, ChannelOpts)),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info, sign, on_chain_tx]),

    {ok, RConnPid} = channel_ws_start(responder, ChannelOpts),

    ok = ?WS:register_test_for_channel_events(RConnPid, [info, sign, on_chain_tx]),

    channel_send_conn_open_infos(RConnPid, IConnPid),

    ChannelCreateFee = channel_create(Config, IConnPid, RConnPid),

    %% ensure new balances
    assert_balance(IPubkey, IStartAmt - IAmt - ChannelCreateFee),
    assert_balance(RPubkey, RStartAmt - RAmt),

    % mine min depth
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 4),

    channel_send_locking_infos(IConnPid, RConnPid),

    channel_send_chan_open_infos(RConnPid, IConnPid),

    ChannelClients = #{initiator => IConnPid,
                       responder => RConnPid},
    {save_config, [{channel_clients, ChannelClients},
                   {channel_options, ChannelOpts} | Config]}.


channel_send_conn_open_infos(RConnPid, IConnPid) ->
    {ok, #{<<"event">> := <<"channel_open">>}} = ?WS:wait_for_channel_event(RConnPid, info),
    {ok, #{<<"event">> := <<"channel_accept">>}} = ?WS:wait_for_channel_event(IConnPid, info).

channel_send_locking_infos(IConnPid, RConnPid) ->
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = ?WS:wait_for_channel_event(RConnPid, info),

    {ok, #{<<"event">> := <<"funding_locked">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"funding_locked">>}} = ?WS:wait_for_channel_event(RConnPid, info).

channel_send_chan_open_infos(RConnPid, IConnPid) ->
    {ok, #{<<"event">> := <<"open">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"open">>}} = ?WS:wait_for_channel_event(RConnPid, info).

channel_participants_balances(IPubkey, RPubkey) ->
    {ok, 200, #{<<"balance">> := BalI}} =
        get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := BalR}} =
        get_accounts_by_pubkey_sut(aec_base58c:encode(account_pubkey, RPubkey)),
    {BalI, BalR}.

channel_create(Config, IConnPid, RConnPid) ->
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants, Config),
    %% initiator gets to sign a create_tx
    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    CrTx = channel_sign_tx(IConnPid, IPrivkey, <<"initiator_sign">>),
    {ok, #{<<"event">> := <<"funding_created">>}} = ?WS:wait_for_channel_event(RConnPid, info),
    %% responder gets to sign a create_tx
    CrTx = channel_sign_tx(RConnPid, RPrivkey, <<"responder_sign">>),
    {ok, #{<<"event">> := <<"funding_signed">>}} = ?WS:wait_for_channel_event(IConnPid, info),

    %% both of them receive the same co-signed channel_create_tx
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = ?WS:wait_for_channel_event(IConnPid, on_chain_tx),
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = ?WS:wait_for_channel_event(RConnPid, on_chain_tx),

    {ok, SSignedCrTx} = aec_base58c:safe_decode(transaction, EncodedSignedCrTx),
    SignedCrTx = aetx_sign:deserialize_from_binary(SSignedCrTx),
    %% same transaction
    CrTx = aetx_sign:tx(SignedCrTx),

    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    IPubkey = aesc_create_tx:initiator_pubkey(Tx),
    RPubkey = aesc_create_tx:responder_pubkey(Tx),
    ChannelCreateFee = aesc_create_tx:fee(Tx),

    %% ensure the tx is in the mempool
    ok = wait_for_signed_transaction_in_pool(SignedCrTx),

    %% balances hadn't changed yet
    assert_balance(IPubkey, IStartAmt),
    assert_balance(RPubkey, RStartAmt),

    % mine the create_tx
    ok = wait_for_signed_transaction_in_block(SignedCrTx),

    ChannelCreateFee.

sc_ws_update(Config) ->
    {ok, ConfigList} = get_saved_config(
                         Config, [sc_ws_open, sc_ws_reestablish]),
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, ConfigList),
    lists:foldl(
        fun(Sender, Round) ->
            channel_update(Conns, Sender, Participants, 1, Round),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder,
         responder,
         initiator,
         responder]),
    {save_config, ConfigList}.

sc_ws_update_fails_and_close(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid} = Conns =
        proplists:get_value(channel_clients, ConfigList),
    lists:foreach(
        fun(Sender) ->
            {ok, #{<<"reason">> := <<"insufficient_balance">>,
                  <<"request">> := _Request0}} = channel_update_fail(Conns, Sender,
                                                                    Participants, 10000),
            {ok, #{<<"reason">> := <<"negative_amount">>,
                  <<"request">> := _Request1}} = channel_update_fail(Conns, Sender,
                                                                     Participants,
                                                                     -1),
            {ok, #{<<"reason">> := <<"invalid_pubkeys">>,
                  <<"request">> := _Request2}} =
                channel_update_fail(Conns, Sender,
                                    #{initiator => #{pub_key => <<42:32/unit:8>>},
                                      responder => #{pub_key => <<43:32/unit:8>>}},
                                    1),
            ok
        end,
        [initiator, responder]),
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_send_messages_and_close(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    lists:foreach(
        fun({Sender, Msg}) ->
            {SenderPubkey, ReceiverPubkey, SenderPid, ReceiverPid} =
                case Sender of
                    initiator ->
                        {IPubkey, RPubkey, IConnPid, RConnPid};
                    responder ->
                        {RPubkey, IPubkey, RConnPid, IConnPid}
                end,
            SenderEncodedK = aec_base58c:encode(account_pubkey, SenderPubkey),
            ReceiverEncodedK = aec_base58c:encode(account_pubkey, ReceiverPubkey),
            ok = ?WS:register_test_for_channel_event(ReceiverPid, message),

            ?WS:send(SenderPid, <<"message">>,
                    #{<<"to">> => ReceiverEncodedK,
                      <<"info">> => Msg}),

            {ok, #{<<"message">> := #{<<"from">> := SenderEncodedK,
                                      <<"to">> := ReceiverEncodedK,
                                      <<"info">> := Msg}}}
                = ?WS:wait_for_channel_event(ReceiverPid, message),
            ok = ?WS:unregister_test_for_channel_event(ReceiverPid, message)
        end,
        [ {initiator, <<"hejsan">>}                   %% initiator can send
        , {responder, <<"svejsan">>}                  %% responder can send
        , {initiator, <<"first message in a row">>}   %% initiator can send two messages in a row
        , {initiator, <<"second message in a row">>}
        , {responder, <<"some message">>}             %% responder can send two messages in a row
        , {responder, <<"other message">>}
        ]),

    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_conflict_and_close(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid} = Conns =
        proplists:get_value(channel_clients, ConfigList),

    lists:foreach(
        fun(FirstSender) ->
            channel_conflict(Conns, FirstSender, Participants, 1, 2)
        end,
        [initiator,
         responder]),

    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

channel_conflict(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount1, Amount2) ->
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey, RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey, IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, conflict]),

    SignUpdate =
        fun TrySignUpdate(ConnPid, Privkey) ->
            case ?WS:wait_for_channel_event(ConnPid, sign) of
                {ok, <<"update_ack">>, _} -> %% this is not the message we are looking for
                    TrySignUpdate(ConnPid, Privkey);
                {ok, <<"update">>, #{<<"tx">> := EncCreateTx}} ->
                    {ok, CreateBinTx} = aec_base58c:safe_decode(transaction, EncCreateTx),
                    Tx = aetx:deserialize_from_binary(CreateBinTx),
                    SignedCreateTx = aec_test_utils:sign_tx(Tx, Privkey),
                    EncSignedCreateTx = aec_base58c:encode(transaction,
                                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
                    ?WS:send(ConnPid, <<"update">>, #{tx => EncSignedCreateTx})
            end
        end,
    %% sender initiates an update
    ?WS:send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount1}),

    %% starter signs the new state

    %% acknowledger initiates an update too
    ?WS:send_tagged(AcknowledgerPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount2}),

    SignUpdate(StarterPid, StarterPrivkey),
    SignUpdate(AcknowledgerPid, AcknowledgerPrivkey),

    {ok, _} = ?WS:wait_for_channel_event(StarterPid, conflict),
    {ok, _} = ?WS:wait_for_channel_event(AcknowledgerPid, conflict),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, conflict]),

    ok.

channel_update(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount,_Round) ->
    true = undefined =/= process_info(IConnPid),
    true = undefined =/= process_info(RConnPid),
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey,
                                    RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey,
                                    IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, update, get]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, update]),

    %% sender initiates an update
    GetBothBalances = fun(ConnPid) ->
                              sc_ws_get_both_balances(
                                ConnPid, StarterPubkey, AcknowledgerPubkey)
                      end,
    {ok, {Ba0, Bb0} = Bal0} = GetBothBalances(IConnPid),
    ct:log("Balances before: ~p", [Bal0]),
    ?WS:send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount}),

    %% starter signs the new state
    UnsignedStateTx = channel_sign_tx(StarterPid, StarterPrivkey, <<"update">>),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    %% verify contents
    {channel_offchain_tx, OffchainTx} = aetx:specialize_type(UnsignedStateTx),
    [Update] = aesc_offchain_tx:updates(OffchainTx),
    Expected = aesc_offchain_update:op_transfer(aec_id:create(account, StarterPubkey),
                                                aec_id:create(account, AcknowledgerPubkey), Amount),
    Expected = Update,


    %% acknowledger signs the new state
    {ok, #{<<"event">> := <<"update">>}} = ?WS:wait_for_channel_event(AcknowledgerPid, info),
    UnsignedStateTx = channel_sign_tx(AcknowledgerPid, AcknowledgerPrivkey, <<"update_ack">>),

    {ok, #{<<"state">> := NewState}} = ?WS:wait_for_channel_event(IConnPid,
                                                                 update),
    {ok, #{<<"state">> := NewState}} = ?WS:wait_for_channel_event(RConnPid,
                                                                 update),
    {ok, SignedStateTxBin} = aec_base58c:safe_decode(transaction, NewState),
    SignedStateTx = aetx_sign:deserialize_from_binary(SignedStateTxBin),

    %% validate it is co-signed
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ok = rpc(aetx_sign, verify, [SignedStateTx, Trees]), % RPC because of DB
    {UnsignedStateTx, _} = % same transaction that was signed
        {aetx_sign:tx(SignedStateTx), UnsignedStateTx},

    {ok, {Ba1, Bb1} = Bal1} = GetBothBalances(IConnPid),
    ct:log("Balances after: ~p", [Bal1]),
    Ba1 = Ba0 - Amount,
    Bb1 = Bb0 + Amount,

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, info, update, get]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, info, update]),

    ok.


channel_update_fail(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey},
                 responder := #{pub_key := RPubkey}},
               Amount) ->
    {StarterPid, _AcknowledgerPid, StarterPubkey, AcknowledgerPubkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, RPubkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, IPubkey}
        end,
    ok = ?WS:register_test_for_channel_event(StarterPid, error),

    %% sender initiates an update
    ?WS:send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount}),

    {ok, _Payload}= Res = ?WS:wait_for_channel_event(StarterPid, error),


    ok = ?WS:unregister_test_for_channel_event(StarterPid, error),
    Res.

sc_ws_close(Config) ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),

    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),


    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_get_both_balances(ConnPid, PubKeyI, PubKeyR) ->
    AccountI = aec_base58c:encode(account_pubkey, PubKeyI),
    AccountR = aec_base58c:encode(account_pubkey, PubKeyR),
    ?WS:send_tagged(ConnPid, <<"get">>, <<"balances">>,
                    #{<<"accounts">> => [AccountI, AccountR]}),
    {ok, <<"balances">>, Res} = ?WS:wait_for_channel_event(ConnPid, get),
    [#{<<"account">> := AccountI, <<"balance">> := BI},
     #{<<"account">> := AccountR, <<"balance">> := BR}] = Res,
    {ok, {BI, BR}}.

sc_ws_close_mutual_initiator(Config) ->
    sc_ws_close_mutual(Config, initiator).

sc_ws_close_mutual_responder(Config) ->
    sc_ws_close_mutual(Config, responder).

sc_ws_close_mutual(Config, Closer) when Closer =:= initiator
                                 orelse Closer =:= responder ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    ct:log("ConfigList = ~p", [ConfigList]),
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants,
                                                                 ConfigList),
    {IStartB, RStartB} = channel_participants_balances(IPubkey, RPubkey),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, on_chain_tx]),


    CloseMutual =
        fun(CloserConn, CloserPrivkey, OtherConn, OtherPrivkey) ->
            ?WS:send(CloserConn, <<"shutdown">>, #{}),

            ShTx = channel_sign_tx(CloserConn, CloserPrivkey, <<"shutdown_sign">>),
            ShTx = channel_sign_tx(OtherConn, OtherPrivkey, <<"shutdown_sign_ack">>)
        end,
    ShutdownTx =
        case Closer of
            initiator -> CloseMutual(IConnPid, IPrivkey, RConnPid, RPrivkey);
            responder -> CloseMutual(RConnPid, RPrivkey, IConnPid, IPrivkey)
        end,

    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = ?WS:wait_for_channel_event(IConnPid, on_chain_tx),
    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = ?WS:wait_for_channel_event(RConnPid, on_chain_tx),

    {ok, SSignedMutualTx} = aec_base58c:safe_decode(transaction, EncodedSignedMutualTx),
    SignedMutualTx = aetx_sign:deserialize_from_binary(SSignedMutualTx),
    %% same transaction
    ShutdownTx = aetx_sign:tx(SignedMutualTx),

    {channel_close_mutual_tx, MutualTx} = aetx:specialize_type(ShutdownTx),

    ok = wait_for_signed_transaction_in_pool(SignedMutualTx),

    assert_balance(IPubkey, IStartB),
    assert_balance(RPubkey, RStartB),

    ok = wait_for_signed_transaction_in_block(SignedMutualTx),

    IChange = aesc_close_mutual_tx:initiator_amount_final(MutualTx),
    RChange = aesc_close_mutual_tx:responder_amount_final(MutualTx),

    assert_balance(IPubkey, IStartB + IChange),
    assert_balance(RPubkey, RStartB + RChange),

    % ensure tx is not hanging in mempool
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),
    ok.

sc_ws_leave(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    #{initiator := #{pub_key  := IPubkey,
                     priv_key := IPrivkey},
      responder := #{pub_key  := RPubkey,
                     priv_key := RPrivkey}} = proplists:get_value(participants,
                                                                  ConfigList),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),
    ok = ?WS:register_test_for_channel_events(IConnPid, [leave, info]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [leave, info]),
    ok = ?WS:register_test_for_events(IConnPid, websocket, [closed]),
    ok = ?WS:register_test_for_events(RConnPid, websocket, [closed]),
    %%
    ok = ?WS:send(IConnPid, <<"leave">>, #{}),
    %%
    {ok, #{ <<"channel_id">> := IDi,
            <<"payload">> := Pi }} =
        ?WS:wait_for_channel_msg(IConnPid, leave),
    #{ <<"state">> := StI } = Pi,
    {ok, #{<<"channel_id">> := IDr,
           <<"payload">> := Pr }} =
        ?WS:wait_for_channel_msg(RConnPid, leave),
    #{ <<"state">> := StR } = Pr,
    {IDi, IDr} = {IDr, IDi},
    {StI, StR} = {StR, StI},
    {ok, #{<<"event">> := <<"died">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"died">>}} = ?WS:wait_for_channel_event(RConnPid, info),
    ok = ?WS:wait_for_event(IConnPid, websocket, closed),
    ok = ?WS:wait_for_event(RConnPid, websocket, closed),
    %%
    Options = proplists:get_value(channel_options, ConfigList),
    Port = maps:get(port, Options),
    RPort = Port+1,
    ReestablOptions = maps:merge(Options, #{existing_channel_id => IDi,
                                            offchain_tx => StI,
                                            port => RPort}),
    {save_config, [{channel_reestabl_options, ReestablOptions} | Config]}.


sc_ws_reestablish(Config) ->
    {sc_ws_leave, ConfigList} = ?config(saved_config, Config),
    ReestablOptions = proplists:get_value(channel_reestabl_options, ConfigList),
    {ok, RrConnPid} = channel_ws_start(responder, ReestablOptions),
    {ok, IrConnPid} = channel_ws_start(initiator, maps:put(
                                                    host, <<"localhost">>,
                                                    ReestablOptions)),
    ok = ?WS:register_test_for_channel_events(
            RrConnPid, [info, update]),
    ok = ?WS:register_test_for_channel_events(
            IrConnPid, [info, update]),
    {ok, #{<<"event">> := <<"channel_reestablished">>}} =
        ?WS:wait_for_channel_event(IrConnPid, info),
    {ok, #{<<"event">> := <<"channel_reestablished">>}} =
        ?WS:wait_for_channel_event(RrConnPid, info),
    {ok, #{<<"event">> := <<"open">>}} =
        ?WS:wait_for_channel_event(IrConnPid, info),
    {ok, #{<<"event">> := <<"open">>}} =
        ?WS:wait_for_channel_event(RrConnPid, info),
    ChannelClients = #{initiator => IrConnPid,
                       responder => RrConnPid},
    {save_config, [{channel_clients, ChannelClients},
                   {channel_options, ReestablOptions} | Config]}.


sc_ws_deposit_initiator_and_close(Config) ->
    sc_ws_deposit_and_close(Config, initiator).

sc_ws_deposit_responder_and_close(Config) ->
    sc_ws_deposit_and_close(Config, responder).


sc_ws_deposit_and_close(Config, Origin) when Origin =:= initiator
                            orelse Origin =:= responder ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    Participants= proplists:get_value(participants, ConfigList),
    Clients = proplists:get_value(channel_clients, ConfigList),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    {SStartB, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx]),
    ?WS:send(SenderConnPid, <<"deposit">>, #{amount => 2}),
    UnsignedStateTx = channel_sign_tx(SenderConnPid, SenderPrivkey, <<"deposit_tx">>),
    {ok, #{<<"event">> := <<"deposit_created">>}} = ?WS:wait_for_channel_event(AckConnPid, info),
    UnsignedStateTx = channel_sign_tx(AckConnPid, AckPrivkey, <<"deposit_ack">>),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, #{<<"tx">> := EncodedSignedDepositTx}} = ?WS:wait_for_channel_event(SenderConnPid, on_chain_tx),
    {ok, #{<<"tx">> := EncodedSignedDepositTx}} = ?WS:wait_for_channel_event(AckConnPid, on_chain_tx),

    {ok, SSignedDepositTx} = aec_base58c:safe_decode(transaction,
                                                     EncodedSignedDepositTx),
    SignedDepositTx = aetx_sign:deserialize_from_binary(SSignedDepositTx),
    ok = wait_for_signed_transaction_in_block(SignedDepositTx),
    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    {SStartB1, _} = {SStartB - 2 - 1, SStartB},

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 5),
    {ok, #{<<"event">> := <<"own_deposit_locked">>}} = ?WS:wait_for_channel_event(SenderConnPid, info),
    {ok, #{<<"event">> := <<"own_deposit_locked">>}} = ?WS:wait_for_channel_event(AckConnPid, info),

    {ok, #{<<"event">> := <<"deposit_locked">>}} = ?WS:wait_for_channel_event(SenderConnPid, info),
    {ok, #{<<"event">> := <<"deposit_locked">>}} = ?WS:wait_for_channel_event(AckConnPid, info),

    ok = ?WS:stop(SenderConnPid),
    ok = ?WS:stop(AckConnPid),
    ok.

sc_ws_withdraw_initiator_and_close(Config) ->
    sc_ws_withdraw_and_close(Config, initiator).

sc_ws_withdraw_responder_and_close(Config) ->
    sc_ws_withdraw_and_close(Config, responder).


sc_ws_withdraw_and_close(Config, Origin) when Origin =:= initiator
                            orelse Origin =:= responder ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, ConfigList),
    Clients = proplists:get_value(channel_clients, ConfigList),
    {SenderRole, AckRole} =
        case Origin of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    {SStartB, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, on_chain_tx]),
    ?WS:send(SenderConnPid, <<"withdraw">>, #{amount => 2}),
    UnsignedStateTx = channel_sign_tx(SenderConnPid, SenderPrivkey, <<"withdraw_tx">>),
    {ok, #{<<"event">> := <<"withdraw_created">>}} = ?WS:wait_for_channel_event(AckConnPid, info),
    UnsignedStateTx = channel_sign_tx(AckConnPid, AckPrivkey, <<"withdraw_ack">>),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, #{<<"tx">> := EncodedSignedWTx}} = ?WS:wait_for_channel_event(SenderConnPid, on_chain_tx),
    {ok, #{<<"tx">> := EncodedSignedWTx}} = ?WS:wait_for_channel_event(AckConnPid, on_chain_tx),

    {ok, SSignedWTx} = aec_base58c:safe_decode(transaction, EncodedSignedWTx),
    SignedWTx = aetx_sign:deserialize_from_binary(SSignedWTx),
    ok = wait_for_signed_transaction_in_block(SignedWTx),
    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    {SStartB1, _} = {SStartB + 2 - 1, SStartB},

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 5),
    {ok, #{<<"event">> := <<"own_withdraw_locked">>}} = ?WS:wait_for_channel_event(SenderConnPid, info),
    {ok, #{<<"event">> := <<"own_withdraw_locked">>}} = ?WS:wait_for_channel_event(AckConnPid, info),

    {ok, #{<<"event">> := <<"withdraw_locked">>}} = ?WS:wait_for_channel_event(SenderConnPid, info),
    {ok, #{<<"event">> := <<"withdraw_locked">>}} = ?WS:wait_for_channel_event(AckConnPid, info),


    ok = ?WS:stop(SenderConnPid),
    ok = ?WS:stop(AckConnPid),
    ok.

sc_ws_contracts(Config) ->
    lists:foreach(
        fun({Owner, TestName}) ->
            sc_ws_contract_(Config, TestName, Owner)
        end,
        [{Owner, Test} || Owner <- [initiator, responder],
                          Test  <- ["identity",
                                    "counter",
                                    "spend_test"]]),
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := IConnPid,
      responder := RConnPid } = proplists:get_value(channel_clients, ConfigList),
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_contract_(Config, TestName, Owner) ->
    ct:log("Start processing contract ~p, initated by ~p", [TestName, Owner]),
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, ConfigList),
    Clients = proplists:get_value(channel_clients, ConfigList),
    {SenderRole, AckRole} =
        case Owner of
            initiator -> {initiator, responder};
            responder -> {responder, initiator}
        end,
    #{pub_key := SenderPubkey,
      priv_key:= SenderPrivkey} = maps:get(SenderRole, Participants),
    #{pub_key := AckPubkey,
      priv_key:= AckPrivkey} = maps:get(AckRole, Participants),
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),

    ok = ?WS:register_test_for_channel_events(SenderConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(AckConnPid, [sign, info, get, error]),

    %% helper lambda for update
    UpdateVolley0 =
        fun(FirstConnPid, FirstPrivkey, SecondConnPid, SecondPrivkey) ->
            UnsignedStateTx = channel_sign_tx(FirstConnPid, FirstPrivkey, <<"update">>),

            % acknowledger signs update_ack
            {ok, #{<<"event">> := <<"update">>}} = ?WS:wait_for_channel_event(SecondConnPid, info),
            UnsignedStateTx = channel_sign_tx(SecondConnPid, SecondPrivkey, <<"update_ack">>)
        end,
    UpdateVolley =
        fun() ->
            UpdateVolley0(SenderConnPid, SenderPrivkey, AckConnPid, AckPrivkey)
        end,
    UpdateVolleyReverse =
        fun() ->
            UpdateVolley0(AckConnPid, AckPrivkey, SenderConnPid, SenderPrivkey)
        end,

    % trigger new contract
    {UnsignedStateTx, Code} = create_contract_(TestName, SenderConnPid, UpdateVolley),

    ContractPubKey = contract_id_from_create_update(SenderPubkey,
                                                    UnsignedStateTx),

    %% helper lambda for decoded result
    GetCallParams =
        fun(UnsignedStateTx00) ->
            {CB1, Tx1} = aetx:specialize_callback(UnsignedStateTx00),
            CallRound = CB1:round(Tx1),
            [U] = CB1:updates(Tx1),
            CallerPubKey = aesc_offchain_update:extract_caller(U),
            CallerId = aec_base58c:encode(account_pubkey, CallerPubKey),
            ContractId = aec_base58c:encode(contract_pubkey, ContractPubKey),
            #{contract   => ContractId,
              caller     => CallerId,
              round      => CallRound}
        end,
    GetDecodedResult =
        fun(Type, UnsignedStateTx00) ->
            GetCallResult =
                fun(ConnPid) ->
                    ?WS:send_tagged(ConnPid, <<"get">>, <<"contract_call">>,
                                    GetCallParams(UnsignedStateTx00)),
                    {ok, <<"contract_call">>, Res} = ?WS:wait_for_channel_event(ConnPid, get),
                    Res
                end,
            CallRes = GetCallResult(SenderConnPid),
            CallRes = GetCallResult(AckConnPid),
            #{<<"caller_id">>         := CallerId,
              <<"caller_nonce">>      := CallRound,
              <<"contract_id">>       := ContractId,
              <<"gas_price">>         := _,
              <<"gas_used">>          := _,
              <<"height">>            := CallRound,
              <<"return_type">>       := <<"ok">>,
              <<"return_value">>      := ReturnValue} = CallRes,
            {ok, 200, #{<<"data">> := Data}} =
                get_contract_decode_data(#{'sophia-type' => Type, data => ReturnValue}),
            _R = contract_result_parse(TestName, Data)
      end,

    %% helper lambdas for pruning and call not found
    PruneCalls =
        fun(ConnPid) ->
            ok = ?WS:register_test_for_channel_events(ConnPid, [calls_pruned]),
            ?WS:send(ConnPid, <<"clean_contract_calls">>, #{}),
            {ok, _} = ?WS:wait_for_channel_event(ConnPid, calls_pruned),
            ok = ?WS:unregister_test_for_channel_events(ConnPid, [calls_pruned])
        end,
    CallMissingCall =
        fun(UnsignedStateTx00, ConnPid) ->
            ?WS:send_tagged(ConnPid, <<"get">>, <<"contract_call">>,
                            GetCallParams(UnsignedStateTx00)),
            {ok, #{<<"reason">> := <<"call_not_found">>}} = ?WS:wait_for_channel_event(ConnPid, error),
            ok
        end,

    % trigger call contract
    % owner can call a contract
    SomeUnsignedStateTx = contract_calls_(TestName, ContractPubKey, Code, SenderConnPid, UpdateVolley,
                    GetDecodedResult, AckConnPid, SenderPubkey, AckPubkey),

    _ = GetDecodedResult(contract_return_type(TestName), SomeUnsignedStateTx),
    ok = PruneCalls(SenderConnPid),
    ok = CallMissingCall(SomeUnsignedStateTx, SenderConnPid),
    % state is still usable

    % acknowledger can call a contract
    contract_calls_(TestName, ContractPubKey, Code, AckConnPid, UpdateVolleyReverse,
                    GetDecodedResult, SenderConnPid, AckPubkey, SenderPubkey),

    GetPoI =
        fun(ConnPid) ->
            ?WS:send_tagged(ConnPid, <<"get">>, <<"poi">>,
                            #{contracts   => [aec_base58c:encode(contract_pubkey, ContractPubKey)],
                              accounts    => [aec_base58c:encode(account_pubkey, SenderPubkey),
                                              aec_base58c:encode(account_pubkey, AckPubkey)]
                            }),

                    {ok, <<"poi">>, #{<<"poi">> := P}} = ?WS:wait_for_channel_event(ConnPid, get),
                    P
                end,

    GetMissingPoI =
        fun(ConnPid, Accs, Cts) ->
            ?WS:send_tagged(ConnPid, <<"get">>, <<"poi">>,
                            #{contracts   => [aec_base58c:encode(contract_pubkey, C) || C <- Cts],
                              accounts    => [aec_base58c:encode(account_pubkey, Acc) || Acc <- Accs]
                            }),

                    {ok, #{<<"reason">> := R}} = ?WS:wait_for_channel_event(ConnPid, error),
                    R
                end,

    EncodedPoI = GetPoI(SenderConnPid),
    EncodedPoI = GetPoI(AckConnPid),

    NegativePoiTests =
        fun(ConnPid) ->
            <<"broken_encoding: accounts">> = GetMissingPoI(ConnPid, [<<123456789>>], []),
            <<"broken_encoding: contracts">> = GetMissingPoI(ConnPid, [], [<<123456789>>]),
            <<"broken_encoding: accounts, contracts">> = GetMissingPoI(ConnPid, [<<123456789>>], [<<123456789>>]),
            AccountByteSize = aec_base58c:byte_size_for_type(account_pubkey),
            FakeAccountId = <<42:AccountByteSize/unit:8>>,
            <<"not_found">> = GetMissingPoI(ConnPid, [FakeAccountId], []),
            ContractByteSize = aec_base58c:byte_size_for_type(contract_pubkey),
            FakeContractId = <<42:ContractByteSize/unit:8>>,
            <<"not_found">> = GetMissingPoI(ConnPid, [], [FakeContractId])
        end,

    NegativePoiTests(SenderConnPid),
    NegativePoiTests(AckConnPid),

    {ok, PoIBin} = aec_base58c:safe_decode(poi, EncodedPoI),
    PoI = aec_trees:deserialize_poi(PoIBin),
    {ok, _SenderAcc} = aec_trees:lookup_poi(accounts, SenderPubkey, PoI),
    {ok, _AckAcc} = aec_trees:lookup_poi(accounts, AckPubkey, PoI),
    {ok, _ContractAcc} = aec_trees:lookup_poi(accounts, ContractPubKey, PoI),
    {ok, _ContractObj} = aec_trees:lookup_poi(contracts, ContractPubKey, PoI),

    ok = ?WS:unregister_test_for_channel_events(SenderConnPid, [sign, info, get, error]),
    ok = ?WS:unregister_test_for_channel_events(AckConnPid, [sign, info, get, error]),
    ok.

contract_id_from_create_update(Owner, OffchainTx) ->
    {CB, Tx} = aetx:specialize_callback(OffchainTx),
    Round = CB:round(Tx),
    aect_contracts:compute_contract_pubkey(Owner, Round).


create_contract_(TestName, SenderConnPid, UpdateVolley) ->
		Code = contract_byte_code(TestName),
    InitArgument = contract_create_init_arg(TestName),
    {ok, EncodedInitData} = aect_sophia:encode_call_data(Code, <<"init">>,
                                                         InitArgument),
    ?WS:send_tagged(SenderConnPid, <<"update">>, <<"new_contract">>,
                    #{vm_version => 1,
                      deposit    => 10,
                      code       => Code,
                      call_data  => EncodedInitData}),
    UnsignedStateTx = UpdateVolley(),
    {UnsignedStateTx, Code}.

contract_calls_("identity", ContractPubKey, Code, SenderConnPid, UpdateVolley,
                GetDecodedResult, _, _ , _) ->
    UnsignedStateTx = call_a_contract(<<"main">>, <<"(42)">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley),
    ExpectedResult = 42,
    DecodedCallResult = GetDecodedResult(contract_return_type("identity"), UnsignedStateTx),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    UnsignedStateTx;
contract_calls_("counter", ContractPubKey, Code, SenderConnPid, UpdateVolley,
                GetDecodedResult, _, _ , _) ->
    TestName = "counter",
    UnsignedStateTx0 = call_a_contract(<<"get">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley),

    InitResult = GetDecodedResult(contract_return_type(TestName), UnsignedStateTx0),
    call_a_contract(<<"tick">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley),

    UnsignedStateTx1 = call_a_contract(<<"get">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley),

    UnsignedStateTx2 = call_a_contract(<<"get">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley),

    ExpectedResult = InitResult + 1,
    DecodedCallResult = GetDecodedResult(contract_return_type(TestName), UnsignedStateTx1),
    DecodedCallResult = GetDecodedResult(contract_return_type(TestName), UnsignedStateTx2),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    UnsignedStateTx0;
contract_calls_("spend_test", ContractPubKey, Code, SenderConnPid, UpdateVolley,
                GetDecodedResult, _AckConnPid, SenderPubkey, AckPubkey) ->
    GetBalance =
        fun(Args) ->
            FunName =
                case Args of
                    <<"()">> -> <<"get_balance">>;
                    _ -> <<"get_balance_of">>
                end,
            UsStateTx = call_a_contract(FunName, Args, ContractPubKey, Code, SenderConnPid,
                            UpdateVolley),
            _DecodedCallResult = GetDecodedResult(contract_return_type("spend_test"), UsStateTx)
        end,
    ContractBalance0 = GetBalance(<<"()">>),

    SenderB0 = GetBalance(list_to_binary("(" ++ aect_utils:hex_bytes(SenderPubkey) ++ ")")),
    AckB0 = GetBalance(list_to_binary("(" ++ aect_utils:hex_bytes(AckPubkey) ++ ")")),

    SpendFun =
        fun(To, Amt) ->
            SpendArgs = list_to_binary("(" ++ aect_utils:hex_bytes(To) ++
                                       ", " ++ integer_to_list(Amt) ++ ")"),
            _SpendStateTx = call_a_contract(<<"spend">>, SpendArgs, ContractPubKey, Code, SenderConnPid,
                                  UpdateVolley)
        end,

    SpendAmt = 3,
    SpendFun(SenderPubkey, SpendAmt),
    ContractBalance = GetBalance(<<"()">>),
    {ContractBalance, _} = {ContractBalance0 - SpendAmt, ContractBalance0},
    SenderB = GetBalance(list_to_binary("(" ++ aect_utils:hex_bytes(SenderPubkey) ++ ")")),
    AckB0 = GetBalance(list_to_binary("(" ++ aect_utils:hex_bytes(AckPubkey) ++ ")")),
    SenderB = SenderB0 + SpendAmt,

    SpendAmt2 = 2,
    UnsignedStateTx = SpendFun(AckPubkey, SpendAmt2),
    ContractBalance1 = GetBalance(<<"()">>),
    {ContractBalance1, _} = {ContractBalance - SpendAmt2, ContractBalance1},
    SenderB = GetBalance(list_to_binary("(" ++ aect_utils:hex_bytes(SenderPubkey) ++ ")")),
    AckB = GetBalance(list_to_binary("(" ++ aect_utils:hex_bytes(AckPubkey) ++ ")")),
    AckB = AckB0 + SpendAmt2,
    UnsignedStateTx.

call_a_contract(Function, Argument, ContractPubKey, Code, SenderConnPid, UpdateVolley) ->
    {ok, EncodedMainData} = aect_sophia:encode_call_data(Code,
                                                         Function,
                                                         Argument),
    ?WS:send_tagged(SenderConnPid, <<"update">>, <<"call_contract">>,
                    #{contract   => aec_base58c:encode(contract_pubkey, ContractPubKey),
                      vm_version => 1,
                      amount     => 0,
                      call_data  => EncodedMainData}),
    _UnsignedStateTx = UpdateVolley().


contract_byte_code(TestName) ->
    %% Compile contract TesName ++ ".aes"
    ContractString = aeso_test_utils:read_contract(TestName),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),
		HexCode.


contract_return_type(_) ->
		<<"int">>.

contract_create_init_arg("identity") ->
		<<"()">>;
contract_create_init_arg("counter") ->
		<<"(21)">>;
contract_create_init_arg("spend_test") ->
		<<"()">>.

contract_result_parse(_TestName, Data) ->
  #{<<"type">> := <<"word">>, <<"value">> := DecodedCallResult} = Data,
  DecodedCallResult.

wait_for_signed_transaction_in_pool(SignedTx) ->
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    WaitForTx =
        fun Try(0) -> no_transaction;
            Try(Attempts) ->
                case tx_in_mempool(TxHash) of
                    true  -> ok;
                    false ->
                        timer:sleep(10),
                        Try(Attempts - 1)
                end
            end,
    ok = WaitForTx(30). % 30 attempts * 10ms

wait_for_signed_transaction_in_block(SignedTx) ->
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    wait_for_tx_hash_on_chain(TxHash).

wait_for_tx_hash_on_chain(TxHash) ->
    case aecore_suite_utils:mine_blocks_until_tx_on_chain(
            aecore_suite_utils:node_name(?NODE), TxHash, 10) of
        {ok, _Blocks} -> ok;
        {error, _Reason} -> did_not_mine
    end.

sc_ws_timeout_open(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 8,
    RAmt = 4,

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{timeout_accept => 100}),
    {ok, IConnPid} = channel_ws_start(initiator, maps:put(host, <<"localhost">>, ChannelOpts)),
    ok = ?WS:register_test_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"died">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    ok.

channel_options(IPubkey, RPubkey, IAmt, RAmt) ->
    channel_options(IPubkey, RPubkey, IAmt, RAmt, #{}).

channel_options(IPubkey, RPubkey, IAmt, RAmt, Other) ->
    maps:merge(#{ port => 12340,
                  initiator_id => aec_base58c:encode(account_pubkey, IPubkey),
                  responder_id => aec_base58c:encode(account_pubkey, RPubkey),
                  lock_period => 10,
                  initiator_amount => IAmt,
                  responder_amount => RAmt,
                  channel_reserve => 2
                }, Other).

peers(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 403, #{<<"reason">> := <<"Call not enabled">>}} = get_peers(),

    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    {ok, 200, #{<<"blocked">> := [], <<"peers">> := Peers}} = get_peers(),

    OkPeers = [ ok || P <- Peers, {ok, _} <- [aec_peers:parse_peer_address(P)] ],

    true = (length(OkPeers) == length(Peers)),

    %% ensure no peers
    lists:foreach(
        fun(Peer) -> rpc(aec_peers, del_peer, [Peer]) end,
        rpc(aec_peers, get_random, [all])),

    {ok, 200, #{<<"blocked">> := [], <<"peers">> := []}} = get_peers(),

    ok.

%% ============================================================
%% WebSocket helpers
%% ============================================================

ws_subscribe(ConnPid, PayLoad) ->
    ok = ?WS:register_test_for_event(ConnPid, chain, subscribe),
    ?WS:send(ConnPid, chain, subscribe, PayLoad),
    {ok, _, #{<<"result">> := <<"ok">>}} = ?WS:wait_for_event(ConnPid, chain, subscribe),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, subscribe).

ws_chain_get(ConnPid, PayLoad) ->
    ws_chain_get(ConnPid, undefined, PayLoad).

ws_chain_get(ConnPid, Tag, PayLoad) ->
    ok = ?WS:register_test_for_event(ConnPid, chain, requested_data),
    case Tag of
        undefined -> ?WS:send(ConnPid, chain, get, PayLoad);
        _         -> ?WS:send(ConnPid, chain, get, Tag, PayLoad)
    end,
    {ok, Tag1, Res} = ?WS:wait_for_event(ConnPid, chain, requested_data),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, requested_data),
    {Tag1, Res}.

ws_mine_key_block(ConnPid, Node, Count) ->
    ok = ?WS:register_test_for_event(ConnPid, chain, mined_block),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(Node), Count),
    {ok, #{<<"height">> := Height, <<"hash">> := Hash}} = ?WS:wait_for_event(ConnPid, chain, mined_block),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, mined_block),
    {Height, Hash}.

ws_mine_key_and_micro_block(ConnPid, Node) ->
    ok = ?WS:register_test_for_events(ConnPid, chain, [mined_block, added_micro_block]),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(Node), 2),
    {ok, #{<<"height">> := Height, <<"hash">> := KeyBlockHash}} =
        ?WS:wait_for_event(ConnPid, chain, mined_block),
    {ok, #{<<"height">> := Height, <<"hash">> := MicroBlockHash}} =
        ?WS:wait_for_event(ConnPid, chain, added_micro_block),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, [mined_block, added_micro_block]),
    {Height, KeyBlockHash, MicroBlockHash}.

%% ============================================================
%% HTTP Requests
%% ============================================================

get_contract_create(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create", Data).

get_contract_create_compute(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create/compute", Data).

get_contract_bytecode(SourceCode) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/compile",
                 #{ <<"code">> => SourceCode, <<"options">> => <<>>}).

call_contract_directly(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/call", Data).

get_contract_call(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/call", Data).

get_contract_call_compute(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/call/compute", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

get_contract_decode_data(Request) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/decode-data", Request).

get_spend(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend", Data).

get_oracle_register(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/oracles/register", Data).

get_oracle_extend(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/oracles/extend", Data).

get_oracle_query(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/oracles/query", Data).

get_oracle_response(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/oracles/respond", Data).

get_name_preclaim(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/preclaim", Data).

get_name_claim(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/claim", Data).

get_name_update(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/update", Data).

get_name_transfer(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/transfer", Data).

get_name_revoke(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/revoke", Data).

get_channel_create(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/create", Data).

get_channel_deposit(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/deposit", Data).

get_channel_withdrawal(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/withdraw", Data).

get_channel_snapshot_solo(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/snapshot/solo", Data).

get_channel_close_mutual(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/close/mutual", Data).

get_channel_close_solo(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/close/solo", Data).

get_channel_slash(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/slash", Data).

get_channel_settle(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/channels/settle", Data).

get_pending_transactions() ->
    Host = internal_address(),
    http_request(Host, get, "debug/transactions/pending", []).

get_tx_nonce(TxHash) ->
    {ok, 200, Tx} = get_transactions_by_hash_sut(TxHash),
    maps:get(<<"nonce">>, maps:get(<<"tx">>, Tx)).

post_spend_tx(RecipientId, Amount, Fee) ->
    {ok, Sender} = rpc(aec_keys, pubkey, []),
    SenderId = aec_base58c:encode(account_pubkey, Sender),
    post_spend_tx(SenderId, RecipientId, Amount, Fee, <<"foo">>).

post_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

get_commitment_id(Name, Salt) ->
    Host = internal_address(),
    http_request(Host, get, "debug/names/commitment-id", [{name, Name}, {salt, Salt}]).

get_balance_at_top() ->
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_node_pubkey(),
    get_accounts_by_pubkey_sut(EncodedPubKey).

get_node_pubkey() ->
    Host = internal_address(),
    http_request(Host, get, "debug/accounts/node", []).

get_peer_pub_key() ->
    Host = external_address(),
    http_request(Host, get, "peers/pubkey", []).

get_peers() ->
    Host = internal_address(),
    http_request(Host, get, "debug/peers", []).

get_contract_poi(ContractAddress) ->
    Host = external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(ContractAddress) ++ "/poi", []).

%% ============================================================
%% Test swagger validation errors
%% ============================================================

swagger_validation_body(_Config) ->
    Host = internal_address(),
    URL = binary_to_list(iolist_to_binary([Host, "/v2/debug/transactions/spend"])),
    Type = "application/json",
    Body = <<"{broken_json">>,

    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"">>,
            <<"info">> := #{
                <<"data">> := <<"{broken_json">>,
                <<"error">> := <<"invalid_body">>
        }}} = process_http_return(R).

%% TODO: different enu
%%swagger_validation_enum(_Config) ->
%%    Host = external_address(),
%%    {ok, 400, #{
%%            <<"reason">> := <<"validation_error">>,
%%            <<"parameter">> := <<"tx_encoding">>,
%%            <<"info">> := #{
%%                <<"data">> := <<"default">>,
%%                <<"error">> := <<"not_in_enum">>
%%        }}} = http_request(Host, get, "block/genesis", #{tx_encoding => <<"default">>}).

%% TODO: use different endpoint to check the validation
%%swagger_validation_required(_Config) ->
%%    Host = external_address(),
%%    {ok, 400, #{
%%            <<"reason">> := <<"validation_error">>,
%%            <<"parameter">> := <<"name">>,
%%            <<"info">> := #{
%%                <<"error">> := <<"missing_required_property">>
%%            }
%%        }} = http_request(Host, get, "names/", []),
%%    ok.

swagger_validation_schema(_Config) ->
    Host = internal_address(),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"body">>,
            <<"info">> :=  #{
                        <<"data">> := <<"wrong_fee_data">>,
                        <<"error">> := <<"wrong_type">>,
                        <<"path">> := [<<"fee">>]
        }}} = http_request(Host, post, "debug/transactions/spend", #{
                   recipient_id => <<"">>,
                   amount => 0,
                   fee => <<"wrong_fee_data">>,
                   ttl => 100,
                   payload => <<"">>}),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"body">>,
            <<"info">> :=  #{
                        <<"data">> := <<"recipient_id">>,
                        <<"error">> := <<"missing_required_property">>,
                        <<"path">> := []
        }}} = http_request(Host, post, "debug/transactions/spend", #{
                   amount => 0,
                   fee => <<"fee">>,
                   ttl => 100,
                   payload => <<"">>}),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"body">>,
            <<"info">> :=  #{
                        <<"data">> := -1,
                        <<"error">> := <<"not_in_range">>,
                        <<"path">> := [<<"amount">>]
        }}} = http_request(Host, post, "debug/transactions/spend", #{
                   recipient_id => <<"">>,
                   amount => -1,
                   fee => <<"fee">>,
                   ttl => 100,
                   payload => <<"">>}).

%%swagger_validation_types(_Config) ->
%%    Host = internal_address(),
%%    {ok, 400, #{
%%            <<"reason">> := <<"validation_error">>,
%%            <<"parameter">> := <<"height">>,
%%            <<"info">> :=  #{
%%                        <<"data">> := <<"not_integer">>,
%%                        <<"error">> := <<"wrong_type">>
%%        }}} = http_request(Host, get,
%%                           "micro-blocks/hash/" ++
%%                           "bh$11111111111111111111111111111111" ++
%%                           "/transactions/index/" ++
%%                           "not_integer", []).

%% ============================================================
%% HTTP Requests with wrong method
%% ============================================================

wrong_http_method_top(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "blocks/top", []).

wrong_http_method_contract_create(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/contracts/create", []).

wrong_http_method_contract_create_compute(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/contracts/create/compute", []).

wrong_http_method_contract_call(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/contracts/call", []).

wrong_http_method_contract_call_compute(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/contracts/call/compute", []).

wrong_http_method_spend(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/transactions/spend", []).

wrong_http_method_oracle_register(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/oracles/register", []).

wrong_http_method_oracle_extend(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/oracles/extend", []).

wrong_http_method_oracle_query(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/oracles/query", []).

wrong_http_method_oracle_response(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/oracles/respond", []).

wrong_http_method_name_preclaim(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/names/preclaim", []).

wrong_http_method_name_claim(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/names/claim", []).

wrong_http_method_name_transfer(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/names/transfer", []).

wrong_http_method_name_revoke(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/names/revoke", []).

wrong_http_method_pending_transactions(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "debug/transactions/pending", []).

wrong_http_method_tx_id(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "transactions/123", []).

wrong_http_method_commitment_hash(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "debug/names/commitment-id", []).

wrong_http_method_name(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "names/test", []).

wrong_http_method_tx(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "transactions", []).

wrong_http_method_node_pubkey(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "debug/accounts/node", []).

wrong_http_method_peers(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "debug/peers", []).

%% ============================================================
%% private functions
%% ============================================================
rpc(Mod, Fun, Args) ->
    rpc(?NODE, Mod, Fun, Args).

rpc(Node, Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"external">>, <<"port">>],
                aehttp, [external, port], 8043]),
    "http://127.0.0.1:" ++ integer_to_list(Port).     % good enough for requests

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"internal">>, <<"port">>],
                aehttp, [internal, port], 8143]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"websocket">>, <<"internal">>, <<"port">>],
                aehttp, [internal, websocket, port], 8144]),
    {"127.0.0.1", Port}.

channel_ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"websocket">>, <<"channel">>, <<"port">>],
                aehttp, [channel, websocket, port], 8045]),
    {"localhost", Port}.

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary([Host, "/v2/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "/v2/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

get_saved_config(Config, Names) ->
    case ?config(saved_config, Config) of
        {Name, ConfigList} ->
            case lists:member(Name, Names) of
                true ->
                    {ok, ConfigList};
                false ->
                    erlang:error({unexpected_saved_config, Name})
            end;
        Other ->
            erlang:error({unexpected, Other})
    end.

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).

prepare_for_spending(BlocksToMine) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), BlocksToMine + 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty
    {ok, 200, _} = get_balance_at_top(), % account present
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    {PubKey, Nonce}.

add_spend_txs() ->
    MineReward = rpc(aec_governance, block_mine_reward, []),
    MinFee = rpc(aec_governance, minimum_tx_fee, []),
    %% For now. Mining is severly slowed down by having too many Tx:s in
    %% the tx pool
    MaxSpendTxsInBlock = 20,
    MinimalAmount = 1,
    MaxTxs = min(MineReward div (MinimalAmount + MinFee), % enough tokens
                 MaxSpendTxsInBlock), % so it can fit in one block
    true = MaxTxs > 0,
    TxsCnt =
        case MaxTxs of
            1 -> 1;
            _ -> rand:uniform(MaxTxs - 1) + 1
        end,
    ct:log("adding ~p spend txs", [TxsCnt]),
    Txs =
        lists:map(
            fun(_) ->
                #{recipient_id => aec_base58c:encode(account_pubkey, random_hash()),
                  amount => MinimalAmount,
                  fee => MinFee}
            end,
            lists:seq(0, TxsCnt -1)),
    populate_block(#{spend_txs => Txs}).

populate_block(Txs) ->
    lists:map(
        fun(#{recipient_id := R, amount := A, fee := F}) ->
                {ok, 200, #{<<"tx">> := SpendTx}} = post_spend_tx(R, A, F),
                sign_and_post_tx(SpendTx)
        end,
        maps:get(spend_txs, Txs, [])).

give_tokens(RecipientPubkey, Amount) ->
    MineReward = rpc(aec_governance, block_mine_reward, []),
    MinFee = rpc(aec_governance, minimum_tx_fee, []),
    NeededBlocks = ((Amount + MinFee)  div MineReward) + 1,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   NeededBlocks),
    SpendData = #{recipient_id => aec_base58c:encode(account_pubkey, RecipientPubkey),
                  amount => Amount,
                  fee => MinFee},
    populate_block(#{spend_txs => [SpendData]}),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ok.

minimal_fee_and_blocks_to_mine(Amount, ChecksCnt) ->
    Fee = rpc(aec_governance, minimum_tx_fee, []),
    MineReward = rpc(aec_governance, block_mine_reward, []),
    TokensRequired = (Amount + Fee) * ChecksCnt,
    BlocksToMine = trunc(math:ceil(TokensRequired / MineReward)),
    {BlocksToMine, Fee}.

ws_start_link() ->
    {Host, Port} = ws_host_and_port(),
    ?WS:start_link(Host, Port).

channel_ws_start(Role, Opts) ->
    {Host, Port} = channel_ws_host_and_port(),
    ?WS:start_channel(Host, Port, Role, Opts).

open_websockets_count() ->
    QueueName = ws_handlers_queue,
    % ensure queue exsits
    true = undefined =/= rpc(jobs, queue_info, [QueueName]),
    length([1 || {_, QName} <- rpc(jobs, info, [monitors]),
                 QName =:= QueueName]).

sign_and_post_tx(EncodedUnsignedTx) ->
    {ok, SerializedUnsignedTx} = aec_base58c:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} = rpc(aec_keys, sign_tx, [UnsignedTx]),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    %% Check that we get the correct hash
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_transactions_sut(aec_base58c:encode(transaction, SerializedTx)),
    %% Check tx is in mempool.
    Fun = fun() ->
                  tx_in_mempool(TxHash)
          end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    TxHash.

tx_in_mempool(TxHash) ->
    case get_transactions_by_hash_sut(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} -> true;
        {ok, 200, #{<<"block_hash">> := Other}} ->
            ct:log("Tx not in mempool, but in chain: ~p", [Other]),
            false;
        {ok, 404, _} -> false
    end.

tx_in_chain(TxHash) ->
    case get_transactions_by_hash_sut(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} ->
            ct:log("Tx not mined, but in mempool"),
            false;
        {ok, 200, #{<<"block_hash">> := _}} -> true;
        {ok, 404, _} -> false
    end.

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.
