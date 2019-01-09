-module(aehttp_integration_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").
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
    post_key_block/1,
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
    post_contract_and_call_tx/1,
    nonce_limit/1
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
    post_broken_api_encoded_tx/1,

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
    node_beneficiary/1,

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

%% test case exports
%% for CORS headers
-export([
    cors_not_returned_when_origin_not_sent/1,
    cors_returned_on_preflight_request/1,
    cors_returned_on_get_request/1]).

%% test case exports
%% for Cowboy handler tests
-export([
    charset_param_in_content_type/1]).

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
    sc_ws_contracts/1,
    sc_ws_oracle_contract/1,
    sc_ws_nameservice_contract/1,
    sc_ws_enviroment_contract/1,
    sc_ws_remote_call_contract/1,
    sc_ws_remote_call_contract_refering_onchain_data/1
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
       {group, naming},
       {group, channel_websocket},
       {group, channel_websocket_legacy}
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
       post_key_block
      ]},
     {on_key_block, [sequence],
      [
       {group, block_info},
       post_key_block
      ]},
     {on_micro_block, [sequence],
      [
       {group, block_info},
       post_key_block
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
       post_spend_tx,
       nonce_limit
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
        post_broken_api_encoded_tx,

        % infos
        peer_pub_key
      ]},
     {internal_endpoints, [sequence],
      [
        broken_spend_tx,
        naming_system_broken_txs,
        node_pubkey,
        node_beneficiary,

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
     {cors_headers, [],
      [cors_not_returned_when_origin_not_sent,
       cors_returned_on_preflight_request,
       cors_returned_on_get_request]},

     {cowboy_handler, [],
      [charset_param_in_content_type]},

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
     {channel_websocket, [sequence],
      channel_websocket_sequence()
     },
     {channel_websocket_legacy, [sequence],
      channel_websocket_sequence()
     }
    ].

channel_websocket_sequence() ->
    [{basic_open_close, [], [sc_ws_timeout_open,
                             sc_ws_open,
                             sc_ws_update,
                             sc_ws_close]},
     %% ensure port is reusable
     {failed_update, [], [sc_ws_open,
                          sc_ws_update_fails_and_close]},
     {generic_messages, [], [sc_ws_open,
                             sc_ws_send_messages_and_close]},
     {update_conflict, [], [sc_ws_open,
                            sc_ws_conflict_and_close]},
     %% initiator can start close mutual
     {initiator_can_start_close_mutual, [], [sc_ws_open,
                                             sc_ws_update,
                                             sc_ws_close_mutual_initiator]},
     %% responder can start close mutual
     {responder_can_start_close_mutual, [], [sc_ws_open,
                                             sc_ws_update,
                                             sc_ws_close_mutual_responder]},
     %% possible to leave and reestablish channel
     {leave_reestablish, [], [sc_ws_open,
                              sc_ws_leave,
                              sc_ws_reestablish,
                              sc_ws_update,
                              sc_ws_close_mutual_initiator]},
     %% initiator can make a deposit
     {initiator_deposit, [], [sc_ws_open,
                              sc_ws_update,
                              sc_ws_deposit_initiator_and_close]},
     %% responder can make a deposit
     {responder_deposit, [], [sc_ws_open,
                              sc_ws_update,
                              sc_ws_deposit_responder_and_close]},
     %% initiator can make a withdrawal
     {initiator_withdrawal, [], [sc_ws_open,
                                 sc_ws_update,
                                 sc_ws_withdraw_initiator_and_close]},
     %% responder can make a withdrawal
     {responder_withdrawal, [], [sc_ws_open,
                                 sc_ws_update,
                                 sc_ws_withdraw_responder_and_close]},
     %% responder can make a withdrawal
     {contracts, [], [sc_ws_open,
                      sc_ws_update,
                      sc_ws_contracts]},
     %% both can refer on-chain objects - oracle
     {onchain_objects_oracles, [], [sc_ws_open,
                                    sc_ws_update,
                                    sc_ws_oracle_contract]},
     %% both can refer on-chain objects - name service
     {onchain_objects_names, [], [sc_ws_open,
                                  sc_ws_update,
                                  sc_ws_nameservice_contract]},

     %% both can refer on-chain objects - chain environment
     {onchain_objects_chain_env, [], [sc_ws_open,
                                      sc_ws_update,
                                      sc_ws_enviroment_contract]},

     %% both can call a remote contract
     {both_can_call_remote_contract, [], [sc_ws_open,
                                          sc_ws_update,
                                          sc_ws_remote_call_contract]},

     %% both can call a remote contract
     {both_can_call_remote_contract_onchain, [], [sc_ws_open,
                                                  sc_ws_update,
                                                  sc_ws_remote_call_contract_refering_onchain_data]}
    ].


suite() ->
    [].

init_per_suite(Config) ->
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
                                                   <<"hard_forks">> => Forks},
                                                  <<"mining">> =>
                                                  #{<<"micro_block_cycle">> => 1}}),
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
    {ok, PendingKeyBlock} = wait_for_key_block_candidate(),
    [{current_block, GenesisBlock},
     {current_block_hash, hash(key, GenesisBlock)},
     {current_block_hash_wrong_type, hash(micro, GenesisBlock)},
     {current_block_height, 0},
     {current_block_type, genesis_block},
     {pending_key_block, PendingKeyBlock} | Config1];
init_per_group(on_key_block = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    %% Mine at least 2 key blocks (fork height may be 0).
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    {ok, PendingKeyBlock} = wait_for_key_block_candidate(),
    [{current_block, KeyBlock},
     {current_block_hash, hash(key, KeyBlock)},
     {current_block_hash_wrong_type, hash(micro, KeyBlock)},
     {current_block_height, aec_blocks:height(KeyBlock)},
     {current_block_type, key_block},
     {pending_key_block, PendingKeyBlock} | Config1];
init_per_group(on_micro_block = Group, Config) ->
    Config1 = start_node(Group, Config),
    [ {NodeId, Node} | _ ] = ?config(nodes, Config1),
    %% Mine at least 2 key blocks (fork height may be 0).
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [_KeyBlock0]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    %% Send spend tx so it gets included into micro block.
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, 20000),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    {ok, [KeyBlock, MicroBlock]} = aecore_suite_utils:mine_micro_blocks(Node, 1),
    {ok, []} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    true = aec_blocks:is_key_block(KeyBlock),
    false = aec_blocks:is_key_block(MicroBlock),
    {ok, PendingKeyBlock} = wait_for_key_block_candidate(),
    [{prev_key_block, KeyBlock},
     {prev_key_block_hash, hash(key, KeyBlock)},
     {prev_key_block_height, aec_blocks:height(KeyBlock)},
     {current_block, MicroBlock},
     {current_block_hash, hash(micro, MicroBlock)},
     {current_block_height, aec_blocks:height(KeyBlock)},
     {current_block_txs, [Tx]},
     {current_block_type, micro_block},
     {pending_key_block, PendingKeyBlock} | Config1];
init_per_group(block_info, Config) ->
    Config;
%% account_endpoints
init_per_group(nonexistent_account = Group, Config) ->
    Config1 = start_node(Group, Config),
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    [{account_id, aehttp_api_encoder:encode(account_pubkey, Pubkey)},
     {account_exists, false} | Config1];
init_per_group(account_with_balance = Group, Config) ->
    Config1 = start_node(Group, Config),
    [ {NodeId, Node} | _ ] = ?config(nodes, Config1),
    {_, Pubkey} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{account_id, aehttp_api_encoder:encode(account_pubkey, Pubkey)},
     {account_exists, true} | Config1];
init_per_group(account_with_pending_tx, Config) ->
    [ {NodeId, Node} | _ ] = ?config(nodes, Config),
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, 20000),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    [{pending_txs, [{aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
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
    [ {NodeId, Node} | _ ] = ?config(nodes, Config1),
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, 20000),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    [{pending_txs, [{aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
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
    [{on_chain_txs, [{aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
     {block_with_txs, MicroBlock},
     {block_with_txs_hash, hash(micro, MicroBlock)},
     {block_with_txs_height, aec_blocks:height(KeyBlock)} | Config1];
init_per_group(post_tx_to_mempool = Group, Config) ->
    Config1 = start_node(Group, Config),
    [ {NodeId, Node} | _ ] = ?config(nodes, Config1),
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{sender_id, aehttp_api_encoder:encode(account_pubkey, Pub)},
     {recipient_id, aehttp_api_encoder:encode(account_pubkey, random_hash())},
     {amount, 1},
     {fee, 20000},
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
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
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
    {ok, 404, _} = get_balance_at_top(),
    IStartAmt = 5000000,
    RStartAmt = 5000000,
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 2),
    {IPubkey, IPrivkey} = initialize_account(IStartAmt),
    {RPubkey, RPrivkey} = initialize_account(RStartAmt),

    Config2 = lists:keystore(sc_ws_protocol, 1, Config1, {sc_ws_protocol, <<"json-rpc">>}),

    Participants = #{initiator => #{pub_key => IPubkey,
                                    priv_key => IPrivkey,
                                    start_amt => IStartAmt},
                     responder => #{pub_key => RPubkey,
                                    priv_key => RPrivkey,
                                    start_amt => RStartAmt}},
    [{participants, Participants} | Config2];
init_per_group(channel_websocket_legacy, Config) ->
    Config1 = init_per_group(channel_websocket, Config),
    Config2 = lists:keyreplace(
                node_start_group, 1, Config1,
                {node_start_group, channel_websocket_legacy}),
    lists:keystore(sc_ws_protocol, 1, Config2, {sc_ws_protocol, <<"legacy">>});
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
end_per_group(account_with_pending_tx, _Config) ->
    ok;
end_per_group(oracle_txs, _Config) ->
    ok;
end_per_group(Group, Config) ->
    ok = stop_node(Group, Config).

init_per_testcase(post_oracle_register, Config) ->
    %% TODO: assert there is enought balance
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    [{account_id, aehttp_api_encoder:encode(account_pubkey, Pubkey)},
     {oracle_id, aehttp_api_encoder:encode(oracle_pubkey, Pubkey)},
     {query_format, <<"something">>},
     {response_format, <<"something else">>},
     {query_fee, 1},
     {fee, 100000},
     {oracle_ttl_type, <<"block">>},
     {oracle_ttl_value, 2000} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_extend, Config) ->
    {post_oracle_register, SavedConfig} = ?config(saved_config, Config),
    OracleTtlDelta = 500,
    [{account_id, ?config(account_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {fee, 100000},
     {oracle_ttl_value_final, ?config(oracle_ttl_value, SavedConfig) + OracleTtlDelta},
     {oracle_ttl_type, <<"delta">>},
     {oracle_ttl_value, OracleTtlDelta} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_query, Config) ->
    {post_oracle_extend, SavedConfig} = ?config(saved_config, Config),
    [{sender_id, ?config(account_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {query, <<"Hejsan Svejsan">>},
     {query_fee, 2},
     {fee, 100000},
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
     {fee, 100000},
     {response_ttl_type, <<"delta">>},
     {response_ttl_value, 20},
     {response, <<"Hejsan">>} | init_per_testcase_all(Config)];
init_per_testcase(_Case, Config) ->
    init_per_testcase_all(Config).

init_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(_Case, Config) ->
    end_per_testcase_all(Config).

end_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
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
    CallEncodedInteger42 = to_contract_bytearray({<<"foo">>, {42, 2}}),
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
    CallEncoded = to_contract_bytearray(CD),
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
                          , #{ <<"type">> := <<"variant">>
                             , <<"value">> :=
                                   [1, #{ <<"type">> := <<"word">>
                                        , <<"value">> := 1 }]} ]}
                  ]
           }
     } = Decoded,

    ok.

broken_decode_sophia_data(_Config) ->
    D = to_contract_bytearray({<<"bar">>, 42}),
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
    {ok, 400, #{<<"reason">> := <<"bad_type">>}} =
        get_contract_decode_data(#{'sophia-type' => <<"foo">>, data => D}),
    {ok, 400, #{<<"reason">> := <<"Data must be hex encoded">>}} =
        get_contract_decode_data(#{'sophia-type' => T, data => <<"foo">>}),
    %% Field valid for both schema and handler, though data
    %% interpreted in a different way than the specified type spec.
    {ok, 200,
     #{<<"data">> :=
           #{<<"type">> := <<"tuple">>,
             <<"value">> :=
                 [#{<<"type">> := <<"string">>, <<"value">> := <<"bar">>},
                  #{<<"type">> := <<"word">>, <<"value">> := 160} ]}}
    } = get_contract_decode_data(#{'sophia-type' => T, data => to_contract_bytearray({<<"bar">>, {42}})}),
    ok.

%% Used in contract-decode endpoint tests.
to_contract_bytearray(Term) ->
    aehttp_api_encoder:encode(contract_bytearray, aeso_heap:to_binary(Term)).

contract_bytearray_decode(X) ->
    case aehttp_api_encoder:safe_decode(contract_bytearray, X) of
        {ok, Y} -> Y;
        {error, _} = E -> error(E)
    end.

decode_data(Type, EncodedData) ->
    {ok, 200, Data} =
        get_contract_decode_data(#{ 'sophia-type' => Type,
                                    data => EncodedData}),
    Data.

%% /blocks/top

get_top_block(Config) ->
    get_top_block(?config(current_block_type, Config),
                  ?config(current_block_hash, Config),
                  Config).

get_top_block(CurrentBlockType, CurrentBlockHash, _Config) when
      CurrentBlockType =:= genesis_block; CurrentBlockType =:= key_block ->
    {ok, 200, #{<<"key_block">> := Block}} = get_top_sut(),
    ?assertEqual(CurrentBlockHash, maps:get(<<"hash">>, Block)),
    ok;
get_top_block(micro_block, CurrentBlockHash, _Config) ->
    {ok, 200, #{<<"micro_block">> := Block}} = get_top_sut(),
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
    get_pending_key_block(CurrentBlockType, Config).

get_pending_key_block(_CurrentBlockType, Config) ->
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

post_key_block(Config) ->
    post_key_block(?config(current_block_type, Config), Config).

post_key_block(_CurrentBlockType, Config) ->
    {ok, 200, #{<<"height">> := Height} = PendingKeyBlock} = get_key_blocks_pending_sut(),

    KeyBlock = PendingKeyBlock#{<<"pow">> => lists:duplicate(42, 1), <<"nonce">> => 1},
    {ok, 400, Error} = post_key_blocks_sut(KeyBlock),
    %% Block is always rejected - pow and nonce are not correct.
    ?assertEqual(<<"Block rejected">>, maps:get(<<"reason">>, Error)),

    KeyBlock1 = PendingKeyBlock#{<<"pow">> => lists:duplicate(42, 0), <<"nonce">> => 0},
    {ok, KeyBlockHeader} = aec_headers:deserialize_from_client(key, KeyBlock1),
    KeyBlockHeaderBin = aec_headers:serialize_to_binary(KeyBlockHeader),
    Target = aec_headers:target(KeyBlockHeader),
    Nonce = aec_pow:pick_nonce(),
    {ok, {Nonce1, PowEvidence}} = mine_key_block(KeyBlockHeaderBin, Target, Nonce, 1000),
    {ok, 200, #{}} = post_key_blocks_sut(PendingKeyBlock#{<<"pow">> => PowEvidence, <<"nonce">> => Nonce1}),
    ok = aecore_suite_utils:wait_for_height(?config(node, Config), Height),
    {ok, 200, CurrentBlock} = get_key_blocks_current_sut(),
    ?assertEqual(Height, maps:get(<<"height">>, CurrentBlock)),
    ?assertEqual(PowEvidence, maps:get(<<"pow">>, CurrentBlock)),
    ?assertEqual(Nonce1, maps:get(<<"nonce">>, CurrentBlock)),
    ok.

mine_key_block(HeaderBir, Target, Nonce, Attempts) when Attempts > 0 ->
    [Config] = rpc(aec_pow_cuckoo, get_miner_configs, []),
    mine_key_block(HeaderBir, Target, Nonce, Config, Attempts).

mine_key_block(HeaderBin, Target, Nonce, Config, Attempts) when Attempts > 0 ->
    case rpc(aec_mining, mine, [HeaderBin, Target, Nonce, Config, 0]) of
        {ok, {_Nonce, _PowEvidence}} = Res ->
            Res;
        {error, no_solution} ->
            mine_key_block(HeaderBin, Target, aec_pow:next_nonce(Nonce, Config), Config, Attempts - 1)
    end;
mine_key_block(_HeaderBin, _Target, _Nonce, _Config, 0) ->
    {error, no_solution}.

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

post_key_blocks_sut(KeyBlock) ->
    Host = internal_address(),
    http_request(Host, post, "key-blocks", KeyBlock).

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
    RandomTxHash = aehttp_api_encoder:encode(tx_hash, random_hash()),
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

nonce_limit(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 5),

    aecore_suite_utils:mine_all_txs(Node),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    Txs = lists:map(
            fun(_N) ->
                    {ok, 200, #{<<"tx">> := SpendTx}} =
                    post_spend_tx(aehttp_api_encoder:encode(account_pubkey, random_hash()),
                                  ?config(amount, Config),
                                  ?config(fee, Config)),
                    {_, Code, _} = sign_and_post_tx_(SpendTx),
                    Code
            end,
            lists:seq(1, 6)),

    ?assertEqual([200, 200, 200, 200, 200, 400], Txs),
    ok.

post_contract_and_call_tx(_Config) ->
    Pubkey = get_pubkey(),

    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := EncodedCode}} = get_contract_bytecode(SophiaCode),
    {ok, Code} = aehttp_api_encoder:safe_decode(contract_bytearray, EncodedCode),
    {ok, EncodedInitCallData} = aehttp_logic:contract_encode_call_data(
                                  <<"sophia">>, Code, <<"init">>, <<"()">>),
    ValidEncoded = #{ owner_id   => Pubkey,
                      code       => EncodedCode,
                      vm_version => ?CURRENT_AEVM_SOPHIA,
                      deposit    => 2,
                      amount     => 1,
                      gas        => 600,
                      gas_price  => 1,
                      fee        => 400000,
                      call_data  => EncodedInitCallData},

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    %%%% {ok, ContractPubKey} = aehttp_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Tx not mined">>}}, get_transactions_info_by_hash_sut(ContractCreateTxHash)),

    % mine
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertMatch({ok, 200, _}, get_transactions_info_by_hash_sut(ContractCreateTxHash)),

    {ok, EncodedCallData} = aehttp_logic:contract_encode_call_data(
                              <<"sophia">>, Code, <<"main">>, <<"42">>),
    ContractCallEncoded = #{ caller_id   => Pubkey,
                             contract_id => EncodedContractPubKey,
                             vm_version  => ?CURRENT_AEVM_SOPHIA,
                             amount      => 1,
                             gas         => 1000,
                             gas_price   => 1,
                             fee         => 500000,
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
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 3),

    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := EncodedCode}} = get_contract_bytecode(SophiaCode),

    % contract_create_tx positive test
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aehttp_logic:contract_encode_call_data(<<"sophia">>,
                                               contract_bytearray_decode(EncodedCode),
                                               InitFunction,
                                               InitArgument),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id   => MinerAddress,
                      code       => EncodedCode,
                      vm_version => ?CURRENT_AEVM_SOPHIA,
                      deposit    => 2,
                      amount     => ContractInitBalance,
                      gas        => 600,
                      gas_price  => 1,
                      fee        => 200000,
                      call_data  => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner_id  => aec_id:create(account, MinerPubkey),
                                code      => contract_bytearray_decode(EncodedCode),
                                call_data => contract_bytearray_decode(EncodedInitCallData)}),

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

    {ok, 200, #{<<"return_value">> := _InitStateAndType}} = get_contract_call_object(ContractCreateTxHash),

    ?assertMatch({ok, 200, #{<<"id">>          := EncodedContractPubKey,
                             <<"owner_id">>    := MinerAddress,
                             <<"active">>      := true,
                             <<"deposit">>     := 2,
                             <<"vm_version">>  := ?CURRENT_AEVM_SOPHIA,
                             <<"referrer_ids">> := [],
                             <<"log">>         := <<>>}},
                 get_contract_sut(EncodedContractPubKey)),
    ?assertEqual({ok, 200, #{<<"bytecode">> => EncodedCode}}, get_contract_code_sut(EncodedContractPubKey)),
    ?assertMatch({ok, 200, #{<<"store">> := [
        #{<<"key">> := <<"0x00">>, <<"value">> := _InitState},
        #{<<"key">> := <<"0x01">>, <<"value">> := _StateType}    %% We store the state type in the Store
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
    RandomOraclePubkey = aehttp_api_encoder:encode(oracle_pubkey, random_hash()),
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
        #{oracle_id    => OracleId,
          query_id     => QueryId,
          response     => Response,
          response_ttl => #{type  => ?config(response_ttl_type, Config),
                            value => ?config(response_ttl_value, Config)},
          fee          => ?config(fee, Config)},
    {TxHash, Tx} = prepare_tx(oracle_response_tx, TxArgs),
    ok = post_tx(TxHash, Tx),
    ok = wait_for_tx_hash_on_chain(TxHash),
    {ok, 200, Resp} = get_oracles_queries_by_pubkey_sut(OracleId, #{type => "open"}),
    ?assertEqual([], maps:get(<<"oracle_queries">>, Resp)),
    {ok, 200, Resp1} = get_oracles_query_by_pubkey_and_query_id(OracleId, QueryId),
    ?assertEqual(QueryId, maps:get(<<"id">>, Resp1)),
    ?assertEqual(OracleId, maps:get(<<"oracle_id">>, Resp1)),
    ?assertEqual({ok, Query}, aehttp_api_encoder:safe_decode(oracle_query, maps:get(<<"query">>, Resp1))),
    ?assertEqual({ok, Response}, aehttp_api_encoder:safe_decode(oracle_response, maps:get(<<"response">>, Resp1))),
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
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ?assertEqual({ok, 400, #{<<"reason">> => <<"Invalid public key">>}},
                 get_channel_by_pubkey_sut(<<"InvalidKey">>)),

    #{i := #{channel_id := ChannelId0, pub := IPub},
      r := #{pub := RPub}
     } = aesc_fsm_SUITE:create_channel_on_port(9311),
    ChannelId = aehttp_api_encoder:encode(channel, ChannelId0),

    {ok, 200, #{
        <<"id">> := ChannelId,
        <<"initiator_id">> := InitiatorId,
        <<"responder_id">> := ResponderId,
        <<"delegate_ids">> := [],         %% Update needed
        <<"state_hash">> := StateHash
      }} = get_channel_by_pubkey_sut(ChannelId),

    ?assertEqual({ok, IPub}, aehttp_api_encoder:safe_decode(account_pubkey, InitiatorId)),
    ?assertEqual({ok, RPub}, aehttp_api_encoder:safe_decode(account_pubkey, ResponderId)),
    ?assertMatch({ok, _}, aehttp_api_encoder:safe_decode(state, StateHash)),
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
       <<"genesis_key_block_hash">>     := GenesisKeyBlocHash,
       <<"solutions">>                  := Solutions,
       <<"difficulty">>                 := Difficulty,
       <<"syncing">>                    := Syncing,
       <<"listening">>                  := Listening,
       <<"protocols">>                  := Protocols,
       <<"node_version">>               := _NodeVersion,
       <<"node_revision">>              := _NodeRevision,
       <<"peer_count">>                 := PeerCount,
       <<"pending_transactions_count">> := PendingTxCount,
       <<"network_id">>                 := NetworkId
      }} = get_status_sut(),
    ?assertMatch({ok, _}, aehttp_api_encoder:safe_decode(key_block_hash, GenesisKeyBlocHash)),
    ?assertMatch(X when is_integer(X) andalso X >= 0, Solutions),
    ?assertMatch(X when is_integer(X), Difficulty),
    ?assertMatch(X when is_boolean(X), Syncing),
    ?assertMatch(X when is_boolean(X), Listening),
    ?assertMatch(X when is_list(X) andalso length(X) > 0, Protocols),
    lists:foreach(fun(P) ->
                          ?assertMatch(X when is_integer(X) andalso X >= 0, maps:get(<<"version">>, P)),
                          ?assertMatch(X when is_integer(X) andalso X >= 0, maps:get(<<"effective_at_height">>, P))
                  end, Protocols),
    ?assertMatch(X when is_integer(X) andalso X >= 0, PeerCount),
    ?assertMatch(X when is_integer(X) andalso X >= 0, PendingTxCount),
    ?assertEqual(NetworkId, aec_governance:get_network_id()),
    ok.

get_status_sut() ->
    Host = external_address(),
    http_request(Host, get, "status", []).

prepare_tx(TxType, Args) ->
    %assert_required_tx_fields(TxType, Args),
    {Host, Path} = tx_object_http_path(TxType),
    {ok, 200, #{<<"tx">> := EncodedSerializedUnsignedTx}} = http_request(Host, post, Path, Args),
    {ok, SerializedUnsignedTx} = aehttp_api_encoder:safe_decode(transaction, EncodedSerializedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),

    NodeT = aecore_suite_utils:node_tuple(?NODE),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(NodeT, UnsignedTx),

    TxHash = aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx = aehttp_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
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
    aehttp_api_encoder:encode(key_block_hash, Hash0);
hash(micro, Block) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(Block),
    aehttp_api_encoder:encode(micro_block_hash, Hash0).

wait_for_key_block_candidate() -> wait_for_key_block_candidate(10).

wait_for_key_block_candidate(0) -> {error, not_found};
wait_for_key_block_candidate(N) ->
    case rpc(aec_conductor, get_key_block_candidate, []) of
        {ok, Block} -> {ok, Block};
        {error, _Rsn} ->
            timer:sleep(10),
            wait_for_key_block_candidate(N - 1)
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
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 3),
    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := EncodedCode}} = get_contract_bytecode(SophiaCode),

    % contract_create_tx positive test
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aehttp_logic:contract_encode_call_data(<<"sophia">>,
                                               contract_bytearray_decode(EncodedCode),
                                               InitFunction,
                                               InitArgument),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id => MinerAddress,
                      code => EncodedCode,
                      vm_version => ?CURRENT_AEVM_SOPHIA,
                      deposit => 2,
                      amount => ContractInitBalance,
                      gas => 600,
                      gas_price => 1,
                      fee => 200000,
                      call_data => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner_id => aec_id:create(account, MinerPubkey),
                                code => contract_bytearray_decode(EncodedCode),
                                call_data => contract_bytearray_decode(EncodedInitCallData)}),

    unsigned_tx_positive_test(ValidDecoded, ValidEncoded, fun get_contract_create/1,
                               fun aect_create_tx:new/1, MinerPubkey),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aehttp_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
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
    ?assertEqual(aehttp_api_encoder:encode(contract_pubkey, ContractPubKey),
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
    _ = maps:get(<<"return_value">>, InitCallObject),

    {ok, 200, #{<<"poi">> := EncPoI}} = get_contract_poi(EncodedContractPubKey),
    {ok, PoIBin} = aehttp_api_encoder:safe_decode(poi, EncPoI),
    PoI = aec_trees:deserialize_poi(PoIBin),
    {ok, ContractInPoI} = aec_trees:lookup_poi(contracts, ContractPubKey, PoI),
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ContractInPoI1 = rpc(aect_state_tree, get_contract, [ContractPubKey,
                                                         aec_trees:contracts(Trees)]),
    %% Don't require the store mp trees to be identical as Erlang terms, but
    %% check that the contents is the same and that the root hashes of the MP
    %% trees are the same
    Store    = aect_contracts:state(ContractInPoI),
    Store1   = aect_contracts:state(ContractInPoI1),
    Contents = aect_contracts_store:contents(Store),
    Contents = rpc(aect_contracts_store, contents, [Store1]),
    RootHash = aeu_mtrees:root_hash(aect_contracts_store:mtree(Store)),
    RootHash = aeu_mtrees:root_hash(aect_contracts_store:mtree(Store1)),
    %% Check the non-store parts of the contract
    Ct = aect_contracts:internal_set_state(dummy, ContractInPoI),
    Ct = aect_contracts:internal_set_state(dummy, ContractInPoI1),
    {ok, ContractAccInPoI} = aec_trees:lookup_poi(accounts, ContractPubKey, PoI),
    ContractAccInPoI = rpc(aec_accounts_trees, get, [ContractPubKey,
                                                     aec_trees:accounts(Trees)]),

    %% Assert the balance is the one which we created the contract with
    {ok, 200, #{<<"balance">> := ContractInitBalance}} =
        get_accounts_by_pubkey_sut(EncodedContractPubKey),
    Function = <<"main">>,
    Argument = <<"42">>,
    {ok, EncodedCallData} =
        aehttp_logic:contract_encode_call_data(<<"sophia">>,
                                               contract_bytearray_decode(EncodedCode),
                                               Function,
                                               Argument),


    ContractCallEncoded = #{ caller_id => MinerAddress,
                             contract_id => EncodedContractPubKey,
                             vm_version => ?CURRENT_AEVM_SOPHIA,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 600000,
                             call_data => EncodedCallData},

    ContractCallDecoded = maps:merge(ContractCallEncoded,
                              #{caller_id => aec_id:create(account, MinerPubkey),
                                contract_id => aec_id:create(contract, ContractPubKey),
                                call_data => contract_bytearray_decode(EncodedCallData)}),

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
    ?assertEqual(aehttp_api_encoder:encode(contract_pubkey, ContractPubKey),
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
                             vm_version => ?CURRENT_AEVM_SOPHIA,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 500000,
                             function => Function,
                             arguments => Argument},

    ComputeCCallDecoded = maps:merge(ComputeCCallEncoded,
                              #{caller_id => aec_id:create(account, MinerPubkey),
                                contract_id => aec_id:create(contract, ContractPubKey),
                                call_data => contract_bytearray_decode(EncodedCallData)}),

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
    ?assertEqual(aehttp_api_encoder:encode(contract_pubkey, ContractPubKey),
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
    RandAddress = aehttp_api_encoder:encode(account_pubkey, random_hash()),
    RandContractAddress =aehttp_api_encoder:encode(contract_pubkey, random_hash()),
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
    {ok, 400, #{<<"reason">> := <<"Not byte array: code">>}} =
        get_contract_create(maps:put(code, InvalidHex1, ValidEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not byte array: code">>}} =
        get_contract_create(maps:put(code, InvalidHex2, ValidEncoded)),
    % invalid call data
    {ok, 400, #{<<"reason">> := <<"Not byte array: call_data">>}} =
        get_contract_create(maps:put(call_data, InvalidHex1, ValidEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not byte array: call_data">>}} =
        get_contract_create(maps:put(call_data, InvalidHex2, ValidEncoded)),
    % invalid call data
    {ok, 400, #{<<"reason">> := <<"Not byte array: call_data">>}} =
        get_contract_call(maps:put(call_data, InvalidHex1, ContractCallEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not byte array: call_data">>}} =
        get_contract_call(maps:put(call_data, InvalidHex2, ContractCallEncoded)),

    {ok, 400, #{<<"reason">> := <<"Failed to compute call_data, reason: bad argument">>}} =
        get_contract_call_compute(maps:put(arguments, <<"garbadge">>,
                                           ComputeCCallEncoded)),

    %% Call objects
    {ok, 200, #{<<"tx">> := SpendTx}} = post_spend_tx(MinerAddress, 1, 20000),
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

    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id => MinerAddress,
                      code => Code,
                      vm_version => ?CURRENT_AEVM_SOPHIA,
                      deposit => 2,
                      amount => ContractInitBalance,
                      gas => 600,
                      gas_price => 1,
                      fee => 200000,
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
                             vm_version => ?CURRENT_AEVM_SOPHIA,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 500000,
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
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),

    % contract_create_tx positive test
    EncodedCode = contract_byte_code("init_error"),
    {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                              <<"sophia">>,
                              contract_bytearray_decode(EncodedCode),
                              <<"init">>, <<"(0x123, 0)">>),
    EncodedInitCallData = aehttp_api_encoder:encode(contract_bytearray, aeso_heap:to_binary({<<"init">>, {}})),
    ValidEncoded = #{ owner_id   => MinerAddress,
                      code       => EncodedCode,
                      vm_version => ?CURRENT_AEVM_SOPHIA,
                      deposit    => 2,
                      amount     => 1,
                      gas        => 30,
                      gas_price  => 1,
                      fee        => 200000,
                      call_data  => EncodedInitData},
    ValidDecoded = maps:merge(ValidEncoded,
        #{owner => MinerPubkey,
            code => contract_bytearray_decode(EncodedCode),
            call_data => contract_bytearray_decode(EncodedInitCallData)}),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
        <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aehttp_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
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
    ?assertEqual(aehttp_api_encoder:encode(contract_pubkey, ContractPubKey),
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
    _ = maps:get(<<"return_value">>, InitCallObject),

    ok.

oracle_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),
    OracleAddress = aehttp_api_encoder:encode(oracle_pubkey, MinerPubkey),

    % oracle_register_tx positive test
    RegEncoded = #{account_id => MinerAddress,
                   query_format => <<"something">>,
                   response_format => <<"something else">>,
                   query_fee => 1,
                   fee => 100000,
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
    ExtEncoded = #{oracle_id => aehttp_api_encoder:encode(oracle_pubkey, MinerPubkey),
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
                     oracle_id => aehttp_api_encoder:encode(oracle_pubkey, MinerPubkey),
                     query => <<"Hejsan Svejsan">>,
                     query_fee => 2,
                     fee => 100000,
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
                        query_id => aehttp_api_encoder:encode(oracle_query_id,
                                                       QueryId),
                        response => <<"Hejsan">>,
                        response_ttl => #{type => <<"delta">>, value => 20},
                        fee => 100000},
    ResponseDecoded = maps:merge(ResponseEncoded,
                              #{oracle_id => aec_id:create(oracle, MinerPubkey),
                                response_ttl => {delta, 20},
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
    RandAddress = aehttp_api_encoder:encode(account_pubkey, random_hash()),
    RandOracleAddress = aehttp_api_encoder:encode(oracle_pubkey, random_hash()),
    RandQueryID = aehttp_api_encoder:encode(oracle_query_id, random_hash()),
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
    BrokenTTL3 = #{<<"type">> => <<"delta">>, <<"value">> => 21}, %% only bad for Response

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
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL1, ResponseEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL2, ResponseEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL3, ResponseEncoded)),
    % test non-relative ttl
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, #{type => <<"block">>,
                                                  value => 2}, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, #{type => <<"block">>,
                                                  value => 2}, ResponseEncoded)),
    ok.

%% tests the following
%% GET preclaim_tx unsigned transaction
%% GET claim_tx unsigned transaction
%% GET update_tx unsigned transaction
%% GET transfer_tx unsigned transaction
%% GET revoke_tx unsigned transaction
nameservice_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),
    nameservice_transaction_preclaim(MinerAddress, MinerPubkey),
    nameservice_transaction_claim(MinerAddress, MinerPubkey),
    nameservice_transaction_update(MinerAddress, MinerPubkey),
    nameservice_transaction_transfer(MinerAddress, MinerPubkey),
    nameservice_transaction_revoke(MinerAddress, MinerPubkey),
    ok.

nameservice_transaction_preclaim(MinerAddress, MinerPubkey) ->
    Commitment = random_hash(),
    Encoded = #{account_id => MinerAddress,
                commitment_id => aehttp_api_encoder:encode(commitment, Commitment),
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
    CorrectAddress = aehttp_api_encoder:encode(PubKeyType, PubKey),
    Msg = list_to_binary("Invalid hash: " ++ atom_to_list(Name)),
    <<_, HashWithBrokenPrefix/binary>> = CorrectAddress,
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HashWithBrokenPrefix, Encoded)),

    <<_Prefix:3/binary, HashWithNoPrefix/binary>> = CorrectAddress,
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HashWithNoPrefix, Encoded)),

    case aehttp_api_encoder:byte_size_for_type(PubKeyType) of
        not_applicable -> pass;
        _ ->
            <<ShortHash:10/binary, _Rest/binary>> = CorrectAddress,
            {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, ShortHash, Encoded)),

            BS = byte_size(PubKey),
            HalfSize = BS div 2,
            <<FirstHalfKey:HalfSize/binary, _SecondHalfKey/binary>> = PubKey,
            HalfHash = aehttp_api_encoder:encode(PubKeyType, FirstHalfKey),
            {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HalfHash, Encoded))
    end,
    ok.

test_missing_address(Key, Encoded, APIFun) ->
    Msg = list_to_binary("Account of " ++ atom_to_list(Key) ++ " not found"),
    RandAddress = aehttp_api_encoder:encode(account_pubkey, random_hash()),
    {ok, 404, #{<<"reason">> := Msg}} =
        APIFun(maps:put(Key, RandAddress, Encoded)),
    ok.

nameservice_transaction_claim(MinerAddress, MinerPubkey) ->
    Name = <<"name.test">>,
    Salt = 1234,

    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = get_commitment_id(Name, Salt),
    {ok, _CHash} = aehttp_api_encoder:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => 100000,
                     account_id    => MinerAddress},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = get_name_preclaim(PreclaimData),
    PreclaimTxHash = sign_and_post_tx(PreclaimTxEnc),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    {ok, _BS1} = aecore_suite_utils:mine_blocks_until_txs_on_chain(
                    aecore_suite_utils:node_name(?NODE), [PreclaimTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    Encoded = #{account_id => MinerAddress,
                name => aehttp_api_encoder:encode(name, Name),
                name_salt => Salt,
                fee => 100000},
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
    Missing = aehttp_api_encoder:encode(name, <<"missing">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: no_registrar">>}} =
        get_name_claim(maps:put(name, Missing, Encoded)),
    MissingReg = aehttp_api_encoder:encode(name, <<"missing.reg">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_name_claim(maps:put(name, MissingReg, Encoded)),
    ok.

nameservice_transaction_update(MinerAddress, MinerPubkey) ->
    NameHash = random_hash(),
    Pointers = [],
    Encoded = #{account_id => MinerAddress,
                name_id => aehttp_api_encoder:encode(name, NameHash),
                name_ttl => 3,
                client_ttl => 2,
                pointers => Pointers,
                fee => 100000},
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
    {ok, 400, #{<<"reason">> := <<"Invalid pointers">>}} =
        get_name_update(maps:put(pointers, [#{<<"key">> => <<"k2">>,
                                              <<"id">> => <<"not a valid pointer">>}], Encoded)),
    {ok, 400, #{<<"reason">> := <<"validation_error">>}} =
        get_name_update(maps:put(pointers, [#{<<"invalid_key">> => <<"k2">>,
                                              <<"id">> => <<"not a valid pointer">>}], Encoded)),
    ok.

nameservice_transaction_transfer(MinerAddress, MinerPubkey) ->
    RandAddress = random_hash(),
    NameHash = random_hash(),
    Encoded = #{account_id => MinerAddress,
                name_id => aehttp_api_encoder:encode(name, NameHash),
                recipient_id => aehttp_api_encoder:encode(account_pubkey, RandAddress),
                fee => 100000},
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
                name_id => aehttp_api_encoder:encode(name, NameHash),
                fee => 10000},
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
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),
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
    Encoded = #{initiator_id => aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
                initiator_amount => 2,
                responder_id => aehttp_api_encoder:encode(account_pubkey, ResponderPubkey),
                responder_amount => 3,
                push_amount => 5, channel_reserve => 5,
                lock_period => 20,
                state_hash => aehttp_api_encoder:encode(state, ?BOGUS_STATE_HASH),
                fee => 100000},
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
    MinerAddress = aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                from_id => MinerAddress,
                amount => 2,
                state_hash => aehttp_api_encoder:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 100000},
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
    MinerAddress = aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                to_id => MinerAddress,
                amount => 2,
                state_hash => aehttp_api_encoder:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 100000},
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
    _PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                from_id => aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                fee => 100000},
    Decoded = maps:merge(Encoded,
                        #{from_id => aec_id:create(account, MinerPubkey),
                          channel_id => aec_id:create(channel, ChannelId)}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_snapshot_solo/1,
                               fun aesc_snapshot_solo_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_snapshot_solo/1),
    ok.

state_channels_close_mutual(ChannelId, InitiatorPubkey) ->
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                from_id => aehttp_api_encoder:encode(account_pubkey, InitiatorPubkey),
                initiator_amount_final => 4,
                responder_amount_final => 3,
                fee => 100000},
    Decoded = maps:merge(Encoded,
                         #{channel_id => aec_id:create(channel, ChannelId),
                           from_id    => aec_id:create(account, InitiatorPubkey)}),
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
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                from_id => aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                poi => aehttp_api_encoder:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 100000},
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
            EncBrokenPoI =  aehttp_api_encoder:encode(poi, BrokenPoI),
            {ok, 400, #{<<"reason">> := <<"Invalid proof of inclusion">>}}
                = get_channel_close_solo(maps:put(poi, EncBrokenPoI, Encoded))
        end,
        BrokenPoIs),
    ok.

state_channels_slash(ChannelId, MinerPubkey) ->
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                from_id => aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                poi => aehttp_api_encoder:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 100000},
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
            EncBrokenPoI =  aehttp_api_encoder:encode(poi, BrokenPoI),
            {ok, 400, #{<<"reason">> := <<"Invalid proof of inclusion">>}}
                = get_channel_slash(maps:put(poi, EncBrokenPoI, Encoded))
        end,
        BrokenPoIs),
    ok.

state_channels_settle(ChannelId, MinerPubkey) ->
    Encoded = #{channel_id => aehttp_api_encoder:encode(channel, ChannelId),
                from_id => aehttp_api_encoder:encode(account_pubkey, MinerPubkey),
                initiator_amount_final => 4,
                responder_amount_final => 3,
                fee => 100000},
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
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aehttp_api_encoder:safe_decode(account_pubkey, MinerAddress),
    RandAddress = random_hash(),
    Encoded = #{sender_id => MinerAddress,
                recipient_id => aehttp_api_encoder:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 100000,
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
    MinerAddress = get_pubkey(),
    RandAddress = random_hash(),
    Encoded = #{sender_id => MinerAddress,
                recipient_id => aehttp_api_encoder:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 20000,
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
            {ok, SerializedTx} = aehttp_api_encoder:safe_decode(transaction, ActualTx),
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
    EncodedPubKey = get_pubkey(),
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
                recipient_id => aehttp_api_encoder:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 20000,
                payload => <<"foo">>},
    {ok, 200, #{<<"tx">> := EncodedSpendTx}} = get_spend(Encoded),
    {ok, SpendTxBin} = aehttp_api_encoder:safe_decode(transaction, EncodedSpendTx),
    SpendTx = aetx:deserialize_from_binary(SpendTxBin),
    {ok, SignedSpendTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
    TxHash = aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSpendTx)),

    SerializedSpendTx = aetx_sign:serialize_to_binary(SignedSpendTx),
    {ok, 200, _} = post_transactions_sut(aehttp_api_encoder:encode(transaction, SerializedSpendTx)),
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
    Node = aecore_suite_utils:node_name(?NODE),

    % For this test we need a clean mempool and all mining rewards for fees
    % to be given to the miner. The test relies on the miner receiving only
    % block rewards (and no transaction fees)

    % mine all pending transactions, if any
    {ok, PendingTxs} = rpc(aec_tx_pool, peek, [infinity]), % empty
    PendingTxHashes =
        [aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))
            || SignedTx <- PendingTxs],
    ct:log("Pending txs: ~p", [PendingTxs]),
    ct:log("Pending tx hashes: ~p", [PendingTxHashes]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, PendingTxHashes, 10),

    %% Mine for the reward delay to avoid worrying about start of the chain effects
    Delay = rpc(aec_governance, beneficiary_reward_delay, []),
    aecore_suite_utils:mine_key_blocks(Node, Delay),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),
    InitialBalance =
        case get_balance_at_top() of
            {ok, 404, #{<<"reason">> := <<"Account not found">>}} -> 0;
            {ok, 200, #{<<"balance">> := Bal00}} -> Bal00
        end,

    AmountToSpent = 3,
    BlocksToMine = blocks_to_mine(AmountToSpent, 1),
    {ok, MinedBlocks1} = aecore_suite_utils:mine_key_blocks(Node, BlocksToMine),
    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),

    ExpectedReward = lists:sum([rpc(aec_governance, block_mine_reward,
                                    [aec_blocks:height(X) - Delay])
                                || X <- MinedBlocks1,
                                   aec_blocks:type(X) =:= key
                               ]),

    ct:log("Bal0: ~p, Initial Balance: ~p, Blocks to mine: ~p, Expected reward: ~p",
           [Bal0, InitialBalance, BlocksToMine, ExpectedReward]),
    ?assertEqual(Bal0, InitialBalance + ExpectedReward),
    true = (is_integer(Bal0) andalso Bal0 >= AmountToSpent),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, ReceiverPubKey)),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aehttp_api_encoder:encode(account_pubkey, ReceiverPubKey), AmountToSpent, 20000),
    sign_and_post_tx(SpendTx),
    {ok, NodeTxs} = rpc(aec_tx_pool, peek, [infinity]),
    true = length(NodeTxs) =:= 1, % not empty anymore
    {ok, 200, #{<<"transactions">> := ReturnedTxs}} = get_pending_transactions(),
    ExpectedTxs = [aetx_sign:serialize_for_client_pending(T) || T <- NodeTxs],
    true = length(ExpectedTxs) =:= length(ReturnedTxs),
    true = lists:all(fun(Tx) -> lists:member(Tx, ExpectedTxs) end, ReturnedTxs),

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, ReceiverPubKey)),

    PendingTxHashes2 =
        [aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))
            || SignedTx <- NodeTxs],
    {ok, MinedBlocks2a} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, PendingTxHashes2, 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    %% Make sure we get the reward...
    {ok, MinedBlocks2b} = aecore_suite_utils:mine_key_blocks(Node, Delay),

    ExpectedReward1 = lists:sum([rpc(aec_governance, block_mine_reward,
                                     [aec_blocks:height(X) - Delay])
                                 || X <- MinedBlocks2a ++ MinedBlocks2b,
                                    aec_blocks:type(X) =:= key
                                ]),
    {ok, 200, #{<<"balance">> := Bal1}} = get_balance_at_top(),
    ct:log("Bal1: ~p, Bal0: ~p, Expected reward: ~p, Amount to spend: ~p",
           [Bal1, Bal0, ExpectedReward1, AmountToSpent]),
    ?assertEqual(Bal1, Bal0 + ExpectedReward1 - AmountToSpent),
    {ok, 200, #{<<"balance">> := AmountToSpent}} =
                 get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, ReceiverPubKey)),
    ok.

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(_Config) ->
    Amount = 1,
    BlocksToMine = blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender_id => aec_id:create(account, PubKey),
            recipient_id => aec_id:create(account, random_hash()),
            amount => Amount,
            fee => 20000,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
    ExpectedHash = aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, 200, #{<<"tx_hash">> := ExpectedHash}} =
        post_transactions_sut(aehttp_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx))),
    {ok, [SignedTx]} = rpc(aec_tx_pool, peek, [infinity]), % same tx
    ok.

post_broken_tx(_Config) ->
    Amount = 1,
    BlocksToMine = blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(max(BlocksToMine, 3)),  %% we need at least 3 blocks
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender_id => aec_id:create(account, PubKey),
            recipient_id => aec_id:create(account, random_hash()),
            amount => Amount,
            fee => 20000,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
    SignedTxBin = aetx_sign:serialize_to_binary(SignedTx),

    {ok, SpendTTLTx} =
        aec_spend_tx:new(
          #{sender_id => aec_id:create(account, PubKey),
            recipient_id => aec_id:create(account, random_hash()),
            amount => Amount,
            fee => 20000,
            nonce => Nonce,
            ttl => 2,
            payload => <<"too low ttl">>}),
    {ok, SignedTTLTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTTLTx),
    SignedTTLTxBin = aetx_sign:serialize_to_binary(SignedTTLTx),

    BrokenTxBin = case SignedTxBin of
                    <<1:1, Rest/bits>> -> <<0:1, Rest/bits>>;
                    <<0:1, Rest/bits>> -> <<1:1, Rest/bits>>
                  end,
    EncodedBrokenTx = aehttp_api_encoder:encode(transaction, BrokenTxBin),
    EncodedBrokenTTLTx = aehttp_api_encoder:encode(transaction, SignedTTLTxBin),
    EncodedSignedTx = aehttp_api_encoder:encode(transaction, SignedTxBin),
    {ok, 400, #{<<"reason">> := <<"Invalid tx">>}} = post_transactions_sut(EncodedBrokenTx),
    {ok, 400, #{<<"reason">> := <<"Invalid tx">>}} = post_transactions_sut(EncodedBrokenTTLTx),
    {ok, 200, _} = post_transactions_sut(EncodedSignedTx),
    ok.

post_broken_api_encoded_tx(_Config) ->
    Amount = 1,
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
    BlocksToMine = blocks_to_mine(Amount, NumberOfChecks),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    lists:foreach(
        fun(_) ->
            {ok, SpendTx} =
                aec_spend_tx:new(
                  #{sender_id => aec_id:create(account, PubKey),
                    recipient_id => aec_id:create(account, random_hash()),
                    amount => Amount,
                    fee => 20000,
                    nonce => Nonce,
                    payload => <<"foo">>}),
            {ok, SignedTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
            <<_, BrokenHash/binary>> =
                aehttp_api_encoder:encode(transaction,
                                   aetx_sign:serialize_to_binary(SignedTx)),
            {ok, 400, #{<<"reason">> := <<"Invalid api encoding">>}} = post_transactions_sut(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

%% positive test of spend_tx is handled in pending_transactions test
broken_spend_tx(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance_at_top(),
    ReceiverPubKey = random_hash(),
    {ok, 404, _} = post_spend_tx(aehttp_api_encoder:encode(account_pubkey, ReceiverPubKey), 42, 2),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

node_pubkey(_Config) ->
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_node_pubkey(),
    ct:log("MinerPubkey = ~p~nEncodedPubKey = ~p", [MinerPubKey,
                                                    EncodedPubKey]),
    {account_pubkey, MinerPubKey} = aehttp_api_encoder:decode(EncodedPubKey),
    ok.

node_beneficiary(_Config) ->
    {ok, 200, #{<<"pub_key">> := SignPubKey0}} = get_node_pubkey(),
    {ok, 200, #{<<"pub_key">> := BeneficiaryPubKey0}} = get_node_beneficiary(),

    ?assertMatch({account_pubkey, _}, aehttp_api_encoder:decode(BeneficiaryPubKey0)),

    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 3),

    {ok, 200, #{<<"pub_key">> := SignPubKey1}} = get_node_pubkey(),
    {ok, 200, #{<<"pub_key">> := BeneficiaryPubKey1}} = get_node_beneficiary(),

    ?assertNotEqual(SignPubKey0, SignPubKey1),
    ?assertNotEqual(SignPubKey1, BeneficiaryPubKey1),
    ?assertEqual(BeneficiaryPubKey0, BeneficiaryPubKey1),
    ok.

peer_pub_key(_Config) ->
    {ok, PeerPubKey} = rpc(aec_keys, peer_pubkey, []),
    {ok, 200, #{<<"pubkey">> := EncodedPubKey}} = get_peer_pub_key(),
    ct:log("PeerPubkey = ~p~nEncodedPubKey = ~p", [PeerPubKey,
                                                    EncodedPubKey]),
    {ok, PeerPubKey} = aehttp_api_encoder:safe_decode(peer_pubkey, EncodedPubKey),
    ok.

naming_system_manage_name(_Config) ->
    {PubKey, PrivKey} = initialize_account(1000000000),
    PubKeyEnc   = aehttp_api_encoder:encode(account_pubkey, PubKey),
    %% TODO: find out how to craete HTTP path with unicode chars
    %%Name        = <<"詹姆斯詹姆斯.test"/utf8>>,
    Name        = <<"without-unicode.test">>,
    NameSalt    = 12345,
    NameTTL     = 20000,
    Pointers    = [#{<<"key">> => <<"account_pubkey">>, <<"id">> => PubKeyEnc}],
    TTL         = 10,
    {ok, NHash} = aens:get_name_hash(Name),
    Fee         = 100000,
    Node        = aecore_suite_utils:node_name(?NODE),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    {ok, 200, #{<<"balance">> := Balance}} = get_accounts_by_pubkey_sut(PubKeyEnc),

    %% Get commitment hash to preclaim a name
    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = get_commitment_id(Name, NameSalt),
    {ok, _CHash} = aehttp_api_encoder:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => Fee,
                     account_id    => PubKeyEnc},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = get_name_preclaim(PreclaimData),
    PreclaimTxHash = sign_and_post_tx(PreclaimTxEnc, PrivKey),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    {ok,_BS1} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [PreclaimTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check fee taken from account
    {ok, 200, #{<<"balance">> := Balance1}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance1, Balance - Fee),

    %% Submit name claim tx and check it is in mempool
    ClaimData = #{account_id => PubKeyEnc,
                  name       => aehttp_api_encoder:encode(name, Name),
                  name_salt  => NameSalt,
                  fee        => Fee},
    {ok, 200, #{<<"tx">> := ClaimTxEnc}} = get_name_claim(ClaimData),
    ClaimTxHash = sign_and_post_tx(ClaimTxEnc, PrivKey),

    %% Mine a block and check mempool empty again
    {ok, _BS2} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ClaimTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check tx fee taken from account, claim fee locked,
    %% then mine reward and fee added to account
    ClaimLockedFee = rpc(aec_governance, name_claim_locked_fee, []),
    {ok, 200, #{<<"balance">> := Balance2}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    {ok, 200, #{<<"height">> := Height3}} = get_key_blocks_current_sut(),
    ?assertEqual(Balance2, Balance1 - Fee - ClaimLockedFee),

    %% Check that name entry is present
    EncodedNHash = aehttp_api_encoder:encode(name, NHash),
    ExpectedTTL1 = (Height3 - 1) + aec_governance:name_claim_max_expiration(),
    {ok, 200, #{<<"id">>       := EncodedNHash,
                <<"ttl">>      := ExpectedTTL1,
                <<"pointers">> := []}} = get_names_entry_by_name_sut(Name),

    %% Submit name updated tx and check it is in mempool
    NameUpdateData = #{account_id => PubKeyEnc,
                       name_id    => aehttp_api_encoder:encode(name, NHash),
                       client_ttl => TTL,
                       pointers   => Pointers,
                       name_ttl   => NameTTL,
                       fee        => Fee},
    {ok, 200, #{<<"tx">> := UpdateEnc}} = get_name_update(NameUpdateData),
    UpdateTxHash = sign_and_post_tx(UpdateEnc, PrivKey),

    %% Mine a block and check mempool empty again
    {ok,_BS3} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [UpdateTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := Balance3}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance3, Balance2 - Fee),

    %% Check that TTL and pointers got updated in name entry
    {ok, 200, #{<<"height">> := Height31}} = get_key_blocks_current_sut(),
    ExpectedTTL2 = (Height31 - 1) + NameTTL,
    {ok, 200, #{<<"ttl">>      := ExpectedTTL2,
                <<"pointers">> := Pointers}} = get_names_entry_by_name_sut(Name),

    {ok, 200, #{<<"tx">> := EncodedSpendTx}} =
        get_spend(#{recipient_id => EncodedNHash, amount => 77, fee => Fee,
                    payload => <<"foo">>, sender_id => PubKeyEnc}),
    SpendTxHash = sign_and_post_tx(EncodedSpendTx, PrivKey),

    {ok,_BS4} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [SpendTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Only fee is lost as recipient = sender
    %% This tests 'resolve_name' because recipient is expressed by name label
    {ok, 200, #{<<"balance">> := Balance4}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance4, Balance3 - Fee),

    %% Submit name transfer tx and check it is in mempool
    TransferData = #{account_id   => PubKeyEnc,
                     recipient_id => PubKeyEnc,
                     name_id      => aehttp_api_encoder:encode(name, NHash),
                     fee          => Fee},
    {ok, 200, #{<<"tx">> := TransferEnc}} = get_name_transfer(TransferData),
    TransferTxHash = sign_and_post_tx(TransferEnc, PrivKey),

    %% Mine a block and check mempool empty again
    {ok,_BS5} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TransferTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check balance
    {ok, 200, #{<<"balance">> := Balance5}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance5, Balance4 - Fee),

    %% Submit name revoke tx and check it is in mempool
    RevokeData = #{account_id => PubKeyEnc,
                   name_id => aehttp_api_encoder:encode(name, NHash),
                   fee => Fee},
    {ok, 200, #{<<"tx">> := RevokeEnc}} = get_name_revoke(RevokeData),
    RevokeTxHash = sign_and_post_tx(RevokeEnc, PrivKey),

    %% Mine a block and check mempool empty again
    {ok,_BS6} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [RevokeTxHash], 10),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check balance
    {ok, 200, #{<<"balance">> := Balance6}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance6, Balance5 - Fee),

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
        get_name_preclaim(#{commitment_id => aehttp_api_encoder:encode(commitment, CHash),
                            fee => Fee,
                            account_id => aehttp_api_encoder:encode(account_pubkey, random_hash())}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_claim(#{name => aehttp_api_encoder:encode(name, Name),
                         name_salt => NameSalt,
                         account_id => aehttp_api_encoder:encode(account_pubkey, random_hash()),
                         fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_update(#{account_id => aehttp_api_encoder:encode(account_pubkey, random_hash()),
                          name_id => aehttp_api_encoder:encode(name, NHash),
                          name_ttl => 5,
                          pointers => [],
                          client_ttl => 5,
                          fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_transfer(#{account_id => aehttp_api_encoder:encode(account_pubkey, random_hash()),
                            recipient_id => aehttp_api_encoder:encode(account_pubkey, random_hash()),
                            name_id => aehttp_api_encoder:encode(name, NHash),
                            fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_revoke(#{account_id => aehttp_api_encoder:encode(account_pubkey, random_hash()),
                          name_id => aehttp_api_encoder:encode(name, NHash),
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

%%
%% Channels
%%
assert_balance(Pubkey, ExpectedBalance) ->
    Address = aehttp_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := ExpectedBalance}} =
        get_accounts_by_pubkey_sut(Address).

channel_sign_tx(ConnPid, Privkey, Tag, Config) ->
    {ok, Tag, #{<<"tx">> := EncCreateTx}} = wait_for_channel_event(ConnPid, sign, Config),
    {ok, CreateBinTx} = aehttp_api_encoder:safe_decode(transaction, EncCreateTx),
    Tx = aetx:deserialize_from_binary(CreateBinTx),
    SignedCreateTx = aec_test_utils:sign_tx(Tx, Privkey),
    EncSignedCreateTx = aehttp_api_encoder:encode(transaction,
                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
    ws_send(ConnPid, Tag,  #{tx => EncSignedCreateTx}, Config),
    Tx.

sc_ws_open(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    {ok, 200, #{<<"balance">> := IStartAmt}} =
                 get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := RStartAmt}} =
                 get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, RPubkey)),
    IAmt = 70000,
    RAmt = 40000,

    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt, #{}, Config),
    {ok, IConnPid} = channel_ws_start(initiator,
                                           maps:put(host, <<"localhost">>, ChannelOpts), Config),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info, get, sign, on_chain_tx]),

    {ok, RConnPid} = channel_ws_start(responder, ChannelOpts, Config),

    ok = ?WS:register_test_for_channel_events(RConnPid, [info, get, sign, on_chain_tx]),

    channel_send_conn_open_infos(RConnPid, IConnPid, Config),

    ChannelCreateFee = channel_create(Config, IConnPid, RConnPid),
    {ok, {IBal, RBal}} = sc_ws_get_both_balances(IConnPid,
                                                 IPubkey,
                                                 RPubkey,
                                                 Config),
    %% assert off-chain balances
    PushAmt = maps:get(push_amount, ChannelOpts),
    IBal = IAmt - PushAmt,
    RBal = RAmt + PushAmt,

    %% ensure new balances
    assert_balance(IPubkey, IStartAmt - IAmt - ChannelCreateFee),
    assert_balance(RPubkey, RStartAmt - RAmt),

    % mine min depth
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 4),

    channel_send_locking_infos(IConnPid, RConnPid, Config),

    channel_send_chan_open_infos(RConnPid, IConnPid, Config),

    ChannelClients = #{initiator => IConnPid,
                       responder => RConnPid},
    {save_config, [{channel_clients, ChannelClients},
                   {channel_options, ChannelOpts} | Config]}.


channel_send_conn_open_infos(RConnPid, IConnPid, Config) ->
    {ok, #{<<"event">> := <<"channel_open">>}} = wait_for_channel_event(RConnPid, info, Config),
    {ok, #{<<"event">> := <<"channel_accept">>}} = wait_for_channel_event(IConnPid, info, Config).

channel_send_locking_infos(IConnPid, RConnPid, Config) ->
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = wait_for_channel_event(IConnPid, info, Config),
    ?WS:log(IConnPid, info, "Funding has been confirmed locally on-chain"),
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = wait_for_channel_event(RConnPid, info, Config),
    ?WS:log(RConnPid, info, "Funding has been confirmed locally on-chain"),

    {ok, #{<<"event">> := <<"funding_locked">>}} = wait_for_channel_event(IConnPid, info, Config),
    ?WS:log(IConnPid, info, "Funding has been confirmed on-chain by other party"),
    {ok, #{<<"event">> := <<"funding_locked">>}} = wait_for_channel_event(RConnPid, info, Config),
    ?WS:log(RConnPid, info, "Funding has been confirmed on-chain by other party").

channel_send_chan_open_infos(RConnPid, IConnPid, Config) ->
    {ok, #{<<"event">> := <<"open">>}} = wait_for_channel_event(IConnPid, info, Config),
    ?WS:log(IConnPid, info, "Channel is `open` and ready to use"),
    {ok, #{<<"event">> := <<"open">>}} = wait_for_channel_event(RConnPid, info, Config),
    ?WS:log(RConnPid, info, "Channel is `open` and ready to use").

channel_participants_balances(IPubkey, RPubkey) ->
    {ok, 200, #{<<"balance">> := BalI}} =
        get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := BalR}} =
        get_accounts_by_pubkey_sut(aehttp_api_encoder:encode(account_pubkey, RPubkey)),
    {BalI, BalR}.

channel_create(Config, IConnPid, RConnPid) ->
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants, Config),
    %% initiator gets to sign a create_tx
    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    CrTx = channel_sign_tx(IConnPid, IPrivkey, <<"initiator_sign">>, Config),
    {ok, #{<<"event">> := <<"funding_created">>}} = wait_for_channel_event(RConnPid, info, Config),
    %% responder gets to sign a create_tx
    CrTx = channel_sign_tx(RConnPid, RPrivkey, <<"responder_sign">>, Config),
    {ok, #{<<"event">> := <<"funding_signed">>}} = wait_for_channel_event(IConnPid, info, Config),

    %% both of them receive the same co-signed channel_create_tx
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    {ok, SSignedCrTx} = aehttp_api_encoder:safe_decode(transaction, EncodedSignedCrTx),
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
            channel_update(Conns, Sender, Participants, 1, Round, Config),
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
            LogPid = maps:get(Sender, Conns),
            ?WS:log(LogPid, info, "Failing update, insufficient balance"),
            {ok, #{<<"reason">> := <<"insufficient_balance">>,
                  <<"request">> := _Request0}} = channel_update_fail(
                                                   Conns, Sender,
                                                   Participants, 10000000, Config),
            ?WS:log(LogPid, info, "Failing update, negative amount"),
            {ok, #{<<"reason">> := <<"negative_amount">>,
                  <<"request">> := _Request1}} = channel_update_fail(
                                                   Conns, Sender,
                                                   Participants, -1, Config),
            ?WS:log(LogPid, info, "Failing update, invalid pubkeys"),
            {ok, #{<<"reason">> := <<"invalid_pubkeys">>,
                  <<"request">> := _Request2}} =
                channel_update_fail(Conns, Sender,
                                    #{initiator => #{pub_key => <<42:32/unit:8>>},
                                      responder => #{pub_key => <<43:32/unit:8>>}},
                                    1, Config),
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
                SenderEncodedK = aehttp_api_encoder:encode(account_pubkey, SenderPubkey),
                ReceiverEncodedK = aehttp_api_encoder:encode(account_pubkey, ReceiverPubkey),
                ok = ?WS:register_test_for_channel_event(ReceiverPid, message),

                ws_send(SenderPid, <<"message">>,
                        #{<<"to">> => ReceiverEncodedK,
                          <<"info">> => Msg}, Config),

                {ok, #{<<"message">> := #{<<"from">> := SenderEncodedK,
                                          <<"to">> := ReceiverEncodedK,
                                          <<"info">> := Msg}}}
                    = wait_for_channel_event(ReceiverPid, message, Config),
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
                channel_conflict(Conns, FirstSender, Participants, 1, 2, Config)
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
               Amount1, Amount2, Config) ->
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
            case wait_for_channel_event(ConnPid, sign, Config) of
                {ok, <<"update_ack">>, _} -> %% this is not the message we are looking for
                    TrySignUpdate(ConnPid, Privkey);
                {ok, <<"update">>, #{<<"tx">> := EncCreateTx}} ->
                    {ok, CreateBinTx} = aehttp_api_encoder:safe_decode(transaction, EncCreateTx),
                    Tx = aetx:deserialize_from_binary(CreateBinTx),
                    SignedCreateTx = aec_test_utils:sign_tx(Tx, Privkey),
                    EncSignedCreateTx = aehttp_api_encoder:encode(transaction,
                                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
                    ws_send(ConnPid, <<"update">>, #{tx => EncSignedCreateTx}, Config)
            end
        end,
    %% sender initiates an update
    ws_send_tagged(StarterPid, <<"update">>, <<"new">>,
                   #{from => aehttp_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aehttp_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount1}, Config),

    %% starter signs the new state

    %% acknowledger initiates an update too
    ws_send_tagged(AcknowledgerPid, <<"update">>, <<"new">>,
                   #{from => aehttp_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aehttp_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount2}, Config),

    SignUpdate(StarterPid, StarterPrivkey),
    SignUpdate(AcknowledgerPid, AcknowledgerPrivkey),

    {ok, _} = wait_for_channel_event(StarterPid, conflict, Config),
    {ok, _} = wait_for_channel_event(AcknowledgerPid, conflict, Config),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, conflict]),

    ok.

channel_update(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount,_Round,Config) ->
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
                                ConnPid, StarterPubkey, AcknowledgerPubkey, Config)
                      end,
    {ok, {Ba0, Bb0} = Bal0} = GetBothBalances(IConnPid),
    ct:log("Balances before: ~p", [Bal0]),
    ws_send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aehttp_api_encoder:encode(account_pubkey, StarterPubkey),
          to => aehttp_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount}, Config),

    %% starter signs the new state
    UnsignedStateTx = channel_sign_tx(StarterPid, StarterPrivkey, <<"update">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    %% verify contents
    {channel_offchain_tx, OffchainTx} = aetx:specialize_type(UnsignedStateTx),
    [Update] = aesc_offchain_tx:updates(OffchainTx),
    Expected = aesc_offchain_update:op_transfer(aec_id:create(account, StarterPubkey),
                                                aec_id:create(account, AcknowledgerPubkey), Amount),
    Expected = Update,


    %% acknowledger signs the new state
    {ok, #{<<"event">> := <<"update">>}} = wait_for_channel_event(AcknowledgerPid, info, Config),
    UnsignedStateTx = channel_sign_tx(AcknowledgerPid, AcknowledgerPrivkey, <<"update_ack">>, Config),

    {ok, #{<<"state">> := NewState}} = wait_for_channel_event(IConnPid, update, Config),
    {ok, #{<<"state">> := NewState}} = wait_for_channel_event(RConnPid, update, Config),
    {ok, SignedStateTxBin} = aehttp_api_encoder:safe_decode(transaction, NewState),
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
               Amount, Config) ->
    {StarterPid, _AcknowledgerPid, StarterPubkey, AcknowledgerPubkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, RPubkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, IPubkey}
        end,
    ok = ?WS:register_test_for_channel_event(StarterPid, error),

    %% sender initiates an update
    ws_send_tagged(StarterPid, <<"update">>, <<"new">>,
                   #{from => aehttp_api_encoder:encode(account_pubkey, StarterPubkey),
                     to => aehttp_api_encoder:encode(account_pubkey, AcknowledgerPubkey),
                     amount => Amount}, Config),

    {ok, _Payload}= Res = wait_for_channel_event(StarterPid, error, Config),


    ok = ?WS:unregister_test_for_channel_event(StarterPid, error),
    Res.

sc_ws_close(Config) ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),

    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),


    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_get_balance(ConnPid, PubKey, Config) ->
    Account = aehttp_api_encoder:encode(account_pubkey, PubKey),
    {ok, Res} = query_balances(ConnPid, [Account], Config),
    [#{<<"account">> := Account, <<"balance">> := B}] = Res,
    {ok, B}.

sc_ws_get_both_balances(ConnPid, PubKeyI, PubKeyR, Config) ->
    AccountI = aehttp_api_encoder:encode(account_pubkey, PubKeyI),
    AccountR = aehttp_api_encoder:encode(account_pubkey, PubKeyR),
    {ok, Res} = query_balances(ConnPid, [AccountI, AccountR], Config),
    [#{<<"account">> := AccountI, <<"balance">> := BI},
     #{<<"account">> := AccountR, <<"balance">> := BR}] = Res,
    {ok, {BI, BR}}.

query_balances(ConnPid, Accounts, Config) ->
    query_balances_(ConnPid, Accounts, sc_ws_protocol(Config)).

query_balances_(ConnPid, Accounts, <<"legacy">>) ->
    ?WS:send_tagged(ConnPid, <<"get">>, <<"balances">>,
                    #{<<"accounts">> => Accounts}),
    {ok, <<"balances">>, Res} = ?WS:wait_for_channel_event(ConnPid, get),
    {ok, Res};
query_balances_(ConnPid, Accounts, <<"json-rpc">>) ->
    {ok, ?WS:json_rpc_call(
            ConnPid, #{ <<"method">> => <<"channels.get.balances">>
                      , <<"params">> => #{<<"accounts">> => Accounts} })}.


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
                ws_send(CloserConn, <<"shutdown">>, #{}, Config),

            ShTx = channel_sign_tx(CloserConn, CloserPrivkey, <<"shutdown_sign">>, Config),
            ShTx = channel_sign_tx(OtherConn, OtherPrivkey, <<"shutdown_sign_ack">>, Config)
        end,
    ShutdownTx =
        case Closer of
            initiator -> CloseMutual(IConnPid, IPrivkey, RConnPid, RPrivkey);
            responder -> CloseMutual(RConnPid, RPrivkey, IConnPid, IPrivkey)
        end,

    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = wait_for_channel_event(IConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = wait_for_channel_event(RConnPid, on_chain_tx, Config),

    {ok, SSignedMutualTx} = aehttp_api_encoder:safe_decode(transaction, EncodedSignedMutualTx),
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

    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),
    ok = ?WS:register_test_for_channel_events(IConnPid, [leave, info]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [leave, info]),
    ok = ?WS:register_test_for_events(IConnPid, websocket, [closed]),
    ok = ?WS:register_test_for_events(RConnPid, websocket, [closed]),
    %%
    ok = ws_send(IConnPid, <<"leave">>, #{}, Config),
    %%
    {ok, #{id := IDi, state := StI}} = wait_for_channel_leave_msg(IConnPid, Config),
    {ok, #{id := IDr, state := StR}} = wait_for_channel_leave_msg(RConnPid, Config),
    {IDi, IDr} = {IDr, IDi},
    {StI, StR} = {StR, StI},
    {ok, #{<<"event">> := <<"died">>}} = wait_for_channel_event(IConnPid, info, Config),
    {ok, #{<<"event">> := <<"died">>}} = wait_for_channel_event(RConnPid, info, Config),
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
    {ok, RrConnPid} = channel_ws_start(responder, ReestablOptions, Config),
    {ok, IrConnPid} = channel_ws_start(initiator, maps:put(
                                                    host, <<"localhost">>,
                                                    ReestablOptions), Config),
    ok = ?WS:register_test_for_channel_events(
            RrConnPid, [info, update]),
    ok = ?WS:register_test_for_channel_events(
            IrConnPid, [info, update]),
    {ok, #{<<"event">> := <<"channel_reestablished">>}} =
        wait_for_channel_event(IrConnPid, info, Config),
    {ok, #{<<"event">> := <<"channel_reestablished">>}} =
        wait_for_channel_event(RrConnPid, info, Config),
    {ok, #{<<"event">> := <<"open">>}} =
        wait_for_channel_event(IrConnPid, info, Config),
    {ok, #{<<"event">> := <<"open">>}} =
        wait_for_channel_event(RrConnPid, info, Config),
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
    ws_send(SenderConnPid, <<"deposit">>, #{amount => 2}, Config),
    UnsignedStateTx = channel_sign_tx(SenderConnPid, SenderPrivkey, <<"deposit_tx">>, Config),
    {ok, #{<<"event">> := <<"deposit_created">>}} = wait_for_channel_event(AckConnPid, info, Config),
    UnsignedStateTx = channel_sign_tx(AckConnPid, AckPrivkey, <<"deposit_ack">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, #{<<"tx">> := EncodedSignedDepositTx}} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedDepositTx}} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),

    {ok, SSignedDepositTx} = aehttp_api_encoder:safe_decode(transaction,
                                                     EncodedSignedDepositTx),
    SignedDepositTx = aetx_sign:deserialize_from_binary(SSignedDepositTx),
    ok = wait_for_signed_transaction_in_block(SignedDepositTx),
    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    {SStartB1, _} = {SStartB - 2 - 20000, SStartB},

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 5),
    {ok, #{<<"event">> := <<"own_deposit_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"own_deposit_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    {ok, #{<<"event">> := <<"deposit_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"deposit_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

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
    ws_send(SenderConnPid, <<"withdraw">>, #{amount => 2}, Config),
    UnsignedStateTx = channel_sign_tx(SenderConnPid, SenderPrivkey, <<"withdraw_tx">>, Config),
    {ok, #{<<"event">> := <<"withdraw_created">>}} = wait_for_channel_event(AckConnPid, info, Config),
    UnsignedStateTx = channel_sign_tx(AckConnPid, AckPrivkey, <<"withdraw_ack">>, Config),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    {ok, #{<<"tx">> := EncodedSignedWTx}} = wait_for_channel_event(SenderConnPid, on_chain_tx, Config),
    {ok, #{<<"tx">> := EncodedSignedWTx}} = wait_for_channel_event(AckConnPid, on_chain_tx, Config),

    {ok, SSignedWTx} = aehttp_api_encoder:safe_decode(transaction, EncodedSignedWTx),
    SignedWTx = aetx_sign:deserialize_from_binary(SSignedWTx),
    ok = wait_for_signed_transaction_in_block(SignedWTx),
    % assert acknowledger balance have not changed
    {SStartB1, AStartB} = channel_participants_balances(SenderPubkey, AckPubkey),
    {SStartB1, _} = {SStartB + 2 - 20000, SStartB},

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 5),
    {ok, #{<<"event">> := <<"own_withdraw_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"own_withdraw_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),

    {ok, #{<<"event">> := <<"withdraw_locked">>}} = wait_for_channel_event(SenderConnPid, info, Config),
    {ok, #{<<"event">> := <<"withdraw_locked">>}} = wait_for_channel_event(AckConnPid, info, Config),


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

sc_ws_oracle_contract(Config) ->

    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, get, error]),

    [sc_ws_contract_generic(Role, fun sc_ws_oracle_contract_/8, Config, [])
        || Role <- [initiator, responder]],

    % cleanup
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_nameservice_contract(Config) ->

    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, get, error]),


    [sc_ws_contract_generic(Role, fun sc_ws_nameservice_contract_/8, Config,
                            [])
        || Role <- [initiator,
                    responder]],

    % cleanup
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_enviroment_contract(Config) ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, get, error]),


    [sc_ws_contract_generic(Role, fun sc_ws_enviroment_contract_/8, Config,
                            [])
        || Role <- [initiator,
                    responder]],

    % cleanup
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_remote_call_contract(Config) ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, get, error]),


    [sc_ws_contract_generic(Role, fun sc_ws_remote_call_contract_/8, Config,
                            [])
        || Role <- [initiator,
                    responder]],

    % cleanup
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_remote_call_contract_refering_onchain_data(Config) ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := IConnPid, responder := RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, get, error]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, get, error]),


    [sc_ws_contract_generic(Role, fun sc_ws_remote_call_contract_refering_onchain_data_/8, Config,
                            [])
        || Role <- [initiator,
                    responder]],

    % cleanup
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

random_unused_name() ->
    random_unused_name(_Attempts = 10).

random_unused_name(Attempts) when Attempts < 1->
    {error, exhausted};
random_unused_name(Attempts) ->
    Size = 10,
    RandStr = base58:binary_to_base58(crypto:strong_rand_bytes(Size)),
    NameL = RandStr ++ ".test",
    Name = list_to_binary(NameL),
    case get_names_entry_by_name_sut(Name) of
        {ok, 404, _Error} -> Name; % name not used yet
        _ -> random_unused_name(Attempts - 1)
    end.

sc_ws_contract_generic(Origin, Fun, Config, Opts) ->
    %% get the infrastructure for users going
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
    SenderConnPid = maps:get(SenderRole, Clients),
    AckConnPid = maps:get(AckRole, Clients),
    %% helper lambda for update
    GetVolley =
        fun(Actor) ->
            case Actor =:= Origin of
                true ->
                    {fun() -> update_volley_(SenderConnPid, SenderPrivkey, AckConnPid, AckPrivkey, Config) end,
                     SenderConnPid, SenderPubkey};
                false ->
                    {fun() -> update_volley_(AckConnPid, AckPrivkey, SenderConnPid, SenderPrivkey, Config) end,
                     AckConnPid, AckPubkey}
            end
        end,

    GetPubkeys =
        fun(Role) ->
            case Origin =:= Role of
                true  -> {SenderPubkey, AckPubkey};
                false -> {AckPubkey, SenderPubkey}
            end
        end,
    Actors = [{R, GetPubkeys(R)} || R <- [initiator, responder]],
    [Fun(Owner, GetVolley, SenderConnPid,
         AckConnPid, OwnerPubkey, OtherPubkey, Opts, Config)
        || {Owner, {OwnerPubkey, OtherPubkey}} <- Actors],
    ok.

sc_ws_oracle_contract_(Owner, GetVolley, ConnPid1, ConnPid2,
                       OwnerPubkey, OtherPubkey, _Opts, Config) ->
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {OraclePubkey, OraclePrivkey} = initialize_account(2000000),
    SophiaStringType = aeso_heap:to_binary(string, 0),
    QueryFee = 3,
    QueryTTL = 30,
    ResponseTTL = 30,
    Question = <<"Fill me in with something reasonable">>,
    register_oracle(OraclePubkey, OraclePrivkey,
                    #{query_format    => SophiaStringType,
                      response_format => SophiaStringType,
                      query_fee       => QueryFee,
                      query_ttl       => QueryTTL,
                      vm_version      => ?CURRENT_AEVM_SOPHIA
                     }),
    OracleQuerySequence =
        fun(Q0, R0) ->
            Q = aeso_heap:to_binary(Q0, 0),
            R = aeso_heap:to_binary(R0, 0),
            QueryId = query_oracle(OraclePubkey, OraclePrivkey, %oracle asks oracle
                                  OraclePubkey,
                                  #{query        => Q,
                                    response_ttl => {delta, ResponseTTL}}),
            respond_oracle(OraclePubkey, OraclePrivkey, QueryId,
                          R, #{response_ttl => {delta, ResponseTTL}}),
            QueryId
        end,

    EncodedCode = contract_byte_code("channel_on_chain_contract_oracle"),
    HexOracleId = aeu_hex:hexstring_encode(OraclePubkey),
    InitArgument = <<"(",HexOracleId/binary, ", \"", Question/binary, "\")">>,
    {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                              <<"sophia">>,
                              contract_bytearray_decode(EncodedCode),
                              <<"init">>, InitArgument),
    {CreateVolley, OwnerConnPid, OwnerPubKey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"update">>, <<"new_contract">>,
                   #{vm_version => ?CURRENT_AEVM_SOPHIA,
                     deposit    => 10,
                     code       => EncodedCode,
                     call_data  => EncodedInitData}, Config),

    UnsignedStateTx = CreateVolley(),
    ContractPubKey = contract_id_from_create_update(OwnerPubKey,
                                                    UnsignedStateTx),

    CallContract =
        fun(Who, Fun, Args, ReturnType, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            Tx = call_a_contract(Fun, Args,
                                 ContractPubKey, EncodedCode,
                                 UpdaterConnPid, UpdateVolley, Config),
            #{<<"value">> := R} =
                ws_get_decoded_result(ConnPid1, ConnPid2,
                                      ReturnType,
                                      Tx, Config),
            {R, R} = {Result, R}

        end,
    [CallContract(Who, <<"place_bet">>, <<"\"", Bet/binary, "\"">>,
                  <<"string">>, <<"ok">>)
        || {Who, Bet} <- [{initiator, <<"I win">>},
                          {responder, <<"no, I win">>}]],

    %% initiator places a bet and then nobody can overwrite it
    ParkedAnswer = <<"I claim this">>,
    CallContract(initiator, <<"place_bet">>, <<"\"", ParkedAnswer/binary, "\"">>,
                  <<"string">>, <<"ok">>),
    [CallContract(Who, <<"place_bet">>, <<"\"", ParkedAnswer/binary, "\"">>,
                  <<"string">>, <<"bet_already_taken">>)
        || Who <- [initiator, responder]],

    %% place some oracle query id with a different question
    ErrQueryId = OracleQuerySequence(<<"other question">>, <<"some answer">>),
    EncodedErrQuery = aeu_hex:hexstring_encode(ErrQueryId),
    [CallContract(Who, <<"resolve">>, <<"(", EncodedErrQuery/binary, ")">>,
                  <<"string">>, <<"different question">>)
        || Who <- [initiator, responder]],

    Answer = <<"other reasonable thingy">>,
    CorrectQueryId = OracleQuerySequence(Question, Answer),
    EncodedQueryId = aeu_hex:hexstring_encode(CorrectQueryId),

    {ok, {OwnerBal0, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    CallContract(Owner, <<"resolve">>, <<"(", EncodedQueryId/binary, ")">>,
                 <<"string">>, <<"no winning bet">>),

    % no changes in balances
    {ok, {OwnerBal0, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),

    % owner posts the correct
    CallContract(Owner, <<"place_bet">>, <<"\"", Answer/binary, "\"">>,
                  <<"string">>, <<"ok">>),
    {ok, {OwnerBal, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                          OwnerPubkey,
                                                          OtherPubkey,
                                                          Config),
    {ok, ContractBalance} = sc_ws_get_balance(ConnPid1, ContractPubKey, Config),
    CallContract(Owner, <<"resolve">>, <<"(", EncodedQueryId/binary, ")">>,
                 <<"string">>, <<"ok">>),
    % contract balance is 0 now
    {ok, 0} = sc_ws_get_balance(ConnPid1, ContractPubKey, Config),
    {ok, 0} = sc_ws_get_balance(ConnPid2, ContractPubKey, Config),

    % contract owner balance is updated
    {ok, {OwnerBal1, OtherBal0}} = sc_ws_get_both_balances(ConnPid1,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    {ok, {OwnerBal1, OtherBal0}} = sc_ws_get_both_balances(ConnPid2,
                                                           OwnerPubkey,
                                                           OtherPubkey,
                                                           Config),
    {OwnerBal1, _} = {OwnerBal + ContractBalance, OwnerBal1},
    ok.

sc_ws_nameservice_contract_(Owner, GetVolley, ConnPid1, ConnPid2,
                            OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    Name = random_unused_name(),
    %% Register an oracle. It will be used in an off-chain contract
    %% Oracle ask itself a question and answers it
    {NamePubkey, NamePrivkey} = initialize_account(2000000),

    EncodedCode = contract_byte_code("channel_on_chain_contract_name_resolution"),
    InitArgument = <<"()">>,
    {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                              <<"sophia">>,
                              contract_bytearray_decode(EncodedCode),
                              <<"init">>, InitArgument),
    {CreateVolley, OwnerConnPid, OwnerPubkey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"update">>, <<"new_contract">>,
                   #{vm_version => ?CURRENT_AEVM_SOPHIA,
                     deposit    => 10,
                     code       => EncodedCode,
                     call_data  => EncodedInitData}, Config),

    UnsignedStateTx = CreateVolley(),
    ContractPubKey = contract_id_from_create_update(OwnerPubkey,
                                                    UnsignedStateTx),

    ContractCanNameResolve =
        fun(Who, Name0, Key0, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name0),
            QKey = AddQuotes(Key0),
            Args = <<"(", QName/binary, ",", QKey/binary,")">>,
            Tx = call_a_contract(<<"can_resolve">>,
                                 Args,
                                 ContractPubKey, EncodedCode,
                                 UpdaterConnPid, UpdateVolley, Config),
             #{<<"value">> := RInt} =
                ws_get_decoded_result(ConnPid1, ConnPid2,
                                      <<"bool">>,
                                      Tx, Config),
            R =
                case RInt of
                    0 -> false;
                    1 -> true
                end,
            {R, R} = {Result, R}

        end,
    Test =
        fun(Name1, Key, Result) ->
            [ContractCanNameResolve(Who, Name1, Key, Result)
                || Who <- [initiator, responder]]
        end,

    Test(Name, <<"oracle">>, false),
    register_name(NamePubkey, NamePrivkey, Name,
                  [{<<"account_pubkey">>, aec_id:create(account, <<1:256>>)},
                   {<<"oracle">>, aec_id:create(oracle, <<2:256>>)},
                   {<<"unexpected_key">>, aec_id:create(account, <<3:256>>)}]),
    Test(Name, <<"account_pubkey">>, true),
    Test(Name, <<"oracle">>, true),
    Test(Name, <<"unexpected_key">>, true),
    Test(Name, <<"missing_key">>, false),
    ok.

sc_ws_enviroment_contract_(Owner, GetVolley, ConnPid1, ConnPid2,
                           OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    EncodedCode = contract_byte_code("channel_env"),
    InitArgument = <<"()">>,
    {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                              <<"sophia">>,
                              contract_bytearray_decode(EncodedCode),
                              <<"init">>, InitArgument),
    {CreateVolley, OwnerConnPid, OwnerPubkey} = GetVolley(Owner),
    ws_send_tagged(OwnerConnPid, <<"update">>, <<"new_contract">>,
                   #{vm_version => ?CURRENT_AEVM_SOPHIA,
                     deposit    => 10,
                     code       => EncodedCode,
                     call_data  => EncodedInitData}, Config),

    UnsignedStateTx = CreateVolley(),
    ContractPubKey = contract_id_from_create_update(OwnerPubkey,
                                                    UnsignedStateTx),

    ContractCall =
        fun(Who, Fun, ResultType, Result) ->
            {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
            Args = <<"()">>,
            Tx = call_a_contract(Fun,
                                 Args,
                                 ContractPubKey, EncodedCode,
                                 UpdaterConnPid, UpdateVolley, Config),
             #{<<"value">> := R} =
                ws_get_decoded_result(ConnPid1, ConnPid2,
                                      ResultType,
                                      Tx, Config),
            case is_function(Result) of
                true -> true = Result(R);
                false ->
                    {R, R} = {Result, R}
            end
        end,
    Test =
        fun(Fun, ResultType, Result) ->
            [ContractCall(Who, Fun, ResultType, Result)
                || Who <- [initiator, responder]]
        end,
    {ok, 200, Block} = get_key_blocks_current_sut(),
    #{<<"height">> := BlockHeight,
      <<"beneficiary">> := EncBeneficiary,
      <<"time">> := Time
     } = Block,
    {ok, Beneficiary} = aehttp_api_encoder:safe_decode(account_pubkey,
                                                EncBeneficiary),
    Test(<<"block_height">>, <<"int">>, BlockHeight),
    Test(<<"coinbase">>, <<"int">>, fun(I) -> <<I:32/unit:8>> =:= Beneficiary end),
    Test(<<"timestamp">>, <<"int">>, fun(T) -> T > Time end),
    ok.


sc_ws_remote_call_contract_(Owner, GetVolley, ConnPid1, ConnPid2,
                           OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% create identity contract off-chain
    CreateContract =
        fun(Name) ->
            EncodedCode = contract_byte_code(Name),
            InitArgument = <<"()">>,
            {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                                      <<"sophia">>,
                                      contract_bytearray_decode(EncodedCode),
                                      <<"init">>,
                                      InitArgument),
            {CreateVolley, OwnerConnPid, OwnerPubkey} = GetVolley(Owner),
            ws_send_tagged(OwnerConnPid, <<"update">>, <<"new_contract">>,
                           #{vm_version => ?CURRENT_AEVM_SOPHIA,
                             deposit    => 10,
                             code       => EncodedCode,
                             call_data  => EncodedInitData}, Config),

            UnsignedStateTx = CreateVolley(),
            ContractPubKey = contract_id_from_create_update(OwnerPubkey,
                                                            UnsignedStateTx),
            {ContractPubKey, EncodedCode}
          end,
    {IdentityCPubKey, IdentityCode} = CreateContract("identity"),
    {RemoteCallCPubKey, RemoteCallCode} = CreateContract("remote_call"),

    ContractCall =
        fun(Who, ContractPubKey, Code, Fun, Args, Result, Amount) ->
                {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
                Tx = call_a_contract(Fun,
                                     Args,
                                     ContractPubKey, Code,
                                     UpdaterConnPid, UpdateVolley, Amount, Config),
                #{<<"value">> := R} =
                    ws_get_decoded_result(ConnPid1, ConnPid2, <<"int">>, Tx, Config),
                {R, R} = {Result, R}
        end,
    CallIdentity =
        fun(Who, Val) ->
            ValB = integer_to_binary(Val),
            ContractCall(Who, IdentityCPubKey, IdentityCode, <<"main">>,
                         <<"(", ValB/binary, ")">>, Val, _Amount = 0)
        end,
    HexEncodedIdentityPubkey = aeu_hex:hexstring_encode(IdentityCPubKey),
    CallRemoteContract =
        fun(Who, Val) ->
            ValB = integer_to_binary(Val),
            ContractCall(Who, RemoteCallCPubKey, RemoteCallCode, <<"call">>,
                         <<"(", HexEncodedIdentityPubkey/binary, ", ",
                           ValB/binary, ")">>, Val,
                         % beacuse of hardcoded value=10 in the
                         % remote_call.aes -> amount in the call must be > 10
                         _Amount = 20)
        end,
    Test =
        fun(Fun, Val) ->
            [Fun(Who, Val)
                || Who <- [initiator, responder]]
        end,

    % actual tests
    Test(CallIdentity, 10),
    Test(CallIdentity, 11),
    Test(CallRemoteContract, 42),
    Test(CallRemoteContract, 43),
    Test(CallIdentity, 12),
    ok.

sc_ws_remote_call_contract_refering_onchain_data_(Owner, GetVolley, ConnPid1, ConnPid2,
                           OwnerPubkey, _OtherPubkey, _Opts, Config) ->
    %% create identity contract off-chain
    CreateContract =
        fun(Name) ->
            EncodedCode = contract_byte_code(Name),
            InitArgument = <<"()">>,
            {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                                      <<"sophia">>,
                                      contract_bytearray_decode(EncodedCode),
                                      <<"init">>,
                                      InitArgument),
            {CreateVolley, OwnerConnPid, OwnerPubkey} = GetVolley(Owner),
            ws_send_tagged(OwnerConnPid, <<"update">>, <<"new_contract">>,
                           #{vm_version => ?CURRENT_AEVM_SOPHIA,
                             deposit    => 10,
                             code       => EncodedCode,
                             call_data  => EncodedInitData}, Config),

            UnsignedStateTx = CreateVolley(),
            ContractPubKey = contract_id_from_create_update(OwnerPubkey,
                                                            UnsignedStateTx),
            {ContractPubKey, EncodedCode}
          end,
    {ResolverCPubKey, ResolverCode} = CreateContract("channel_on_chain_contract_name_resolution"),
    {RemoteCallCPubKey, RemoteCallCode} = CreateContract("channel_remote_on_chain_contract_name_resolution"),

    ContractCall =
        fun(Who, ContractPubKey, Code, Fun, Args, Result, Amount) ->
                {UpdateVolley, UpdaterConnPid, _UpdaterPubKey} = GetVolley(Who),
                Tx = call_a_contract(Fun,
                                     Args,
                                     ContractPubKey, Code,
                                     UpdaterConnPid, UpdateVolley, Amount, Config),
                #{<<"value">> := RInt} =
                    ws_get_decoded_result(ConnPid1, ConnPid2, <<"int">>, Tx, Config),
                R =
                    case RInt of
                        0 -> false;
                        1 -> true
                    end,
                {R, R} = {Result, R}
        end,
    CallResolve =
        fun(Who, Name, Key, IsResolvable) ->
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name),
            QKey = AddQuotes(Key),
            Args = <<"(", QName/binary, ",", QKey/binary,")">>,
            ContractCall(Who, ResolverCPubKey, ResolverCode, <<"can_resolve">>,
                         Args, IsResolvable, _Amount = 0)
        end,
    HexEncodedResolverPubkey = aeu_hex:hexstring_encode(ResolverCPubKey),
    CallRemoteContract =
        fun(Who, Name, Key, IsResolvable) ->
            AddQuotes = fun(B) when is_binary(B) -> <<"\"", B/binary, "\"">> end,
            QName = AddQuotes(Name),
            QKey = AddQuotes(Key),
            Args = <<"(", HexEncodedResolverPubkey/binary, ", ",
                     QName/binary, ",", QKey/binary,")">>,
            ContractCall(Who, RemoteCallCPubKey, RemoteCallCode, <<"remote_resolve">>,
                         Args, IsResolvable,
                         % beacuse of hardcoded value=10 in the
                         % remote_call.aes -> amount in the call must be > 10
                         _Amount = 20)
        end,
    Test =
        fun(Fun, N, K, Res) ->
            [Fun(Who, N, K, Res)
                || Who <- [initiator, responder]]
        end,

    % actual tests
    % we have two contracts: c
    % * channel_on_chain_contract_name_resolution.aes that has
    %     `can_resolve(Name, Key)` function. It resolves on-chain names
    % * channel_remote_on_chain_contract_name_res.aes that has 
    %     `remote_resolve(Contract, Name, Key)` function that makes a remote
    %     call to the first contract and uses it to resolve the name on-chain
    % both functions shall return the same result
    Name = random_unused_name(),

    % name is not present on-chain, both contracts shall return false:
    Test(CallResolve, Name, <<"account_pubkey">>, false),
    Test(CallRemoteContract, Name, <<"account_pubkey">>, false),

    % registering the name on-chain
    {NamePubkey, NamePrivkey} = initialize_account(2000000),
    register_name(NamePubkey, NamePrivkey, Name,
                  [{<<"account_pubkey">>, aec_id:create(account, <<1:256>>)}]),

    % now the name is on-chain, both must return true:
    Test(CallResolve, Name, <<"account_pubkey">>, true),
    Test(CallRemoteContract, Name, <<"account_pubkey">>, false), % BUG
    ok.


register_oracle(OraclePubkey, OraclePrivkey, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:register_tx(OraclePubkey, Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, OraclePrivkey),
    OracleId = aehttp_api_encoder:encode(oracle_pubkey, OraclePubkey),
    {ok, 200, _Resp} = get_oracles_by_pubkey_sut(OracleId),
    ok.

query_oracle(FromPubkey, FromPrivkey, OraclePubkey, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [FromPubkey]),
    Tx = aeo_test_utils:query_tx(FromPubkey, aec_id:create(oracle, OraclePubkey),
                                 Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, FromPrivkey),
    {aeo_query_tx, QueryTx} = aetx:specialize_callback(Tx),
    aeo_query_tx:query_id(QueryTx).

respond_oracle(OraclePubkey, OraclePrivkey, QueryId, Response, Opts) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OraclePubkey]),
    Tx = aeo_test_utils:response_tx(OraclePubkey, QueryId,
                                    Response, Opts#{nonce => Nonce}, #{}),
    sign_post_mine(Tx, OraclePrivkey),
    ok.

sign_post_mine(Tx, Privkey) ->
    SignedTx = aec_test_utils:sign_tx(Tx, Privkey),
    TxHash = aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx =
        aehttp_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    ok = post_tx(TxHash, EncodedSerializedSignedTx),
    ok = wait_for_tx_hash_on_chain(TxHash).

register_name(Owner, OwnerPrivKey, Name, Pointers) ->
    Salt = rand:uniform(10000),
    preclaim_name(Owner, OwnerPrivKey, Name, Salt),
    claim_name(Owner, OwnerPrivKey, Name, Salt),
    update_pointers(Owner, OwnerPrivKey, Name, Pointers),
    ok.

preclaim_name(Owner, OwnerPrivKey, Name, Salt) ->
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    CHash = aens_hash:commitment_hash(NameAscii, Salt),
    TxSpec = aens_test_utils:preclaim_tx_spec(Owner, CHash, #{nonce => Nonce}, #{}),
    {ok, Tx} = aens_preclaim_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

claim_name(Owner, OwnerPrivKey, Name, Salt) ->
    Delta = aec_governance:name_claim_preclaim_delta(),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:mine_key_blocks(Node, Delta),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    TxSpec = aens_test_utils:claim_tx_spec(Owner, Name, Salt,  #{nonce => Nonce},#{}),
    {ok, Tx} = aens_claim_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.

update_pointers(Owner, OwnerPrivKey, Name, Pointers0) ->
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [Owner]),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    NHash = aens_hash:name_hash(NameAscii),
    Pointers =
        lists:map(
            fun({PointerName, Value}) ->
                aens_pointer:new(PointerName, Value)
            end,
            Pointers0),
    NameTTL  = 40000,
    TxSpec = aens_test_utils:update_tx_spec(
                Owner, NHash, #{pointers => Pointers,
                                name_ttl => NameTTL,
                                nonce => Nonce}, #{}),
    {ok, Tx} = aens_update_tx:new(TxSpec),
    sign_post_mine(Tx, OwnerPrivKey),
    ok.


initialize_account(Amount) ->
    {Pubkey, Privkey} = generate_key_pair(),
    Fee = 20000,
    BlocksToMine = 3,

    Node = aecore_suite_utils:node_name(?NODE),

    aecore_suite_utils:mine_key_blocks(Node, BlocksToMine),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aehttp_api_encoder:encode(account_pubkey, Pubkey), Amount, Fee),
    sign_and_post_tx(SpendTx),
    {ok, [_KeyBlock, MicroBlock]} = aecore_suite_utils:mine_blocks(Node, 2),
    [_Spend1] = aec_blocks:txs(MicroBlock),
    assert_balance(Pubkey, Amount),
    {Pubkey, Privkey}.

update_volley_(FirstConnPid, FirstPrivkey, SecondConnPid, SecondPrivkey, Config) ->
    UnsignedStateTx = channel_sign_tx(FirstConnPid, FirstPrivkey, <<"update">>, Config),

    % acknowledger signs update_ack
    {ok, #{<<"event">> := <<"update">>}} = wait_for_channel_event(SecondConnPid, info, Config),
    UnsignedStateTx = channel_sign_tx(SecondConnPid, SecondPrivkey,
                                      <<"update_ack">>, Config).

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
    UpdateVolley =
        fun() ->
            update_volley_(SenderConnPid, SenderPrivkey, AckConnPid, AckPrivkey, Config)
        end,
    UpdateVolleyReverse =
        fun() ->
            update_volley_(AckConnPid, AckPrivkey, SenderConnPid, SenderPrivkey, Config)
        end,

    % trigger new contract
    {UnsignedStateTx, Code} = create_contract_(TestName, SenderConnPid, UpdateVolley, Config),

    ContractPubKey = contract_id_from_create_update(SenderPubkey,
                                                    UnsignedStateTx),

    %% helper lambdas for pruning and call not found
    PruneCalls =
        fun(ConnPid) ->
            ok = ?WS:register_test_for_channel_events(ConnPid, [calls_pruned]),
            ws_send(ConnPid, <<"clean_contract_calls">>, #{}, Config),
            {ok, _} = wait_for_channel_event(ConnPid, calls_pruned, Config),
            ok = ?WS:unregister_test_for_channel_events(ConnPid, [calls_pruned])
        end,
    CallMissingCall =
        fun(UnsignedStateTx00, ConnPid) ->
            ws_send_tagged(ConnPid, <<"get">>, <<"contract_call">>,
                           ws_get_call_params(UnsignedStateTx00), Config),
            {ok, #{<<"reason">> := <<"call_not_found">>}} = wait_for_channel_event(ConnPid, error, Config),
            ok
        end,

    % trigger call contract
    % owner can call a contract
    SomeUnsignedStateTx = contract_calls_(TestName, ContractPubKey, Code, SenderConnPid, UpdateVolley,
                    AckConnPid, SenderPubkey, AckPubkey, Config),
    _ = contract_result_parse(TestName,
                              ws_get_decoded_result(SenderConnPid, AckConnPid,
                                                    contract_return_type(TestName),
                                                    SomeUnsignedStateTx, Config)),
    ok = PruneCalls(SenderConnPid),
    ok = CallMissingCall(SomeUnsignedStateTx, SenderConnPid),
    % state is still usable

    % acknowledger can call a contract
    contract_calls_(TestName, ContractPubKey, Code, AckConnPid, UpdateVolleyReverse,
                    SenderConnPid, AckPubkey, SenderPubkey, Config),

    GetPoI =
        fun(ConnPid) ->
            ws_send_tagged(ConnPid, <<"get">>, <<"poi">>,
                           #{contracts   => [aehttp_api_encoder:encode(contract_pubkey, ContractPubKey)],
                             accounts    => [aehttp_api_encoder:encode(account_pubkey, SenderPubkey),
                                             aehttp_api_encoder:encode(account_pubkey, AckPubkey)]
                            }, Config),

                    {ok, <<"poi">>, #{<<"poi">> := P}} = wait_for_channel_event(ConnPid, get, Config),
                    P
                end,

    GetMissingPoI =
        fun(ConnPid, Accs, Cts) ->
            ws_send_tagged(ConnPid, <<"get">>, <<"poi">>,
                            #{contracts   => [aehttp_api_encoder:encode(contract_pubkey, C) || C <- Cts],
                              accounts    => [aehttp_api_encoder:encode(account_pubkey, Acc) || Acc <- Accs]
                            }, Config),

                    {ok, #{<<"reason">> := R}} = wait_for_channel_event(ConnPid, error, Config),
                    R
                end,

    EncodedPoI = GetPoI(SenderConnPid),
    EncodedPoI = GetPoI(AckConnPid),

    NegativePoiTests =
        fun(ConnPid) ->
            <<"broken_encoding: accounts">> = GetMissingPoI(ConnPid, [<<123456789>>], []),
            <<"broken_encoding: contracts">> = GetMissingPoI(ConnPid, [], [<<123456789>>]),
            <<"broken_encoding: accounts, contracts">> = GetMissingPoI(ConnPid, [<<123456789>>], [<<123456789>>]),
            AccountByteSize = aehttp_api_encoder:byte_size_for_type(account_pubkey),
            FakeAccountId = <<42:AccountByteSize/unit:8>>,
            <<"not_found">> = GetMissingPoI(ConnPid, [FakeAccountId], []),
            ContractByteSize = aehttp_api_encoder:byte_size_for_type(contract_pubkey),
            FakeContractId = <<42:ContractByteSize/unit:8>>,
            <<"not_found">> = GetMissingPoI(ConnPid, [], [FakeContractId])
        end,

    NegativePoiTests(SenderConnPid),
    NegativePoiTests(AckConnPid),

    {ok, PoIBin} = aehttp_api_encoder:safe_decode(poi, EncodedPoI),
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


create_contract_(TestName, SenderConnPid, UpdateVolley, Config) ->
    EncodedCode = contract_byte_code(TestName),
    InitArgument = contract_create_init_arg(TestName),
    {ok, EncodedInitData} = aehttp_logic:contract_encode_call_data(
                              <<"sophia">>,
                              contract_bytearray_decode(EncodedCode),
                              <<"init">>, InitArgument),

    ws_send_tagged(SenderConnPid, <<"update">>, <<"new_contract">>,
                   #{vm_version => ?CURRENT_AEVM_SOPHIA,
                     deposit    => 10,
                     code       => EncodedCode,
                     call_data  => EncodedInitData}, Config),
    UnsignedStateTx = UpdateVolley(),
    {UnsignedStateTx, EncodedCode}.

contract_calls_("identity", ContractPubKey, Code, SenderConnPid, UpdateVolley,
                AckConnPid, _ , _, Config) ->
    UnsignedStateTx = call_a_contract(<<"main">>, <<"(42)">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley, Config),
    ExpectedResult = 42,
    DecodedCallResult =
        contract_result_parse("identity",
                              ws_get_decoded_result(SenderConnPid, AckConnPid,
                                                    contract_return_type("identity"),
                                                    UnsignedStateTx, Config)),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    UnsignedStateTx;
contract_calls_("counter", ContractPubKey, Code, SenderConnPid, UpdateVolley,
                AckConnPid, _ , _, Config) ->
    TestName = "counter",
    UnsignedStateTx0 = call_a_contract(<<"get">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley, Config),
    GetDecodedResult =
        fun(Tx) ->
            contract_result_parse("counter",
                                  ws_get_decoded_result(SenderConnPid, AckConnPid,
                                                        contract_return_type(TestName),
                                                        Tx, Config))
        end,

    InitResult = GetDecodedResult(UnsignedStateTx0),
    call_a_contract(<<"tick">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley, Config),

    UnsignedStateTx1 = call_a_contract(<<"get">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley, Config),

    UnsignedStateTx2 = call_a_contract(<<"get">>, <<"()">>, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley, Config),

    ExpectedResult = InitResult + 1,
    DecodedCallResult = GetDecodedResult(UnsignedStateTx1),
    DecodedCallResult = GetDecodedResult(UnsignedStateTx2),
    {ExpectedResult, _} = {DecodedCallResult, ExpectedResult},
    UnsignedStateTx0;
contract_calls_("spend_test", ContractPubKey, Code, SenderConnPid, UpdateVolley,
                AckConnPid, SenderPubkey, AckPubkey, Config) ->
    GetBalance =
        fun(Args) ->
            FunName =
                case Args of
                    <<"()">> -> <<"get_balance">>;
                    _ -> <<"get_balance_of">>
                end,
            UsStateTx = call_a_contract(FunName, Args, ContractPubKey, Code, SenderConnPid,
                            UpdateVolley, Config),
            _DecodedCallResult =
                contract_result_parse("spend_test",
                                      ws_get_decoded_result(SenderConnPid, AckConnPid,
                                                            contract_return_type("spend_test"),
                                                            UsStateTx, Config))
        end,
    ContractBalance0 = GetBalance(<<"()">>),

    SenderB0 = GetBalance(format_args(SenderPubkey)),
    AckB0 = GetBalance(format_args(AckPubkey)),

    SpendFun =
        fun(To, Amt) ->
            SpendArgs = format_args(To, Amt),
            _SpendStateTx = call_a_contract(<<"spend">>, SpendArgs, ContractPubKey, Code, SenderConnPid,
                                  UpdateVolley, Config)
        end,

    SpendAmt = 3,
    SpendFun(SenderPubkey, SpendAmt),
    ContractBalance = GetBalance(<<"()">>),
    {ContractBalance, _} = {ContractBalance0 - SpendAmt, ContractBalance0},
    SenderB = GetBalance(format_args(SenderPubkey)),
    AckB0 = GetBalance(format_args(AckPubkey)),
    SenderB = SenderB0 + SpendAmt,

    SpendAmt2 = 2,
    UnsignedStateTx = SpendFun(AckPubkey, SpendAmt2),
    ContractBalance1 = GetBalance(<<"()">>),
    {ContractBalance1, _} = {ContractBalance - SpendAmt2, ContractBalance1},
    SenderB = GetBalance(format_args(SenderPubkey)),
    AckB = GetBalance(format_args(AckPubkey)),
    AckB = AckB0 + SpendAmt2,
    UnsignedStateTx.

call_a_contract(Function, Argument, ContractPubKey, Code, SenderConnPid, UpdateVolley, Config) ->
    call_a_contract(Function, Argument, ContractPubKey, Code, SenderConnPid,
                    UpdateVolley, 0, Config).
call_a_contract(Function, Argument, ContractPubKey, Code, SenderConnPid,
                UpdateVolley, Amount, Config) ->
    {ok, EncodedMainData} = aehttp_logic:contract_encode_call_data(<<"sophia">>,
                                                                   contract_bytearray_decode(Code),
                                                                   Function,
                                                                   Argument),
    ws_send_tagged(SenderConnPid, <<"update">>, <<"call_contract">>,
                   #{contract   => aehttp_api_encoder:encode(contract_pubkey, ContractPubKey),
                     vm_version => ?CURRENT_AEVM_SOPHIA,
                     amount     => Amount,
                     call_data  => EncodedMainData}, Config),
    _UnsignedStateTx = UpdateVolley().


contract_byte_code(ContractName) ->
    {ok, BinCode} = aect_test_utils:compile_contract(
                      filename:join(["contracts", 
                                     filename:basename(ContractName, ".aes") ++ ".aes"])),
    aehttp_api_encoder:encode(contract_bytearray, BinCode).

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
    TxHash = aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
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
    TxHash = aehttp_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    wait_for_tx_hash_on_chain(TxHash).

wait_for_tx_hash_on_chain(TxHash) ->
    case aecore_suite_utils:mine_blocks_until_txs_on_chain(
            aecore_suite_utils:node_name(?NODE), [TxHash], 10) of
        {ok, _Blocks} -> ok;
        {error, _Reason} -> did_not_mine
    end.

sc_ws_timeout_open(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 8,
    RAmt = 4,

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{timeout_accept => 100}, Config),
    {ok, IConnPid} = channel_ws_start(initiator, maps:put(host, <<"localhost">>, ChannelOpts), Config),
    ok = ?WS:register_test_for_channel_event(IConnPid, info),
    ok = wait_for_channel_event(<<"died">>, IConnPid, info, Config),
    ok.

%% channel_options(IPubkey, RPubkey, IAmt, RAmt) ->
%%     channel_options(IPubkey, RPubkey, IAmt, RAmt, #{}).

channel_options(IPubkey, RPubkey, IAmt, RAmt, Other, Config) ->
    maps:merge(#{ port => 12340,
                  initiator_id => aehttp_api_encoder:encode(account_pubkey, IPubkey),
                  responder_id => aehttp_api_encoder:encode(account_pubkey, RPubkey),
                  lock_period => 10,
                  push_amount => 1,
                  initiator_amount => IAmt,
                  responder_amount => RAmt,
                  channel_reserve => 2,
                  protocol => proplists:get_value(sc_ws_protocol, Config, <<"legacy">>)
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

format_args(X) ->
    iolist_to_binary(["(", format_arg(X), ")"]).

format_args(X, Y) ->
    iolist_to_binary(["(", format_arg(X), ", ", format_arg(Y), ")"]).

format_arg(B) when is_binary(B)  -> aeu_hex:hexstring_encode(B);
format_arg(I) when is_integer(I) -> integer_to_binary(I).

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
    {_, Sender} = aecore_suite_utils:sign_keys(?NODE),
    SenderId = aehttp_api_encoder:encode(account_pubkey, Sender),
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
    EncodedPubKey = get_pubkey(),
    get_accounts_by_pubkey_sut(EncodedPubKey).

get_pubkey() ->
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    aehttp_api_encoder:encode(account_pubkey, Pubkey).

get_node_pubkey() ->
    Host = internal_address(),
    http_request(Host, get, "debug/accounts/node", []).

get_node_beneficiary() ->
    Host = internal_address(),
    http_request(Host, get, "debug/accounts/beneficiary", []).

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
                   sender_id => <<"">>,
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
                   sender_id => <<"">>,
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
%% Test CORS headers
%% ============================================================

cors_not_returned_when_origin_not_sent(_Config) ->
    Host = external_address(),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc_request(get, {Host ++ "/v2/blocks/top", []}, [], []),

    undefined = proplists:get_value(<<"access-control-allow-origin">>, Headers),
    ok.

cors_returned_on_preflight_request(_Config) ->
    Host = external_address(),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc_request(options, {Host ++ "/v2/blocks/top", [{"origin", "example.com"}]}, [], []),

    "example.com" = proplists:get_value("access-control-allow-origin", Headers),
    "DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT" = proplists:get_value("access-control-allow-methods", Headers),
    "1800" = proplists:get_value("access-control-max-age", Headers),
    "true" = proplists:get_value("access-control-allow-credentials", Headers),
    ok.

cors_returned_on_get_request(_Config) ->
    Host = external_address(),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc_request(get, {Host ++ "/v2/blocks/top", [{"origin", "example.com"}]}, [], []),

    "example.com" = proplists:get_value("access-control-allow-origin", Headers),
    ok.

%% ============================================================
%% Test Cowboy API handler
%% ============================================================

charset_param_in_content_type(_Config) ->
    Host = external_address(),
    BorkedPayload = <<"anything">>,

    {ok, {{_, 400, "Bad Request"}, _, _}} =
        httpc_request(post, {Host ++ "/v2/transactions", [], "application/json", BorkedPayload}, [], []),
    {ok, {{_, 400, "Bad Request"}, _, _}} =
        httpc_request(post, {Host ++ "/v2/transactions", [], "application/json;charset=UTF-8", BorkedPayload}, [], []),
    ok.

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
    {_, PubKey} = aecore_suite_utils:sign_keys(?NODE),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    {PubKey, Nonce}.

add_spend_txs() ->
    MineReward = rpc(aec_governance, block_mine_reward, [1]),
    Fee = 20000,
    %% For now. Mining is severly slowed down by having too many Tx:s in
    %% the tx pool
    MaxSpendTxsInBlock = 20,
    MinimalAmount = 1,
    MaxTxs = min(MineReward div (MinimalAmount + Fee), % enough tokens
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
                #{recipient_id => aehttp_api_encoder:encode(account_pubkey, random_hash()),
                  amount => MinimalAmount,
                  fee => Fee}
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
    Fee = 20000,
    BlocksToMine = blocks_to_mine(Amount + Fee, 1),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    SpendData = #{recipient_id => aehttp_api_encoder:encode(account_pubkey, RecipientPubkey),
                  amount => Amount,
                  fee => Fee},
    populate_block(#{spend_txs => [SpendData]}),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ok.

blocks_to_mine(Amount, ChecksCnt) ->
    Height = case rpc(aec_chain, top_header, []) of
                 undefined -> 1;
                 Header -> aec_headers:height(Header) + 1
             end,
    Delay = rpc(aec_governance, beneficiary_reward_delay, []),
    blocks_to_mine_1(Height, Delay, Amount * ChecksCnt, 0, 0).

blocks_to_mine_1(_Height,_Delay, Goal, N, Acc) when Acc >= Goal ->
    N;
blocks_to_mine_1(Height, Delay, Goal, N, Acc) ->
    case Height - Delay < 1 of
        true ->
            blocks_to_mine_1(Height + 1, Delay, Goal, N + 1, Acc);
        false ->
            Reward = rpc(aec_governance, block_mine_reward, [Height - Delay]),
            blocks_to_mine_1(Height + 1, Delay, Goal, N + 1, Acc + Reward)
    end.

channel_ws_start(Role, Opts, Config) ->
    Opts1 = set_log_option(Opts, Config),
    {Host, Port} = channel_ws_host_and_port(),
    ?WS:start_channel(Host, Port, Role, Opts1).

set_log_option(Opts, Config) ->
    %% TCLogBase = atom_to_list(?config(tc_name, Config)),
    TCLogBase = log_basename(Config),
    MsgLogFile = filename:join(?config(priv_dir, Config), TCLogBase ++ ".md"),
    ct:log("MsgLogFile = ~p", [MsgLogFile]),
    Opts#{ {int,logfile} => MsgLogFile }.

log_basename(Config) ->
    GOpts = ?config(tc_group_properties, Config),
    GPath = ?config(tc_group_path, Config),
    GName = ?config(name, GOpts),
    Path = [?config(name, Opts) || Opts <- GPath],
    intersperse(remove_leading_all(lists:reverse([GName|Path])), ".").

remove_leading_all([all|T]) -> T;
remove_leading_all(L      ) -> L.

intersperse([H|T], Delim) ->
    lists:flatten([H | [[Delim, E] || E <- T]]).

sign_and_post_tx(EncodedUnsignedTx) ->
    sign_and_post_tx(EncodedUnsignedTx, on_node).

sign_and_post_tx(EncodedUnsignedTx, PrivKey) ->
    %% Check that we get the correct hash
    {ok, 200, #{<<"tx_hash">> := TxHash}} = sign_and_post_tx_(EncodedUnsignedTx, PrivKey),
    %% Check tx is in mempool.
    Fun = fun() ->
                  tx_in_mempool(TxHash)
          end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    TxHash.

sign_and_post_tx_(EncodedUnsignedTx) ->
    sign_and_post_tx_(EncodedUnsignedTx, on_node).

sign_and_post_tx_(EncodedUnsignedTx, PrivKey) ->
    {ok, SerializedUnsignedTx} = aehttp_api_encoder:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} =
        case PrivKey =:= on_node of
            true  -> aecore_suite_utils:sign_on_node(?NODE, UnsignedTx);
            false -> {ok, aec_test_utils:sign_tx(UnsignedTx, PrivKey)}
        end,
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    post_transactions_sut(aehttp_api_encoder:encode(transaction, SerializedTx)).

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

ws_get_decoded_result(ConnPid1, ConnPid2, Type, Tx, Config) ->
    %% helper lambda for decoded result
    GetCallResult =
        fun(ConnPid) ->
            ws_send_tagged(ConnPid, <<"get">>, <<"contract_call">>,
                           ws_get_call_params(Tx), Config),
            {ok, <<"contract_call">>, Res} = wait_for_channel_event(ConnPid, get, Config),
            Res
        end,
    CallRes = GetCallResult(ConnPid1),
    CallRes = GetCallResult(ConnPid2),
    #{<<"caller_id">>         := _CallerId,
      <<"caller_nonce">>      := CallRound,
      <<"contract_id">>       := _ContractId,
      <<"gas_price">>         := _,
      <<"gas_used">>          := _,
      <<"height">>            := CallRound,
      <<"return_type">>       := <<"ok">>,
      <<"return_value">>      := ReturnValue} = CallRes,
    {ok, 200, #{<<"data">> := Data}} =
        get_contract_decode_data(#{'sophia-type' => Type, data => ReturnValue}),
    Data.

ws_get_call_params(UnsignedTx) ->
    {CB1, Tx1} = aetx:specialize_callback(UnsignedTx),
    CallRound = CB1:round(Tx1),
    [U] = CB1:updates(Tx1),
    CallerPubKey = aesc_offchain_update:extract_caller(U),
    ContractPubKey = aesc_offchain_update:extract_contract_pubkey(U),
    CallerId = aehttp_api_encoder:encode(account_pubkey, CallerPubKey),
    ContractId = aehttp_api_encoder:encode(contract_pubkey, ContractPubKey),
    #{contract   => ContractId,
      caller     => CallerId,
      round      => CallRound}.

%% wait_for_channel_msg(ConnPid, Action, Config) ->
%%     wait_for_channel_msg_(ConnPid, Action, sc_ws_protocol(Config)).

wait_for_channel_msg_(ConnPid, Action, <<"legacy">>) ->
    ?WS:wait_for_channel_msg(ConnPid, Action);
wait_for_channel_msg_(ConnPid, Action, <<"json-rpc">>) ->
    wait_for_channel_event_(ConnPid, Action, <<"json-rpc">>).

wait_for_channel_event(ConnPid, Action, Config) ->
    wait_for_channel_event_(ConnPid, Action, sc_ws_protocol(Config)).

wait_for_channel_event_(ConnPid, Action, <<"legacy">>) ->
    ?WS:wait_for_channel_event(ConnPid, Action);
wait_for_channel_event_(ConnPid, error, <<"json-rpc">>) ->
    case ?WS:wait_for_channel_msg(ConnPid, error) of   % whole msg
        {ok, #{ <<"jsonrpc">> := <<"2.0">>
              , <<"channel_id">> := _
              , <<"id">>      := null
              , <<"error">>   := E } } ->
            {ok, lift_reason(E)}
    end;
wait_for_channel_event_(ConnPid, Action, <<"json-rpc">>) ->
    Method = method_pfx(Action),
    Sz = byte_size(Method),
    case {?WS:wait_for_channel_msg(ConnPid, Action), Method} of   % whole msg
        {{ok, #{ <<"jsonrpc">> := <<"2.0">>
               , <<"method">>  := <<Method:Sz/binary, _/binary>>
               , <<"params">>  := #{<<"channel_id">> := _} = Params }}, _} ->
            Data = maps:get(<<"data">>, Params, no_data),
            {ok, Data};
        {{ok, Tag, #{ <<"jsonrpc">> := <<"2.0">>
                    , <<"method">>  := <<Method:Sz/binary, _/binary>>
                    , <<"params">>  := Params }}, _} ->
            Data = maps:get(<<"data">>, Params, no_data),
            {ok, Tag, Data}
    end.

wait_for_channel_event(Event, ConnPid, Type, Config) ->
    wait_for_channel_event_(Event, ConnPid, Type, sc_ws_protocol(Config)).

wait_for_channel_event_(Event, ConnPid, Type, <<"legacy">>) ->
    {ok, #{<<"event">> := Event}} = ?WS:wait_for_channel_event(ConnPid, Type),
    ok;
wait_for_channel_event_(Event, ConnPid, Action, <<"json-rpc">>) ->
    {ok, #{ <<"data">> := #{ <<"event">> := Event } }} =
        wait_for_json_rpc_action(ConnPid, Action),
    ok.

wait_for_channel_leave_msg(ConnPid, Config) ->
    wait_for_channel_leave_msg_(ConnPid, sc_ws_protocol(Config)).

wait_for_channel_leave_msg_(ConnPid, <<"legacy">> = L) ->
    {ok, #{ <<"channel_id">> := ChId,
            <<"payload">> := P }} =
        wait_for_channel_msg_(ConnPid, leave, L),
    #{ <<"state">> := St } = P,
    {ok, #{id => ChId, state => St}};
wait_for_channel_leave_msg_(ConnPid, <<"json-rpc">>) ->
    {ok, #{ <<"channel_id">> := ChId,
            <<"data">> := #{ <<"state">> := St } }} =
        wait_for_json_rpc_action(ConnPid, leave),
    {ok, #{id => ChId, state => St}}.

wait_for_json_rpc_action(ConnPid, Action) ->
    Method0 = method_pfx(Action),
    Sz = byte_size(Method0),
    {ok, #{ <<"jsonrpc">> := <<"2.0">>
          , <<"method">>  := <<Method0:Sz/binary, _/binary>>
          , <<"params">>  := #{<<"channel_id">> := _} = Params }} =
        ?WS:wait_for_channel_msg(ConnPid, Action),
    {ok, Params}.

lift_reason(#{ <<"message">> := <<"Rejected">>
             , <<"data">>    := Data } = E) ->
    Codes = lists:sort([Code || #{<<"code">> := Code} <- Data]),
    E#{ <<"reason">> => data_code_to_reason(Codes) };
lift_reason(#{ <<"code">> := Code } = E) ->
    E#{ <<"reason">> => code_to_reason(Code) }.

data_code_to_reason([100 ])      -> <<"not_found">>;
data_code_to_reason([107 ])      -> <<"conflict">>;
data_code_to_reason([1001])      -> <<"insufficient_balance">>;
data_code_to_reason([1002])      -> <<"negative_amount">>;
data_code_to_reason([1003])      -> <<"invalid_pubkeys">>;
data_code_to_reason([1004])      -> <<"call_not_found">>;
data_code_to_reason([1005])      -> <<"broken_encoding: accounts">>;
data_code_to_reason([1006])      -> <<"broken_encoding: contracts">>;
data_code_to_reason([1005,1006]) -> <<"broken_encoding: accounts, contracts">>;
data_code_to_reason([Code])      -> sc_ws_api_jsonrpc:error_data_msg(Code).

code_to_reason(Code) ->
    sc_ws_api_jsonrpc:error_msg(Code).

method_pfx(Action) ->
    <<"channels.", (bin(Action))/binary>>.

ws_send(ConnPid, Action, Payload, Config) ->
    ws_send_(ConnPid, Action, Payload, sc_ws_protocol(Config)).

ws_send_(ConnPid, Action, Payload, <<"legacy">>) ->
    ?WS:send(ConnPid, Action, Payload);
ws_send_(ConnPid, Action, Payload, <<"json-rpc">>) ->
    ?WS:json_rpc_notify(ConnPid, #{ <<"method">> => <<"channels.", (bin(Action))/binary>>
                                  , <<"params">> => Payload }).

ws_send_tagged(ConnPid, Action, Tag, Payload, Config) ->
    ws_send_tagged_(ConnPid, Action, Tag, Payload, sc_ws_protocol(Config)).

ws_send_tagged_(ConnPid, Action, Tag, Payload, <<"legacy">>) ->
    ?WS:send_tagged(ConnPid, Action, Tag, Payload);
ws_send_tagged_(ConnPid, Action, Tag, Payload, <<"json-rpc">>) ->
    ?WS:json_rpc_notify(
       ConnPid,
       #{ <<"method">> => iolist_to_binary([<<"channels.">>, bin(Action), ".", bin(Tag)])
        , <<"params">> => Payload }).

bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
bin(B) when is_binary(B) ->
    B.

sc_ws_protocol(Config) ->
    {_, Protocol} = lists:keyfind(sc_ws_protocol, 1, Config),
    Protocol.
