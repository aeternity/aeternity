-module(aehttp_integration_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").
-import(aecore_suite_utils, [http_request/4, httpc_request/4, process_http_return/1]).
-import(aecore_suite_utils, [internal_address/0, external_address/0, rpc/3, rpc/4]).

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
    get_account_by_pubkey_and_height/1,
    get_pending_account_transactions_by_pubkey/1
   ]).

-export(
   [
    get_transaction_by_hash/1,
    get_transaction_info_by_hash/1,
    post_spend_tx/1,
    post_spend_tx_w_hash_sig/1,
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

%% test case exports
%% external endpoints
-export(
   [
    % non signed txs
    contract_transactions/1,
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

    peers/1,
    sum_token_supply/1
   ]).

%% test case exports
%% debug endpoints
-export([
    disabled_debug_endpoints/1,
    enabled_debug_endpoints/1
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

%% test case exports for HTTP cache headers
-export([
    expires_cache_header/1,
    etag_cache_header/1]).

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
    wrong_http_method_contract_call/1,
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

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

-define(MAX_MINED_BLOCKS, 20).

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

       {group, external_endpoints},
       {group, internal_endpoints},
       {group, debug_endpoints},
       {group, swagger_validation},
       {group, wrong_http_method_endpoints},
       {group, naming}
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
       get_account_by_pubkey_and_height,
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
       post_spend_tx_w_hash_sig,
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

     {external_endpoints, [sequence],
      [
        % non signed txs
        contract_transactions,
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
        peers,
        sum_token_supply
      ]},
     {debug_endpoints, [sequence], [
        disabled_debug_endpoints,
        enabled_debug_endpoints
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

     {http_cache, [],
      [expires_cache_header,
       etag_cache_header]},

     {cowboy_handler, [],
      [charset_param_in_content_type]},

     {wrong_http_method_endpoints, [], [
        wrong_http_method_top,
        wrong_http_method_contract_create,
        wrong_http_method_contract_call,
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
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks},
               <<"mining">> =>
                   #{<<"micro_block_cycle">> => 1}},
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [{symlink_name, "latest.http_endpoints"}, {test_module, ?MODULE}] ++ Config),
    [ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
    , {started_apps, StartedApps} ]  ++ Config1.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(all, Config) ->
    Config;
init_per_group(Group, Config) when
      Group =:= debug_endpoints;
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
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, ?SPEND_FEE),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    ct:log("Spend tx ~p", [Tx]),
    case mine_micro_block_emptying_mempool_or_fail(Node, 3) of
        {ok, [KeyBlock, MicroBlock]} ->
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
        {error, Reason} ->
            ct:fail({could_not_setup_on_micro_block, Reason})
    end;
init_per_group(block_info, Config) ->
    Config;
%% account_endpoints
init_per_group(nonexistent_account = Group, Config) ->
    Config1 = start_node(Group, Config),
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    [{account_id, aeser_api_encoder:encode(account_pubkey, Pubkey)},
     {account_exists, false} | Config1];
init_per_group(account_with_balance = Group, Config) ->
    Config1 = start_node(Group, Config),
    [ {NodeId, Node} | _ ] = ?config(nodes, Config1),
    {_, Pubkey} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock1, KeyBlock2]} = aecore_suite_utils:mine_key_blocks(Node, 2),
    true = aec_blocks:is_key_block(KeyBlock1),
    true = aec_blocks:is_key_block(KeyBlock2),
    [{account_id, aeser_api_encoder:encode(account_pubkey, Pubkey)},
     {account_exists, true} | Config1];
init_per_group(account_with_pending_tx, Config) ->
    [ {NodeId, Node} | _ ] = ?config(nodes, Config),
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, ?SPEND_FEE),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    [{pending_txs, [{aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
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
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, ?SPEND_FEE),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    [{pending_txs, [{aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
     {block_with_txs, undefined},
     {block_with_txs_hash, <<"none">>},
     {block_with_txs_height, -1} | Config];
init_per_group(tx_is_on_chain = Group, Config) ->
    Config1 = start_node(Group, Config),
    Node = ?config(node, Config1),
    case mine_micro_block_emptying_mempool_or_fail(Node, 3) of
        {ok, [KeyBlock, MicroBlock]} ->
            true = aec_blocks:is_key_block(KeyBlock),
            false = aec_blocks:is_key_block(MicroBlock),
            [Tx] = aec_blocks:txs(MicroBlock),
            [{on_chain_txs, [{aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx)), Tx}]},
             {block_with_txs, MicroBlock},
             {block_with_txs_hash, hash(micro, MicroBlock)},
             {block_with_txs_height, aec_blocks:height(KeyBlock)} | Config1];
        {error, Reason} ->
            ct:fail({could_not_setup_tx_is_on_chain, Reason})
    end;
init_per_group(post_tx_to_mempool = Group, Config) ->
    Config1 = start_node(Group, Config),
    [ {NodeId, Node} | _ ] = ?config(nodes, Config1),
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{sender_id, aeser_api_encoder:encode(account_pubkey, Pub)},
     {recipient_id, aeser_api_encoder:encode(account_pubkey, random_hash())},
     {amount, 1},
     {fee, ?SPEND_FEE},
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
    [{account_id, aeser_api_encoder:encode(account_pubkey, Pubkey)},
     {oracle_id, aeser_api_encoder:encode(oracle_pubkey, Pubkey)},
     {query_format, <<"something">>},
     {response_format, <<"something else">>},
     {query_fee, 1},
     {fee, 100000 * aec_test_utils:min_gas_price()},
     {oracle_ttl_type, <<"block">>},
     {oracle_ttl_value, 2000} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_extend, Config) ->
    {post_oracle_register, SavedConfig} = ?config(saved_config, Config),
    OracleTtlDelta = 500,
    [{account_id, ?config(account_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {fee, 100000 * aec_test_utils:min_gas_price()},
     {oracle_ttl_value_final, ?config(oracle_ttl_value, SavedConfig) + OracleTtlDelta},
     {oracle_ttl_type, <<"delta">>},
     {oracle_ttl_value, OracleTtlDelta} | init_per_testcase_all(Config)];
init_per_testcase(post_oracle_query, Config) ->
    {post_oracle_extend, SavedConfig} = ?config(saved_config, Config),
    [{sender_id, ?config(account_id, SavedConfig)},
     {oracle_id, ?config(oracle_id, SavedConfig)},
     {query, <<"Hejsan Svejsan">>},
     {query_fee, 2},
     {fee, 100000 * aec_test_utils:min_gas_price()},
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
     {fee, 100000 * aec_test_utils:min_gas_price()},
     {response_ttl_type, <<"delta">>},
     {response_ttl_value, 20},
     {response, <<"Hejsan">>} | init_per_testcase_all(Config)];
init_per_testcase(Case, Config) when
        Case =:= disabled_debug_endpoints; Case =:= enabled_debug_endpoints ->
    {ok, HttpInternal} = rpc(?NODE, application, get_env, [aehttp, internal]),
    [{http_internal_config, HttpInternal} | init_per_testcase_all(Config)];
init_per_testcase(_Case, Config) ->
    init_per_testcase_all(Config).

init_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(Case, Config) when
        Case =:= disabled_debug_endpoints; Case =:= enabled_debug_endpoints ->
    HttpInternal = ?config(http_internal_config, Config),
    ok = rpc(?NODE, application, set_env, [aehttp, internal, HttpInternal]),
    end_per_testcase_all(Config);
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

start_node(true, _Group, Config) -> Config;
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
            stop_node_(?NODE, Config),
            ok;
        false ->
            ok
    end;
stop_node(false, _Group, _Config) ->
    ok.

stop_node_(Node, Config) ->
    RpcFun = fun(M, F, A) -> rpc(Node, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(Node, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg).

%% ============================================================
%% Test cases
%% ============================================================

contract_bytearray_decode(X) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, X) of
        {ok, Y} -> Y;
        {error, _} = E -> error(E)
    end.

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
    Nonce = aeminer_pow:pick_nonce(),
    {ok, {Nonce1, PowEvidence}} = mine_key_block(KeyBlockHeaderBin, Target, Nonce, 1000),
    {ok, 200, #{}} = post_key_blocks_sut(PendingKeyBlock#{<<"pow">> => PowEvidence, <<"nonce">> => Nonce1}),
    ok = aecore_suite_utils:wait_for_height(?config(node, Config), Height),
    {ok, 200, CurrentBlock} = get_key_blocks_current_sut(),
    ?assertEqual(Height, maps:get(<<"height">>, CurrentBlock)),
    ?assertEqual(PowEvidence, maps:get(<<"pow">>, CurrentBlock)),
    ?assertEqual(Nonce1, maps:get(<<"nonce">>, CurrentBlock)),
    ok.

mine_key_block(HeaderBir, Target, Nonce, Attempts) when Attempts > 0 ->
    [Config] = rpc(aec_mining, get_miner_configs, []),
    mine_key_block(HeaderBir, Target, Nonce, Config, Attempts).

mine_key_block(HeaderBin, Target, Nonce, Config, Attempts) when Attempts > 0 ->
    case rpc(aec_mining, generate, [HeaderBin, Target, Nonce, Config, 0]) of
        {ok, {_Nonce, _PowEvidence}} = Res ->
            Res;
        {error, no_solution} ->
            mine_key_block(HeaderBin, Target, aeminer_pow:trim_nonce(aeminer_pow:next_nonce(Nonce, Config), Config), Config, Attempts - 1)
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

get_account_by_pubkey_and_height(Config) ->
    get_account_by_pubkey_and_height(?config(account_exists, Config), Config).

get_account_by_pubkey_and_height(false, Config) ->
    AccountId = ?config(account_id, Config),
    Header = rpc(?NODE, aec_chain, top_header, []),
    {ok, Hash} = aec_headers:hash_header(Header),
    EncodedHash = aeser_api_encoder:encode(key_block_hash, Hash),
    Height = aec_headers:height(Header),
    {ok, 404, Error1} = get_accounts_by_pubkey_and_height_sut(AccountId, Height),
    {ok, 404, Error1} = get_accounts_by_pubkey_and_hash_sut(AccountId, EncodedHash),
    ?assertEqual(<<"Account not found">>, maps:get(<<"reason">>, Error1)),
    {ok, 404, Error2} = get_accounts_by_pubkey_and_height_sut(AccountId, Height + 2),
    ?assertEqual(<<"Height not available">>, maps:get(<<"reason">>, Error2)),
    BadHash = case Hash of
                  <<1:1, Rest/bits>> -> <<0:1, Rest/bits>>;
                  <<0:1, Rest/bits>> -> <<1:1, Rest/bits>>
              end,
    EncodedBadHash = aeser_api_encoder:encode(key_block_hash, BadHash),
    {ok, 404, Error3} = get_accounts_by_pubkey_and_hash_sut(AccountId, EncodedBadHash),
    ?assertEqual(<<"Hash not available">>, maps:get(<<"reason">>, Error3)),
    BadPrefixHash = aeser_api_encoder:encode(contract_pubkey, Hash),
    {ok, 400, Error4} = get_accounts_by_pubkey_and_hash_sut(AccountId, BadPrefixHash),
    ?assertEqual(<<"Illegal hash: invalid_prefix">>, maps:get(<<"reason">>, Error4)),
    {ok, 400, Error5} = get_accounts_by_pubkey_and_hash_sut(AccountId, <<"Hello">>),
    ?assertEqual(<<"Illegal hash: invalid_encoding">>, maps:get(<<"reason">>, Error5)),

    ok;
get_account_by_pubkey_and_height(true, Config) ->
    AccountId = ?config(account_id, Config),
    Header = rpc(?NODE, aec_chain, top_header, []),
    Height = aec_headers:height(Header),
    {ok, Hash} = aec_headers:hash_header(Header),
    PrevHash = aec_headers:prev_hash(Header),
    EncodedHash = aeser_api_encoder:encode(key_block_hash, Hash),
    EncodedPrevHash = aeser_api_encoder:encode(key_block_hash, PrevHash),
    {ok, 200, Account1} = get_accounts_by_pubkey_and_height_sut(AccountId, Height - 1),
    {ok, 200, Account2} = get_accounts_by_pubkey_and_height_sut(AccountId, Height),
    {ok, 200, Account1} = get_accounts_by_pubkey_and_hash_sut(AccountId, EncodedPrevHash),
    {ok, 200, Account2} = get_accounts_by_pubkey_and_hash_sut(AccountId, EncodedHash),
    ?assertEqual(AccountId, maps:get(<<"id">>, Account1)),
    ?assertEqual(AccountId, maps:get(<<"id">>, Account2)),
    ?assert(maps:get(<<"balance">>, Account1) > 0),
    ?assert(maps:get(<<"balance">>, Account2) > maps:get(<<"balance">>, Account1)),
    {ok, 404, Error1} = get_accounts_by_pubkey_and_height_sut(AccountId, 0),
    ?assertEqual(<<"Account not found">>, maps:get(<<"reason">>, Error1)),
    {ok, 404, Error2} = get_accounts_by_pubkey_and_height_sut(AccountId, Height + 2),
    ?assertEqual(<<"Height not available">>, maps:get(<<"reason">>, Error2)),
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

get_accounts_by_pubkey_and_hash_sut(Id, Hash) ->
    Host = external_address(),
    IdS = binary_to_list(http_uri:encode(Id)),
    http_request(Host, get, "accounts/" ++ IdS ++ "/hash/" ++ Hash, []).

get_accounts_by_pubkey_and_height_sut(Id, Height) ->
    Host = external_address(),
    IdS = binary_to_list(http_uri:encode(Id)),
    HeightS = integer_to_list(Height),
    http_request(Host, get, "accounts/" ++ IdS ++ "/height/" ++ HeightS, []).

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
    RandomTxHash = aeser_api_encoder:encode(tx_hash, random_hash()),
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
    post_spend_tx_(Config, false).

post_spend_tx_w_hash_sig(Config) ->
    post_spend_tx_(Config, true).

post_spend_tx_(Config, SignHash) ->
    TxArgs =
        #{sender_id    => ?config(sender_id, Config),
          recipient_id => ?config(recipient_id, Config),
          amount       => ?config(amount, Config),
          fee          => ?config(fee, Config),
          payload      => ?config(payload, Config)},
    {TxHash, Tx} = prepare_tx(spend_tx, TxArgs, SignHash),
    case lists:last(aec_hard_forks:sorted_protocol_versions()) of
        Vsn when Vsn < ?LIMA_PROTOCOL_VSN andalso SignHash ->
            ?assertMatch({ok, 400, #{<<"reason">> := <<"Invalid tx">>}},
                         post_transactions_sut(Tx));
        _ ->
            ?assertEqual(ok, post_tx(TxHash, Tx))
    end,
    ok.

nonce_limit(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 5),

    aecore_suite_utils:mine_all_txs(Node, ?MAX_MINED_BLOCKS),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    Txs = lists:map(
            fun(_N) ->
                    {ok, 200, #{<<"tx">> := SpendTx}} =
                    post_spend_tx(aeser_api_encoder:encode(account_pubkey, random_hash()),
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

    {ok, EncodedCode} = get_contract_bytecode(identity),
    {ok, EncodedInitCallData} = encode_call_data(identity, "init", []),
    ValidEncoded = #{ owner_id    => Pubkey,
                      code        => EncodedCode,
                      vm_version  => latest_sophia_vm(),
                      abi_version => latest_sophia_abi(),
                      deposit     => 2,
                      amount      => 1,
                      gas         => 600,
                      gas_price   => aec_test_utils:min_gas_price(),
                      fee         => 400000 * aec_test_utils:min_gas_price(),
                      call_data   => EncodedInitCallData},

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    %%%% {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Tx not mined">>}}, get_contract_call_object(ContractCreateTxHash)),

    % mine
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCreateTxHash)),
    ?assertMatch({ok, 200, _}, get_contract_call_object(ContractCreateTxHash)),

    {ok, EncodedCallData} = encode_call_data(identity, "main", ["42"]),
    ContractCallEncoded = #{ caller_id   => Pubkey,
                             contract_id => EncodedContractPubKey,
                             abi_version => latest_sophia_abi(),
                             amount      => 1,
                             gas         => 1000,
                             gas_price   => aec_test_utils:min_gas_price(),
                             fee         => 500000 * aec_test_utils:min_gas_price(),
                             call_data   => EncodedCallData},
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallTx}} = get_contract_call(ContractCallEncoded),
    ContractCallTxHash = sign_and_post_tx(EncodedUnsignedContractCallTx),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCallTxHash)),
    ?assertEqual({ok, 404, #{<<"reason">> => <<"Tx not mined">>}}, get_contract_call_object(ContractCallTxHash)),

    % mine
    ok = wait_for_tx_hash_on_chain(ContractCallTxHash),
    ?assert(tx_in_chain(ContractCallTxHash)),

    ?assertMatch({ok, 200, _}, get_transactions_by_hash_sut(ContractCallTxHash)),
    ?assertMatch({ok, 200, _}, get_contract_call_object(ContractCallTxHash)),
    ok.

get_transactions_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ http_uri:encode(Hash), []).

post_transactions_sut(Tx) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => Tx}).

%% /contracts/*

get_contract(_Config) ->
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 3),

    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),
    {ok, EncodedCode} = get_contract_bytecode(identity),

    % contract_create_tx positive test
    {ok, EncodedInitCallData} = encode_call_data(identity, "init", []),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id    => MinerAddress,
                      code        => EncodedCode,
                      vm_version  => latest_sophia_vm(),
                      abi_version => latest_sophia_abi(),
                      deposit     => 2,
                      amount      => ContractInitBalance,
                      gas         => 600,
                      gas_price   => aec_test_utils:min_gas_price(),
                      fee         => 200000 * aec_test_utils:min_gas_price(),
                      call_data   => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner_id  => aeser_id:create(account, MinerPubkey),
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

    {ok, 200, #{<<"call_info">> := #{<<"return_value">> := _InitStateAndType}}} =
        get_contract_call_object(ContractCreateTxHash),

    VM = latest_sophia_vm(),
    ABI = latest_sophia_abi(),

    ?assertMatch({ok, 200, #{<<"id">>          := EncodedContractPubKey,
                             <<"owner_id">>    := MinerAddress,
                             <<"active">>      := true,
                             <<"deposit">>     := 2,
                             <<"vm_version">>  := VM,
                             <<"abi_version">> := ABI,
                             <<"referrer_ids">> := []}},
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
    RandomOraclePubkey = aeser_api_encoder:encode(oracle_pubkey, random_hash()),
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
    ?assertEqual({ok, Query}, aeser_api_encoder:safe_decode(oracle_query, maps:get(<<"query">>, Resp1))),
    ?assertEqual({ok, Response}, aeser_api_encoder:safe_decode(oracle_response, maps:get(<<"response">>, Resp1))),
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
    ChannelId = aeser_api_encoder:encode(channel, ChannelId0),

    {ok, 200, #{
        <<"id">> := ChannelId,
        <<"initiator_id">> := InitiatorId,
        <<"responder_id">> := ResponderId,
        <<"delegate_ids">> := [],         %% Update needed
        <<"state_hash">> := StateHash
      }} = get_channel_by_pubkey_sut(ChannelId),

    ?assertEqual({ok, IPub}, aeser_api_encoder:safe_decode(account_pubkey, InitiatorId)),
    ?assertEqual({ok, RPub}, aeser_api_encoder:safe_decode(account_pubkey, ResponderId)),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(state, StateHash)),
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
       <<"sync_progress">>              := SyncProgress,
       <<"listening">>                  := Listening,
       <<"protocols">>                  := Protocols,
       <<"node_version">>               := _NodeVersion,
       <<"node_revision">>              := _NodeRevision,
       <<"peer_count">>                 := PeerCount,
       <<"pending_transactions_count">> := PendingTxCount,
       <<"network_id">>                 := NetworkId,
       <<"peer_pubkey">>                := PeerPubKey,
       <<"top_key_block_hash">>         := TopKeyBlockHash,
       <<"top_block_height">>           := TopBlockHeight
      }} = get_status_sut(),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(key_block_hash, GenesisKeyBlocHash)),
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
    ?assertEqual(100.0, SyncProgress),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(peer_pubkey, PeerPubKey)),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(key_block_hash, TopKeyBlockHash)),
    ?assertMatch(X when is_integer(X) andalso X >= 0, TopBlockHeight),
    ok.

get_status_sut() ->
    Host = external_address(),
    http_request(Host, get, "status", []).

prepare_tx(TxType, Args) ->
    SignHash = lists:last(aec_hard_forks:sorted_protocol_versions()) >= ?LIMA_PROTOCOL_VSN,
    prepare_tx(TxType, Args, SignHash).

prepare_tx(TxType, Args, SignHash) ->
    %assert_required_tx_fields(TxType, Args),
    {Host, Path} = tx_object_http_path(TxType),
    {ok, 200, #{<<"tx">> := EncodedSerializedUnsignedTx}} = http_request(Host, post, Path, Args),
    {ok, SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedSerializedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),

    NodeT = aecore_suite_utils:node_tuple(?NODE),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(NodeT, UnsignedTx, SignHash),

    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedSignedTx = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
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
    aeser_api_encoder:encode(key_block_hash, Hash0);
hash(micro, Block) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(Block),
    aeser_api_encoder:encode(micro_block_hash, Hash0).

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
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),
    {ok, EncodedCode} = get_contract_bytecode(identity),

    % contract_create_tx positive test
    {ok, EncodedInitCallData} = encode_call_data(identity, "init", []),

    ContractInitBalance = 1,
    ValidEncoded = #{ owner_id => MinerAddress,
                      code => EncodedCode,
                      vm_version => latest_sophia_vm(),
                      abi_version => latest_sophia_abi(),
                      deposit => 2,
                      amount => ContractInitBalance,
                      gas => 600,
                      gas_price => aec_test_utils:min_gas_price(),
                      fee => 200000 * aec_test_utils:min_gas_price(),
                      call_data => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner_id => aeser_id:create(account, MinerPubkey),
                                code => contract_bytearray_decode(EncodedCode),
                                call_data => contract_bytearray_decode(EncodedInitCallData)}),

    unsigned_tx_positive_test(ValidDecoded, ValidEncoded, fun get_contract_create/1,
                               fun aect_create_tx:new/1, MinerPubkey),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
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
    {ok, 200, #{<<"call_info">> := InitCallObject}} = get_contract_call_object(ContractCreateTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, InitCallObject)),
    ?assertEqual(get_tx_nonce(ContractCreateTxHash), maps:get(<<"caller_nonce">>, InitCallObject)),
    ?assertEqual(aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
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
    {ok, PoIBin} = aeser_api_encoder:safe_decode(poi, EncPoI),
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

    {ok, EncodedCallData} = encode_call_data(identity, "main", ["42"]),

    ContractCallEncoded = #{ caller_id => MinerAddress,
                             contract_id => EncodedContractPubKey,
                             abi_version => latest_sophia_abi(),
                             amount => 1,
                             gas => 1000,
                             gas_price => aec_test_utils:min_gas_price(),
                             fee => 600000 * aec_test_utils:min_gas_price(),
                             call_data => EncodedCallData},

    ContractCallDecoded = maps:merge(ContractCallEncoded,
                              #{caller_id => aeser_id:create(account, MinerPubkey),
                                contract_id => aeser_id:create(contract, ContractPubKey),
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
    {ok, 200, #{<<"call_info">> := CallObject}} = get_contract_call_object(ContractCallTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, CallObject, <<>>)),
    ?assertEqual(get_tx_nonce(ContractCallTxHash), maps:get(<<"caller_nonce">>, CallObject)),
    ?assertEqual(aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
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

    %% negative tests
    %% Invalid hashes
    %% invalid owner hash
    <<_, InvalidHash/binary>> = MinerAddress,
    {ok, 400, #{<<"reason">> := <<"Invalid hash: owner_id">>}} =
        get_contract_create(maps:put(owner_id, InvalidHash, ValidEncoded)),
    % invalid caller hash
    {ok, 400, #{<<"reason">> := <<"Invalid hash: caller_id">>}} =
        get_contract_call(maps:put(caller_id, InvalidHash, ContractCallEncoded)),

    %% account not found
    RandAddress = aeser_api_encoder:encode(account_pubkey, random_hash()),
    RandContractAddress =aeser_api_encoder:encode(contract_pubkey, random_hash()),
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

    %% Call objects
    {ok, 200, #{<<"tx">> := SpendTx}} = post_spend_tx(MinerAddress, 1,
                                                      ?SPEND_FEE),
    SpendTxHash = sign_and_post_tx(SpendTx),
    ok = wait_for_tx_hash_on_chain(SpendTxHash),
    {ok, 400, #{<<"reason">> := <<"Tx has no info">>}} =
        get_contract_call_object(SpendTxHash),

    ok.

contract_create_transaction_init_error(_Config) ->
    % miner has an account
    {ok, 200, _} = get_balance_at_top(),
    MinerAddress = get_pubkey(),
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),

    % contract_create_tx positive test
    EncodedCode = contract_byte_code("init_error"),
    {ok, EncodedInitData} = encode_call_data(init_error, "init",
                                             [aeser_api_encoder:encode(contract_pubkey, <<123:256>>), "0"]),
    EncodedInitCallData = aeser_api_encoder:encode(contract_bytearray, aeb_heap:to_binary({<<"init">>, {}})),
    ValidEncoded = #{ owner_id    => MinerAddress,
                      code        => EncodedCode,
                      vm_version  => latest_sophia_vm(),
                      abi_version => latest_sophia_abi(),
                      deposit     => 2,
                      amount      => 1,
                      gas         => 30,
                      gas_price   => aec_test_utils:min_gas_price(),
                      fee         => 200000 * aec_test_utils:min_gas_price(),
                      call_data   => EncodedInitData},
    ValidDecoded = maps:merge(ValidEncoded,
        #{owner => MinerPubkey,
            code => contract_bytearray_decode(EncodedCode),
            call_data => contract_bytearray_decode(EncodedInitCallData)}),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
        <<"contract_id">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(EncodedUnsignedContractCreateTx),

    %% Try to get the contract init call object while in mempool
    {ok, 404, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCreateTxHash),

    % mine blocks
    ok = wait_for_tx_hash_on_chain(ContractCreateTxHash),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Get the contract init call object
    {ok, 200, #{<<"call_info">> := InitCallObject}} = get_contract_call_object(ContractCreateTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_id">>, InitCallObject)),
    ?assertEqual(get_tx_nonce(ContractCreateTxHash), maps:get(<<"caller_nonce">>, InitCallObject)),
    ?assertEqual(aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
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
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),
    OracleAddress = aeser_api_encoder:encode(oracle_pubkey, MinerPubkey),

    % oracle_register_tx positive test
    RegEncoded = #{account_id => MinerAddress,
                   query_format => <<"something">>,
                   response_format => <<"something else">>,
                   query_fee => 1,
                   fee => 100000 * aec_test_utils:min_gas_price(),
                   abi_version => 0, %% ABI_NO_VM - raw strings.
                   oracle_ttl => #{type => <<"block">>, value => 2000}},
    RegDecoded = maps:merge(RegEncoded,
                            #{account_id => aeser_id:create(account, MinerPubkey),
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
    ExtEncoded = #{oracle_id => aeser_api_encoder:encode(oracle_pubkey, MinerPubkey),
                   fee => 2 * aec_test_utils:min_gas_price(),
                   oracle_ttl => #{type => <<"delta">>, value => 500}},
    ExtDecoded = maps:merge(ExtEncoded,
                            #{oracle_id => aeser_id:create(oracle, MinerPubkey),
                              oracle_ttl => {delta, 500}}),
    unsigned_tx_positive_test(ExtDecoded, ExtEncoded,
                               fun get_oracle_extend/1,
                               fun aeo_extend_tx:new/1, MinerPubkey),

    % oracle_query_tx positive test
    QueryEncoded = #{sender_id => MinerAddress,
                     oracle_id => aeser_api_encoder:encode(oracle_pubkey, MinerPubkey),
                     query => <<"Hejsan Svejsan">>,
                     query_fee => 2,
                     fee => 100000 * aec_test_utils:min_gas_price(),
                     query_ttl => #{type => <<"block">>, value => 50},
                     response_ttl => #{type => <<"delta">>, value => 20}},
    QueryDecoded = maps:merge(QueryEncoded,
                              #{sender_id => aeser_id:create(account, MinerPubkey),
                                oracle_id => aeser_id:create(oracle, MinerPubkey),
                                query_ttl => {block, 50},
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
                        query_id => aeser_api_encoder:encode(oracle_query_id,
                                                       QueryId),
                        response => <<"Hejsan">>,
                        response_ttl => #{type => <<"delta">>, value => 20},
                        fee => 100000 * aec_test_utils:min_gas_price()},
    ResponseDecoded = maps:merge(ResponseEncoded,
                              #{oracle_id => aeser_id:create(oracle, MinerPubkey),
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
    RandAddress = aeser_api_encoder:encode(account_pubkey, random_hash()),
    RandOracleAddress = aeser_api_encoder:encode(oracle_pubkey, random_hash()),
    RandQueryID = aeser_api_encoder:encode(oracle_query_id, random_hash()),
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
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),
    nameservice_transaction_preclaim(MinerAddress, MinerPubkey),
    nameservice_transaction_claim(MinerAddress, MinerPubkey),
    nameservice_transaction_update(MinerAddress, MinerPubkey),
    nameservice_transaction_transfer(MinerAddress, MinerPubkey),
    nameservice_transaction_revoke(MinerAddress, MinerPubkey),
    ok.

nameservice_transaction_preclaim(MinerAddress, MinerPubkey) ->
    Commitment = random_hash(),
    Encoded = #{account_id => MinerAddress,
                commitment_id => aeser_api_encoder:encode(commitment, Commitment),
                fee => 1 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{account_id => aeser_id:create(account, MinerPubkey),
                          commitment_id => aeser_id:create(commitment, Commitment)}),
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
    CorrectAddress = aeser_api_encoder:encode(PubKeyType, PubKey),
    Msg = list_to_binary("Invalid hash: " ++ atom_to_list(Name)),
    <<_, HashWithBrokenPrefix/binary>> = CorrectAddress,
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HashWithBrokenPrefix, Encoded)),

    <<_Prefix:3/binary, HashWithNoPrefix/binary>> = CorrectAddress,
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HashWithNoPrefix, Encoded)),

    case aeser_api_encoder:byte_size_for_type(PubKeyType) of
        not_applicable -> pass;
        _ ->
            <<ShortHash:10/binary, _Rest/binary>> = CorrectAddress,
            {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, ShortHash, Encoded)),

            BS = byte_size(PubKey),
            HalfSize = BS div 2,
            <<FirstHalfKey:HalfSize/binary, _SecondHalfKey/binary>> = PubKey,
            HalfHash = aeser_api_encoder:encode(PubKeyType, FirstHalfKey),
            {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(MapKey, HalfHash, Encoded))
    end,
    ok.

test_missing_address(Key, Encoded, APIFun) ->
    Msg = list_to_binary("Account of " ++ atom_to_list(Key) ++ " not found"),
    RandAddress = aeser_api_encoder:encode(account_pubkey, random_hash()),
    {ok, 404, #{<<"reason">> := Msg}} =
        APIFun(maps:put(Key, RandAddress, Encoded)),
    ok.

nameservice_transaction_claim(MinerAddress, MinerPubkey) ->
    Name = <<"name.test">>,
    Salt = 1234,

    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = get_commitment_id(Name, Salt),
    {ok, _CHash} = aeser_api_encoder:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => 100000 * aec_test_utils:min_gas_price(),
                     account_id    => MinerAddress},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = get_name_preclaim(PreclaimData),
    PreclaimTxHash = sign_and_post_tx(PreclaimTxEnc),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    ok = wait_for_tx_hash_on_chain(PreclaimTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    Encoded = #{account_id => MinerAddress,
                name => aeser_api_encoder:encode(name, Name),
                name_salt => Salt,
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{account_id => aeser_id:create(account, MinerPubkey),
                          name => Name}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_claim/1,
                               fun aens_claim_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, account_id, Encoded, fun get_name_claim/1),
    test_invalid_hash({name, MinerPubkey}, name, Encoded, fun get_name_claim/1),
    test_missing_address(account_id, Encoded, fun get_name_claim/1),

    %% missing registar
    Missing = aeser_api_encoder:encode(name, <<"missing">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: no_registrar">>}} =
        get_name_claim(maps:put(name, Missing, Encoded)),
    MissingReg = aeser_api_encoder:encode(name, <<"missing.reg">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_name_claim(maps:put(name, MissingReg, Encoded)),
    ok.

nameservice_transaction_update(MinerAddress, MinerPubkey) ->
    NameHash = random_hash(),
    Pointers = [],
    Encoded = #{account_id => MinerAddress,
                name_id => aeser_api_encoder:encode(name, NameHash),
                name_ttl => 3,
                client_ttl => 2,
                pointers => Pointers,
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{account_id => aeser_id:create(account, MinerPubkey),
                          pointers => Pointers,
                          name_id => aeser_id:create(name, NameHash)}),
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
                name_id => aeser_api_encoder:encode(name, NameHash),
                recipient_id => aeser_api_encoder:encode(account_pubkey, RandAddress),
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{account_id => aeser_id:create(account, MinerPubkey),
                          recipient_id => aeser_id:create(account, RandAddress),
                          name_id => aeser_id:create(name, NameHash)}),
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
                name_id => aeser_api_encoder:encode(name, NameHash),
                fee => 10000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{account_id => aeser_id:create(account, MinerPubkey),
                          name_id => aeser_id:create(name, NameHash)}),
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
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),
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
    Encoded = #{initiator_id => aeser_api_encoder:encode(account_pubkey, MinerPubkey),
                initiator_amount => 2,
                responder_id => aeser_api_encoder:encode(account_pubkey, ResponderPubkey),
                responder_amount => 3,
                push_amount => 5, channel_reserve => 5,
                lock_period => 20,
                state_hash => aeser_api_encoder:encode(state, ?BOGUS_STATE_HASH),
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{initiator_id => aeser_id:create(account, MinerPubkey),
                          responder_id => aeser_id:create(account, ResponderPubkey),
                          state_hash => ?BOGUS_STATE_HASH}),
    {ok, Tx} = unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_create/1,
                               fun aesc_create_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, initiator_id, Encoded, fun get_channel_create/1),
    test_invalid_hash({account_pubkey, ResponderPubkey}, responder_id, Encoded, fun get_channel_create/1),
    test_missing_address(initiator_id, Encoded, fun get_channel_create/1),

    %% test delegates
    TestDelegates =
        fun(Ds) ->
            Enc = Encoded#{delegate_ids => [aeser_api_encoder:encode(account_pubkey, D)
                                            || D <- Ds]},
            Dec = Decoded#{delegate_ids => [aeser_id:create(account, D) || D <- Ds]},
            {ok, _Tx} = unsigned_tx_positive_test(Dec, Enc,
                                                  fun get_channel_create/1,
                                                  fun aesc_create_tx:new/1, MinerPubkey),
            Msg = list_to_binary("Invalid hash: delegate_ids"),
            TestBroken =
                fun(Broken) ->
                    BrokenDelegates1 = [Broken | maps:get(delegate_ids, Enc)],
                    BrokenDelegates2 = maps:get(delegate_ids, Enc) ++ [Broken],
                    {ok, 400, #{<<"reason">> := Msg}} =
                        get_channel_create(Enc#{delegate_ids =>
                                                BrokenDelegates1}),
                    {ok, 400, #{<<"reason">> := Msg}} =
                        get_channel_create(Enc#{delegate_ids =>
                                                BrokenDelegates2}),
                    ok
                end,
            CorrectAddress = <<1234:32/unit:8>>,
            <<_, HashWithBrokenPrefix/binary>> = CorrectAddress,
            <<_Prefix:3/binary, HashWithNoPrefix/binary>> = CorrectAddress,
            TestBroken(HashWithBrokenPrefix),
            TestBroken(HashWithNoPrefix),


            ok
        end,
    TestDelegates([]),
    TestDelegates([<<42:32/unit:8>>]),
    TestDelegates([<<42:32/unit:8>>, <<43:32/unit:8>>]),


    {ok, Tx}.

state_channels_deposit(ChannelId, MinerPubkey) ->
    MinerAddress = aeser_api_encoder:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                from_id => MinerAddress,
                amount => 2,
                state_hash => aeser_api_encoder:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{channel_id => aeser_id:create(channel, ChannelId),
                          from_id => aeser_id:create(account, MinerPubkey),
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
    MinerAddress = aeser_api_encoder:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                to_id => MinerAddress,
                amount => 2,
                state_hash => aeser_api_encoder:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{channel_id => aeser_id:create(channel, ChannelId),
                          to_id => aeser_id:create(account, MinerPubkey),
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
    Payload = <<"hejsan svejsan">>, %%TODO proper payload
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                from_id => aeser_api_encoder:encode(account_pubkey, MinerPubkey),
                payload => aeser_api_encoder:encode(bytearray, Payload),
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{from_id => aeser_id:create(account, MinerPubkey),
                          channel_id => aeser_id:create(channel, ChannelId),
                          payload => Payload}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_snapshot_solo/1,
                               fun aesc_snapshot_solo_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_snapshot_solo/1),
    ok.

state_channels_close_mutual(ChannelId, InitiatorPubkey) ->
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                from_id => aeser_api_encoder:encode(account_pubkey, InitiatorPubkey),
                initiator_amount_final => 4,
                responder_amount_final => 3,
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                         #{channel_id => aeser_id:create(channel, ChannelId),
                           from_id    => aeser_id:create(account, InitiatorPubkey)}),
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
    Payload = <<"hejsan svejsan">>, %%TODO proper payload
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                from_id => aeser_api_encoder:encode(account_pubkey, MinerPubkey),
                payload => aeser_api_encoder:encode(bytearray, Payload),
                poi => aeser_api_encoder:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{from_id => aeser_id:create(account, MinerPubkey),
                          channel_id => aeser_id:create(channel, ChannelId),
                          payload => Payload,
                          poi => PoI}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_close_solo/1,
                               fun aesc_close_solo_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_close_solo/1),
    BrokenPoIs = [<<>>, <<"hejsan svejsan">>],
    lists:foreach(
        fun(BrokenPoI) ->
            EncBrokenPoI =  aeser_api_encoder:encode(poi, BrokenPoI),
            {ok, 400, #{<<"reason">> := <<"Invalid proof of inclusion">>}}
                = get_channel_close_solo(maps:put(poi, EncBrokenPoI, Encoded))
        end,
        BrokenPoIs),
    ok.

state_channels_slash(ChannelId, MinerPubkey) ->
    Payload = <<"hejsan svejsan">>, %%TODO proper payload
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                from_id => aeser_api_encoder:encode(account_pubkey, MinerPubkey),
                payload => aeser_api_encoder:encode(bytearray, Payload),
                poi => aeser_api_encoder:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{from_id => aeser_id:create(account, MinerPubkey),
                          channel_id => aeser_id:create(channel, ChannelId),
                          payload => Payload,
                          poi => PoI}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_slash/1,
                               fun aesc_slash_tx:new/1, MinerPubkey),
    test_invalid_hash({account_pubkey, MinerPubkey}, from_id, Encoded, fun get_channel_slash/1),

    BrokenPoIs = [<<>>, <<"hejsan svejsan">>],
    lists:foreach(
        fun(BrokenPoI) ->
            EncBrokenPoI =  aeser_api_encoder:encode(poi, BrokenPoI),
            {ok, 400, #{<<"reason">> := <<"Invalid proof of inclusion">>}}
                = get_channel_slash(maps:put(poi, EncBrokenPoI, Encoded))
        end,
        BrokenPoIs),
    ok.

state_channels_settle(ChannelId, MinerPubkey) ->
    Encoded = #{channel_id => aeser_api_encoder:encode(channel, ChannelId),
                from_id => aeser_api_encoder:encode(account_pubkey, MinerPubkey),
                initiator_amount_final => 4,
                responder_amount_final => 3,
                fee => 100000 * aec_test_utils:min_gas_price()},
    Decoded = maps:merge(Encoded,
                        #{from_id => aeser_id:create(account, MinerPubkey),
                          channel_id => aeser_id:create(channel, ChannelId)}),
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
    {ok, MinerPubkey} = aeser_api_encoder:safe_decode(account_pubkey, MinerAddress),
    RandAddress = random_hash(),
    Payload = <<"hejsan svejsan">>,
    Encoded = #{sender_id => MinerAddress,
                recipient_id => aeser_api_encoder:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => 100000 * aec_test_utils:min_gas_price(),
                ttl => 43,
                payload => aeser_api_encoder:encode(bytearray, Payload)
               },
    Decoded = maps:merge(Encoded,
                        #{sender_id => aeser_id:create(account, MinerPubkey),
                          recipient_id => aeser_id:create(account, RandAddress),
                          payload => Payload}),
    {ok, T} = unsigned_tx_positive_test(Decoded, Encoded,
                                  fun get_spend/1,
                                  fun aec_spend_tx:new/1, MinerPubkey),
    {spend_tx, SpendTx} = aetx:specialize_type(T),
    ?assertEqual(Payload, aec_spend_tx:payload(SpendTx)),

    %% Test that we can also still pass unencoded payload.
    {ok, _T2} = unsigned_tx_positive_test(Decoded, Encoded#{payload => Payload},
                                  fun get_spend/1,
                                  fun aec_spend_tx:new/1, MinerPubkey),

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
                recipient_id => aeser_api_encoder:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => ?SPEND_FEE,
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
            {ok, SerializedTx} = aeser_api_encoder:safe_decode(transaction, ActualTx),
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
                recipient_id => aeser_api_encoder:encode(account_pubkey, RandAddress),
                amount => 2,
                fee => ?SPEND_FEE,
                payload => <<"foo">>},
    {ok, 200, #{<<"tx">> := EncodedSpendTx}} = get_spend(Encoded),
    {ok, SpendTxBin} = aeser_api_encoder:safe_decode(transaction, EncodedSpendTx),
    SpendTx = aetx:deserialize_from_binary(SpendTxBin),
    {ok, SignedSpendTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedSpendTx)),

    SerializedSpendTx = aetx_sign:serialize_to_binary(SignedSpendTx),
    {ok, 200, _} = post_transactions_sut(aeser_api_encoder:encode(transaction, SerializedSpendTx)),
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
        [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))
            || SignedTx <- PendingTxs],
    ct:log("Pending txs: ~p", [PendingTxs]),
    ct:log("Pending tx hashes: ~p", [PendingTxHashes]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, PendingTxHashes, ?MAX_MINED_BLOCKS),

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

    DevRewardEnabled = rpc(aec_dev_reward, enabled, []) andalso
        aect_test_utils:latest_protocol_version() >= ?FORTUNA_PROTOCOL_VSN,
    DevRewardSharesSum = rpc(aec_dev_reward, allocated_shares, []),
    DevRewardTotalShares = rpc(aec_dev_reward, total_shares, []),

    BlockRewards = fun (Blocks) ->
                           [begin
                                Height = aec_blocks:height(B) - Delay,
                                rpc(aec_governance, block_mine_reward, [Height])
                            end || B <- Blocks]
                   end,

    MinedRewards1 =
        case DevRewardEnabled of
            true ->
                [Amount - (Amount * DevRewardSharesSum div DevRewardTotalShares) ||
                    Amount <- BlockRewards(MinedBlocks1)];
            false ->
                BlockRewards(MinedBlocks1)
        end,
    ExpectedReward1 = lists:sum(MinedRewards1),
    ct:log("Bal0: ~p, Initial Balance: ~p, Blocks to mine: ~p, Expected reward: ~p",
           [Bal0, InitialBalance, BlocksToMine, ExpectedReward1]),
    ?assertEqual(Bal0, InitialBalance + ExpectedReward1),

    true = (is_integer(Bal0) andalso Bal0 >= AmountToSpent),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, ReceiverPubKey)),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aeser_api_encoder:encode(account_pubkey, ReceiverPubKey), AmountToSpent,
                      ?SPEND_FEE),
    sign_and_post_tx(SpendTx),
    {ok, NodeTxs} = rpc(aec_tx_pool, peek, [infinity]),
    true = length(NodeTxs) =:= 1, % not empty anymore
    {ok, 200, #{<<"transactions">> := ReturnedTxs}} = get_pending_transactions(),
    ExpectedTxs = [aetx_sign:serialize_for_client_pending(T) || T <- NodeTxs],
    true = length(ExpectedTxs) =:= length(ReturnedTxs),
    true = lists:all(fun(Tx) -> lists:member(Tx, ExpectedTxs) end, ReturnedTxs),

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, ReceiverPubKey)),

    PendingTxHashes2 =
        [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))
            || SignedTx <- NodeTxs],
    {ok, MinedBlocks2a} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, PendingTxHashes2, ?MAX_MINED_BLOCKS),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    {ok, 200, #{<<"transactions">> := []}} = get_pending_transactions(),

    %% Make sure we get the reward...
    {ok, MinedBlocks2b} = aecore_suite_utils:mine_key_blocks(Node, Delay),

    MinedRewards2 =
        case DevRewardEnabled of
            true ->
                [Amount - (Amount * DevRewardSharesSum div DevRewardTotalShares) ||
                    Amount <- BlockRewards(MinedBlocks2a ++ MinedBlocks2b)];
            false ->
                BlockRewards(MinedBlocks2a ++ MinedBlocks2b)
        end,

    ExpectedReward2 = lists:sum(MinedRewards2) -
        case DevRewardEnabled of
            true ->
                %% We get SPEND_FEE back as miner reward except the cut for protocol beneficiary
                ?SPEND_FEE * DevRewardSharesSum div DevRewardTotalShares;
            false ->
                0
        end,
    {ok, 200, #{<<"balance">> := Bal1}} = get_balance_at_top(),
    ct:log("Bal1: ~p, Bal0: ~p, Expected reward: ~p, Amount to spend: ~p",
           [Bal1, Bal0, ExpectedReward2, AmountToSpent]),

    ?assertEqual(Bal1, Bal0 + ExpectedReward2 - AmountToSpent),

    {ok, 200, #{<<"balance">> := AmountToSpent}} =
                 get_accounts_by_pubkey_sut(aeser_api_encoder:encode(account_pubkey, ReceiverPubKey)),
    ok.

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(_Config) ->
    Amount = 1,
    BlocksToMine = blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender_id => aeser_id:create(account, PubKey),
            recipient_id => aeser_id:create(account, random_hash()),
            amount => Amount,
            fee => ?SPEND_FEE,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
    ExpectedHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, 200, #{<<"tx_hash">> := ExpectedHash}} =
        post_transactions_sut(aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx))),
    {ok, [SignedTx]} = rpc(aec_tx_pool, peek, [infinity]), % same tx
    ok.

post_broken_tx(_Config) ->
    Amount = 1,
    BlocksToMine = blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(max(BlocksToMine, 3)),  %% we need at least 3 blocks
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender_id => aeser_id:create(account, PubKey),
            recipient_id => aeser_id:create(account, random_hash()),
            amount => Amount,
            fee => ?SPEND_FEE,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
    SignedTxBin = aetx_sign:serialize_to_binary(SignedTx),

    {ok, SpendTTLTx} =
        aec_spend_tx:new(
          #{sender_id => aeser_id:create(account, PubKey),
            recipient_id => aeser_id:create(account, random_hash()),
            amount => Amount,
            fee => ?SPEND_FEE,
            nonce => Nonce,
            ttl => 2,
            payload => <<"too low ttl">>}),
    {ok, SignedTTLTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTTLTx),
    SignedTTLTxBin = aetx_sign:serialize_to_binary(SignedTTLTx),

    BrokenTxBin = case SignedTxBin of
                    <<1:1, Rest/bits>> -> <<0:1, Rest/bits>>;
                    <<0:1, Rest/bits>> -> <<1:1, Rest/bits>>
                  end,
    EncodedBrokenTx = aeser_api_encoder:encode(transaction, BrokenTxBin),
    EncodedBrokenTTLTx = aeser_api_encoder:encode(transaction, SignedTTLTxBin),
    EncodedSignedTx = aeser_api_encoder:encode(transaction, SignedTxBin),
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
                  #{sender_id => aeser_id:create(account, PubKey),
                    recipient_id => aeser_id:create(account, random_hash()),
                    amount => Amount,
                    fee => ?SPEND_FEE,
                    nonce => Nonce,
                    payload => <<"foo">>}),
            {ok, SignedTx} = aecore_suite_utils:sign_on_node(?NODE, SpendTx),
            <<_, BrokenHash/binary>> =
                aeser_api_encoder:encode(transaction,
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
    {ok, 404, _} = post_spend_tx(aeser_api_encoder:encode(account_pubkey, ReceiverPubKey), 42, 2),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

node_pubkey(_Config) ->
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_node_pubkey(),
    ct:log("MinerPubkey = ~p~nEncodedPubKey = ~p", [MinerPubKey,
                                                    EncodedPubKey]),
    {account_pubkey, MinerPubKey} = aeser_api_encoder:decode(EncodedPubKey),
    ok.

node_beneficiary(_Config) ->
    {ok, 200, #{<<"pub_key">> := SignPubKey0}} = get_node_pubkey(),
    {ok, 200, #{<<"pub_key">> := BeneficiaryPubKey0}} = get_node_beneficiary(),

    ?assertMatch({account_pubkey, _}, aeser_api_encoder:decode(BeneficiaryPubKey0)),

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
    {ok, PeerPubKey} = aeser_api_encoder:safe_decode(peer_pubkey, EncodedPubKey),
    ok.

naming_system_manage_name(_Config) ->
    {PubKey, PrivKey} = initialize_account(1000000000 *
                                           aec_test_utils:min_gas_price()),
    PubKeyEnc   = aeser_api_encoder:encode(account_pubkey, PubKey),
    %% TODO: find out how to craete HTTP path with unicode chars
    %%Name        = <<"詹姆斯詹姆斯.test"/utf8>>,
    Name        = <<"without-unicode.test">>,
    NameSalt    = 12345,
    NameTTL     = 20000,
    Pointers    = [#{<<"key">> => <<"account_pubkey">>, <<"id">> => PubKeyEnc}],
    TTL         = 10,
    {ok, NHash} = aens:get_name_hash(Name),
    Fee         = 100000 * aec_test_utils:min_gas_price(),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    {ok, 200, #{<<"balance">> := Balance}} = get_accounts_by_pubkey_sut(PubKeyEnc),

    %% Get commitment hash to preclaim a name
    {ok, 200, #{<<"commitment_id">> := EncodedCHash}} = get_commitment_id(Name, NameSalt),
    {ok, _CHash} = aeser_api_encoder:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    PreclaimData = #{commitment_id => EncodedCHash,
                     fee           => Fee,
                     account_id    => PubKeyEnc},
    {ok, 200, #{<<"tx">> := PreclaimTxEnc}} = get_name_preclaim(PreclaimData),
    PreclaimTxHash = sign_and_post_tx(PreclaimTxEnc, PrivKey),
    {ok, 200, #{<<"tx">> := PreclaimTx}} = get_transactions_by_hash_sut(PreclaimTxHash),
    ?assertEqual(EncodedCHash, maps:get(<<"commitment_id">>, PreclaimTx)),

    %% Mine a block and check mempool empty again
    ok = wait_for_tx_hash_on_chain(PreclaimTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check fee taken from account
    {ok, 200, #{<<"balance">> := Balance1}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance1, Balance - Fee),

    %% Submit name claim tx and check it is in mempool
    ClaimData = #{account_id => PubKeyEnc,
                  name       => aeser_api_encoder:encode(name, Name),
                  name_salt  => NameSalt,
                  fee        => Fee},
    {ok, 200, #{<<"tx">> := ClaimTxEnc}} = get_name_claim(ClaimData),
    ClaimTxHash = sign_and_post_tx(ClaimTxEnc, PrivKey),

    %% Mine a block and check mempool empty again
    ok = wait_for_tx_hash_on_chain(ClaimTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check tx fee taken from account, claim fee locked,
    %% then mine reward and fee added to account
    ClaimLockedFee = rpc(aec_governance, name_claim_locked_fee, []),
    {ok, 200, #{<<"balance">> := Balance2}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    {ok, 200, #{<<"height">> := Height3}} = get_key_blocks_current_sut(),
    ?assertEqual(Balance2, Balance1 - Fee - ClaimLockedFee),

    %% Check that name entry is present
    EncodedNHash = aeser_api_encoder:encode(name, NHash),
    ExpectedTTL1 = (Height3 - 1) + aec_governance:name_claim_max_expiration(),
    {ok, 200, #{<<"id">>       := EncodedNHash,
                <<"ttl">>      := ExpectedTTL1,
                <<"pointers">> := []}} = get_names_entry_by_name_sut(Name),

    %% Submit name updated tx and check it is in mempool
    NameUpdateData = #{account_id => PubKeyEnc,
                       name_id    => aeser_api_encoder:encode(name, NHash),
                       client_ttl => TTL,
                       pointers   => Pointers,
                       name_ttl   => NameTTL,
                       fee        => Fee},
    {ok, 200, #{<<"tx">> := UpdateEnc}} = get_name_update(NameUpdateData),
    UpdateTxHash = sign_and_post_tx(UpdateEnc, PrivKey),

    %% Mine a block and check mempool empty again
    ok = wait_for_tx_hash_on_chain(UpdateTxHash),
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

    ok = wait_for_tx_hash_on_chain(SpendTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Only fee is lost as recipient = sender
    %% This tests 'resolve_name' because recipient is expressed by name label
    {ok, 200, #{<<"balance">> := Balance4}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance4, Balance3 - Fee),

    %% Submit name transfer tx and check it is in mempool
    TransferData = #{account_id   => PubKeyEnc,
                     recipient_id => PubKeyEnc,
                     name_id      => aeser_api_encoder:encode(name, NHash),
                     fee          => Fee},
    {ok, 200, #{<<"tx">> := TransferEnc}} = get_name_transfer(TransferData),
    TransferTxHash = sign_and_post_tx(TransferEnc, PrivKey),

    %% Mine a block and check mempool empty again
    ok = wait_for_tx_hash_on_chain(TransferTxHash),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check balance
    {ok, 200, #{<<"balance">> := Balance5}} = get_accounts_by_pubkey_sut(PubKeyEnc),
    ?assertEqual(Balance5, Balance4 - Fee),

    %% Submit name revoke tx and check it is in mempool
    RevokeData = #{account_id => PubKeyEnc,
                   name_id => aeser_api_encoder:encode(name, NHash),
                   fee => Fee},
    {ok, 200, #{<<"tx">> := RevokeEnc}} = get_name_revoke(RevokeData),
    RevokeTxHash = sign_and_post_tx(RevokeEnc, PrivKey),

    %% Mine a block and check mempool empty again
    ok = wait_for_tx_hash_on_chain(RevokeTxHash),
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
    Fee         = 2 * aec_test_utils:min_gas_price(),

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
        get_name_preclaim(#{commitment_id => aeser_api_encoder:encode(commitment, CHash),
                            fee => Fee,
                            account_id => aeser_api_encoder:encode(account_pubkey, random_hash())}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_claim(#{name => aeser_api_encoder:encode(name, Name),
                         name_salt => NameSalt,
                         account_id => aeser_api_encoder:encode(account_pubkey, random_hash()),
                         fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_update(#{account_id => aeser_api_encoder:encode(account_pubkey, random_hash()),
                          name_id => aeser_api_encoder:encode(name, NHash),
                          name_ttl => 5,
                          pointers => [],
                          client_ttl => 5,
                          fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_transfer(#{account_id => aeser_api_encoder:encode(account_pubkey, random_hash()),
                            recipient_id => aeser_api_encoder:encode(account_pubkey, random_hash()),
                            name_id => aeser_api_encoder:encode(name, NHash),
                            fee => Fee}),
    {ok, 404, #{<<"reason">> := <<"Account of account_id not found">>}} =
        get_name_revoke(#{account_id => aeser_api_encoder:encode(account_pubkey, random_hash()),
                          name_id => aeser_api_encoder:encode(name, NHash),
                          fee => Fee}),
    %% Check mempool still empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

assert_balance_at_least(Pubkey, MinExpectedBalance) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := Balance}} =
        get_accounts_by_pubkey_sut(Address),
    true = MinExpectedBalance =< Balance.

initialize_account(Amount) ->
    KeyPair = aecore_suite_utils:generate_key_pair(),
    initialize_account(Amount, KeyPair).

initialize_account(Amount, {Pubkey, Privkey}) ->
    Fee = ?SPEND_FEE,
    BlocksToMine = 3,

    Node = aecore_suite_utils:node_name(?NODE),

    aecore_suite_utils:mine_key_blocks(Node, BlocksToMine),

    {ok, 200, #{<<"tx">> := SpendTx}} =
        post_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Amount, Fee),
    TxHash = sign_and_post_tx(SpendTx),
    ok = wait_for_tx_hash_on_chain(TxHash),
    assert_balance_at_least(Pubkey, Amount),
    {Pubkey, Privkey}.

encode_call_data(Name, Fun, Args) when is_atom(Name) ->
    encode_call_data(contract_code(Name), Fun, Args);
encode_call_data(Src, Fun, Args) ->
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    {ok, aeser_api_encoder:encode(contract_bytearray, CallData)}.
wait_for_tx_hash_on_chain(TxHash) ->
    Node = aecore_suite_utils:node_name(?NODE),
    case rpc:call(Node, aec_chain, find_tx_location, [TxHash]) of
        BlockHash when is_binary(BlockHash) ->
            ct:log("TxHash is already on chain (~p)", [TxHash]),
            ok;
        _ ->
            Rate = aecore_suite_utils:expected_mine_rate(),
            Opts = #{strictly_follow_top => true},
            case aecore_suite_utils:mine_blocks_until_txs_on_chain(
                   aecore_suite_utils:node_name(?NODE), [TxHash], Rate, ?MAX_MINED_BLOCKS, Opts) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
    end.

contract_code(ContractName) ->
    {ok, BinSrc} = aect_test_utils:read_contract(ContractName),
    BinSrc.

contract_byte_code(ContractName) ->
    {ok, BinCode} = aect_test_utils:compile_contract(ContractName),
    aeser_api_encoder:encode(contract_bytearray, BinCode).

get_contract_bytecode(ContractName) ->
    {ok, contract_byte_code(ContractName)}.

peers(_Config) ->
    {ok, 200, #{<<"blocked">> := [], <<"peers">> := Peers}} = get_peers(),
    OkPeers = [ ok || P <- Peers, {ok, _} <- [aec_peers:parse_peer_address(P)] ],
    true = (length(OkPeers) == length(Peers)),

    %% ensure no peers
    lists:foreach(
        fun(Peer) -> rpc(aec_peers, del_peer, [Peer]) end,
        rpc(aec_peers, get_random, [all])),

    {ok, 200, #{<<"blocked">> := [], <<"peers">> := []}} = get_peers(),
    ok.

-define(pending_token_supply_pattern(X, Y, Z),
        #{ <<"accounts">> := X
         , <<"contracts">> := 0
         , <<"contract_oracles">> := 0
         , <<"locked">> := 0
         , <<"oracles">> := 0
         , <<"oracle_queries">> := 0
         , <<"pending_rewards">> := Y
         , <<"total">> := Z
         }).

sum_token_supply(_Config) ->
    Height = current_height(),
    case Height < 2 of
        true ->
            Mine = 2 - Height,
            NodeName = aecore_suite_utils:node_name(?NODE),
            aecore_suite_utils:mine_key_blocks(NodeName, Mine);
        false ->
            ok
    end,
    {ok, 200, Supply0} = get_token_supply_sut(0),
    {ok, 200, Supply1} = get_token_supply_sut(1),
    {ok, 200, Supply2} = get_token_supply_sut(2),
    Pending0 = rpc(?NODE, aec_coinbase, coinbase_at_height, [0]),
    Pending1 = rpc(?NODE, aec_coinbase, coinbase_at_height, [1]) + Pending0,
    Pending2 = rpc(?NODE, aec_coinbase, coinbase_at_height, [2]) + Pending1,
    ?assertMatch(?pending_token_supply_pattern(_, Pending0, _), Supply0),
    Accounts = maps:get(<<"accounts">>, Supply0),
    Total1 = Accounts + Pending1,
    ?assertMatch(?pending_token_supply_pattern(Accounts, Pending1, Total1),
                 Supply1),
    Total2 = Accounts + Pending2,
    ?assertMatch(?pending_token_supply_pattern(Accounts, Pending2, Total2),
                 Supply2),
    {ok, 400, #{<<"reason">> := <<"Chain too short">>}} =
        get_token_supply_sut(Height + 5),
    ok.

current_height() ->
    case rpc(aec_chain, top_header, []) of
        undefined -> 1;
        Header -> aec_headers:height(Header) + 1
    end.

get_token_supply_sut(Height) ->
    Host = internal_address(),
    HeightS = integer_to_list(Height),
    http_request(Host, get, "debug/token-supply/height/" ++ HeightS, []).

enabled_debug_endpoints(_Config) ->
    ?assertMatch({ok, 400, _}, post_key_blocks_sut(#{})),
    ?assertMatch({ok, 200, _}, get_peers()),
    ?assertMatch({ok, 400, _}, get_contract_create(#{})),
    ok.

disabled_debug_endpoints(_Config) ->
    {ok, Internal} = rpc(?NODE, application, get_env, [aehttp, internal]),
    NewInternal = proplists:delete(debug_endpoints, Internal),
    ok = rpc(?NODE, application, set_env, [aehttp, internal, NewInternal]),
    ?assertMatch(false, rpc(?NODE, aehttp_app, enable_internal_debug_endpoints, [])),

    %% post_key_blocks_sut should be always enabled
    ?assertMatch({ok, 400, _}, post_key_blocks_sut(#{})),
    ?assertMatch({ok, 403, _}, get_peers()),
    ?assertMatch({ok, 403, _}, get_contract_create(#{})),
    ok.

%% ============================================================
%% HTTP Requests
%% ============================================================

get_contract_create(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create", Data).

get_contract_call(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/call", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

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
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
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
    aeser_api_encoder:encode(account_pubkey, Pubkey).

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
%% Test HTTP cache headers
%% ============================================================

expires_cache_header(_Config) ->
    Host = external_address(),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc_request(get, {Host ++ "/v2/blocks/top", []}, [], []),

    true = proplists:is_defined("expires", Headers),
    Blocktime = case get_top_sut() of
        {ok, 200, #{<<"key_block">> := Block}} -> maps:get(<<"time">>, Block);
        {ok, 200, #{<<"micro_block">> := MicroBlock}} -> maps:get(<<"time">>, MicroBlock)
    end,
    ExpiresStr = proplists:get_value("expires", Headers),
    Expires = http_datetime_to_unixtime(ExpiresStr) * 1000, % to msecs
    true = Expires - Blocktime =< aec_governance:micro_block_cycle(),
    ok.

etag_cache_header(_Config) ->
    Host = external_address(),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc_request(get, {Host ++ "/v2/key-blocks/height/0", []}, [], []),

    true = proplists:is_defined("etag", Headers),
    ETag = proplists:get_value("etag", Headers),

    {ok, {{_, 304, _}, _, []}} =
        httpc_request(get, {Host ++ "/v2/key-blocks/height/0", [{"if-none-match", ETag}]}, [], []),
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

wrong_http_method_contract_call(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "debug/contracts/call", []).

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
    Fee = ?SPEND_FEE,
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
                #{recipient_id => aeser_api_encoder:encode(account_pubkey, random_hash()),
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
    Fee = ?SPEND_FEE,
    BlocksToMine = blocks_to_mine(Amount + Fee, 1),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    SpendData = #{recipient_id => aeser_api_encoder:encode(account_pubkey, RecipientPubkey),
                  amount => Amount,
                  fee => Fee},
    populate_block(#{spend_txs => [SpendData]}),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ok.

blocks_to_mine(Amount, ChecksCnt) ->
    Height = current_height(),
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
    {ok, SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} =
        case PrivKey =:= on_node of
            true  -> aecore_suite_utils:sign_on_node(?NODE, UnsignedTx);
            false -> {ok, aec_test_utils:sign_tx(UnsignedTx, PrivKey)}
        end,
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    post_transactions_sut(aeser_api_encoder:encode(transaction, SerializedTx)).

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

latest_sophia_abi() ->
    aect_test_utils:latest_sophia_abi_version().

latest_sophia_vm() ->
    aect_test_utils:latest_sophia_vm_version().

mine_micro_block_emptying_mempool_or_fail(_Node, 0) ->
    {error, retries_exhausted};
mine_micro_block_emptying_mempool_or_fail(Node, Retries) ->
    %% Very rarely we get two key-blocks followed by a microblock
    {ok, Blocks} = aecore_suite_utils:mine_micro_blocks(Node, 1),
    [MicroBlock, KeyBlock | _] = lists:reverse(Blocks),
    true = aec_blocks:is_key_block(KeyBlock),
    false = aec_blocks:is_key_block(MicroBlock),

    case rpc:call(Node, aec_tx_pool, peek, [infinity]) of
        {ok, []} ->
            {ok, [KeyBlock, MicroBlock]};
        {ok, [_|_] = MempoolTxs} ->
            %% So, Tx(s) is/are back in the mempool, this means (unless some Txs arrived
            %% from thin air) that we had a micro-fork. Let's check what state we stopped in
            timer:sleep(100), %% and avoid races...
            NewBlocks = aecore_suite_utils:flush_new_blocks(),
            case [ aec_blocks:type(B) || B <- NewBlocks ] of
                [key, micro] -> %% We had a microfork but then added the Tx again!
                    {ok, NewBlocks};
                [key]        -> %% We had a microfork and stopped, we can retry
                    mine_micro_block_emptying_mempool_or_fail(Node, Retries - 1);
                _Other       ->
                    ct:log("Key block mined shortly after micro block:\nTxs in mempool: ~p\n"
                           "More blocks mined: ~p\n", [MempoolTxs, NewBlocks]),
                    {error, micro_block_pushed_out_of_chain}
            end
    end.

http_datetime_to_unixtime(S) ->
    BaseSecs = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    ExpiresDt = httpd_util:convert_request_date(S),
    calendar:datetime_to_gregorian_seconds(ExpiresDt) - BaseSecs.

simple_auth_meta(Owner, Secret, GANonce, InnerTx) ->
    AuthData = simple_auth(Secret, GANonce),
    meta(Owner, AuthData, InnerTx).

simple_auth(Secret, Nonce) ->
    aega_test_utils:make_calldata("simple_auth", "authorize", [Secret, Nonce]).

meta(Owner, AuthData, InnerTx) ->
    aecore_suite_utils:meta_tx(Owner, #{}, AuthData, InnerTx).

account_type(Pubkey) ->
    {value, Account} = rpc(aec_chain, get_account, [Pubkey]),
    aec_accounts:type(Account).

