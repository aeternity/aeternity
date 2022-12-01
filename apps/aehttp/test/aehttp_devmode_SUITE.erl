-module(aehttp_devmode_SUITE).

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

%% Exports for other tests
-export(
   [ initialize_account/1
   , get_name_preclaim/1
   , get_name_claim/1
   , get_names_entry_by_name_sut/1
   , get_commitment_id/2
   , get_accounts_by_pubkey_sut/1
   , get_accounts_by_pubkey_and_height_sut/2
   , get_accounts_next_nonce_sut/1
   , get_transactions_by_hash_sut/1
   , check_transaction_in_pool_sut/1
   , get_contract_call_object/1
   , get_top_block/1
   , get_top_header/1
   , get_chain_ends/1
   , wait_for_tx_hash_on_chain/1
   , sign_and_post_tx/2
   , end_per_testcase_all/1
   , get_spend/1
   , post_transactions_sut/1
   , get_transactions_pending_sut/0
   , delete_tx_from_mempool_sut/1
   , get_key_blocks_current_height_sut/0
   ]).

-export(
   [
    get_current_key_block/1,
    get_current_key_block_hash/1,
    get_current_key_block_height/1,
    %% get_pending_key_block/1,
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
    get_transaction_info_by_hash_sut/1,
    post_spend_tx/1,
    post_spend_tx_w_hash_sig/1,
    post_contract_and_call_tx/1,
    nonce_limit/1,
    get_contract_create/1,
    get_contract_call/1,
    get_contract_bytecode/1,
    encode_call_data/3
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
    named_oracle_transactions/1,
    nameservice_transactions/1,
    spend_transaction/1,
    next_nonce_missing_nonce/1,
    state_channels_onchain_transactions/1,
    unknown_atom_in_spend_tx/1,

    get_transaction/1,
    check_transaction_in_pool/1,

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

%%
%% test case exports
%% rollback combined with dev mode
-export([
    rollback_returns_to_dev_mode/1,
    repeated_rollbacks_basic/1,
    repeated_rollbacks/1,
    repeated_rollbacks_to_key_hash/1,
    repeated_rollbacks_to_key_hash_multiple_blocks/1,
    repeated_rollbacks_to_micro_hash/1,
    rollback_when_gc_enabled/1,
    rollback_when_gc_enabled_and_beyond_kept_history/1
    ]).

-export([post_paying_for_tx/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).
-define(SPEND_FEE, 20000 * min_gas_price()).

-define(MAX_MINED_BLOCKS, 20).

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [sequence],
      [{group, swagger2},
       {group, oas3}]},
     {swagger2, [sequence],
      [
       %% /key-blocks/* /micro-blocks/* /generations/* status/chain-ends
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
       {group, naming},
       {group, paying_for_tx},
       {group, rollback}
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
       get_chain_ends,
       get_current_key_block,
       get_current_key_block_hash,
       get_current_key_block_height,
       %% get_pending_key_block,
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
        named_oracle_transactions,
        nameservice_transactions,
        spend_transaction,
        next_nonce_missing_nonce,
        state_channels_onchain_transactions,
        unknown_atom_in_spend_tx,

        get_transaction,
        check_transaction_in_pool,

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
      ]},
     {oas3, [sequence],
      [
       %% /key-blocks/* /micro-blocks/* /generations/* status/chain-ends
       {group, oas_block_endpoints},
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
       {group, naming},
       {group, paying_for_tx},
       {group, rollback}
      ]},
     {oas_block_endpoints, [sequence],
      [
       {group, oas_on_genesis_block} %% standalone
      ]},
     {oas_on_genesis_block, [sequence],
      [
       {group, oas_block_info}
      ]},
     {oas_block_info, [sequence],
      [
       get_top_header,
       get_chain_ends,
       get_current_key_block,
       get_current_key_block_hash,
       get_current_key_block_height,
       %% get_pending_key_block,
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
     {paying_for_tx, [sequence],
      [post_paying_for_tx]},
     {rollback, [sequence],
      [rollback_returns_to_dev_mode,
       repeated_rollbacks_basic,
       repeated_rollbacks,
       repeated_rollbacks_to_key_hash,
       repeated_rollbacks_to_key_hash_multiple_blocks,
       repeated_rollbacks_to_micro_hash,
       rollback_when_gc_enabled,
       rollback_when_gc_enabled_and_beyond_kept_history
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    NwId = <<"ae_dev">>,
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => false,
                     <<"consensus">> =>
                         #{<<"0">> => #{<<"name">> => <<"on_demand">>}}},
               <<"fork_management">> =>
                   #{<<"network_id">> => NwId},
               <<"mining">> =>
                   #{<<"name_claim_bid_timeout">> => 0 %% NO name auctions
                    }},
    %% The CT node is started (from Makefile) using e.g. the config/test-iris.config
    %% which typically sets the network id (via aecore) to <<"local_iris_testnet">>.
    %% This affects signatures, since part of that work is done in the CT node.
    %% We don't want that, so we stuff the aecore env with the <<"ae_dev">> nw id,
    %% then restore, if needed, in end_per_suite/1.
    OldNwId = application:get_env(aecore, network_id),
    application:set_env(aecore, network_id, NwId),
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [{symlink_name, "latest.http_devmode"}, {test_module, ?MODULE}] ++ Config),
    Config2 = [ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
              , {started_apps, StartedApps}
              , {old_network_id, OldNwId} ]  ++ Config1,
    aecore_suite_utils:start_node(?NODE, Config2),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    true = rpc(aecore_env, is_dev_mode, []),
    %% a bit hacky way but since the CT setup does not come with default hard
    %% forks, we can not rely on the
    %% aec_hard_forks:protocols_from_network_id/1 as the ae_dev is a bit
    %% special. Instead we start the node and use the values set there instead
    Forks =
        maps:fold(fun(K, V, Acc) ->
                          Acc#{integer_to_binary(K) => V}
                  end, #{}, rpc(aec_hard_forks, protocols, [])),
    application:set_env(aecore, hard_forks, Forks),
    [{node, Node} | Config2].

end_per_suite(Config) ->
    case ?config(old_network_id, Config) of
        {ok, OldNwId} ->
            application:set_env(aecore, network_id, OldNwId);
        _ ->
            ok
    end,
    aecore_suite_utils:stop_node(?NODE, Config),
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(all, Config) ->
    Config;
init_per_group(SwaggerVsn, Config) when SwaggerVsn =:= swagger2;
                                        SwaggerVsn =:= oas3 ->
    [{swagger_version, SwaggerVsn} | Config];
init_per_group(Group, Config) when
      Group =:= debug_endpoints;
      Group =:= account_endpoints;
      Group =:= transaction_endpoints;
      %%Group =:= contract_endpoint;
      Group =:= oracle_endpoints;
      Group =:= name_endpoints;
      %%Group =:= channel_endpoints;
      Group =:= peer_endpoints;
      Group =:= status_endpoints ->
    Node = aecore_suite_utils:node_name(?NODE),
    ToMine = aecore_suite_utils:latest_fork_height() + 1,
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    Protocols = aec_hard_forks:sorted_protocol_versions(),
    {ok, KeyBlocks} = aecore_suite_utils:mine_key_blocks(Node, length(Protocols) + 1),
    KeyBlock = lists:last(KeyBlocks),
    true = aec_blocks:is_key_block(KeyBlock),
    [{current_block, KeyBlock},
     {current_block_hash, hash(key, KeyBlock)},
     {current_block_hash_wrong_type, hash(micro, KeyBlock)},
     {current_block_height, aec_blocks:height(KeyBlock)},
     {current_block_type, key_block} | Config];
%% block_endpoints
init_per_group(BlockEndpoints, Config) when BlockEndpoints =:= block_endpoints;
                                            BlockEndpoints =:= oas_block_endpoints ->
    ok = rpc(aec_conductor, reinit_chain, []),
    Config;
init_per_group(OnGenesis, Config) when OnGenesis =:= on_genesis_block;
                                       OnGenesis =:= oas_on_genesis_block ->
    rpc(aec_conductor, reinit_chain, []),
    GenesisBlock = rpc(aec_chain, genesis_block, []),
    [{current_block, GenesisBlock},
     {current_block_hash, hash(key, GenesisBlock)},
     {current_block_hash_wrong_type, hash(micro, GenesisBlock)},
     {current_block_height, 0},
     {current_block_type, genesis_block} | Config];
init_per_group(Group, Config) when Group =:= on_key_block;
                                   Group =:= rollback ->
    Node = aecore_suite_utils:node_name(?NODE),
    ToMine = aecore_suite_utils:latest_fork_height() + 1,
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    Protocols = aec_hard_forks:sorted_protocol_versions(),
    {ok, KeyBlocks} = aecore_suite_utils:mine_key_blocks(Node, length(Protocols) + 1),
    KeyBlock = lists:last(KeyBlocks),
    true = aec_blocks:is_key_block(KeyBlock),
    [{current_block, KeyBlock},
     {current_block_hash, hash(key, KeyBlock)},
     {current_block_hash_wrong_type, hash(micro, KeyBlock)},
     {current_block_height, aec_blocks:height(KeyBlock)},
     {current_block_type, key_block} | Config];
init_per_group(on_micro_block, Config) ->
    [ {NodeId, Node} | _ ] = ?config(nodes, Config),
    %% Mine at least 2 key blocks (fork height may be 0).
    ToMine = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [_KeyBlock0]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    %% Send spend tx so it gets included into micro block.
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    rpc(lager, log, [debug, test_case, "init_per_group(on_micro_block, Config)"]),
    {ok, Tx} = aecore_suite_utils:spend(Node, Pub, Pub, 1, ?SPEND_FEE),
    {ok, [Tx]} = rpc:call(Node, aec_tx_pool, peek, [infinity]),
    ct:log("Spend tx ~p", [Tx]),
    case aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(Node) of
        {ok, Blocks} ->
            MicroBlock = lists:last(Blocks),
            KeyBlock = get_prev_key_block(Blocks),
            true = aec_blocks:is_key_block(KeyBlock),
            false = aec_blocks:is_key_block(MicroBlock),
            [{prev_key_block, KeyBlock},
             {prev_key_block_hash, hash(key, KeyBlock)},
             {prev_key_block_height, aec_blocks:height(KeyBlock)},
             {current_block, MicroBlock},
             {current_block_hash, hash(micro, MicroBlock)},
             {current_block_height, aec_blocks:height(KeyBlock)},
             {current_block_txs, [Tx]},
             {current_block_type, micro_block} | Config];
        {error, Reason} ->
            ct:fail({could_not_setup_on_micro_block, Reason})
    end;
init_per_group(block_info, Config) ->
    Config;
%% account_endpoints
init_per_group(nonexistent_account, Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    {_, Pubkey} = aecore_suite_utils:sign_keys(?NODE),
    [{account_id, aeser_api_encoder:encode(account_pubkey, Pubkey)},
     {account_exists, false} | Config];
init_per_group(account_with_balance, Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    [ {NodeId, Node} | _ ] = ?config(nodes, Config),
    {_, Pubkey} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock1, KeyBlock2]} = aecore_suite_utils:mine_key_blocks(Node, 2),
    true = aec_blocks:is_key_block(KeyBlock1),
    true = aec_blocks:is_key_block(KeyBlock2),
    [{account_id, aeser_api_encoder:encode(account_pubkey, Pubkey)},
     {account_exists, true} | Config];
init_per_group(account_with_pending_tx, Config) ->
    aehttp_integration_SUITE:init_per_group(account_with_pending_tx, Config);
init_per_group(account_info, Config) ->
    Config;
%% transaction_endpoints
init_per_group(nonexistent_tx, Config) ->
    Config;
init_per_group(tx_is_pending, Config) ->
    [ {NodeId, Node} | _ ] = ?config(nodes, Config),
    ok = rpc(aec_conductor, reinit_chain, []),
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
    aehttp_integration_SUITE:init_per_group(Group, Config);
init_per_group(post_tx_to_mempool, Config) ->
    [ {NodeId, Node} | _ ] = ?config(nodes, Config),
    ok = rpc(aec_conductor, reinit_chain, []),
    {_, Pub} = aecore_suite_utils:sign_keys(NodeId),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    [{sender_id, aeser_api_encoder:encode(account_pubkey, Pub)},
     {recipient_id, aeser_api_encoder:encode(account_pubkey, random_hash())},
     {amount, 1},
     {fee, ?SPEND_FEE},
     {payload, <<"foo">>} | Config];
init_per_group(tx_info, Config) ->
    Config;
%% contract_endpoints
%% oracle_endpoints
init_per_group(nonexistent_oracle, Config) ->
    Config;
init_per_group(oracle_txs, Config) ->
    Node = aecore_suite_utils:node_name(?NODE),
    ok = rpc(aec_conductor, reinit_chain, []),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    Config;
%% name_endpoints
init_per_group(nonexistent_name, Config) ->
    Config;
init_per_group(name_txs, _Config) ->
    {skip, not_implemented};

init_per_group(external_endpoints, Config) ->
    [ {_, Node} | _ ] = ?config(nodes, Config),
    ok = rpc(aec_conductor, reinit_chain, []),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    Config;

init_per_group(Group, Config) ->
    aehttp_integration_SUITE:init_per_group(Group, Config).

end_per_group(Group, Config) ->
    aehttp_integration_SUITE:end_per_group(Group, Config).

init_per_testcase(Test, Config) ->
    aehttp_integration_SUITE:init_per_testcase(Test, Config).

end_per_testcase(Case, Config) ->
    aehttp_integration_SUITE:end_per_testcase(Case, Config).

end_per_testcase_all(Config) ->
    aehttp_integration_SUITE:end_per_testcase_all(Config).

%% This has been added in the devmode suite to increase the chances of
%% testcases passing. The integration suite stuffs current block data in
%% the config, and there may be minor differences when using devmode (not such
%% that semantics are changed, but assumptions of testcases may be broken).
cur_config(Config) ->
    TopBlock = rpc(aec_chain, top_block, []),
    IntType = aec_blocks:type(TopBlock),
    BlockHash = hash(IntType, TopBlock),
    Height = aec_blocks:height(TopBlock),
    {Type, Txs} = case {Height, IntType} of
               {0, key} -> {genesis_block, []};
               {_, key} -> {key_block, []};
               {_, micro} -> {micro_block, rpc(aec_blocks, txs, [TopBlock])}
           end,
    Config1 = [ {current_block, TopBlock}
              , {current_block_hash, BlockHash}
              , {current_block_height, Height}
              , {current_block_type, Type}
              , {current_block_txs, Txs} | Config],
    ct:log("curr_config => ~p", [Config1]),
    Config1.

%% ============================================================
%% Test cases
%% ============================================================

%% /status/chain-ends

get_chain_ends(Config) ->
    %%
    %% This is modified, and differs from the integration suite
    %%
    TopHdr = rpc(aec_chain, top_header, []),
    Height = aec_headers:height(TopHdr),
    case Height > 1 of
        true -> ok;
        false ->
            aehttp_integration_SUITE:get_chain_ends(Config)
    end.

%% /blocks/top

get_top_block(Config) ->
    aehttp_integration_SUITE:get_top_block(Config).

get_top_sut() ->
    Host = external_address(),
    http_request(Host, get, "blocks/top", []).

get_top_header_sut() ->
    get_top_header_sut([]).

get_top_header_sut(Opts) ->
    Host = external_address(),
    http_request(Host, get, "headers/top", Opts).

%% /key-blocks/*

get_current_key_block(Config) ->
    aehttp_integration_SUITE:get_current_key_block(cur_config(Config)).

get_current_key_block_hash(Config) ->
    aehttp_integration_SUITE:get_current_key_block_hash(cur_config(Config)).

get_current_key_block_height(Config) ->
    aehttp_integration_SUITE:get_current_key_block_height(cur_config(Config)).

%% get_pending_key_block(Config) ->
%%     aehttp_integration_SUITE:get_pending_key_block(Config).

get_key_block_by_hash(Config) ->
    aehttp_integration_SUITE:get_key_block_by_hash(Config).

get_key_block_by_height(Config) ->
    aehttp_integration_SUITE:get_key_block_by_height(Config).

post_key_block(Config) ->
    post_key_block(?config(current_block_type, Config), Config).

post_key_block(_CurrentBlockType, _Config) ->
    %%
    %% This is modified, and differs from the integration suite
    %%
    {ok, 200, #{<<"height">> := _Height} = PendingKeyBlock} = get_key_blocks_pending_sut(),

    KeyBlock = PendingKeyBlock#{<<"pow">> => lists:duplicate(42, 1), <<"nonce">> => 1},
    Node = aecore_suite_utils:node_name(?NODE),
    NetwId = rpc:call(Node, aec_governance, get_network_id, []),
    ct:log("Network ID: ~p", [NetwId]),
    {ok, 200, #{}} = post_key_blocks_sut(KeyBlock),
    %% Block is normally rejected due to pow, but this is devmode
    ok.

get_key_blocks_current_height_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current/height", []).

get_key_blocks_pending_sut() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/pending", []).

post_key_blocks_sut(KeyBlock) ->
    Host = internal_address(),
    http_request(Host, post, "key-blocks", KeyBlock).

%% /micro-blocks/*

get_micro_block_header_by_hash(Config) ->
    aehttp_integration_SUITE:get_micro_block_header_by_hash(Config).

get_micro_block_transactions_by_hash(Config) ->
    aehttp_integration_SUITE:get_micro_block_transactions_by_hash(cur_config(Config)).

get_micro_block_transactions_count_by_hash(Config) ->
    aehttp_integration_SUITE:get_micro_block_transactions_count_by_hash(Config).

get_micro_block_transaction_by_hash_and_index(Config) ->
    aehttp_integration_SUITE:get_micro_block_transaction_by_hash_and_index(Config).

%% /generations/*

get_generation_current(Config) ->
    aehttp_integration_SUITE:get_generation_current(cur_config(Config)).

get_generation_by_hash(Config) ->
    aehttp_integration_SUITE:get_generation_by_hash(cur_config(Config)).

get_generation_by_height(Config) ->
    aehttp_integration_SUITE:get_generation_by_height(cur_config(Config)).

%% /accounts/*

get_account_by_pubkey(Config) ->
    aehttp_integration_SUITE:get_account_by_pubkey(Config).

get_account_by_pubkey_and_height(Config) ->
    aehttp_integration_SUITE:get_account_by_pubkey_and_height(Config).

get_pending_account_transactions_by_pubkey(Config) ->
    aehttp_integration_SUITE:get_pending_account_transactions_by_pubkey(Config).

get_accounts_by_pubkey_sut(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ aeu_uri:encode(Id), []).

get_accounts_next_nonce_sut(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ binary_to_list(Id) ++ "/next-nonce", []).

get_accounts_by_pubkey_and_height_sut(Id, Height) ->
    Host = external_address(),
    IdS = binary_to_list(aeu_uri:encode(Id)),
    HeightS = integer_to_list(Height),
    http_request(Host, get, "accounts/" ++ IdS ++ "/height/" ++ HeightS, []).

get_transactions_pending_sut() ->
    Host = internal_address(),
    http_request(Host, get, "debug/transactions/pending", []).

delete_tx_from_mempool_sut(Hash) when is_binary(Hash) ->
    delete_tx_from_mempool_sut(binary_to_list(Hash));
delete_tx_from_mempool_sut(Hash) when is_list(Hash) ->
    Host = internal_address(),
    http_request(Host, delete, "node/operator/mempool/hash/" ++ Hash, []).

%% /transactions/*

get_transaction_by_hash(Config) ->
    aehttp_integration_SUITE:get_transaction_by_hash(Config).

get_transaction_info_by_hash(_Config) ->
    {skip, not_implemented}.

post_spend_tx(Config) ->
    aehttp_integration_SUITE:post_spend_tx(Config).

post_spend_tx_w_hash_sig(Config) ->
    aehttp_integration_SUITE:post_spend_tx_w_hash_sig(Config).

nonce_limit(Config) ->
    aehttp_integration_SUITE:nonce_limit(Config).

post_contract_and_call_tx(Config) ->
    aehttp_integration_SUITE:post_contract_and_call_tx(Config).

get_transactions_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ aeu_uri:encode(Hash), []).

check_transaction_in_pool_sut(Hash) ->
    Host = internal_address(),
    http_request(Host, get, "debug/check-tx/pool/" ++ aeu_uri:encode(Hash), []).

get_transaction_info_by_hash_sut(Hash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ aeu_uri:encode(Hash) ++ "/info", []).

post_transactions_sut(Tx) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => Tx}).

%% /contracts/*

get_contract(Config) ->
    aehttp_integration_SUITE:get_contract(Config).

% /oracles/*

get_oracle_by_pubkey(Config) ->
    aehttp_integration_SUITE:get_oracle_by_pubkey(Config).

post_oracle_register(Config) ->
    aehttp_integration_SUITE:post_oracle_register(Config).

post_oracle_extend(Config) ->
    aehttp_integration_SUITE:post_oracle_extend(Config).

post_oracle_query(Config) ->
    aehttp_integration_SUITE:post_oracle_query(Config).

post_oracle_response(Config) ->
    aehttp_integration_SUITE:post_oracle_response(Config).

%% /names/*

get_name_entry_by_name(_Config) ->
    NonexistentName = <<"Nonexistent_name">>,
    {ok, 400, _Error} = get_names_entry_by_name_sut(NonexistentName),
    ok.

get_names_entry_by_name_sut(Name) ->
    Host = external_address(),
    http_request(Host, get, "names/" ++ Name, []).

%% /channels/*

get_channel_by_pubkey(Config) ->
    aehttp_integration_SUITE:get_channel_by_pubkey(Config).

%% /peers/*

get_peer_pubkey(_Config) ->
    {ok, 200, _PeerPubkey} = get_peers_pubkey_sut(),
    ok.

get_peers_pubkey_sut() ->
    Host = external_address(),
    http_request(Host, get, "peers/pubkey", []).

%% /status/*

get_status(Config) ->
    aehttp_integration_SUITE:get_status(Config).

hash(key, Block) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(Block),
    aeser_api_encoder:encode(key_block_hash, Hash0);
hash(micro, Block) ->
    {ok, Hash0} = aec_blocks:hash_internal_representation(Block),
    aeser_api_encoder:encode(micro_block_hash, Hash0).

%% endpoints

%% tests the following
%% GET contract_create_tx unsigned transaction
%% GET contract_call_tx unsigned transaction
%% due to complexity of contract_call_tx (needs a contract in the state tree)
%% both positive and negative cases are tested in this test
contract_transactions(Config) ->    % miner has an account
    aehttp_integration_SUITE:contract_transactions(Config).

contract_create_transaction_init_error(Config) ->
    aehttp_integration_SUITE:contract_create_transaction_init_error(Config).

oracle_transactions(Config) ->
    aehttp_integration_SUITE:oracle_transactions(Config).

named_oracle_transactions(Config) ->
    aehttp_integration_SUITE:named_oracle_transactions(Config).

%% tests the following
%% GET preclaim_tx unsigned transaction
%% GET claim_tx unsigned transaction
%% GET update_tx unsigned transaction
%% GET transfer_tx unsigned transaction
%% GET revoke_tx unsigned transaction
nameservice_transactions(Config) ->
    aehttp_integration_SUITE:nameservice_transactions(Config).

%% tests the following
%% GET channel_create_tx unsigned transaction
%% GET channel_deposit_tx unsigned transaction
%% GET channel_withdraw_tx unsigned transaction
%% GET channel_close_mutual_tx unsigned transaction
%% GET channel_close_solo unsigned transaction
%% GET channel_slash_tx unsigned transaction
%% GET channel_settle_tx unsigned transaction
state_channels_onchain_transactions(Config) ->
    aehttp_integration_SUITE:state_channels_onchain_transactions(Config).

%% tests the following
%% GET spend_tx unsigned transaction
spend_transaction(Config) ->
    aehttp_integration_SUITE:spend_transaction(Config).

next_nonce_missing_nonce(Config) ->
    aehttp_integration_SUITE:next_nonce_missing_nonce(Config).

%% tests the following
%% GET spend_tx unsigned transaction with an non-present key in request
unknown_atom_in_spend_tx(Config) ->
    aehttp_integration_SUITE:unknown_atom_in_spend_tx(Config).

get_transaction(Config) ->
    aehttp_integration_SUITE:get_transaction(Config).

check_transaction_in_pool(Config) ->
    aehttp_integration_SUITE:check_transaction_in_pool(Config).

%% Maybe this test should be broken into a couple of smaller tests
%% it currently tests the positive cases for
%% GET externalAPI/transactions
%% POST internalAPI/debug/transactions/spend
%% GET externalAPI/account/balance
pending_transactions(Config) ->
    aehttp_integration_SUITE:pending_transactions(Config).

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(Config) ->
    aehttp_integration_SUITE:post_correct_tx(Config).

post_broken_tx(Config) ->
    aehttp_integration_SUITE:post_broken_tx(Config).

post_broken_api_encoded_tx(Config) ->
    aehttp_integration_SUITE:post_broken_api_encoded_tx(Config).

%% positive test of spend_tx is handled in pending_transactions test
broken_spend_tx(Config) ->
    aehttp_integration_SUITE:broken_spend_tx(Config).

node_pubkey(Config) ->
    aehttp_integration_SUITE:node_pubkey(Config).

node_beneficiary(Config) ->
    aehttp_integration_SUITE:node_beneficiary(Config).

peer_pub_key(Config) ->
    aehttp_integration_SUITE:peer_pub_key(Config).

naming_system_manage_name(Config) ->
    aehttp_integration_SUITE:naming_system_manage_name(Config).

naming_system_broken_txs(Config) ->
    aehttp_integration_SUITE:naming_system_broken_txs(Config).

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

peers(Config) ->
    aehttp_integration_SUITE:peers(Config).

sum_token_supply(Config) ->
    aehttp_integration_SUITE:sum_token_supply(Config).

enabled_debug_endpoints(Config) ->
    aehttp_integration_SUITE:enabled_debug_endpoints(Config).

disabled_debug_endpoints(Config) ->
    aehttp_integration_SUITE:disabled_debug_endpoints(Config).

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

get_name_preclaim(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/preclaim", Data).

get_name_claim(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/names/claim", Data).

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

%% ============================================================
%% Test swagger validation errors
%% ============================================================

swagger_validation_body(Config) ->
    aehttp_integration_SUITE:swagger_validation_body(Config).

swagger_validation_schema(Config) ->
    aehttp_integration_SUITE:swagger_validation_schema(Config).

%% ============================================================
%% Test CORS headers
%% ============================================================

cors_not_returned_when_origin_not_sent(Config) ->
    aehttp_integration_SUITE:cors_not_returned_when_origin_not_sent(Config).

cors_returned_on_preflight_request(Config) ->
    aehttp_integration_SUITE:cors_returned_on_preflight_request(Config).

cors_returned_on_get_request(Config) ->
    aehttp_integration_SUITE:cors_returned_on_get_request(Config).

%% ============================================================
%% Test HTTP cache headers
%% ============================================================

expires_cache_header(Config) ->
    aehttp_integration_SUITE:expires_cache_header(Config).

etag_cache_header(Config) ->
    aehttp_integration_SUITE:etag_cache_header(Config).

%% ============================================================
%% Test Cowboy API handler
%% ============================================================

charset_param_in_content_type(Config) ->
    aehttp_integration_SUITE:charset_param_in_content_type(Config).

%% ============================================================
%% HTTP Requests with wrong method
%% ============================================================

wrong_http_method_top(Config) ->
    aehttp_integration_SUITE:wrong_http_method_top(Config).

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
%% rollback test cases
%% ============================================================

rollback_returns_to_dev_mode(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    dev_mode = rpc:call(Node, app_ctrl, get_mode, []),
    Height = rpc:call(Node, aec_chain, top_height, []),
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ok = do_rollback(Node, Height),
    dev_mode = rpc:call(Node, app_ctrl, get_mode, []),
    ok.

repeated_rollbacks_basic(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    Height = rpc:call(Node, aec_chain, top_height, []),
    TxPool = rpc:call(Node, erlang, whereis, [aec_tx_pool]),
    Action = fun() ->
                     {ok,_} = aecore_suite_utils:mine_key_blocks(Node, 1),
                     ok
             end,
    ok = do_rollback(Node, Height),  % verifies height
    ok = Action(),
    ok = do_rollback(Node, Height),
    ok = Action(),
    ok = do_rollback(Node, Height),
    ok = Action(),
    TxPool = rpc:call(Node, erlang, whereis, [aec_tx_pool]),
    ok.

repeated_rollbacks(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    Height = rpc:call(Node, aec_chain, top_height, []),
    Action = fun() ->
                     aecore_suite_utils:mine_key_blocks(Node, 1),
                     ok = post_contract_and_call_tx(Config),
                     {ok,_} = aecore_suite_utils:mine_key_blocks(Node, 1),
                     ok
             end,
    ok = do_rollback(Node, Height),  % verifies height
    ok = Action(),
    ok = do_rollback(Node, Height),
    ok = Action(),
    ok = do_rollback(Node, Height),
    ok = Action().

repeated_rollbacks_to_key_hash(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 1),
    Hash = rpc:call(Node, aec_chain, top_block_hash, []),
    ct:log("Top hash is ~p", [Hash]),
    Action = fun() ->
                     ok = post_contract_and_call_tx(Config)
             end,
    ok = do_rollback_to_keyblock(Node, Hash),  % verifies hash
    ok = Action(),
    ok = do_rollback_to_keyblock(Node, Hash),
    ok = Action(),
    ok = do_rollback_to_keyblock(Node, Hash),
    ok = Action().

repeated_rollbacks_to_key_hash_multiple_blocks(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 1),
    Hash = rpc:call(Node, aec_chain, top_block_hash, []),
    ct:log("Top hash is ~p", [Hash]),
    Action = fun() ->
                     aecore_suite_utils:mine_key_blocks(Node, 3),
                     ok = post_contract_and_call_tx(Config)
             end,
    ok = do_rollback_to_keyblock(Node, Hash),  % verifies hash
    ok = Action(),
    ok = do_rollback_to_keyblock(Node, Hash),
    ok = Action(),
    ok = do_rollback_to_keyblock(Node, Hash),
    ok = Action().

repeated_rollbacks_to_micro_hash(Config0) ->
    Config = init_per_group(on_micro_block, Config0),
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    Hash = rpc:call(Node, aec_chain, top_block_hash, []),
    ct:log("Top hash is ~p", [Hash]),
    Action = fun() ->
                     ok = post_contract_and_call_tx(Config)
             end,
    ok = do_rollback_to_microblock(Node, Hash),  % verifies hash
    ok = Action(),
    ok = do_rollback_to_microblock(Node, Hash),
    ok = Action(),
    ok = do_rollback_to_microblock(Node, Hash),
    ok = Action().

rollback_when_gc_enabled(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    #{enabled  := false
    , interval := Interval
    , history  := History} = rpc:call(Node, aec_db_gc, config, []),
    ShortHistory = 51,
    enable_gc(Node, ShortHistory),
    #{enabled  := true 
    , interval := Interval
    , history  := ShortHistory} = rpc:call(Node, aec_db_gc, config, []),
    Height= rpc:call(Node, aec_chain, top_height, []),
    BlocksCnt = 3,
    true = BlocksCnt < ShortHistory,
    aecore_suite_utils:mine_key_blocks(Node, BlocksCnt),
    ok = do_rollback(Node, Height),
    ct:log("Top height ~p", [Height]),
    disable_gc(Node, History),
    #{enabled  := false
    , interval := Interval
    , history  := History} = rpc:call(Node, aec_db_gc, config, []),
    ok.

rollback_when_gc_enabled_and_beyond_kept_history(Config) ->
    [{_, Node}] = ?config(nodes, Config), % important that there is only one
    #{enabled  := false
    , interval := Interval
    , history  := History} = rpc:call(Node, aec_db_gc, config, []),
    ShortHistory = 51,
    enable_gc(Node, ShortHistory),
    #{enabled  := true 
    , interval := Interval
    , history  := ShortHistory} = rpc:call(Node, aec_db_gc, config, []),
    Height= rpc:call(Node, aec_chain, top_height, []),
    BlocksCnt = 53,
    true = BlocksCnt > ShortHistory,
    {ok, _} = aecore_suite_utils:mine_key_blocks(Node, BlocksCnt),
    ok = fail_rollback(Node, Height),
    ct:log("Top height ~p", [Height]),
    disable_gc(Node, History),
    #{enabled  := false
    , interval := Interval
    , history  := History} = rpc:call(Node, aec_db_gc, config, []),
    ok.


enable_gc(N, History) ->
    %% note that this only changes the config and GC is not really started
    Config
        = #{<<"chain">> => #{ <<"garbage_collection">> => #{ <<"enabled">> => true
                                                           , <<"history">> => History}}},
    rpc:call(N, aeu_env, update_config, [Config, _Notify = true, _InfoReport = silent]),
    ok.

disable_gc(N, History) ->
    Config
        = #{<<"chain">> => #{ <<"garbage_collection">> => #{ <<"enabled">> => false 
                                                           , <<"history">> => History}}},
    rpc:call(N, aeu_env, update_config, [Config, _Notify = true, _InfoReport = silent]),
    ok.


do_rollback(N, Height) ->
    attempt_rollback(N, Height),
    NewHeight = rpc:call(N, aec_chain, top_height, []),
    {NewHeight, Height} = {Height, NewHeight},
    ok.

fail_rollback(N, Height) ->
    InitialHeight = rpc:call(N, aec_chain, top_height, []),
    attempt_rollback(N, Height),
    {InitialHeight, InitialHeight} =
        {InitialHeight, rpc:call(N, aec_chain, top_height, [])},
    ok.

attempt_rollback(N, Height) ->
    NSetupHome = rpc:call(N, setup, home, []),
    NAeCmd = filename:join([NSetupHome, "bin", "aeternity"]),
    NRBCmd = NAeCmd ++ " db_rollback -h " ++ integer_to_list(Height),
    ct:log("NRBCmd: ~p", [NRBCmd]),
    NRBCmdRes = os:cmd(NRBCmd),
    ct:log("NRBCmdRes = ~n"
           "=========================================~n"
           "~s~n"
           "=========================================", [NRBCmdRes]),
    ok.

do_rollback_to_microblock(N, Hash) ->
    do_rollback_to_blockhash(N, micro, Hash).

do_rollback_to_keyblock(N, Hash) ->
    do_rollback_to_blockhash(N, key, Hash).

do_rollback_to_blockhash(N, Type, Hash) ->
    Hdr = rpc:call(N, aec_db, get_header, [Hash]),
    Type = aec_headers:type(Hdr),  % assertion
    EncType = case Type of
                  key   -> key_block_hash;
                  micro -> micro_block_hash
              end,
    EncHash = aeser_api_encoder:encode(EncType, Hash),
    NSetupHome = rpc:call(N, setup, home, []),
    NAeCmd = filename:join([NSetupHome, "bin", "aeternity"]),
    NRBCmd = NAeCmd ++ " db_rollback -b " ++ binary_to_list(EncHash),
    NRBCmdRes = os:cmd(NRBCmd),
    ct:log("NRBCmdRes = ~n"
           "=========================================~n"
           "~s~n"
           "=========================================", [NRBCmdRes]),
    NewTop = rpc:call(N, aec_chain, top_block_hash, []),
    {NewTop, Hash} = {Hash, NewTop},
    ok.

%% ============================================================
%% private functions
%% ============================================================
random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).

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

%% sign_and_post_tx_(EncodedUnsignedTx) ->
%%     sign_and_post_tx_(EncodedUnsignedTx, on_node).

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

%%%%%
%% OAS3
%%%%%
get_top_header(_Config) ->
    %%
    %% NOTE: This has been modified, and differs from the integration suite.
    %%
    aecore_suite_utils:mine_key_blocks(aecore_suite_utils:node_name(?NODE), 2),
    %% this is present in oas3.yaml
    {ok, 200, HeaderOrig}= get_top_header_sut(),
    #{<<"beneficiary">> := <<"ak_",Acc/binary>>,
      <<"hash">> := <<"kh_",Hash/binary>>,
      <<"height">> := Height,
      <<"info">> := <<"cb_",Info/binary>>,
      <<"miner">> := <<"ak_",Miner/binary>>,
      <<"prev_hash">> := <<"kh_",PrevHash/binary>>,
      <<"prev_key_hash">> := <<"kh_",PrevKeyHash/binary>>,
      <<"state_hash">> := <<"bs_",StateHash/binary>>,
      <<"target">> := Target,
      <<"time">> := Time,<<"version">> := Version} = HeaderOrig,
    %% this is not present in oas3.yaml but swagger.yaml
    {ok, 404, #{}} = get_top_sut(),
    {ok, 200, HeaderOrig}= get_top_header_sut([{'int-as-string', false}]),
    {ok, 200, HeaderStrings}= get_top_header_sut([{'int-as-string', true}]),
    ct:log("HeaderOrig = ~p", [HeaderOrig]),
    ct:log("HeaderStrings = ~p", [HeaderStrings]),
    #{<<"beneficiary">> := <<"ak_",Acc/binary>>,
      <<"hash">> := <<"kh_",Hash/binary>>,
      <<"height">> := HeightStr,
      <<"info">> := <<"cb_",Info/binary>>,
      <<"miner">> := <<"ak_",Miner/binary>>,
      <<"prev_hash">> := <<"kh_",PrevHash/binary>>,
      <<"prev_key_hash">> := <<"kh_",PrevKeyHash/binary>>,
      <<"state_hash">> := <<"bs_",StateHash/binary>>,
      <<"target">> := TargetStr,
      <<"time">> := TimeStr,<<"version">> := VersionStr} = HeaderStrings,
    Height = binary_to_integer(HeightStr),
    Target = binary_to_integer(TargetStr),
    Time = binary_to_integer(TimeStr),
    Version = binary_to_integer(VersionStr),
    ok.

post_paying_for_tx(Config) ->
    aehttp_integration_SUITE:post_paying_for_tx(Config).

min_gas_price() ->
    aec_test_utils:min_gas_price().

get_prev_key_block([KB, _MB]) ->
    true = aec_blocks:is_key_block(KB),
    KB;
get_prev_key_block([MB]) ->
    PrevKeyHash = aec_blocks:prev_key_hash(MB),
    {ok, KB} = rpc(aec_chain, get_block, [PrevKeyHash]),
    KB.
