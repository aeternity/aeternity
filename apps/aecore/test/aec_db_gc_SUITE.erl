-module(aec_db_gc_SUITE).

%% common_test exports
-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export([write_unreachable_nodes/1,
         create_accounts/1,
         start_2nd_node_and_mine/1,
         check_1st_node_gced/1,
         restart_1st/1,
         check_accounts/1,
         mine_again_1st_node/1,
         mine_again_2nd_node/1
        ]).

-include_lib("common_test/include/ct.hrl").

-define(MAX_MINED_BLOCKS, 20).
-define(NUM_ACCOUNTS, 20).
-define(NUM_GCED_NODES, 20).

all() ->
    [{group, all_nodes}].

groups() ->
    [{all_nodes, [sequence], [{group, two_nodes}]},
     {two_nodes, [sequence],
      [write_unreachable_nodes,
       create_accounts,
       start_2nd_node_and_mine,
       check_1st_node_gced,
       restart_1st,
       check_accounts,
       mine_again_1st_node,
       write_unreachable_nodes,
       mine_again_2nd_node,
       check_1st_node_gced,
       restart_1st,
       check_accounts]}].

suite() ->
    [].

init_per_suite(Config0) ->
    Config1 = aec_metrics_test_utils:make_port_map([dev1, dev2], Config0),
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks,
                     <<"garbage_collection">> => #{<<"enabled">> => true,
                                                   <<"interval">> => 51,
                                                   <<"history">> => 50}},
               <<"sync">> => #{<<"single_outbound_per_group">> => false},
               <<"mempool">> => #{<<"tx_ttl">> => 100},
               <<"mining">> => #{<<"micro_block_cycle">> => 100}},
    Accounts = [new_pubkey() || _ <- lists:seq(1, ?NUM_ACCOUNTS)],
    GCedNodes = [crypto:hash(sha256, crypto:strong_rand_bytes(32)) ||
                    _ <- lists:seq(1, ?NUM_GCED_NODES)],
    Config = aecore_suite_utils:init_per_suite([dev1, dev2], DefCfg,
                                               [{add_peers, true}],
                                               [{symlink_name, "latest.sync"},
                                                {test_module, ?MODULE},
                                                {accounts, Accounts},
                                                {gced_nodes, GCedNodes}] ++ Config1),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config),
    Config.

end_per_suite(Config) ->
    [begin
         {ok, DbCfg} = node_db_cfg(X),
         aecore_suite_utils:stop_node(X, Config),
         aecore_suite_utils:delete_node_db_if_persisted(DbCfg)
     end || X <- [dev1, dev2]].

init_per_group(two_nodes, Config) ->
    Config1 = config(Config),
    InitialApps = {aec_test_utils:running_apps(), aec_test_utils:loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(all_nodes, Config) ->
    Config.

end_per_group(two_nodes, Config) ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    ok = aec_metrics_test_utils:stop_statsd_loggers(),
    {_, {OldRunningApps, OldLoadedApps}} = proplists:lookup(initial_apps, Config),
    ok = aec_test_utils:restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps);
end_per_group(all_nodes, _Config) ->
   ok.


init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

write_unreachable_nodes(Config) ->
    Nodes = ?config(gced_nodes, Config),
    NVal  = lists:duplicate(17, <<>>),
    N1    = aecore_suite_utils:node_name(dev1),
    lists:foreach(
      fun (NodeKey) ->
              Node = {aec_account_state, NodeKey, NVal},
              ok = rpc:call(N1, mnesia, dirty_write, [Node])
      end, Nodes).

create_accounts(Config) ->
    PKs = ?config(accounts, Config),
    N1  = aecore_suite_utils:node_name(dev1),
    Fee = 1500000 * aec_test_utils:min_gas_price(),
    lists:foreach(
      fun ({Nonce, PK}) ->
              {ok, Tx} = add_spend_tx(N1, 10, Fee, Nonce, 100, PK),
              aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx], ?MAX_MINED_BLOCKS)
      end, lists:zip(lists:seq(1, length(PKs)), PKs)),
    check_accounts(Config),
    ok.

start_2nd_node_and_mine(Config) ->
    [N1, N2] = [aecore_suite_utils:node_name(X) || X <- [dev1, dev2]],
    aecore_suite_utils:start_node(dev2, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev2)),
    ok = aecore_suite_utils:check_for_logs([dev2], Config),
    H  = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    aecore_suite_utils:mine_key_blocks(N2, H + 9),
    ok.

check_1st_node_gced(Config) ->
    N1   = aecore_suite_utils:node_name(dev1),
    true = lists:all(
             fun (NodeKey) ->
                     Ns = rpc:call(N1, mnesia, dirty_read, [aec_account_state, NodeKey]),
                     length(Ns) == 0
             end, ?config(gced_nodes, Config)),
    ok.

restart_1st(Config) ->
    {ok, DbCfg} = node_db_cfg(dev1),
    aecore_suite_utils:stop_node(dev1, Config),
    aecore_suite_utils:start_node(dev1, Config),
    N = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N),
    ct:log("~w restarted", [dev1]),
    ok.

check_accounts(Config) ->
    N1 = aecore_suite_utils:node_name(dev1),
    lists:foreach(
      fun (PK) ->
              {value, _} = rpc:call(N1, aec_chain, get_account, [PK])
      end, ?config(accounts, Config)),
    ok.

mine_again_1st_node(_Config) ->
    N = aecore_suite_utils:node_name(dev1),
    H = aec_headers:height(rpc:call(N, aec_chain, top_header, [])),
    aecore_suite_utils:mine_key_blocks(N, H + 1),
    ok.

mine_again_2nd_node(_Config) ->
    [N1, N2] = [aecore_suite_utils:node_name(X) || X <- [dev1, dev2]],
    H = aec_headers:height(rpc:call(N1, aec_chain, top_header, [])),
    %% aecore_suite_utils:mine_key_blocks(N1, H + 1),
    aecore_suite_utils:mine_key_blocks(N2, H + 2),
    ok.


%% ==================================================
%% Private functions
%% ==================================================

config(Config) ->
    [{devs, [dev1]}, {nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config].

node_db_cfg(Node) ->
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(
                    fun (M, F, A)->
                            rpc:call(aecore_suite_utils:node_name(Node), M, F, A, 5000)
                    end),
    {ok, DbCfg}.

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Recipient) ->
    Sender = aecore_suite_utils:patron(),
    SenderId = aeser_id:create(account, maps:get(pubkey, Sender)),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => <<>>,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, maps:get(privkey, Sender)),
    Res = rpc:call(Node, aec_tx_pool, push, [STx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_pubkey() ->
    #{public := PubKey} = enacl:sign_keypair(),
    PubKey.
