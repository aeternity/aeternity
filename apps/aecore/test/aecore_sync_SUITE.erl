-module(aecore_sync_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    start_first_node/1,
    start_second_node/1,
    start_third_node/1,
    start_blocked_second/1,
    mine_on_first_up_to_latest_consensus_protocol/1,
    mine_on_first/1,
    mine_on_second/1,
    mine_on_second_not_empty/1,
    mine_on_third/1,
    mine_again_on_first/1,
    restart_first/1,
    restart_second/1,
    restart_third/1,
    tx_first_pays_second/1,
    tx_first_pays_second_more_it_can_afford/1,
    ensure_tx_pools_empty/1,
    ensure_tx_pools_one_tx/1,
    ensure_tx_pools_one_tx_first_node/1,
    ensure_tx_pools_one_tx_second_node/1,
    ensure_tx_pools_one_tx_third_node/1,
    report_metrics/1,
    check_metrics_logged/1,
    crash_syncing_worker/1,
    large_msgs/1,
    inject_long_chain/1,
    measure_second_node_sync_time/1
   ]).

-include_lib("common_test/include/ct.hrl").

-import(aecore_suite_utils, [patron/0]).
-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-define(MAX_MINED_BLOCKS, 20).

all() ->
    [ {group, all_nodes} ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes},
                              {group, three_nodes},
                              {group, semantically_invalid_tx},
                              {group, mempool_sync},
                              {group, one_blocked},
                              {group, large_msgs},
                              {group, performance}
                             ]},
     {two_nodes, [sequence],
      [start_first_node,
       mine_on_first_up_to_latest_consensus_protocol,
       mine_on_first,
       start_second_node,
       tx_first_pays_second,
       mine_again_on_first,
       ensure_tx_pools_empty,
       mine_on_second,
       tx_first_pays_second,
       restart_second,
       restart_first,
       report_metrics,
       check_metrics_logged,
       crash_syncing_worker]},
     {three_nodes, [sequence],
      [start_first_node,
       mine_on_first,
       start_second_node,
       start_third_node,
       mine_again_on_first,
       mine_on_second,
       mine_on_third,
       restart_third,
       report_metrics,
       check_metrics_logged]},
     {semantically_invalid_tx, [sequence],
      [start_first_node,
       mine_on_first_up_to_latest_consensus_protocol,
       mine_on_first,
       start_second_node,
       mine_on_second, %% We need to make sure first
       mine_on_second, %% doesn't get a delayed reward
       ensure_tx_pools_empty,
       tx_first_pays_second_more_it_can_afford,
       mine_on_second_not_empty,
       ensure_tx_pools_one_tx]},
     {mempool_sync, [sequence],
      [start_first_node,
       mine_on_first_up_to_latest_consensus_protocol,
       mine_on_first,
       start_second_node,
       tx_first_pays_second,   %% Tx nonce = 1
       ensure_tx_pools_one_tx, %% for dev1 & dev2
       mine_again_on_first,
       ensure_tx_pools_empty,  %% for dev1 & dev2
       tx_first_pays_second,
       ensure_tx_pools_one_tx, %% Tx nonce = 2,
       start_third_node,
       ensure_tx_pools_one_tx, %% for dev1 & dev2
       ensure_tx_pools_one_tx_third_node %% check mempool on dev3
      ]},
     {one_blocked, [sequence],
      [start_first_node,
       mine_on_first,
       start_blocked_second]},
     {large_msgs, [sequence],
      [large_msgs]},
     {performance, [sequence],
          [{group, bench_only_key},
           {group, bench_only_micro},
           {group, bench_mixed_small},
           {group, bench_mixed_big}]},
     {bench_only_key, [{group, run_benchmark}]},
     {bench_only_micro, [{group, run_benchmark}]},
     {bench_mixed_small, [{group, run_benchmark}]},
     {bench_mixed_big, [{group, run_benchmark}]},
     {run_benchmark, [sequence],
      [start_first_node,
       inject_long_chain,
       measure_second_node_sync_time]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Config = aec_metrics_test_utils:make_port_map([dev1, dev2, dev3], Config0),
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{
        <<"metrics">> => #{
            <<"rules">> => [
                #{<<"name">> => <<"ae.epoch.system.**">>,
                  <<"actions">> => <<"log">>},
                #{<<"name">> => <<"ae.epoch.aecore.**">>,
                  <<"actions">> => <<"log,send">>}
            ]
        },
        <<"chain">> => #{
            <<"persist">> => true,
            <<"hard_forks">> => Forks
        },
        <<"sync">> => #{
            <<"single_outbound_per_group">> => false
        },
        <<"mempool">> => #{
            <<"tx_ttl">> => 100
        },
        <<"mining">> => #{
            <<"micro_block_cycle">> => 100
        }
    },
    aecore_suite_utils:init_per_suite([dev1, dev2, dev3], DefCfg, [{add_peers, true}], [{symlink_name, "latest.sync"}, {test_module, ?MODULE}] ++ Config).

end_per_suite(Config) ->
    stop_devs(Config).

init_per_group(TwoNodes, Config) when
        TwoNodes =:= two_nodes; TwoNodes =:= semantically_invalid_tx;
        TwoNodes =:= mempool_sync; TwoNodes =:= run_benchmark ->
    Config1 = config({devs, [dev1, dev2]}, Config),
    InitialApps = {running_apps(), loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(performance, Config) ->
    case os:getenv("SYNC_BENCHMARK") of
        false ->
            {skip, "Enable sync performance tests by adding "
                   "SYNC_BENCHMARK os environment variable"};
        _ ->
            Config
    end;
init_per_group(three_nodes, Config) ->
    Config1 = config({devs, [dev1, dev2, dev3]}, Config),
    InitialApps = {running_apps(), loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(Group, Config) when Group =:= one_blocked;
                                   Group =:= large_msgs ->
    Config1 = config({devs, [dev1, dev2]}, Config),
    InitialApps = {running_apps(), loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [Dev1, Dev2 | _] = proplists:get_value(devs, Config1),
    EpochCfg = aecore_suite_utils:epoch_config(Dev1, Config),
    aecore_suite_utils:create_config(Dev1, Config, EpochCfg,
                                            [{block_peers, [ Dev2 ]},
                                             {add_peers, true}
                                            ]),
    [{initial_apps, InitialApps} | Config1];
init_per_group(bench_only_key, Config) ->
    [{generations, 1000},{micro_per_generation, 0},{tx_per_micro, 0}|Config];
init_per_group(bench_only_micro, Config) ->
    [{generations, 1},{micro_per_generation, 1000},{tx_per_micro, 1}|Config];
init_per_group(bench_mixed_small, Config) ->
    [{generations, 10},{micro_per_generation, 10},{tx_per_micro, 1}|Config];
init_per_group(bench_mixed_big, Config) ->
    [{generations, 50},{micro_per_generation, 100},{tx_per_micro, 0}|Config];
init_per_group(all_nodes, Config) ->
    Config.

end_per_group(Group, Config) when Group =:= one_blocked;
                                   Group =:= large_msgs ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    ok = aec_metrics_test_utils:stop_statsd_loggers(),
    stop_devs(Config),
    {_, {OldRunningApps, OldLoadedApps}} = proplists:lookup(initial_apps, Config),
    ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps),
    %% reset dev1 config to no longer block any peers.
    Config1 = config({devs, [dev1]}, Config),
    [Dev1 | _] = proplists:get_value(devs, Config1),
    EpochCfg = aecore_suite_utils:epoch_config(Dev1, Config),
    aecore_suite_utils:create_config(Dev1, Config,
                                     maps:without([<<"blocked_peers">>], EpochCfg),
                                     [{add_peers, true}]),
    ok;
end_per_group(mempool_sync, Config) ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    ok = aec_metrics_test_utils:stop_statsd_loggers(),
    Devs = proplists:get_value(devs, Config, []),
    stop_devs([dev3 | Devs], Config),
    {_, {OldRunningApps, OldLoadedApps}} = proplists:lookup(initial_apps, Config),
    ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps),
    ok;
end_per_group(Group, Config) when Group =:= two_nodes;
                                  Group =:= three_nodes;
                                  Group =:= semantically_invalid_tx;
                                  Group =:= run_benchmark ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    ok = aec_metrics_test_utils:stop_statsd_loggers(),
    stop_devs(Config),
    case proplists:lookup(initial_apps, Config) of
        none -> ok;
        {_, {OldRunningApps, OldLoadedApps}} ->
            ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
    end,
    ok;
end_per_group(Benchmark, _Config) when
    Benchmark =:= performance;
    Benchmark =:= bench_only_key; Benchmark =:= bench_only_micro;
    Benchmark =:= bench_mixed_small; Benchmark =:= bench_mixed_big ->
    ok;
end_per_group(all_nodes, _Config) ->
   ok.

stop_devs(Config) ->
    Devs = proplists:get_value(devs, Config, []),
    stop_devs(Devs, Config).

stop_devs(Devs, Config) ->
    lists:foreach(
        fun(Node) ->
            {ok, DbCfg} = node_db_cfg(Node),
            aecore_suite_utils:stop_node(Node, Config),
            aecore_suite_utils:delete_node_db_if_persisted(DbCfg)
        end,
        Devs),
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

start_first_node(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config),
    ok.

start_second_node(Config) ->
    [Dev1, Dev2] = [dev1, dev2],
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),
    aecore_suite_utils:await_aehttp(N2),
    ct:log("Connected peers on dev2: ~p",
           [rpc:call(N2, aec_peers, connected_peers, [], 5000)]),
    B1 = rpc:call(N1, aec_chain, top_block, [], 5000),
    ok = aecore_suite_utils:check_for_logs([Dev2], Config),
    true = expect_block(N2, B1).

start_third_node(Config) ->
    Dev3 = dev3,
    N3 = aecore_suite_utils:node_name(Dev3),
    T0 = os:timestamp(),
    aecore_suite_utils:start_node(Dev3, Config),
    aecore_suite_utils:connect(N3),
    aecore_suite_utils:await_aehttp(N3),
    ct:log("Connected peers on dev3: ~p",
           [rpc:call(N3, aec_peers, connected_peers, [], 5000)]),
    ok = aecore_suite_utils:check_for_logs([Dev3], Config),
    true = expect_same(T0, Config).

mine_on_first_up_to_latest_consensus_protocol(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    N = aecore_suite_utils:node_name(Dev1),
    aecore_suite_utils:mine_key_blocks(N, aecore_suite_utils:latest_fork_height()),
    ok.

mine_on_first(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    N = aecore_suite_utils:node_name(Dev1),
    aecore_suite_utils:mine_key_blocks(N, 3),
    ok.

start_blocked_second(Config) ->
    [ Dev1, Dev2 | _ ] = proplists:get_value(devs, Config),
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),
    timer:sleep(2000),

    %% Check that there is only one non-blocked peer (dev3) and no connected peers
    NonBlocked = rpc:call(N1, aec_peers, get_random, [all], 5000),
    Standby = rpc:call(N1, aec_peers, count, [standby], 5000),
    Connected  = rpc:call(N1, aec_peers, connected_peers, [], 5000),
    ct:log("Non-blocked peers on dev1: ~p", [NonBlocked]),
    ct:log("Connected peers on dev1: ~p", [Connected]),
    [] = Connected,
    0 = Standby,

    %% Also check that they have different top blocks
    B1 = rpc:call(N1, aec_chain, top_block, [], 5000),
    B2 = rpc:call(N2, aec_chain, top_block, [], 5000),
    true = (B1 /= B2),

    %% Unblock dev2 at dev1 and check that peers sync
    rpc:call(N1, aec_peers, unblock_all, [], 5000),
    expect_same_top([N1, N2], 5).

tx_first_pays_second(Config) ->
    tx_first_pays_second_(Config, fun(_) -> 1 end).

tx_first_pays_second_more_it_can_afford(Config) ->
    tx_first_pays_second_(Config, fun(Bal1) -> Bal1 + 1 end).

tx_first_pays_second_(Config, AmountFun) ->
    [ Dev1, Dev2 | _ ] = proplists:get_value(devs, Config),
    NodeT1 = aecore_suite_utils:node_tuple(Dev1),
    NodeT2 = aecore_suite_utils:node_tuple(Dev2),

    PK1 = get_pubkey(NodeT1),
    PK2 = get_pubkey(NodeT1),

    TxFee = 20000 * aec_test_utils:min_gas_price(),
    Bal1 = get_balance_(NodeT1, PK1),
    true = (is_integer(Bal1) andalso Bal1 > 0),

    {ok, Pool11} = get_pool(NodeT1),
    {ok, Pool21} = get_pool(NodeT2),
    Pool11 = Pool21,                % tx pools are ordered

    ok = new_tx(#{node1  => {PK1, NodeT1},
                  node2  => {PK2, NodeT2},
                  amount => AmountFun(Bal1),
                  fee    => TxFee}),

    {ok, Pool12} = get_pool(NodeT1),
    [NewTx] = Pool12 -- Pool11,
    true = ensure_new_tx(NodeT1, NewTx).

ensure_tx_pools_empty(Config) ->
    ensure_tx_pools_n_txs_(Config, 0).

ensure_tx_pools_one_tx(Config) ->
    ensure_tx_pools_n_txs_(Config, 1).

ensure_tx_pools_one_tx_first_node(_Config) ->
    ensure_tx_pools_on_dev(dev1, 1).

ensure_tx_pools_one_tx_second_node(_Config) ->
    ensure_tx_pools_on_dev(dev2, 1).

ensure_tx_pools_one_tx_third_node(_Config) ->
    ensure_tx_pools_on_dev(dev3, 1).

ensure_tx_pools_on_dev(Dev, TxsCount) ->
    Node = aecore_suite_utils:node_tuple(Dev),
    ensure_tx_pools_n_txs_on_nodes([Node], TxsCount).

ensure_tx_pools_n_txs_(Config, TxsCount) ->
    Nodes = ?config(nodes, Config),
    ensure_tx_pools_n_txs_on_nodes(Nodes, TxsCount).

ensure_tx_pools_n_txs_on_nodes(Nodes, TxsCount) ->
    retry(
      fun() ->
              [APool | RestPools] =
                  lists:map(
                    fun(Node) ->
                            case get_pool(Node) of
                                {ok, Pool} when is_list(Pool) ->
                                    ct:log("Pool (~p) = ~p", [Node, Pool]),
                                    Pool
                            end
                    end, Nodes),
              if
                  length(APool) =:= TxsCount ->
                      lists:all(fun(P) when P =:= APool -> true;
                                   (_) -> false
                                end, RestPools);
                  true ->
                      false
              end
      end, {?LINE, ensure_tx_pools_n_txs_, TxsCount, Nodes}).

report_metrics(Config) ->
    Ns = [N || {_, N} <- ?config(nodes, Config)],
    [rpc:call(N, exometer_report, trigger_interval,
              [aec_metrics_main, default]) || N <- Ns],
    timer:sleep(1000).

check_metrics_logged(Config) ->
    %% No system metrics should have been sent to the 'statsd' ports.
    check_no_system_metrics_sent(),
    %% a user config filter is applied in init_per_suite, which turns off
    %% metrics logging for "ae.epoch.system.**".
    Dir = aecore_suite_utils:shortcut_dir(Config),
    GrepFiles = filelib:wildcard(filename:join(Dir, "dev?/log/aeternity_metrics.log")),
    Res1 = aecore_suite_utils:cmd("grep", ".", ".", ["peers" | GrepFiles], [], false),
    Res2 = aecore_suite_utils:cmd("grep", ".", ".", ["aecore" | GrepFiles], [], false),
    {0, [_|_]} = aecore_suite_utils:cmd_res(Res1),
    {0, [_|_]} = aecore_suite_utils:cmd_res(Res2),
    ok.

check_no_system_metrics_sent() ->
    lists:foreach(
      fun({_Id, Data}) ->
              [] = [Line || {_,L} = Line <- Data,
                            nomatch =/= re:run(L, "^ae\\.epoch\\.system", [])]
      end, aec_metrics_test_utils:fetch_data()).

%% It takes about 300 ms to fetch the blocks from the other chain.
%% In that interval we need to have the sync process crash
%% We agressively look whether sync has started and kill a.s.a.p.
crash_syncing_worker(Config) ->
    [ Dev1, Dev2 | _ ] = proplists:get_value(devs, Config),
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    {ok, DbCfg} = node_db_cfg(Dev2),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    aecore_suite_utils:stop_node(Dev2, Config),

    %% Hotfix - make sure mempool is empty
    aecore_suite_utils:mine_all_txs(N1, ?MAX_MINED_BLOCKS),
    {ok, []} = rpc:call(N1, aec_tx_pool, peek, [infinity], 5000),

    Top1 = rpc:call(N1, aec_chain, top_block, [], 5000),
    ct:log("top at Dev1 = ~p", [Top1]),
    ExtraBlocks = 5,  %% Might need more if CPU is really fast!
    aecore_suite_utils:mine_key_blocks(N1, ExtraBlocks),
    H1 = aec_blocks:height(Top1) + ExtraBlocks,

    %% Ensure compatible notation of uri:
    {ok, PeerInfo} = aec_peers:parse_peer_address(aecore_suite_utils:peer_info(Dev1)),
    PeerId = aec_peers:peer_id(PeerInfo),
    ct:log("PeerId of Dev1 ~p", [PeerId]),

    spawn_link(fun() -> kill_sync_worker(N2, PeerId) end),

    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),
    ct:log("node connected ~p", [N2]),

    %% Set the same mining_rate to validate target
    %% Only needed when chain more than 18 blocks
    ok = rpc:call(N2, application, set_env, [aecore, expected_mine_rate, aecore_suite_utils:expected_mine_rate()], 5000),

    %% Takes re-ping + fetching blocks to get in sync with Dev1
    %% Configuration changed to have re-ping after 700ms.
    timer:sleep(2000),
    Top2 = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("top at Dev2 = ~p", [Top2]),
    {ok, B2} = rpc:call(N2, aec_chain, get_key_block_by_height, [H1], 5000),
    ct:log("Block @ ~p on Dev2 = ~p", [ H1, B2]),

    {ok, B2} = rpc:call(N1, aec_chain, get_key_block_by_height, [H1], 5000),
    ok.

kill_sync_worker(N, PeerId) ->
    case rpc:call(N, aec_sync, worker_for_peer, [PeerId], 5000) of
        false ->
            timer:sleep(10),
            kill_sync_worker(N, PeerId);
        {badrpc, _} ->
            timer:sleep(20),
            kill_sync_worker(N, PeerId);
        {ok, Pid} ->
            exit(Pid, kill),
            ct:log("killed worker ~p", [Pid])
    end.

mine_again_on_first(Config) ->
    mine_and_compare(aecore_suite_utils:node_name(dev1), Config, true).

mine_on_second(Config) ->
    mine_and_compare(aecore_suite_utils:node_name(dev2), Config, true).

mine_on_second_not_empty(Config) ->
    mine_and_compare(aecore_suite_utils:node_name(dev2), Config, false).

restart_first(Config) ->
    restart_node(1, Config).

restart_second(Config) ->
    restart_node(2, Config).

restart_third(Config) ->
    restart_node(3, Config).

restart_node(Nr, Config) ->
    Dev = lists:nth(Nr, proplists:get_value(devs, Config)),
    {ok, DbCfg} = node_db_cfg(Dev),
    aecore_suite_utils:stop_node(Dev, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    T0 = os:timestamp(),
    aecore_suite_utils:start_node(Dev, Config),
    N = aecore_suite_utils:node_name(Dev),
    aecore_suite_utils:connect(N),
    ct:log("~w restarted", [Dev]),
    true = expect_same(T0, Config).

mine_on_third(Config) ->
    mine_and_compare(aecore_suite_utils:node_name(dev3), Config, true).

mine_and_compare(N1, Config, EmptyPoolExpected) ->
    AllNodes = [N || {_, N} <- ?config(nodes, Config)],
    PrevTop = rpc:call(N1, aec_chain, top_block, [], 5000),
    %% If there are txs in the mempool, there will be an additional micro block.
    %% Micro blocks may result in micro forks, so we need to mine sufficiently many
    %% blocks.
    %% Better to use: aecore_suite_utils:mine_blocks_until_txs_on_chain/3, but
    %% then we need to pass on the TxHashes... which this test structure does
    %% make difficult. But not impossible:
    {ok, [KeyBlock | _OtherBlocks]} =
        case rpc:call(N1, aec_tx_pool, peek, [infinity], 5000) of
            {ok, STxs = [_ | _]} when EmptyPoolExpected ->
                TxHashes = [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx)) || STx <- STxs],
                aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, TxHashes, ?MAX_MINED_BLOCKS);
            {ok, _} ->
                aecore_suite_utils:mine_key_blocks(N1, 1)
        end,

    true = aec_blocks:is_key_block(KeyBlock),
    NewTop = rpc:call(N1, aec_chain, top_block, [], 5000),
    true = (NewTop =/= PrevTop),
    Bal1 = get_balance(N1),
    ct:log("Balance on dev1: ~p", [Bal1]),
    lists:foreach(
      fun(Nx) ->
              true = expect_block(Nx, NewTop)
      end, AllNodes -- [N1]).

expect_same(T0, Config) ->
    Nodes = [N || {_, N} <- ?config(nodes, Config)],
    aecore_suite_utils:await_sync_complete(T0, Nodes),
    expect_same_top(Nodes, 5),
    expect_same_tx(Nodes).

expect_same_top(Nodes, Tries) when Tries > 0 ->
    Blocks = lists:map(
               fun(N) ->
                       B = rpc:call(N, aec_chain, top_block, [], 5000),
                       {N, B}
               end, Nodes),
    case lists:ukeysort(2, Blocks) of
        [_] ->
            ok;
        [_,_|_] = Dups ->
            ct:log("Blocks differ, retrying:~n~p", [Dups]),
            timer:sleep(2000),
            expect_same_top(Nodes, Tries-1)
    end;
expect_same_top(Nodes, _) ->
    ct:log("tries exhausted", []),
    erlang:error({top_blocks_differ, Nodes}).

expect_same_tx(Nodes) ->
    retry(fun() ->
                  expect_same_tx_(Nodes)
          end, {?LINE, expect_same_tx, Nodes}).

expect_same_tx_(Nodes) ->
    Txs = lists:map(
            fun(N) ->
                    case rpc:call(N, aec_tx_pool, peek, [infinity], 5000) of
                        {ok, T} ->
                            ct:log("Txs (~p): ~p", [N, T]),
                            T;
                        Other ->
                            ct:log("Txs ERROR (~p): ~p", [N, Other]),
                            error
                    end
            end, Nodes),
    case lists:usort(Txs) of
        [X] when X =/= error -> true;
        _ -> false
    end.

large_msgs(Config) ->
    [ Dev1, Dev2 | _ ] = proplists:get_value(devs, Config),
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),

    aecore_suite_utils:start_node(Dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(Dev1)),

    ok = rpc:call(N1, application, set_env, [aecore, block_gas_limit, 100000000]),

    %% Insert enough transactions to make a large generation
    Blob = fun(Size) -> << <<171:8>> || _ <- lists:seq(1, Size) >> end,
    {ok, Tx1} = add_spend_tx(N1, 10, 1500000 * aec_test_utils:min_gas_price(), 1, 100, Blob(16#ffff)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx1], ?MAX_MINED_BLOCKS),

    {ok, Tx2} = add_spend_tx(N1, 10, 3000000 * aec_test_utils:min_gas_price(), 2, 100, Blob(16#1ffff)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx2], ?MAX_MINED_BLOCKS),

    {ok, Tx3} = add_spend_tx(N1, 10, 4000000 * aec_test_utils:min_gas_price(), 3, 100, Blob(16#2ffff)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx3], ?MAX_MINED_BLOCKS),

    {ok, Tx4} = add_spend_tx(N1, 10, 8000000 * aec_test_utils:min_gas_price(), 4, 100, Blob(16#5ffff)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx4], ?MAX_MINED_BLOCKS),

    {ok, Tx5} = add_spend_tx(N1, 10, 1500000 * aec_test_utils:min_gas_price(), 5, 100, Blob(16#fce3)), %% Should exactly fit in one message
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx5], ?MAX_MINED_BLOCKS),

    {ok, Tx6} = add_spend_tx(N1, 10, 1500000 * aec_test_utils:min_gas_price(), 6, 100, Blob(16#fce4)), %% Wee bit too large
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx6], ?MAX_MINED_BLOCKS),

    {ok, Tx7} = add_spend_tx(N1, 10, 3000000 * aec_test_utils:min_gas_price(), 7, 100, Blob(16#1fcb8)), %% Even multiple of fragment size
    aecore_suite_utils:mine_blocks_until_txs_on_chain(N1, [Tx7], ?MAX_MINED_BLOCKS),

    T0 = os:timestamp(),
    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),

    %% Set the same mining_rate to validate target
    %% Only needed when chain more than 18 blocks
    ok = rpc:call(N2, application, set_env, [aecore, expected_mine_rate, aecore_suite_utils:expected_mine_rate()], 5000),

    ok = rpc:call(N2, application, set_env, [aecore, block_gas_limit, 10000000]),

    rpc:call(N1, aec_peers, unblock_all, [], 5000),

    true = expect_same(T0, Config).

inject_long_chain(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    Node = aecore_suite_utils:node_name(Dev1),
    {G, M, NTx} = get_bench_config(Config),
    %% Ensure not mining
    stopped = rpc:call(Node, aec_conductor, get_mining_state, []),
    %% Disable PoW checks
    aecore_suite_utils:start_mock(Node, block_pow),
    %% Create chain
    put(nonce, 1),
    {T, _} = timer:tc(fun() -> [inject_generation(Node, M, NTx) || _ <- lists:seq(1, G)] end),
    {G, M, NTx} = get_bench_config(Config),
    io:format(user, "\nTest chain(~p/~p/~p) on Node1 created and inserted in ~p us\n", [G, M, NTx, T]).

inject_generation(Node, NMicro, NTx) ->
    %% Start a generation
    TopHash = rpc:call(Node, aec_chain, top_block_hash, []),
    {ok, Pub} = rpc:call(Node, aec_keys, pubkey, []),
    {ok, Block} = rpc:call(Node, aec_block_key_candidate, create, [TopHash, Pub]),
    ok = rpc:call(Node, aec_conductor, post_block, [Block]),
    %% Add microblocks
    [inject_microblock(Node, NTx) || _ <- lists:seq(1, NMicro)].

inject_microblock(Node, NTx) ->
    TopHash = rpc:call(Node, aec_chain, top_block_hash, []),
    inject_txs(Node, NTx),
    {ok, MicroBlock, _} = rpc:call(Node, aec_block_micro_candidate, create, [TopHash]),
    NTx = length(aec_blocks:txs(MicroBlock)),
    {ok, MicroBlockS} = rpc:call(Node, aec_keys, sign_micro_block, [MicroBlock]),
    ok = rpc:call(Node, aec_conductor, post_block, [MicroBlockS]),
    ok.

inject_txs(Node, NTx) ->
    [ begin
        Nonce = get(nonce),
        add_spend_tx(Node, 10, 500000 * aec_test_utils:min_gas_price(), Nonce, 100, <<>>),
        put(nonce, Nonce+1)
      end || _ <- lists:seq(1, NTx)].

measure_second_node_sync_time(Config) ->
    [Dev1, Dev2] = [dev1, dev2],
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    aecore_suite_utils:start_node(Dev2, Config),
    {T, _} = timer:tc(fun() ->
        aecore_suite_utils:connect(N2),
        aecore_suite_utils:start_mock(N2, block_pow),
        Self = self(),
        Watcher = rpc:call(N2, erlang, spawn, [fun() ->
            aec_events:subscribe(chain_sync),
            receive
                {gproc_ps_event,chain_sync, #{info := {chain_sync_done, _}}} ->
                    Self ! {self(), sync_done}
            end end]),
        receive
            {Watcher, sync_done} ->
                ok
        after
            60000 ->
                error(sync_failed)
        end
      end),
    {G, M, NTx} = get_bench_config(Config),
    Fmt = "\nSyncing on Node2 ~p generations with ~p microblocks per generation "
          "with ~p SpendTx per microblock took ~p us\n",
    ct:log(Fmt, [G, M, NTx, T]),
    %% This test is meant to be run manually in the shell
    io:format(user, Fmt, [G, M, NTx, T]).

%% ==================================================
%% Private functions
%% ==================================================

expect_block(N, B) ->
    retry(fun() -> expect_block_(N, B) end,
          {?LINE, expect_block, N, B}).

expect_block_(N, B) ->
    Bn = rpc:call(N, aec_chain, top_block, [], 5000),
    case B =:= Bn of
        true ->
            Bal = get_balance(N),
            ct:log("Got block (~p); Balance = ~p", [N, Bal]),
            true;
        false ->
            {false, Bn}
    end.

retry(Test, Info) ->
    retry(Test, 5, Info).

retry(Test, Retries, Info) ->
    retry_(Test, #{prev => undefined,
                   retries => Retries,
                   tries => Retries,
                   info => Info}).

retry_(Test, #{tries := Tries} = S) when Tries > 0 ->
    case Test() of
        true ->
            true;
        false ->
            timer:sleep(1000),
            retry_(Test, S#{tries => Tries -1});
        {false, V} ->
            timer:sleep(1000),
            retry_(Test, S#{tries => Tries -1, prev => V})
    end;
retry_(_, S) ->
    ct:log("exhausted retries (~p)", [S]),
    ct:fail({retry_exhausted, S}).

%% ============================================================
%% Transaction support functions
%% ============================================================

get_pubkey({NodeId, _}) ->
    {_Priv, Pub} = aecore_suite_utils:sign_keys(NodeId),
    Pub.

get_balance_({_, NodeName}, Pubkey) ->
    {_, Account} = rpc:call(NodeName, aec_chain, get_account, [Pubkey]),
    aec_accounts:balance(Account).

%%

get_balance(N) ->
    rpc:call(N, aec_mining, get_miner_account_balance, [], 5000).

get_pool({_, Name}) ->
    rpc:call(Name, aec_tx_pool, peek, [infinity], 5000).

new_tx(#{node1 := {PK1, {_,N1} = T1}, node2 := {PK2, _}, amount := Am, fee := Fee} = _M) ->
    ExtPort = rpc:call(N1, aeu_env, user_config_or_env,
                       [ [<<"http">>, <<"external">>, <<"port">>],
                         aehttp, [external, port], 8043], 5000),
    IntPort = rpc:call(N1, aeu_env, user_config_or_env,
                       [ [<<"http">>, <<"internal">>, <<"port">>],
                         aehttp, [internal, port], 8143], 5000),
    Params = #{sender_id => aeser_api_encoder:encode(account_pubkey, PK1),
               recipient_id => aeser_api_encoder:encode(account_pubkey, PK2),
               amount => Am,
               fee => Fee,
               payload => <<"foo">>},
    %% It's internal API so ext_addr is not included here.
    Cfg = [{ext_http, "http://127.0.0.1:" ++ integer_to_list(ExtPort)},
           {int_http, "http://127.0.0.1:" ++ integer_to_list(IntPort)}],
    ct:log(">>> PARAMS: ~p", [Params]),

    {ok, 200, #{tx := SpendTx}} = aehttp_client:request('PostSpend', Params, Cfg),
    SignedSpendTx = sign_tx(T1, SpendTx),
    {ok, 200, _} = aehttp_client:request('PostTransaction', #{tx => SignedSpendTx}, Cfg),
    ok.

ensure_new_tx(T, Tx) ->
    retry(fun() -> ensure_new_tx_(T, Tx) end,
          {?LINE, ensure_new_tx, T, Tx}).

ensure_new_tx_(T, Tx) ->
    {ok, Txs} = get_pool(T),
    lists:member(Tx, Txs).

config({devs, Devs}, Config) ->
    [ {devs, Devs}
    , {nodes, [aecore_suite_utils:node_tuple(Dev) || Dev <- Devs]}
    | Config
    ].

node_db_cfg(Node) ->
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(
                    fun(M, F, A)->
                        rpc:call(aecore_suite_utils:node_name(Node),
                                  M, F, A, 5000)
                    end),
    {ok, DbCfg}.

sign_tx(T, Tx) ->
    {ok, TxDec} = aeser_api_encoder:safe_decode(transaction, Tx),
    UnsignedTx = aetx:deserialize_from_binary(TxDec),
    {ok, SignedTx} = aecore_suite_utils:sign_on_node(T, UnsignedTx),
    aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)).

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Payload) ->
    add_spend_tx(Node, Amount, Fee, Nonce, TTL, Payload, patron(), new_pubkey()).

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Payload, Sender, Recipient) ->
    SenderId = aeser_id:create(account, maps:get(pubkey, Sender)),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => Payload,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, maps:get(privkey, Sender)),
    Res = rpc:call(Node, aec_tx_pool, push, [STx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_pubkey() ->
    #{ public := PubKey } = enacl:sign_keypair(),
    PubKey.

get_bench_config(Config) ->
    { proplists:get_value(generations, Config)
    , proplists:get_value(micro_per_generation, Config)
    , proplists:get_value(tx_per_micro, Config)}.
