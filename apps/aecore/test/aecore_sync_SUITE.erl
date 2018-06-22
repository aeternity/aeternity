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
    mine_on_third/1,
    mine_again_on_first/1,
    restart_first/1,
    restart_second/1,
    restart_third/1,
    tx_first_pays_second/1,
    tx_first_pays_second_more_it_can_afford/1,
    ensure_tx_pools_empty/1,
    ensure_tx_pools_one_tx/1,
    report_metrics/1,
    check_metrics_logged/1,
    crash_syncing_worker/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ {group, all_nodes} ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes},
                              {group, three_nodes},
                              {group, semantically_invalid_tx},
                              {group, one_blocked}
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
       ensure_tx_pools_empty,
       tx_first_pays_second_more_it_can_afford,
       mine_on_second,
       ensure_tx_pools_one_tx]},
     {one_blocked, [sequence],
      [start_first_node,
       mine_on_first,
       start_blocked_second]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.sync"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
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
        }
    },
    Config2 = aec_metrics_test_utils:make_port_map([dev1, dev2, dev3], Config1),
    aecore_suite_utils:create_configs(Config2, DefCfg, [{add_peers, true}]),
    aecore_suite_utils:make_multi(Config2),
    Config2.

end_per_suite(Config) ->
    stop_devs(Config).

init_per_group(two_nodes, Config) ->
    config({devs, [dev1, dev2]}, Config);
init_per_group(three_nodes, Config) ->
    config({devs, [dev1, dev2, dev3]}, Config);
init_per_group(semantically_invalid_tx, Config) ->
    config({devs, [dev1, dev2]}, Config);
init_per_group(one_blocked, Config) ->
    Config1 = config({devs, [dev1, dev2]}, Config),
    preblock_second(Config1),
    Config1;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_, Config) ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    aec_metrics_test_utils:stop_statsd_loggers(Config),
    stop_devs(Config).

stop_devs(Config) ->
    Devs = proplists:get_value(devs, Config, []),
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
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    aecore_suite_utils:start_node(Dev1, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(Dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config),
    ok.

start_second_node(Config) ->
    [ Dev1, Dev2 | _ ] = proplists:get_value(devs, Config),
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),
    aecore_suite_utils:await_aehttp(N2),
    ct:log("Connected peers on dev2: ~p",
           [rpc:call(N2, aec_peers, connected_peers, [], 5000)]),
    B1 = rpc:call(N1, aec_chain, top_block, [], 5000),
    ok = aecore_suite_utils:check_for_logs([dev2], Config),
    true = expect_block(N2, B1).

start_third_node(Config) ->
    [ _, _, Dev3 | _ ] = proplists:get_value(devs, Config),
    N3 = aecore_suite_utils:node_name(Dev3),
    T0 = os:timestamp(),
    aecore_suite_utils:start_node(Dev3, Config),
    aecore_suite_utils:connect(N3),
    aecore_suite_utils:await_aehttp(N3),
    ct:log("Connected peers on dev3: ~p",
           [rpc:call(N3, aec_peers, connected_peers, [], 5000)]),
    ok = aecore_suite_utils:check_for_logs([dev3], Config),
    true = expect_same(T0, Config).

mine_on_first_up_to_latest_consensus_protocol(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    N = aecore_suite_utils:node_name(Dev1),
    aecore_suite_utils:mine_blocks(N, aecore_suite_utils:latest_fork_height()),
    ok.

mine_on_first(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    N = aecore_suite_utils:node_name(Dev1),
    aecore_suite_utils:mine_blocks(N, 1),
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
    Connected  = rpc:call(N1, aec_peers, connected_peers, [], 5000),
    ct:log("Non-blocked peers on dev1: ~p", [NonBlocked]),
    ct:log("Connected peers on dev1: ~p", [Connected]),
    [] = Connected,
    [_] = NonBlocked,

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
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    {ok, PK1} = get_pubkey(N1),
    ct:log("PK1 = ~p", [PK1]),
    {ok, Bal1} = get_balance(N1),
    ct:log("Balance on dev1: ~p", [Bal1]),
    true = (is_integer(Bal1) andalso Bal1 > 0),
    {ok, Pool11} = get_pool(N1),
    {ok, Pool21} = get_pool(N2),
    Pool11 = Pool21,                % tx pools are ordered
    ok = new_tx(#{node1  => N1,
                  node2  => N2,
                  amount => AmountFun(Bal1),
                  fee    => 1}),
    {ok, Pool12} = get_pool(N1),
    [NewTx] = Pool12 -- Pool11,
    true = ensure_new_tx(N2, NewTx).

ensure_tx_pools_empty(Config) ->
    ensure_tx_pools_n_txs_(Config, 0).

ensure_tx_pools_one_tx(Config) ->
    ensure_tx_pools_n_txs_(Config, 1).

ensure_tx_pools_n_txs_(Config, TxsCount) ->
    Ns = [N || {_,N} <- ?config(nodes, Config)],
    retry(
      fun() ->
              [APool | RestPools] =
                  lists:map(
                    fun(N) ->
                            case get_pool(N) of
                                {ok, Pool} when is_list(Pool) ->
                                    ct:log("Pool (~p) = ~p", [N, Pool]),
                                    Pool
                            end
                    end, Ns),
              if
                  length(APool) =:= TxsCount ->
                      lists:all(fun(P) when P =:= APool -> true;
                                   (_) -> false
                                end, RestPools);
                  true ->
                      false
              end
      end, {?LINE, ensure_tx_pools_n_txs_, TxsCount, Ns}).

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
    Cmd1 = ["grep peers ", Dir, "/dev?/log/epoch_metrics.log"],
    Cmd2 = ["grep aecore ", Dir, "/dev?/log/epoch_metrics.log"],
    Res1 = aecore_suite_utils:cmd_res(aecore_suite_utils:cmd(Cmd1)),
    Res2 = aecore_suite_utils:cmd_res(aecore_suite_utils:cmd(Cmd2)),
    {[_|_], [], _} = Res1,
    {[_|_]   , [], _} = Res2,
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
    aecore_suite_utils:mine_blocks(N1, 4),
    {ok, []} = rpc:call(N1, aec_tx_pool, peek, [infinity], 5000),

    Top1 = rpc:call(N1, aec_chain, top_block, [], 5000),
    ct:log("top at Dev1 = ~p", [Top1]),
    ExtraBlocks = 5,  %% Might need more if CPU is really fast!
    aecore_suite_utils:mine_blocks(N1, ExtraBlocks),
    H1 = aec_blocks:height(Top1) + ExtraBlocks,

    %% Ensure compatible notation of uri:
    PeerId = aec_peers:peer_id(aecore_suite_utils:peer_info(Dev1)),
    ct:log("PeerId of Dev1 ~p", [PeerId]),

    spawn_link(fun() -> kill_sync_worker(N2, PeerId) end),

    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),
    ct:log("node connected ~p", [N2]),

    %% Set the same mining_rate to validate target
    %% Only needed when chain more than 10 blocks
    ok = rpc:call(N2, application, set_env, [aecore, expected_mine_rate, 10], 5000),

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
    case rpc:call(N, aec_sync, sync_in_progress, [PeerId], 5000) of
        false ->
            timer:sleep(10),
            kill_sync_worker(N, PeerId);
        {badrpc, _} ->
            timer:sleep(20),
            kill_sync_worker(N, PeerId);
        SyncPeer ->
            Pid = element(7, SyncPeer),
            exit(Pid, kill),
            ct:log("killed worker ~p", [SyncPeer])
    end.

mine_again_on_first(Config) ->
    mine_and_compare(aecore_suite_utils:node_name(dev1), Config).

mine_on_second(Config) ->
    mine_and_compare(aecore_suite_utils:node_name(dev2), Config).

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
    mine_and_compare(aecore_suite_utils:node_name(dev3), Config).

mine_and_compare(N1, Config) ->
    AllNodes = [N || {_, N} <- ?config(nodes, Config)],
    PrevTop = rpc:call(N1, aec_chain, top_block, [], 5000),
    %% If there are txs in the mempool, the second block will be a micro block.
    {ok, [KeyBlock, _KeyOrMicroBlock]} = aecore_suite_utils:mine_blocks(N1, 2),
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

get_pubkey(N) ->
    rpc:call(N, aec_keys, pubkey, [], 5000).

get_balance(N) ->
    rpc:call(N, aec_mining, get_miner_account_balance, [], 5000).

get_pool(N) ->
    rpc:call(N, aec_tx_pool, peek, [infinity], 5000).

new_tx(#{node1 := N1, node2 := N2, amount := Am, fee := Fee} = _M) ->
    PK1 = ok(get_pubkey(N1)),
    PK2 = ok(get_pubkey(N2)),
    Port = rpc:call(N1, aeu_env, user_config_or_env,
                    [ [<<"http">>, <<"internal">>, <<"port">>],
                      aehttp, [internal, port], 8143], 5000),
    Params = #{sender_pubkey => PK1,
               recipient_pubkey => aec_base58c:encode(account_pubkey, PK2),
               amount => Am,
               fee => Fee,
               payload => <<"foo">>},
    %% It's internal API so ext_addr is not included here.
    Cfg = [{int_http, "http://127.0.0.1:" ++ integer_to_list(Port)}],
    {ok, 200, _} = aehttp_client:request('PostSpendTx', Params, Cfg),
    ok.

ensure_new_tx(N, Tx) ->
    retry(fun() -> ensure_new_tx_(N, Tx) end,
          {?LINE, ensure_new_tx, N, Tx}).

ensure_new_tx_(N, Tx) ->
    {ok, Txs} = get_pool(N),
    lists:member(Tx, Txs).

ok({ok, V}) ->
    V.

preblock_second(Config) ->
    [Dev1, Dev2 | _] = proplists:get_value(devs, Config),
    EpochCfg = aecore_suite_utils:epoch_config(Dev1, Config),
    aecore_suite_utils:create_config(Dev1, Config, EpochCfg,
                                            [{block_peers, [ Dev2 ]},
                                             {add_peers, true}
                                            ]).

config({devs, Devs}, Config) ->
    aec_metrics_test_utils:start_statsd_loggers(
      [{devs, Devs}, {nodes, [aecore_suite_utils:node_tuple(Dev)
                              || Dev <- Devs]}
       | Config]).

node_db_cfg(Node) ->
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(
                    fun(M, F, A)->
                        rpc:call(aecore_suite_utils:node_name(Node),
                                  M, F, A, 5000)
                    end),
    {ok, DbCfg}.
