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
    test_subscription/1,
    start_first_node/1,
    start_second_node/1,
    start_third_node/1,
    start_blocked_second/1,
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
    check_metrics_logged/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ {group, all_nodes} ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes},
                              {group, three_nodes},
                              {group, semantically_invalid_tx},
                              {group, one_blocked}]},
     {two_nodes, [sequence],
      [start_first_node,
       test_subscription,
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
       check_metrics_logged]},
     {three_nodes, [sequence],
      [start_first_node,
       test_subscription,
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
    DefCfg = #{<<"metrics">> =>
                   #{<<"rules">> =>
                         [#{<<"name">> => <<"ae.epoch.system.**">>,
                            <<"actions">> => <<"log">>},
                          #{<<"name">> => <<"ae.epoch.aecore.**">>,
                            <<"actions">> => <<"log,send">>}]},
              <<"chain">> => #{<<"persist">> => true}},
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
    ct:log("Peers on dev2: ~p", [rpc:call(N2, aec_peers, all, [], 5000)]),
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
    ct:log("Peers on dev3: ~p", [rpc:call(N3, aec_peers, all, [], 5000)]),
    ok = aecore_suite_utils:check_for_logs([dev3], Config),
    true = expect_same(T0, Config).

test_subscription(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    N = aecore_suite_utils:node_name(Dev1),
    aecore_suite_utils:subscribe(N, app_started),
    Debug0 = aecore_suite_utils:call_proxy(N, debug),
    ct:log("After subscription (~p, app_started): ~p", [self(), Debug0]),
    true = rpc:call(N, setup, patch_app, [sasl], 5000),
    {ok, _} = rpc:call(N, setup, reload_app, [sasl], 5000),
    ok = rpc:call(N, application, start, [sasl], 5000),
    receive
        {app_started, #{info := sasl}} ->
            ct:log("got app_started: sasl", []),
            ok
    after 1000 ->
            Debug = aecore_suite_utils:call_proxy(N, debug),
            Since = aecore_suite_utils:events_since(N, app_started, 0),
            ct:log("Debug = ~p~n"
                   "Since = ~p", [Debug, Since]),
            ok
    end.

mine_on_first(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    N = aecore_suite_utils:node_name(Dev1),
    aecore_suite_utils:mine_blocks(N, 1),
    ok.

start_blocked_second(Config) ->
    [ Dev1, Dev2 | _ ] = proplists:get_value(devs, Config),
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    T0 = os:timestamp(),
    aecore_suite_utils:start_node(Dev2, Config),
    aecore_suite_utils:connect(N2),
    timer:sleep(2000),
    ct:log("Peers on dev1: ~p", [rpc:call(N1, aec_peers, all, [], 5000)]),
    ct:log("Unblocked peers on dev1: ~p",
           [rpc:call(N1, aec_peers, get_random, [10], 5000)]),
    await_sync_abort(T0, [N1, N2]).

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
    {ok, PK2} = get_pubkey(N2),
    {ok, Bal1} = get_balance(N1),
    ct:log("Balance on dev1: ~p", [Bal1]),
    true = (is_integer(Bal1) andalso Bal1 > 0),
    {ok, Pool11} = get_pool(N1),
    {ok, Pool21} = get_pool(N2),
    Pool11 = Pool21,                % tx pools are ordered
    ok = new_tx(#{node1  => N1,
                  node2  => N2,
                  amount => AmountFun(Bal1),
                  sender    => PK1,
                  recipient => PK2,
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
    aecore_suite_utils:mine_blocks(N1, 1),
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
    [aecore_suite_utils:subscribe(N, chain_sync) || N <- Nodes],
    AllEvents = lists:flatten(
                  [aecore_suite_utils:events_since(N, chain_sync, T0) || N <- Nodes]),
    ct:log("AllEvents = ~p", [AllEvents]),
    Nodes1 =
        lists:foldl(
          fun(Msg, Acc) ->
                  check_sync_event(Msg, Acc)
          end, Nodes, AllEvents),
    ct:log("Nodes1 = ~p", [Nodes1]),
    collect_sync_events(Nodes1),
    expect_same_top(Nodes, 5),
    expect_same_tx(Nodes).

collect_sync_events([]) ->
    done;
collect_sync_events(Nodes) ->
    receive
        {gproc_ps_event, chain_sync, Msg} ->
            collect_sync_events(check_sync_event(Msg, Nodes))
    after 20000 ->
            ct:log("Timeout in collect_sync_events: ~p~n"
                   "~p", [Nodes, process_info(self(), messages)]),
            error(timeout)
    end.

check_sync_event(#{sender := From, info := Info}, Nodes) ->
    case Info of
        {E, _} when E =:= server_done; E =:= client_done ->
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.

await_sync_abort(T0, Nodes) ->
    [aecore_suite_utils:subscribe(N, chain_sync) || N <- Nodes],
    AllEvents = lists:flatten(
                  [aecore_suite_utils:events_since(N, chain_sync, T0) || N <- Nodes]),
    Nodes1 =
        lists:foldl(
          fun(Msg, Acc) ->
                  check_sync_abort_event(Msg, Acc)
          end, Nodes, AllEvents),
    ct:log("Nodes1 = ~p", [Nodes1]),
    await_sync_abort_(Nodes1).

await_sync_abort_([]) ->
    ok;
await_sync_abort_(Nodes) ->
    receive
        {gproc_ps_event, chain_sync, Msg} ->
            await_sync_abort_(check_sync_abort_event(Msg, Nodes))
    after 20000 ->
            ct:log("Timeout in await_sync_abort: ~p~n"
                   "~p", [Nodes, process_info(self(), messages)]),
            error(timeout)
    end.

check_sync_abort_event(#{sender := From, info := Info}, Nodes) ->
    case Info of
        {sync_aborted, _} ->
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.

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

new_tx(#{node1 := N1, node2 := N2, amount := Am, fee := Fee} = M) ->
    PK1 = maps_get(pk1, M, fun() -> ok(get_pubkey(N1)) end),
    PK2 = maps_get(pk2, M, fun() -> ok(get_pubkey(N2)) end),
    Port = rpc:call(N1, aeu_env, user_config_or_env, 
                    [ [<<"http">>, <<"internal">>, <<"port">>], 
                      aehttp, [internal, swagger_port], 8143], 5000),
    Uri = aeu_requests:pp_uri({http, "127.0.0.1", Port}),
    {ok, ok} = aeu_requests:new_spend_tx(
                 Uri, #{sender_pubkey => PK1,
                        recipient_pubkey => PK2,
                        amount => Am,
                        fee => Fee}),
    ok.

ensure_new_tx(N, Tx) ->
    retry(fun() -> ensure_new_tx_(N, Tx) end,
          {?LINE, ensure_new_tx, N, Tx}).

ensure_new_tx_(N, Tx) ->
    {ok, Txs} = get_pool(N),
    lists:member(Tx, Txs).

ok({ok, V}) ->
    V.

maps_get(K, #{} = M, Def) when is_function(Def, 0) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
             Def()
    end.

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
