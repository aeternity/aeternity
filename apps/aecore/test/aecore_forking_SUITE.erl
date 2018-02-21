-module(aecore_forking_SUITE).

%% This code is brutaly copied form aecore_sync_SUITE and should use joined code base.

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
    create_dev1_chain/1,
    create_dev2_chain/1,
    sync_fork_in_wrong_order/1
   ]).


-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, all_nodes}
    ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes}]},
     {two_nodes, [sequence],
      [create_dev1_chain,
       create_dev2_chain,
       sync_fork_in_wrong_order]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.fork"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_configs(Config1),
    aecore_suite_utils:make_multi(Config1),
    Config1.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(dev1, Config),
    aecore_suite_utils:stop_node(dev2, Config),
    ok.

init_per_group(two_nodes, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(dev1),
              aecore_suite_utils:node_tuple(dev2)]} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
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

create_dev1_chain(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    {ok, Blocks} = aecore_suite_utils:mine_blocks(N1, 8), 
    true = (length(lists:usort(Blocks)) >= 4),
    N1Top = rpc:call(N1, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev1: ~p (mined ~p)", [ N1Top, hd(Blocks)]),
    N1Top = hd(Blocks),
    aecore_suite_utils:stop_node(dev1, Config),   %% make sure we do not sync with dev2.
    ok = aecore_suite_utils:check_for_logs([dev1], Config),
    ok.

create_dev2_chain(Config) ->
    aecore_suite_utils:start_node(dev2, Config),
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:connect(N2),
    aecore_suite_utils:mine_blocks(N2, 1),
    ForkTop = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("top of fork dev2: ~p", [ ForkTop ]),
    aecore_suite_utils:stop_node(dev2, Config),
    ok = aecore_suite_utils:check_for_logs([dev2], Config),
    ok.

sync_fork_in_wrong_order(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    N1Top = rpc:call(N1, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev1: ~p", [ N1Top ]),
    aecore_suite_utils:stop_node(dev1, Config),
   
    aecore_suite_utils:start_node(dev2, Config),
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:connect(N2),
    ForkTop = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev2: ~p", [ ForkTop ]),
    
    false = (ForkTop == N1Top),
    timer:sleep(100),
    %% unexepctedly last block of dev1 arrives before rest of the chain
    ok = rpc:call(N2, aec_conductor, post_block, [N1Top], 5000),

    T0 = os:timestamp(),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(N1),
    await_sync_complete(T0, [N2]),

    N2Top = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev2: ~p", [ N2Top ]),
    {N1Top, N2Top} = {N2Top, N1Top},
    ok.

await_sync_complete(T0, Nodes) ->
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
    collect_sync_events(Nodes1).

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

check_sync_event(#{sender := From, info := Info} = Msg, Nodes) ->
    case Info of
        {E, _} when E =:= client_done ->
            ct:log("got sync_event ~p", [Msg]),
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.

