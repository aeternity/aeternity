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
    TopDir = top_dir(DataDir),
    Config1 = [{top_dir, TopDir}|Config],
    make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    create_configs(Config1),
    make_multi(Config1),
    Config1.

end_per_suite(Config) ->
    stop_node(dev1, Config),
    stop_node(dev2, Config),
    ok.

init_per_group(two_nodes, Config) ->
    [{nodes, [n(dev1),
              n(dev2)]} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

create_dev1_chain(Config) ->
    start_node_(dev1, Config),
    N1 = node_(dev1),
    connect(N1),
    Blocks = [ mine_one_block(N1) || _ <- lists:seq(1,8)],
    true = (length(lists:usort(Blocks)) >= 4),
    N1Top = lists:last(Blocks),
    ct:log("top of chain dev1: ~p", [ N1Top ]),
    stop_node(dev1, Config),   %% make sure we do not sync with dev2.
    ok.

create_dev2_chain(Config) ->
    start_node_(dev2, Config),
    N2 = node_(dev2),
    connect(N2),
    mine_one_block(N2),
    {ok, ForkTop} = rpc_call(N2, aec_chain, top, []),
    ct:log("top of fork dev2: ~p", [ ForkTop ]),
    stop_node(dev2, Config),
    ok.

sync_fork_in_wrong_order(Config) ->
    start_node_(dev1, Config),
    N1 = node_(dev1),
    connect(N1),
    {ok, N1Top} = rpc_call(N1, aec_chain, top, []),
    ct:log("top of chain dev1: ~p", [ N1Top ]),
    stop_node(dev1, Config),
   
    start_node_(dev2, Config),
    N2 = node_(dev2),
    connect(N2),
    {ok, ForkTop} = rpc_call(N2, aec_chain, top, []),
    ct:log("top of chain dev2: ~p", [ ForkTop ]),
    
    false = (ForkTop == N1Top),
    timer:sleep(100),
    %% unexepctedly last block of dev1 arrives before rest of the chain
    rpc_call(N2, aec_conductor, post_block, [N1Top]), 

    T0 = os:timestamp(),
    start_node_(dev1, Config),
    connect(N1),
    await_sync_complete(T0, [N1, N2]),

    {ok, N2Top} = rpc_call(N2, aec_chain, top, []),
    ct:log("top of chain dev2: ~p", [ N2Top ]),
    true = (N1Top == N2Top),
    ok.


await_sync_complete(T0, Nodes) ->
    [subscribe(N, chain_sync) || N <- Nodes],
    AllEvents = lists:flatten(
                  [events_since(N, chain_sync, T0) || N <- Nodes]),
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
        {E, _} when E =:= server_done; E =:= client_done ->
            ct:log("got sync_event ~p", [Msg]),
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.


mine_one_block(N) ->
    subscribe(N, block_created),
    rpc_call(N, application, set_env, [aecore, aec_pow_cuckoo, {"lean16", "-t 5", 16}]), 
    StartRes = rpc_call(N, aec_conductor, start_mining, []),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [N, StartRes]),
    receive
        {gproc_ps_event, block_created, Info} ->
            StopRes = rpc_call(N, aec_conductor, stop_mining, []),
            ct:log("aec_conductor:stop_mining() (~p) -> ~p", [N, StopRes]),
            ct:log("block created, Info=~p", [Info]),
            maps:get(info, Info)
    after 30000 ->
            StopRes = rpc_call(N, aec_conductor, stop_mining, []),
            ct:log("aec_conductor:stop_mining() (~p) -> ~p", [N, StopRes]),
            ct:log("timeout waiting for block event~n"
                   "~p", [process_info(self(), messages)]),
            error(timeout_waiting_for_block)
    end.

%% ============================================================
%% Proxy process
%% ============================================================

-define(PROXY, epoch_multi_node_test_proxy).



rpc_call(N, M, F, A) ->
    %% use a default timeout
    rpc:call(N, M, F, A, _Timeout = 5000).

all_events_since(N, TS) ->
    [{E, try events_since(N, E, TS) catch error:Err -> Err end}
     || E <- [block_created, chain_sync, app_started]].

events_since(N, EvType, TS) ->
    call_proxy(N, {events, EvType, TS}).

subscribe(N, Event) ->
    call_proxy(N, {subscribe, Event}).

unsubscribe(N, Event) ->
    call_proxy(N, {unsubscribe, Event}).

-define(PROXY_CALL_RETRIES, 5).

call_proxy(N, Req) ->
    call_proxy(N, Req, ?PROXY_CALL_RETRIES, 3000).

call_proxy(N, Req, Tries, Timeout) when Tries > 0 ->
    Ref = erlang:monitor(process, {?PROXY, N}),
    {?PROXY, N} ! {self(), Ref, Req},
    receive
        {'DOWN', Ref, _, _, noproc} ->
            ct:log("proxy not yet there, retrying in 1 sec...", []),
            receive
            after 1000 ->
                    call_proxy(N, Req, Tries-1, Timeout)
            end;
        {'DOWN', Ref, _, _, Reason} ->
            error({proxy_died, N, Reason});
        {Ref, Result} ->
            erlang:demonitor(Ref),
            Result
    after Timeout ->
            error(proxy_call_timeout)
    end;
call_proxy(N, _, _, _) ->
    erlang:error({proxy_not_running, N}).



%% ==================================================
%% Private functions
%% ==================================================


stop_node(N, Config) ->
    cmd(["(cd ", node_shortcut(N, Config),
         " && ./bin/epoch stop)"]).

%% Split the DataDir path at "_build"
top_dir(DataDir) ->
    [Top, _] = re:split(DataDir, "_build", [{return, list}]),
    Top.

make_multi(Config) ->
    Top = ?config(top_dir, Config),
    ct:log("Top = ~p", [Top]),
    Epoch = filename:join(Top, "_build/test/rel/epoch"),
    [setup_node(N, Top, Epoch, Config) || N <- [dev1, dev2, dev3]].

setup_node(N, Top, Epoch, Config) ->
    ct:log("setup_node(~p,Config)", [N]),
    DDir = node_shortcut(N, Config),
    filelib:ensure_dir(filename:join(DDir, "foo")),
    cp_dir(filename:join(Epoch, "releases"), DDir ++ "/"),
    cp_dir(filename:join(Epoch, "bin"), DDir ++ "/"),
    symlink(filename:join(Epoch, "lib"), filename:join(DDir, "lib")),
    %%
    CfgD = filename:join([Top, "config/", N]),
    RelD = filename:dirname(
             hd(filelib:wildcard(
                    filename:join(DDir, "releases/*/epoch.rel")))),
    cp_file(filename:join(CfgD, "sys.config"),
            filename:join(RelD, "sys.config")),
    cp_file(filename:join(CfgD, "vm.args"),
            filename:join(RelD, "vm.args")),
    delete_file(filename:join(RelD, "vm.args.orig")),
    delete_file(filename:join(RelD, "sys.config.orig")),
    TestD = filename:join(filename:dirname(code:which(?MODULE)), "data"),
    cp_file(filename:join(TestD, "sync_SUITE.config"),
            filename:join(DDir , "sync_SUITE.config")).



cp_dir(From, To) ->
    cmd(["cp -r ", From, " ", To]).

cp_file(From, To) ->
    {ok, _} = file:copy(From, To),
    ct:log("Copied ~s to ~s", [From, To]),
    ok.

symlink(From, To) ->
    ok = file:make_symlink(From, To),
    ct:log("symlinked ~s to ~s", [From, To]),
    ok.

cmd(C) ->
    Cmd = binary_to_list(iolist_to_binary(C)),
    CmdRes = exec:run(Cmd, [sync, stdout, stderr]),
    {Fmt, Args} =
        case cmd_res(CmdRes) of
            {Out, "", []}    -> {"> ~s~n~s", [Cmd, Out]};
            {Out, Err, []}   -> {"> ~s~n~s~nERR: ~n", [Cmd, Out, Err]};
            {Out, Err, Rest} ->
                {"> ~s~n~s~nERR: ~s~nRest = ~p", [Cmd, Out, Err, Rest]}
        end,
    ct:log(Fmt, Args),
    CmdRes.

cmd_res({_, L}) ->
    {Err,_L1} = take(stderr, L, ""),
    {Out,L2} = take(stdout, L, ""),
    {Out, Err, L2}.

take(K, L, Def) ->
    case lists:keytake(K, 1, L) of
        false ->
            {Def, L};
        {value, {_, V}, Rest} ->
            {V, Rest}
    end.

n(N) when N == dev1; N == dev2; N == dev3 ->
    {N, node_(N)}.

node_(N) when N == dev1; N == dev2; N == dev3 ->
    [_,H] = re:split(atom_to_list(node()), "@", [{return,list}]),
    list_to_atom("epoch_" ++ atom_to_list(N) ++ "@" ++ H).

connect(N) ->
    connect(N, 5),
    report_node_config(N).

connect(N, Tries) when Tries > 0 ->
    case net_kernel:hidden_connect(N) of
        true ->
            ct:log("hidden_connect(~p) -> true", [N]),
            await_aehttp(N),
            true;
        false ->
            ct:log("hidden_connect(~p) -> false, retrying ...", [N]),
            timer:sleep(2000),
            connect(N, Tries-1)
    end;
connect(N, _) ->
    ct:log("exhausted retries (~p)", [N]),
    erlang:error({could_not_connect, N}).


start_node_(N, Config) ->
    %% Flags = "-pa " ++ ?config(priv_dir, Config),
    MyDir = filename:dirname(code:which(?MODULE)),
    Flags = ["-pa ", MyDir, " -config ./sync_SUITE"],
    cmd(["(cd ", node_shortcut(N, Config),
         " && ERL_FLAGS=\"", Flags, "\"",
         " EPOCH_CONFIG=./data/epoch.json"
         " RUNNER_LOG_DIR=`pwd`/log"
         " CODE_LOADING_MODE=interactive"
         " ./bin/epoch start)"]).

report_node_config(_N) ->
    [ct:log("~w env: ~p", [A, application:get_all_env(A)]) ||
        A <- [aeutil, aecore, aehttp]].


await_aehttp(N) ->
    subscribe(N, app_started),
    Events = events_since(N, app_started, 0),
    ct:log("`app_started` Events since 0: ~p", [Events]),
    case [true || #{info := aehttp} <- Events] of
        [] ->
            receive
                {app_started, #{info := aehttp}} ->
                    ct:log("aehttp started", []),
                    ok
            after 20000 ->
                    error(timeout_waiting_for_aehttp)
            end;
        [_|_] ->
            ct:log("aehttp already started", []),
            ok
    end,
    unsubscribe(N, app_started),
    ok.


%% ============================================================
%% Transaction support functions
%% ============================================================

create_configs(Config) ->
    [create_config(N, Config) || N <- [dev1, dev2, dev3]].

create_config(N, Config) ->
    EpochCfg = epoch_config(N, Config),
    ok = filelib:ensure_dir(EpochCfg),
    write_config(EpochCfg, config(N, Config)).

write_config(F, Config) ->
    JSON = jsx:prettify(jsx:encode(Config)),
    {ok, Fd} = file:open(F, [write]),
    ct:log("Writing config (~p)~n~s", [F, JSON]),
    try io:fwrite(Fd, "~s~n", [JSON])
    after
        file:close(Fd)
    end,
    VRes = aeu_env:check_config(F),
    ct:log("Config (~p) check: ~p", [F, VRes]),
    {ok,_} = VRes.

config(N, Config) ->
    {A,B,C} = os:timestamp(),
     #{<<"keys">> =>
           #{<<"dir">> => bin(keys_dir(N, Config)),
             <<"password">> => bin(io_lib:format("~w.~w.~w", [A,B,C]))},
       <<"logging">> =>
           #{<<"hwm">> => 500},
       <<"mining">> =>
           #{<<"autostart">> => false},
       <<"chain">> =>
           #{<<"persist">> => true},
       <<"websocket">> =>
           #{<<"internal">> =>
                 #{<<"acceptors">> => 10}
            }
      }.

priv_dir(Config) ->
    filename:join(?config(priv_dir, Config), ?MODULE_STRING).

make_shortcut(Config) ->
    PrivDir  = priv_dir(Config),
    ok = filelib:ensure_dir(filename:join(PrivDir, "foo")),
    Shortcut = shortcut_dir(Config),
    delete_file(Shortcut),
    ok = file:make_symlink(PrivDir, Shortcut),
    ct:log("Made symlink ~s to ~s", [PrivDir, Shortcut]),
    ok.

delete_file(F) ->
    case file:delete(F) of
        ok -> ok;
        {error, enoent} -> ok;
        Other ->
            erlang:error(Other, [F])
    end.

node_shortcut(N, Config) ->
    filename:join(shortcut_dir(Config), N).

shortcut_dir(Config) ->
    Top = ?config(top_dir, Config),
    filename:join(Top, "_build/test/logs/latest.fork").

data_dir(N, Config) ->
    filename:join(node_shortcut(N, Config), "data").

keys_dir(N, Config) ->
    filename:join(data_dir(N, Config), "keys").

epoch_config(N, Config) ->
    filename:join(data_dir(N, Config), "epoch.json").

bin(S) ->
    iolist_to_binary(S).
