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
    mine_on_first/1,
    mine_on_second/1,
    mine_on_third/1,
    mine_again_on_first/1,
    restart_first/1,
    restart_second/1,
    restart_third/1,
    tx_first_pays_second/1,
    ensure_tx_pools_empty/1
   ]).

-export([start_proxy/0, proxy/0]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, all_nodes}
    ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes},
                              {group, three_nodes}]},
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
       restart_first]},
     {three_nodes, [sequence],
      [start_first_node,
       test_subscription,
       mine_on_first,
       start_second_node,
       start_third_node,
       mine_again_on_first,
       mine_on_second,
       mine_on_third,
       restart_third]}
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
    stop_node(dev3, Config),
    ok.

init_per_group(two_nodes, Config) ->
    [{nodes, [n(dev1),
              n(dev2)]} | Config];
init_per_group(three_nodes, Config) ->
    Config1 = [{nodes, [n(dev1),
                        n(dev2),
                        n(dev3)]} | Config],
    stop_nodes(Config1),
    Config1;
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

start_first_node(Config) ->
    start_node_(dev1, Config),
    connect(node_(dev1)),
    ok.

test_subscription(_Config) ->
    N = node_(dev1),
    subscribe(N, app_started),
    Debug0 = call_proxy(N, debug),
    ct:log("After subscription (~p, app_started): ~p", [self(), Debug0]),
    true = rpc:call(N, setup, patch_app, [sasl]),
    {ok, _} = rpc:call(N, setup, reload_app, [sasl]),
    ok = rpc:call(N, application, start, [sasl]),
    receive
        {app_started, #{info := sasl}} ->
            ct:log("got app_started: sasl", []),
            ok
    after 1000 ->
            Debug = call_proxy(N, debug),
            Since = events_since(N, app_started, 0),
            ct:log("Debug = ~p~n"
                   "Since = ~p", [Debug, Since]),
            ok
    end.

mine_on_first(_Config) ->
    N = node_(dev1),
    mine_one_block(N),
    ok.

start_second_node(Config) ->
    N1 = node_(dev1),
    N2 = node_(dev2),
    start_node_(dev2, Config),
    connect(N2),
    timer:sleep(2000),
    ct:log("Peers on dev2: ~p", [rpc_call(N2, aec_peers, all, [])]),
    {ok, B1} = rpc_call(N1, aec_chain, top, []),
    true = expect_block(N2, B1).

tx_first_pays_second(_Config) ->
    N1 = node_(dev1),
    N2 = node_(dev2),
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
                  amount => 1,
                  sender    => PK1,
                  recipient => PK2,
                  fee    => 1}),
    {ok, Pool12} = get_pool(N1),
    [NewTx] = Pool12 -- Pool11,
    true = ensure_new_tx(N2, NewTx).

ensure_tx_pools_empty(Config) ->
    Ns = [N || {_,N} <- ?config(nodes, Config)],
    retry(
      fun() ->
              Results = lists:map(
                          fun(N) ->
                                  {ok, Pool} = get_pool(N),
                                  ct:log("Pool (~p) = ~p", [N, Pool]),
                                  Pool
                          end, Ns),
              lists:all(fun([]) -> true;
                           (_)  -> false
                        end, Results)
      end, {?LINE, ensure_tx_pools_empty, Ns}).


mine_again_on_first(Config) ->
    mine_and_compare(node_(dev1), Config).

mine_on_second(Config) ->
    mine_and_compare(node_(dev2), Config).

restart_first(Config) ->
    restart_node(dev1, Config).

restart_second(Config) ->
    restart_node(dev2, Config).

restart_third(Config) ->
    restart_node(dev3, Config).

restart_node(Dev, Config) ->
    stop_node(Dev, Config),
    T0 = os:timestamp(),
    start_node_(Dev, Config),
    N = node_(Dev),
    connect(N),
    ct:log("~w restarted", [Dev]),
    true = expect_same(T0, Config).


start_third_node(Config) ->
    N3 = node_(dev3),
    T0 = os:timestamp(),
    start_node_(dev3, Config),
    connect(N3),
    await_aehttp(N3),
    ct:log("Peers on dev3: ~p", [rpc_call(N3, aec_peers, all, [])]),
    true = expect_same(T0, Config).

mine_on_third(Config) ->
    mine_and_compare(node_(dev3), Config).

mine_and_compare(N1, Config) ->
    AllNodes = [N || {_, N} <- ?config(nodes, Config)],
    {ok, PrevTop} = rpc_call(N1, aec_chain, top, []),
    ok = mine_one_block(N1),
    {ok, NewTop} = rpc_call(N1, aec_chain, top, []),
    true = (NewTop =/= PrevTop),
    Bal1 = get_balance(N1),
    ct:log("Balance on dev1: ~p", [Bal1]),
    lists:foreach(
      fun(Nx) ->
              true = expect_block(Nx, NewTop)
      end, AllNodes -- [N1]).

expect_same(T0, Config) ->
    Nodes = [N || {_, N} <- ?config(nodes, Config)],
    AllEvents = lists:flatten(
                  [events_since(N, chain_sync, T0) || N <- Nodes]),
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
        {gproc_ps, event, chain_sync, Msg} ->
            collect_sync_events(check_sync_event(Msg, Nodes))
    after 20000 ->
            ct:log("Timeout in collect_sync_events: ~p~n"
                   "~p", [Nodes, process_info(self(), messages)]),
            error(timeout)
    end.

check_sync_event(#{sender := From, info := Info}, Nodes) ->
    case Info of
        {E, _} when E =:= server_waiting; E =:= client_done ->
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.

expect_same_top(Nodes, Tries) when Tries > 0 ->
    Blocks = lists:map(
               fun(N) ->
                       {ok, B} = rpc_call(N, aec_chain, top, []),
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

mine_one_block(N) ->
    subscribe(N, block_created),
    StartRes = rpc_call(N, aec_conductor, start_mining, []),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [N, StartRes]),
    receive
        {gproc_ps_event, block_created, Info} ->
            StopRes = rpc_call(N, aec_conductor, stop_mining, []),
            ct:log("aec_conductor:stop_mining() (~p) -> ~p", [N, StopRes]),
            ct:log("block created, Info=~p", [Info]),
            ok
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

start_proxy() ->
    io:fwrite("starting proxy...~n", []),
    proc_lib:spawn(?MODULE, proxy, []).

proxy() ->
    register(?PROXY, self()),
    process_flag(trap_exit, true),
    aec_test_event_handler:install(),
    error_logger:info_msg("starting test suite proxy~n", []),
    proxy_loop([{marker, app_started}], dict:new()).

proxy_loop(Subs, Events) ->
    receive
        {From, Ref, debug} ->
            From ! {Ref, #{pid => self(),
                           subs => Subs,
                           events => Events}},
            proxy_loop(Subs, Events);
        {From, Ref, {subscribe, Event}} ->
            case lists:keymember(Event, 2, Subs) of
                true ->
                    From ! {Ref, ok},
                    proxy_loop([{From, Event}|Subs], Events);
                false ->
                    case lists:member(Event, events()) of
                        true ->  % pre-subscribed
                            From ! {Ref, ok},
                            proxy_loop([{From, Event}|Subs], Events);
                        false ->
                            case catch aec_events:subscribe(Event) of
                                ok ->
                                    From ! {Ref, ok},
                                    proxy_loop(
                                      [{From, Event}|
                                       ensure_markers([Event], Subs)], Events);
                                Other ->
                                    From ! {Ref, Other},
                                    proxy_loop(Subs, Events)
                            end
                    end
            end;
        {From, Ref, {unsubscribe, Event}} ->
            From ! {Ref, ok},
            proxy_loop([S || S <- Subs,
                             S =/= {From, Event}], Events);
        {From, Ref, {events, E, Since}} ->
            Res = case dict:find(E, Events) of
                      error -> [];
                      {ok, Es} ->
                          lists:dropwhile(
                            fun(#{time := T}) -> T < Since end, Es)
                  end,
            From ! {Ref, Res},
            proxy_loop(Subs, Events);
        {gproc_ps_event, Event, Info} = Msg ->
            tell_subscribers(Subs, Event, Msg),
            proxy_loop(Subs, dict:append(Event, Info, Events));
        {application_started, T, App} ->
            Info = #{time => T, info => App},
            tell_subscribers(Subs, app_started, {app_started, Info}),
            Subs1 = case App of
                        gproc ->
                            Es = set_subscriptions(),
                            ensure_markers(Es, Subs);
                        _ -> Subs
                    end,
            proxy_loop(Subs1, dict:append(
                               app_started,
                               #{time => T, info => App}, Events));
        Other ->
            io:fwrite("Proxy got ~p~n", [Other]),
            proxy_loop(Subs, Events)
    end.

tell_subscribers(Subs, Event, Msg) ->
    lists:foreach(
      fun({P, E}) when E =:= Event, is_pid(P) ->
              P ! Msg;
         (_) ->
              ok
      end, Subs).

set_subscriptions() ->
    Es = events(),
    [aec_events:subscribe(E) || E <- Es],
    Es.

ensure_markers(Es, Subs) ->
    lists:foldl(
      fun(E, Acc) ->
              case lists:member({marker, E}, Acc) of
                  true ->
                      Acc;
                  false ->
                      [{marker, E}|Acc]
              end
      end, Subs, Es).

events() -> [block_created, chain_sync].

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

call_proxy(N, Req) ->
    call_proxy(N, Req, 3000).

call_proxy(N, Req, Timeout) ->
    Ref = erlang:monitor(process, {?PROXY, N}),
    {?PROXY, N} ! {self(), Ref, Req},
    receive
        {'DOWN', Ref, _, _, Reason} ->
            error({proxy_died, N, Reason});
        {Ref, Result} ->
            erlang:demonitor(Ref),
            Result
    after Timeout ->
            error(proxy_call_timeout)
    end.


%% ==================================================
%% Private functions
%% ==================================================

%% set_trace(N, Spec) ->
%%     dbg:n(N),
%%     lists:map(
%%       fun(tracer) ->
%%               {tracer,
%%                dbg:tracer(process, {fun(Msg,_) ->
%%                                             ct:log(">>~p~n", [Msg]), 0
%%                                     end, 0})};
%%          (stop) ->
%%               {stop, dbg:stop()};
%%          ({Op, Args} = S) when Op==tp; Op==tpl; Op==ct; Op==ctpl;
%%                                Op==p; Op==n; Op==cn ->
%%               {S, apply(dbg, Op, Args)}
%%       end, Spec).

stop_nodes(Config) ->
    [stop_node(N, Config) || {N,_} <- ?config(nodes, Config)].

stop_node(N, Config) ->
    cmd(["(cd ", node_shortcut(N, Config),
         " && ./bin/epoch stop)"]).

%% stop_node(N, Config) ->
%%     Top = ?config(top_dir, Config),
%%     cmd(["(cd ", Top, " && make ", atom_to_list(N), "-stop)"]).

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
    %% cp_and_mod(filename:join(CfgD, "vm.args"),
    %%            filename:join(RelD, "vm.args"),
    %%            fun(Data) -> change_sname(Data, N) end),
    delete_file(filename:join(RelD, "vm.args.orig")),
    delete_file(filename:join(RelD, "sys.config.orig")),
    TestD = filename:join(filename:dirname(code:which(?MODULE)), "data"),
    cp_file(filename:join(TestD, "sync_SUITE.config"),
            filename:join(DDir , "sync_SUITE.config")).
    %% modify_vm_args(N, filename:join(RelD, "vm.args")).
    %% modify_sys_config(filename:join(RelD, "sys.config")).
    %% cmd(["(cd ", TopDir, " && make multi-build)"]).


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

%% cp_and_mod(From, To, F) ->
%%     {ok, B} = file:read_file(From),
%%     B1 = F(B),
%%     ok = file:write_file(To, B1),
%%     ct:log("Wrote to ~s:~n"
%%            "\"~s\"", [To, B1]),
%%     ok.

%% change_sname(Data, N) ->
%%     re:replace(Data, "epoch_dev1", "epoch_" ++ atom_to_list(N),
%%                [{return, binary}]).

%% modify_vm_args(dev1, _) ->
%%     ok;
%% modify_vm_args(N, F) ->
%%     {ok, Bin} = file:read_file(F),
%%     Bin1 = re:replace(Bin, "epoch_dev1", "epoch_" ++ atom_to_list(N),
%%                       [{return, binary}]),
%%     ct:log("modify vm.args (~p):~n"
%%            "~p ->~n   ~p", [filename:dirname(F), Bin, Bin1]),
%%     file:write_file(F, Bin1).


%% modify_sys_config(SysCfg) ->
%%     ct:log("modify_sys_config(~p)", [SysCfg]),
%%     {ok, [Terms]} = file:consult(SysCfg),
%%     Terms1 =
%%         whitelist_test_handler(
%%           add_setup_phase(Terms)),
%%     ct:log("Terms1 = ~p~n", [Terms1]),
%%     {ok, Fd} = file:open(SysCfg, [write]),
%%     try io:fwrite(Fd, "~p.~n", [Terms1]),
%%          ct:log("wrote sys.config", [])
%%     after
%%         file:close(Fd)
%%     end,
%%     check_sys_config(SysCfg, Terms1).

%% check_sys_config(SysCfg, Terms) ->
%%     {ok, [Terms]} = file:consult(SysCfg),
%%     ct:log("sys.config verified (~p)", [SysCfg]),
%%     ok.

%% whitelist_test_handler(Terms) ->
%%     Handler = aec_test_event_handler,
%%     K = error_logger_whitelist,
%%     Lager =
%%         case lists:keyfind(lager, 1, Terms) of
%%             {lager, Env} ->
%%                 Env1 =
%%                     case lists:keyfind(K, 1, Env) of
%%                         {_, L} ->
%%                             L1 = [Handler|L -- [Handler]],
%%                             lists:keystore(K, 1, Env, {K, L1});
%%                         false  ->
%%                             lists:keystore(K, 1, Env, {K, [Handler]})
%%                     end,
%%                 {lager, Env1};
%%             false ->
%%                 {lager, [{K, [Handler]}]}
%%         end,
%%     lists:keystore(lager, 1, Terms, Lager).

%% add_setup_phase(Terms) ->
%%     Hook = {normal, [
%%                      {10, {?MODULE, start_proxy, []}}
%%                     ]},
%%     Setup =
%%         case lists:keyfind(setup, 1, Terms) of
%%             {_, Env} ->
%%                 case lists:keyfind('$setup_hooks', 1, Env) of
%%                     {_, Hooks} ->
%%                         {setup, lists:keystore(
%%                                   '$setup_hooks', 1, Env, [Hook|Hooks])};
%%                     false ->
%%                         {setup, [{'$setup_hooks', [Hook]}]}
%%                 end;
%%             false ->
%%                 {setup, [{'$setup_hooks', [Hook]}]}
%%         end,
%%     lists:keystore(setup, 1, Terms, Setup).

%% copy_files(Ops) ->
%%     lists:map(
%%       fun({ln, From, To}) ->
%%               file:make_symlink(From, To);
%%          ({cp, From, To}) ->
%%               cmd(["cp -r ", From, " ", To])
%%       end, Ops).

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
            %% {ok,Bin} = file:read_file(code:which(?MODULE)),
            %% LoadRes =
            %%     rpc_call(N, code, load_binary,
            %%              [?MODULE, "./" ++ ?MODULE_STRING ++ ".beam", Bin]),
            %% ct:log("LoadRes = ~p", [LoadRes]),
            %% rpc_call(N, proc_lib, spawn, [?MODULE, proxy, []]),
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

%% start_tgt(dev1) -> "dev1-start";
%% start_tgt(dev2) -> "dev2-start";
%% start_tgt(dev3) -> "dev3-start".

expect_block(N, B) ->
    retry(fun() -> expect_block_(N, B) end,
          {?LINE, expect_block, N, B}).

expect_block_(N, B) ->
    {ok, Bn} = rpc_call(N, aec_chain, top, []),
    case B =:= Bn of
        true ->
            Bal = get_balance(N),
            ct:log("Got block (~p); Balance = ~p", [N, Bal]),
            true;
        false ->
            {false, Bn}
    end.

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
    rpc_call(N, aec_keys, pubkey, []).

get_balance(N) ->
    rpc_call(N, aec_conductor, get_miner_account_balance, []).

get_pool(N) ->
    rpc_call(N, aec_tx_pool, peek, [infinity]).

new_tx(#{node1 := N1, node2 := N2, amount := Am, fee := Fee} = M) ->
    PK1 = maps_get(pk1, M, fun() -> ok(get_pubkey(N1)) end),
    PK2 = maps_get(pk2, M, fun() -> ok(get_pubkey(N2)) end),
    Uri = rpc_call(N1, aehttp_app, local_internal_http_uri, []),
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
       <<"websocket">> =>
           #{<<"acceptors">> => 10}
      }.

%% data_dir(Config) ->
%%     ?config(data_dir, Config).

priv_dir(Config) ->
    ?config(priv_dir, Config).

%% node_dir(N, Config) ->
%%     filename:join(priv_dir(Config), N).

make_shortcut(Config) ->
    PrivDir  = priv_dir(Config),
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
    filename:join(Top, "_build/test/logs/latest.sync").


data_dir(N, Config) ->
    filename:join(node_shortcut(N, Config), "data").

keys_dir(N, Config) ->
    filename:join(data_dir(N, Config), "keys").

epoch_config(N, Config) ->
    filename:join(data_dir(N, Config), "epoch.json").

bin(S) ->
    iolist_to_binary(S).
