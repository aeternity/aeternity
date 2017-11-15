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

-export([proxy/0]).

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
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    patch_files(Config1),
    create_configs(Config),
    make_multi(TopDir),
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
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% ============================================================
%% Test cases
%% ============================================================

start_first_node(Config) ->
    start_node_(dev1, Config),
    connect(node_(dev1)),
    ok.

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
    Bal1 = get_balance(N1),
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
    start_node_(Dev, Config),
    N = node_(Dev),
    connect(N),
    ct:log("~w restarted", [Dev]),
    true = expect_same(Config).


start_third_node(Config) ->
    N3 = node_(dev3),
    start_node_(dev3, Config),
    connect(N3),
    timer:sleep(2000),
    ct:log("Peers on dev3: ~p", [rpc_call(N3, aec_peers, all, [])]),
    true = expect_same(Config).

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

expect_same(Config) ->
    Nodes = ?config(nodes, Config),
    expect_same(Nodes, 5),
    expect_same_tx(Nodes).

expect_same(Nodes, Tries) when Tries > 0 ->
    Blocks = lists:map(
               fun({_, N}) ->
                       {ok, B} = rpc_call(N, aec_chain, top, []),
                       {N, B}
               end, Nodes),
    case lists:ukeysort(2, Blocks) of
        [_] ->
            ok;
        [_,_|_] = Dups ->
            ct:log("Blocks differ, retrying:~n~p", [Dups]),
            timer:sleep(2000),
            expect_same(Nodes, Tries-1)
    end;
expect_same(Nodes, _) ->
    ct:log("tries exhausted", []),
    erlang:error({top_blocks_differ, Nodes}).

expect_same_tx(Nodes0) ->
    Nodes = [N || {_, N} <- Nodes0],
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
    rpc_call(N, aec_miner, resume, []),
    receive
        {gproc_ps_event, block_created, Info} ->
            rpc_call(N, aec_miner, suspend, []),
            ct:log("block created, Info=~p", [Info]),
            ok
    after 30000 ->
            rpc_call(N, aec_miner, suspend, []),
            error(timeout_waiting_for_block)
    end.


%% ============================================================
%% Proxy process
%% ============================================================

-define(PROXY, epoch_multi_node_test_proxy).

proxy() ->
    register(?PROXY, self()),
    process_flag(trap_exit, true),
    proxy_loop([]).

proxy_loop(Subs) ->
    receive
        {From, Ref, {subscribe, Event}} ->
            case lists:keymember(Event, 2, Subs) of
                true ->
                    From ! {Ref, ok},
                    proxy_loop([{From, Event}|Subs]);
                false ->
                    Res = (catch aec_events:subscribe(Event)),
                    From ! {Ref, Res},
                    proxy_loop([{From, Event}|Subs])
            end;
        {From, Ref, {unsubscribe, Event}} ->
            From ! {Ref, ok},
            catch aec_events:unsubscribe(Event),
            proxy_loop([S || S <- Subs,
                             S =/= {From, Event}]);
        {gproc_ps_event, Event, _} = Msg ->
            lists:foreach(
              fun({P, E}) when E == Event ->
                      P ! Msg;
                 (_) ->
                      ok
              end, Subs),
            proxy_loop(Subs)
    end.

rpc_call(N, M, F, A) ->
    %% use a default timeout
    rpc:call(N, M, F, A, _Timeout = 5000).

subscribe(N, Event) ->
    call_proxy(N, {subscribe, Event}).

%% unsubscribe(N, Event) ->
%%     call_proxy(N, {unsubscribe, Event}).

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
    Top = ?config(top_dir, Config),
    cmd(["(cd ", Top, " && make ", atom_to_list(N), "-stop)"]).

%% Split the DataDir path at "_build"
top_dir(DataDir) ->
    [Top, _] = re:split(DataDir, "_build", []),
    Top.

make_multi(TopDir) ->
    cmd(["(cd ", TopDir, " && make multi-build)"]).

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

%%
%% Patches:
%% - use fast mining NIF (aec_pow_cuckoo.erl)
%%
patch_files(Config) ->
    ok = parse_trans_mod:transform_module(
           aec_pow_cuckoo,
           fun pow_cuckoo_xform/2,
           [{pt_pp_src, true}]),
    move_files_and_compile(["aec_pow_cuckoo.xfm"], Config).

move_files_and_compile(Files, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ok = filelib:ensure_dir(filename:join(PrivDir, "foo")),
    FromDir = filename:join(?config(top_dir, Config),
                            "_build/test/lib/aecore/src"),
    lists:foreach(
      fun(F) ->
              Base = filename:basename(F, ".xfm"),
              ErlF = Base ++ ".erl",
              Src = filename:join(FromDir, F),
              Tgt = filename:join(PrivDir, ErlF),
              ct:log("moving ~s~n -> ~s~n", [Src, Tgt]),
              ok = file:rename(Src, Tgt),
              cmd(["(cd ", PrivDir, " && erlc -W ", ErlF, ")"])
      end, Files).

pow_cuckoo_xform(Forms, _Options) ->
    parse_trans:plain_transform(fun pow_cuckoo_xform/1, Forms).

pow_cuckoo_xform({string,L,"aec_pow_cuckoo28_nif"}) ->
    {string,L,"aec_pow_cuckoo20_nif"};
pow_cuckoo_xform(_) ->
    continue.


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
            {ok,Bin} = file:read_file(code:which(?MODULE)),
            LoadRes =
                rpc_call(N, code, load_binary,
                         [?MODULE, "./" ++ ?MODULE_STRING ++ ".beam", Bin]),
            ct:log("LoadRes = ~p", [LoadRes]),
            spawn(N, ?MODULE, proxy, []),
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
    Flags = "-pa " ++ ?config(priv_dir, Config),
    Top = ?config(top_dir, Config),
    cmd(["(cd ", Top, " && ERL_FLAGS=\"", Flags, "\"",
         " EPOCH_CONFIG=", epoch_config(N, Config),
         " make ", start_tgt(N),")"]).

report_node_config(_N) ->
    [ct:log("~w env: ~p", [A, application:get_all_env(A)]) ||
        A <- [aeutil, aecore, aehttp]].

start_tgt(dev1) -> "dev1-start";
start_tgt(dev2) -> "dev2-start";
start_tgt(dev3) -> "dev3-start".

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
    await_gproc(N),
    case rpc:call(N, gproc, await,
                  [{n,l,{epoch,app,aehttp}},10000], 10500) of
        {P, _} when is_pid(P) -> ok;
        Other ->
            ct:log("gproc:await(<aehttp>) -> ~p", [Other])
    end.

await_gproc(N) ->
    retry(fun() -> case rpc:call(N, application, which_applications, []) of
                       {badrpc, _} = Error ->
                           error(Error);
                       Apps ->
                           lists:keymember(gproc, 1, Apps)
                   end
          end, {?LINE, await_gproc, N}).


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
    rpc_call(N, aec_miner, get_balance, []).

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
    end.

config(N, Config) ->
    {A,B,C} = os:timestamp(),
     #{<<"keys">> =>
           #{<<"dir">> => bin(keys_dir(N, Config)),
             <<"password">> => bin(io_lib:format("~w.~w.~w", [A,B,C]))},
       <<"logging">> =>
           #{<<"hwm">> => 500}
      }.

%% data_dir(Config) ->
%%     ?config(data_dir, Config).

priv_dir(Config) ->
    ?config(priv_dir, Config).

node_dir(N, Config) ->
    filename:join(priv_dir(Config), N).

keys_dir(N, Config) ->
    filename:join(node_dir(N, Config), "keys").

epoch_config(N, Config) ->
    filename:join(node_dir(N, Config), "epoch.json").

bin(S) ->
    iolist_to_binary(S).
