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
    restart_third/1
   ]).

-export([proxy/0]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, two_nodes},
     {group, three_nodes}
    ].

groups() ->
    [
     {two_nodes, [],
      [start_first_node,
       mine_on_first,
       start_second_node,
       mine_again_on_first,
       mine_on_second,
       restart_second,
       restart_first]},
     {three_nodes, [],
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
    ok = expect_block(N2, B1).

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
    ok = expect_same(Config).


start_third_node(Config) ->
    N3 = node_(dev3),
    start_node_(dev3, Config),
    connect(N3),
    timer:sleep(2000),
    ct:log("Peers on dev3: ~p", [rpc_call(N3, aec_peers, all, [])]),
    expect_same(Config).

mine_on_third(Config) ->
    mine_and_compare(node_(dev3), Config).

mine_and_compare(N1, Config) ->
    AllNodes = [N || {_, N} <- ?config(nodes, Config)],
    {ok, PrevTop} = rpc_call(N1, aec_chain, top, []),
    ok = mine_one_block(N1),
    {ok, NewTop} = rpc_call(N1, aec_chain, top, []),
    true = (NewTop =/= PrevTop),
    lists:foreach(
      fun(Nx) ->
              ok = expect_block(Nx, NewTop)
      end, AllNodes -- [N1]).

expect_same(Config) ->
    Nodes = ?config(nodes, Config),
    expect_same(Nodes, 5).

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


mine_one_block(N) ->
    subscribe(N, block_created),
    ok = rpc_call(N, aec_miner, resume, []),
    receive
        {gproc_ps_event, block_created, Height} ->
            ct:log("block created, height=~p", [Height]),
            ok
    after 20000 ->
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
                    Res = (catch aec_sync:subscribe(Event)),
                    From ! {Ref, Res},
                    proxy_loop([{From, Event}|Subs])
            end;
        {From, Ref, {unsubscribe, Event}} ->
            From ! {Ref, ok},
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
%% - turn off continuous mining in aec_miner.erl
%%
patch_files(Config) ->
    ok = parse_trans_mod:transform_module(
           aec_miner,
           fun miner_xform/2,
           [{pt_pp_src, true}]),
    ok = parse_trans_mod:transform_module(
           aec_pow_cuckoo,
           fun pow_cuckoo_xform/2,
           [{pt_pp_src, true}]),
    move_files_and_compile(["aec_miner.xfm",
                            "aec_pow_cuckoo.xfm"], Config).

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


miner_xform(Forms, _Opts) ->
    parse_trans:plain_transform(fun miner_xform/1, Forms).

miner_xform({function,L,init,1,_}) ->
    L1 = L+1, L2 = L+2,
    {function,L,init,1,
     [{clause,L1,
       [{var,L1,'_'}],
       [],
       [{tuple,L2,
         [{atom,L2,ok},
          {atom,L2,idle},
          {record,L2,state,[]}]}]}]};
miner_xform({function,L,running,3,Clauses}) ->
    Clauses1 =
        lists:map(
          fun({clause,_,[{atom,_,cast},{atom,_,mine},_],_,_} = C) ->
                  [C1] =
                      parse_trans:plain_transform(fun miner_xform_1/1, [C]),
                  C1;
             (Clause) ->
                  Clause
          end, Clauses),
    {function,L,running,3,Clauses1};
miner_xform(_) ->
    continue.

%% Remove all calls to gen_statem:cast(aec_miner, mine) from the
%% 'running' state - i.e. don't automatically re-mine
miner_xform_1({call,L,
               {remote,_,{atom,_,gen_statem},{atom,_,cast}},
               [{atom,_,aec_miner},{atom,_,mine}]}) ->
    {atom,L,ok};
miner_xform_1({clause,_,[{tuple,_,[{atom,_,error},
                                   {atom,_,generation_count_exhausted}]}],
               _,_} = C) ->
    %% don't change next state to idle!
    C;
miner_xform_1({tuple,L,[{atom,_,next_state},{atom,_,running},{var,_,S}]}) ->
    {tuple,L,[{atom,L,next_state},{atom,L,idle},{var,L,S}]};
miner_xform_1(_) ->
    continue.

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
    connect(N, 5).

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
            ct:log(
              "await: ~p",
              [rpc:call(N, gproc, await,
                        [{n,l,{epoch,app,aehttp}},10000], 10500)]),
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
         " make ", start_tgt(N),")"]).

start_tgt(dev1) -> "dev1-start";
start_tgt(dev2) -> "dev2-start";
start_tgt(dev3) -> "dev3-start".

expect_block(N, B) ->
    expect_block(N, B, undefined, 5).

expect_block(N, B, _Prev, Tries) when Tries > 0 ->
    {ok, Bn} = rpc_call(N, aec_chain, top, []),
    case B == Bn of
        true ->
            ok;
        false ->
            timer:sleep(1000),
            expect_block(N, B, Bn, Tries-1)
    end;
expect_block(N, B, Prev, _) ->
    ct:log("exhausted retries (~p)", [N]),
    erlang:error({different_top_blocks, [N, B, Prev]}).
