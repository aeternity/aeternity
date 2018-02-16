-module(aecore_suite_utils).

-export([top_dir/1,
         epoch_config/2,
         create_configs/1,
         create_configs/2,
         create_configs/3,
         create_config/4,
         make_multi/1,
         make_multi/2,
         make_shortcut/1,
         shortcut_dir/1]).

-export([cmd/1,
         cmd_res/1]).

-export([start_node/2,
         stop_node/2,
         get_node_db_config/1,
         delete_node_db_if_persisted/1,
         mine_blocks/2,
         mine_blocks/3]).

-export([node_tuple/1,
         node_name/1,
         connect/1,
         subscribe/2,
         unsubscribe/2,
         events_since/3,
         all_events_since/2,
         check_for_logs/2]).

-export([proxy/0,
         start_proxy/0,
         call_proxy/2,
         await_aehttp/1
         ]).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

epoch_config(Node, CTConfig) ->
    EpochConfig = epoch_config_dir(Node, CTConfig),
    [OrigCfg] = jsx:consult(EpochConfig, [return_maps]),
    backup_config(EpochConfig),
    OrigCfg.

create_configs(CTConfig) ->
    create_configs(CTConfig, #{}, []).

create_configs(CTConfig, CustomConfig) ->
    create_configs(CTConfig, CustomConfig, []).

create_configs(CTConfig, CustomConfig, Options) ->
    [create_config(N, CTConfig, CustomConfig, Options) || N <- [dev1, dev2, dev3]].

create_config(Node, CTConfig, CustomConfig, Options) ->
    EpochCfgPath = epoch_config_dir(Node, CTConfig),
    ok = filelib:ensure_dir(EpochCfgPath),
    MergedCfg = maps:merge(default_config(Node, CTConfig), CustomConfig),
    MergedCfg1 = aec_metrics_test_utils:set_statsd_port_config(
                   Node, MergedCfg, CTConfig),
    write_config(EpochCfgPath, config_apply_options(Node, MergedCfg1, Options)).


make_multi(Config) ->
    make_multi(Config, [dev1, dev2, dev3]).

make_multi(Config, NodesList) ->
    make_multi(Config, NodesList, "test").

make_multi(Config, NodesList, RefRebarProfile) ->
    ct:log("RefRebarProfile = ~p", [RefRebarProfile]),
    Top = ?config(top_dir, Config),
    ct:log("Top = ~p", [Top]),
    Epoch = filename:join(Top, "_build/" ++ RefRebarProfile ++ "/rel/epoch"),
    [setup_node(N, Top, Epoch, Config) || N <- NodesList].

make_shortcut(Config) ->
    PrivDir  = priv_dir(Config),
    ok = filelib:ensure_dir(filename:join(PrivDir, "foo")),
    Shortcut = shortcut_dir(Config),
    delete_file(Shortcut),
    ok = file:make_symlink(PrivDir, Shortcut),
    ct:log("Made symlink ~s to ~s", [PrivDir, Shortcut]),
    ok.


start_node(N, Config) ->
    %TestModule = ?config(test_module, Config),
    MyDir = filename:dirname(code:which(?MODULE)),
    ConfigFilename = proplists:get_value(config_name, Config, "default"), 
    Flags = ["-pa ", MyDir, " -config ./" ++ ConfigFilename],
    cmd(["(cd ", node_shortcut(N, Config),
         " && ERL_FLAGS=\"", Flags, "\"",
         " EPOCH_CONFIG=./data/epoch.json"
         " RUNNER_LOG_DIR=`pwd`/log"
         " CODE_LOADING_MODE=interactive"
         " ./bin/epoch start)"]).

stop_node(N, Config) ->
    cmd(["(cd ", node_shortcut(N, Config),
         " && ./bin/epoch stop)"]).

get_node_db_config(Rpc) when is_function(Rpc, 3) ->
    IsDbPersisted = Rpc(application, get_env, [aecore, persist, false]),
    MaybeMnesiaDir =
        case Rpc(application, get_env, [mnesia, dir]) of
            undefined -> undefined;
            {ok, MnesiaDir0} ->
                {ok, Rpc(filename, absname, [MnesiaDir0])}
        end,
    ct:log("Is DB persisted? ~p. What is Mnesia dir if any? ~p",
           [IsDbPersisted, MaybeMnesiaDir]),
    {ok, {IsDbPersisted, MaybeMnesiaDir}}.

delete_node_db_if_persisted({false, undefined}) ->
    ok;
delete_node_db_if_persisted({true, {ok, MnesiaDir}}) ->
    ct:log("Deleting Mnesia Dir ~p", [MnesiaDir]),
    {true, _} = {filelib:is_file(MnesiaDir), MnesiaDir},
    {true, _} = {filelib:is_dir(MnesiaDir), MnesiaDir},
    RmMnesiaDir = "rm -r '" ++ MnesiaDir ++ "'",
    ct:log("Running command ~p", [RmMnesiaDir]),
    os:cmd(RmMnesiaDir),
    {false, _} = {filelib:is_file(MnesiaDir), MnesiaDir},
    ok.

mine_blocks(Node, NumBlocksToMine) ->
    mine_blocks(Node, NumBlocksToMine, 10).

mine_blocks(Node, NumBlocksToMine, MiningRate) ->
    ok = rpc:call(
           Node, application, set_env, [aecore, expected_mine_rate, MiningRate],
           5000),
    aecore_suite_utils:subscribe(Node, block_created),
    StartRes = rpc:call(Node, aec_conductor, start_mining, [], 5000),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [Node, StartRes]),
    Res = mine_blocks_loop(NumBlocksToMine),
    StopRes = rpc:call(Node, aec_conductor, stop_mining, [], 5000),
    ct:log("aec_conductor:stop_mining() (~p) -> ~p", [Node, StopRes]),
    aecore_suite_utils:unsubscribe(Node, block_created),
    case Res of
        {ok, _BlocksReverse} = OK ->
            OK;
        {error, Reason} ->
            erlang:error(Reason)
    end.

mine_blocks_loop(Cnt) ->
    mine_blocks_loop([], Cnt).

mine_blocks_loop(Blocks, 0) ->
    {ok, Blocks};
mine_blocks_loop(Blocks, BlocksToMine) ->
    receive
        {gproc_ps_event, block_created, Info} ->
            ct:log("block created, Info=~p", [Info]),
            #{info := Block} = Info,
            mine_blocks_loop([Block | Blocks], BlocksToMine - 1)
    after 30000 ->
            ct:log("timeout waiting for block event~n"
                  "~p", [process_info(self(), messages)]),
            {error, timeout_waiting_for_block}
    end.

top_dir(DataDir) ->
    %% Split the DataDir path at "_build"
    [Top, _] = re:split(DataDir, "_build", [{return, list}]),
    Top.

node_tuple(N) when N == dev1; N == dev2; N == dev3 ->
    {N, node_name(N)}.

node_name(N) when N == dev1; N == dev2; N == dev3 ->
    [_,H] = re:split(atom_to_list(node()), "@", [{return,list}]),
    list_to_atom("epoch_" ++ atom_to_list(N) ++ "@" ++ H).

connect(N) ->
    connect(N, 5),
    report_node_config(N).

subscribe(N, Event) ->
    call_proxy(N, {subscribe, Event}).

unsubscribe(N, Event) ->
    call_proxy(N, {unsubscribe, Event}).

all_events_since(N, TS) ->
    [{E, try events_since(N, E, TS) catch error:Err -> Err end}
     || E <- [block_created, chain_sync, app_started]].

events_since(N, EvType, TS) ->
    call_proxy(N, {events, EvType, TS}).

check_for_logs(Nodes, Config) ->
    [] = [{N, Fs} || {N, Fs} <- [{N1, check_for_missing_logs(N1, Config)}
                                 || N1 <- Nodes],
                     Fs =/= []],
    ok.

check_for_missing_logs(N, Config) ->
    LogDir = log_dir(N, Config),
    [{missing, F}
     || F <- expected_logs(),
        file_missing(filename:join(LogDir, F))].

file_missing(F) ->
    case file:read_link_info(F) of
        {ok, _} ->
            false;
        _ ->
            true
    end.

expected_logs() ->
    ["epoch.log", "epoch_mining.log",
     "epoch_pow_cuckoo.log", "epoch_metrics.log"].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

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



setup_node(N, Top, Epoch, Config) ->
    ct:log("setup_node(~p,Config)", [N]),
    DDir = node_shortcut(N, Config),
    filelib:ensure_dir(filename:join(DDir, "foo")),
    cp_dir(filename:join(Epoch, "releases"), DDir ++ "/"),
    cp_dir(filename:join(Epoch, "bin"), DDir ++ "/"),
    symlink(filename:join(Epoch, "lib"), filename:join(DDir, "lib")),
    symlink(filename:join(Epoch, "patches"), filename:join(DDir, "patches")),
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
    TestsDir = filename:dirname(code:which(?MODULE)), 
    TestD = filename:join(TestsDir, "data"),
    ConfigFilename = proplists:get_value(config_name, Config, "default") ++ ".config", 
    cp_file(filename:join(TestD, ConfigFilename),
            filename:join(DDir , ConfigFilename)),
    aec_test_utils:copy_genesis_dir(Epoch, DDir).


cp_dir(From, To) ->
    ToDir = case lists:last(To) of
		$/ ->
		    filename:join(To, filename:basename(From));
		_ ->
		    To
	    end,
    ok = filelib:ensure_dir(filename:join(ToDir, "foo")),
    cp_dir(file:list_dir(From), From, ToDir).

cp_dir({ok, Fs}, From, To) ->
    Res =
	lists:foldl(
	  fun(F, Acc) ->
		  FullF = filename:join(From, F),
		  case filelib:is_dir(FullF) of
		      true ->
			  To1 = filename:join(To, F),
			  cp_dir(FullF, To1),
			  [FullF|Acc];
		      false ->
			  Tgt = filename:join(To, F),
			  ok = filelib:ensure_dir(Tgt),
			  {ok,_} = file:copy(FullF, Tgt),
			  ok = match_mode(FullF, Tgt),
			  [FullF|Acc]
		  end
	  end, [], Fs),
    ct:log("cp_dir(~p, ~p) -> ~p", [From, To, Res]),
    ok;
cp_dir({error, _} = Error, From, To) ->
    ct:log("cp_dir(~p, ~p) -> ~p", [From, To, Error]),
    Error.

match_mode(A, B) ->
    case {file:read_link_info(A), file:read_file_info(B)} of
	{{ok, #file_info{mode = M}}, {ok, FI}} ->
	    file:write_file_info(B, FI#file_info{mode = M});
	Other ->
	    ct:log("Error matching mode ~p -> ~p: ~p", [A, B, Other]),
	    {error, {match_mode, {A, B}, Other}}
    end.

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


config_apply_options(_Node, Cfg, []) ->
    Cfg;
config_apply_options(Node, Cfg, [{block_peers, BlockedPeers}| T]) ->
    Cfg1 = Cfg#{<<"blocked_peers">> => [peer_uri(P) || P <- BlockedPeers]},
    config_apply_options(Node, Cfg1, T);
config_apply_options(Node, Cfg, [{add_peers, true}| T]) ->
    Cfg1 = Cfg#{<<"peers">> =>
              [peer_uri(N1) || N1 <- [dev1, dev2, dev3] -- [Node]]},
    config_apply_options(Node, Cfg1, T).

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

default_config(N, Config) ->
    {A,B,C} = os:timestamp(),
     #{<<"keys">> =>
           #{<<"dir">> => iolist_to_binary(keys_dir(N, Config)),
             <<"password">> => iolist_to_binary(io_lib:format("~w.~w.~w", [A,B,C]))},
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

epoch_config_dir(N, Config) ->
    filename:join(data_dir(N, Config), "epoch.json").

%% dirs 
node_shortcut(N, Config) ->
    filename:join(shortcut_dir(Config), N).

shortcut_dir(Config) ->
    Top = ?config(top_dir, Config),
    SymlinkName = ?config(symlink_name, Config),
    filename:join([Top, "_build/test/logs", SymlinkName]).

priv_dir(Config) ->
    SubDir = atom_to_list(?config(test_module, Config)),
    filename:join(?config(priv_dir, Config), SubDir).

data_dir(N, Config) ->
    filename:join(node_shortcut(N, Config), "data").

log_dir(N, Config) ->
    filename:join(node_shortcut(N, Config), "log").

keys_dir(N, Config) ->
    filename:join(data_dir(N, Config), "keys").

delete_file(F) ->
    case file:delete(F) of
        ok -> ok;
        {error, enoent} -> ok;
        Other ->
            erlang:error(Other, [F])
    end.

hostname() ->
    {ok, H} = inet:gethostname(),
    H.

peer_uri(N) ->
    iolist_to_binary(
      ["http://", hostname(), ":", integer_to_list(port_number(N)), "/"]).

port_number(dev1) -> 3013;
port_number(dev2) -> 3023;
port_number(dev3) -> 3033.


backup_config(EpochConfig) ->
    Dir = filename:dirname(EpochConfig),
    Ext = filename:extension(EpochConfig),
    {A,B,C} = os:timestamp(),
    BackupBase = lists:flatten(
                   ["epoch-",
                    integer_to_list(A),
                    "-",
                    integer_to_list(B),
                    "-",
                    integer_to_list(C),
                    Ext]),
    Backup = filename:join(Dir, BackupBase),
    ct:log("Back up ~p to ~p", [EpochConfig, Backup]),
    cp_file(EpochConfig, Backup).


%% ============================================================
%% Proxy process
%% ============================================================

-define(PROXY, epoch_multi_node_test_proxy).
-define(PROXY_CALL_RETRIES, 5).

proxy() ->
    register(?PROXY, self()),
    process_flag(trap_exit, true),
    aec_test_event_handler:install(),
    error_logger:info_msg("starting test suite proxy~n", []),
    proxy_loop([{marker, app_started}], dict:new()).

start_proxy() ->
    io:fwrite("starting proxy...~n", []),
    proc_lib:spawn(?MODULE, proxy, []).


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
        {gproc_ps_event, Event, Info0} ->
            Info = Info0#{test_node => node()}, % for easier debugging
            tell_subscribers(Subs, Event, {gproc_ps_event, Event, Info}),
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

events() -> [block_created, chain_sync].

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

