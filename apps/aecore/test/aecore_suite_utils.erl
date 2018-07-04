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
         mine_blocks/3,
         mine_blocks_until/3,
         mine_blocks_until/4,
         mine_key_blocks/2,
         mine_micro_blocks/2,
         wait_for_height/2,
         spend/4,         %% (Node, FromPub, ToPub, Amount) -> ok
         spend/5,         %% (Node, FromPub, ToPub, Amount, Fee) -> ok
         forks/0,
         latest_fork_height/0]).

-export([node_tuple/1,
         node_name/1,
         peer_info/1,
         connect/1,
         subscribe/2,
         unsubscribe/2,
         events_since/3,
         all_events_since/2,
         check_for_logs/2]).

-export([proxy/0,
         start_proxy/0,
         call_proxy/2,
         await_aehttp/1,
         await_sync_complete/2
        ]).

-export([sign_keys/0]).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

%% Keys for P2P communication
peer_keys() ->
    [{dev1, {<<120,30,108,92,13,32,45,162,66,181,135,13,102,186,226,7,134,64,127,57,44,122,62,198,148,18,128,51,162,218,180,97>>,
             <<128,38,224,217,226,249,89,153,69,120,34,192,93,224,163,234,105,76,186,215,58,166,69,75,31,103,31,243,148,225,253,127>>}},
     {dev2, {<<112,66,119,236,84,180,214,104,63,254,231,93,110,189,200,155,126,48,77,78,163,89,198,71,59,16,145,112,73,249,93,91>>,
             <<137,121,210,193,164,178,71,99,63,76,25,128,199,153,210,37,125,233,17,162,151,39,188,155,185,197,70,250,93,44,83,52>>}},
     {dev3, {<<192,145,22,50,217,175,73,12,42,218,16,92,216,240,151,252,189,80,190,47,62,203,178,89,230,75,253,78,114,65,96,78>>,
             <<177,115,250,203,226,39,102,92,8,182,166,254,125,117,140,134,199,149,211,182,184,107,119,43,218,70,251,60,10,56,12,53>>}}
    ].

%% Keys for signing / verification
sign_keys() ->
    [{dev1, {<<238,121,108,68,47,65,15,139,26,172,250,135,122,63,231,52,188,121,206,144,200,39,37,112,172,29,216,205,172,56,241,4,217,202,108,173,192,99,
               13,10,129,124,71,86,232,121,148,177,243,254,160,88,174,204,22,114,15,42,51,71,75,19,135,16>>,
             <<217,202,108,173,192,99,13,10,129,124,71,86,232,121,148,177,243,254,160,88,174,204,22,114,15,42,51,71,75,19,135,16>>}},
     {dev2, {<<133,191,59,166,119,215,123,78,192,54,29,91,247,72,123,72,245,85,161,97,70,225,58,34,166,141,6,63,193,79,58,65,40,25,191,50,209,111,19,239,
               98,126,125,211,15,133,93,12,13,125,167,137,94,138,27,55,23,50,106,33,28,222,180,102>>,
             <<40,25,191,50,209,111,19,239,98,126,125,211,15,133,93,12,13,125,167,137,94,138,27,55,23,50,106,33,28,222,180,102>>}},
     {dev3, {<<238,230,20,172,221,171,100,208,126,164,204,120,180,48,69,184,235,69,115,91,190,182,78,22,50,182,78,251,154,80,216,250,207,253,207,144,121,
               89,70,193,75,247,195,248,104,132,11,199,133,103,156,209,167,244,82,126,86,51,156,36,165,214,45,50>>,
             <<207,253,207,144,121,89,70,193,75,247,195,248,104,132,11,199,133,103,156,209,167,244,82,126,86,51,156,36,165,214,45,50>>}}].


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
    Config = config_apply_options(Node, MergedCfg1, Options),
    write_keys(Node, Config),
    write_config(EpochCfgPath, Config).

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

mine_key_blocks(Node, NumBlocksToMine) ->
    mine_blocks(Node, NumBlocksToMine, 100, key).

mine_micro_blocks(Node, NumBlocksToMine) ->
    mine_blocks(Node, NumBlocksToMine, 100, micro).

mine_blocks(Node, NumBlocksToMine) ->
    mine_blocks(Node, NumBlocksToMine, 100, any).

mine_blocks(Node, NumBlocksToMine, MiningRate) ->
    mine_blocks(Node, NumBlocksToMine, MiningRate, any).

mine_blocks(Node, NumBlocksToMine, MiningRate, Type) ->
    ok = rpc:call(
           Node, application, set_env, [aecore, expected_mine_rate, MiningRate],
           5000),
    aecore_suite_utils:subscribe(Node, block_created),
    aecore_suite_utils:subscribe(Node, micro_block_created),
    StartRes = rpc:call(Node, aec_conductor, start_mining, [], 5000),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [Node, StartRes]),
    Res = mine_blocks_loop(NumBlocksToMine, Type),
    StopRes = rpc:call(Node, aec_conductor, stop_mining, [], 5000),
    ct:log("aec_conductor:stop_mining() (~p) -> ~p", [Node, StopRes]),
    aecore_suite_utils:unsubscribe(Node, block_created),
    aecore_suite_utils:unsubscribe(Node, micro_block_created),
    case Res of
        {ok, BlocksReverse} ->
            {ok, lists:reverse(BlocksReverse)};
        {error, Reason} ->
            erlang:error(Reason)
    end.

mine_blocks_until(Node, ConditionFun, Max) ->
    mine_blocks_until(Node, ConditionFun, 100, Max).

mine_blocks_until(Node, ConditionFun, MiningRate, Max) ->
    ok = rpc:call(
           Node, application, set_env, [aecore, expected_mine_rate, MiningRate],
           5000),
    aecore_suite_utils:subscribe(Node, block_created),
    aecore_suite_utils:subscribe(Node, micro_block_created),
    StartRes = rpc:call(Node, aec_conductor, start_mining, [], 5000),
    ct:log("aec_conductor:start_mining() (~p) -> ~p", [Node, StartRes]),
    Res = mine_blocks_until_loop(ConditionFun, Max),
    StopRes = rpc:call(Node, aec_conductor, stop_mining, [], 5000),
    ct:log("aec_conductor:stop_mining() (~p) -> ~p", [Node, StopRes]),
    aecore_suite_utils:unsubscribe(Node, block_created),
    aecore_suite_utils:unsubscribe(Node, micro_block_created),
    case Res of
        {ok, BlocksReverse} ->
            {ok, lists:reverse(BlocksReverse)};
        {error, Reason} ->
            erlang:error(Reason)
    end.

mine_blocks_until_loop(ConditionFun, Max) ->
    mine_blocks_until_loop(ConditionFun, Max, []).

mine_blocks_until_loop(_ConditionFun, 0,_Acc) ->
    {error, max_reached};
mine_blocks_until_loop(ConditionFun, Max, Acc) ->
    case mine_blocks_loop(1, key) of
        {ok, Blocks} ->
            NewAcc = Blocks ++ Acc,
            case ConditionFun() of
                true  -> {ok, NewAcc};
                false ->
                    mine_blocks_until_loop(ConditionFun, Max - 1, NewAcc)
            end;
        {error, _} = Error -> Error
    end.

mine_blocks_loop(Cnt, Type) ->
    mine_blocks_loop([], Cnt, Type).

mine_blocks_loop(Blocks, 0,_Type) ->
    {ok, Blocks};
mine_blocks_loop(Blocks, BlocksToMine, Type) ->
    Block = wait_for_new_block(),
    case aec_blocks:type(Block) of
        micro when Type =:= key ->
            %% Don't decrement
            mine_blocks_loop([Block | Blocks], BlocksToMine, Type);
        key when Type =:= micro ->
            %% Don't decrement
            mine_blocks_loop([Block | Blocks], BlocksToMine, Type);
        _ ->
            mine_blocks_loop([Block | Blocks], BlocksToMine - 1, Type)
    end.

wait_for_new_block() ->
    receive
        {gproc_ps_event, block_created, Info} ->
            ct:log("key block created, Info=~p", [Info]),
            #{info := Block} = Info,
            Block;
        {gproc_ps_event, micro_block_created, Info} ->
            ct:log("micro block created, Info=~p", [Info]),
            #{info := Block} = Info,
            Block
    after 30000 ->
            ct:log("timeout waiting for block event~n"
                  "~p", [process_info(self(), messages)]),
            {error, timeout_waiting_for_block}
    end.

%% block the process until a certain height is reached
%% this has the expectation that the Node is mining
%% there is a timeout of 30 seconds for a single block to be produced
wait_for_height(Node, Height) ->
    aecore_suite_utils:subscribe(Node, block_created),
    aecore_suite_utils:subscribe(Node, micro_block_created),
    wait_for_height_(Node, Height),
    aecore_suite_utils:unsubscribe(Node, block_created),
    aecore_suite_utils:unsubscribe(Node, micro_block_created).

wait_for_height_(Node, Height) ->
    TopHeight =
        case rpc:call(Node, aec_chain, top_header, []) of
            undefined -> 0;
            Header -> aec_headers:height(Header)
        end,
    case TopHeight >= Height of
        true -> % reached height
            ok;
        false ->
            _ = wait_for_new_block(),
            wait_for_height_(Node, Height)
    end.

spend(Node, FromPub, ToPub, Amount) ->
    spend(Node, FromPub, ToPub, Amount, aec_governance:minimum_tx_fee()).

spend(Node, FromPub, ToPub, Amount, Fee) ->
    {ok, Nonce} = rpc:call(Node, aec_next_nonce, pick_for_account, [FromPub]),
    Params = #{sender => FromPub,
               recipient => ToPub,
               amount => Amount,
               fee => Fee,
               nonce => Nonce,
               payload => <<"foo">>},
    {ok, Tx} = rpc:call(Node, aec_spend_tx, new, [Params]),
    {ok, SignedTx} = rpc:call(Node, aec_keys, sign_tx, [Tx]),
    ok = rpc:call(Node, aec_tx_pool, push, [SignedTx]),
    {ok, SignedTx}.


forks() ->
    Vs = aec_governance:sorted_protocol_versions(),
    Hs = lists:seq(0, (length(Vs) - 1)),
    maps:from_list(lists:zip(Vs, Hs)).

latest_fork_height() ->
    lists:max(maps:values(forks())).

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
     || E <- [block_created, micro_block_created, chain_sync, app_started]].

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
    ["epoch.log", "epoch_mining.log", "epoch_sync.log",
     "epoch_pow_cuckoo.log", "epoch_metrics.log"].

await_sync_complete(T0, Nodes) ->
    [aecore_suite_utils:subscribe(N, chain_sync) || N <- Nodes],
    AllEvents = lists:flatten(
                  [aecore_suite_utils:events_since(N, chain_sync, T0) || N <- Nodes]
                 ),
    ct:log("AllEvents = ~p", [AllEvents]),
    SyncNodes =
        lists:foldl(
          fun(Msg, Acc) ->
                  check_event(Msg, Acc)
          end, Nodes, AllEvents),
    ct:log("SyncNodes = ~p", [SyncNodes]),
    collect_sync_events(SyncNodes, 100).

collect_sync_events(_, 0) -> error(retry_exhausted);
collect_sync_events([], _) -> done;
collect_sync_events(SyncNodes, N) ->
    receive
        {gproc_ps_event, chain_sync, Msg} ->
            SyncNodes1 = check_event(Msg, SyncNodes),
            collect_sync_events(SyncNodes1, N-1)
    after 20000 ->
            ct:log("Timeout in collect_sync_events: ~p~n"
                   "~p", [SyncNodes, process_info(self(), messages)]),
            error(timeout)
    end.

check_event(#{sender := From, info := Info}, Nodes) ->
    case Info of
        {chain_sync_done, _} ->
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.


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

report_node_config(N) ->
    [ct:log("~w env: ~p", [A, rpc:call(N, application, get_all_env, [A], 2000)]) ||
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
            after 30000 ->
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
    {ok, VerContents} = file:read_file(filename:join(Epoch, "VERSION")),
    [VerB |_ ] = binary:split(VerContents, [<<"\n">>], [global]),
    Version = binary_to_list(VerB),
    %%
    CfgD = filename:join([Top, "config/", N]),
    RelD = filename:dirname(filename:join([DDir, "releases", Version, "epoch.rel"])),
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
    Cfg1 = Cfg#{<<"blocked_peers">> => [peer_info(P) || P <- BlockedPeers]},
    config_apply_options(Node, Cfg1, T);
config_apply_options(Node, Cfg, [{add_peers, true}| T]) ->
    Cfg1 = Cfg#{<<"peers">> =>
              [peer_info(N1) || N1 <- [dev1, dev2, dev3] -- [Node]]},
    config_apply_options(Node, Cfg1, T).

write_keys(Node, Config) ->
    #{ <<"keys">> := #{ <<"dir">> := Path, <<"password">> := Pwd } } = Config,
    ok = filelib:ensure_dir(filename:join(Path, "foo")),
    ct:log("Writing peer and sign keys to ~p (~p)", [Path, filelib:is_dir(Path)]),
    {Node, {PeerPrivKey, PeerPubKey}} = lists:keyfind(Node, 1, peer_keys()),
    ok = file:write_file(filename:join(Path, "peer_key.pub"), aec_keys:encrypt_key(Pwd, PeerPubKey)),
    ok = file:write_file(filename:join(Path, "peer_key"), aec_keys:encrypt_key(Pwd, PeerPrivKey)),
    {Node, {SignPrivKey, SignPubKey}} = lists:keyfind(Node, 1, sign_keys()),
    ok = file:write_file(filename:join(Path, "sign_key.pub"), aec_keys:encrypt_key(Pwd, SignPubKey)),
    ok = file:write_file(filename:join(Path, "sign_key"), aec_keys:encrypt_key(Pwd, SignPrivKey)),
    ok.

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
    {N, {_PrivKey, PubKey}} = lists:keyfind(N, 1, sign_keys()),
    #{<<"keys">> =>
          #{<<"dir">> => iolist_to_binary(keys_dir(N, Config)),
            <<"password">> => iolist_to_binary(io_lib:format("~w.~w.~w", [A,B,C]))},
      <<"logging">> =>
          #{<<"hwm">> => 500},
      <<"mining">> =>
          #{<<"autostart">> => false,
            <<"beneficiary">> => aec_base58c:encode(account_pubkey, PubKey)},
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

peer_info(N) ->
    list_to_binary(["aenode://", aec_base58c:encode(peer_pubkey, pubkey(N)),
                  "@", hostname(), ":", integer_to_list(port_number(N))]).

port_number(dev1) -> 3015;
port_number(dev2) -> 3025;
port_number(dev3) -> 3035.

pubkey(N) ->
    {N, {_, PubKey}} = lists:keyfind(N, 1, peer_keys()),
    PubKey.

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

events() -> [block_created, micro_block_created, chain_sync].

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

