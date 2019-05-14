-module(aestratum_client_suite_utils).

-export([init_per_suite/2,
         init_per_suite/3,
         start_node/2,
         stop_node/2,
         connect/1
        ]).

-export([node_name/1,
         node_tuple/1
        ]).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

-define(OPS_BIN, "aestratum_client").
-define(CONFIG_FILE, "aestratum_client.json").

init_per_suite(NodesList, CTCfg) ->
    init_per_suite(NodesList, #{}, CTCfg).

init_per_suite(NodesList, CustomNodeCfg, CTCfg) ->
    DataDir = ?config(data_dir, CTCfg),
    TopDir = top_dir(DataDir),
    TopClientDir = top_client_dir(TopDir),
    CTCfg1 = [{top_dir, TopDir}, {top_client_dir, TopClientDir} | CTCfg],
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    create_configs(NodesList, CTCfg1, CustomNodeCfg),
    make_multi(CTCfg1, NodesList),
    CTCfg1.

start_node(N, Cfg) ->
    TopClientDir = ?config(top_client_dir, Cfg),
    Flags = ["-pa ", TopClientDir ++ "aestratum_client/_build/test/lib/aestratum_client"],
    aecore_suite_utils:cmd(?OPS_BIN, node_shortcut(N, Cfg), "bin", ["start"],
        [
         {"ERL_FLAGS", Flags},
         {"AESTRATUM_CLIENT_CONFIG", "data/" ++ ?CONFIG_FILE},
         {"RUNNER_LOG_DIR","log"},
         {"CODE_LOADING_MODE", "interactive"}
        ]).

stop_node(N, Cfg) ->
    aecore_suite_utils:cmd(?OPS_BIN, node_shortcut(N, Cfg), "bin", ["stop"]).

connect(N) ->
    connect(N, 100),
    report_node_config(N).

node_name(N) ->
    [_,H] = re:split(atom_to_list(node()), "@", [{return,list}]),
    list_to_atom(atom_to_list(N) ++ "@" ++ H).

node_tuple(N) ->
    {N, node_name(N)}.

create_configs(NodesList, CTCfg, CustomCfg) ->
    {top_dir, Top} = lists:keyfind(top_dir, 1, CTCfg),
    CfgSchema = filename:join([top_client_dir(Top), "priv", "config_schema.json"]),
    [create_config(N, CfgSchema, CTCfg, CustomCfg) || N <- NodesList].

create_config(Node, CfgSchema, CTCfg, CustomCfg) ->
    ClientCfgPath = client_config_path(Node, CTCfg),
    ok = filelib:ensure_dir(ClientCfgPath),
    MergedCfg = maps_merge(default_config(Node, CTCfg), CustomCfg),
    write_config(ClientCfgPath, CfgSchema, MergedCfg).

default_config(N, Cfg) ->
    #{<<"connection">> =>
        #{<<"transport">> => <<"tcp">>,
          <<"host">> => <<"localhost">>,
          <<"port">> => 9999,
          <<"req_timeout">> => 15,
          <<"req_retries">> => 3},
      <<"user">> =>
        #{<<"account">> => <<"ak_DummyPubKeyDoNotEverUse999999999999999999999999999">>,
          <<"worker">> => <<"worker1">>},
      <<"miners">> =>
        [#{<<"exec">> => <<"mean29-generic">>,
           <<"exec_group">> => <<"aecuckoo">>,
           <<"extra_args">> => <<"">>,
           <<"hex_enc_hdr">> => false,
           <<"repeats">> => 100,
           <<"edge_bits">> => 29}]
     }.

write_config(F, CfgSchema, Cfg) ->
    JSON = jsx:prettify(jsx:encode(Cfg)),
    {ok, Fd} = file:open(F, [write]),
    ct:log("Writing config (~p)~n~s", [F, JSON]),
    try io:fwrite(Fd, "~s~n", [JSON])
    after
        file:close(Fd)
    end,
    VRes = aeu_env:check_config(F, CfgSchema),
    ct:log("Cfg (~p) check: ~p", [F, VRes]),
    {ok,_} = VRes.

make_multi(Cfg, NodesList) ->
    make_multi(Cfg, NodesList, "test").

make_multi(Cfg, NodesList, RefRebarProfile) ->
    ct:log("RefRebarProfile = ~p", [RefRebarProfile]),
    TopClientDir = ?config(top_client_dir, Cfg),
    ct:log("Top client dir = ~p", [TopClientDir]),
    Client =
        filename:join(TopClientDir,
                      "_build/" ++ RefRebarProfile ++ "/rel/aestratum_client"),
    [setup_node(N, TopClientDir, Client, Cfg) || N <- NodesList].

setup_node(N, Top, Client, Cfg) ->
    ct:log("setup_node(~p,Cfg)", [N]),
    DDir = node_shortcut(N, Cfg),
    filelib:ensure_dir(filename:join(DDir, "foo")),
    cp_dir(filename:join(Client, "releases"), DDir ++ "/"),
    cp_dir(filename:join(Client, "bin"), DDir ++ "/"),
    symlink(filename:join(Client, "lib"), filename:join(DDir, "lib")),
    {ok, VerContents} = file:read_file(filename:join(Client, "VERSION")),
    [VerB |_ ] = binary:split(VerContents, [<<"\n">>, <<"\r">>], [global]),
    Version = binary_to_list(VerB),
    %%
    CfgD = filename:join([Top, "config/", "test"]),
    PrivD = filename:join([Top, "priv"]),
    RelD = filename:dirname(filename:join([DDir, "releases", Version, "aestratum_client.rel"])),
    cp_file(filename:join(CfgD, "sys.config"),
            filename:join(RelD, "sys.config")),
    cp_file(filename:join(CfgD, "vm.args"),
            filename:join(RelD, "vm.args")),
    delete_file(filename:join(RelD, "vm.args")), %% cookie and node name
    make_vm_args_file(filename:join(RelD, "vm.args"), N),
    delete_file(filename:join(RelD, "vm.args.orig")),
    delete_file(filename:join(RelD, "sys.config.orig")).

connect(N, Timeout) when Timeout < 10000 ->
    timer:sleep(Timeout),
    case net_kernel:hidden_connect(N) of
        true ->
            ct:log("hidden_connect(~p) -> true", [N]),
            await_status(N, 5),
            true;
        false ->
            ct:log("hidden_connect(~p) -> false, retrying ...", [N]),
            connect(N, Timeout * 2)
    end;
connect(N, _) ->
    ct:log("exhausted retries (~p)", [N]),
    erlang:error({could_not_connect, N}).

report_node_config(N) ->
    [ct:log("~w env: ~p", [A, rpc:call(N, application, get_all_env, [A], 2000)]) ||
        A <- [aestratum_client]].

await_status(N, Retries) when Retries > 0 ->
    case client_status(N) of
        S when is_map(S) ->
            ok;
        _Other ->
            timer:sleep(1000),
            await_status(N, Retries - 1)
    end;
await_status(_N, 0) ->
    error(timeout_waiting_for_status).

client_status(N) ->
    rpc:call(N, aestratum_client, status, []).

client_config_path(N, Cfg) ->
    filename:join(data_dir(N, Cfg), ?CONFIG_FILE).

top_dir(DataDir) ->
    %% Split the DataDir path at "_build"
    [Top, _] = re:split(DataDir, "_build", [{return, list}]),
    Top.

top_client_dir(Top) ->
    Top ++ "_build/test/aestratum_client/".

make_vm_args_file(F, N) ->
    {ok, S} = file:open(F, [write]),
    io:format(S, "-sname ~p~n-setcookie aeternity_cookie~n",
              [atom_to_list(node_name(N))]),
    file:close(S).

node_shortcut(N, Cfg) ->
    filename:join(shortcut_dir(Cfg), N).

shortcut_dir(Cfg) ->
    Top = ?config(top_dir, Cfg),
    SymlinkName = ?config(symlink_name, Cfg),
    filename:join([Top, "_build/test/logs", SymlinkName]).

priv_dir(Cfg) ->
    SubDir = atom_to_list(?config(test_module, Cfg)),
    filename:join(?config(priv_dir, Cfg), SubDir).

data_dir(N, Cfg) ->
    filename:join(node_shortcut(N, Cfg), "data").

log_dir(N, Cfg) ->
    filename:join(node_shortcut(N, Cfg), "log").

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

cp_file(From, To) ->
    case file:copy(From, To) of
        {ok, _} ->
            ct:log("Copied ~s to ~s", [From, To]);
        Err ->
            ct:fail("Error copying ~s to ~s: ~p", [From, To, Err])
    end,
    ok.

symlink(From, To) ->
    ok = file:make_symlink(From, To),
    ct:log("symlinked ~s to ~s", [From, To]),
    ok.

match_mode(A, B) ->
    case {file:read_link_info(A), file:read_file_info(B)} of
        {{ok, #file_info{mode = M}}, {ok, FI}} ->
            file:write_file_info(B, FI#file_info{mode = M});
        Other ->
            ct:log("Error matching mode ~p -> ~p: ~p", [A, B, Other]),
            {error, {match_mode, {A, B}, Other}}
    end.

delete_file(F) ->
    case file:delete(F) of
        ok -> ok;
        {error, enoent} -> ok;
        Other ->
            erlang:error(Other, [F])
    end.

maps_merge(V1, V2) when not is_map(V1); not is_map(V2) ->
    V2;
maps_merge(Map1, Map2) ->
    lists:foldl(fun({K, V}, Map) ->
                    case maps:is_key(K, Map) of
                        false -> Map#{K => V};
                        true  -> Map#{K => maps_merge(V, maps:get(K, Map))}
                    end
                end, Map2, maps:to_list(Map1)).
