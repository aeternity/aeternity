-module(aestratum_client_suite_utils).

-export([init_per_suite/2,
         init_per_suite/3
        ]).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

init_per_suite(NodesList, CTConfig) ->
    init_per_suite(NodesList, #{}, CTConfig).

init_per_suite(NodesList, CustomNodeCfg, CTConfig) ->
    DataDir = ?config(data_dir, CTConfig),
    TopDir = top_dir(DataDir),
    CTConfig1 = [{top_dir, TopDir} | CTConfig],
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    create_configs(NodesList, CTConfig1, CustomNodeCfg),
    %make_multi(CTConfig1, NodesList),
    CTConfig1.

create_configs(NodesList, CTConfig, CustomConfig) ->
    [create_config(N, CTConfig, CustomConfig) || N <- NodesList].

create_config(Node, CTConfig, CustomConfig) ->
    ClientCfgPath = client_config_dir(Node, CTConfig),
    ok = filelib:ensure_dir(ClientCfgPath),
    MergedCfg = maps_merge(default_config(Node, CTConfig), CustomConfig),
    write_config(ClientCfgPath, MergedCfg).

default_config(N, Config) ->
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

client_config_dir(N, Config) ->
    filename:join(data_dir(N, Config), "aestratum_client.json").

top_dir(DataDir) ->
    %% Split the DataDir path at "_build"
    [Top, _] = re:split(DataDir, "_build", [{return, list}]),
    Top.

top_client_dir(Top) ->
    Top ++ "_build/test/aestratum_client/".

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

