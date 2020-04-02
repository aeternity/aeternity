-module(aest_docker).

%=== EXPORTS ===================================================================

%% API exports
-export([start/1]).
-export([stop/1]).
-export([prepare_spec/2]).
-export([peer_from_spec/2]).
-export([setup_node/2]).
-export([delete_node/1]).
-export([start_node/1]).
-export([stop_node/2]).
-export([kill_node/1]).
-export([stop_container/2]).
-export([node_logs/1]).
-export([get_peer_address/1]).
-export([get_service_address/2]).
-export([get_internal_address/2]).
-export([get_node_pubkey/1]).
-export([extract_archive/3]).
-export([run_cmd_in_node_dir/3]).
-export([connect_node/2]).
-export([disconnect_node/2]).
-export([get_log_path/1]).
-export([export/2]).

%=== MACROS ====================================================================

-define(CONFIG_FILE_TEMPLATE, "aeternity.yaml.mustache").
-define(AETERNITY_OPS_BIN, "/home/aeternity/node/bin/aeternity").
-define(AETERNITY_CONFIG_FILE, "/home/aeternity/aeternity.yaml").
-define(EPOCH_LOG_FOLDER, "/home/aeternity/node/log").
-define(EPOCH_KEYS_FOLDER, "/home/aeternity/node/keys").
-define(EPOCH_GENESIS_FILE, "/home/aeternity/node/data/aecore/.genesis/accounts_test.json").
-define(EPOCH_MINE_RATE, 1000).
-define(EPOCH_MAX_INBOUND, 100).
-define(EXT_HTTP_PORT, 3013).
-define(EXT_SYNC_PORT, 3015).
-define(INT_HTTP_PORT, 3113).
-define(EXT_WS_PORT, 3014).
-define(EPOCH_STOP_TIMEOUT, 30000).
-define(PEER_KEYS_PASSWORD, <<"top secret">>).
-define(DEFAULT_NETWORKS, [epoch]).

%=== TYPES =====================================================================

-type log_fun() :: fun((io:format(), list()) -> ok) | undefined.
-type test_uid() :: binary() | undefined.
-type service_label() :: sync | ext_http | int_http | ext_ws.

%% State of the docker backend
-type backend_state() :: #{
    postfix := binary(),        % A unique postfix to add to container and networks names.
    log_fun := log_fun(),       % Function to use for logging.
    data_dir := binary(),       % The directory where the templates can be found.
    temp_dir := binary()        % A temporary directory that can be used to generate
                                % configuration files and save the log files.
}.

%% Node specification
-type node_spec() :: #{
    name := atom(),
    pubkey => binary(),         % Public part of the peer key
    privkey => binary(),        % Private part of the peer key
    peers := [binary()],        % URLs of the peer nodes
    source := {pull, binary()}, % Source of the node image
    ulimits := [{Name :: atom(), Soft :: integer(), Hard :: integer()}],
    mine_rate => default | pos_integer(),
    cuckoo_miner => default | #{ex := binary(),
                                args := binary(),
                                bits := pos_integer()},
    hard_forks => #{non_neg_integer() => non_neg_integer()}, % Consensus protocols (version -> height)
    config_guest_path => nonempty_string(),
    config => #{atom() => term()},
    % Tuple of host/guest paths where the node DB is meant to be if persisted
    db_path => {binary(), binary()} | undefined,
    % proplist of encoded addresses and balances
    genesis_accounts => [{binary(), non_neg_integer()}],
    % the path to the accounts_test.json to be used for genesis block
    genesis => file:filename_all(),
    entrypoint => [string(), ...],
    % Mind that using custom_command cancels usage of mine_rate (default mine_rate is used).
    % Technically mine_rate is passed as a command to Docker, which is overwritten by custom_command (if used).
    custom_command => [string(), ...],
    mining => #{autostart => boolean(),
                strictly_follow_top => boolean()}
}.

%% State of a node
-type node_state() :: #{
    postfix := binary(),        % A unique postfix to add to container and networks names.
    log_fun := log_fun(),       % Function to use for logging
    hostname := atom(),         % Hostname of the container running the node
    pubkey := binary(),         % Public part of the peer key
    privkey := binary(),        % Private part of the peer key
    exposed_ports := #{service_label() => pos_integer()},
    local_ports := #{service_label() => pos_integer()},
    sockets := [gen_tcp:socket()], % Reserved socket to prevent port clash
    container_id := term(),
    config_path := term(),      % Host path of user config
    config := term(),           % Content of user config
    % Tuple of host/guest paths where the node logs are
    log_path := {binary(), binary()}
}.

-type start_options() :: #{
    test_id => test_uid(),
    log_fun => log_fun(),
    data_dir := binary(),
    temp_dir := binary()
}.

-type stop_node_options() :: #{
    soft_timeout => pos_integer() | infinity,
    hard_timeout => pos_integer()
}.


%=== GENERIC API FUNCTIONS =====================================================

-spec start(start_options()) -> backend_state().
start(Options) ->
    TestId = maps:get(test_id, Options),
    Postfix = uid2postfix(TestId),
    LogFun = maps:get(log_fun, Options),
    {ok, DataDir} = maps:find(data_dir, Options),
    {ok, TempDir} = maps:find(temp_dir, Options),
    ok = aest_docker_api:start(),
    #{postfix => Postfix,
      log_fun => LogFun,
      data_dir => DataDir,
      temp_dir => TempDir
    }.

-spec stop(backend_state()) -> ok.
stop(BackendState) ->
    aest_docker_api:prune_networks(),
    log(BackendState, "Networks pruned", []),
    ok.

-spec prepare_spec(node_spec(), backend_state()) -> node_spec().
prepare_spec(#{pubkey := PubKey, privkey := PrivKey} = Spec, BackendState) ->
    #{data_dir := DataDir} = BackendState,
    #{name := Name} = Spec,
    KeysDir = keys_dir(DataDir, Name),
    Password = ?PEER_KEYS_PASSWORD,
    {PubFile, PrivFile} = aec_keys:peer_key_filenames(KeysDir),
    file:delete(PubFile),
    file:delete(PrivFile),
    aec_keys:save_peer_keys(Password, KeysDir, PubKey, PrivKey),
    Spec;
prepare_spec(#{pubkey := _PubKey}, _BackendState) ->
    error(pubkey_without_privkey);
prepare_spec(#{privkey := _PrivKey}, _BackendState) ->
    error(privkey_without_pubkey);
prepare_spec(Spec, BackendState) ->
    #{data_dir := DataDir} = BackendState,
    #{name := Name} = Spec,
    Password = ?PEER_KEYS_PASSWORD,
    KeysDir = keys_dir(DataDir, Name),
    {_, PeerPub, _, PeerPriv} = aec_keys:setup_peer_keys(Password, KeysDir),
    Spec#{pubkey => PeerPub, privkey => PeerPriv}.

-spec peer_from_spec(node_spec(), backend_state()) -> binary().
peer_from_spec(Spec, BackendState) ->
    #{postfix := Postfix} = BackendState,
    #{name := Name, pubkey := Key} = Spec,
    Hostname = format("~s~s", [Name, Postfix]),
    aec_peers:encode_peer_address(
        #{host => Hostname, port => ?EXT_SYNC_PORT, pubkey => Key}).

-spec setup_node(node_spec(), backend_state()) -> node_state().
setup_node(Spec, BackendState) ->
    #{log_fun := LogFun,
      postfix := Postfix,
      data_dir := DataDir,
      temp_dir := TempDir} = BackendState,
    #{name := Name,
      pubkey := PubKey,
      privkey := PrivKey,
      peers := Peers,
      source := {pull, Image}} = Spec,
    ExtraConfig = maps:get(config, Spec, #{}),

    Hostname = format("~s~s", [Name, Postfix]),
    ExposedPorts = #{
        sync => ?EXT_SYNC_PORT,
        ext_http => ?EXT_HTTP_PORT,
        int_http => ?INT_HTTP_PORT,
        ext_ws => ?EXT_WS_PORT
    },
    {LocalPorts, Sockets} = allocate_ports([sync, ext_http, int_http, ext_ws]),
    NodeState = #{
        postfix => Postfix,
        log_fun => LogFun,
        hostname => Hostname,
        pubkey => PubKey,
        privkey => PrivKey,
        exposed_ports => ExposedPorts,
        local_ports => LocalPorts,
        sockets => Sockets
    },

    NetworkSpecs = maps:get(networks, Spec, ?DEFAULT_NETWORKS),
    [Network  | OtherNetworks] = setup_networks(NetworkSpecs, NodeState),

    ConfigFileName = format("aeternity_~s.yaml", [Name]),
    ConfigFileHostPath = filename:join([TempDir, "config", ConfigFileName]),
    ct:log("~p will be using config ~p", [Name, ConfigFileHostPath]),
    TemplateFile = filename:join(DataDir, ?CONFIG_FILE_TEMPLATE),
    PeerVars = lists:map(fun (Addr) -> #{peer => Addr} end, Peers),
    CuckooMinerVars =
        case maps:find(cuckoo_miner, Spec) of
            error -> #{};
            {ok, CuckooMiner} ->
                #{cuckoo_miner_present => [#{}],
                  cuckoo_miner =>
                      %% This may be improved upon.
                      [#{executable => maps:get(ex, CuckooMiner),
                         extra_args => maps:get(args, CuckooMiner),
                         edge_bits => maps:get(bits, CuckooMiner)
                        }]}
        end,
    HardForkVars =
        case maps:find(hard_forks, Spec) of
            {ok, HardForks} ->
                #{hard_forks =>
                      #{present => true,
                        hard_fork_info =>
                            lists:map(
                              fun({V, H}) -> #{version => V, height => H} end,
                              maps:to_list(HardForks))}};
            error ->
                #{hard_forks =>
                      #{present => false,
                        hard_fork_info => []}}
        end,
    Mining = maps:merge(#{autostart => true,
                          strictly_follow_top => false}, maps:get(mining, Spec, #{})),
    ct:log("~p has set mining ~p", [Name, Mining]),
    ForkManagementVars =
        case maps:find(fork_management, Spec) of
            {ok, #{fork :=
                       #{enabled := Enabled,
                         signalling_start_height := SigStartHeight,
                         signalling_end_height := SigEndHeight,
                         signalling_block_count := SigBlockCount,
                         info_field := InfoField,
                         version := Version}}} ->
                #{fork_management =>
                      #{fork =>
                            #{enabled => Enabled,
                              signalling_start_height => SigStartHeight,
                              signalling_end_height => SigEndHeight,
                              signalling_block_count => SigBlockCount,
                              info_field => InfoField,
                              version => Version}}};
            error ->
                #{fork_management => false}
        end,
    RootVars = maps:merge(CuckooMinerVars, HardForkVars),
    RootVars1 = maps:merge(RootVars, ForkManagementVars),
    RootVars2 = RootVars1#{
        hostname => Name,
        ext_addr => format("http://~s:~w/", [Hostname, ?EXT_HTTP_PORT]),
        peers => PeerVars,
        key_password => ?PEER_KEYS_PASSWORD,
        config => ExtraConfig,
        services => #{
            sync => #{port => ?EXT_SYNC_PORT},
            ext_http => #{port => ?EXT_HTTP_PORT},
            int_http => #{port => ?INT_HTTP_PORT},
            ext_ws => #{port => ?EXT_WS_PORT}
        },
        mining => Mining
    },
    Context = #{aeternity_config => RootVars2},
    {ok, ConfigString} = write_template(TemplateFile, ConfigFileHostPath, Context),
    Command =
        case maps:find(custom_command, Spec) of
            error -> [];
            {ok, CustomCommand} -> CustomCommand
        end,
    LogPath = filename:join(TempDir, format("~s_logs", [Name])),
    ok = filelib:ensure_dir(filename:join(LogPath, "DUMMY")),
    KeysDir = keys_dir(DataDir, Name),
    PortMapping = maps:fold(fun(Label, Port, Acc) ->
        [{tcp, maps:get(Label, LocalPorts), Port} | Acc]
    end, [], ExposedPorts),
    OptsUlimits = maps:get(ulimits, Spec, []),
    AllUlimits = case lists:keyfind(nofile, 1, OptsUlimits) of
        false -> [{nofile, 1024, 1024} | OptsUlimits];
        _ -> OptsUlimits
    end,
    Genesis =
        case maps:get(genesis_accounts, Spec, false) of
            false ->
                maps:get(genesis, Spec, undefined);
            from_file ->
                filename:join([TempDir, "accounts_test.json"]);
            GenesisAccounts ->
                AccountsFile = filename:join([TempDir, "accounts_test.json"]),
                ok = file:write_file(AccountsFile, jsx:encode(GenesisAccounts)),
                log(NodeState, "Genesis file ~p", [AccountsFile]),
                AccountsFile
        end,
    ConfigFileGuestPath = maps:get(config_guest_path, Spec, ?AETERNITY_CONFIG_FILE),
    DbPath =
        case maps:find(db_path, Spec) of
            error -> undefined;
            {ok, TmpDbPath = {_, _}} ->
                ok = filelib:ensure_dir(filename:join(element(1, TmpDbPath), "DUMMY")),
                TmpDbPath
        end,
    DockerConfig0 = #{
        hostname => Hostname,
        network => Network,
        image => Image,
        ulimits => AllUlimits,
        command => Command,
        env => #{"AETERNITY_CONFIG" => ConfigFileGuestPath,
                 "ERL_CRASH_DUMP" => format("~s/erl_crash.dump", [?EPOCH_LOG_FOLDER])},
        labels => #{epoch_system_test => <<"true">>},
        volumes => [
            {rw, KeysDir, ?EPOCH_KEYS_FOLDER},
            {ro, ConfigFileHostPath, ConfigFileGuestPath},
            {rw, LogPath, ?EPOCH_LOG_FOLDER}] ++
            [ {ro, Genesis, ?EPOCH_GENESIS_FILE} || Genesis =/= undefined ] ++
            [ {rw, element(1, DbPath), element(2, DbPath)} || DbPath =/= undefined ],
        ports => PortMapping
    },
    DockerConfig =
        case maps:find(entrypoint, Spec) of
            {ok, Entrypoint} -> DockerConfig0#{entrypoint => Entrypoint};
            error -> DockerConfig0
        end,
    #{'Id' := ContId} = aest_docker_api:create_container(Hostname, DockerConfig),
    log(NodeState, "Container ~p [~s] created", [Name, ContId]),

    lists:map(fun(NetId) ->
        aest_docker_api:connect_container(ContId, NetId),
        log(NodeState, "Container [~s] connected to network [~s]",
            [ContId, NetId])
    end, OtherNetworks),

    [YamlConfig] = yamerl:decode(ConfigString),
    NodeState#{
        container_id => ContId,
        config_path => ConfigFileHostPath,
        config => YamlConfig,
        log_path => {LogPath, list_to_binary(?EPOCH_LOG_FOLDER)}
    }.

-spec delete_node(node_state()) -> ok.
delete_node(#{container_id := ID, hostname := Name} = NodeState) ->
    aest_docker_api:delete_container(ID),
    log(NodeState, "Container ~p [~s] deleted", [Name, ID]),
    ok.

-spec start_node(node_state()) -> node_state().
start_node(NodeState) ->
    #{container_id := ID, hostname := Name, sockets := Sockets} = NodeState,
    [gen_tcp:close(S) || S <- Sockets],
    aest_docker_api:start_container(ID),
    log(NodeState, "Container ~p [~s] started", [Name, ID]),
    NodeState#{sockets := []}.

-spec stop_node(node_state(), stop_node_options()) -> node_state().
stop_node(#{container_id := ID, hostname := Name} = NodeState, Opts) ->
    Timeout = maps:get(soft_timeout, Opts, ?EPOCH_STOP_TIMEOUT),
    case is_running(ID) of
        false ->
            log(NodeState, "Container ~p [~s] already not running", [Name, ID]);
        true ->
            attempt_epoch_stop(NodeState, Timeout),
            case aest_docker_api:wait_stopped(ID, Timeout) of
                timeout ->
                    aest_docker_api:kill_container(ID);
                ok ->
                    log(NodeState,
                        "Container ~p [~s] detected as stopped", [Name, ID]),
                    ok
            end,
            log(NodeState, "Container ~p [~s] stopped", [Name, ID])
    end,
    NodeState.

-spec kill_node(node_state()) -> node_state().
kill_node(#{container_id := ID, hostname := Name} = NodeState) ->
    aest_docker_api:kill_container(ID),
    log(NodeState, "Container ~p [~s] killed", [Name, ID]),
    NodeState.

-spec stop_container(node_state(), stop_node_options()) -> node_state().
stop_container(#{container_id := ID, hostname := Name} = NodeState, Opts) ->
    case is_running(ID) of
        false ->
            log(NodeState, "Container ~p [~s] already not running", [Name, ID]);
        true ->
            Timeout = maps:get(soft_timeout, Opts, ?EPOCH_STOP_TIMEOUT),
            case aest_docker_api:wait_stopped(ID, Timeout) of
                timeout ->
                    aest_docker_api:kill_container(ID);
                ok ->
                    log(NodeState,
                        "Container ~p [~s] detected as stopped", [Name, ID]),
                    ok
            end,
            log(NodeState, "Container ~p [~s] stopped", [Name, ID])
    end,
    NodeState.

-spec node_logs(node_state()) -> iodata().
node_logs(#{container_id := ID} = _NodeState) ->
    aest_docker_api:container_logs(ID).

-spec get_peer_address(node_state()) -> binary().
get_peer_address(NodeState) ->
    get_internal_address(sync, NodeState).

-spec get_service_address(service_label(), node_state()) -> binary().
get_service_address(sync, NodeState) ->
    #{local_ports := #{sync := Port}, pubkey := Key} = NodeState,
    aec_peers:encode_peer_address(#{host => <<"localhost">>,
                                    port => Port,
                                    pubkey => Key});
get_service_address(Service, NodeState)
  when Service == ext_http; Service == int_http ->
    #{local_ports := #{Service := Port}} = NodeState,
    format("http://localhost:~w/", [Port]);
get_service_address(ext_ws, NodeState) ->
    #{local_ports := #{ext_ws := Port}} = NodeState,
    format("ws://localhost:~w/", [Port]).

-spec get_internal_address(service_label(), node_state()) -> binary().
get_internal_address(sync, NodeState) ->
    #{hostname := Hostname,
      exposed_ports := #{sync := Port},
      pubkey := Key} = NodeState,
    aec_peers:encode_peer_address(#{host => Hostname,
                                    port => Port,
                                    pubkey => Key});
get_internal_address(Service, NodeState)
  when Service == ext_http; Service == int_http ->
    #{hostname := Hostname, exposed_ports := #{Service := Port}} = NodeState,
    format("http://~s:~w/", [Hostname, Port]);
get_internal_address(ext_ws, NodeState) ->
    #{hostname := Hostname, exposed_ports := #{ext_ws := Port}} = NodeState,
    format("ws://~s:~w/", [Hostname, Port]).

-spec get_node_pubkey(node_state()) -> binary().
get_node_pubkey(#{pubkey := PubKey}) -> PubKey.

extract_archive(#{container_id := ID, hostname := Name} = NodeState, Path, Archive) ->
    ok = aest_docker_api:extract_archive(ID, Path, Archive),
    log(NodeState, "Extracted archive of size ~p in container ~p [~s] at path ~p", [byte_size(Archive), Name, ID, Path]),
    NodeState.

run_cmd_in_node_dir(#{container_id := ID, hostname := Name} = NodeState, Cmd, Opts) ->
    log(NodeState, "Running command ~p on container ~p [~s] with options ~p", [Cmd, Name, ID, Opts]),
    {ok, Status, Result} = aest_docker_api:exec(ID, Cmd, Opts),
    log(NodeState, "Run command ~p on container ~p [~s] with result ~p",
        [Cmd, Name, ID, Result]),
    case Result of
        undefined -> {ok, {Status, ""}, NodeState};
        Str when is_binary(Str) ->
            {ok, {Status, binary_to_list(Str)}, NodeState}
    end.

-spec connect_node(atom(), node_state()) -> node_state().
connect_node(NetName, NodeState) ->
    #{container_id := ContId} = NodeState,
    [NetId] = setup_networks([NetName], NodeState),
    aest_docker_api:connect_container(ContId, NetId),
    log(NodeState, "Container [~s] connected to network [~s]", [ContId, NetId]),
    NodeState.

-spec disconnect_node(atom(), node_state()) -> node_state().
disconnect_node(NetName, NodeState) ->
    #{container_id := ContId} = NodeState,
    [NetId] = setup_networks([NetName], NodeState),
    aest_docker_api:disconnect_container(ContId, NetId),
    log(NodeState, "Container [~s] disconnected from network [~s]",
        [ContId, NetId]),
    NodeState.

-spec get_log_path(node_state()) -> binary().
get_log_path(#{log_path := LogPath}) -> LogPath.

export(#{container_id := ID} = _NodeState, Name) ->
    #{'Id' := ImageID} = aest_docker_api:commit(ID, Name),
    {pull, ImageID}.

%=== INTERNAL FUNCTIONS ========================================================

keys_dir(DataDir, Name) ->
    KeysDir = filename:join([DataDir, "keys", Name]),
    ok = filelib:ensure_dir(filename:join(KeysDir, "DUMMY")),
    KeysDir.

log(#{log_fun := LogFun}, Fmt, Args) -> log(LogFun, Fmt, Args);
log(undefined, _Fmt, _Args) -> ok;
log(LogFun, Fmt, Args) when is_function(LogFun) -> LogFun(Fmt, Args).

uid2postfix(undefined) -> <<>>;
uid2postfix(<<>>) -> <<>>;
uid2postfix(Uid) -> <<"_", Uid/binary>>.

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    {ok, Port, Socket}.

allocate_ports(Labels) -> allocate_ports(Labels, #{}, []).

allocate_ports([], Ports, Sockets) ->
    {Ports, Sockets};
allocate_ports([Label | Labels], Ports, Sockets) ->
    {ok, Port, Socket} = free_port(),
    allocate_ports(Labels, Ports#{Label => Port}, [Socket | Sockets]).


format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

write_template(TemplateFile, OutputFile, Context) ->
    {{ok, TemplateBin}, _} = {file:read_file(TemplateFile), TemplateFile},
    Data = bbmustache:render(TemplateBin, Context, [{key_type, atom}]),
    ok = filelib:ensure_dir(OutputFile),
    ok = file:write_file(OutputFile, Data),
    {ok, Data}.

is_running(Id) -> is_running(Id, 5).

is_running(_Id, 0) -> error(retry_exausted);
is_running(Id, Retries) ->
    case aest_docker_api:inspect(Id) of
        #{'State' := State} -> maps:get('Running', State, false);
        _ ->
            % Inspect may fail sometime when stopping a node, just retry
            timer:sleep(100),
            is_running(Id, Retries - 1)
    end.

attempt_epoch_stop(#{container_id := ID, hostname := Name, sockets := Sockets} = NodeState, Timeout) ->
    Cmd = [?AETERNITY_OPS_BIN, "stop"],
    CmdStr = lists:join(" " , Cmd),
    log(NodeState,
        "Container ~p [~s] still running: "
        "attempting to stop node by executing command ~s",
        [Name, ID, CmdStr]),
    try
        retry_epoch_stop(NodeState, ID, Cmd, #{timeout => Timeout, stderr => false}, 5),
        [ gen_tcp:close(S) || S <- Sockets ]
    catch
        throw:{exec_start_timeout, TimeoutInfo} ->
            log(NodeState,
                "Command execution timed out on container ~p [~s]:~n~p",
                [Name, ID, TimeoutInfo])
    end,
    ok.

%% Sometime the stop command do not succeed...
retry_epoch_stop(_NodeState, _ID, _Cmd, _Opts, 0) ->
    error(retry_exausted);
retry_epoch_stop(NodeState, ID, Cmd, Opts, Retry) ->
    #{container_id := ID, hostname := Name} = NodeState,
    {ok, Status, Res0} = aest_docker_api:exec(ID, Cmd, Opts),
    Res1 = string:trim(Res0, trailing, [$\r, $\n]),
    case {Status, string:equal(Res1, <<"ok">>)} of
        {137, true} ->
            ok;
        _ ->
            CmdStr = lists:join(" ", Cmd),
            log(NodeState, "Command executed on container ~p [~s]: ~s (~p)~n~s",
                    [Name, ID, CmdStr, Status, Res0]),
            retry_epoch_stop(NodeState, ID, Cmd, Opts, Retry - 1)
    end.

setup_networks(NetworkSpecs, NodeState) ->
    Networks = maps:from_list(lists:map(
        fun(#{'Name' := Name, 'Id' := Id}) -> {Name, Id} end,
        aest_docker_api:get_networks())),
    setup_networks(NetworkSpecs, NodeState, Networks, []).

setup_networks([], _NodeState, _Networks, Acc) -> lists:reverse(Acc);
setup_networks([NetName | Rest], NodeState, Networks, Acc) ->
    #{postfix := Postfix} = NodeState,
    FullName = format("~s~s", [NetName, Postfix]),
    case maps:find(FullName, Networks) of
        {ok, NetId} ->
            setup_networks(Rest, NodeState, Networks, [NetId | Acc]);
        error ->
            NetSpec = #{name => FullName},
            #{'Id' := NetId} = aest_docker_api:create_network(NetSpec),
            log(NodeState, "Network ~p [~s] created", [NetName, NetId]),
            setup_networks(Rest, NodeState, Networks, [NetId | Acc])
    end.
