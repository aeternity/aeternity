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
-export([node_logs/1]).
-export([get_peer_address/1]).
-export([get_service_address/2]).
-export([get_node_pubkey/1]).
-export([extract_archive/3]).
-export([run_cmd_in_node_dir/3]).
-export([connect_node/2]).
-export([disconnect_node/2]).
-export([get_log_path/1]).
-export([export/2]).

%=== MACROS ====================================================================

-define(CONFIG_FILE_TEMPLATE, "epoch.yaml.mustache").
-define(EPOCH_CONFIG_FILE, "/home/epoch/epoch.yaml").
-define(EPOCH_LOG_FOLDER, "/home/epoch/node/log").
-define(EPOCH_KEYS_FOLDER, "/home/epoch/node/keys").
-define(EPOCH_GENESIS_FILE, "/home/epoch/node/data/aecore/.genesis/accounts.json").
-define(EPOCH_MINE_RATE, 1000).
-define(EPOCH_MAX_INBOUND, 100).
-define(EXT_HTTP_PORT, 3013).
-define(EXT_SYNC_PORT, 3015).
-define(INT_HTTP_PORT, 3113).
-define(INT_WS_PORT, 3114).
-define(EXT_WS_PORT, 3014).
-define(EPOCH_STOP_TIMEOUT, 30000).
-define(PEER_KEYS_PASSWORD, <<"top secret">>).
-define(DEFAULT_NETWORKS, [epoch]).

%=== TYPES =====================================================================

-type log_fun() :: fun((io:format(), list()) -> ok) | undefined.
-type test_uid() :: binary() | undefined.
-type service_label() :: sync | ext_http | int_http | int_ws | ext_ws.

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
    mine_rate => default | pos_integer(),
    cuckoo_miner => default | #{ex := binary(),
                                args := binary(),
                                bits := pos_integer()},
    hard_forks => #{non_neg_integer() => non_neg_integer()}, % Consensus protocols (version -> height)
    config => #{atom() => term()}
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
    log_path := binary()        % Path where the node logs are
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

-spec setup_node(node_spec(), backend_state()) -> node_spec().
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
    MineRate = maps:get(mine_rate, Spec, ?EPOCH_MINE_RATE),
    ExtraConfig = maps:get(config, Spec, #{}),

    Hostname = format("~s~s", [Name, Postfix]),
    ExposedPorts = #{
        sync => ?EXT_SYNC_PORT,
        ext_http => ?EXT_HTTP_PORT,
        int_http => ?INT_HTTP_PORT,
        int_ws => ?INT_WS_PORT
    },
    {LocalPorts, Sockets} = allocate_ports([sync, ext_http, int_http, int_ws, ext_ws]),
    NodeState = #{
        postfix => Postfix,
        log_fun => LogFun,
        name => Name,
        hostname => Hostname,
        pubkey => PubKey,
        privkey => PrivKey,
        exposed_ports => ExposedPorts,
        local_ports => LocalPorts,
        sockets => Sockets
    },

    NetworkSpecs = maps:get(networks, Spec, ?DEFAULT_NETWORKS),
    [Network  | OtherNetworks] = setup_networks(NetworkSpecs, NodeState),

    ConfigFileName = format("epoch_~s.yaml", [Name]),
    ConfigFilePath = filename:join([TempDir, "config", ConfigFileName]),
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
                         node_bits => maps:get(bits, CuckooMiner)
                        }]}
        end,
    HardForkVars =
        case maps:find(hard_forks, Spec) of
            error -> #{};
            {ok, HardForks} ->
                #{hard_forks_present => [#{}],
                  hard_forks =>
                      lists:map(fun({V, H}) -> #{version => V, height => H} end,
                                maps:to_list(HardForks))}
        end,
    RootVars = (maps:merge(CuckooMinerVars, HardForkVars))#{
        hostname => Name,
        ext_addr => format("http://~s:~w/", [Hostname, ?EXT_HTTP_PORT]),
        peers => PeerVars,
        key_password => ?PEER_KEYS_PASSWORD,
        config => ExtraConfig,
        services => #{
            sync => #{port => ?EXT_SYNC_PORT},
            ext_http => #{port => ?EXT_HTTP_PORT},
            int_http => #{port => ?INT_HTTP_PORT},
            ext_ws => #{port => ?EXT_WS_PORT},
            int_ws => #{port => ?INT_WS_PORT}
        },
        mining => maps:merge(#{autostart => true}, maps:get(mining, Spec, #{}))
    },
    Context = #{epoch_config => RootVars},
    ok = write_template(TemplateFile, ConfigFilePath, Context),
    Command =
        case MineRate of
            default -> [];
            _ when is_integer(MineRate), MineRate > 0 ->
                ["-aecore", "expected_mine_rate", MineRate]
        end,
    LogPath = filename:join(TempDir, format("~s_logs", [Name])),
    ok = filelib:ensure_dir(filename:join(LogPath, "DUMMY")),
    KeysDir = keys_dir(DataDir, Name),
    PortMapping = maps:fold(fun(Label, Port, Acc) ->
        [{tcp, maps:get(Label, LocalPorts), Port} | Acc]
    end, [], ExposedPorts),
    Genesis = maps:get(genesis, Spec, undefined),
    DockerConfig = #{
        hostname => Hostname,
        network => Network,
        image => Image,
        ulimits => [{nofile, 1024, 1024}],
        command => Command,
        env => #{"EPOCH_CONFIG" => ?EPOCH_CONFIG_FILE},
        labels => #{epoch_system_test => <<"true">>},
        volumes => [
            {rw, KeysDir, ?EPOCH_KEYS_FOLDER},
            {ro, ConfigFilePath, ?EPOCH_CONFIG_FILE},
            {rw, LogPath, ?EPOCH_LOG_FOLDER}] ++ 
            [ {ro, Genesis, ?EPOCH_GENESIS_FILE} || Genesis =/= undefined ],
        ports => PortMapping
    },
    #{'Id' := ContId} = aest_docker_api:create_container(Hostname, DockerConfig),
    log(NodeState, "Container ~p [~s] created", [Name, ContId]),

    lists:map(fun(NetId) ->
        aest_docker_api:connect_container(ContId, NetId),
        log(NodeState, "Container [~s] connected to network [~s]",
            [ContId, NetId])
    end, OtherNetworks),

    NodeState#{
        container_name => Hostname,
        container_id => ContId,
        config_path => ConfigFilePath,
        log_path => LogPath
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

-spec node_logs(node_state()) -> iodata().
node_logs(#{container_id := ID} = _NodeState) ->
    aest_docker_api:container_logs(ID).

-spec get_peer_address(node_state()) -> binary().
get_peer_address(NodeState) ->
    #{hostname := Hostname,
      exposed_ports := #{sync := Port},
      pubkey := Key} = NodeState,
    aec_peers:encode_peer_address(#{host => Hostname,
                                    port => Port,
                                    pubkey => Key}).

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
get_service_address(Service, NodeState) 
  when Service == ext_ws; Service == int_ws ->
    #{local_ports := #{Service := Port}} = NodeState,
    format("ws://localhost:~w/", [Port]).

-spec get_node_pubkey(node_state()) -> binary().
get_node_pubkey(#{pubkey := PubKey}) -> PubKey.

extract_archive(#{container_id := ID, hostname := Name} = NodeState, Path, Archive) ->
    ok = aest_docker_api:extract_archive(ID, Path, Archive),
    log(NodeState, "Extracted archive of size ~p in container ~p [~s] at path ~p", [byte_size(Archive), Name, ID, Path]),
    NodeState.

run_cmd_in_node_dir(#{container_id := ID, hostname := Name} = NodeState, Cmd, Timeout) ->
    log(NodeState, "Running command ~p on container ~p [~s]", [Cmd, Name, ID]),
    {ok, Status, Result} = aest_docker_api:exec(ID, Cmd, #{timeout => Timeout}),
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
    file:write_file(OutputFile, Data).

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
    Cmd = ["/home/epoch/node/bin/epoch", "stop"],
    CmdStr = lists:join(" " , Cmd),
    log(NodeState,
        "Container ~p [~s] still running: "
        "attempting to stop node by executing command ~s",
        [Name, ID, CmdStr]),
    try
        retry_epoch_stop(NodeState, ID, Cmd, #{timeout => Timeout}, 5),
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
    case aest_docker_api:exec(ID, Cmd, Opts) of
        {ok, 137, <<"ok\r\n">>} -> ok;
        {ok, Status, Res} ->
            CmdStr = lists:join(" ", Cmd),
            log(NodeState, "Command executed on container ~p [~s]: ~s (~p)~n~s",
                    [Name, ID, CmdStr, Status, Res]),
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
