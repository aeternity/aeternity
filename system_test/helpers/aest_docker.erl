-module(aest_docker).

%=== EXPORTS ===================================================================

%% API exports
-export([start/1]).
-export([stop/1]).
-export([setup_node/2]).
-export([delete_node/1]).
-export([start_node/1]).
-export([stop_node/1, stop_node/2]).
-export([kill_node/1]).
-export([get_service_address/2]).

%=== MACROS ====================================================================

-define(CONFIG_FILE_TEMPLATE, "epoch.yaml.mustache").
-define(EPOCH_CONFIG_FILE, "/home/epoch/epoch.yaml").
-define(EPOCH_LOG_FOLDER, "/home/epoch/node/log").
-define(EPOCH_KEYS_FOLDER, "/home/epoch/node/keys").
-define(EPOCH_MINE_RATE, 1000).
-define(EXT_HTTP_PORT, 3013).
-define(EXT_SYNC_PORT, 3015).
-define(INT_HTTP_PORT, 3113).
-define(INT_WS_PORT, 3114).

%=== TYPES =====================================================================

-type log_fun() :: fun((io:format(), list()) -> ok) | undefined.
-type test_uid() :: binary() | undefined.
-type service_label() :: ext_http | int_http | int_ws.

%% State of the docker backend
-type backend_state() :: #{
    postfix := binary(),        % A unique postfix to add to container names.
    log_fun := log_fun(),       % Function to use for logging.
    data_dir := binary(),       % The directory where the templates can be found.
    temp_dir := binary(),       % A temporary directory that can be used to generate
                                % configuration files and save the log files.
    net_id := binary()          % Docker network identifier
}.

%% Node specification
-type node_spec() :: #{
    name := atom(),
    % Names or URLs of the peer nodes
    peers := [atom() | binary()],
    source := {pull, binary()}  % Source of the node image
}.

%% State of a node
-type node_state() :: #{
    spec := node_spec(),        % Backup of the spec used when adding the node
    log_fun := log_fun(),       % Function to use for logging
    hostname := atom(),         % Hostname of the container running the node
    exposed_ports := #{service_label() => pos_integer()},
    local_ports := #{service_label() => pos_integer()}
}.

-type start_options() :: #{
    test_id => test_uid(),
    log_fun => log_fun(),
    data_dir := binary(),
    temp_dir := binary()
}.

-type stop_node_options() :: #{
    soft_timeout => pos_integer(),
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
    NetName = <<"epoch", Postfix/binary>>,
    #{'Id' := NetId} = aest_docker_api:create_network(#{name => NetName}),
    log(LogFun, "Network ~p [~s] created", [NetName, NetId]),
    #{postfix => Postfix,
      log_fun => LogFun,
      data_dir => DataDir,
      temp_dir => TempDir,
      net_id => NetId
    }.

-spec stop(backend_state()) -> ok.
stop(BackendState) ->
    aest_docker_api:prune_networks(),
    log(BackendState, "Networks pruned", []),
    ok.

-spec setup_node(node_spec(), backend_state()) -> node_spec().
setup_node(Spec, BackendState) ->
    #{log_fun := LogFun,
      postfix := Postfix,
      data_dir := DataDir,
      temp_dir := TempDir,
      net_id := NetId} = BackendState,
    #{name := Name,
      peers := Peers,
      source := {pull, Image}} = Spec,

    Hostname = format("~s~s", [Name, Postfix]),
    ExposedPorts = #{
        sync => ?EXT_SYNC_PORT,
        ext_http => ?EXT_HTTP_PORT,
        int_http => ?INT_HTTP_PORT,
        int_ws => ?INT_WS_PORT
    },
    LocalPorts = allocate_ports([sync, ext_http, int_http, int_ws]),
    NodeState = #{
        spec => spec,
        log_fun => LogFun,
        name => Name,
        hostname => Hostname,
        exposed_ports => ExposedPorts,
        local_ports => LocalPorts
    },

    ConfigFileName = format("epoch_~s.yaml", [Name]),
    ConfigFilePath = filename:join([TempDir, "config", ConfigFileName]),
    TemplateFile = filename:join(DataDir, ?CONFIG_FILE_TEMPLATE),
    PeerVars = lists:map(fun
        (PeerName) when is_atom(PeerName) ->
            PeerHostname = format("~s~s", [PeerName, Postfix]),
            PeerInfo = aec_peers:encode_peer_address(
                          #{ host => PeerHostname, port => ?EXT_SYNC_PORT,
                             pubkey => pubkey(PeerName)}),
            #{peer => PeerInfo}
    end, Peers),
    ct:log("PeerVars: ~p", [PeerVars]),
    RootVars = #{
        hostname => Name,
        ext_addr => format("http://~s:~w/", [Hostname, ?EXT_HTTP_PORT]),
        peers => PeerVars,
        services => #{
            sync => #{port => ?EXT_SYNC_PORT},
            ext_http => #{port => ?EXT_HTTP_PORT},
            int_http => #{port => ?INT_HTTP_PORT},
            int_ws => #{port => ?INT_WS_PORT}
        }
    },
    Context = #{epoch_config => RootVars},
    ok = write_template(TemplateFile, ConfigFilePath, Context),
    LogPath = filename:join(TempDir, format("~s_logs", [Name])),
    ok = filelib:ensure_dir(filename:join(LogPath, "DUMMY")),
    KeysDir = filename:join([DataDir, "keys", Name]),
    PortMapping = maps:fold(fun(Label, Port, Acc) ->
        [{tcp, maps:get(Label, LocalPorts), Port} | Acc]
    end, [], ExposedPorts),
    DockerConfig = #{
        hostname => Hostname,
        network => NetId,
        image => Image,
        ulimits => [{nofile, 1024, 1024}],
        command => ["-aecore", "expected_mine_rate", ?EPOCH_MINE_RATE],
        env => #{"EPOCH_CONFIG" => ?EPOCH_CONFIG_FILE},
        volumes => [
            {ro, KeysDir, ?EPOCH_KEYS_FOLDER},
            {ro, ConfigFilePath, ?EPOCH_CONFIG_FILE},
            {rw, LogPath, ?EPOCH_LOG_FOLDER}
        ],
        ports => PortMapping
    },
    #{'Id' := ContId} = aest_docker_api:create_container(Hostname, DockerConfig),
    log(NodeState, "Container ~p [~s] created", [Name, ContId]),
    NodeState#{
        container_name => Hostname,
        container_id => ContId,
        config_path => ConfigFilePath
    }.

-spec delete_node(node_state()) -> ok.
delete_node(#{container_id := ID, hostname := Name} = NodeState) ->
    aest_docker_api:delete_container(ID),
    log(NodeState, "Container ~p [~s] deleted", [Name, ID]),
    ok.

-spec start_node(node_state()) -> node_state().
start_node(#{container_id := ID, hostname := Name} = NodeState) ->
    aest_docker_api:start_container(ID),
    log(NodeState, "Container ~p [~s] started", [Name, ID]),
    NodeState.

-spec stop_node(node_state()) -> node_state().
stop_node(NodeState) -> stop_node(NodeState, #{}).

-spec stop_node(node_state(), stop_node_options()) -> node_state().
stop_node(#{container_id := ID, hostname := Name} = NodeState, Opts) ->
    aest_docker_api:stop_container(ID, Opts),
    log(NodeState, "Container ~p [~s] stopped", [Name, ID]),
    NodeState.

-spec kill_node(node_state()) -> node_state().
kill_node(#{container_id := ID, hostname := Name} = NodeState) ->
    aest_docker_api:kill_container(ID),
    log(NodeState, "Container ~p [~s] killed", [Name, ID]),
    NodeState.

-spec get_service_address(node_state(), service_label()) -> binary().
get_service_address(Service, NodeState) ->
    #{local_ports := #{Service := Port}} = NodeState,
    format("http://localhost:~w/", [Port]).

%=== INTERNAL FUNCTIONS ========================================================

log(#{log_fun := LogFun}, Fmt, Args) -> log(LogFun, Fmt, Args);
log(undefined, _Fmt, _Args) -> ok;
log(LogFun, Fmt, Args) when is_function(LogFun) -> LogFun(Fmt, Args).

uid2postfix(undefined) -> <<>>;
uid2postfix(<<>>) -> <<>>;
uid2postfix(Uid) -> <<"_", Uid/binary>>.

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.

allocate_ports(Labels) -> allocate_ports(Labels, #{}).

allocate_ports([], Acc) -> Acc;
allocate_ports([Label | Labels], Acc) ->
    allocate_ports(Labels, Acc#{Label => free_port()}).


format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

write_template(TemplateFile, OutputFile, Context) ->
    {{ok, TemplateBin}, _} = {file:read_file(TemplateFile), TemplateFile},
    Data = bbmustache:render(TemplateBin, Context, [{key_type, atom}]),
    ok = filelib:ensure_dir(OutputFile),
    file:write_file(OutputFile, Data).

pubkey(node1) ->
    <<37,195,115,246,90,69,150,234,253,209,246,49,199,88,5,116,191,57,106,189,48,134,209,227,116,85,44,59,51,41,245,55>>;
pubkey(node2) ->
    <<149,164,91,254,32,218,238,174,159,207,156,5,246,182,63,10,57,70,109,226,193,2,33,168,116,32,244,228,169,122,154,94>>;
pubkey(node3) ->
    <<29,110,222,56,140,83,227,182,7,100,207,18,240,52,200,151,221,151,247,213,94,191,198,219,184,33,139,118,35,95,157,120>>.
