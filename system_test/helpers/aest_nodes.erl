-module(aest_nodes).

-behaviour(gen_server).

%=== EXPORTS ===================================================================

%% Common Test API exports
-export([ct_setup/1]).
-export([ct_cleanup/1]).

%% QuickCheck API exports
-export([eqc_setup/2]).
-export([eqc_cleanup/1]).

%% Generic API exports
-export([setup_nodes/2]).
-export([start_node/2]).
-export([stop_node/3]).
-export([kill_node/2]).
-export([extract_archive/4]).
-export([run_cmd_in_node_dir/3]).
-export([get_service_address/3]).
-export([http_get/5]).

%% Helper function exports
-export([request/4]).
-export([get/5]).
-export([get_block/3]).
-export([get_top/2]).
-export([wait_for_height/4]).

%% Behaviour gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%=== MACROS ====================================================================

-define(BACKENDS, [aest_docker]).
-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).
-define(CALL_TIMEOUT, 60000).
-define(NODE_TEARDOWN_TIMEOUT, 0).
-define(DEFAULT_HTTP_TIMEOUT, 3000).

%=== TYPES ====================================================================

-type test_ctx() :: pid() | proplists:proplist().
-type node_service() :: ext_http | int_http | int_ws.
-type http_path() :: [atom() | binary() | number()] | binary().
-type http_query() :: #{atom() | binary() => atom() | binary()}.
-type json_object() :: term().
-type milliseconds() :: non_neg_integer().
-type seconds() :: non_neg_integer().
-type path() :: binary() | string().
-type peer_spec() :: atom() | binary().

-type node_spec() :: #{
    % The unique name of the node
    name    := atom(),
    % If peer is given as an atom it is expected to be a node name,
    % if given as a binary it is expected to be the external URL of the peer.
    peers   := [peer_spec()],
    backend := aest_docker,

%% When `backend` is `aest_docker`:

    % The source of the docker image
    source  := {pull, binary() | string()}
}.

%=== COMMON TEST API FUNCTIONS =================================================

%% @doc Setups the the node manager for Common Test.
%% The CT config passed as argument is returned with extra values used
%% to contact with the node manager. This config must be passed to all
%% the the other functions as the `Ctx` parameter.
-spec ct_setup(proplists:proplist()) -> proplists:proplist().
ct_setup(Config) ->
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    ct:log("Node logs can be found here: ~n<a href=\"file://~s\">~s</a>",
        [PrivDir, PrivDir]
    ),
    LogFun = fun(Fmt, Args) -> ct:log(Fmt, Args) end,
    case start(DataDir, PrivDir, LogFun) of
        {ok, Pid} -> [{?CT_CONF_KEY, Pid} | Config];
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

%% @doc Stops the node manager and all the nodes that were started.
-spec ct_cleanup(test_ctx()) -> ok.
ct_cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    call(Pid, dump_logs),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000),
    ok.

%=== QICKCHECK API FUNCTIONS ===================================================

%% @doc Setups the node manager for Quick Check tests.
-spec eqc_setup(path(), path()) -> test_ctx().
eqc_setup(DataDir, TempDir) ->
    case start(DataDir, TempDir, undefined) of
        {ok, Pid} -> Pid;
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

%% @doc Stops the node manager for QuickCheck tests.
-spec eqc_cleanup(test_ctx()) -> ok.
eqc_cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000),
    ok.

%=== GENERIC API FUNCTIONS =====================================================

%% @doc Creates and setups a list of nodes.
%% The nodes are not started, use `start_node/2` for that.
-spec setup_nodes([node_spec()], test_ctx()) -> ok.
setup_nodes(NodeSpecs, Ctx) ->
    call(ctx2pid(Ctx), {setup_nodes, NodeSpecs}).

%% @doc Starts a node previously setup.
-spec start_node(atom(), test_ctx()) -> ok.
start_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {start_node, NodeName}).

%% @doc Stops a node previously started with explicit timeout (in seconds)
%% after which the node will be killed.
-spec stop_node(atom(), seconds() | infinity, test_ctx()) -> ok.
stop_node(NodeName, Timeout, Ctx) ->
    call(ctx2pid(Ctx), {stop_node, NodeName, Timeout}).

%% @doc Kills a node.
-spec kill_node(atom(), test_ctx()) -> ok.
kill_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {kill_node, NodeName}).

extract_archive(NodeName, Path, Archive, Ctx) ->
    call(ctx2pid(Ctx), {extract_archive, NodeName, Path, Archive}).

run_cmd_in_node_dir(NodeName, Cmd, Ctx) ->
    call(ctx2pid(Ctx), {run_cmd_in_node_dir, NodeName, Cmd}).

%% @doc Retrieves the address of a given node's service.
-spec get_service_address(atom(), node_service(), test_ctx()) -> binary().
get_service_address(NodeName, Service, Ctx) ->
    call(ctx2pid(Ctx), {get_service_address, NodeName, Service}).

%% @doc Performs and HTTP get on a node service (ext_http or int_http).
-spec http_get(atom(), ext_http | int_http, http_path(), http_query(), test_ctx()) ->
        {ok, pos_integer(), json_object()} | {error, term()}.
http_get(NodeName, Service, Path, Query, Ctx) ->
    Addr = get_service_address(NodeName, Service, Ctx),
    http_addr_get(Addr, Path, Query).

%=== HELPER FUNCTIONS ==========================================================

%% @doc Performs an HTTP get request on the node external API.
%% Should preferably use `get/5` with service `ext_http`.
-spec request(atom(), http_path(), http_query(), test_ctx) -> json_object().
request(NodeName, Path, Query, Ctx) ->
    get(NodeName, ext_http, Path, Query, Ctx).

%% @doc Performs an HTTP get request on a node HTTP service.
-spec get(atom(), int_http | ext_http, http_path(), http_query(), test_ctx()) -> json_object().
get(NodeName, Service, Path, Query, Ctx) ->
    case http_get(NodeName, Service, Path, Query, Ctx) of
        {ok, 200, Response} -> Response;
        {ok, Status, _Response} -> error({unexpected_status, Status});
        {error, Reason} -> error({http_error, Reason})
    end.

%% @doc Retrieves a block at given height from the given node.
%% It will throw an excpetion if the block does not exists.
-spec get_block(atom(), non_neg_integer(), test_ctx()) -> json_object().
get_block(NodeName, Height, Ctx) ->
    case http_get(NodeName, ext_http, [v2, 'block-by-height'],
                  #{height => Height}, Ctx) of
        {ok, 200, Response} -> Response;
        {ok, Status, _Response} -> error({unexpected_status, Status});
        {error, Reason} -> error({http_error, Reason})
    end.

%% @doc Retrieves the top block from the given node.
-spec get_top(atom(), test_ctx()) -> json_object().
get_top(NodeName, Ctx) ->
    case http_get(NodeName, ext_http, [v2, 'top'], #{}, Ctx) of
        {ok, 200, Response} -> Response;
        {ok, Status, _Response} -> error({unexpected_status, Status});
        {error, Reason} -> error({http_error, Reason})
    end.

%% @doc Waits for each specified nodes to have a block at given heigth.
-spec wait_for_height(non_neg_integer(), [atom()], milliseconds(), test_ctx()) -> ok.
wait_for_height(MinHeight, NodeNames, Timeout, Ctx) ->
    Addrs = [get_service_address(N, ext_http, Ctx) || N <- NodeNames],
    Expiration = make_expiration(Timeout),
    wait_for_height(MinHeight, Addrs, [], 500, Expiration).

%=== BEHAVIOUR GEN_SERVER CALLBACK FUNCTIONS ===================================

init([DataDir, TempDir, LogFun]) ->
    process_flag(trap_exit, true), % Make sure terminate always cleans up
    mgr_setup(DataDir, TempDir, LogFun).

handle_call(Request, From, State) ->
    try
        handlex(Request, From, State)
    catch
        throw:Reason ->
            {reply, {'$error', Reason, erlang:get_stacktrace()}, State}
    end.

handlex({get_service_address, NodeName, Service}, _From, State) ->
    {reply, mgr_get_service_address(NodeName, Service, State), State};
handlex({setup_nodes, NodeSpecs}, _From, State) ->
    {reply, ok, mgr_setup_nodes(NodeSpecs, State)};
handlex({start_node, NodeName}, _From, State) ->
    {reply, ok, mgr_start_node(NodeName, State)};
handlex({stop_node, NodeName, Timeout}, _From, State) ->
    {reply, ok, mgr_stop_node(NodeName, Timeout, State)};
handlex({kill_node, NodeName}, _From, State) ->
    {reply, ok, mgr_kill_node(NodeName, State)};
handlex({extract_archive, NodeName, Path, Archive}, _From, State) ->
    {reply, ok, mgr_extract_archive(NodeName, Path, Archive, State)};
handlex({run_cmd_in_node_dir, NodeName, Cmd}, _From, State) ->
    {ok, Reply, NewState} = mgr_run_cmd_in_node_dir(NodeName, Cmd, State),
    {reply, Reply, NewState};
handlex(dump_logs, _From, State) ->
    ok = mgr_dump_logs(State),
    {reply, ok, State};
handlex(cleanup, _From, State) ->
    {reply, ok, mgr_cleanup(State)};
handlex(stop, _From, State) ->
    {stop, normal, ok, State};
handlex(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mgr_cleanup(State),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

log(#{log_fun := undefined}, _Fmt, _Args) -> ok;
log(#{log_fun := LogFun}, Fmt, Args) -> LogFun(Fmt, Args).

uid() ->
    iolist_to_binary([[io_lib:format("~2.16.0B",[X])
                       || <<X:8>> <= crypto:strong_rand_bytes(8) ]]).

ctx2pid(Pid) when is_pid(Pid) -> Pid;
ctx2pid(Props) when is_list(Props) ->
    case proplists:lookup(?CT_CONF_KEY, Props) of
        {?CT_CONF_KEY, Pid} when is_pid(Pid) -> Pid;
        _ ->
            erlang:error({system_test_not_setup, []})
    end.

call(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?CALL_TIMEOUT) of
        {'$error', Reason, Stacktrace} ->
            erlang:raise(throw, Reason, Stacktrace);
        Reply ->
            Reply
    end.

start(DataDir, TempDir, LogFun) ->
    {ok, _} = application:ensure_all_started(hackney),
    gen_server:start(?MODULE, [DataDir, TempDir, LogFun], []).

wait_for_exit(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _Reason} -> ok
    after Timeout -> error({process_not_stopped, Pid})
    end.

make_expiration(Timeout) ->
    {os:timestamp(), Timeout}.

assert_expiration({StartTime, Timeout}) ->
    Now = os:timestamp(),
    Delta = timer:now_diff(Now, StartTime),
    case Delta > (Timeout * 1000) of
        true -> error(timeout);
        false -> ok
    end.

wait_for_height(_Height, [], [], _Delay, _Expiration) -> ok;
wait_for_height(Height, [], Rem, Delay, Expiration) ->
    assert_expiration(Expiration),
    timer:sleep(Delay),
    wait_for_height(Height, lists:reverse(Rem), [], Delay, Expiration);
wait_for_height(Height, [Addr | Addrs], Rem, Delay, Expiration) ->
    case http_addr_get(Addr, [v2, 'block-by-height'], #{height => Height}) of
        {ok, 200, _} ->
            wait_for_height(Height, Addrs, Rem, Delay, Expiration);
        _ ->
            wait_for_height(Height, Addrs, [Addr | Rem], Delay, Expiration)
    end.

http_addr_get(Addr, Path, Query) ->
    http_addr_get(Addr, Path, Query, #{}).

http_addr_get(Addr, Path, Query, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_HTTP_TIMEOUT),
    HttpOpts = [{recv_timeout, Timeout}],
    case hackney:request(get, url(Addr, Path, Query), [], <<>>, HttpOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case hackney_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

url(Base, Path, QS) when is_list(Path) ->
    hackney_url:make_url(Base, [to_binary(P) || P <- Path], maps:to_list(QS));
url(Base, Item, QS) ->
    url(Base, [Item], QS).

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
to_binary(Term)                    -> Term.

hackney_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} -> decode(BodyJson)
    end.

decode(<<>>) -> {ok, undefined};
decode(Data) -> decode_json(Data).

decode_json(Data) ->
    try jsx:decode(Data, [{labels, attempt_atom}, return_maps]) of
        JsonObj -> {ok, JsonObj}
    catch
        error:badarg -> {error, {bad_json, Data}}
    end.

%--- NODE MANAGER PROCESS FUNCTION ---------------------------------------------

mgr_setup(DataDir, TempDir, LogFun) ->
    TestId = uid(),
    BackendOpts = #{
        test_id => TestId,
        log_fun => LogFun,
        data_dir => DataDir,
        temp_dir => TempDir
    },
    Backends = mgr_setup_backends(?BACKENDS, BackendOpts),
    {ok, BackendOpts#{backends => Backends, nodes => #{}}}.

mgr_setup_backends(BackendMods, Opts) ->
    lists:foldl(fun(Mod, Acc) -> Acc#{Mod => Mod:start(Opts)} end,
                #{}, BackendMods).

mgr_dump_logs(#{nodes := Nodes1} = State) ->
    maps:map(fun(Name, {Backend, NodeState}) ->
        try
            log(State, "Logs of node ~p:~n~s", [Name, Backend:node_logs(NodeState)])
        catch
            _:E ->
                ST = erlang:get_stacktrace(),
                log(State, "Error while dumping logs of node ~p: ~p~n~p", [Name, E, ST])
        end
    end, Nodes1),
    ok.

mgr_cleanup(State) ->
    %% So node cleanup can be disabled for debugging without commenting
    %% and accidently pushing code without cleanup...
    case os:getenv("EPOCH_DISABLE_NODE_CLEANUP") of
        Value when Value =:= "true"; Value =:= "1" ->
            State;
        _ ->
            State2 = mgr_safe_stop_all(?NODE_TEARDOWN_TIMEOUT, State),
            State3 = mgr_safe_delete_all(State2),
            mgr_safe_stop_backends(State3)
    end.

mgr_get_service_address(NodeName, Service, #{nodes := Nodes}) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Mod:get_service_address(Service, NodeState).

mgr_setup_nodes(NodeSpecs, State) ->
    NodeSpecs2 = mgr_prepare_specs(NodeSpecs, State),
    lists:foldl(fun mgr_setup_node/2, State, NodeSpecs2).

mgr_setup_node(#{backend := Mod, name := Name} = NodeSpec, State) ->
    #{backends := Backends, nodes := Nodes} = State,
    #{Mod := BackendState} = Backends,
    NodeState = Mod:setup_node(NodeSpec, BackendState),
    State#{nodes := Nodes#{Name => {Mod, NodeState}}}.

mgr_start_node(NodeName, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:start_node(NodeState),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_stop_node(NodeName, Timeout, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Opts = #{soft_timeout => Timeout},
    NodeState2 = Mod:stop_node(NodeState, Opts),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_kill_node(NodeName, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:kill_node(NodeState),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_extract_archive(NodeName, Path, Archive, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:extract_archive(NodeState, Path, Archive),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_run_cmd_in_node_dir(NodeName, Cmd, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    {ok, Result, NodeState2} = Mod:run_cmd_in_node_dir(NodeState, Cmd),
    {ok, Result, State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}}.

mgr_safe_stop_backends(#{backends := Backends} = State) ->
    maps:map(fun(Mod, BackendState) ->
        try
            Mod:stop(BackendState)
        catch
            _:E ->
                ST = erlang:get_stacktrace(),
                log(State, "Error while stopping backend ~p: ~p~n~p", [Mod, E, ST])
        end
    end, Backends),
    State#{backends := #{}}.


mgr_safe_stop_all(Timeout, #{nodes := Nodes1} = State) ->
    Opts = #{soft_timeout => Timeout},
    Nodes2 = maps:map(fun(Name, {Backend, NodeState}) ->
        try
            {Backend, Backend:stop_node(NodeState, Opts)}
        catch
            _:E ->
                ST = erlang:get_stacktrace(),
                log(State, "Error while stopping node ~p: ~p~n~p", [Name, E, ST]),
                {Backend, NodeState}
        end
    end, Nodes1),
    State#{nodes := Nodes2}.

mgr_safe_delete_all(#{nodes := Nodes1} = State) ->
    maps:map(fun(Name, {Backend, NodeState}) ->
        try
            {Backend, Backend:delete_node(NodeState)}
        catch
            _:E ->
                ST = erlang:get_stacktrace(),
                log(State, "Error while stopping node ~p: ~p~n~p", [Name, E, ST]),
                {Backend, NodeState}
        end
    end, Nodes1),
    State#{nodes := #{}}.

mgr_prepare_specs(NodeSpecs, State) ->
    #{backends := Backends, nodes := Nodes} = State,
    CurrAddrs = maps:map(fun(_, {M, S}) -> M:get_peer_address(S) end, Nodes),
    AllAddrs = lists:foldl(fun(#{backend := Mod, name := Name} = S, Acc) ->
        #{Mod := BackendState} = Backends,
        Acc#{Name => Mod:peer_from_spec(S, BackendState)}
    end, CurrAddrs, NodeSpecs),
    lists:map(fun(#{peers := Peers} = Spec) ->
        NewPeers = lists:map(fun
            (Addr) when is_binary(Addr) -> Addr;
            (Name) when is_atom(Name) ->
                case maps:find(Name, AllAddrs) of
                    {ok, Addr} -> Addr;
                    _ -> error({peer_not_found, Name})
                end
        end, Peers),
        Spec#{peers := NewPeers}
    end, NodeSpecs).
