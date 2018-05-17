%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Keeping state of running nodes during system test.
%%% @end
%%% --------------------------------------------------------------------

-module(aest_nodes_mgr).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/2, start_link/2, stop/0]).
-export([cleanup/0, dump_logs/0, setup_nodes/1, start_node/1, stop_node/2, 
         get_service_address/2, get_log_paths/0, log/2]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

%=== MACROS ====================================================================

-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).
-define(CALL_TIMEOUT, 60000).
-define(NODE_TEARDOWN_TIMEOUT, 0).
-define(DEFAULT_HTTP_TIMEOUT, 3000).

%=== TYPES ====================================================================



%=== GENERIC API FUNCTIONS =====================================================

start(Backends, #{data_dir := _DataDir,
                  temp_dir := _TempDir} = EnvMap) ->
    {ok, _} = application:ensure_all_started(hackney),
    gen_server:start({local, ?SERVER}, ?MODULE, [Backends, EnvMap], []).

start_link(Backends, #{data_dir := _DataDir,
                  temp_dir := _TempDir} = EnvMap) ->
    {ok, _} = application:ensure_all_started(hackney),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Backends, EnvMap], []).

stop() ->
    gen_server:stop(?SERVER).

cleanup() ->
    gen_server:call(?SERVER, cleanup).

dump_logs() ->
    gen_server:call(?SERVER, dump_logs).

get_log_paths() ->
    gen_server:call(?SERVER, get_log_paths).

setup_nodes(NodeSpecs) ->
    gen_server:call(?SERVER, {setup_nodes, NodeSpecs}).

start_node(NodeName) ->
    gen_server:call(?SERVER, {start_node, NodeName}).

stop_node(NodeName, Timeout) ->
    gen_server:call(?SERVER, {stop_node, NodeName, Timeout}, 60000).

get_service_address(NodeName, Service) ->
    gen_server:call(?SERVER, {get_service_address, NodeName, Service}).

log(Format, Params) ->
    gen_server:call(?SERVER, {log, Format, Params}).

%=== BEHAVIOUR GEN_SERVER CALLBACK FUNCTIONS ===================================

init([Backends, EnvMap]) ->
    Opts = EnvMap#{test_id => maps:get(test_id, EnvMap, <<"quickcheck">>), 
                   log_fun => maps:get(log_fun, EnvMap, undefined)},
    InitialState = Opts,
    %% why keeping dirs twice??
    {ok, InitialState#{backends => [{Backend, Backend:start(Opts)} || Backend <- Backends ],
                       nodes => #{} }}.

handle_call(get_log_paths, _From, State) ->
    {reply, mgr_get_log_paths(State), State};
handle_call(cleanup, _From, State) ->
    CleanState = mgr_cleanup(State),
    {reply, ok, CleanState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(dump_logs, _From, #{nodes := Nodes} = State) ->
    [ Backend:node_logs(Node) || {Backend, Node} <- maps:values(Nodes) ],
    {reply, ok, State};
handle_call({setup_nodes, NodeSpecs}, _From, State) ->
    #{backends := Backends, nodes := Nodes} = State,
    PreparedSpecs = mgr_prepare_specs(NodeSpecs, State),
    SetupNodes = [mgr_setup_node(Spec, Backends) || Spec <- PreparedSpecs],
    NewNodes = add_nodes(SetupNodes, Nodes),
    {reply, ok, State#{nodes => NewNodes}};
handle_call({get_node_pubkey, NodeName}, _From, State) ->
    {reply, mgr_get_node_pubkey(NodeName, State), State};
handle_call({get_service_address, NodeName, Service}, _From, State) ->
    {reply, mgr_get_service_address(NodeName, Service, State), State};
handle_call({start_node, NodeName}, _From, #{nodes := Nodes} = State) ->
    {Mod, NodeState} = maps:get(NodeName, Nodes),
    NodeState2 = Mod:start_node(NodeState),
    {reply, ok, State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}};
handle_call({stop_node, NodeName, Timeout}, _From, #{nodes := Nodes} =State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Opts = #{soft_timeout => Timeout},
    NodeState2 = Mod:stop_node(NodeState, Opts),
    {reply, ok, State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}};
handle_call({kill_node, NodeName}, _From, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:kill_node(NodeState),
    {reply, ok, State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}};
handle_call({extract_archive, NodeName, Path, Archive}, _From, State) ->
    {reply, ok, mgr_extract_archive(NodeName, Path, Archive, State)};
handle_call({run_cmd_in_node_dir, NodeName, Cmd, Timeout}, _From, State) ->
    {ok, Reply, NewState} = mgr_run_cmd_in_node_dir(NodeName, Cmd, Timeout, State),
    {reply, Reply, NewState};
handle_call({connect_node, NodeName, NetName}, _From, State) ->
    {reply, ok, mgr_connect_node(NodeName, NetName, State)};
handle_call({disconnect_node, NodeName, NetName}, _From, State) ->
    {reply, ok, mgr_disconnect_node(NodeName, NetName, State)};
handle_call({log, Format, Params}, _From, State) ->
    {reply, ok, mgr_log(Format, Params, State)};
handle_call({export, NodeName, Name}, _From, State) ->
    {ok, Reply, NewState} = mgr_export_node(NodeName, Name, State),
    {reply, Reply, NewState};
handle_call(Request, From, _State) ->
    erlang:error({unknown_request, Request, From}).

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mgr_cleanup(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%=== INTERNAL FUNCTIONS ========================================================

log(LogFun, Format, Params) when is_function(LogFun) ->
    LogFun(Format, Params);
log(#{log_fun := LogFun}, Format, Params) when is_function(LogFun) ->
    LogFun(Format, Params);
log(_, _, _) -> ok.

mgr_get_log_paths(#{nodes := Nodes}) ->
    maps:fold(fun(NodeName, {Mod, NodeState}, Acc) ->
        Acc#{NodeName => Mod:get_log_path(NodeState)}
    end, #{}, Nodes).

mgr_cleanup(State) ->
    %% Node cleanup can be disabled for debugging,
    %% then we keep dockers running
    case os:getenv("EPOCH_DISABLE_NODE_CLEANUP") of
        Value when Value =:= "true"; Value =:= "1" ->
            State;
        _ ->
            [ begin 
                  Backend:stop_node(Node, #{soft_timeout => 20000}),
                  Backend:delete_node(Node)
              end || {Backend, Node} <- maps:values(maps:get(nodes, State)) ],
            [ Backend:stop(BackendState) || {Backend, BackendState} <- maps:get(backends, State) ],
            State#{backends => [], nodes => #{}}
    end.

mgr_get_service_address(NodeName, Service, #{nodes := Nodes}) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Mod:get_service_address(Service, NodeState).

mgr_get_node_pubkey(NodeName, #{nodes := Nodes}) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Mod:get_node_pubkey(NodeState).

mgr_extract_archive(NodeName, Path, Archive, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:extract_archive(NodeState, Path, Archive),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_run_cmd_in_node_dir(NodeName, Cmd, Timeout, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    {ok, Result, NodeState2} = Mod:run_cmd_in_node_dir(NodeState, Cmd, Timeout),
    {ok, Result, State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}}.

mgr_connect_node(NodeName, NetName, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:connect_node(NetName, NodeState),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_disconnect_node(NodeName, NetName, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:disconnect_node(NetName, NodeState),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_prepare_specs(NodeSpecs, State) ->
    #{backends := Backends, nodes := Nodes} = State,
    PrepSpecs = lists:foldl(fun(#{backend := Mod} = S, Acc) ->
        [{Mod, BackendState}] = Backends,
        [Mod:prepare_spec(S, BackendState) | Acc]
    end, [], NodeSpecs),
    CurrAddrs = maps:map(fun(_, {M, S}) -> M:get_peer_address(S) end, Nodes),
    AllAddrs = lists:foldl(fun(#{backend := Mod, name := Name} = S, Acc) ->
        [{Mod, BackendState}] = Backends,
        Acc#{Name => Mod:peer_from_spec(S, BackendState)}
    end, CurrAddrs, PrepSpecs),
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
    end, PrepSpecs).

mgr_setup_node(#{backend := Mod, name := Name} = NodeSpec, Backends) ->
    case proplists:get_value(Mod, Backends) of
        undefined -> erlang:error({backend_not_provided, Mod});
        BackendState ->
            NodeState = Mod:setup_node(NodeSpec, BackendState),
            {Name, {Mod, NodeState}}
    end.

mgr_log(Format, Params, #{log_fun := LogFun} = State) ->
    log(LogFun, Format, Params),
    State.

mgr_export_node(NodeName, Name, #{nodes := Nodes} = State) ->
    {Mod, NodeState} = maps:get(NodeName, Nodes),
    {ok, Mod:export(NodeState, Name), State}.

add_nodes(New, Old) ->
    lists:foldl(fun({N, S}, Ns) ->
        case maps:is_key(N, Ns) of
            true  -> error({duplicate_node, N});
            false -> maps:put(N, S, Ns)
        end
    end, Old, New).
