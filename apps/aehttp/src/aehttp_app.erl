%%%-------------------------------------------------------------------
%% @doc aehttp public API
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_app).

-behaviour(application).


-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).
-define(INT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_WEBSOCKET_INTERNAL_PORT, 8044).

%% Application callbacks
-export([start/2, stop/1]).

-export([local_peer_uri/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aehttp_sup:start_link(),
    {ok, _} = ws_task_worker_sup:start_link(),
    ok = start_swagger_external(),
    ok = start_websocket_internal(),
    gproc:reg({n,l,{epoch, app, aehttp}}),
    MaxWsHandlers = proplists:get_value(handlers, get_websocket_config()),
    ok = jobs:add_queue(ws_handlers_queue, [{standard_counter, MaxWsHandlers}]),
    {ok, Pid}.

local_peer_uri() ->
    Port = get_external_port(),
    local_peer(Port).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_swagger_external() ->
    Port = get_external_port(),
    Spec = swagger_server:child_spec(swagger_ext, #{
                                       ip => {0, 0, 0, 0},
                                       port => Port,
                                       net_opts => [],
                                       logic_handler => aehttp_dispatch_ext
                                      }),
    {ok, _} = supervisor:start_child(aehttp_sup, Spec),
    ok.

start_websocket_internal() ->
    Port = get_internal_port(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, ?INT_ACCEPTORS_POOLSIZE,
                                 [{port, Port}, {ip, {127, 0, 0, 1}}],
                                 [{env, [{dispatch, Dispatch}]}]),
    ok.

local_peer(Port) ->
    case application:get_env(aehttp, local_peer_address) of
        {ok, Addr} ->
            case aeu_requests:parse_uri(Addr) of
                {_Scheme, _Host, Port} ->    % same port as above
                    Addr;
                {_Scheme, _Host, _OtherPort} ->
                    erlang:error({port_mismatch,
                                  [{swagger_port_external, Port},
                                   {local_peer_address, Addr}]});
                error ->
                    case re:run(Addr, "[:/]", []) of
                        {match, _} ->
                            erlang:error({cannot_parse, [{local_peer_address,
                                                          Addr}]});
                        nomatch ->
                            aec_peers:uri_from_ip_port(Addr, Port)
                    end
            end;
        _ ->
            {ok, Host} = inet:gethostname(),
            aec_peers:uri_from_ip_port(Host, Port)
    end.

get_external_port() ->
    application:get_env(aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT).

get_websocket_config() ->
    InternalConfig = application:get_env(aehttp, internal, []),
    proplists:get_value(websocket, InternalConfig, []).

get_internal_port() ->
    proplists:get_value(port, get_websocket_config(), ?DEFAULT_WEBSOCKET_INTERNAL_PORT).

