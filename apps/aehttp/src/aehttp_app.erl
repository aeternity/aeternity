%%%-------------------------------------------------------------------
%% @doc aehttp public API
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_app).

-behaviour(application).


-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).
-define(DEFAULT_SWAGGER_INTERNAL_PORT, 8143).
-define(DEFAULT_WEBSOCKET_INTERNAL_PORT, 8144).
-define(DEFAULT_WEBSOCKET_LISTEN_ADDRESS, "127.0.0.1").
-define(INT_ACCEPTORS_POOLSIZE, 10).

%% Application callbacks
-export([start/2, stop/1]).

%% Tests only
-export([ws_handlers_queue_max_size/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aehttp_sup:start_link(),
    {ok, _} = ws_task_worker_sup:start_link(),
    ok = start_swagger_external(),
    ok = start_swagger_internal(),
    ok = start_websocket_internal(),
    MaxWsHandlers = get_internal_websockets_acceptors(),
    ok = jobs:add_queue(ws_handlers_queue, [{standard_counter, MaxWsHandlers},
                                            {max_size, ws_handlers_queue_max_size()}]),
    gproc:reg({n,l,{epoch, app, aehttp}}),
    {ok, Pid}.


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

start_swagger_internal() ->
    Port = get_internal_port(),
    Spec = swagger_server:child_spec(swagger_int, #{
                                       ip => {0, 0, 0, 0},
                                       port => Port,
                                       net_opts => [],
                                       logic_handler => aehttp_dispatch_int
                                      }),
    {ok, _} = supervisor:start_child(aehttp_sup, Spec),
    ok.

start_websocket_internal() ->
    Port = get_internal_websockets_port(),
    PoolSize = get_internal_websockets_acceptors(),
    ListenAddress = get_internal_websockets_listen_address(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, PoolSize,
                                 [{port, Port}, {ip, ListenAddress}],
                                 [{env, [{dispatch, Dispatch}]}]),
    ok.

get_external_port() ->
    aeu_env:user_config_or_env([<<"http">>, <<"external">>, <<"port">>],
                               aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT).

get_internal_port() ->
    aeu_env:user_config_or_env([<<"http">>, <<"internal">>, <<"port">>],
                               aehttp, [internal, swagger_port], ?DEFAULT_SWAGGER_INTERNAL_PORT).

get_internal_websockets_listen_address() ->
    {ok, ListenAddress} = inet:parse_address(aeu_env:user_config_or_env([<<"websocket">>, <<"internal">>, <<"listen_address">>],
                                                                        aehttp, [internal, websocket, listen_address], ?DEFAULT_WEBSOCKET_LISTEN_ADDRESS)),
    ListenAddress.

get_internal_websockets_port() ->
    aeu_env:user_config_or_env([<<"websocket">>, <<"internal">>, <<"port">>],
                               aehttp, [internal, websocket, port], ?DEFAULT_WEBSOCKET_INTERNAL_PORT).

get_internal_websockets_acceptors() ->
    aeu_env:user_config_or_env([<<"websocket">>, <<"internal">>, <<"acceptors">>],
                               aehttp, [internal, websocket, handlers], ?INT_ACCEPTORS_POOLSIZE).

ws_handlers_queue_max_size() -> 5.
