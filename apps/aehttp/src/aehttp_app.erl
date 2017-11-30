%%%-------------------------------------------------------------------
%% @doc aehttp public API
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_app).

-behaviour(application).


-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).
-define(DEFAULT_SWAGGER_INTERNAL_PORT, 8143).
-define(DEFAULT_WEBSOCKET_INTERNAL_PORT, 8144).
-define(INT_ACCEPTORS_POOLSIZE, 10).

%% Application callbacks
-export([start/2, stop/1]).

-export([local_peer_uri/0,
         local_internal_http_uri/0]).

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
    ok = jobs:add_queue(ws_handlers_queue, [{standard_counter, MaxWsHandlers}]),
    gproc:reg({n,l,{epoch, app, aehttp}}),
    {ok, Pid}.

local_peer_uri() ->
    Port = get_external_port(),
    local_peer(Port).

local_internal_http_uri() ->
    Port = get_internal_port(),
    aec_peers:uri_from_ip_port("127.0.0.1", Port).

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
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, PoolSize,
                                 [{port, Port}, {ip, {127, 0, 0, 1}}],
                                 [{env, [{dispatch, Dispatch}]}]),
    ok.

local_peer(Port) ->
    Addr = get_local_peer_address(),
    case aeu_requests:parse_uri(Addr) of
        {_Scheme, _Host, _Port} -> % a valid address
            Addr;
        error ->
            case re:run(Addr, "[:/]", []) of
                {match, _} ->
                    erlang:error({cannot_parse, [{local_peer_address,
                                                  Addr}]});
                nomatch ->
                    aec_peers:uri_from_ip_port(Addr, Port)
            end
    end.


-spec get_local_peer_address() -> string().
get_local_peer_address() ->
    H =
        case aeu_env:user_config(
              [<<"http">>, <<"external">>, <<"peer_address">>]) of
            {ok, A} ->
                binary_to_list(A);
            undefined ->
                case aeu_env:get_env(aehttp, local_peer_address) of
                    {ok, Addr} ->
                        Addr;
                    undefined ->
                        {ok, Host} = inet:gethostname(),
                        Host
                end
        end,
    case io_lib:deep_latin1_char_list(H) of
        true ->
            H;
        false ->
            erlang:error(unsupported_external_peer_address)
    end.

get_external_port() ->
    case aeu_env:user_config([<<"http">>, <<"external">>, <<"port">>]) of
        {ok, P} -> P;
        undefined ->
            aeu_env:get_env(
              aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT)
    end.

get_internal_port() ->
    case aeu_env:user_config([<<"http">>, <<"internal">>, <<"port">>]) of
        {ok, P} -> P;
        undefined ->
            aeu_env:get_env(
              aehttp, [internal, swagger_port], ?DEFAULT_SWAGGER_INTERNAL_PORT)
    end.

get_internal_websockets_port() ->
    case aeu_env:user_config([<<"websocket">>, <<"internal">>, <<"port">>]) of
        {ok, P} -> P;
        undefined ->
            aeu_env:get_env(
              aehttp, [internal, websocket, port],
              ?DEFAULT_WEBSOCKET_INTERNAL_PORT)
    end.

get_internal_websockets_acceptors() ->
    case aeu_env:user_config(
           [<<"websocket">>, <<"internal">>, <<"acceptors">>]) of
        {ok, Sz} -> Sz;
        undefined ->
            aeu_env:get_env(
              aehttp,
              [internal, websocket, handlers], ?INT_ACCEPTORS_POOLSIZE)
    end.
