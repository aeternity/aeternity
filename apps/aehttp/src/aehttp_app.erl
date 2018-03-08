%%%-------------------------------------------------------------------
%% @doc aehttp public API
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_app).

-behaviour(application).


-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).
-define(DEFAULT_SWAGGER_EXTERNAL_LISTEN_ADDRESS, <<"0.0.0.0">>).
-define(DEFAULT_SWAGGER_INTERNAL_PORT, 8143).
-define(DEFAULT_SWAGGER_INTERNAL_LISTEN_ADDRESS, <<"127.0.0.1">>).
-define(DEFAULT_WEBSOCKET_INTERNAL_PORT, 8144).
-define(DEFAULT_WEBSOCKET_LISTEN_ADDRESS, <<"127.0.0.1">>).
-define(INT_ACCEPTORS_POOLSIZE, 10).

%% Application callbacks
-export([start/2, stop/1]).

-export([check_env/0]).

%% Tests only
-export([ws_handlers_queue_max_size/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aehttp_sup:start_link(),
    {ok, _} = ws_task_worker_sup:start_link(),
    ok = start_http_api(),
    ok = start_websocket_internal(),
    MaxWsHandlers = get_internal_websockets_acceptors(),
    ok = jobs:add_queue(ws_handlers_queue, [{standard_counter, MaxWsHandlers},
                                            {max_size, ws_handlers_queue_max_size()}]),
    gproc:reg({n,l,{epoch, app, aehttp}}),
    {ok, Pid}.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%------------------------------------------------------------------------------
%% Check user-provided environment
%%------------------------------------------------------------------------------

check_env() ->
    %TODO: we need to validate that all tags are present
    GroupDefaults = #{<<"gossip">>        => true,
                      <<"name_service">>   => true,
                      <<"chain">>         => true,
                      <<"transactions">>  => true,
                      <<"node_operator">> => true,
                      <<"dev">>           => true,
                      <<"debug">>         => false,
                      <<"obsolete">>      => true
                      },
    EnabledGroups =
        lists:foldl(
            fun({Key, Default}, Accum) ->
                Enabled =
                    case aeu_env:user_config([<<"http">>, <<"endpoints">>, Key]) of
                        {ok, Setting} when is_boolean(Setting) ->
                            Setting;
                        undefined ->
                            Default
                    end,
                case Enabled of
                    true -> [Key | Accum];
                    false -> Accum
                end
            end,
            [],
            maps:to_list(GroupDefaults)),
        application:set_env(aehttp, enabled_endpoint_groups, EnabledGroups),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_http_api() ->
    ok = start_http_api(external, aehttp_dispatch_ext),
    ok = start_http_api(internal, aehttp_dispatch_int).

start_http_api(Target, LogicHandler) ->
    PoolSize = get_http_api_acceptors(Target),
    Port = get_http_api_port(Target),
    ListenAddress = get_http_api_listen_address(Target),

    Paths = aehttp_api_router:get_paths(Target, LogicHandler),
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    {ok, _} = cowboy:start_http(Target, PoolSize, [{port, Port}, {ip, ListenAddress}], [
            {env, [ {dispatch, Dispatch} ]},
            {middlewares, [cowboy_router, aehttp_api_middleware, cowboy_handler]}
        ]),
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

get_and_parse_ip_address_from_config_or_env(CfgKey, App, EnvKey, Default) ->
    Config = aeu_env:user_config_or_env(CfgKey, App, EnvKey, Default),
    {ok, IpAddress} = inet:parse_address(binary_to_list(Config)),
    IpAddress.

get_http_api_acceptors(_) -> ?INT_ACCEPTORS_POOLSIZE.

get_http_api_port(external) ->
    aeu_env:user_config_or_env([<<"http">>, <<"external">>, <<"port">>],
                               aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT);
get_http_api_port(internal) ->
    aeu_env:user_config_or_env([<<"http">>, <<"internal">>, <<"port">>],
                               aehttp, [internal, swagger_port], ?DEFAULT_SWAGGER_INTERNAL_PORT).


get_http_api_listen_address(external) ->
    get_and_parse_ip_address_from_config_or_env([<<"http">>, <<"external">>, <<"listen_address">>],
                                                aehttp, [http, websocket, listen_address], ?DEFAULT_SWAGGER_EXTERNAL_LISTEN_ADDRESS);
get_http_api_listen_address(internal) ->
    get_and_parse_ip_address_from_config_or_env([<<"http">>, <<"internal">>, <<"listen_address">>],
                                                aehttp, [http, websocket, listen_address], ?DEFAULT_SWAGGER_INTERNAL_LISTEN_ADDRESS).

get_internal_websockets_listen_address() ->
    get_and_parse_ip_address_from_config_or_env([<<"websocket">>, <<"internal">>, <<"listen_address">>],
                                                aehttp, [internal, websocket, listen_address], ?DEFAULT_WEBSOCKET_LISTEN_ADDRESS).

get_internal_websockets_port() ->
    aeu_env:user_config_or_env([<<"websocket">>, <<"internal">>, <<"port">>],
                               aehttp, [internal, websocket, port], ?DEFAULT_WEBSOCKET_INTERNAL_PORT).

get_internal_websockets_acceptors() ->
    aeu_env:user_config_or_env([<<"websocket">>, <<"internal">>, <<"acceptors">>],
                               aehttp, [internal, websocket, handlers], ?INT_ACCEPTORS_POOLSIZE).

ws_handlers_queue_max_size() -> 5.
