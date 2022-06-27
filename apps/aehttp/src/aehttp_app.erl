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
-define(DEFAULT_ROSETTA_PORT, 8243).
-define(DEFAULT_ROSETTA_LISTEN_ADDRESS, <<"127.0.0.1">>).

-define(DEFAULT_HTTP_ACCEPTORS, 10).

-define(DEFAULT_WEBSOCKET_INTERNAL_PORT, 8144).
-define(DEFAULT_WEBSOCKET_LISTEN_ADDRESS, <<"127.0.0.1">>).

-define(DEFAULT_CHANNEL_WEBSOCKET_PORT, 8044).
-define(DEFAULT_CHANNEL_WEBSOCKET_LISTEN_ADDRESS, <<"127.0.0.1">>).
-define(DEFAULT_CHANNEL_ACCEPTORS, 10).

%% Application callbacks
-export([start/2, stop/1]).

-export([check_env/0]).
-export([enable_internal_debug_endpoints/0]).

-include("aehttp_spec.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aehttp_sup:start_link(),
    ok = start_http_api(),
    ok = start_channel_websocket(),
    {ok, Pid}.


%%--------------------------------------------------------------------
stop(_State) ->
    _ = cowboy:stop_listener(internal),
    _ = cowboy:stop_listener(external),
    _ = cowboy:stop_listener(rosetta),
    _ = cowboy:stop_listener(channels_socket),
    ok.


%%------------------------------------------------------------------------------
%% Check user-provided environment
%%------------------------------------------------------------------------------

check_env() ->
    {ok, MMode} = aeu_env:find_config([<<"system">>, <<"maintenance_mode">>],
                                      [user_config, schema_default]),
    Default0 = not MMode,
    %% TODO: Consider which endpoints might be enabled by default during maintenance mode
    %TODO: we need to validate that all tags are present
    % Q: Why are obsolete groups in the config file if they are obsolete?
    %    This could be a linguistic ambiguity -- "obsolete" is the stage after "deprecated"
    GroupDefaults = #{<<"chain">>         => Default0,
                      <<"transaction">>   => Default0,
                      <<"account">>       => Default0,
                      <<"contract">>      => Default0,
                      <<"oracle">>        => Default0,
                      <<"name_service">>  => Default0,
                      <<"channel">>       => Default0,
                      <<"node_info">>     => Default0,
                      <<"debug">>         => true,
                      <<"dry-run">>       => false,
                      <<"node-operator">> => false,

                      %% Rosetta API tags
                      <<"rosetta">>       => true,
                      <<"Account">>       => true,
                      <<"Block">>         => true,
                      <<"Call">>          => Default0,
                      <<"Construction">>  => Default0,
                      <<"Events">>        => true,
                      <<"Mempool">>       => true,
                      <<"Network">>       => true,
                      <<"Search">>        => true
                      },
    EnabledGroups =
        lists:foldl(
            fun({Key, Default}, Accum) ->
                {ok, Enabled} = aeu_env:find_config([<<"http">>, <<"endpoints">>, Key],
                                                    [ user_config
                                                    , schema_default
                                                    , {value, Default} ]),
                case Enabled of
                    true -> [Key | Accum];
                    false -> Accum
                end
            end,
            [],
            maps:to_list(GroupDefaults)),
    {ok, Obsolete} = aeu_env:find_config([<<"http">>, <<"endpoints">>, <<"obsolete">>],
                                         [user_config, schema_default, {value, []}]),
    Enabled = EnabledGroups -- Obsolete,
    application:set_env(aehttp, enabled_endpoint_groups, Enabled),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_http_api() ->
    ok = start_http_api(external, aehttp_dispatch_ext),
    ok = start_http_api(internal, aehttp_dispatch_int),
    ok = start_http_api(rosetta, aehttp_dispatch_rosetta).

start_http_api(Target, LogicHandler) ->
    PoolSize = get_http_api_acceptors(Target),
    Port = get_http_api_port(Target),
    ListenAddress = get_http_api_listen_address(Target),

    %% caches spec and validator
    case Target of
        rosetta ->
            _ = aehttp_api_validate:validator(?ROSETTA);
        _ ->
            _ = aehttp_api_validate:validator(?SWAGGER2),
            _ = aehttp_api_validate:validator(?OAS3)
    end,

    Paths = aehttp_api_router:get_paths(Target, LogicHandler),
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    Opts = [{port, Port},
            {ip, ListenAddress},
            {num_acceptors, PoolSize}],
    Env = #{env => #{dispatch => Dispatch},
            middlewares => [aehttp_cors_middleware,
                            cowboy_router,
                            cowboy_handler]},
    lager:debug("Target = ~p, Opts = ~p", [Target, Opts]),
    {ok, _} = cowboy:start_clear(Target, Opts, Env),
    ok.

start_channel_websocket() ->
    Port = get_channel_websockets_port(),
    Acceptors = get_channel_websockets_acceptors(),
    ListenAddress = get_channel_websockets_listen_address(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/channel", sc_ws_handler, []}
        ]}
    ]),
    Opts = [{port, Port},
            {ip, ListenAddress},
            {num_acceptors, Acceptors}],
    Env = #{env => #{dispatch => Dispatch}},
    lager:debug("Opts = ~p", [Opts]),
    {ok, _} = cowboy:start_clear(channels_socket, Opts, Env),
    ok.

get_and_parse_ip_address_from_config_or_env(CfgKey, App, EnvKey, Default) ->
    Config = aeu_env:config_value(CfgKey, App, EnvKey, Default),
    {ok, IpAddress} = inet:parse_address(binary_to_list(Config)),
    IpAddress.

get_http_api_acceptors(external) ->
    aeu_env:config_value([<<"http">>, <<"external">>, <<"acceptors">>],
                         aehttp, [external, acceptors], ?DEFAULT_HTTP_ACCEPTORS);
get_http_api_acceptors(internal) ->
    aeu_env:config_value([<<"http">>, <<"internal">>, <<"acceptors">>],
                               aehttp, [internal, acceptors], ?DEFAULT_HTTP_ACCEPTORS);
get_http_api_acceptors(rosetta) ->
    aeu_env:config_value([<<"http">>, <<"rosetta">>, <<"acceptors">>],
                               aehttp, [rosetta, acceptors], ?DEFAULT_HTTP_ACCEPTORS).

get_http_api_port(external) ->
    aeu_env:config_value([<<"http">>, <<"external">>, <<"port">>],
                         aehttp, [external, port], ?DEFAULT_SWAGGER_EXTERNAL_PORT);
get_http_api_port(internal) ->
    aeu_env:config_value([<<"http">>, <<"internal">>, <<"port">>],
                         aehttp, [internal, port], ?DEFAULT_SWAGGER_INTERNAL_PORT);
get_http_api_port(rosetta) ->
    aeu_env:config_value([<<"http">>, <<"rosetta">>, <<"port">>],
                         aehttp, [rosetta, port], ?DEFAULT_ROSETTA_PORT).

get_http_api_listen_address(external) ->
    get_and_parse_ip_address_from_config_or_env([<<"http">>, <<"external">>, <<"listen_address">>],
                                                aehttp, [http, websocket, listen_address], ?DEFAULT_SWAGGER_EXTERNAL_LISTEN_ADDRESS);
get_http_api_listen_address(internal) ->
    get_and_parse_ip_address_from_config_or_env([<<"http">>, <<"internal">>, <<"listen_address">>],
                                                aehttp, [http, websocket, listen_address], ?DEFAULT_SWAGGER_INTERNAL_LISTEN_ADDRESS);
get_http_api_listen_address(rosetta) ->
    get_and_parse_ip_address_from_config_or_env([<<"http">>, <<"rosetta">>, <<"listen_address">>],
                                                aehttp, [http, websocket, listen_address], ?DEFAULT_ROSETTA_LISTEN_ADDRESS).

get_channel_websockets_listen_address() ->
    get_and_parse_ip_address_from_config_or_env(
      [<<"websocket">>, <<"channel">>, <<"listen_address">>],
      aehttp, [channel, websocket,
               listen_address], ?DEFAULT_CHANNEL_WEBSOCKET_LISTEN_ADDRESS).

get_channel_websockets_port() ->
    aeu_env:config_value([<<"websocket">>, <<"channel">>, <<"port">>],
                         aehttp, [channel, websocket, port],
                         ?DEFAULT_CHANNEL_WEBSOCKET_PORT).

get_channel_websockets_acceptors() ->
    aeu_env:config_value([<<"websocket">>, <<"channel">>, <<"acceptors">>],
                         aehttp, [channel, websocket, handlers],
                         ?DEFAULT_CHANNEL_ACCEPTORS).

enable_internal_debug_endpoints() ->
    aeu_env:config_value([<<"http">>, <<"internal">>, <<"debug_endpoints">>],
                         aehttp, [internal, debug_endpoints], false).
