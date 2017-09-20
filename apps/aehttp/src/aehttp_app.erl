%%%-------------------------------------------------------------------
%% @doc aehttp public API
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_app).

-behaviour(application).


-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aehttp_sup:start_link(),
    ok = start_swagger_external(),
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_swagger_external() ->
    Port = application:get_env(aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT),
    aec_peers:set_local_peer_uri(local_peer(Port)),
    Spec = swagger_server:child_spec(swagger_ext, #{
                                       ip => {0, 0, 0, 0},
                                       port => Port,
                                       net_opts => [],
                                       logic_handler => aehttp_dispatch_ext
                                      }),
    {ok, _} = supervisor:start_child(aehttp_sup, Spec),
    ok.

local_peer(Port) ->
    case application:get_env(aehttp, local_peer_address) of
	{ok, Addr} ->
	    Addr;
	_ ->
	    {ok, Host} = inet:gethostname(),
	    aec_peers:uri_from_ip_port(Host, Port)
    end.
