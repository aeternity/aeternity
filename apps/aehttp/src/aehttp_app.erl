%%%-------------------------------------------------------------------
%% @doc aehttp public API
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_app).

-behaviour(application).


-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).

%% Application callbacks
-export([start/2, stop/1]).

-export([local_peer_uri/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aehttp_sup:start_link(),
    ok = start_swagger_external(),
    gproc:reg({n,l,{epoch, app, aehttp}}),
    {ok, Pid}.

local_peer_uri() ->
    Port = get_port(),
    local_peer(Port).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_swagger_external() ->
    Port = get_port(),
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

get_port() ->
    application:get_env(aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT).
