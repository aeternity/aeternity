%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    Service handling UPnP and NAT-PMP protocols discovery and port mappings,
%%%    to help to achieve better P2P connectivity. This works for routers with
%%%    UPnP or NAT-PNP enabled, and should be used when there is no port
%%%    forwarding configured.
%%% @end
%%%=============================================================================

-module(aec_upnp).

-behaviour(gen_server).

%% API
-export([is_enabled/0]).

%% gen_server API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Port mapping lifetime in seconds.
%% NAT-PMP RFC (https://tools.ietf.org/html/rfc6886) recommends to set it
%% to 7200 seconds (two hours). No recommendation for UPnP found.
-define(MAPPING_LIFETIME, 7200).

%%%===================================================================
%%% API
%%%===================================================================

is_enabled() ->
    case aeu_env:find_config([<<"sync">>, <<"upnp_enabled">>],
                             [user_config, schema_default]) of
        {ok, IsEnabled} -> IsEnabled;
        undefined       -> false
    end.

%%%===================================================================
%%% Gen server API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    epoch_sync:info("Starting UPnP/NAT-PMP service"),
    erlang:send_after(rand:uniform(1000), self(), add_port_mapping),
    {ok, Args}.

handle_call(Request, _From, State) ->
    epoch_sync:error("Received unknown request: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Other, State) ->
    epoch_sync:error("Received unknown cast: ~p", [Other]),
    {noreply, State}.

handle_info(add_port_mapping, State) ->
    ok = add_port_mapping(),
    %% Give additional 10 secs for UPnP/NAT-PMP discovery and setup, to
    %% make sure there is continuity in port mapping.
    erlang:send_after(1000 * (?MAPPING_LIFETIME - 10), self(), add_port_mapping),
    {noreply, State};
handle_info(Other, State) ->
    epoch_sync:error("Received unknown info message: ~p", [Other]),
    {noreply, State}.

terminate(_Reason, _State) ->
    delete_port_mapping(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_port_mapping() ->
    case nat:discover() of
        {ok, Ctx} ->
            InternalPort = aec_connection_sup:sync_port(),
            ExternalPort = aec_connection_sup:ext_sync_port(),
            case nat:add_port_mapping(Ctx, tcp, InternalPort, ExternalPort, ?MAPPING_LIFETIME) of
                {ok, _, _, _, _} ->
                    epoch_sync:info("UPnP/NAT-PMP mapping between ~p and ~p added", [InternalPort, ExternalPort]);
                {error, _Reason} = Error ->
                    epoch_sync:warning("Adding UPnP/NAT-PMP mapping between ~p and ~p failed: ~p", [InternalPort, ExternalPort, Error])
            end;
        no_nat ->
            epoch_sync:warning("UPnP/NAT-PMP discovery failed")
    end.

delete_port_mapping() ->
    case nat:discover() of
        {ok, Ctx} ->
            InternalPort = aec_connection_sup:sync_port(),
            ExternalPort = aec_connection_sup:ext_sync_port(),
            case nat:delete_port_mapping(Ctx, tcp, InternalPort, ExternalPort) of
                ok ->
                    epoch_sync:info("UPnP/NAT-PMP mapping between ~p and ~p removed", [InternalPort, ExternalPort]);
                {error, _Reason} = Error ->
                    epoch_sync:warning("Removal of UPnP/NAT-PMP mapping between ~p and ~p failed: ~p", [InternalPort, ExternalPort, Error])
            end;
        no_nat ->
            epoch_sync:warning("UPnP/NAT-PMP discovery failed")
    end.
