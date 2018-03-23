%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module holding the entry point for incoming P2P connections
%%% @end
%%%=============================================================================
-module(aec_peer_connection_listener).

-define(DEFAULT_SYNC_PORT, 3099).

-export([start_link/0, stop/0]).

start_link() ->
    Pid = spawn_link(fun pc_listener/0),
    register(?MODULE, Pid),
    {ok, Pid}.

stop() ->
    ?MODULE ! stop,
    ok.

pc_listener() ->
    {_Scheme, Host, _Port} = aeu_env:local_peer(),
    {ok, SecKey} = aec_keys:peer_privkey(),
    {ok, PubKey} = aec_keys:peer_pubkey(),
    Port = aeu_env:user_config_or_env([<<"sync">>, <<"port">>], aecore, sync_port, ?DEFAULT_SYNC_PORT),
    lager:info("Starting peer_connection_listener at port ~p", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}, binary, {reuseaddr, true}]),
    pc_listener(LSock, #{ seckey => SecKey, pubkey => PubKey, local_host => Host, local_port => Port }).

pc_listener(LSock, Opts) ->
    Self = self(),
    spawn_link(fun() -> acceptor(Self, LSock) end),
    receive
        {accept, {ok, TcpSock}} ->
            aec_peer_connection:accept(TcpSock, Opts),
            pc_listener(LSock, Opts);
        {accept, Err = {error, _}} ->
            lager:error("accept failed with reason ~p", [Err]),
            pc_listener(LSock, Opts);
        stop ->
            gen_tcp:close(LSock)
    end.

acceptor(Parent, LSock) ->
    Res = gen_tcp:accept(LSock),
    case Res of
        {ok, Sock} ->
            LPort = aeu_env:user_config_or_env([<<"sync">>, <<"port">>], aecore, sync_port, ?DEFAULT_SYNC_PORT),
            {ok, {Host, Port}} = inet:peername(Sock),
            lager:debug("ZZPC: port ~p got tcp_connect ~p from ~p", [LPort, Sock, {Host, Port}]),
            ok = gen_tcp:controlling_process(Sock, Parent);
        {error, _} -> ok
    end,
    Parent ! {accept, Res}.

