%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module holding the entry point for incoming P2P connections
%%% @end
%%%=============================================================================
-module(aec_peer_connection_listener).

-export([start_link/0, stop/0]).

%% TODO: Make this a gen_server
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
    Port = aec_peers:sync_port(),
    erlang:process_flag(trap_exit, true),
    lager:info("Starting peer_connection_listener at port ~p", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}, binary, {reuseaddr, true}]),
    pc_listener(LSock, #{ seckey => SecKey, pubkey => PubKey, local_host => Host, local_port => Port }).

pc_listener(LSock, Opts) ->
    Self = self(),
    spawn_link(fun() -> acceptor(Self, LSock) end),
    receive
        {accept, {ok, TcpSock}} ->
            %% TODO: should be started by/added to a supervisor
            aec_peer_connection:accept(TcpSock, Opts),
            pc_listener(LSock, Opts);
        {accept, Err = {error, _}} ->
            lager:error("accept failed with reason ~p", [Err]),
            pc_listener(LSock, Opts);
        {'EXIT', _From, shutdown} ->
            gen_tcp:close(LSock),
            exit(shutdown);
        {'EXIT', From, Reason} ->
            lager:info("~p terminated with reason ~p", [From, Reason]),
            pc_listener(LSock, Opts);
        stop ->
            gen_tcp:close(LSock)
    end.

acceptor(Parent, LSock) ->
    Res = gen_tcp:accept(LSock),
    case Res of
        {ok, Sock} ->
            ok = gen_tcp:controlling_process(Sock, Parent);
        {error, _} -> ok
    end,
    Parent ! {accept, Res}.

