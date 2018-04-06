%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module holding the entry point for incoming P2P connections
%%% @end
%%%=============================================================================
-module(aec_peer_connection_listener).

-behaviour(gen_server).

%% API
-export([ start_link/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, { lsock
               , opts
               , acceptor
               }).

%%====================================================================
%% API functions
%%====================================================================

start_link(SyncPort, Address) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SyncPort, Address], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port, Address]) ->
    {ok, SecKey} = aec_keys:peer_privkey(),
    {ok, PubKey} = aec_keys:peer_pubkey(),
    erlang:process_flag(trap_exit, true),
    lager:info("Starting peer_connection_listener at Address: ~p port ~p",
               [Address, Port]),
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}, binary,
                                        {reuseaddr, true},
                                        {ip, Address}]),
    S = #state{ lsock = LSock
              , opts = #{ seckey => SecKey
                        , pubkey => PubKey
                        }},
    {ok, spawn_acceptor(S)}.

handle_cast(What, State) ->
    lager:error("Got unxpected cast: ~p", [What]),
    {noreply, State}.

handle_call(What,_From, State) ->
    lager:error("Got unxpected call: ~p", [What]),
    {reply, {nyi, What}, State}.

handle_info({APid, accept, {ok, TcpSock}}, #state{acceptor = APid} = S) ->
    aec_peer_connection:accept(TcpSock, S#state.opts),
    {noreply, spawn_acceptor(S)};
handle_info({APid, accept, Err = {error, _}}, #state{acceptor = APid} = S) ->
    lager:error("accept failed with reason ~p", [Err]),
    {noreply, spawn_acceptor(S)};
handle_info({'EXIT', _, normal}, S) ->
    {noreply, S};
handle_info({'EXIT', Pid, Reason}, S) ->
    lager:info("~p terminated with reason ~p", [Pid, Reason]),
    case Pid =:= S#state.acceptor of
        true -> {noreply, spawn_acceptor(S)};
        false -> {noreply, S}
    end;
handle_info(What, S) ->
    lager:info("Got unexpected: ~p", [What]),
    {noreply, S}.

terminate(_Reason, State) ->
    try
        gen_tcp:close(State#state.lsock),
        ok
    catch _:_ -> ok
    end.

code_change(_OldVsn, State,_Extra) ->
    {ok, State}.

%%====================================================================
%% Acceptor process
%%====================================================================

spawn_acceptor(#state{lsock = LSock} = S) ->
    Self = self(),
    S#state{acceptor = spawn_link(fun() -> acceptor(Self, LSock) end)}.

acceptor(Parent, LSock) ->
    Res = gen_tcp:accept(LSock),
    case Res of
        {ok, Sock} -> ok = gen_tcp:controlling_process(Sock, Parent);
        {error, _} -> ok
    end,
    Parent ! {self(), accept, Res}.

