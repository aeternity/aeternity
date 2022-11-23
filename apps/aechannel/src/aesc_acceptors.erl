-module(aesc_acceptors).
-behavior(gen_server).

-export([start_link/0]).

-export([ start_acceptors/3
        , start_acceptors/5
        , stop_acceptors/1
        , worker_done/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-define(DEFAULT_N, 1).
-define(SHUTDOWN_TIME, 5000).

-record(st, { ports  = #{}
            , socks  = #{}
            , linked = #{} }).

start_acceptors(Port, LSock, InitF) ->
    start_acceptors(Port, LSock, InitF, ?DEFAULT_N, #{}).

start_acceptors(Port, LSock, InitF, N, Opts) ->
    gen_server:call(?MODULE, {start_acceptors, Port, LSock, InitF, N, Opts}).

stop_acceptors(Port) ->
    gen_server:call(?MODULE, {stop_acceptors, Port}, 2*?SHUTDOWN_TIME).

worker_done(Port) ->
    lager:debug("Acceptor 'done' on port ~p", [Port]),
    gen_server:cast(?MODULE, {worker_done, self(), Port}).

%% gen_server API & callbacks ==================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #st{}}.


handle_call({start_acceptors, Port, LSock, InitF, N, Opts}, _From, #st{ ports = Ports
                                                                      , socks = Socks} = St) ->
    LSMRef = erlang:monitor(port, LSock),
    PortMap = #{ n => N
               , lsock => LSock
               , init_f => InitF
               , opts => Opts
               , workers => #{}},
    St1 = maybe_start_workers(Port, St#st{socks = Socks#{LSock => #{ mref => LSMRef
                                                                   , port => Port}},
                                          ports = Ports#{Port => PortMap}}),
    {reply, ok, St1};
handle_call({stop_acceptors, Port}, _From, #st{ports = Ports} = St) ->
    St1 = case maps:is_key(Port, Ports) of
              true ->
                  stop_workers(Port, St);
              false ->
                  St
          end,
    {reply, ok, St1};
handle_call(_, _, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({worker_done, Pid, Port}, St) ->
    St1 = delete_worker(Pid, Port, St),
    {noreply, maybe_start_workers(Port, St1)};
handle_cast(_, St) ->
    {noreply, St}.

handle_info({'EXIT', Pid, _}, #st{linked = Linked} = St) ->
    case maps:find(Pid, Linked) of
        {ok, Port} ->
            St1 = delete_worker(Pid, Port, St),
            {noreply, maybe_start_workers(Port, St1)};
        error ->
            {noreply, St}
    end;
handle_info({'DOWN', _MRef, port, LSock, _}, #st{socks = Socks} = St) ->
    St1 = case maps:find(LSock, Socks) of
              {ok, #{port := Port}} ->
                  Socks1 = maps:remove(LSock, Socks),
                  #st{ports = Ports1} = St1_ = stop_workers(Port, St#st{socks = Socks1}),
                  St1_#st{ports = maps:remove(Port, Ports1)};
              error ->
                  St
          end,
    {noreply, St1};
handle_info(_, St) ->
    {noreply, St}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

%% Internal functions ==========================================================


maybe_start_workers(Port, #st{ports = Ports} = St) ->
    case maps:find(Port, Ports) of
        {ok, #{ n := N
              , workers := Ws } = Map} when map_size(Ws) < N ->
            {Pids, Map1} = start_workers_(Port, N, Ws, Map),
            Ports1 = Ports#{Port := Map1},
            Linked1 = add_map_elems(Pids, Port, St#st.linked),
            St#st{ports = Ports1, linked = Linked1};
        _ ->
            St
    end.

start_workers_(Port, N, Ws, #{lsock := LSock,
                             init_f := InitF,
                             opts := Opts} = Map) ->
    Pids = [start_worker_(InitF, Port, LSock, Opts) || _ <- lists:seq(1, N - map_size(Ws))],
    Ws1 = add_map_elems(Pids, 1, Ws),
    {Pids, Map#{workers := Ws1}}.

start_worker_(InitF, Port, LSock, Opts) ->
    {ok, Pid} = InitF(Port, LSock, Opts),
    Pid.

delete_worker(Pid, Port, #st{linked = Linked, ports = Ports} = St) ->
    Ports1 = case maps:find(Port, Ports) of
                 {ok, #{workers := Ws} = Map} ->
                     Ws1 = maps:remove(Pid, Ws),
                     Map1 = Map#{workers := Ws1},
                     Ports#{Port := Map1};
                 error ->
                     Ports
             end,
    Linked1 = maps:remove(Pid, Linked),
    St#st{linked = Linked1, ports = Ports1}.

stop_workers(Port, #st{ports = Ports, linked = Linked} = St) ->
    case maps:find(Port, Ports) of
        {ok, #{workers := Ws} = Map} ->
            Pids = maps:keys(Ws),
            shut_down_pids(Pids),
            Map1 = Map#{workers := Ws},
            Linked1 = maps:without(Pids, Linked),
            St#st{ports = Ports#{Port := Map1}, linked = Linked1};
        error ->
            St
    end.

shut_down_pids(Pids) ->
    [unlink(P) || P <- Pids],
    MRefs = maps:from_list([{P, erlang:monitor(process, P)} || P <- Pids]),
    TRef = erlang:start_timer(?SHUTDOWN_TIME, self(), worker_shutdown),
    [exit(P, shutdown) || P <- Pids],
    await_downs(MRefs, TRef).

await_downs(MRefs, TRef) when map_size(MRefs) > 0 ->
    receive
        {'DOWN', MRef, _, _, _} when is_map_key(MRef, MRefs) ->
            await_downs(maps:remove(MRef, MRefs), TRef);
        {timeout, TRef, worker_shutdown} ->
            [kill_worker(Pid, MRef) || {Pid, MRef} <- maps:to_list(MRefs)]
    end;
await_downs(_, TRef) ->
    erlang:cancel_timer(TRef),
    ok.

kill_worker(Pid, MRef) ->
    erlang:demonitor(MRef),
    exit(Pid, kill).

add_map_elems(Keys, Value, Map) ->
    maps:merge(Map, maps:from_list([{K, Value} || K <- Keys])).

