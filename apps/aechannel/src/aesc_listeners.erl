-module(aesc_listeners).
-behaviour(gen_server).

-export([
          start_link/0
        , listen/3
        , close/1
        , lsock_info/1  %% (LSock) -> lsock_info(LSock, all).
        , lsock_info/2  %% (LSock, Item | list(Item)) -> undefined | map()
        ]).

-export([
          init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export([ patterns/0
        , record_fields/1]).

-record(st, {
              responders = new_tab(aesc_listeners_responders)
            , ports = new_tab(aesc_listeners_ports)
            , socks = new_tab(aesc_listener_socks)
            , refs  = new_tab(aesc_listeners_refs)
            }).
-record(resp, {key}).
-record(port, {key, mref, lsock}).
-record(sock, {key, port}).
-record(ref , {mref, port, lsock, responder, pid}).

-define(SERVER, ?MODULE).

%% ==================================================================
%% for tracing
-define(EXCEPTION_TRACE, {'_', [], [{exception_trace}]}).
patterns() ->
    exports(?MODULE).

exports(M) ->
    %% [{M, F, A, [?EXCEPTION_TRACE]} || {F,A} <- M:module_info(exports)].
    [{M,'_','_', [?EXCEPTION_TRACE]}].


record_fields(st) -> record_info(fields, st);
record_fields(_ ) -> no.
%% ==================================================================


listen(Port, Responder, Opts) ->
    call({listen, Port, Responder, Opts}).

close(Port) ->
    call({close, Port}).

lsock_info(LSock) ->
    lsock_info(LSock, all).

lsock_info(LSock, Item) ->
    call({lsock_info, LSock, Item}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

handle_call({listen, Port, Responder, Opts}, {Pid,_Ref}, #st{ responders = Resps
                                                            , ports = Ports
                                                            , socks = Socks
                                                            , refs = Refs} = St) ->
    case db_lookup(Ports, {Port,0}) of
        [#port{lsock = LSock}] ->
            MRef = erlang:monitor(process, Pid),
            Ports1 = db_insert(Ports, #port{key = {Port, Pid}, mref = MRef}),
            Refs1 = db_insert(Refs, #ref{ mref = MRef
                                        , port = Port
                                        , responder = Responder
                                        , lsock = LSock
                                        , pid = Pid }),
            Resps1 = db_insert(Resps, #resp{key = {Port, Responder, Pid}}),
            {reply, {ok, LSock}, St#st{ responders = Resps1
                                      , ports = Ports1
                                      , refs  = Refs1 }};
        [] ->
            try gen_tcp:listen(Port, Opts) of
                {ok, LSock} ->
                    MRef = erlang:monitor(process, Pid),
                    Ports1 = db_insert(
                               Ports, [#port{key = {Port,0}, lsock = LSock},
                                       #port{key = {Port,Pid},
                                             lsock = LSock,
                                             mref = MRef}]),
                    Socks1 = db_insert(Socks, #sock{key = LSock, port = Port}),
                    Refs1 = db_insert(Refs, #ref{mref = MRef, port = Port,
                                                 responder = Responder,
                                                 lsock = LSock, pid = Pid}),
                    Resps1 = db_insert(Resps, #resp{key = {Port, Responder, Pid}}),
                    {reply, {ok, LSock}, St#st{ responders = Resps1
                                              , ports = Ports1
                                              , socks = Socks1
                                              , refs  = Refs1 }};
                {error, _} = Error ->
                    {reply, Error, St}
            catch
                error:Reason ->
                    {reply, {exception, Reason}, St}
            end
    end;
handle_call({close, LSock}, {Pid,_Ref}, #st{ responders = Resps
                                           , socks = Socks
                                           , ports = Ports
                                           , refs  = Refs} = St) ->
    case db_lookup(Socks, LSock) of
        [#sock{port = Port}] ->
            case db_lookup(Ports, {Port, Pid}) of
                [#port{mref = MRef}] ->
                    [#ref{ responder = Responder }]
                        = db_lookup(Refs, MRef),
                    erlang:demonitor(MRef),
                    Refs1 = db_delete(Refs, MRef),
                    Ports1 = db_delete(Ports, {Port, Pid}),
                    Resps1 = db_delete(Resps, {Port, Responder, Pid}),
                    {reply, ok, maybe_close_lsock(
                                  Port, LSock, St#st{ responders = Resps1
                                                    , refs  = Refs1
                                                    , ports = Ports1 })};
                [] ->
                    {reply, {exception, not_found}, St}
            end;
        [] ->
            {reply, {exception, not_found}, St}
    end;
handle_call({lsock_info, LSock, Item}, _, #st{socks = Socks} = St) ->
    I = case db_lookup(Socks, LSock) of
            [#sock{port = Port} = Found] ->
                lager:debug("Found = ~p", [Found]),
                get_lsock_info(Item, #{lsock => LSock, port => Port}, St);
            [] ->
                lager:debug("~p not found", [LSock]),
                undefined
        end,
    {reply, I, St};
handle_call(_Req, _From, St) ->
    {reply, {exception, {unknown_call, _Req}}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', MRef, process, Pid, _Reason}, #st{ responders = Resps
                                                      , refs = Refs
                                                      , ports = Ports} = St) ->
    case db_lookup(Refs, MRef) of
        [#ref{port = Port, lsock = LSock, responder = Responder, pid = Pid}] ->
            Refs1  = db_delete(Refs, MRef),
            Ports1 = db_delete(Ports, {Port,Pid}),
            Resps1 = db_delete(Resps, {Port, Responder, Pid}),
            {noreply, maybe_close_lsock(Port, LSock, St#st{ responders = Resps1
                                                          , refs  = Refs1
                                                          , ports = Ports1 })};
        [] ->
            {noreply, St}
    end;
handle_info(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

call(Req) ->
    case gen_server:call(?SERVER, Req) of
        {exception, Reason} ->
            erlang:error(Reason);
        Other ->
            Other
    end.

maybe_close_lsock(Port, LSock, #st{ ports = Ports, socks = Socks } = St) ->
    case db_next(Ports, {Port,0}) of
        {Port,_}        -> St;
        '$end_of_table' -> St;
        {_OtherPort, _} ->
            %% no more references to LSock
            ok = gen_tcp:close(LSock),
            Socks1 = db_delete(Socks, LSock),
            Ports1 = db_delete(Ports, {Port, 0}),
            St#st{ ports = Ports1
                 , socks = Socks1 }
    end.

new_tab(Name) ->
    db_new(Name, [{keypos, 2}, ordered_set, public, named_table]).

db_new(Name, Opts) ->
    {_, P} = lists:keyfind(keypos, 1, Opts),
    #{name => Name, keypos => P, db => ets:new(Name, Opts)}.

db_lookup(#{db := Tab, keypos := _P}, Key) ->
    ets:lookup(Tab, Key).

db_delete(#{db := Tab, keypos := _P} = Db, Key) ->
    ets:delete(Tab, Key),
    Db.

db_insert(#{keypos := _P, db := Tab} = Db, Objs) when is_list(Objs) ->
    ets:insert(Tab, Objs),
    Db;
db_insert(#{db := Tab, keypos := _P} = Db, Obj) ->
    ets:insert(Tab, Obj),
    Db.

db_next(#{db := Tab, keypos := _P}, Key) ->
    ets:next(Tab, Key).

db_select(#{db := Tab}, Pat) ->
    ets:select(Tab, Pat).

list_pids(#{db := PortsTab}, P) ->
    ets:select(PortsTab, [{ #port{ key = {P,'$1'}, _ = '_'}, [], ['$1'] }]).

get_lsock_info(pids, #{port := Port} = I, #st{ ports = Ports }) ->
    I#{pids => list_pids(Ports, Port)};
get_lsock_info(Item, I, _) when Item == port; Item == lsock ->
    I;
get_lsock_info(responders, #{port := Port} = I, #st{ responders = Resps }) ->
    Rs = db_select(Resps, [{ #resp{ key = { Port, '$1', '_'}, _ = '_' }, [], ['$1'] }]),
    I#{responders => lists:usort(Rs)};
get_lsock_info(Items, I, St) when is_list(Items) ->
    lists:foldl(
      fun(Item, Acc) ->
              get_lsock_info(Item, Acc, St)
      end, I, Items);
get_lsock_info(all, I, St) ->
    get_lsock_info([pids, responders], I, St).

