%% -*- mode:erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aesc_listeners).
-behaviour(gen_server).

-export([
          start_link/0
        , ensure_listener/1
        , ensure_listener/2
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
              ports = new_tab(aesc_listeners_ports)
            , pids  = new_tab(aesc_listeners_pids)
            , socks = new_tab(aesc_listener_socks)
            , refs  = new_tab(aesc_listeners_refs)
            }).
-record(port, {key, mref, lsock, type}).
-record(pid , {key, lsock, mref}).          % key :: {Port, pid()}
-record(sock, {key, port}).
-record(ref , {mref, port, lsock, pid}).

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

ensure_listener(Port) ->
    ensure_listener(Port, #{}).

ensure_listener(Port, Opts) ->
    call({ensure_listener, Port, Opts}).

close(Port) ->
    call({close, Port}).

lsock_info(LSock) ->
    lsock_info(LSock, all).

lsock_info(LSock, Item) ->
    call({lsock_info, LSock, Item}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    St = start_listeners(#st{}),
    {ok, St}.

start_listeners(St0) ->
    Key = [<<"channels">>, <<"listeners">>],
    case aeu_env:find_config(Key, [user_config, {value, []}]) of
{ok, Ls0} ->
    {ok, [#{<<"acceptors">> := AcceptorsDefault}]} = aeu_env:schema_default_values(Key),
            Ls = lists:map(
                   fun(#{<<"port">> := P} = L) ->
                           As = maps:get(<<"acceptors">>, L, AcceptorsDefault),
                           #{port => P, acceptors => As, type => preconfigured}
                   end, Ls0),
            lager:info("Preconfigured channel listeners: ~p", [Ls]),
            lager:info("Acceptors default: ~p", [AcceptorsDefault]),
            lists:foldl(fun new_listener_or_fail/2, St0, Ls);
        _ ->
            St0
    end.

new_listener_or_fail(#{port := Port} = L, #st{ports = Ports} = St) ->
    case db_lookup(Ports, Port) of
        [_] ->
            error({duplicate_port, Port});
        [] ->
            new_listener_(L, St)
    end.

ensure_listener_(Port, Opts, Pid, #st{ports = Ports} = St) ->
    case db_lookup(Ports, Port) of
        [#port{lsock = LSock}] ->
            lager:debug("Listener already active on port ~p: ~p", [Port, LSock]),
            {ok, LSock, St};
        [] ->
            lager:debug("No listener on port ~p yet - create one", [Port]),
            As = acceptors(St),
            try new_listener(#{port => Port, opts => Opts, acceptors => As,
                               type => dynamic, pid => Pid}, St)
            catch
                error:Reason ->
                    {error, {exception, Reason}}
            end
    end.

new_listener(#{port := Port} = L, #st{ ports = Ports } = St) ->
    St1 = new_listener_(L, St),
    [#port{lsock = LSock}] = db_lookup(Ports, Port),
    {ok, LSock, St1}.

-spec new_listener_(map(), #st{}) -> #st{} | no_return().
new_listener_(#{port := Port, acceptors := As, type := Type} = L, #st{ports = Ports, socks = Socks} = St) ->
    try gen_tcp:listen(Port, listen_opts(St)) of
        {ok, LSock} ->
            lager:debug("New listener on port ~p: ~p", [Port, LSock]),
            Ports1 = db_insert(Ports, #port{key = Port, lsock = LSock, type = Type}),
            Socks1 = db_insert(Socks, #sock{key = LSock, port = Port}),
            start_acceptors(LSock, Port, As),
            maybe_dynamic(L, LSock, St#st{ports = Ports1, socks = Socks1});
        {error, Err} ->
            lager:error("Cannot open State Channel listener on ~p: ~p", [Port, Err]),
            error({Err, Port})
    catch
        error:Ex ->
            lager:error("Cannot open State Channel listener on ~p: CAUGHT error:~p", [Port, Ex]),
            error({Ex, Port})
    end.

maybe_dynamic(#{type := dynamic, pid := Pid, port := Port}, LSock, #st{refs = Refs, pids = Pids} = St) ->
    MRef = erlang:monitor(process, Pid),
    Refs1 = db_insert(Refs, #ref{ mref  = MRef
                                , port  = Port
                                , lsock = LSock
                                , pid   = Pid }),
    Pids1 = db_insert(Pids, #pid{key = {Port, Pid}, lsock = LSock, mref = MRef}),
    St#st{refs = Refs1, pids = Pids1};
maybe_dynamic(_, _, St) ->
    St.

listen_opts(_) ->
    [ {reuseaddr, true}
    , {mode, binary} ].

%% Default number of concurrent acceptors to initiate for a listener.
acceptors(_) ->
    1.

handle_call({ensure_listener, Port, Opts}, {Pid,_Ref}, #st{} = St) ->
    case ensure_listener_(Port, Opts, Pid, St) of
        {ok, LSock, St1} ->
            {reply, {ok, LSock}, St1};
        {error, _} = Error ->
            {reply, Error, St}
    end;
handle_call({close, LSock}, {Pid,_Ref}, #st{ socks = Socks
                                           , pids  = Pids
                                           , refs  = Refs} = St) ->
    case db_lookup(Socks, LSock) of
        [#sock{port = Port}] ->
            case db_lookup(Pids, {Port, Pid}) of
                [#pid{mref = MRef}] ->
                    erlang:demonitor(MRef),
                    Refs1 = db_delete(Refs, MRef),
                    Pids1 = db_delete(Pids, {Port, Pid}),
                    {reply, ok, maybe_close_lsock(
                                  Port, LSock, St#st{ refs = Refs1
                                                    , pids = Pids1 })};
                [] ->
                    {reply, ok, maybe_close_lsock(Port, LSock, St)}
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

handle_info({'DOWN', MRef, process, Pid, _Reason}, #st{ refs = Refs
                                                      , pids = Pids } = St) ->
    case db_lookup(Refs, MRef) of
        [#ref{port = Port, lsock = LSock, pid = Pid}] ->
            Refs1  = db_delete(Refs, MRef),
            Pids1  = db_delete(Pids, {Port, Pid}),
            {noreply, maybe_close_lsock(Port, LSock, St#st{ refs  = Refs1
                                                          , pids  = Pids1 })};
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

start_acceptors(LSock, Port, As) ->
    InitF = fun aesc_session_noise:start_generic_acceptor/3,
    aesc_acceptors:start_acceptors(Port, LSock, InitF, As, []).

stop_acceptors(Port) ->
    aesc_acceptors:stop_acceptors(Port).

maybe_close_lsock(Port, LSock, #st{ pids = Pids } = St) ->
    case db_next(Pids, {Port,0}) of
        {Port,_}        -> St;
        '$end_of_table' -> St;
        {_OtherPort, _} ->
            %% no more references to LSock
            close_lsock_if_dynamic(Port, LSock, St)
    end.

close_lsock_if_dynamic(Port, LSock, #st{ports = Ports, socks = Socks} = St) ->
    case db_lookup(Ports, Port) of
        [#port{lsock = LSock, type = T}] when element(1,T) == dynamic ->
            stop_acceptors(Port),
            ok = gen_tcp:close(LSock),
            Socks1 = db_delete(Socks, LSock),
            Ports1 = db_delete(Ports, Port),
            St#st{ ports = Ports1
                 , socks = Socks1 };
        _ ->
            St
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

db_insert(#{db := Tab, keypos := _P} = Db, Obj) ->
    ets:insert(Tab, Obj),
    Db.

db_next(#{db := Tab, keypos := _P}, Key) ->
    ets:next(Tab, Key).

db_select(#{db := Tab}, Pat) ->
    ets:select(Tab, Pat).

list_pids(Pids, P) ->
    db_select(Pids, [{ #pid{ key = {P,'$1'}, _ = '_'}, [], ['$1'] }]).

get_lsock_info(pids, #{port := Port} = I, #st{ pids = Pids }) ->
    I#{pids => list_pids(Pids, Port)};
get_lsock_info(Item, I, _) when Item == port; Item == lsock ->
    I;
get_lsock_info(Items, I, St) when is_list(Items) ->
    lists:foldl(
      fun(Item, Acc) ->
              get_lsock_info(Item, Acc, St)
      end, I, Items);
get_lsock_info(all, I, St) ->
    get_lsock_info([pids, responders], I, St).
