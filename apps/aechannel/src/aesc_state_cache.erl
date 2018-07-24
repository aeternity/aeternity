-module(aesc_state_cache).
-behaviour(gen_server).

-export([
          start_link/0
        , new/3
        , reestablish/2
        , update/3
        , fetch/2
        , delete/1
        ]).

-export([minimum_depth_achieved/4]).

-export([
          init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export([
         table_specs/1
        ]).

%% for diagnostics
-export([
         cache_status/1
        ]).

-define(MIN_DEPTH, 4).
-record(st, {mons_chid = ets:new(mons_chid, [ordered_set]),
             mons_ref  = ets:new(mons_ref, [set]),
             watchers  = ets:new(watchers, [set]),
             min_depth = ?MIN_DEPTH}).
-record(ch, {id, state}).
-record(pch, {id, pubkeys = [], state}).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(PTAB, ?MODULE).

new(ChId, PubKey, State) ->
    gen_server:call(?SERVER, {new, ChId, PubKey, self(), State}).

reestablish(ChId, PubKey) ->
    gen_server:call(?SERVER, {reestablish, ChId, PubKey, self()}).

update(ChId, PubKey, State) ->
    gen_server:cast(?SERVER, {update, ChId, PubKey, State}).

fetch(ChId, PubKey) ->
    gen_server:call(?SERVER, {fetch, ChId, PubKey}).

delete(ChId) ->
    gen_server:cast(?SERVER, {delete, ChId}).

cache_status(ChId) ->
    gen_server:call(?SERVER, {cache_status, ChId}).

minimum_depth_achieved(_, ChId, close, _) ->
    gen_server:call(?SERVER, {min_depth_achieved, ChId, self()}).

table_specs(Mode) ->
    [
     {?MODULE, [
                 aec_db:tab_copies(Mode)
               , {type, ordered_set}
               , {record_name, pch}
               , {attributes, record_info(fields, pch)}
               , {user_properties, [{vsn, table_vsn(pch)}]}
               ]}
    ].

table_vsn(_) -> 1.

start_link() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?MODULE, [ordered_set, public, named_table,
                              {keypos, #ch.id}]);
        _ ->
            ok
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

handle_call({new, ChId, PubKey, Pid, State}, _From, St) ->
    case ets:insert_new(?TAB, #ch{id = key(ChId, PubKey), state = State}) of
        true ->
            St1 = monitor_fsm(Pid, ChId, PubKey, St),
            {reply, ok, St1};
        false ->
            {reply, {error, exists}, St}
    end;
handle_call({reestablish, ChId, PubKey, Pid}, _From, St) ->
    case try_reestablish_cached(ChId, PubKey) of
        {ok, Result} ->
            St1 = monitor_fsm(Pid, ChId, PubKey,
                              remove_watcher(ChId, St)),
            {reply, {ok, Result}, St1};
        {error, _} = Error ->
            {reply, Error, St}
    end;
handle_call({fetch, ChId, PubKey}, _From, St) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch{state = State}] ->
            {reply, {ok, State}, St};
        [] ->
            {reply, error, St}
    end;
handle_call({min_depth_achieved, ChId, _Watcher}, _From, St) ->
    lager:debug("min_depth_achieved - ~p gone", [ChId]),
    {reply, ok, delete_for_chid(ChId, St)};
handle_call({cache_status, ChId}, _From, St) ->
    {reply, cache_status_(ChId), St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({update, ChId, PubKey, State}, St) ->
    ets:update_element(?TAB, key(ChId, PubKey), {#ch.state, State}),
    {noreply, St};
handle_cast({delete, ChId}, St) ->
    {noreply, delete_for_chid(ChId, St)};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', MRef, process, _Pid, _}, St) ->
    case lookup_by_mref(MRef, St) of
        {ChId, PubKey} ->
            move_state_to_persistent(ChId),
            lager:debug("state moved (~p). Cache status: ~p",
                        [ChId, cache_status_(ChId)]),
            St1 = remove_monitor(MRef, ChId, PubKey, St),
            case channel_watcher(ChId, St) of
                {ok, _WPid} ->
                    {noreply, St1};
                error ->
                    {noreply, start_watcher(ChId, St1)}
            end;
        error ->
            {noreply, St}
    end;
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

key(ChId, PubKey) ->
    {ChId, PubKey}.

cache_status_(ChId) ->
    InRam = ets:select(
              ?TAB, [{ #ch{id = key(ChId, '$1'), _ = '_'}, [], ['$1']}]),
    OnDisk = ([] =/= mnesia:dirty_read(?PTAB, ChId)),
    [ {in_ram, InRam}
    , {on_disk, OnDisk}].


try_reestablish_cached(ChId, PubKey) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch{state = State}] ->
            {ok, State};
        [] ->
            case read_persistent(ChId) of
                {ok, Pubkeys, State} ->
                    ets:insert(
                      ?TAB, [#ch{id = key(ChId, PK), state = State}
                             || PK <- Pubkeys]),
                    delete_persistent(ChId),
                    {ok, State};
                error ->
                    {error, not_found}
            end
    end.

move_state_to_persistent(ChId) ->
    Found = ets:select(
              ?TAB, [{ #ch{id = key(ChId, '_'), _ = '_'}, [], ['$_'] }]),
    case Found of
        [] -> ok;
        [#ch{id = {_,PKa}, state = A}, #ch{id = {_,PKb}, state = A}] ->
            Pch = #pch{id = ChId, pubkeys = [PKa, PKb], state = A},
            write_persistent(Pch);
        [#ch{id = {_,PKa}, state = A}, #ch{id = {_,PKb}, state = B}] ->
            Pch0 = #pch{id = ChId, pubkeys = [PKa, PKb]},
            ToSave = case {aesc_offchain_state:get_latest_signed_tx(A),
                           aesc_offchain_state:get_latest_signed_tx(B)} of
                         {{Ra,_}, {Rb,_}} when Ra > Rb ->
                             Pch0#pch{state = A};
                         {{Ra,_}, {Rb,_}} when Ra =< Rb ->
                             %% TODO: when (if at all) could this be == ?
                             Pch0#pch{state = B}
                     end,
            write_persistent(ToSave);
        [#ch{id = {_, PK}, state = State}] ->
            Pch = #pch{id = ChId, pubkeys = [PK], state = State},
            write_persistent(Pch)
    end,
    ets:select_delete(?TAB, [{ #ch{id = key(ChId,'_'), _ = '_'}, [], [true] }]),
    ok.

write_persistent(Pch) ->
    activity(fun() -> mnesia:write(?PTAB, Pch, write) end).

read_persistent(ChId) ->
    activity(fun() ->
                     case mnesia:read(?PTAB, ChId) of
                         [#pch{pubkeys = PKs, state = State}] ->
                             {ok, PKs, State};
                         [] ->
                             error
                     end
             end).

delete_persistent(ChId) ->
    activity(fun() -> mnesia:delete(?PTAB, ChId, write) end).

activity(F) ->
    aec_db:ensure_transaction(F).


monitor_fsm(Pid, ChId, PubKey, #st{mons_ref = MR, mons_chid = MC} = St) ->
    MRef = erlang:monitor(process, Pid),
    ets:insert(MR, {MRef, ChId, PubKey}),
    ets:insert(MC, {{ChId, PubKey}, MRef}),
    St.

lookup_by_mref(MRef, #st{mons_ref = MR}) ->
    case ets:lookup(MR, MRef) of
        [{_, ChId, PubKey}] ->
            {ChId, PubKey};
        [] ->
            error
    end.

remove_monitor(MRef, ChId, PubKey, #st{mons_ref = MR, mons_chid = MC} = St) ->
    ets:delete(MR, MRef),
    ets:delete(MC, {ChId, PubKey}),
    St.

remove_monitor_by_chid(ChId, #st{mons_chid = MC} = St) ->
    Found = ets:select(MC, [{ {{ChId, '$1'}, '$2'}, [], [{{'$1','$2'}}] }]),
    lists:foldl(
      fun({PubKey, MRef}, St1) ->
              remove_monitor(MRef, ChId, PubKey, St1)
      end, St, Found).

channel_watcher(ChId, #st{watchers = Ws}) ->
    case ets:lookup(Ws, ChId) of
        [{_, Pid}] ->
            case is_process_alive(Pid) of
                true ->
                    lager:debug("watcher already running: ~p (~p)",
                                [Pid, ChId]),
                    {ok, Pid};
                false ->
                    lager:debug("ERROR: watcher ~p (~p) not running",
                                [Pid, ChId]),
                    error
            end;
        [] ->
            error
    end.

start_watcher(ChId, #st{watchers = Ws, min_depth = Min} = St) ->
    {ok, Pid} = aesc_fsm_min_depth_watcher:watch_for_channel_close(
                  ChId, Min, ?MODULE),
    lager:debug("watcher started for ~p: ~p", [ChId, Pid]),
    ets:insert(Ws, {ChId, Pid}),
    St.

remove_watcher(ChId, #st{watchers = Ws} = St) ->
    case ets:lookup(Ws, ChId) of
        [{_, Pid}] ->
            unlink(Pid),
            exit(Pid, kill),
            ets:delete(Ws, ChId);
        [] ->
            ok
    end,
    St.

delete_for_chid(ChId, St) ->
    Res = delete_state(
            ChId, remove_watcher(
                    ChId, remove_monitor_by_chid(
                            ChId, St))),
    delete_persistent(ChId),
    lager:debug("delete_for_chid ~p; cache_status: ~p",
                [ChId, cache_status_(ChId)]),
    Res.

delete_state(ChId, St) ->
    ets:select_delete(
      ?TAB, [{ #ch{id = key(ChId, '_'), _ = '_'}, [], [true] }]),
    St.
