-module(aesc_state_cache).
-behaviour(gen_server).

-export([
          start_link/0
        , new/3
        , new/4
        , reestablish/2
        , reestablish/3
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
        , check_tables/1
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
-type ch_cache_id() :: { aeser_id:val()
                       , aeser_id:val()}.
-record(ch_cache, { cache_id    :: ch_cache_id()
                  , state       :: aesc_offchain_state:state()
                  , salt        :: binary()
                  , session_key :: binary()
                  }).
-record(pch_cache, { cache_id        :: ch_cache_id()
                   , salt            :: binary()
                   , nonce           :: binary()
                   , encrypted_state :: binary()
                   }).

-define(SERVER, ?MODULE).
-define(TAB, aesc_state_cache_ch).
-define(PTAB, aesc_state_cache).
-define(CACHE_DEFAULT_PASSWORD, "correct horse battery staple").

new(ChId, Pubkey, State) ->
    new(ChId, Pubkey, State, ?CACHE_DEFAULT_PASSWORD).

new(ChId, PubKey, State, Password) ->
    gen_server:call(?SERVER, {new, ChId, PubKey, self(), State, Password}).

-spec reestablish(aeser_id:val(), aeser_id:val()) -> {ok, aesc_offchain_state:state()} | {error, atom()}.
reestablish(ChId, Pubkey) ->
    reestablish(ChId, Pubkey, ?CACHE_DEFAULT_PASSWORD).

-spec reestablish(aeser_id:val(), aeser_id:val(), binary()) -> {ok, aesc_offchain_state:state()} | {error, atom()}.
reestablish(ChId, PubKey, Password) ->
    gen_server:call(?SERVER, {reestablish, ChId, PubKey, self(), Password}).

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
     {?PTAB, [
                 aec_db:tab_copies(Mode)
               , {type, ordered_set}
               , {record_name, pch_cache}
               , {attributes, record_info(fields, pch_cache)}
               , {user_properties, [{vsn, table_vsn(pch_cache)}]}
               ]}
    ].

check_tables(Acc) ->
    lists:foldl(
      fun({Tab, Spec}, Acc1) ->
              aec_db:check_table(Tab, Spec, Acc1)
      end, Acc, table_specs(disc)).

table_vsn(_) -> 1.

start_link() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                              {keypos, #ch_cache.cache_id}]);
        _ ->
            ok
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.

generate_session_key(Password, Salt) ->
    enacl:pwhash(Password, Salt).

setup_keys(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    case generate_session_key(Password, Salt) of
        {ok, SessionKey} ->
            {ok, #ch_cache{salt = Salt, session_key = SessionKey}};
        {error, _} = Err ->
            Err
    end.

handle_call({new, ChId, PubKey, Pid, State, Password}, _From, St) ->
    case setup_keys(Password) of
        {ok, Result} ->
            case ets:insert_new(?TAB, Result#ch_cache{cache_id = key(ChId, PubKey), state = State}) of
                true ->
                    St1 = monitor_fsm(Pid, ChId, PubKey, St),
                    {reply, ok, St1};
                false ->
                    {reply, {error, exists}, St}
            end;
        {error, _} = Err ->
            {reply, Err, St}
    end;
handle_call({reestablish, ChId, PubKey, Pid, Password}, _From, St) ->
    case try_reestablish_cached(ChId, PubKey, Password) of
        {ok, Result} ->
            St1 = monitor_fsm(Pid, ChId, PubKey,
                              remove_watcher(ChId, St)),
            {reply, {ok, Result}, St1};
        {error, _} = Error ->
            {reply, Error, St}
    end;
handle_call({fetch, ChId, PubKey}, _From, St) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch_cache{state = State}] ->
            {reply, {ok, State}, St};
        [] ->
            {reply, error, St}
    end;
handle_call({min_depth_achieved, ChId, _Watcher}, _From, St) ->
    lager:debug("min_depth_achieved - ~p gone", [ChId]),
    {reply, ok, delete_all_for_child(ChId, St)};
handle_call({cache_status, ChId}, _From, St) ->
    {reply, cache_status_(ChId), St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({update, ChId, PubKey, State}, St) ->
    ets:update_element(?TAB, key(ChId, PubKey), {#ch_cache.state, State}),
    {noreply, St};
handle_cast({delete, ChId}, St) ->
    {noreply, delete_all_for_child(ChId, St)};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', MRef, process, _Pid, _Reason}, St) ->
    case lookup_by_mref(MRef, St) of
        {ChId, PubKey} = CacheId ->
            move_state_to_persistent(CacheId),
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
handle_info(Msg, St) ->
    lager:debug("Got ~p", [Msg]),
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

-spec key(aeser_id:val(), aeser_id:val()) -> ch_cache_id().
key(ChId, PubKey) ->
    {ChId, PubKey}.

cache_status_(ChId) ->
    InRam = ets:select(
        ?TAB, [{ #ch_cache{cache_id = key(ChId, '$1'), _ = '_'}, [], ['$1']}]),
    OnDisk = mnesia:dirty_select(
        ?PTAB, [{#pch_cache{cache_id = key(ChId, '$1'), _ = '_'}, [], ['$1']}]),
    [ {in_ram, InRam}
    , {on_disk, OnDisk}].

try_reestablish_cached(ChId, PubKey, Password) ->
    CacheId = key(ChId, PubKey),
    case ets:lookup(?TAB, CacheId) of
        [#ch_cache{state = State}] ->
            {ok, State};
        [] ->
            case read_persistent(CacheId) of
                {ok, Encrypted} ->
                    decrypt_state_and_move_to_ram(Encrypted, Password);
                error ->
                    {error, missing_state_trees}
            end
    end.

decrypt_with_key_and_move_to_ram(#pch_cache{ cache_id = CacheId
                                           , nonce = Nonce
                                           , encrypted_state = EncryptedState
                                           }, Cache, SessionKey) ->
    case enacl:secretbox_open(EncryptedState, Nonce, SessionKey) of
        {ok, SerializedState} ->
            State = aesc_offchain_state:deserialize_from_binary(SerializedState),
            ets:insert(
                    ?TAB, [Cache#ch_cache{cache_id = CacheId, state = State}]),
                    delete_persistent(CacheId),
                    {ok, State};
        {error, _} = Err ->
            Err
    end.

decrypt_state_and_move_to_ram(#pch_cache{salt = Salt} = PersistedCache, Password) ->
    case { generate_session_key(Password, Salt)
         , setup_keys(Password)} of
        {{ok, SessionKey}, {ok, Cache}} ->
            decrypt_with_key_and_move_to_ram(PersistedCache, Cache, SessionKey);
        {{error, _} = Err, _} ->
            Err;
        {_, {error, _} = Err} ->
            Err
    end.

move_state_to_persistent(CacheId) ->
    Found = ets:lookup(?TAB, CacheId),
    case Found of
        [] -> ok;
        [#ch_cache{cache_id = CacheId} = ChCache] ->
            encrypt_and_persist(ChCache)
    end,
    ets:delete(?TAB, CacheId),
    ok.

encrypt_and_persist(#ch_cache{ cache_id = CacheId
                             , salt =  Salt
                             , state =  State
                             , session_key = SessionKey}) ->
    Nonce = crypto:strong_rand_bytes(enacl:secretbox_nonce_size()),
    SerializedState = aesc_offchain_state:serialize_to_binary(State),
    Encrypted = #pch_cache{
        cache_id = CacheId,
        salt = Salt,
        nonce = Nonce,
        encrypted_state = enacl:secretbox(SerializedState, Nonce, SessionKey)
    },
    write_persistent(Encrypted).

write_persistent(Pch) ->
    activity(fun() -> mnesia:write(?PTAB, Pch, write) end).

read_persistent(CacheId) ->
    activity(fun() ->
                     case mnesia:read(?PTAB, CacheId) of
                         [#pch_cache{} = Encrypted] ->
                             {ok, Encrypted};
                         [] ->
                             error
                     end
             end).

delete_persistent(CacheId) ->
    activity(fun() -> mnesia:delete(?PTAB, CacheId, write) end).

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
    {ok, Pid} = aesc_chain_watcher:watch_for_channel_close(
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

delete_all_for_child(ChId, St) ->
    Res = delete_all_state_for(
            ChId, remove_watcher(
                    ChId, remove_monitor_by_chid(
                            ChId, St))),
    lager:debug("delete_for_chid ~p; cache_status: ~p",
                [ChId, cache_status_(ChId)]),
    Res.

delete_all_state_for(ChId, St) ->
    ets:select_delete(
      ?TAB, [{ #ch_cache{cache_id = key(ChId, '_'), _ = '_'}, [], [true] }]),

    activity(fun() ->
        Records = mnesia:select(
          ?PTAB, [{ #pch_cache{cache_id = key(ChId, '$1'), _ = '_'}, [], ['$1'] }]),
        [mnesia:delete(?PTAB, key(ChId, Pubkey), write) || Pubkey <- Records]
    end),
    St.
