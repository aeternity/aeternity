%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module monitoring the FSM's used for SC. After each state update in a given FSM/SC
%%%    the offchain state is saved in ETS in order for the user to be able to leave and then reestablish
%%%    the channel. In case of an unexpected FSM crash the state is encrypted with an user provided password
%%%    and saved in persistent storage. Please note that it is impossible to reestablish a channel without the state.
%%%    TODO: Protect the state in case of an unexpected power failure - for instance allow the user or a delegate to provide the missing state
%%% @end
%%%=============================================================================
-module(aesc_state_cache).
-behaviour(gen_server).

-export([
          start_link/0
        , new/3
        , new/4
        , reestablish/2
        , reestablish/3
        , change_state_password/3
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
        , migrate/2
        ]).

%% for diagnostics
-export([
         cache_status/1
        ]).

%% for mocking
-ifdef(TEST).
-export([
         mock_kdf_init/0,
         mock_kdf_end/0
        ]).
-endif.

-define(MIN_DEPTH, 4).
-record(st, {mons_chid = ets:new(mons_chid, [ordered_set]),
             mons_ref  = ets:new(mons_ref, [set]),
             watchers  = ets:new(watchers, [set]),
             min_depth = ?MIN_DEPTH}).
-type ch_cache_id() :: { aeser_id:val()
                       , aeser_id:val() | atom()}.
-record(ch_cache, { cache_id    :: ch_cache_id() | undefined
                  , state       :: aesc_offchain_state:state() | atom() | undefined
                  , salt        :: binary() | atom()
                  , session_key :: binary() | atom()
                  }).

-define(VSN_pch, 1).
-record(pch, {id, pubkeys = [], state}).
-define(VSN_pch_encrypted_cache, 2).
-record(pch_encrypted_cache, { cache_id        :: ch_cache_id()
                             , salt            :: binary() | atom()
                             , nonce           :: binary() | atom()
                             , encrypted_state :: binary() | atom()
                             }).

-define(SERVER, ?MODULE).
-define(TAB, aesc_state_cache_ch).
-define(PTAB, aesc_state_cache).
-define(CACHE_DEFAULT_PASSWORD, "correct horse battery staple").

-spec new(aeser_id:val(), aeser_id:val(), aesc_offchain_state:state()) -> ok | {error, any()}.
new(ChId, Pubkey, State) ->
    new(ChId, Pubkey, State, ?CACHE_DEFAULT_PASSWORD).

-spec new(aeser_id:val(), aeser_id:val(), aesc_offchain_state:state(), string()) -> ok | {error, any()}.
new(ChId, PubKey, State, Password) ->
    gen_server:call(?SERVER, {new, ChId, PubKey, self(), State, unicode:characters_to_binary(Password)}).

-spec reestablish(aeser_id:val(), aeser_id:val()) -> {ok, aesc_offchain_state:state()} | {error, atom()}.
reestablish(ChId, Pubkey) ->
    reestablish(ChId, Pubkey, ?CACHE_DEFAULT_PASSWORD).

-spec reestablish(aeser_id:val(), aeser_id:val(), string()) -> {ok, aesc_offchain_state:state()} | {error, atom()}.
reestablish(ChId, PubKey, Password) ->
    gen_server:call(?SERVER, {reestablish, ChId, PubKey, self(), unicode:characters_to_binary(Password)}).

change_state_password(ChId, Pubkey, Password) ->
    gen_server:call(?SERVER, {change_state_password, ChId, Pubkey, unicode:characters_to_binary(Password)}).

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
               , {record_name, pch_encrypted_cache}
               , {attributes, record_info(fields, pch_encrypted_cache)}
               , {user_properties, [{vsn, table_vsn(pch_encrypted_cache)}]}
               ]}
    ].

check_tables(Acc) ->
    lists:foldl(
      fun({Tab, Spec}, Acc1) ->
              aec_db:check_table(Tab, Spec, Acc1)
      end, Acc, table_specs(disc)).

table_vsn(pch) -> ?VSN_pch;
table_vsn(pch_encrypted_cache) -> ?VSN_pch_encrypted_cache.

migrate(?VSN_pch, ?VSN_pch_encrypted_cache) ->
    %%TODO: Move the migration logic partially to aec_db - this is only here because this is the first migration in the entire codebase :)
    ?VSN_pch = table_vsn(pch),
    ?VSN_pch_encrypted_cache = table_vsn(pch_encrypted_cache),
    lager:debug("Waiting for state cache table to be accesible", []),
    case mnesia:wait_for_tables([?PTAB], 10000) of
        ok ->
            lager:debug("State cache table is loaded", []);
        _ ->
            lager:debug("Timeout while waiting for state cache table to be loaded", []),
            erlang:exit(load_timeout)
    end,
    %% http://erlang.org/pipermail/erlang-questions/2009-May/044106.html
    %% I need to use mnesia_schema because the typespec for mnesia:transform_table/4 is broken
    {atomic, ok} = mnesia_schema:transform_table(?PTAB,
                                          ignore,
                                          record_info(fields, pch_encrypted_cache),
                                          pch_encrypted_cache),
    activity(fun do_migrate/0),
    {atomic, ok} = mnesia:write_table_property(?PTAB, {vsn, table_vsn(pch_encrypted_cache)}),
    ok.

do_migrate() ->
    FirstKey = mnesia:first(?PTAB),
    do_migrate_(FirstKey).

do_migrate_('$end_of_table') -> ok;
do_migrate_(Key) ->
    case mnesia:read(?PTAB, Key) of
        [#pch{} = OldRecord] ->
            ToWrite = transform_record_to_encrypted_form_with_default_password(OldRecord),
            [ok = mnesia:write(?PTAB, NewRecord, write) || NewRecord <- ToWrite],
            ok = mnesia:delete(?PTAB, Key, write);
        _ ->
            ok
    end,
    do_migrate_(mnesia:next(?PTAB, Key)).

transform_record_to_encrypted_form_with_default_password(#pch{id = ChId, pubkeys = [PubKey], state = State}) ->
    {ok, Cache0} = setup_keys_in_cache(?CACHE_DEFAULT_PASSWORD),
    Cache1 = Cache0#ch_cache{cache_id = key(ChId, PubKey), state = State},
    [encrypt_cache(Cache1)];
transform_record_to_encrypted_form_with_default_password(#pch{pubkeys = [Pub1, Pub2]} = Record) ->
    Records = [Record#pch{pubkeys = [Pub1]}, Record#pch{pubkeys = [Pub2]}],
    lists:flatten([transform_record_to_encrypted_form_with_default_password(Record) || Record <- Records]).

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
    %% Argon2
    enacl:pwhash(Password, Salt).

setup_keys_in_cache(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    case generate_session_key(Password, Salt) of
        {ok, SessionKey} ->
            {ok, #ch_cache{salt = Salt, session_key = SessionKey}};
        {error, _} = Err ->
            Err
    end.

handle_call({new, ChId, PubKey, Pid, State, Password}, _From, St) ->
    Resp = case setup_keys_in_cache(Password) of
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
    end,
    garbage_collect(),
    Resp;
handle_call({reestablish, ChId, PubKey, Pid, Password}, _From, St) ->
    Resp = case try_reestablish_cached(ChId, PubKey, Password) of
        {ok, Result} ->
            St1 = monitor_fsm(Pid, ChId, PubKey,
                              remove_watcher(ChId, St)),
            {reply, {ok, Result}, St1};
        {error, _} = Error ->
            {reply, Error, St}
    end,
    garbage_collect(),
    Resp;
handle_call({fetch, ChId, PubKey}, _From, St) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch_cache{state = State}] ->
            {reply, {ok, State}, St};
        [] ->
            {reply, error, St}
    end;
handle_call({change_state_password, ChId, Pubkey, Password}, _From, St) ->
    lager:debug("Changing state password"),
    Resp = case setup_keys_in_cache(Password) of
        {ok, #ch_cache{salt = Salt, session_key = SessionKey}} ->
            case ets:update_element(?TAB, key(ChId, Pubkey),
                [ {#ch_cache.salt, Salt}
                , {#ch_cache.session_key, SessionKey}]) of
                true ->
                    {reply, ok, St};
                false ->
                    {reply, {error, missing_state_trees}, St}
            end;
        {error, _} = Err ->
            {reply, Err, St}
    end,
    garbage_collect(),
    Resp;
handle_call({min_depth_achieved, ChId, _Watcher}, _From, St) ->
    lager:debug("min_depth_achieved - ~p gone", [ChId]),
    {reply, ok, delete_all_state_for_channel(ChId, St)};
handle_call({cache_status, ChId}, _From, St) ->
    {reply, cache_status_(ChId), St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({update, ChId, PubKey, State}, St) ->
    ets:update_element(?TAB, key(ChId, PubKey), {#ch_cache.state, State}),
    {noreply, St};
handle_cast({delete, ChId}, St) ->
    {noreply, delete_all_state_for_channel(ChId, St)};
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

-spec key(aeser_id:val(), aeser_id:val() | atom()) -> ch_cache_id().
key(ChId, PubKey) ->
    {ChId, PubKey}.

cache_status_(ChId) ->
    InRam = ets:select(
        ?TAB, [{ #ch_cache{cache_id = key(ChId, '$1'), _ = '_'}, [], ['$1']}]),
    OnDisk = mnesia:dirty_select(
        ?PTAB, [{#pch_encrypted_cache{cache_id = key(ChId, '$1'), _ = '_'}, [], ['$1']}]),
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

decrypt_with_key_and_move_to_ram(#pch_encrypted_cache{ cache_id = CacheId
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
        {error, _} ->
            {error, invalid_password}
    end.

decrypt_state_and_move_to_ram(#pch_encrypted_cache{salt = Salt} = PersistedCache, Password) ->
    %% For security reasons we use a new salt
    case { generate_session_key(Password, Salt)
         , setup_keys_in_cache(Password)} of
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

encrypt_and_persist(Cache) ->
    Encrypted = encrypt_cache(Cache),
    write_persistent(Encrypted).

encrypt_cache(#ch_cache{ cache_id = CacheId
                       , salt =  Salt
                       , state =  State
                       , session_key = SessionKey}) ->
    Nonce = crypto:strong_rand_bytes(enacl:secretbox_nonce_size()),
    SerializedState = aesc_offchain_state:serialize_to_binary(State),
    #pch_encrypted_cache{
        cache_id = CacheId,
        salt = Salt,
        nonce = Nonce,
        encrypted_state = enacl:secretbox(SerializedState, Nonce, SessionKey)
    }.

write_persistent(Pch) ->
    activity(fun() -> mnesia:write(?PTAB, Pch, write) end).

read_persistent(CacheId) ->
    activity(fun() ->
                     case mnesia:read(?PTAB, CacheId) of
                         [#pch_encrypted_cache{} = Encrypted] ->
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

delete_all_state_for_channel(ChId, St) ->
    Res = delete_offchain_state_for_channel(
            ChId, remove_watcher(
                    ChId, remove_monitor_by_chid(
                            ChId, St))),
    lager:debug("delete_for_chid ~p; cache_status: ~p",
                [ChId, cache_status_(ChId)]),
    Res.

delete_offchain_state_for_channel(ChId, St) ->
    ets:select_delete(
      ?TAB, [{ #ch_cache{cache_id = key(ChId, '_'), _ = '_'}, [], [true] }]),

    activity(fun() ->
        Records = mnesia:select(
          ?PTAB, [{ #pch_encrypted_cache{cache_id = key(ChId, '$1'), _ = '_'}, [], ['$1'] }]),
        [mnesia:delete(?PTAB, key(ChId, Pubkey), write) || Pubkey <- Records]
    end),
    St.

-ifdef(TEST).
enacl_pwhash_mock(Password, Salt) ->
    {ok, crypto:hash(sha256, erlang:iolist_to_binary([Password, Salt]))}.

mock_kdf_init() ->
    %% In production we are using Argon2 for KDF - evaluating Argon2 takes > 100ms and is expensive
    %% Mock the KDF to use sha256 in order to speed up tests
    lager:debug("Mocking KDF in state cache"),
    ok = meck:new(enacl, [no_link, no_history, passthrough]),
    ok = meck:expect(enacl, pwhash, fun enacl_pwhash_mock/2),
    ok.

mock_kdf_end() ->
    lager:debug("Unloading KDF mock"),
    ok = meck:unload(enacl),
    ok.
-endif.
