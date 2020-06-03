%%%=============================================================================
%%% @copyright 2018-19, Aeternity Anstalt
%%% @doc
%%%    Module monitoring the FSMs used for SC. After each state update in a given FSM/SC
%%%    the offchain state is saved in ETS in order for the user to be able to leave and then
%%%    reestablish the channel. In case of an unexpected FSM crash the state is encrypted with
%%%    a token identifying the FSM and saved in persistent storage.
%%%    Please note that it is impossible to reestablish a channel without the state.
%%%    TODO: Protect the state in case of an unexpected power failure
%%%    TODO: for instance allow the user or a delegate to provide the missing state
%%%
%%%    Security details:
%%%    The randomness source is crypto:strong_rand_bytes/1
%%%    The FSM ID is used directly as an encryption key - the user must make sure that the ID
%%%    is transported to the client using a secure transport protocol.
%%%    Encryption and decryption is done by enacl:secretbox/3 which encrypts the provided data
%%%    with XSalsa20 and appends a Poly1305 MAC to the ciphertext.
%%%
%%%    Encryption can be summarized in pseudocode as:
%%%    Key        = rand_bytes(32)
%%%    Nonce      = rand_bytes(24)
%%%    Ciphertext = XSalsa20_Poly1305(serialize(ChannelState), Nonce, Key)
%%%
%%%    In the DB we persist the tuple (Ciphertext, Nonce). The FSM ID is thrown away
%%%    immediately after using it to encrypt the state. After a state is decrypted a
%%%    new FSM ID should be generated. In case of reconnecting to an already running FSM
%%%    the ID can be reused.
%%% @end
%%%=============================================================================
-module(aesc_state_cache).
-behaviour(gen_server).

-export([
          start_link/0
        , new/5
        , reestablish/3
        , change_fsm_id/3
        , authenticate_user/3
        , update/3
        , update/4
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
                       , aeser_id:val() | atom()}.
-type opts() :: map().

%% The split between `opts` and `dyn_opts` is so that we can write updates more
%% efficiently. The two are merged at read time, and when moved to persistent store.
%%
-record(ch_cache, { cache_id       :: ch_cache_id() | undefined
                  , state          :: aesc_offchain_state:state() | atom() | undefined
                  , opts = #{}     :: map() | '_'
                  , dyn_opts = #{} :: map() | '_'
                  , fsm_id_wrapper :: aesc_fsm_id:wrapper() | atom()
                  }).

-define(VSN_pch_encrypted_cache, 1).
-record(pch_encrypted_cache, { cache_id        :: ch_cache_id()
                             , nonce           :: binary() | atom()
                             , encrypted_state :: binary() | atom()
                             , opts = #{}      :: opts() | '_'         % unencrypted
                             }).

-define(SERVER, ?MODULE).
-define(TAB, aesc_state_cache_ch).
%% A new table name is needed in case the user wants to downgrade to a pre-lima release
-define(PTAB, aesc_state_cache_v2).

-spec new(aeser_id:val(), aeser_id:val(), aesc_offchain_state:state(), opts(), aesc_fsm_id:wrapper()) -> ok | {error, any()}.
new(ChId, PubKey, State, Opts, FsmIdWrapper) ->
    gen_server:call(?SERVER, {new, ChId, PubKey, self(), State, Opts, FsmIdWrapper}).

-spec reestablish(aeser_id:val(), aeser_id:val(), aesc_fsm_id:wrapper()) -> {ok, aesc_offchain_state:state(), opts()} | {error, atom()}.
reestablish(ChId, PubKey, FsmIdWrapper) ->
    gen_server:call(?SERVER, {reestablish, ChId, PubKey, self(), FsmIdWrapper}).

%% @doc We need to ensure that the fsm id is changed only when the fsm has sent the new ID to the
%% user. Otherwise we get a nasty race condition:
%% 1. The user reestablishes a channel
%% 2. The cache decrypts the state with the old token, the fsm generates a new token a changes it
%% 3. The fsm crashes before sending the token to the sc_ws_handler
%% 4. The cache encrypts the state with the new token which results in a undecryptable state
%% @end
-spec change_fsm_id(aeser_id:val(), aeser_id:val(), aesc_fsm_id:wrapper()) -> ok.
change_fsm_id(ChId, Pubkey, FsmIdWrapper) ->
    gen_server:call(?SERVER, {change_fsm_id, ChId, Pubkey, FsmIdWrapper}).

%% @doc
%% Authenticate the user using the provided token - this should be used for authentication
%% of reconnection requests
%% @end
-spec authenticate_user(aeser_id:val(), aeser_id:val(), aesc_fsm_id:wrapper()) -> boolean().
authenticate_user(ChId, Pubkey, FsmIdWrapper) ->
    gen_server:call(?SERVER, {authenticate_user, ChId, Pubkey, FsmIdWrapper}).

update(ChId, PubKey, State) ->
    update(ChId, PubKey, State, #{}).

update(ChId, PubKey, State, Opts) ->
    gen_server:cast(?SERVER, {update, ChId, PubKey, State, Opts}).

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

table_vsn(pch_encrypted_cache) -> ?VSN_pch_encrypted_cache.

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

handle_call({new, ChId, PubKey, Pid, State, Opts, FsmIdWrapper}, _From, St) ->
    case ets:insert_new(?TAB, #ch_cache{ cache_id = key(ChId, PubKey)
                                       , state = State
                                       , opts = Opts
                                       , fsm_id_wrapper = FsmIdWrapper}) of
        true ->
            St1 = monitor_fsm(Pid, ChId, PubKey, St),
            {reply, ok, St1};
        false ->
            {reply, {error, exists}, St}
    end;
handle_call({reestablish, ChId, PubKey, Pid, FsmIdWrapper}, _From, St) ->
    case try_reestablish_cached(ChId, PubKey, FsmIdWrapper) of
        {ok, _State, _Opts} = Result ->
            St1 = monitor_fsm(Pid, ChId, PubKey,
                              remove_watcher(ChId, St)),
            {reply, Result, St1};
        {error, _} = Error ->
            {reply, Error, St}
    end;
handle_call({change_fsm_id, ChId, PubKey, FsmIdWrapper}, _From, St) ->
    %% This shouldn't fail in normal usage but for tests let inform about errors
    case ets:update_element(?TAB, key(ChId, PubKey), {#ch_cache.fsm_id_wrapper, FsmIdWrapper}) of
        true ->
            {reply, ok, St};
        false ->
            {reply, {error, missing_state_trees}, St}
    end;
handle_call({authenticate_user, ChId, PubKey, FsmIdWrapper}, _From, St) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch_cache{fsm_id_wrapper = ExistingFsmIdWrapper}] ->
            {reply, aesc_fsm_id:compare(ExistingFsmIdWrapper, FsmIdWrapper), St};
        [] ->
            {reply, false, St}
    end;
handle_call({fetch, ChId, PubKey}, _From, St) ->
    case ets:lookup(?TAB, key(ChId, PubKey)) of
        [#ch_cache{state = State, opts = Opts, dyn_opts = DOpts}] ->
            {reply, {ok, State, maps:merge(Opts, DOpts)}, St};
        [] ->
            {reply, error, St}
    end;
handle_call({min_depth_achieved, ChId, _Watcher}, _From, St) ->
    lager:debug("min_depth_achieved - ~p gone", [ChId]),
    {reply, ok, delete_all_state_for_channel(ChId, St)};
handle_call({cache_status, ChId}, _From, St) ->
    {reply, cache_status_(ChId), St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({update, ChId, PubKey, State, Opts}, St) ->
    %% dyn_opts are always overwritten
    Key = key(ChId, PubKey),
    ets:update_element(?TAB, Key, [ {#ch_cache.state, State}
                                  , {#ch_cache.dyn_opts, Opts} ]),
    {noreply, St};
handle_cast({delete, ChId}, St) ->
    {noreply, delete_all_state_for_channel(ChId, St)};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', MRef, process, _Pid, _Reason}, St) ->
    lager:debug("Received 'DOWN' from ~p", [_Pid]),
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
                    lager:debug("starting watcher for ~p", [ChId]),
                    {noreply, start_watcher(ChId, St1)}
            end;
        error ->
            lager:debug("no state found for mref", []),
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

try_reestablish_cached(ChId, PubKey, FsmIdWrapper) ->
    CacheId = key(ChId, PubKey),
    case ets:lookup(?TAB, CacheId) of
        [#ch_cache{state = State, opts = Opts, dyn_opts = DOpts}] ->
            {ok, State, maps:merge(Opts, DOpts)};
        [] ->
            case read_persistent(CacheId) of
                {ok, Encrypted} ->
                    decrypt_with_key_and_move_to_ram(Encrypted, FsmIdWrapper);
                error ->
                    {error, missing_state_trees}
            end
    end.

decrypt_with_key_and_move_to_ram(#pch_encrypted_cache{ cache_id = CacheId
                                                     , nonce = Nonce
                                                     , encrypted_state = EncryptedState
                                                     , opts = Opts
                                                     }, FsmIdWrapper) ->
    case enacl:secretbox_open(EncryptedState, Nonce, aesc_fsm_id:retrieve(FsmIdWrapper)) of
        {ok, SerializedState} ->
            State = aesc_offchain_state:deserialize_from_binary(SerializedState),
            ets:insert(
                    ?TAB, [#ch_cache{cache_id = CacheId, state = State, opts = Opts, fsm_id_wrapper = FsmIdWrapper}]),
                    delete_persistent(CacheId),
                    {ok, State, Opts};
        {error, _} ->
            {error, invalid_fsm_id}
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
                       , state =  State
                       , opts = Opts
                       , dyn_opts = DOpts
                       , fsm_id_wrapper = FsmIdWrapper}) ->
    Nonce = crypto:strong_rand_bytes(enacl:secretbox_nonce_size()),
    SerializedState = aesc_offchain_state:serialize_to_binary(State),
    #pch_encrypted_cache{
        cache_id = CacheId,
        nonce = Nonce,
        opts = maps:merge(Opts, DOpts),
        encrypted_state = enacl:secretbox(
            SerializedState,
            Nonce,
            aesc_fsm_id:retrieve(FsmIdWrapper))
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
    Parent = self(),
    Pid = proc_lib:spawn_link(
            fun() ->
                    ok = aesc_chain_watcher:register(
                           ChId, ?MODULE, [aesc_chain_watcher:close_req(Min)]),
                    watcher_loop(Parent, ChId)
            end),
    %% {ok, Pid} = aesc_chain_watcher:watch_for_channel_close(
    %%               ChId, Min, ?MODULE),
    lager:debug("watcher started for ~p: ~p", [ChId, Pid]),
    ets:insert(Ws, {ChId, Pid}),
    St.

watcher_loop(Parent, ChId) ->
    %% Basically, we only need some receive clause to linger in,
    %% keeping the watcher pid alive. The callbacks result in messages
    %% directly to the state cache process.
    receive
        {Parent, die} ->
            %% In remove_watcher/2 below, we use exit(Pid, kill) instead
            lager:debug("Received 'die' (ChId=~p)", [ChId]),
            Parent ! {self(), ok},
            ok  % process will terminate
    end.

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
