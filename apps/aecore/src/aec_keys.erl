%%%-------------------------------------------------------------------
%%% @author Sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%     Key manager for AE node
%%%     * it will open keys that it finds using sys.config
%%%
%%%     The gen_server aec_keys is the keeper of the signing and the
%%%     peer keypairs for the miner. To avoid leaking the sensitive
%%%     information (password, private key, etc) a separate worker
%%%     process handles all things that need this information.
%%%
%%%     The worker process is linked to the server process, but traps
%%%     exits, performs all operations in try...catch etc to avoid
%%%     crash dumps. The worker process is flagged as 'sensitive',
%%%     meaning that it cannot be traced, it will not be visible in
%%%     crash dumps, etc.
%%%
%%%
%%% @end Created : 28 Aug 2017
%%%-------------------------------------------------------------------
-module(aec_keys).

-behaviour(gen_server).

%% API
-export([peer_pubkey/0,
         peer_privkey/0,
         pubkey/0,
         candidate_pubkey/0,
         promote_candidate/1,
         sign_micro_block/1
        ]).

%% Supervisor API
-export([start_link/0,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% System test API
-export([peer_key_filenames/1,
         save_peer_keys/4,
         setup_peer_keys/2
        ]).

%% Test API
-ifdef(TEST).
-export([check_peer_keys/2,
         check_sign_keys/2,
         encrypt_key/2,
         sign_privkey/0,
         start_link/1
        ]).
-endif.

-define(SERVER, ?MODULE).

-define(PUB_SIZE, 32).
-define(SIGN_PRIV_SIZE, 64).
-define(PEER_PRIV_SIZE, 32).

-define(FILENAME_PEERPUB, "peer_key.pub").
-define(FILENAME_PEERPRIV, "peer_key").

-record(state, {
          worker_pid     :: pid(),
          candidate_pub  :: binary(),
          sign_pub       :: binary(),
          peer_pub       :: binary()
         }).

-record(worker_state, {
          parent_pid     :: pid(),
          peer_pass      :: password(),
          keys_dir       :: binary(),

          candidate_pub  :: binary() | undefined,
          candidate_priv :: binary() | undefined,
          sign_pub       :: binary() | undefined,
          sign_priv      :: binary() | undefined,
          peer_pub       :: binary() | undefined,
          peer_priv      :: binary() | undefined,
          peer_pub_file  :: binary() | undefined,
          peer_priv_file :: binary() | undefined
         }).

-type password() :: binary().
-type pubkey()   :: <<_:256>>. %% 256 = 32 * 8
-type privkey()  :: <<_:512>>. %% 512 = 64 * 8

-type block() :: aec_blocks:block().

-export_type([privkey/0, pubkey/0]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").


%%%===================================================================
%%% API
%%%===================================================================

-spec sign_micro_block(block()) -> {ok, block()} | {error, term()}.
sign_micro_block(MicroBlock) ->
    Header = aec_blocks:to_micro_header(MicroBlock),
    Bin = aec_headers:serialize_to_signature_binary(Header),
    {ok, Signature} = gen_server:call(?MODULE, {sign, Bin}),
    {ok, aec_blocks:set_signature(MicroBlock, Signature)}.

-spec pubkey() -> {ok, binary()} | {error, key_not_found}.
pubkey() ->
    gen_server:call(?MODULE, pubkey).

-spec candidate_pubkey() -> {ok, binary()} | {error, key_not_found}.
candidate_pubkey() ->
    gen_server:call(?MODULE, candidate_pubkey).

-spec peer_pubkey() -> {ok, binary()} | {error, key_not_found}.
peer_pubkey() ->
    gen_server:call(?MODULE, peer_pubkey).

-spec peer_privkey() -> {ok, binary()} | {error, key_not_found}.
peer_privkey() ->
    gen_server:call(?MODULE, peer_privkey).

-spec promote_candidate(pubkey()) -> ok | {error, key_not_found}.
promote_candidate(PubKey) ->
    gen_server:call(?MODULE, {promote_candidate, PubKey}).

%%%===================================================================
%%% Test API
%%%===================================================================

-ifdef(TEST).
-spec sign_privkey() -> {ok, binary()} | {error, key_not_found}.
sign_privkey() ->
    gen_server:call(?MODULE, privkey).
-endif.

%% Used from system tests
save_peer_keys(Password, KeysDir, PubKey, PrivKey) ->
    case check_peer_keys(PubKey, PrivKey) of
        true ->
            {PeerPubFile, PeerPrivFile} = p_gen_peer_filename(KeysDir),
            {EncPub, EncPriv} =
                p_save_keys(Password, PeerPubFile, PubKey,
                            PeerPrivFile, PrivKey),
            {PeerPubFile, EncPub, PeerPrivFile, EncPriv};
        false ->
            error({key_pair_check_failed, [PubKey, PrivKey]})
    end.

peer_key_filenames(KeysDir) -> p_gen_peer_filename(KeysDir).

%%%===================================================================
%%% Gen server API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-ifdef(TEST).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

-endif.

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

-ifdef(TEST).

init([]) ->
    lager:info("Initializing keys manager"),
    WorkerPid = start_worker(),
    receive
        {WorkerPid, pubkeys, {SignPub, CPub, PeerPub}} ->
            {ok, #state{sign_pub = SignPub,
                        candidate_pub = CPub,
                        peer_pub = PeerPub,
                        worker_pid = WorkerPid
                        }}
    end;
init([Pwd, KeysDir]) ->
    %% Test interface
    lager:info("Initializing keys manager"),
    WorkerPid = start_worker(Pwd, KeysDir),
    receive
        {WorkerPid, pubkeys, {SignPub, CPub, PeerPub}} ->
            {ok, #state{sign_pub = SignPub,
                        candidate_pub = CPub,
                        peer_pub = PeerPub,
                        worker_pid = WorkerPid
                        }}
    end.

-else.

init([]) ->
    lager:info("Initializing keys manager"),
    WorkerPid = start_worker(),
    receive
        {WorkerPid, pubkeys, {SignPub, CPub, PeerPub}} ->
            {ok, #state{sign_pub = SignPub,
                        candidate_pub = CPub,
                        peer_pub = PeerPub,
                        worker_pid = WorkerPid
                        }}
    end.

-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({sign, Bin} = Msg, From, State) ->
    case is_binary(Bin) of
        true  ->
            call_worker(Msg, From, State),
            {noreply, State};
        false ->
            lager:debug("Illegal sign request: ~p", [Bin]),
            {reply, {error, can_only_sign_binaries}, State}
    end;
handle_call(pubkey, _From, #state{sign_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(privkey, From, State) ->
    call_worker_if_test(privkey, From, State),
    {noreply, State};
handle_call(candidate_pubkey, _From, #state{candidate_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(peer_pubkey,_From, #state{peer_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(peer_privkey, From, State) ->
    call_worker(peer_privkey, From, State),
    {noreply, State};
handle_call({promote_candidate, CPubKey} = Msg, From, #state{candidate_pub=CPubKey} = State) ->
    call_worker(Msg, From, State),
    {noreply, State};
handle_call({promote_candidate, _},_From, State) ->
    {reply, {error, key_not_found}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({WorkerPid, promoted_candidate, {SignPub, CPub}}, #state{worker_pid = WorkerPid} = State) ->
    {noreply, State#state{sign_pub = SignPub, candidate_pub = CPub}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Worker
%%%===================================================================

call_worker(Msg, From, #state{worker_pid = Worker}) ->
    Worker ! {self(), From, Msg}.

-ifdef(TEST).

call_worker_if_test(Msg, From, #state{worker_pid = Worker}) ->
    Worker ! {self(), From, Msg}.

-else.

-spec call_worker_if_test(term(), pid(), #state{}) -> no_return().
call_worker_if_test(Msg,_From, #state{}) ->
    error({not_in_test, Msg}).

-endif.

start_worker() ->
    Parent = self(),
    Fun = fun() ->
                  process_flag(sensitive, true), %% Protect against tracing etc
                  process_flag(trap_exit, true),
                  #{ keys_dir := KeysDir , peer_password := PeerPwd } = check_env(),
                  enter_worker(Parent, PeerPwd, KeysDir)
          end,
    spawn_link(Fun).

-ifdef(TEST).

start_worker(PeerPwd, KeysDir) ->
    Parent = self(),
    Fun = fun() ->
                  %% NOTE: This is only used in tests: omit the sensitive flag
                  process_flag(trap_exit, true),
                  enter_worker(Parent, PeerPwd, KeysDir)
          end,
    spawn_link(Fun).
-endif.

enter_worker(Parent, PeerPwd, KeysDir) ->
    try worker_init(Parent, PeerPwd, KeysDir) of
        {ok, State} ->
            parent_state_update(pubkeys, State),
            worker_loop(State)
    ?_catch_(Type, What, StackTrace)
        lager:debug("Error starting worker: ~p", [{Type, What, StackTrace}]),
        lager:error("aec_keys worker_failed"),
        error(init_failed)
    end.

worker_init(Parent, PeerPwd, KeysDir) ->
    %% Ensure there is directory for keys
    ensure_dir(KeysDir),

    State0 = #worker_state{ parent_pid     = Parent
                          , peer_pass      = PeerPwd
                          , keys_dir       = KeysDir },

    State1 = s_setup_sign_keys(State0),
    State2 = s_setup_candidate_keys(State1),
    State3 = s_setup_peer_keys(State2),
    {ok, State3}.

worker_loop(#worker_state{ parent_pid = Parent } = State) ->
    receive
        {'EXIT', Parent, _} -> ok;
        {Parent, From, Msg} ->
            {ok, NewState} = worker_handle_message(Msg, From, State),
            worker_loop(NewState);
        Other ->
            lager:debug("Worker got unexpected: ~p", [Other]),
            worker_loop(State)
    end.

worker_handle_message({sign, Bin}, From, State) when is_binary(Bin) ->
    try enacl:sign_detached(Bin, State#worker_state.sign_priv) of
        Signature -> worker_reply(From, {ok, Signature}, State)
    catch
        _Type:_What -> worker_reply(From, {error, failed_sign}, State)
    end;
worker_handle_message(privkey, From, #worker_state{sign_priv=PrivKey} = State) ->
    worker_reply_if_test(From, {ok, PrivKey}, State);
worker_handle_message(peer_privkey, From, #worker_state{peer_priv=PrivKey} = State) ->
    worker_reply(From, {ok, PrivKey}, State);
worker_handle_message({promote_candidate, CPubKey}, From,
                      #worker_state{candidate_pub=CPubKey} = State0) ->
    State1 = s_promote_candidate(State0),
    State2 = parent_state_update(promoted_candidate, State1),
    worker_reply(From, ok, State2);
worker_handle_message({promote_candidate, _}, From, State) ->
    worker_reply(From, {error, key_not_found}, State);
worker_handle_message(Msg, From, State) ->
    worker_reply(From, {error, illegal_query, Msg}, State).

worker_reply(From, Reply, State) ->
    gen_server:reply(From, Reply),
    {ok, State}.

parent_state_update(pubkeys, State) ->
    #worker_state{ sign_pub = SP, candidate_pub = CP,
                   peer_pub = PP, parent_pid = Parent } = State,
    Parent ! {self(), pubkeys, {SP, CP, PP}},
    State;
parent_state_update(promoted_candidate, State) ->
    #worker_state{ sign_pub = SP, candidate_pub = CP,
                   parent_pid = Parent} = State,
    Parent ! {self(), promoted_candidate, {SP, CP}},
    State.

-ifdef(TEST).

worker_reply_if_test(From, Reply, State) ->
    worker_reply(From, Reply, State).

-else.

worker_reply_if_test(From,_Reply, State) ->
    worker_reply(From, {error, not_in_test_mode}, State).

-endif.


%%%===================================================================
%%% Internal functions
%%%===================================================================

s_promote_candidate(#worker_state{candidate_priv = Priv, candidate_pub  = Pub} = State) ->
    s_setup_candidate_keys( State#worker_state{ sign_priv = Priv, sign_pub = Pub } ).

s_setup_sign_keys(State) ->
    case p_gen_new_keypair() of
        {ok, {Pub, Priv}} -> State#worker_state{ sign_priv = Priv , sign_pub = Pub };
        {error, Reason} -> error(Reason)
    end.

s_setup_candidate_keys(State) ->
    case p_gen_new_keypair() of
        {ok, {Pub, Priv}} -> State#worker_state{ candidate_priv = Priv, candidate_pub = Pub };
        {error, Reason} -> error(Reason)
    end.

s_setup_peer_keys(#worker_state{peer_pass = PeerPwd, keys_dir = KeysDir} = State) ->
    {PeerPubFile, PeerPub, PeerPrivFile, PeerPriv} = setup_peer_keys(PeerPwd, KeysDir),
    State#worker_state{ peer_priv      = PeerPriv
                      , peer_pub       = PeerPub
                      , peer_priv_file = PeerPrivFile
                      , peer_pub_file  = PeerPubFile }.

setup_peer_keys(Pwd, KeysDir) ->
    {PubFile, PrivFile} = p_gen_peer_filename(KeysDir),
    case read_keys(Pwd, PubFile, PrivFile, ?PUB_SIZE, ?PEER_PRIV_SIZE) of
        {error, enoent} ->
            p_gen_new_peer(Pwd, PubFile, PrivFile);
        {Pub, Priv} ->
            case check_peer_keys(Pub, Priv) of
                true  ->
                    {PubFile, Pub, PrivFile, Priv};
                false ->
                    erlang:error({invalid_peer_key_pair, [PubFile, PrivFile]})
            end
    end.

read_keys(Pwd, PubFile, PrivFile, PubSize, PrivSize) ->
    case {from_local_dir(PubFile), from_local_dir(PrivFile)} of
        {{ok, EPub}, {ok, EPriv}} ->
            Pub = decrypt_key(Pwd, EPub, PubSize),
            Priv = decrypt_key(Pwd, EPriv, PrivSize),
            {Pub, Priv};
        _ ->
            {error, enoent}
    end.

hash(Bin) ->
    crypto:hash(sha256, Bin).

%% INFO: keep separate APIs and encrypt both priv & pub to protect external HDs
%%       (there is known atack vector using master pub)
encrypt_key(Password, Bin) ->
    crypto:block_encrypt(aes_ecb, hash(Password),  Bin).

decrypt_key(Password, Bin, Size) ->
    <<Key:Size/binary>> = crypto:block_decrypt(aes_ecb, hash(Password), Bin),
    Key.

p_gen_new_keypair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    case check_sign_keys(PubKey, PrivKey) of
        true  -> {ok, {PubKey, PrivKey}};
        false -> {error, {generated_key_pair_check_failed, [PubKey, PrivKey]}}
    end.

p_gen_new_peer(Pwd, PubFile, PrivFile) ->
    #{ public := SignPubKey, secret := SignPrivKey } = enacl:sign_keypair(),
    PubKey  = enacl:crypto_sign_ed25519_public_to_curve25519(SignPubKey),
    PrivKey = enacl:crypto_sign_ed25519_secret_to_curve25519(SignPrivKey),
    case check_peer_keys(PubKey, PrivKey) of
        true ->
            p_save_keys(Pwd, PubFile, PubKey, PrivFile, PrivKey),
            {PubFile, PubKey, PrivFile, PrivKey};
        false ->
            error({generated_key_pair_check_failed, [PubKey, PrivKey]})
    end.

ensure_dir(KeysDir) ->
    case filelib:is_dir(KeysDir) of
        false ->
            KeysDirFile = filename:join(KeysDir, "keyfile"),
            ok = filelib:ensure_dir(KeysDirFile);
        true ->
            ok
    end.

from_local_dir(NewFile) ->
    file:read_file(NewFile).

check_env() ->
    DefaultFile = filename:join(aeu_env:data_dir(aecore), "keys"),
    KeysDir = aeu_env:user_config_or_env([<<"keys">>, <<"dir">>],
                                         aecore, keys_dir, DefaultFile),
    PeerPwd = aeu_env:user_config_or_env([<<"keys">>, <<"peer_password">>],
                                         aecore, peer_password, <<>>),
    #{keys_dir => KeysDir, peer_password => PeerPwd}.

p_gen_peer_filename(KeysDir) ->
    p_gen_filename(KeysDir, ?FILENAME_PEERPUB, ?FILENAME_PEERPRIV).

p_gen_filename(KeysDir, PubFile0, PrivFile0) ->
    PubFile = filename:join(KeysDir, PubFile0),
    PrivFile = filename:join(KeysDir, PrivFile0),
    {PubFile, PrivFile}.

p_save_keys(Pwd, PubFile, PubKey, PrivFile, PrivKey) ->
    %% We only get here after reading the file. If it already exists at this point
    %% the user must have set write but not read permissions or so.
    EncPub = encrypt_key(Pwd, PubKey),
    EncPriv = encrypt_key(Pwd, PrivKey),
    ok = file:write_file(PubFile, EncPub),
    ok = file:write_file(PrivFile, EncPriv),
    {EncPub, EncPriv}.

check_sign_keys(PubKey, PrivKey) ->
    SampleMsg = <<"random message">>,
    Signature = enacl:sign_detached(SampleMsg, PrivKey),
    {ok, SampleMsg} == enacl:sign_verify_detached(Signature, SampleMsg, PubKey).

check_peer_keys(PubKey, PrivKey) ->
    PubKey == enacl:curve25519_scalarmult_base(PrivKey).
