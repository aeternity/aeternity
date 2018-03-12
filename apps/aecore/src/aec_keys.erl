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
         sign/1
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

-define(FILENAME_SIGNPUB, "sign_key.pub").
-define(FILENAME_SIGNPRIV, "sign_key").

-define(FILENAME_PEERPUB, "peer_key.pub").
-define(FILENAME_PEERPRIV, "peer_key").

-record(state, {
          worker_pid     :: pid(),
          sign_pub       :: binary(),
          peer_pub       :: binary()
         }).

-record(worker_state, {
          sign_pub       :: binary(),
          sign_priv      :: binary(),
          sign_pub_file  :: binary(),
          sign_priv_file :: binary(),
          sign_pass      :: password(),
          peer_pub       :: binary(),
          peer_priv      :: binary(),
          peer_pub_file  :: binary(),
          peer_priv_file :: binary(),
          peer_pass      :: password(),
          keys_dir       :: binary()
         }).

-type password() :: binary().
-type pubkey()   :: <<_:256>>. %% 256 = 32 * 8
-type privkey()  :: <<_:512>>. %% 512 = 64 * 8

-type tx() :: aetx:tx().
-type signed_tx() :: aetx_sign:signed_tx().

-export_type([privkey/0, pubkey/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec sign(tx()) -> {ok, signed_tx()} | {error, term()}.
sign(Bin) when is_binary(Bin) ->
    {ok, Signature} = gen_server:call(?MODULE, {sign, Bin}),
    {ok, aetx_sign:new(Bin, [Signature])};
sign(Tx) ->
    %% Serialize first to maybe (hopefully) pass as reference.
    Bin = aetx:serialize_to_binary(Tx),
    {ok, Signature} = gen_server:call(?MODULE, {sign, Bin}),
    {ok, aetx_sign:new(Tx, [Signature])}.

-spec pubkey() -> {ok, binary()} | {error, key_not_found}.
pubkey() ->
    gen_server:call(?MODULE, pubkey).

-spec peer_pubkey() -> {ok, binary()} | {error, key_not_found}.
peer_pubkey() ->
    gen_server:call(?MODULE, peer_pubkey).

-spec peer_privkey() -> {ok, binary()} | {error, key_not_found}.
peer_privkey() ->
    gen_server:call(?MODULE, peer_privkey).

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
        {WorkerPid, pubkeys, SignPub, PeerPub} ->
            {ok, #state{sign_pub = SignPub,
                        peer_pub = PeerPub,
                        worker_pid = WorkerPid
                        }}
    end;
init([Pwd, KeysDir]) ->
    %% Test interface
    lager:info("Initializing keys manager"),
    WorkerPid = start_worker(Pwd, KeysDir),
    receive
        {WorkerPid, pubkeys, SignPub, PeerPub} ->
            {ok, #state{sign_pub = SignPub,
                        peer_pub = PeerPub,
                        worker_pid = WorkerPid
                        }}
    end.

-else.

init([]) ->
    lager:info("Initializing keys manager"),
    WorkerPid = start_worker(),
    receive
        {WorkerPid, pubkeys, SignPub, PeerPub} ->
            {ok, #state{sign_pub = SignPub,
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
handle_call(peer_pubkey,_From, #state{peer_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(peer_privkey, From, State) ->
    call_worker(peer_privkey, From, State),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
                  #{ keys_dir := KeysDir
                   , password := SignPwd
                   , peer_password := PeerPwd } = check_env(),
                  enter_worker(Parent, SignPwd, PeerPwd, KeysDir)
          end,
    spawn_link(Fun).

-ifdef(TEST).

start_worker(Pwd, KeysDir) ->
    Parent = self(),
    Fun = fun() ->
                  %% NOTE: This is only used in tests: omit the sensitive flag
                  process_flag(trap_exit, true),
                  enter_worker(Parent, Pwd, Pwd, KeysDir)
          end,
    spawn_link(Fun).
-endif.

enter_worker(Parent, SignPwd, PeerPwd, KeysDir) ->
    try worker_init(SignPwd, PeerPwd, KeysDir) of
        {ok, #worker_state{sign_pub = SP, peer_pub = PP} = State} ->
            Parent ! {self(), pubkeys, SP, PP},
            worker_loop(Parent, State)
    catch Type:What ->
            lager:debug("Error starting worker: ~p",
                        [{Type, What, erlang:get_stacktrace()}]),
            lager:error("aec_keys worker_failed"),
            error(init_failed)
    end.

worker_init(SignPwd, PeerPwd, KeysDir) ->
    %% Ensure there is directory for keys
    case filelib:is_dir(KeysDir) of
        false ->
            ok = file:make_dir(KeysDir);
        true ->
            ok
    end,

    %% Setup Sign keys
    {SignPubFile, SignPub, SignPrivFile, SignPriv} =
        setup_sign_keys(SignPwd, KeysDir),

    %% Setup Peer keys
    {PeerPubFile, PeerPub, PeerPrivFile, PeerPriv} =
        setup_peer_keys(PeerPwd, KeysDir),

    %% For sake of simplicity, if the initialization fails the
    %% initialization above crashes
    {ok, #worker_state{ sign_priv      = SignPriv
                      , sign_pub       = SignPub
                      , sign_priv_file = SignPrivFile
                      , sign_pub_file  = SignPubFile
                      , peer_priv      = PeerPriv
                      , peer_pub       = PeerPub
                      , peer_priv_file = PeerPrivFile
                      , peer_pub_file  = PeerPubFile
                      , sign_pass      = SignPwd
                      , peer_pass      = PeerPwd
                      , keys_dir       = KeysDir
                      }}.

worker_loop(Parent, State) ->
    receive
        {'EXIT', Parent, _} ->
            ok;
        {Parent, From, Msg} ->
            worker_handle_message(Msg, From, State),
            worker_loop(Parent, State);
        Other ->
            lager:debug("Worker got unexpected: ~p", [Other]),
            worker_loop(Parent, State)
    end.

worker_handle_message({sign, Bin}, From, WS) when is_binary(Bin) ->
    #worker_state{sign_priv=PrivKey} = WS,
    try enacl:sign_detached(Bin, PrivKey) of
        Signature -> worker_reply(From, {ok, Signature})
    catch
        _Type:_What -> worker_reply(From, {error, failed_sign})
    end;
worker_handle_message(privkey, From, #worker_state{sign_priv=PrivKey}) ->
    worker_reply_if_test(From, {ok, PrivKey});
worker_handle_message(peer_privkey, From, #worker_state{peer_priv=PrivKey}) ->
    worker_reply(From, {ok, PrivKey});
worker_handle_message(Msg, From, #worker_state{}) ->
    worker_reply(From, {error, illegal_query, Msg}).

worker_reply(From, Reply) ->
    gen_server:reply(From, Reply).

-ifdef(TEST).

worker_reply_if_test(From, Reply) ->
    worker_reply(From, Reply).

-else.

worker_reply_if_test(From,_Reply) ->
    worker_reply(From, {error, not_in_test_mode}).

-endif.


%%%===================================================================
%%% Internal functions
%%%===================================================================

read_keys(Pwd, PubFile, PrivFile, PubSize, PrivSize) ->
    case {from_local_dir(PubFile), from_local_dir(PrivFile)} of
        {{ok, EPub}, {ok, EPriv}} ->
            Pub = decrypt_key(Pwd, EPub, PubSize),
            Priv = decrypt_key(Pwd, EPriv, PrivSize),
            {Pub, Priv};
        _ ->
            {error, enoent}
    end.


setup_sign_keys(Pwd, KeysDir) ->
    {PubFile, PrivFile} = p_gen_sign_filename(KeysDir),
    case read_keys(Pwd, PubFile, PrivFile, ?PUB_SIZE, ?SIGN_PRIV_SIZE) of
        {error, enoent} ->
            p_gen_new_sign(Pwd, PubFile, PrivFile);
        {Pub, Priv} ->
            case check_sign_keys(Pub, Priv) of
                true ->
                    {PubFile, Pub, PrivFile, Priv};
                false ->
                    erlang:error({invalid_sign_key_pair, [PubFile, PrivFile]})
            end
    end.

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

hash(Bin) ->
    crypto:hash(sha256, Bin).

%% INFO: keep separate APIs and encrypt both priv & pub to protect external HDs
%%       (there is known atack vector using master pub)
encrypt_key(Password, Bin) ->
    crypto:block_encrypt(aes_ecb, hash(Password),  Bin).

decrypt_key(Password, Bin, Size) ->
    <<Key:Size/binary>> = crypto:block_decrypt(aes_ecb, hash(Password), Bin),
    Key.

p_gen_new_sign(Pwd, PubFile, PrivFile) ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    case check_sign_keys(PubKey, PrivKey) of
        true ->
            p_save_keys(Pwd, PubFile, PubKey, PrivFile, PrivKey),
            {PubFile, PubKey, PrivFile, PrivKey};
        false ->
            error({generated_key_pair_check_failed, [PubKey, PrivKey]})
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

from_local_dir(NewFile) ->
    file:read_file(NewFile).

check_env() ->
    DefaultFile =  filename:join(aeu_env:data_dir(aecore), "keys"),
    KeysDir = aeu_env:user_config_or_env([<<"keys">>, <<"dir">>],
                                         aecore, keys_dir, DefaultFile),
    Pwd = aeu_env:user_config_or_env([<<"keys">>, <<"password">>],
                                     aecore, password, <<>>),
    PeerPwd = aeu_env:user_config_or_env([<<"keys">>, <<"peer_password">>],
                                         aecore, peer_password, Pwd),
    #{keys_dir => KeysDir, password => Pwd, peer_password => PeerPwd}.

p_gen_sign_filename(KeysDir) ->
    p_gen_filename(KeysDir, ?FILENAME_SIGNPUB, ?FILENAME_SIGNPRIV).

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
