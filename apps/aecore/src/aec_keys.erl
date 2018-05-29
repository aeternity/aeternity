%%%-------------------------------------------------------------------
%%% @author Sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%     Key manager for AE node
%%%     * it will open keys that it finds using sys.config
%%% @end
%%% Created : 28 Aug 2017
%%%-------------------------------------------------------------------
-module(aec_keys).

-behaviour(gen_server).

-include("aec_crypto.hrl").

%% API
-export([start_link/0,
         start_link/1,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([sign/1, pubkey/0,
         wait_for_pubkey/0,
         setup_peer_keys/2,
         save_peer_keys/4,
         peer_key_filenames/1]).

-export([peer_pubkey/0, peer_privkey/0, check_peer_keys/2]).

-export([check_key_pair/0]).

-ifdef(TEST).
-export([encrypt_key/2, check_sign_keys/2, sign_privkey/0]).
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
          sign_pub       :: undefined | binary(),
          sign_priv      :: undefined | binary(),
          sign_pub_file  :: undefined | binary(),
          sign_priv_file :: undefined | binary(),
          sign_pass      :: password(),
          peer_pub       :: undefined | binary(),
          peer_priv      :: undefined | binary(),
          peer_pub_file  :: undefined | binary(),
          peer_priv_file :: undefined | binary(),
          peer_pass      :: password(),
          keys_dir       :: undefined | binary()
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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    %% INFO: set the password to re-use keys between restarts
    #{ keys_dir := KeysDir
     , password := SignPwd
     , peer_password := PeerPwd } = check_env(),
    Args = [SignPwd, PeerPwd, KeysDir],
    start_link(Args).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

-spec sign(tx()) -> {ok, signed_tx()} | {error, term()}.
sign(Tx) ->
    gen_server:call(?MODULE, {sign, Tx}).

-spec pubkey() -> {ok, binary()} | {error, key_not_found}.
pubkey() ->
    gen_server:call(?MODULE, pubkey).

-ifdef(TEST).
-spec sign_privkey() -> {ok, binary()} | {error, key_not_found}.
sign_privkey() ->
    gen_server:call(?MODULE, privkey).
-endif.

-spec peer_pubkey() -> {ok, binary()} | {error, key_not_found}.
peer_pubkey() ->
    gen_server:call(?MODULE, peer_pubkey).

-spec peer_privkey() -> {ok, binary()} | {error, key_not_found}.
peer_privkey() ->
    gen_server:call(?MODULE, peer_privkey).

-spec wait_for_pubkey() -> {ok, binary()}.
wait_for_pubkey() ->
    wait_for_pubkey(1).

wait_for_pubkey(Sleep) ->
    case pubkey() of
        {error, key_not_found} ->
            timer:sleep(Sleep),
            wait_for_pubkey(Sleep+10);
        R -> R
    end.

-spec check_key_pair() -> boolean().
check_key_pair() ->
    gen_server:call(?MODULE, check_key_pair).

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
init([Pwd, KeysDir]) ->
    init([Pwd, Pwd, KeysDir]);
init([SignPwd, PeerPwd, KeysDir]) when is_binary(SignPwd), is_binary(PeerPwd) ->
    lager:info("Initializing keys manager"),
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
    %% initialization above crashes - rather than collecting failure
    %% reason and returning `{stop, Reason::term()}`.
    {ok, #state{sign_priv=SignPriv, sign_pub=SignPub,
                sign_priv_file=SignPrivFile, sign_pub_file=SignPubFile,
                peer_priv=PeerPriv, peer_pub=PeerPub,
                peer_priv_file=PeerPrivFile, peer_pub_file=PeerPubFile,
                sign_pass=SignPwd, peer_pass=PeerPwd,
                keys_dir=KeysDir}}.

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
handle_call({sign, _}, _From, #state{sign_priv=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call({sign, Tx}, _From, #state{sign_priv=PrivKey} = State) ->
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    {reply, {ok, SignedTx}, State};
handle_call(pubkey, _From, #state{sign_pub=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(pubkey, _From, #state{sign_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(privkey, _From, #state{sign_priv=PrivKey} = State) ->
    case PrivKey of
        undefined -> {reply, {error, key_not_found}, State};
        _         -> {reply, {ok, PrivKey}, State}
    end;
handle_call(peer_pubkey, _From, #state{peer_pub=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(peer_pubkey, _From, #state{peer_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(peer_privkey, _From, #state{peer_priv=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(peer_privkey, _From, #state{peer_priv=PrivKey} = State) ->
    {reply, {ok, PrivKey}, State};
handle_call(check_key_pair, _From, #state{sign_pub = PubKey, sign_priv = PrivKey} = State) ->
    {reply, check_sign_keys(PubKey, PrivKey), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
            {EncPub, EncPriv} =
                p_save_keys(Pwd, PubFile, PubKey, PrivFile, PrivKey),
            lager:debug(" PubKey: ~p", [EncPub]),
            lager:debug(" PrivKey: ~p", [EncPriv]),
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
            {EncPub, EncPriv} =
                p_save_keys(Pwd, PubFile, PubKey, PrivFile, PrivKey),
            lager:debug("New PeerPubKey: ~p", [EncPub]),
            lager:debug("New PeerPrivKey: ~p", [EncPriv]),
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
    EncPub = encrypt_key(Pwd, PubKey),
    EncPriv = encrypt_key(Pwd, PrivKey),
    ok = to_local_dir(PubFile, EncPub),
    ok = to_local_dir(PrivFile, EncPriv),
    {EncPub, EncPriv}.

to_local_dir(NewFile, Bin) ->
    lager:debug("Saving keys to ~p", [NewFile]),
    case file:read_file(NewFile) of
        {error, enoent} ->
            {ok, IODevice} = file:open(NewFile, [write, binary, raw]),
            ok = file:write_file(NewFile, Bin),
            ok = file:close(IODevice);
        {ok, _Out} ->
            %% INFO: for now do not let to overwrite existing keys
            {error, existing_keys}
    end.

check_sign_keys(PubKey, PrivKey) ->
    SampleMsg = <<"random message">>,
    Signature = enacl:sign_detached(SampleMsg, PrivKey),
    {ok, SampleMsg} == enacl:sign_verify_detached(Signature, SampleMsg, PubKey).

check_peer_keys(PubKey, PrivKey) ->
    PubKey == enacl:curve25519_scalarmult_base(PrivKey).
