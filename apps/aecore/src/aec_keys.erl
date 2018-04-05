%%%-------------------------------------------------------------------
%%% @author Sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%     Key manager for AE node
%%%     * it will open keys that it finds using sys.config
%%%     * delete/0 removes files from the filesystem and from the state (DANGER!)
%%% @end
%%% Created : 28 Aug 2017
%%%-------------------------------------------------------------------
-module(aec_keys).

-behaviour(gen_server).

-include("common.hrl").
-include("aec_crypto.hrl").

%% API
-export([start_link/0,
         start_link/1,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([sign/1, pubkey/0, delete/0,
         wait_for_pubkey/0]).

-export([peer_pubkey/0, peer_privkey/0, check_peer_keys/2]).

-export([check_key_pair/0]).

-ifdef(TEST).
-export([encrypt_peerkey/2, check_key_pair/2]).
-endif.

-define(SERVER, ?MODULE).
-define(PUB_SIZE, 65).
-define(PRIV_SIZE, 32).
-define(FILENAME_PUB, "key.pub").
-define(FILENAME_PRIV, "key").

-define(FILENAME_PEERPUB, "peer_key.pub").
-define(FILENAME_PEERPRIV, "peer_key").

-record(state, {
          pub       :: undefined | binary(),
          priv      :: undefined | binary(),
          peer_pub  :: undefined | binary(),
          peer_priv :: undefined | binary(),
          pass      :: password(),
          keys_dir  :: undefined | binary(),
          pub_file  :: undefined | binary(),
          priv_file :: undefined | binary(),
          peer_pub_file  :: undefined | binary(),
          peer_priv_file :: undefined | binary()
         }).

-type password() :: binary().

-type tx() :: aetx:tx().
-type signed_tx() :: aetx_sign:signed_tx().

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
    #{keys_dir := KeysDir, password := Password} = check_env(),
    Args = [Password, KeysDir],
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

-spec delete() -> ok | error.
delete() ->
    gen_server:call(?MODULE, delete).
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
init([Password, KeysDir]) when is_binary(Password) ->
    lager:info("Initializing keys manager"),
    %% Ensure there is directory for keys
    case filelib:is_dir(KeysDir) of
        false ->
            ok = file:make_dir(KeysDir);
        true ->
            ok
    end,

    {PubFile, PrivFile} = p_gen_filename(KeysDir),
    {Pub1, Priv1} =
        case from_local_dir(PubFile) of
            {error, enoent} ->
                p_gen_new(Password, PubFile, PrivFile);
            {ok, Pub} ->
                {ok, Priv} = from_local_dir(PrivFile),
                Pub0 = decrypt_pubkey(Password, Pub),
                Priv0 = decrypt_privkey(Password, Priv),
                case check_key_pair(Pub0, Priv0) of
                    true ->
                        {Pub0, Priv0};
                    false ->
                        erlang:error({invalid_key_pair_from_local,
                                      [PubFile, PrivFile]})
                end
        end,

    %% Setup Peer keys
    {PeerPubFile, PeerPrivFile} = p_gen_peer_filename(KeysDir),
    {PeerPub, PeerPriv} =
        case {from_local_dir(PeerPubFile), from_local_dir(PeerPrivFile)} of
            {{ok, EPeerPub}, {ok, EPeerPriv}} ->
                PeerPub0  = decrypt_peerkey(Password, EPeerPub),
                PeerPriv0 = decrypt_peerkey(Password, EPeerPriv),
                case check_peer_keys(PeerPub0, PeerPriv0) of
                    true  -> {PeerPub0, PeerPriv0};
                    false -> erlang:error({invalid_peer_key_pair_from_local,
                                           [PeerPubFile, PeerPrivFile]})
                end;
            _ ->
                p_gen_new_peer(Password, PeerPubFile, PeerPrivFile)
        end,

    %% For sake of simplicity, if the initialization fails the
    %% initialization above crashes - rather than collecting failure
    %% reason and returning `{stop, Reason::term()}`.
    {ok, #state{priv=Priv1, pub=Pub1,
                priv_file=PrivFile, pub_file=PubFile,
                peer_priv=PeerPriv, peer_pub=PeerPub,
                peer_priv_file=PeerPrivFile, peer_pub_file=PeerPubFile,
                pass=Password,
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
handle_call({sign, _}, _From, #state{priv=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call({sign, Tx}, _From,
            #state{pub = PubKey, priv=PrivKey} = State) ->
    Signers = aetx:signers(Tx),
    case lists:member(PubKey, Signers) of
        false ->
            {reply, {error, not_a_signer}, State};
        true ->
            SignedTx = aetx_sign:sign(Tx, PrivKey),
            {reply, {ok, SignedTx}, State}
    end;
handle_call({verify, Signatures, Tx}, _From, #state{crypto = C} = State) ->
    SignersPubKeys = aetx:signers(Tx),
    Bin = aetx:serialize_to_binary(Tx),
    Res = lists:all(fun(SignerPubKey) ->
                            has_signers_signature(Bin, SignerPubKey, Signatures, C)
                    end, SignersPubKeys),
    {reply, Res, State};
handle_call(pubkey, _From, #state{pub=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(pubkey, _From, #state{pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(peer_pubkey, _From, #state{peer_pub=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(peer_pubkey, _From, #state{peer_pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};
handle_call(peer_privkey, _From, #state{peer_priv=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(peer_privkey, _From, #state{peer_priv=PrivKey} = State) ->
    {reply, {ok, PrivKey}, State};
handle_call(check_key_pair, _From, #state{pub = PubKey, priv = PrivKey} = State) ->
    {reply, check_key_pair(PubKey, PrivKey), State};
handle_call(delete, _From, #state{pub_file=PubFile, priv_file=PrivFile} = State) ->
    try
        ok = file:delete(PubFile),
        ok = file:delete(PrivFile),
        {reply, ok, State#state{pub_file=undefined, priv_file=undefined,
                                pub=undefined, priv=undefined}}
    catch
        Err:R ->
            lager:error("New keys not deleted ~p ~p", [Err, R]),
            {reply, error, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

has_signers_signature(Bin, SignerPubKey, Signatures,
                      #crypto{algo = Algo,
                              digest = Digest,
                              curve = Curve}) ->
    lists:any(
      fun(Signature) ->
              crypto:verify(
                Algo, Digest, Bin, Signature, [SignerPubKey, crypto:ec_curve(Curve)])
      end, Signatures).

hash(Bin) ->
    crypto:hash(sha256, Bin).

%% INFO: keep separate APIs and encrypt both priv & pub to protect external HDs
%%       (there is known atack vector using master pub)
encrypt_privkey(Password, Bin) ->
    crypto:block_encrypt(aes_ecb, hash(Password),  pad_privkey(Bin)).

encrypt_pubkey(Password, Bin) ->
    %% TODO: is it safe to use 0s as padding? Consider moving to stream encryption API
    crypto:block_encrypt(aes_ecb, hash(Password),  padding128(Bin)).

decrypt_privkey(Password, Bin) ->
    crypto:block_decrypt(aes_ecb, hash(Password), Bin).

decrypt_pubkey(Password, Bin) ->
    <<Pub:65/binary, _Padding/binary>> = crypto:block_decrypt(aes_ecb, hash(Password), Bin),
    Pub.

padding128(Bin) ->
    Pad = 128 - size(Bin),
    <<Bin/binary, 0:(Pad*8)>>.

%% crypto:generate_keys/2 gives you a binary with as many bytes as are needed to fit the
%% private key. It does not pad with zeros.
pad_privkey(Bin) ->
    Pad = ?PRIV_SIZE - size(Bin),
    <<0:(Pad*8), Bin/binary>>.

p_gen_filename(KeysDir) ->
    %% TODO: consider checking whats in the dir and genrerating file with suffix
    PubFile = filename:join(KeysDir, ?FILENAME_PUB),
    PrivFile = filename:join(KeysDir, ?FILENAME_PRIV),
    {PubFile, PrivFile}.

p_gen_new(Password, PubFilename, PrivFilename) ->
    {NewPubKey, NewPrivKey} =
        crypto:generate_key(?CRYPTO_KEYTYPE, crypto:ec_curve(?CRYPTO_CURVE)),
    case check_key_pair(NewPubKey, NewPrivKey) of
        true ->
            EncPub = encrypt_pubkey(Password, NewPubKey),
            lager:debug("New PubKey: ~p", [EncPub]),
            EncPriv = encrypt_privkey(Password, NewPrivKey),
            lager:debug("New PrivKey: ~p", [EncPriv]),
            ok = to_local_dir(PubFilename, EncPub),
            ok = to_local_dir(PrivFilename, EncPriv),
            {NewPubKey, NewPrivKey};
        false ->
            error({generated_key_pair_check_failed, [NewPubKey, NewPrivKey]})
    end.

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

from_local_dir(NewFile) ->
    file:read_file(NewFile).

check_key_pair(PubKey, PrivKey) ->
    SampleMsg = <<"random message">>,
    Curve = crypto:ec_curve(?CRYPTO_CURVE),
    Signature = crypto:sign(?CRYPTO_ALGO, ?CRYPTO_DIGEST, SampleMsg,
                            [PrivKey, Curve]),
    crypto:verify(?CRYPTO_ALGO, ?CRYPTO_DIGEST, SampleMsg, Signature,
                  [PubKey, Curve]).

check_env() ->
    DefaultFile =  filename:join(aeu_env:data_dir(aecore), "keys"),
    KeysDir = aeu_env:user_config_or_env([<<"keys">>, <<"dir">>],
                                         aecore, keys_dir, DefaultFile),
    Pwd = aeu_env:user_config_or_env([<<"keys">>, <<"password">>],
                                     aecore, password, <<>>),
    #{keys_dir => KeysDir, password => Pwd}.

%%% --- PEER KEYS ---
%%% Basically a copy of node keys, once we drop the original node keys - we can
%%% promote these functions...

p_gen_peer_filename(KeysDir) ->
    %% TODO: consider checking whats in the dir and genrerating file with suffix
    PubFile = filename:join(KeysDir, ?FILENAME_PEERPUB),
    PrivFile = filename:join(KeysDir, ?FILENAME_PEERPRIV),
    {PubFile, PrivFile}.

encrypt_peerkey(Password, Bin) ->
    crypto:block_encrypt(aes_ecb, hash(Password),  pad_peerkey(Bin)).

decrypt_peerkey(Password, Bin) ->
    crypto:block_decrypt(aes_ecb, hash(Password), Bin).

pad_peerkey(Bin) ->
    Pad = 32 - size(Bin),
    <<0:(Pad*8), Bin/binary>>.

p_gen_new_peer(Password, PeerPubFile, PeerPrivFile) ->
    KeyPair = enoise_keypair:new(dh25519),
    PubKey  = enoise_keypair:pubkey(KeyPair),
    PrivKey = enoise_keypair:seckey(KeyPair),
    case check_peer_keys(PubKey, PrivKey) of
        true ->
            EncPub = encrypt_peerkey(Password, PubKey),
            lager:debug("New PubKey: ~p", [EncPub]),
            EncPriv = encrypt_peerkey(Password, PrivKey),
            lager:debug("New PrivKey: ~p", [EncPriv]),
            ok = to_local_dir(PeerPubFile, EncPub),
            ok = to_local_dir(PeerPrivFile, EncPriv),
            {PubKey, PrivKey};
        false ->
            error({generated_key_pair_check_failed, [PubKey, PrivKey]})
    end.

check_peer_keys(PubKey, PrivKey) ->
    PubKey == enacl:curve25519_scalarmult_base(PrivKey).
