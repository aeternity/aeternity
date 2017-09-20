%%%-------------------------------------------------------------------
%%% @author Sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%     Key manager for AE node
%%%     * it will open keys that it finds using sys.config
%%%     * if the password is not correct, use open/1
%%%     * generating new/1 or setting set/3 keys will fail when another keys are present
%%%     * delete/0 removes files from the filesystem and from the state (DANGER!)
%%% @end
%%% Created : 28 Aug 2017
%%%-------------------------------------------------------------------
-module(aec_keys).

-behaviour(gen_server).

-include("common.hrl").
-include("txs.hrl").

%% API
-export([start_link/0,
         start_link/1,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([new/1, open/1, set/3, sign/1, pubkey/0, delete/0]).

-define(SERVER, ?MODULE).
-define(PUB_SIZE, 65).
-define(PRIV_SIZE, 32).
-define(FILENAME_PUB, "key.pub").
-define(FILENAME_PRIV, "key").

-record(state, {
          pub       :: undefined | binary(),
          priv      :: undefined | binary(),
          pass      :: undefined | binary(),
          keys_dir  :: undefined | binary(),
          pub_file  :: undefined | binary(),
          priv_file :: undefined | binary(),
          type   :: atom(),
          algo   :: atom(),
          digest :: atom(),
          curve  :: atom()}).

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
    Password = application:get_env(aecore, password, undefined),
    KeysDir = application:get_env(aecore, keys_dir, filename:join(code:root_dir(), "keys")),
    Args = [Password, KeysDir],
    start_link(Args).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

-spec sign(term()) -> {ok, term()}.
sign(Term) ->
    gen_server:call(?MODULE, {sign, Term}).

-spec pubkey() -> {ok, binary()} | {error, key_not_found}.
pubkey() ->
    gen_server:call(?MODULE, pubkey).

-spec open(binary()) -> {ok, Password :: binary()} | {error, keys_not_loaded}.
open(Password) ->
    gen_server:call(?MODULE, {open, Password}).

-spec new(binary()) -> {Priv :: binary(), Pub :: binary()} | error.
new(Password) ->
    gen_server:call(?MODULE, {new, Password}).

-spec set(binary(), binary(), binary()) -> ok | error.
set(Password, Priv, Pub) ->
    gen_server:call(?MODULE, {set, Password, Priv, Pub}).

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
init([Password, KeysDir]) ->
    lager:info("Initializing keys manager"),
    %% TODO: consider moving the crypto config to config
    Algo = ecdsa,
    KeyType = ecdh,
    Digest = sha256,
    Curve = secp256k1,

    %% Ensure there is directory for keys
    case filelib:is_dir(KeysDir) of
        false ->
            ok = file:make_dir(KeysDir);
        true ->
            ok
    end,

    %% INFO: previous keys are picked up from KEYS_DIR (handled in timeout)
    {ok, #state{algo=Algo,
                type=KeyType,
                digest=Digest,
                curve=Curve,
                pass=Password,
                keys_dir=KeysDir}, 0}.

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
handle_call({sign, Term}, _From, #state{priv=PrivKey, algo=Algo, digest=Digest, curve=Curve} = State) ->
    Signature = crypto:sign(Algo, Digest, term_to_binary(Term),  [PrivKey, crypto:ec_curve(Curve)]), %% TODO Review transaction serialization.
    {reply, {ok, #signed_tx{data = Term, signatures = [Signature]}}, State};

handle_call(pubkey, _From, #state{pub=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(pubkey, _From, #state{pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};

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
    end;

handle_call({new, Password}, _From, #state{type=KeyType, curve=Curve, keys_dir=KeysDir} = State) ->
    try
        {NewPubFile, NewPrivFile} = p_gen_filename(KeysDir),
        {NewPubKey, NewPrivKey} = p_gen_new(Password, KeyType, Curve, NewPubFile, NewPrivFile),
        {reply, {ok, NewPubKey}, State#state{priv=NewPrivKey, pub=NewPubKey, pass=Password,
                                             pub_file=NewPubFile, priv_file=NewPrivFile}}
    catch
        Err:R ->
            lager:error("New keys not generated or saved ~p ~p", [Err, R]),
            {reply, error, State}
    end;

handle_call({open, Password}, _From, #state{pub_file=PubFile, priv_file=PrivFile} = State) ->
    try
        {ok, Pub} = from_local_dir(PubFile),
        {ok, Priv} = from_local_dir(PrivFile),
        Pub0 = decrypt_pubkey(Password, Pub),
        Priv0 = decrypt_privkey(Password, Priv),
        {reply, ok, State#state{priv=Priv0, pub=Pub0, pass=Password}}
    catch
        Err:R ->
            lager:error("Can't open keys files ~p ~p", [Err, R]),
            {reply, {error, keys_not_loaded}, State}
    end;

handle_call({set, Password, Priv, Pub}, _From, #state{pub_file=PubFile, priv_file=PrivFile} = State) ->
    try
        EncPub = encrypt_pubkey(Password, Pub),
        EncPriv = encrypt_privkey(Password, Priv),
        ok = to_local_dir(PubFile, EncPub),
        ok = to_local_dir(PrivFile, EncPriv),
        {reply, ok, State#state{priv = Priv, pub = Pub}}
    catch
        Err:R ->
            lager:error("Can't set keys ~p ~p", [Err, R]),
            {reply, {error, keys_not_loaded}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{priv=undefined, pub=undefined, pass=Password,
                           type=KeyType, curve=Curve, keys_dir=KeysDir} = State) ->
    try
        {PubFile, PrivFile} = p_gen_filename(KeysDir),
        {Pub1, Priv1} = case from_local_dir(PubFile) of
             {error, enoent} ->
                 p_gen_new(Password, KeyType, Curve, PubFile, PrivFile);
             {ok, Pub} ->
                 {ok, Priv} = from_local_dir(PrivFile),
                 Pub0 = decrypt_pubkey(Password, Pub),
                 Priv0 = decrypt_privkey(Password, Priv),
                 {Pub0, Priv0}
        end,
        {noreply, State#state{priv=Priv1, pub=Pub1, priv_file=PrivFile, pub_file=PubFile}}
    catch
        Err:R ->
            lager:error("Can't open keys files ~p ~p", [Err, R]),
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
hash(Bin) ->
    crypto:hash(sha256, Bin).

%% INFO: keep separate APIs and encrypt both priv & pub to protect external HDs
%%       (there is known atack vector using master pub)
encrypt_privkey(Password, Bin) ->
    crypto:block_encrypt(aes_ecb, hash(Password),  Bin).

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

p_gen_filename(KeysDir) ->
    %% TODO: consider checking whats in the dir and genrerating file with suffix
    PubFile = filename:join(KeysDir, ?FILENAME_PUB),
    PrivFile = filename:join(KeysDir, ?FILENAME_PRIV),
    {PubFile, PrivFile}.

p_gen_new(undefined, _, _, _, _) ->
    no_password_provided;
p_gen_new(Password, KeyType, Curve, PubFilename, PrivFilename) ->
    {NewPubKey, NewPrivKey} = crypto:generate_key(KeyType, crypto:ec_curve(Curve)),
    EncPub = encrypt_pubkey(Password, NewPubKey),
    lager:debug("New PubKey: ~p", [EncPub]),
    EncPriv = encrypt_privkey(Password, NewPrivKey),
    lager:debug("New PrivKey: ~p", [EncPriv]),
    ok = to_local_dir(PubFilename, EncPub),
    ok = to_local_dir(PrivFilename, EncPriv),
    {NewPubKey, NewPrivKey}.

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
