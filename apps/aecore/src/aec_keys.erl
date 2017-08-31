%%%-------------------------------------------------------------------
%%% @author Sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%
%%% @end
%%% Created : 28 Aug 2017
%%%-------------------------------------------------------------------
-module(aec_keys).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([new/1, open/1, set/3, sign/1, pubkey/0]).

-define(SERVER, ?MODULE).
-define(KEYS_DIR, filename:join(code:root_dir(), "keys")).
-define(PUB_SIZE, 65).
-define(PRIV_SIZE, 32).
-define(FILENAME_PUB, "key.pub").
-define(FILENAME_PRIV, "key").

-record(state, {
          priv   :: binary(),
          pub    :: binary(),
          pass   :: binary(),
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec sign(term()) -> binary().
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
init([]) ->
    lager:info("Initializing keys manager"),
    %% TODO: consider moving the crypto config to config
    Algo = ecdsa,
    KeyType = ecdh,
    Digest = sha256,
    Curve = secp256k1,

    %% INFO: set the password to re-use keys between restarts
    Password = application:get_env(aecrypto, password, undefined),

    %% Ensure there is directory for keys
    case filelib:is_dir(?KEYS_DIR) of
        false ->
            file:make_dir(?KEYS_DIR);
        true ->
            ok
    end,

    %% INFO: previous keys are picked up from KEYS_DIR (handled in timeout)
    {ok, #state{algo=Algo,
                type=KeyType,
                digest=Digest,
                curve=Curve,
                pass=Password}, 0}.

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
    Signed = crypto:sign(Algo, Digest, term_to_binary(Term),  [PrivKey, crypto:ec_curve(Curve)]),
    {reply, Signed, State};

handle_call(pubkey, _From, #state{pub=undefined} = State) ->
    {reply, {error, key_not_found}, State};
handle_call(pubkey, _From, #state{pub=PubKey} = State) ->
    {reply, {ok, PubKey}, State};

handle_call({new, Password}, _From, #state{type=KeyType, curve=Curve} = State) ->
    try
        {NewPubKey, NewPrivKey} = crypto:generate_key(KeyType, crypto:ec_curve(Curve)),
        EncPub = encrypt_pubkey(Password, NewPubKey),
        lager:debug("New PubKey: ~p", [EncPub]),
        EncPriv = encrypt_privkey(Password, NewPrivKey),
        lager:debug("New PrivKey: ~p", [EncPriv]),
        ok = to_local_dir(?FILENAME_PUB, EncPub),
        ok = to_local_dir(?FILENAME_PRIV, EncPriv),
        {reply, {ok, NewPubKey}, State#state{priv=NewPrivKey, pub=NewPubKey, pass=Password}}
    catch
        Err:R ->
            lager:error("New keys not generated or saved ~p ~p", [Err, R]),
            {reply, error, State}
    end;

handle_call({open, Password}, _From, State) ->
    try
        {ok, Pub} = from_local_dir(?FILENAME_PUB),
        {ok, Priv} = from_local_dir(?FILENAME_PRIV),
        Pub0 = decrypt_pubkey(Password, Pub),
        Priv0 = decrypt_privkey(Password, Priv),
        {reply, ok, State#state{priv=Priv0, pub=Pub0, pass=Password}}
    catch
        Err:R ->
            lager:error("Can't open keys files ~p ~p", [Err, R]),
            {reply, {error, keys_not_loaded}, State}
    end;

handle_call({set, Password, Priv, Pub}, _From, State) ->
    try
        EncPub = encrypt_pubkey(Password, Pub),
        EncPriv = encrypt_privkey(Password, Priv),
        ok = to_local_dir(?FILENAME_PUB, EncPub),
        ok = to_local_dir(?FILENAME_PRIV, EncPriv),
        {reply, ok, State#state{priv = Priv, pub = Pub}}
    catch
        Err:R ->
            lager:error("Can't set keys ~p ~p", [Err, R]),
            {reply, {error, keys_not_loaded}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timout, #state{priv=undefined, pub=undefined, pass=Password} = State) ->
    try
        {ok, Pub} = from_local_dir(?FILENAME_PUB),
        {ok, Priv} = from_local_dir(?FILENAME_PRIV),
        Pub0 = decrypt_pubkey(Password, Pub),
        Priv0 = decrypt_privkey(Password, Priv),
        {noreply, State#state{priv=Priv0, pub=Pub0}}
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

to_local_dir(Filename, Bin) ->
    NewFile = filename:join(?KEYS_DIR, Filename),
    lager:debug("Saving keys to ~p", [NewFile]),
    case file:read_file(NewFile) of
        {error, enoent} ->
            {ok, IODevice} = file:open(NewFile, [write, binary, raw]),
            file:write_file(NewFile, Bin),
            file:close(IODevice);
        {ok, _Out} ->
            %% INFO: for now do not let to overwrite existing keys
            {error, existing_keys}
    end.

from_local_dir(Filename) ->
    NewFile = filename:join(?KEYS_DIR, Filename),
    file:read_file(NewFile).