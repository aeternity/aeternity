-module(aec_preset_keys).

-behaviour(gen_server).

%% behaviour API
-export([get_pubkey/0,
         candidate_pubkey/0,
         promote_candidate/1,
         sign_micro_block/1,
         produce_key_header_signature/2,
         is_ready/0,
         sign_binary/2
        ]).

-export([set_candidate/1
        ]).


%% Supervisor API
-export([start_link/1,
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

-include("blocks.hrl").
-define(SERVER, ?MODULE).
-define(NOT_SET, not_set).

-type keys_pool() :: #{aec_keys:pubkey() => aec_keys:privkey()}.

-record(state, {
          keys                       :: keys_pool(),
          active         = ?NOT_SET  :: aec_keys:pubkey() | ?NOT_SET,
          candidate      = ?NOT_SET  :: aec_keys:pubkey() | ?NOT_SET
         }).

-spec get_pubkey() -> {ok, binary()} | {error, no_active}.
get_pubkey() ->
    case gen_server:call(?SERVER, active_pubkey) of
        ?NOT_SET -> {error, no_active};
        Pubkey when is_binary(Pubkey) -> {ok, Pubkey}
    end.

-spec candidate_pubkey() -> {ok, binary()} | {error, no_candidate}.
candidate_pubkey() ->
    case gen_server:call(?SERVER, candidate_pubkey) of
        ?NOT_SET -> {error, no_candidate};
        Pubkey when is_binary(Pubkey) -> {ok, Pubkey}
    end.

-spec promote_candidate(aec_keys:pubkey()) -> ok | {error, key_not_found}.
promote_candidate(Pubkey) ->
    case gen_server:call(?SERVER, {promote_candidate, Pubkey}) of
        ok -> ok;
        {error, not_found} -> {error, key_not_found}
    end.

-spec set_candidate(aec_keys:pubkey()) -> ok | {error, key_not_found}.
set_candidate(Pubkey) ->
    case gen_server:call(?SERVER, {set_candidate, Pubkey}) of
        ok -> ok;
        {error, not_found} -> {error, key_not_found}
    end.


-spec sign_micro_block(aec_blocks:block()) -> {ok, aec_blocks:block()} | {error, term()}.
sign_micro_block(MicroBlock) ->
    Header = aec_blocks:to_micro_header(MicroBlock),
    Bin = aec_headers:serialize_to_signature_binary(Header),
    {ok, Signature} = gen_server:call(?MODULE, {sign, Bin}),
    {ok, aec_blocks:set_signature(MicroBlock, Signature)}.

-spec sign_binary(Bin, Signer) -> {ok, Signature} | {error, Reason}
    when Bin :: binary(),
         Signer :: aec_keys:pubkey(),
         Signature :: binary(),
         Reason :: not_found | failed_sign.
sign_binary(Bin, Signer) ->
    gen_server:call(?MODULE, {sign, Bin, Signer}).


%% used in PoS context§
-spec produce_key_header_signature(aec_headers:key_header(), aec_keys:pubkey()) -> {ok, block_signature()} | {error, term()}.
produce_key_header_signature(Header, ByWho) ->
    Bin = aec_headers:serialize_to_signature_binary(Header),
    case gen_server:call(?MODULE, {sign, Bin, ByWho}) of
        {ok, _Signature} = OK -> OK;
        {error, _Reason} = Err -> Err
    end.

-spec is_ready() -> boolean().
is_ready() ->
    case get_pubkey() of
        {ok, _} -> true;
        {error, no_active} -> true %% keys are loaded, it is not this node that is the leader, though
    end.

%%%===================================================================
%%% Gen server API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(#{}) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Keys) when is_map(Keys) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Keys, []).


stop() ->
    gen_server:stop(?SERVER).


init(Keys) when is_map(Keys) ->
    {ok, #state{ keys = Keys }}.

handle_call(active_pubkey, _From, #state{ active = Active } = State) ->
    {reply, Active, State};
handle_call(candidate_pubkey, _From, #state{ candidate = Candidate } = State) ->
    {reply, Candidate, State};
handle_call({promote_candidate, Pubkey}, _From, #state{ candidate = _Candidate } = State) ->
    %% We ignore the candidate on purpose: this fuction gets called both in
    %% top change, no matter if this node produced the candidate or not.
    case is_known_pubkey(Pubkey, State) of
        true ->
            {reply, ok, State#state{active = Pubkey, candidate = ?NOT_SET}};
        false ->
            {reply, {error, not_found}, State#state{active = ?NOT_SET, candidate = ?NOT_SET}}
    end;
handle_call({set_candidate, Pubkey}, _From, #state{} = State) ->
    case is_known_pubkey(Pubkey, State) of
        true ->
            {reply, ok, State#state{candidate = Pubkey}};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call({sign, Bin, ByWho}, _From, #state{} = State) ->
    case privkey(ByWho, State) of
        {ok, Privkey} ->
            Res =
                try enacl:sign_detached(Bin, Privkey) of
                    Signature -> {ok, Signature}
                catch
                    _Type:_What -> {error, failed_sign}
                end,
            {reply, Res, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({sign, Bin}, _From, #state{active = Active} = State) ->
    case privkey(Active, State) of
        {ok, Privkey} ->
            Res =
                try enacl:sign_detached(Bin, Privkey) of
                    Signature -> {ok, Signature}
                catch
                    _Type:_What -> {error, failed_sign}
                end,
            {reply, Res, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, todo}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec privkey(aec_keys:pubkey(), #state{}) -> {ok, aec_keys:privkey()} | error.
privkey(Pubkey, #state{keys = Keys}) ->
    maps:find(Pubkey, Keys).

-spec is_known_pubkey(aec_keys:pubkey(), #state{}) -> boolean().
is_known_pubkey(Pubkey, #state{keys = Keys}) ->
    maps:is_key(Pubkey, Keys).

