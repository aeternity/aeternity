%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_chain_sim_connector).

-behaviour(aehc_connector).
-behaviour(gen_server).

%% API.
-export([start_link/1, stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([send_tx/3, get_block_by_hash/1, get_top_block/0, dry_send_tx/3]).

%% API.

-spec start_link(Args::map()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%%  aehc_connector behaviour
%%%===================================================================

-spec send_tx(binary(), binary(), binary()) -> binary().
send_tx(Delegate, Commitment, PoGF) ->
    gen_server:call(?MODULE, {send_tx, Delegate, Commitment, PoGF}).

-spec get_top_block() -> aehc_parent_block:parent_block().
get_top_block() ->
    gen_server:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(binary()) -> aehc_parent_block:parent_block().
get_block_by_hash(Hash) ->
    gen_server:call(?MODULE, {get_block_by_hash, Hash}).

-spec dry_send_tx(binary(), binary(), binary()) -> binary().
dry_send_tx(Delegate, Commitment, PoGF) ->
    gen_server:call(?MODULE, {dry_send_tx, Delegate, Commitment, PoGF}).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { pid::pid(), height = 0::non_neg_integer() }).

init(Args) ->
    process_flag(trap_exit, true),
    true = aec_events:subscribe({parent_chain, top_changed}),
    State = maps:get(<<"genesis_state">>, Args),
    {ok, Pid} = aec_chain_sim:start(#{ sim_type => parent_chain, genesis_state => State }),
    lager:info("Parent chain's connector ~p is attached: ~p", [?MODULE, Pid]),
    {ok, #state{ pid = Pid }}.

handle_call({send_tx, Delegate, Commitment, PoGF}, _From, State) ->
    %% The current validator credentials;
    %% Requested transaction by hash from a simulator's block should satisfy the origin of validator;
    {ok, PrivKey} = aec_keys:sign_privkey(),
    %% The main intention of this call is to emulate post action with signed payload from delegate;
    %% Fee, nonce, ttl and amount fields have decorated nature;
    Header = aehc_commitment_header:new(Delegate, Commitment, PoGF),
    Payload = term_to_binary(aehc_commitment:new(Header), [compressed]),
    {ok, Tx} = aec_spend_tx:new(#{ sender_id => Delegate, recipient_id => Delegate, amount => 1,
                                        fee => 5, nonce => 1, payload => Payload, ttl => 0 }),
    BinaryTx = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    SignedTx = aetx_sign:new(Tx, [enacl:sign_detached(BinaryTx, PrivKey)]),
    TxHash = aetx_sign:hash(SignedTx),
    %% The next format is prepared accordingly to simulator internal representation;
    Res = aec_chain_sim:push(#{ tx_hash => TxHash, signed_tx  => SignedTx }),
    {reply, Res, State};

handle_call({get_top_block}, _From, State) ->
    Hash = aec_chain_sim:top_block_hash(),
    {ok, Block} = aec_chain_sim:block_by_hash(Hash),
    {reply, format_block(Block), State};

handle_call({get_block_by_hash, Hash}, _From, State) ->
    {ok, Block} = aec_chain_sim:block_by_hash(Hash),
    {reply, format_block(Block), State};

handle_call({dry_send_tx, _Delegate, _Commitment, _PoGF}, _From, State) ->
    {reply, ok, State};

handle_call(Request, _From, State) ->
    lager:info("Unexpected call: ~p", [Request]),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, {parent_chain, top_changed}, #{info := Info}}, State) ->
    Pid = maps:get(pid, Info, undefined),
    (State#state.pid == Pid) andalso
        begin
            Hash = maps:get(block_hash, Info),
            PrevHash = maps:get(prev_hash, Info),
            Height = maps:get(height, Info),
            Txs = maps:get(txs, Info),
            ParentBlock = aehc_connector:parent_block(Height, Hash, PrevHash, commitments(Txs)),
            aehc_connector:publish_block(<<"chain_sim">>, ParentBlock)
        end,
    {noreply, State};

handle_info(Info, State) ->
    lager:info("Unexpected message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    aec_chain_sim:stop(),
    ok.

format_block(Block) ->
    Header = aec_blocks:to_header(Block),
    Height = aec_headers:height(Header),
    Type = aec_headers:type(Header),
    Txs = case Type of
              micro ->
                  aec_blocks:txs(Block);
              key ->
                  []
          end,
    {ok, Hash} = aec_headers:hash_header(Header),
    PrevHash = aec_headers:prev_hash(Header),
    aehc_connector:parent_block(Height, Hash, PrevHash, commitments(Txs)).

-spec commitments(aec_blocks:tx_list()) -> [aehc_commitment:commitment()].
commitments(Txs) ->
     %% TODO: This function has to provide the actual verification of included tx;
    %% This is extremely simplified procedure to pass proto test SUITE;
    Match = <<"Test commitment">>,
    Txs = lists:filter(fun (Tx) -> payload(Tx) == Match end, Txs),
    [aehc_connector:commitment(sender_id(Tx), payload(Tx)) || Tx <- Txs].

%%%===================================================================
%%%  Txs accessors
%%%===================================================================
-spec payload(aetx_sign:signed_tx()) -> binary().
payload(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx), SpendTx = aetx:tx(Tx),
    Payload = aec_spend_tx:payload(SpendTx), true = is_binary(Payload),
    Payload.

-spec sender_id(aetx_sign:signed_tx()) -> binary().
sender_id(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx), SpendTx = aetx:tx(Tx),
    SenderId = aec_spend_tx:sender_id(SpendTx), true = is_binary(SenderId),
    SenderId.
