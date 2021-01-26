%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_chain_sim_connector).

-behaviour(aeconnector).
-behaviour(gen_server).

%% API.
-export([connect/2]).
-export([send_tx/2, dry_send_tx/2]).
-export([get_top_block/0, get_block_by_hash/1]).
-export([disconnect/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-type block() :: aeconnector_block:block().

%%%===================================================================
%%%  aeconnector behaviour
%%%===================================================================

-spec connect(map(), function()) -> {ok, pid()} | {error, term()}.
connect(Args, Callback) ->
    Genesis = maps:get(<<"genesis_state">>, Args),
    State = state(Genesis, Callback),
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

-spec dry_send_tx(binary(), binary()) -> boolean().
dry_send_tx(Delegate, Payload) ->
    gen_server:call(?MODULE, {dry_send_tx, Delegate, Payload}).

-spec send_tx(binary(), binary()) -> ok | {error, term()}.
send_tx(Delegate, Payload) ->
    gen_server:call(?MODULE, {send_tx, Delegate, Payload}).

-spec get_top_block() -> {ok, binary()} | {error, term()}.
get_top_block() ->
    gen_server:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(binary()) -> {ok, block()} | {error, term()}.
get_block_by_hash(Hash) ->
    gen_server:call(?MODULE, {get_block_by_hash, Hash}).

-spec disconnect() -> ok.
disconnect() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { pid::pid(), callback::function(), genesis::term() }).

-type state() :: #state{}.

init(State) ->
    process_flag(trap_exit, true),
    true = aec_events:subscribe(parent_top_changed),
    Genesis = genesis(State),
    {ok, Pid} = aec_chain_sim:start(#{ sim_type => parent_chain, genesis_state => Genesis }),
    lager:info("Parent chain's connector ~p is attached: ~p", [?MODULE, Pid]),
    {ok, pid(State, Pid)}.

handle_call({dry_send_tx, _Delegate, _Commitment, _PoGF}, _From, State) ->
    {reply, ok, State};

handle_call({send_tx, Delegate, Payload}, _From, State) ->
    %% The current validator credentials;
    %% Requested transaction by hash from a simulator's block should satisfy the origin of validator;
    {ok, PrivKey} = aec_keys:sign_privkey(),
    %% The main intention of this call is to emulate post action with signed payload from delegate;
    %% Fee, nonce, ttl and amount fields have decorated nature;
    {ok, Tx} = aec_spend_tx:new(#{ sender_id => Delegate, recipient_id => Delegate, amount => 1,
                                        fee => 5, nonce => 1, payload => Payload, ttl => 0 }),
    BinaryTx = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    SignedTx = aetx_sign:new(Tx, [enacl:sign_detached(BinaryTx, PrivKey)]),
    TxHash = aetx_sign:hash(SignedTx),
    %% The next format is prepared accordingly to simualtor internal representation;
    ok = aec_chain_sim:push(#{ tx_hash => TxHash, signed_tx  => SignedTx }),
    {reply, ok, State};

handle_call({get_top_block}, _From, State) ->
    Hash = aec_chain_sim:top_block_hash(),
    {ok, Block} = aec_chain_sim:block_by_hash(Hash),
    Header = aec_blocks:to_header(Block),
    Reply = aec_headers:hash_header(Header),
    {reply, Reply, State};

handle_call({get_block_by_hash, Hash}, _From, State) ->
    {ok, Block} = aec_chain_sim:block_by_hash(Hash),
    {reply, {ok, block(Block)}, State};

handle_call(Request, _From, State) ->
    lager:info("Unexpected call: ~p", [Request]),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, parent_top_changed, #{info := Info}}, State) ->
    Pid = maps:get(pid, Info, undefined),
    (State#state.pid == Pid) andalso
        begin
            Hash = maps:get(block_hash, Info),
            PrevHash = maps:get(prev_hash, Info),
            Height = maps:get(height, Info),
            Txs = maps:get(txs, Info),
            Block = aeconnector_block:block(Height, Hash, PrevHash, txs(Txs)),
            Callback = callback(State),
            catch(Callback(?MODULE, Block))
%%            aehc_connector:publish_block(<<"chain_sim">>, ParentBlock)
        end,
    {noreply, State};

handle_info(Info, State) ->
    lager:info("Unexpected message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    aec_chain_sim:stop(),
    ok.

%%%===================================================================
%%%  State access layer
%%%===================================================================

-spec state(term(), function()) -> state().
state(Genesis, Callback) ->
    #state{ genesis = Genesis, callback = Callback }.

-spec callback(state()) -> function().
callback(State) ->
    State#state.callback.

-spec genesis(state()) -> term().
genesis(State) ->
    State#state.genesis.

-spec pid(state(), pid()) -> state().
pid(State, Pid) ->
    State#state{ pid = Pid }.

block(Block) ->
    Header = aec_blocks:to_header(Block),
    Height = aec_headers:height(Header),
    Type = aec_headers:type(Header),
    Txs = case Type of
              micro ->
                  [txs(Tx)|| Tx <- aec_blocks:txs(Block)];
              key ->
                  []
          end,
    {ok, Hash} = aec_headers:hash_header(Header),
    PrevHash = aec_headers:prev_hash(Header),
    aeconnector_block:block(Height, Hash, PrevHash, Txs).

-spec txs(aec_blocks:tx_list()) -> [aehc_commitment:commitment()].
txs(Txs) ->
     %% TODO: This function has to provide the actual verification of included tx;
    %% This is extremely simplified procedure to pass proto test SUITE;
    Match = <<"Test commitment">>,
    Txs = lists:filter(fun (Tx) -> payload(Tx) == Match end, Txs),
    [aeconnector_tx:tx(sender_id(Tx), payload(Tx)) || Tx <- Txs].


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
