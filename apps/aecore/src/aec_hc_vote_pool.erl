%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity Foundation
%%% @doc
%%%     Pool for HC votes (short lived transport Txs)
%%% @end
%%%-------------------------------------------------------------------
-module(aec_hc_vote_pool).

-behaviour(gen_server).
-compile({no_auto_import, [size/1]}).

%% API
-export([ start_link/0
        , stop/0
        ]).

-export([ peek/1
        , push/1
        , push/2
        , size/0
        ]).

-include_lib("aecontract/include/hard_forks.hrl").

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-import(aeu_debug, [pp/1]).

-define(SERVER, ?MODULE).

-record(state, { gc_height       :: height()
               , hash_pool = #{} :: #{tx_hash() => vote()}
               , h_cache   = #{} :: #{height() => tx_hash()}
               , t_cache   = #{} :: #{{height(), binary()} => [vote()]} }).

-type height() :: aec_blocks:height().

-type event() :: tx_created | tx_received.

-type vote()    :: aec_hc_vote_tx:tx().
-type tx_hash() :: binary().

-define(DEFAULT_TX_TTL, 5).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    %% implies also clearing the mempool
    gen_server:stop(?SERVER).

-define(PUSH_EVENT(E), Event =:= tx_created; Event =:= tx_received).

-spec push(aetx_sign:signed_tx()) -> ok | {error, atom()}.
push(Tx) ->
    push(Tx, tx_created).

-spec push(aetx_sign:signed_tx(), event()) -> ok | {error, atom()}.
push(Tx, Event = tx_received) ->
    TxHash = safe_tx_hash(Tx),
    case aec_tx_gossip_cache:in_cache(TxHash) of
        true ->
            ok;
        false ->
            %% Transported through gossip, use 'tx_pool_push' job queue
            aec_jobs_queues:run(tx_pool_push, fun() -> push_(Tx, Event) end)
    end;
push(Tx, Event = tx_created) ->
    push_(Tx, Event).

safe_tx_hash(Tx) ->
    try
        aetx_sign:hash(Tx)
    catch _:_ ->
        error({illegal_transaction, Tx})
    end.

push_(STx, Event) ->
    case validate_vote_tx(STx) of
        Err = {error, _} ->
            lager:debub("Validation error ~p for HCVoteTx: ~p", [Err, STx]),
            Err;
        {ok, Vote} ->
            gen_server:call(?SERVER, {push, Vote, STx, Event})
    end.

%% The specified maximum number of transactions avoids requiring
%% building in memory the complete list of all transactions in the
%% pool.
-spec peek(height() | {height(), binary()}) -> {ok, [vote()]}.
peek(At) when is_integer(At) ->
    gen_server:call(?SERVER, {peek, At});
peek({H, B} = At) when is_integer(H), is_binary(B) ->
    gen_server:call(?SERVER, {peek, At}).

-spec size() -> {ok, non_neg_integer()}.
size() ->
    gen_server:call(?SERVER, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    GCHeight = top_height(),
    lager:debug("init: GCHeight = ~p", [GCHeight]),
    aec_events:subscribe(top_changed),
    {ok, #state{gc_height = GCHeight}}.

handle_call({push, Vote, STx, Event}, _From, State) ->
    State1 = do_add_vote(Vote, State),
    aec_events:publish(Event, STx),
    {reply, ok, State1};
handle_call({peek, PeekAt}, _From, State) ->
    Ts = do_pool_peek(PeekAt, State),
    {reply, {ok, Ts}, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({gproc_ps_event, top_changed, #{info := #{block_type := key, height := Height}}},
            State) ->
    {noreply, do_update_top(Height, State)};
handle_info({gproc_ps_event, top_changed, #{info := #{block_type := micro}}}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_add_vote(Vote, #state{ hash_pool = HashPool
                                  , t_cache   = TCache
                                  , h_cache   = HCache } = State) ->
    THash = aec_vote:hash(Vote),
    BlockHash = aec_vote:block_hash(Vote),
    case maps:is_key(THash, HashPool) of
        true ->
            lager:debug("Duplicate vote: ~p", [Vote]),
            State;
        false ->
            HashPool1 = HashPool#{THash => Vote},
            Height    = aec_vote:height(Vote),
            T         = {Height, aec_vote:block_hash(Vote)},
            TCache1   = TCache#{T => [THash | maps:get(T, TCache, [])]},
            HCache1   = HCache#{Height => [THash | maps:get(Height, HCache, [])]},
            State#state{ hash_pool = HashPool1
                       , h_cache   = HCache1
                       , t_cache   = TCache1 }
    end.

do_pool_peek(Height, #state{hash_pool = HPool, h_cache = HCache}) when is_integer(Height) ->
    get_votes(maps:get(Height, HCache, []), HPool);
do_pool_peek(T = {_, _}, #state{hash_pool = HPool, t_cache = TCache}) ->
    get_votes(maps:get(T, TCache, []), HPool).

get_votes(THs, HPool) ->
    [ maps:get(TH, HPool) || TH <- THs ].

top_height() ->
    case aec_chain:dirty_top_header() of
        undefined -> 0;
        Header    -> aec_headers:height(Header)
    end.

do_update_top(_Height, State) ->
    %% TODO: do some GC/cleanup
    State.

validate_vote_tx(STx) ->
    {Block, _BlockHash, Trees} = get_onchain_env(),
    Checks = [ fun check_tx_type/3
             , fun check_valid_at_protocol/3
             , fun check_signature/3
             , fun check_ttl/3
             ],
    case aeu_validation:run(Checks, [STx, Block, Trees]) of
        ok ->
            extract_vote(STx);
        Err = {error, _} ->
            Err
    end.

check_tx_type(STx, _Block, _Trees) ->
    case aetx:specialize_type(aetx_sign:tx(STx)) of
        {vote_tx, _} ->
            ok;
        _ ->
            lager:info("Only HCVoteTXs expected in vote_pool: ~p", [STx]),
            {error, only_hc_vote_tx_allowed}
    end.

check_valid_at_protocol(STx, Block, _Trees) ->
    Protocol = aec_blocks:version(Block),
    aetx:check_protocol(aetx_sign:tx(STx), Protocol).

check_signature(STx, Block, Trees) ->
    Protocol = aec_blocks:version(Block),
    case aetx_sign:verify(STx, Trees, Protocol) of
        {error, _} = E ->
            lager:info("Failed signature check on tx: ~p\n", [E]),
            E;
        ok ->
            ok
    end.

check_ttl(STx, Block, _Trees) ->
    Height = aec_blocks:height(Block),
    Tx = aetx_sign:tx(STx),
    case Height > aetx:ttl(Tx) of
        true  -> {error, ttl_expired};
        false -> ok
    end.

-spec extract_vote(aetx_sign:signed_tx()) -> {ok, vote()} | {error, term()}.
extract_vote(STx) ->
    {vote_tx, TTx} = aetx:specialize_type(aetx_sign:tx(STx)),
    Height    = aec_vote_tx:height(TTx),
    WitnessPK = aec_vote_tx:witness_pubkey(TTx),
    BlockHash = aec_vote_tx:block_hash(TTx),
    Signature = aec_vote_tx:signature(TTx),
    Vote = aec_vote:new(Height, BlockHash, WitnessPK, Signature),

    case aec_vote:validate(Vote) of
        true ->
            {ok, Vote};
        false ->
            {error, invalid_vote}
    end.

validate_vote(Vote) ->
    %% TODO: Actually do check something more here!?
    Top = aec_chain:top_block(),
    case aec_vote:height(Vote) > aec_blocks:height(Top) of
        true ->
            {error, vote_from_the_future};
        false ->
            ok
    end.
