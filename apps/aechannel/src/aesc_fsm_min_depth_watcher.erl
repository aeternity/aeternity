-module(aesc_fsm_min_depth_watcher).
-behaviour(gen_server).

-export([start_link/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(GEN_SERVER_OPTS, []).

start_link(TxHash, ChanId, MinDepth) ->
    gen_server:start_link(?MODULE, #{tx_hash   => TxHash,
                                     chan_id   => ChanId,
                                     fsm       => self(),
                                     min_depth => MinDepth},
                          ?GEN_SERVER_OPTS).

init(#{} = Arg) ->
    lager:debug("started min_depth watcher for ~p", [maps:get(fsm, Arg)]),
    true = aec_events:subscribe(top_changed),
    true = aec_events:subscribe(block_created),
    lager:debug("subscribed to top_changed & block_created", []),
    self() ! check_status,
    {ok, Arg}.

handle_info({gproc_ps_event, top_changed, _}, St) ->
    check_status(St);
handle_info({gproc_ps_event, block_created, _}, St) ->
    check_status(St);
handle_info(check_status, St) ->
    check_status(St);
handle_info(_Msg, St) ->
    lager:debug("got unknown Msg: ~p", [_Msg]),
    {noreply, St, 1000}.


handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

check_status(#{funding_locked := true} = St) ->
    watch_for_chain_transaction(St);
check_status(#{fsm := Fsm, chan_id := ChanId,
               tx_hash := TxHash, min_depth := MinDepth} = St) ->
    lager:debug("check_status(~p)", [Fsm]),
    case min_depth_achieved(TxHash, MinDepth) of
        true ->
            lager:debug("min_depth achieved", []),
            aesc_fsm:own_funding_locked(Fsm, ChanId),
            {noreply, St#{funding_locked => true}};
        _ ->
            lager:debug("min_depth not yet achieved", []),
            {noreply, St, 5000}
    end.

watch_for_chain_transaction(St) ->
    lager:debug("watch_for_chain_transaction(~p)", [St]),
    {noreply, St, 5000}.

min_depth_achieved(TxHash, MinDepth) ->
    case aec_chain:find_transaction_in_main_chain_or_mempool(TxHash) of
        none ->
            lager:debug("couldn't find tx hash", []),
            undefined;
        {mempool, _} ->
            lager:debug("tx still in mempool", []),
            undefined;
        {BlockHash, _} ->
            lager:debug("tx in Block ~p", [BlockHash]),
            determine_depth(BlockHash, MinDepth)
    end.

determine_depth(BHash, MinDepth) ->
    case aec_chain:get_header(BHash) of
        {ok, Hdr} ->
            Height = aec_headers:height(Hdr),
            lager:debug("tx at height ~p", [Height]),
            determine_depth_(Height, MinDepth);
        error ->
            undefined
    end.

determine_depth_(Height, MinDepth) ->
    case aec_chain:top_header() of
        undefined ->
            undefined;
        TopHdr ->
            TopHeight = aec_headers:height(TopHdr),
            lager:debug("top height = ~p", [TopHeight]),
            (TopHeight - Height) >= MinDepth
    end.


%% block_has_channel(ChanId, Block) ->
%%     Txs = aec_blocks:txs(Block),
%%     has_chan_id(Txs, ChanId).

%% has_chan_id([], _) ->
%%     false;
%% has_chan_id([SignedTx|T], ChanId) ->
%%     Tx = aetx_sign:tx(SignedTx),
%%     case aetx:specialize_type(Tx) of
%%         {channel_create_tx, CTx} ->
%%             Initiator   = aesc_create_tx:initiator(CTx),
%%             Responder = aesc_create_tx:responder(CTx),
%%             Nonce       = aesc_create_tx:nonce(CTx),
%%             case aesc_channels:id(Initiator, Nonce, Responder) of
%%                 ChanId -> true;
%%                 _      -> has_chan_id(T, ChanId)
%%             end;
%%         _ ->
%%             has_chan_id(T, ChanId)
%%     end.
