-module(aesc_fsm_min_depth_watcher).
-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(ChanId, MinDepth) ->
    gen_server:start_link(?MODULE, #{chan_id   => ChanId,
                                     fsm       => self(),
                                     min_depth => MinDepth}, []).

init(#{} = Arg) ->
    true = aec_events:subscribe(top_changed),
    {ok, Arg}.

handle_info({gproc_ps_event, top_changed, #{info := Block}},
            #{chan_id   := ChanId,
              min_depth := MinDepth,
              fsm       := Fsm} = St) ->
    CurHeight = aec_blocks:height(Block),
    case maps:find(channel_at, St) of
        {ok, ChHeight} when CurHeight - ChHeight >= MinDepth ->
            aesc_fsm:own_funding_locked(Fsm, ChanId),
            {stop, normal, St};
        {ok, ChHeight} when CurHeight - ChHeight < MinDepth ->
            {noreply, St};
        error ->
            case block_has_channel(ChanId, Block) of
                true when MinDepth == 0 ->
                    aesc_fsm:own_funding_locked(Fsm, ChanId),
                    {stop, normal, St};
                true ->
                    {noreply, St#{channel_at => CurHeight}};
                false ->
                    {noreply, St}
            end
    end;
handle_info(_Msg, St) ->
    {noreply, St}.


handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.


block_has_channel(ChanId, Block) ->
    Txs = aec_blocks:txs(Block),
    has_chan_id(Txs, ChanId).

has_chan_id([], _) ->
    false;
has_chan_id([SignedTx|T], ChanId) ->
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_type(Tx) of
        {channel_create_tx, CTx} ->
            Initiator   = aesc_create_tx:initiator(CTx),
            Participant = aesc_create_tx:participant(CTx),
            Nonce       = aesc_create_tx:nonce(CTx),
            case aesc_channels:id(Initiator, Nonce, Participant) of
                ChanId -> true;
                _      -> has_chan_id(T, ChanId)
            end;
        _ ->
            has_chan_id(T, ChanId)
    end.
