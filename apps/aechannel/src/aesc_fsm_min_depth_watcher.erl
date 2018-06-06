
-module(aesc_fsm_min_depth_watcher).
-behaviour(gen_server).

-export([start_link/4,   %% (TxHash, ChanId, MinimumDepth)     -> {ok, Pid}
         watch/4]).      %% (WatcherPid, Type, TxHash, MinimumDepth) -> ok

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(GEN_SERVER_OPTS, []).

start_link(Type, TxHash, ChanId, MinDepth) ->
    gen_server:start_link(?MODULE, #{tx_hash   => TxHash,
                                     chan_id   => ChanId,
                                     type      => Type,
                                     fsm       => self(),
                                     min_depth => MinDepth},
                          ?GEN_SERVER_OPTS).

watch(Watcher, Type, TxHash, MinDepth) ->
    gen_server:call(Watcher, {watch, Type, TxHash, MinDepth}).

init(#{} = Arg) ->
    lager:debug("started min_depth watcher for ~p", [maps:get(fsm, Arg)]),
    true = aec_events:subscribe(top_changed),
    lager:debug("subscribed to top_changed", []),
    self() ! check_status,
    {ok, Arg}.

handle_info({gproc_ps_event, top_changed, _}, St) ->
    check_status(St);
handle_info(check_status, St) ->
    check_status(St);
handle_info(_Msg, St) ->
    lager:debug("got unknown Msg: ~p", [_Msg]),
    {noreply, St}.


handle_call({watch, Type, TxHash, MinDepth}, From, #{tx_hash := TxHash0} = St) ->
    case TxHash0 of
        undefined ->
            gen_server:reply(From, ok),
            check_status(St#{tx_hash => TxHash,
                             type    => Type,
                             min_depth => MinDepth});
        Existing ->
            {reply, {error, {already_watching, Existing}}, St}
    end;
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

check_status(#{tx_hash := undefined} = St) ->
    watch_for_chain_transaction(St);
check_status(#{fsm := Fsm, chan_id := ChanId,
               tx_hash := TxHash, type := Type, min_depth := MinDepth} = St) ->
    lager:debug("check_status(~p)", [Fsm]),
    case min_depth_achieved(TxHash, MinDepth) of
        true ->
            lager:debug("min_depth achieved", []),
            aesc_fsm:minimum_depth_achieved(Fsm, ChanId, Type, TxHash),
            {noreply, St#{funding_locked => true,
                          type    => undefined,
                          tx_hash => undefined}};
        _ ->
            lager:debug("min_depth not yet achieved", []),
            {noreply, St}
    end.

watch_for_chain_transaction(St) ->
    lager:debug("watch_for_chain_transaction(~p)", [St]),
    {noreply, St}.

min_depth_achieved(TxHash, MinDepth) ->
    case aec_chain:find_tx_with_location(TxHash) of
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

