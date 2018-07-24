
-module(aesc_fsm_min_depth_watcher).
-behaviour(gen_server).

-export([start_link/4,   %% (TxHash, ChanId, MinimumDepth)     -> {ok, Pid}
         watch/4,        %% (WatcherPid, Type, TxHash, MinimumDepth) -> ok
         watch_for_channel_close/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(GEN_SERVER_OPTS, []).

watch_for_channel_close(ChanId, MinDepth, Mod) ->
    gen_server:start_link(?MODULE, #{type         => close,
                                     chan_id      => ChanId,
                                     min_depth    => MinDepth,
                                     parent       => self(),
                                     callback_mod => Mod},
                          ?GEN_SERVER_OPTS).

start_link(Type, TxHash, ChanId, MinDepth) ->
    gen_server:start_link(?MODULE, #{tx_hash   => TxHash,
                                     chan_id   => ChanId,
                                     type      => Type,
                                     parent    => self(),
                                     min_depth => MinDepth},
                          ?GEN_SERVER_OPTS).

watch(Watcher, Type, TxHash, MinDepth) ->
    gen_server:call(Watcher, {watch, Type, TxHash, MinDepth}).

init(#{parent := Parent} = Arg) ->
    lager:debug("started min_depth watcher for ~p", [Parent]),
    erlang:monitor(process, Parent),
    true = aec_events:subscribe(top_changed),
    lager:debug("subscribed to top_changed", []),
    self() ! check_status,
    {ok, Arg}.

handle_info({gproc_ps_event, top_changed, _},
            #{type := close, closes_at := H, min_depth := Min} = St) ->
    case determine_depth_(H, Min) of
        true ->
            #{callback_mod := Mod, parent := Parent, chan_id := ChanId} = St,
            case Mod:minimum_depth_achieved(
                   Parent, ChanId, close, undefined) of
                continue ->
                    {noreply, St};
                stop ->
                    {stop, normal, St}
            end;
        _ ->
            {noreply, St}
    end;
handle_info({gproc_ps_event, top_changed, _}, St) ->
    check_status(St);
handle_info({'DOWN', _, process, Parent, _}, #{parent := Parent} = St) ->
    {stop, normal, St};
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

check_status(#{type := close, chan_id := ChId, parent := Parent} = St) ->
    lager:debug("check_status(type = close, parent = ~p)", [Parent]),
    TopHeight = top_height(),
    St1 =
        case aec_chain:get_channel(ChId) of
            {ok, Ch} ->
                case aesc_channels:is_active(Ch) of
                    true ->
                        St;
                    false ->
                        St#{closes_at => aesc_channels:closes_at(Ch)}
                end;
            {error, _} ->
                %% Set closing time to current top height, wait for min_depth
                St#{closes_at => TopHeight}
        end,
    {noreply, St1};
check_status(#{tx_hash := undefined} = St) ->
    watch_for_chain_transaction(St);
check_status(#{parent := Parent, chan_id := ChanId,
               tx_hash := TxHash, type := Type, min_depth := MinDepth} = St) ->
    lager:debug("check_status(~p)", [Parent]),
    case min_depth_achieved(TxHash, MinDepth) of
        true ->
            lager:debug("min_depth achieved", []),
            case aesc_fsm:minimum_depth_achieved(
                   Parent, ChanId, Type, TxHash) of
                ok ->
                    {noreply, St#{funding_locked => true,
                                  type    => undefined,
                                  tx_hash => undefined}};
                stop ->
                    {stop, normal, St}
            end;
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

top_height() ->
    TopHeight = case aec_chain:top_header() of
                    undefined ->
                        undefined;
                    TopHdr ->
                        aec_headers:height(TopHdr)
                end,
    lager:debug("top height = ~p", [TopHeight]),
    TopHeight.

determine_depth_(Height, MinDepth) ->
    case top_height() of
        undefined ->
            undefined;
        TopHeight ->
            (TopHeight - Height) >= MinDepth
    end.

