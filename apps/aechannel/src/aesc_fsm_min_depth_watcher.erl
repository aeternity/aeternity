
-module(aesc_fsm_min_depth_watcher).
-behaviour(gen_server).

-export([start_link/5,   %% (TxHash, ChanId, MinimumDepth, Mod)   -> {ok, Pid}
         watch/5,        %% (WatcherPid, Type, TxHash, MinimumDepth, Mod) -> ok
         watch_for_channel_close/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(GEN_SERVER_OPTS, []).

-record(st, { parent
            , chan_id
            , closing = false
            , requests = [] }).

watch_for_channel_close(ChanId, MinDepth, Mod) ->
    gen_server:start_link(?MODULE, #{parent  => self(),
                                     chan_id => ChanId,
                                     requests =>
                                         [#{mode         => close,
                                            min_depth    => MinDepth,
                                            type         => close,
                                            parent       => self(),
                                            callback_mod => Mod}]},
                          ?GEN_SERVER_OPTS).

start_link(Type, TxHash, ChanId, MinDepth, Mod) ->
    gen_server:start_link(?MODULE, #{parent   => self(),
                                     chan_id  => ChanId,
                                     requests =>
                                         [#{mode         => tx_hash,
                                            tx_hash      => TxHash,
                                            type         => Type,
                                            parent       => self(),
                                            callback_mod => Mod,
                                            min_depth    => MinDepth},
                                          #{mode         => watch,
                                            parent       => self(),
                                            callback_mod => Mod}]},
                          ?GEN_SERVER_OPTS).

watch(Watcher, Type, TxHash, MinDepth, Mod) ->
    gen_server:call(Watcher, #{mode         => tx_hash,
                               type         => Type,
                               tx_hash      => TxHash,
                               min_depth    => MinDepth,
                               callback_mod => Mod,
                               parent       => self()}).

init(#{parent := Parent, chan_id := ChanId, requests := Reqs}) ->
    lager:debug("started min_depth watcher for ~p", [Parent]),
    erlang:monitor(process, Parent),
    true = aec_events:subscribe(top_changed),
    lager:debug("subscribed to top_changed", []),
    self() ! check_status,
    {ok, #st{parent = Parent, chan_id = ChanId, requests = Reqs}}.

handle_info({gproc_ps_event, top_changed, _}, #st{} = St) ->
    {noreply, check_status(St)};
handle_info({'DOWN', _, process, Parent, _}, #st{parent = Parent} = St) ->
    {stop, normal, St};
handle_info(check_status, St) ->
    {noreply, check_status(St)};
handle_info(_Msg, St) ->
    lager:debug("got unknown Msg: ~p", [_Msg]),
    {noreply, St}.

handle_call(#{mode := _, callback_mod := _, parent := _} = Req, From,
            #st{requests = Reqs} = St) ->
    case maps:find(tx_hash, Req) of
        {ok, TxHash} ->
            case [true || #{tx_hash := TH} <- Reqs,
                          TH =:= TxHash] of
                [] ->
                    gen_server:reply(From, ok),
                    {noreply, check_status(St#st{requests = Reqs ++ [Req]})};
                [_|_] ->
                    {reply, {error, {already_watching, TxHash}}, St}
            end;
        error ->
            gen_server:reply(From, ok),
            {noreply, check_status(St#st{requests = [Req|Reqs]})}
    end;
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

check_status(#st{requests = Reqs} = St) ->
    check_status(Reqs, St, #{}, []).

check_status([Req|Reqs], St, Cache, Acc) ->
    case check_status_(Req, St, Cache) of
        {done, Cache1} when is_map(Cache1) ->
            check_status(Reqs, St, Cache1, Acc);
        {done, #st{} = St1, Cache1} when is_map(Cache1) ->
            check_status(Reqs, St1, Cache1, Acc);
        {Req1, Cache1} when is_map(Req1) ->
            check_status(Reqs, St, Cache1, [Req1|Acc])
    end;
check_status([], St, _, Acc) ->
    St#st{requests = lists:reverse(Acc)}.

check_status_(#{mode := close, locked_until := H, min_depth := Min} = R,
              #st{chan_id = ChId}, Cache) ->
    {HasDepth, Cache1} = determine_depth_(H, Min, Cache),
    case HasDepth of
        true ->
            #{callback_mod := Mod, parent := Parent} = R,
            Mod:minimum_depth_achieved(Parent, ChId, close, undefined),
            {done, Cache1};
        _ ->
            {R, Cache1}
    end;
check_status_(#{mode := close, parent := Parent} = R, #st{chan_id = ChId}, C) ->
    lager:debug("check_status(type = close, parent = ~p)", [Parent]),
    {TopHeight, C1} = top_height(C),
    {Status, C2} = channel_status(ChId, C1),
    case Status of
        #{ is_active := true } ->
            {R, C2};
        #{ locked_until := LockedUntil } ->
            {R#{ locked_until => LockedUntil }, C2};
        undefined ->
            %% Set closing time to current top height, wait for min_depth
            {R#{ locked_until => TopHeight }, C2}
    end;
check_status_(#{mode := watch} = R, St, C) ->
    watch_for_channel_closing(R, St, C);
check_status_(#{mode := tx_hash, tx_hash := TxHash, parent := Parent,
                type := Type, min_depth := MinDepth} = R,
              #st{chan_id = ChanId}, C) ->
    lager:debug("check_status(tx_hash = ~p, parent = ~p)", [TxHash, Parent]),
    case min_depth_achieved(TxHash, MinDepth, C) of
        {true, C1} ->
            lager:debug("min_depth achieved", []),
            #{ callback_mod := Mod } = R,
            Mod:minimum_depth_achieved(Parent, ChanId, Type, TxHash),
            {done, C1};
        {_, C1} ->
            lager:debug("min_depth not yet achieved", []),
            {R, C1}
    end.

watch_for_channel_closing(#{callback_mod := Mod, parent := Parent},
                          #st{closing = true, chan_id = ChanId}, C) ->
    Mod:channel_closing_on_chain(Parent, ChanId),
    {done, C};
watch_for_channel_closing(#{check_at_height := _} = R, St, C) ->
    {CurHeight, C1} = top_height(C),
    watch_for_channel_closing(CurHeight, R, St, C1);
watch_for_channel_closing(R, St, C) ->
    {CurHeight, C1} = top_height(C),
    watch_for_channel_closing(
      CurHeight, R#{check_at_height => CurHeight}, St, C1).

watch_for_channel_closing(CurHeight, #{check_at_height := H} = R,
                          #st{chan_id = ChanId} = St, C)
  when CurHeight >= H ->
    {Status, C1} = channel_status(ChanId, C),
    case Status of
        #{is_active := true, lock_period := LP} ->
            NextHeight = CurHeight + erlang:min(1, LP div 2),
            {R#{check_at_height => NextHeight}, C1};
        #{is_active := false} ->
            #{callback_mod := Mod, parent := Parent} = R,
            Mod:channel_closing_on_chain(Parent, ChanId),
            {done, St#st{closing = true}, C1};
        undefined ->
            {R, C1}
    end;
watch_for_channel_closing(_, R, _, C) ->
    {R, C}.


channel_status(ChId, C) ->
    channel_status(maps:find({status, ChId}, C), ChId, C).

channel_status({ok, S}, _, C) -> {S, C};
channel_status(error, ChId, C) ->
    St = case aec_chain:get_channel(ChId) of
             {ok, Ch} ->
                 #{ is_active    => aesc_channels:is_active(Ch)
                  , lock_period  => aesc_channels:lock_period(Ch)
                  , locked_until => aesc_channels:locked_until(Ch) };
             _ ->
                 undefined
         end,
    {St, C#{ {status, ChId} => St }}.

min_depth_achieved(TxHash, MinDepth, C) ->
    {L, C1} = tx_location(TxHash, C),
    case L of
        undefined ->
            {undefined, C1};
        _ ->
            determine_depth(L, MinDepth, C1)
    end.

tx_location(TxHash, C) ->
    tx_location(maps:find({location,TxHash}, C), TxHash, C).

tx_location({ok, L}, _, C) -> {L, C};
tx_location(error, TxHash, C) ->
    L = case aec_chain:find_tx_with_location(TxHash) of
            none ->
                lager:debug("couldn't find tx hash", []),
                undefined;
            {mempool, _} ->
                lager:debug("tx still in mempool", []),
                undefined;
            {BlockHash, _} ->
                lager:debug("tx in Block ~p", [BlockHash]),
                BlockHash
        end,
    {L, C#{ {location, TxHash} => L }}.

determine_depth(BHash, MinDepth, C) ->
    {H, C1} = height(BHash, C),
    case H of
        undefined ->
            {undefined, C1};
        _ ->
            determine_depth_(H, MinDepth, C1)
    end.

top_height(#{top_height := H} = C) -> {H, C};
top_height(C) ->
    TopHeight = case aec_chain:top_header() of
                    undefined ->
                        undefined;
                    TopHdr ->
                        aec_headers:height(TopHdr)
                end,
    {TopHeight, C#{ top_height => TopHeight }}.

height(BHash, C) ->
    height(maps:find({height, BHash}, C), BHash, C).

height({ok, H}, _BHash, C) -> {H, C};
height(error, BHash, C) ->
    Height = case aec_chain:get_header(BHash) of
                 {ok, Hdr} ->
                     H = aec_headers:height(Hdr),
                     lager:debug("tx at height ~p", [H]),
                     H;
                 error ->
                     undefined
             end,
    {Height, C#{ {height, BHash} => Height }}.

determine_depth_(Height, MinDepth, C) ->
    case top_height(C) of
        {undefined, _} = Res -> Res;
        {TopHeight, C1} ->
            {(TopHeight - Height) >= MinDepth, C1}
    end.

