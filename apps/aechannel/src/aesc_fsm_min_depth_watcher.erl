
-module(aesc_fsm_min_depth_watcher).
-behaviour(gen_server).

-export([start_link/6,       %% (TxHash, ChanId, MinimumDepth, Mod, Opts) -> {ok, Pid}
         watch/5,            %% (WatcherPid, Type, TxHash, MinimumDepth, Mod) -> ok
         watch_for_channel_close/3,
         watch_for_unlock/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Fetching tx history
-export([get_txs_since/2]).

-define(GEN_SERVER_OPTS, []).

-record(st, { parent
            , chan_id
            , chan_vsn
            , last_block                  %% last time we update channel vsn
            , last_top                    %% the block hash of the last check
            , tx_log = aesc_window:new()
            , rpt_log = aesc_window:new()
            , closing = false
            , requests = []
            , opts = #{} }).

-type mode()       :: close
                    | unlock
                    | tx_hash
                    | watch.

-type req_info() :: #{ type         := any()
                     , parent       := pid()
                     , callback_mod := module() }.

-type close_req() :: #{ mode      := close
                      , min_depth := aec_blocks:height()
                      , info      := req_info() }.
-type unlock_req() :: #{ mode := unlock
                       , info := req_info() }.
-type watch_req() :: #{ mode := watch
                      , info := req_info() }.
-type tx_req() :: #{ mode      := tx_hash
                   , tx_hash   := tx_hash()
                   , min_depth := aec_blocks:height()
                   , info      := req_info() }.

-type req() :: close_req() | unlock_req() | watch_req() | tx_req().
%% -type req() :: map().

-type block_hash() :: binary().
-type tx_hash()    :: binary().
-type scenario()   :: top | next_block | fork_switch | {has_tx, tx_hash()}.
-type chan_vsn()   :: undefined | { aesc_channels:round()
                                  , aesc_channels:solo_round()
                                  , aesc_channels:is_active()
                                  , aesc_channels:locked_until()
                                  , aesc_channels:state_hash() }.
-type ch_status() :: undefined | #{ is_active    := aesc_channels:is_active()
                                  , vsn          := chan_vsn()
                                  , changed      := boolean()
                                  , channel      := aesc_channels:channel()
                                  , lock_period  := aesc_channels:lock_period()
                                  , locked_until := aesc_channels:locked_until() }.

%% -type cache() :: #{ mode := mode()
%%                   , tx_log := aesc_window:window()
%%                   , rpt_log := aesc_window:window()
%%                   , last_block := block_hash()
%%                   , chan_vsn   := chan_vsn()
%%                   , block_hash => block_hash()
%%                   , scenario   => scenario()
%%                   , ch_status  => ch_status()
%%                   , channel    => aesc_channels:channel()
%%                   , tx_hashes  => [tx_hash()] }.
-type cache() :: map().

watch_for_channel_close(Pid, MinDepth, Mod) when is_pid(Pid) ->
    gen_server:call(Pid, close_req(MinDepth, Mod));
watch_for_channel_close(ChanId, MinDepth, Mod) ->
    gen_server:start_link(?MODULE, #{parent  => self(),
                                     chan_id => ChanId,
                                     requests => [close_req(MinDepth, Mod)]},
                          ?GEN_SERVER_OPTS).

watch_for_unlock(Pid, Mod) when is_pid(Pid) ->
    gen_server:call(Pid, unlock_req(Mod));
watch_for_unlock(ChanId, Mod) ->
    gen_server:start_link(?MODULE, #{parent => self(),
                                     chan_id => ChanId,
                                     requests => [unlock_req(Mod)]},
                          ?GEN_SERVER_OPTS).

close_req(MinDepth, Mod) ->
    #{mode         => close,
      min_depth    => MinDepth,
      info         => #{ type  => close
                       , parent => self()
                       , callback_mod => Mod } }.

unlock_req(Mod) ->
    #{mode => unlock,
      info => #{ parent       => self()
               , type         => closing
               , callback_mod => Mod }}.

get_txs_since({all_after_tx, _Hash} = StopCond, ChId) ->
    get_txs_since_(StopCond, ChId);
get_txs_since({any_after_block, _Hash} = StopCond, ChId) ->
    get_txs_since_(StopCond, ChId).

get_txs_since_(StopCond, ChId) ->
    get_txs_since(StopCond, aec_chain:top_block_hash(), ChId, #{}).


start_link(Type, TxHash, ChanId, MinDepth, Mod, Opts) ->
    I = #{ parent       => self()
         , type         => Type
         , callback_mod => Mod },
    DefReqs = [#{ mode      => tx_hash
                , tx_hash   => TxHash
                , min_depth => MinDepth
                , info      => I},
               #{ mode  => watch
                , info  => I }],
    gen_server:start_link(?MODULE, #{parent   => self(),
                                     chan_id  => ChanId,
                                     opts     => Opts,
                                     requests => maps:get(
                                                   requests, Opts, DefReqs)},
                          ?GEN_SERVER_OPTS).

watch(Watcher, Type, TxHash, MinDepth, Mod) ->
    I = #{ callback_mod => Mod
         , type         => Type
         , parent       => self()},
    gen_server:call(Watcher, #{mode         => tx_hash,
                               tx_hash      => TxHash,
                               min_depth    => MinDepth,
                               info         => I }).

init(#{parent := Parent, chan_id := ChanId, requests := Reqs} = I) ->
    Opts = maps:get(opts, I, #{}),
    lager:debug("started min_depth watcher for ~p", [Parent]),
    erlang:monitor(process, Parent),
    true = aec_events:subscribe(top_changed),
    true = aec_events:subscribe({tx_event, {channel, ChanId}}),
    lager:debug("subscribed to top_changed", []),
    self() ! check_status,
    {ok, #st{parent = Parent, chan_id = ChanId, requests = Reqs, opts = Opts}}.


%% Strategy for monitoring the chain.
%%
%% Challenges:
%% 1. There is currently no revenue model for the SC FSM, i.e. the node
%%    running the FSM has no way of getting reimbursed for the cost.
%%    This means the FSM *and* the watcher must be as lightweight as possible.
%% 2. The watcher must keep up with fork switches.
%%
%% To allow for lightweight monitoring, we use custom tx events keyed on the
%% channel id. These are generated at block insertion, and are published
%% *before* any corresponding `top_changed' event.
%%
%% We keep the tx events in a sliding window LIFO buffer. For each `top_changed'
%% event, we need to consider the following scenarios:
%% 1. The new block contains a tx for our channel: Read the channel object
%%    and report accordingly
%% 2. We are waiting for a minimum-depth condition: Verify that it's still
%%    relevant, and check the depth.
%% 3. A fork switch may have happened, changing the channel object even if
%%    no channel tx is present in the top block: Dig into the chain to figure
%%    out the current channel state. Remove txes from the tx_log that have
%%    been returned to the mempool.
%% 4. No fork switch, no channel tx in the top block, and no depth watch:
%%    Do nothing.
handle_info({gproc_ps_event, top_changed, #{info := Hash}}, #st{} = St) ->
    lager:debug("(Fsm = ~p) top_changed: ~p", [St#st.parent, Hash]),
    {noreply, check_status(Hash, St)};
handle_info({gproc_ps_event, {tx_event, {channel, ChId}},
             #{info := #{ block_hash := _BlockHash
                        , tx_hash    := _TxHash
                        , type       := _Type } = I}}, #st{chan_id = ChId} = St) ->
    lager:debug("tx_event: I = ~p", [I]),
    {noreply, log_tx(I, St)};
handle_info({'DOWN', _, process, Parent, _}, #st{parent = Parent} = St) ->
    {stop, normal, St};
handle_info(check_status, St) ->
    {noreply, check_status(top, St)};
handle_info(_Msg, St) ->
    lager:debug("got unknown Msg: ~p", [_Msg]),
    {noreply, St}.

handle_call(#{ mode := _
             , info := #{ type := _
                        , callback_mod := _
                        , parent := _}} = Req, From,
            #st{requests = Reqs} = St) ->
    case maps:find(tx_hash, Req) of
        {ok, TxHash} ->
            case [true || #{tx_hash := TH} <- Reqs,
                          TH =:= TxHash] of
                [] ->
                    gen_server:reply(From, ok),
                    {noreply, check_status(top, St#st{requests = Reqs ++ [Req]})};
                [_|_] ->
                    {reply, {error, {already_watching, TxHash}}, St}
            end;
        error ->
            gen_server:reply(From, ok),
            {noreply, check_status(top, St#st{requests = [Req|Reqs]})}
    end;
handle_call(Req, _From, St) ->
    {reply, {error, {unknown_request, Req}}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

check_status(Hash, #st{ requests   = Reqs
                      , last_block = LastBlock
                      , last_top   = LastTop
                      , tx_log     = TxLog
                      , rpt_log    = RptLog
                      , chan_vsn   = Vsn } = St) ->
    lager:debug("Reqs = ~p", [Reqs]),
    do_dirty(
      fun() ->
              C0 = #{ tx_log     => TxLog
                    , rpt_log    => RptLog
                    , last_block => LastBlock
                    , last_top   => LastTop
                    , chan_vsn   => Vsn},
              C1 = case Hash of
                      top ->
                          {Top, C01} = top_hash(C0),
                          C01#{ block_hash => Top };
                      H ->
                          C0#{ block_hash => H }
                  end,
              C2 = pick_scenario(Hash, St, C1),
              lager:debug("scenario = ~p", [maps:get(scenario, C2)]),
              check_status(Reqs, St, C2, [])
      end).


-spec pick_scenario(block_hash(), #st{}, cache()) -> cache().
pick_scenario(BHash, #st{tx_log = TxLog} = St, C) ->
    case aesc_window:info_find([{block_hash, BHash}], 2, TxLog) of
        false ->
            maybe_fork_switch(BHash, St, C);
        LastTx ->
            C#{scenario => {has_tx, LastTx}}
    end.

maybe_fork_switch(top, _St, C) ->
    C#{scenario => top};
maybe_fork_switch(BHash, #st{last_block = LastBlock}, C) ->
    {Hdr, C1} = get_header(BHash, C),
    S = case aec_headers:prev_hash(Hdr) of
            LastBlock ->
                next_block;
            _ ->
                fork_switch
        end,
    C1#{scenario => S}.


do_dirty(F) ->
    aec_db:transaction(
      fun() ->
              try F()
              catch
                  error:E ->
                      lager:error("CAUGHT ~p / ~p", [E, erlang:get_stacktrace()]),
                      error(E)
              end
      end).

-spec check_status([req()], #st{}, cache(), [req()]) -> #st{}.
check_status([Req|Reqs], St, Cache, Acc) ->
    lager:debug("Req = ~p", [Req]),
    try check_status_(Req, St, Cache) of
        {done, Cache1} when is_map(Cache1) ->
            check_status(Reqs, St, Cache1, Acc);
        %% commented out to please dialyzer:
        %% {done, #st{} = St1, Cache1} when is_map(Cache1) ->
        %%     check_status(Reqs, St1, Cache1, Acc);
        {Req1, Cache1} when is_map(Req1), is_map(Cache1) ->
            check_status(Reqs, St, Cache1, [Req1|Acc]);
        Other ->
            lager:error("BAD return ~p from Req = ~p", [Other, Req]),
            error({bad_return, Other})
        %% {Req1, #st{} = St1, Cache1} when is_map(Req1), is_map(Cache1) ->
        %%     check_status(Reqs, St1, Cache1, [Req1|Acc])
    catch
        error:E ->
            lager:error("CAUGHT ~p / ~p", [E, erlang:get_stacktrace()]),
            error(E)
    end;
check_status([], St, Cache, Acc) ->
    St1 = update_chan_vsn(Cache, St),
    St1#st{requests = lists:reverse(Acc),
           tx_log = maps:get(tx_log, Cache),
           rpt_log = maps:get(rpt_log, Cache)}.

check_status_(#{mode := close, locked_until := H, min_depth := Min} = R,
              #st{chan_id = ChId, chan_vsn = Vsn}, Cache) ->
    if Vsn =/= undefined ->
            {HasDepth, Cache1} = determine_depth_(H, Min, Cache),
            case HasDepth of
                true ->
                    #{callback_mod := Mod, parent := Parent} = R,
                    Mod:minimum_depth_achieved(Parent, ChId, close, undefined),
                    {done, Cache1};
                _ ->
                    {R, Cache1}
            end;
       true ->
            {R, Cache}
    end;
check_status_(#{mode := close} = R, #st{chan_id = ChId, chan_vsn = Vsn}, C) ->
    if Vsn =/= undefined ->
            #{info := #{parent := Parent}} = R,
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
       true ->
            {R, C}
    end;
check_status_(#{mode := unlock} = R, #st{chan_id = ChId, chan_vsn = Vsn} = St, C) ->
    if Vsn =/= undefined ->
            lager:debug("checking", []),
            case get_basic_ch_status_(ChId, C) of
                {#{locked_until := 0} = Ch, C1} ->
                    lager:debug("locked_until = 0", []),
                    report_channel_unlocked(R, Ch, St, C1),
                    {done, C1};
                {#{locked_until := LockedUntil} = Ch, C1} ->
                    {Height, C2} = top_height(C1),
                    lager:debug("LockedUntil = ~p, Height = ~p", [LockedUntil, Height]),
                    case LockedUntil < Height of
                        true  ->
                            lager:debug("locked_until expired", []),
                            report_channel_unlocked(R, Ch, St, C2),
                            {done, C2};
                        false ->
                            lager:debug("still locked", []),
                            {R, C2}
                    end;
                {undefined, C1} ->
                    lager:debug("couldn't find channel", []),
                    {done, C1}
            end;
       true ->
            lager:debug("Vsn = undefined", []),
            {R, C}
    end;
check_status_(#{mode := watch} = R, #st{chan_vsn = Vsn} = St, C) ->
    if Vsn =/= undefined ->
            %% At least wait until there's a channel object
            watch_for_channel_change(R, St, C);
       true ->
            {R, C}
    end;
check_status_(#{mode := tx_hash, tx_hash := TxHash, min_depth := MinDepth} = R,
              #st{chan_id = ChanId} = St, C) ->
    #{info := #{parent := Parent}} = R,
    lager:debug("check_status(tx_hash = ~p, parent = ~p)", [TxHash, Parent]),
    case min_depth_achieved(TxHash, MinDepth, St, C) of
        {true, C1} ->
            lager:debug("min_depth achieved", []),
            #{ info := #{callback_mod := Mod, type := Type} } = R,
            Mod:minimum_depth_achieved(Parent, ChanId, Type, TxHash),
            {done, C1};
        {_, C1} ->
            lager:debug("min_depth not yet achieved", []),
            {R, C1}
    end.

%% watch_for_channel_change(#{callback_mod := Mod, parent := Parent},
%%                           #st{closing = true, chan_id = ChanId}, C) ->
%%     Mod:channel_closing_on_chain(Parent, ChanId),
%%     {done, C};
%% watch_for_channel_change(#{check_at_height := _} = R, St, C) ->
%%     {CurHeight, C1} = top_height(C),
%%     watch_for_channel_change(CurHeight, R, St, C1);
watch_for_channel_change(R, St, #{ scenario   := Scenario } = C) ->
    case Scenario of
        next_block ->
            lager:debug("Will not check channel", []),
            {R, C};
        _ ->
            {CurHeight, C1} = top_height(C),
            lager:debug("Will check channel on chain (~p)", [Scenario]),
            watch_for_channel_change(CurHeight, R, St, C1)
    end.
%% watch_for_channel_change(R, St, C) ->
%%     {CurHeight, C1} = top_height(C),
%%     lager:debug("CurHeight = ~p", [CurHeight]),
%%     watch_for_channel_change(
%%       CurHeight, R#{check_at_height => CurHeight}, St, C1).

watch_for_channel_change(CurHeight, R, #st{chan_id = ChanId} = St, C) ->
    lager:debug("Will check channel status (V=~p)", [St#st.chan_vsn]),
    {Status, C1} = channel_status(ChanId, C),
    lager:debug("Status = ~p", [Status]),
    watch_for_change_in_ch_status(Status, CurHeight, R, St, C1).
%% watch_for_channel_change(_, R, _, C) ->
%%     lager:debug("Will not check now", []),
%%     {R, C}.

watch_for_change_in_ch_status(undefined, _CurHeight, R, _St, C) ->
    lager:debug("No channel object yet", []),
    {R, C};
watch_for_change_in_ch_status(Status, _CurHeight, R, St, C) ->
    case Status of
        #{changed := true} ->
            lager:debug("Channel has changed: ~p", [Status]),
            %% NextHeight = calc_next_height(Status, CurHeight, St),
            #{callback_mod := Mod, parent := Parent} = maps:get(info, R),
            C1 = report_status_change(Status, Mod, Parent, St, C),
            {R, C1};
        _ ->
            lager:debug("No change in channel: ~p", [Status]),
            {R, C}
    end.

report_status_change(#{channel := Ch, is_active := IsActive,
                       tx := SignedTx}, Mod, Parent, St, C) ->
    Event = if IsActive -> changed_on_chain;
               true     -> closing_on_chain
            end,
    BlockHash = maps:get(block_hash, C),
    RptKey = {Event, aetx_sign:hash(SignedTx)},
    Info = #{ chan_id => St#st.chan_id
            , tx => SignedTx
            , channel => Ch
            , block_hash => BlockHash },
    maybe_report(
      RptKey, Info,
      fun() ->
              case Event of
                  changed_on_chain ->
                      Mod:channel_changed_on_chain(Parent, Info);
                  closing_on_chain ->
                      Mod:channel_closing_on_chain(Parent, Info)
              end
      end, C).

maybe_report(RptKey, Info, Rpt, C) when is_function(Rpt, 0) ->
    RptLog = maps:get(rpt_log, C),
    case aesc_window:keyfind(RptKey, 1, RptLog) of
        false ->
            Rpt(),
            C#{rpt_log => aesc_window:add({RptKey, Info}, RptLog)};
        _ ->
            C
    end.

report_channel_unlocked(#{info := #{callback_mod := Mod, parent := Parent}}, Ch, St, C) ->
    BlockHash = maps:get(block_hash, C),
    Mod:channel_unlocked(Parent, #{ chan_id => St#st.chan_id
                                  , channel => Ch
                                  , block_hash => BlockHash }).

%% calc_next_height(Status, CurHeight, #st{opts = Opts}) ->
%%     Incr = case maps:find(skip_blocks, Opts) of
%%                {ok, false} -> 1;
%%                {ok, N} when is_integer(N), N >= 0 -> 1+N;
%%                {ok, auto} -> auto_skip(Status);
%%                error      -> auto_skip(Status)
%%            end,
%%     CurHeight + Incr.

%% auto_skip(#{lock_period := LP}) ->
%%     erlang:max(1, LP div 2);
%% auto_skip(_) ->
%%     1.


channel_status(ChId, #{block_hash := Hash} = C) ->
    cached_get({ch_status, Hash}, C, fun(C1) -> get_ch_status(ChId, Hash, C1) end).

channel_status_changed(V, #{chan_vsn := V0}) ->
    lager:debug("(~p) V = ~p; V0 = ~p", [V =/= V0, V, V0]),
    V =/= V0.

update_chan_vsn(#{block_hash := Hash} = Cache, #st{} = St) ->
    case maps:find({channel, Hash}, Cache) of
        {ok, #{vsn := Vsn, changed := true}} ->
            lager:debug("Update Vsn = ~p", [Vsn]),
            St#st{chan_vsn = Vsn,
                  last_block = Hash};
        _ ->
            St
    end.

get_ch_status(ChId, Hash, C) ->
    case get_basic_ch_status_(ChId, Hash, C) of
        {#{changed := Changed} = Status, C1} ->
            case Changed of
                true ->
                    lager:debug("status has changed", []),
                    handle_ch_status_changed(ChId, C1);
                false ->
                    lager:debug("status hasn't changed", []),
                    {Status, C1}
            end;
        Other ->
            lager:debug("Other = ~p", [Other]),
            Other
    end.

handle_ch_status_changed(ChId, #{ last_block := Last
                                , block_hash := Hash } = C) ->
    lager:debug("channel status changed, Hash=~p, Last=~p", [Hash,Last]),
    {TxLog, C1} = get_txs_since({any_after_block, Last}, Hash, ChId, C),
    lager:debug("New TxLog: ~p", [TxLog]),
    #{tx_hash := Tx, block_hash := BlockHash} = get_latest_tx(TxLog),
    lager:debug("Latest Tx = ~p, Hash = ~p", [Tx, BlockHash]),
    {Status, C2} = get_basic_ch_status_(ChId, BlockHash, C1),
    lager:debug("ChStatus(~p) = ~p", [BlockHash, Status]),
    %% Assert that this is really a changed channel object
    true = channel_status_changed(maps:get(vsn, Status), C2),
    lager:debug("asserted status changed", []),
    Status1 = Status#{tx => Tx},
    {Status1, C2#{{channel, BlockHash} => Status1}}.

get_basic_ch_status_(ChId, #{ block_hash := Hash } = C) ->
    get_basic_ch_status_(ChId, Hash, C).

get_basic_ch_status_(ChId, BlockHash, C) ->
    {Ch, C1} = get_channel(ChId, BlockHash, C),
    case Ch of
        undefined ->
            {undefined, C1};
        _ ->
            Vsn = channel_vsn(Ch),
            Changed = channel_status_changed(Vsn, C1),
            {#{ is_active    => aesc_channels:is_active(Ch)
              , vsn          => Vsn
              , changed      => Changed
              , channel      => Ch
              , lock_period  => aesc_channels:lock_period(Ch)
              , locked_until => aesc_channels:locked_until(Ch) },
             C1}
    end.

get_channel(ChId, Hash, C) ->
    cached_get({channel, Hash}, C, fun() -> get_channel_(ChId, Hash) end).

get_channel_(ChId, Hash) ->
    case aec_chain:get_block_state(Hash) of
        {ok, Trees} ->
            case aec_chain:get_channel(ChId, Trees) of
                {ok, Ch} ->
                    Ch;
                {error, _} ->
                    undefined
            end;
        error ->
            undefined
    end.

cached_get(Key, C, Get) ->
    case maps:find(Key, C) of
        {ok, V} ->
            {V, C};
        error ->
            get_and_cache(Get, Key, C)
    end.

get_and_cache(F, Key, C) when is_function(F, 0) ->
    V = F(),
    {V, C#{ Key => V }};
get_and_cache(F, Key, C) when is_function(F, 1) ->
    {V, C1} = F(C),
    {V, C1#{ Key => V }}.

log_tx(#{ tx_hash      := TxHash
        , block_hash   := BlockHash
        , block_origin := _Origin
        , type         := _Type } = Info, #st{tx_log = TxLog} = St) ->
    Key = {TxHash, BlockHash},
    case aesc_window:keymember(Key, 1, TxLog) of
        true ->
            St;
        false ->
            TxLog1 = aesc_window:add({{TxHash, BlockHash}, Info}, TxLog),
            St#st{tx_log = TxLog1}
    end.

get_latest_tx(Log) ->
    {{{_TxHash, _BlockHash}, Tx}, _TxLog1} = aesc_window:pop(Log),
    lager:debug("Tx = ~p", [Tx]),
    Tx.

%% Find recent txs. Two different stop conditions:
%% - {any_after_block, BlockHash}: Stop as soon as a block is found with
%%   relevant txs in it, or when BlockHash is reached.
%% - {all_after_tx, TxHash}: Find all txs after TxHash. Note that we must
%%   first verify that TxHash exists on chain.
%% Find recent txs related to ChId since LastBlock (not including those in
%% LastBlock) up until, and including those in, Hash.
%% This is like aec_chain:get_transactions_between/2, except it ignores all
%% txs other than channel txs related to ChId, groups by block, etc.
%%
get_txs_since(StopCond, Hash, ChId, C) ->
    case verify_stop_cond(StopCond, C) of
        {ok, C1} ->
            lager:debug("StopCond = ~p", [StopCond]),
            TxLog = maps:get(tx_log, C1),
            {Found, C2} = get_txs_since(StopCond, Hash, ChId, C1, []),
            TxLog1 =
                lists:foldl(
                  fun({BlockHash, Txs}, Acc) ->
                          lists:foldl(
                            fun(TxHash, Acc1) ->
                                    aesc_window:add(
                                      { {TxHash, BlockHash},
                                        #{ tx_hash      => TxHash
                                         , block_hash => BlockHash
                                         , block_origin => chain } }, Acc1)
                            end, Acc, Txs)
                  end, TxLog, Found),
            {TxLog1, C2};
        {{error, Error}, _} ->
            lager:error("Bad StopCond (~p): ~p", [StopCond, Error]),
            error(bad_stop_condition)
    end.

verify_stop_cond({any_after_block, _}, C) ->
    %% assume this is ok (it will be as long as the watcher uses it correctly)
    {ok, C};
verify_stop_cond({all_after_tx, TxHash}, C) ->
    case tx_location(TxHash, C) of
        {BlockHash, C1} when is_binary(BlockHash) ->
            {ok, C1};
        {_, C1} ->
            {{error, tx_not_on_chain}, C1}
    end.


%% The iteration checks most recent block first, but the accumulator is
%% not reversed, so the result will be grouped by block in chronological order,
%% with txs sorted by nonce (so in the order in which they were processed).
%%
get_txs_since({any_after_block, Hash}, Hash, _, C, Acc) ->
    {Acc, C};
get_txs_since(StopCond, Hash, ChId, C, Acc) ->
    {Hdr, C1} = get_header(Hash, C),
    case Hdr of
        undefined ->
            lager:debug("No header for ~p", [Hash]),
            {Acc, C1};
        _ ->
            {Found, C2} = txs_for_chid(ChId, Hash, Hdr, C1),
            Sorted = sort_txs(Found),
            lager:debug("txs in ~p: ~p", [Hash, Found]),
            PrevHash = aec_headers:prev_hash(Hdr),
            case stop_cond(StopCond, PrevHash, Sorted) of
                {true, Sorted1} ->
                    {[{Hash, Sorted1}|Acc], C2};
                false ->
                    get_txs_since(
                      StopCond, PrevHash, ChId, C2, [{Hash, Sorted}|Acc])
            end
    end.

stop_cond({all_after_tx, TxHash}, _PrevHash, Found) ->
    case Found of
        [] -> false;
        [_|_] ->
            tail_after_tx(TxHash, Found)
    end;
stop_cond({any_after_block, Hash}, PrevHash, Found) ->
    if Hash == PrevHash ->
            {true, Found};
       true ->
            has_create_tx(Found)
    end.

%% Safety. If a create_tx exists in Found, it must be the first one.
has_create_tx([]) ->
    false;
has_create_tx([SignedTx|_] = Found) ->
    case aetx:specialize_type(aetx_sign:tx(SignedTx)) of
        {channel_create_tx, _} ->
            {true, Found};
        _ ->
            false
    end.

tail_after_tx(_, []) ->
    false;
tail_after_tx(Hash, [H|T]) ->
    case aetx_sign:hash(H) of
        Hash ->
            {true, T};
        _ ->
            tail_after_tx(Hash, T)
    end.

tx_hashes(BlockHash, BlockHdr, C) ->
    cached_get({tx_hashes, BlockHash}, C,
               fun() -> tx_hashes_(BlockHash, BlockHdr) end).

tx_hashes_(Hash, Hdr) ->
    case aec_headers:type(Hdr) of
        key -> [];
        micro ->
            case aec_db:find_block_tx_hashes(Hash) of
                {value, TxHashes} ->
                    TxHashes;
                not_found ->
                    []
            end
    end.

txs_for_chid(ChId, Hash, Hdr, C) ->
    {TxHashes, C1} = tx_hashes(Hash, Hdr, C),
    lists:foldl(
      fun(TxHash, {Acc, Cx}) ->
              {SignedTx, Cx1} = get_signed_tx(TxHash, Cx),
              lager:debug("SignedTx = ~p", [SignedTx]),
              case is_tx_for_chid(SignedTx, ChId) of
                  true  -> {[SignedTx|Acc], Cx1};
                  false -> {Acc, Cx1}
              end
      end, {[], C1}, TxHashes).

get_signed_tx(TxHash, C) ->
    cached_get({signed_tx, TxHash}, C, fun() -> get_signed_tx_(TxHash) end).

get_signed_tx_(TxHash) ->
    case aec_db:find_signed_tx(TxHash) of
        {value, STx} ->
            STx;
        none ->
            undefined
    end.


sort_txs(Txs) ->
    lists:sort(fun compare_txs/2, Txs).

%% See lists:sort/2: The fun Comp(A,B) should return true if A =< B
compare_txs(SignedA, SignedB) ->
    aetx:nonce(aetx_sign:tx(SignedA)) =< aetx:nonce(aetx_sign:tx(SignedB)).

is_tx_for_chid(undefined, _) -> false;
is_tx_for_chid(SignedTx, ChId) ->
    %% Likely only channel txs have a channel_id/1 callback, so prepare for
    %% 'undef' exceptions.
    {CB, Tx} = aetx:specialize_callback(aetx_sign:tx(SignedTx)),
    PK = try CB:channel_pubkey(Tx)
         catch error:_ -> error end,
    lager:debug("Pubkey(Tx) = ~p", [PK]),
    R = case PK of
            ChId -> true;
            _    -> false
        end,
    lager:debug("(~p) Tx=~p ChId=~p", [R,Tx,ChId]),
    R.


-spec channel_vsn(aesc_channels:channel()) -> chan_vsn().
channel_vsn(Ch) ->
    { aesc_channels:round(Ch)
    , aesc_channels:solo_round(Ch)
    , aesc_channels:is_active(Ch)
    , aesc_channels:locked_until(Ch)
    , aesc_channels:state_hash(Ch) }.


min_depth_achieved(TxHash, MinDepth, #st{chan_id = ChId, chan_vsn = Vsn}, C) ->
    {L, C1} = tx_location(TxHash, C),
    case L of
        undefined ->
            {undefined, C1};
        _ when is_binary(L) ->
            %% If the tx is on chain, perhaps fetch an initial channel object
            C2 = if Vsn == undefined ->
                         {_Status, C2_} = channel_status(ChId, C1),
                         C2_;
                    true -> C1
                 end,
            determine_depth(L, MinDepth, C2)
    end.

tx_location(TxHash, C) ->
    cached_get({location, TxHash}, C, fun(C1) -> tx_location_(TxHash, C1) end).

tx_location_(TxHash, C) ->
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
    {L, update_tx_log(TxHash, L, C)}.

update_tx_log(TxHash, BlockHash, #{tx_log := TxLog} =C)
  when is_binary(BlockHash) ->
    C#{tx_log => aesc_window:add(
                   { {TxHash, BlockHash},
                     #{ tx_hash      => TxHash
                      , block_hash   => BlockHash
                      , block_origin => chain } }, TxLog)};
update_tx_log(_, _, C) ->
    C.

determine_depth(BHash, MinDepth, C) ->
    {H, C1} = height(BHash, C),
    case H of
        undefined ->
            {undefined, C1};
        _ ->
            determine_depth_(H, MinDepth, C1)
    end.

top_height(C) ->
    cached_get(top_height, C, fun(C1) -> top_height_(C1) end).

top_height_(C) ->
    {TopHash, C1} = top_hash(C),
    height(TopHash, C1).

top_hash(C) ->
    cached_get(top_hash, C, fun aec_chain:top_block_hash/0).


height(BHash, C) ->
    cached_get({height, BHash}, C, fun(C1) -> height_(BHash, C1) end).

height_(BHash, C) ->
    {Hdr, C1} = get_header(BHash, C),
    case Hdr of
        undefined -> {undefined, C1};
        _ -> {aec_headers:height(Hdr), C1}
    end.

get_header(BHash, C) ->
    cached_get({header, BHash}, C, fun() -> get_header_(BHash) end).

get_header_(BHash) ->
    case aec_chain:get_header(BHash) of
        {ok, Hdr} -> Hdr;
        error     -> undefined
    end.

determine_depth_(Height, MinDepth, C) ->
    case top_height(C) of
        {undefined, _} = Res -> Res;
        {TopHeight, C1} ->
            {(TopHeight - Height) >= MinDepth, C1}
    end.
