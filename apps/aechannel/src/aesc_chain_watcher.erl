-module(aesc_chain_watcher).
-behaviour(gen_server).

-export([start_link/0]). %% () -> {ok, Pid}

%% New client API
-export([ register/3     %% (ChId, Mod, Reqs) -> ok
        , request/2      %% (ChId, Req)       -> ok | error()    (register first)
        ]).

%% Request objects
-export([ watch_req/0          %% ()                       -> watch_req()
        , min_depth_req/3      %% (TxHash, MinDepth, Type) -> tx_req()
        , close_req/1          %% (MinDepth)               -> close_req()
        , unlock_req/0 ]).     %% ()                       -> unlock_req()

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-export([table_names/0]).
-export([get_cache_reinit_interval/0]).
-endif.

%% Fetching tx history
-export([get_txs_since/2]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").

-define(SERVER, ?MODULE).

-define(CACHE_REINIT_INTERVAL, 5).    %% TODO: presumably raise this number

%% ======================================================================
%% ETS tables

%% Main request table: ordered on {ChId, Type, Client}
-define(T_REQUESTS    , aesc_chain_watcher_reqs).

%% Mapping of Client pid to Request
-define(T_CLIENT2REQ  , aesc_chain_watcher_client2req).

%% Mapping of Client pid to Channel Id
-define(T_CLIENT2CH   , aesc_chain_watcher_client2ch).

%% Current channel state (one entry per ch+client)
%% Held in ETS to survive cache reset
-define(T_CH_STATES   , aesc_chain_watcher_ch_state).

%% Ordered table of {Height, ReqKey} to keep track of requests to be checked
%% at a certain height. New requests get check_at_height=0 to ensure that they
%% get checked at the next pass.
-define(T_AT_HEIGHT   , aesc_chain_watcher_at_height).

%% Audit table (set) with only the channel id
-define(T_AUDIT       , aesc_chain_watcher_audit).

%% Monitor references per client - only one for each client: {Pid, MRef} (set)
-define(T_MREFS       , aesc_chain_watcher_mrefs).

%% ======================================================================

-define(GEN_SERVER_OPTS, []).
-define(IS_SCENARIO(S),
        ( (S =:= top)
          orelse (S =:= next_block)
          orelse (S =:= fork_switch)
          orelse ( is_tuple(S)
                   andalso (tuple_size(S) =:= 2)
                   andalso (element(1, S) =:= has_tx)
                   andalso (is_map(element(2, S)))
                   %% andalso ?IS_INFO_OF_SCENARIO_HAS_TX(element(2, S))
                 )
        ) ).

-record(st, { parent
            , last_block                  %% last time we updated channel vsn
            , last_top                    %% the block hash of the last check
            , tx_log = aesc_window:new() :: tx_log()
            , rpt_log = aesc_window:new() :: rpt_log()
            , cache
            , cache_init_height}).

-record(tx_log_entry,
        { key   :: tx_log_entry_key()
        , value :: tx_log_entry_value()
        }).

-record(rpt_log_entry,
        { key   :: {changed_on_chain | closing_on_chain | closed_on_chain, tx_hash(), pid()}
        , value :: #{ atom() := term() }
        }).

-type ch_id() :: binary().

%% In the initial version, requests are primarily keyed by watcher pid
-type req_key() :: { ch_id()
                   , req_key_() | '_'
                   , pid()      | '_' }.

-type req_key_() :: {tx_hash, tx_hash()}
                  | {watch  , ch_id()}
                  | {close  , ch_id()}
                  | {unlock , ch_id()}.

-record(req, { key       :: req_key()
             , mode      :: mode()    | '_'
             , type      :: any()     | '_'
             , ch_id     :: ch_id()   | '_'
             , client    :: pid()     | '_'
             , module    :: atom()    | '_'
             , info      :: map()     | '_' }).

-record(ch_state, { key   :: {ch_id(), pid()}
                  , vsn   :: chan_vsn()
                  , block :: block_hash()
                  , info  :: map() }).

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

-type block_hash()  :: binary().
-type tx_hash()     :: binary().
-type tx_location() :: {block_hash(), aetx:tx_type(), aetx_sign:signed_tx()}.
-type info_of_scenario_has_tx() :: {tx_log_entry_key(), tx_log_entry_value()}.

-type scenario()   :: top
                    | next_block
                    | fork_switch
                    | {has_tx, info_of_scenario_has_tx()}.

-type chan_vsn()   :: undefined
                    | { aesc_channels:round()
                      , aesc_channels:solo_round()
                      , aesc_channels:is_active()
                      , aesc_channels:locked_until()
                      , aesc_channels:state_hash() }
                    | #{closed_at := #{ block_hash := block_hash()
                                      , height     := aec_blocks:height() }}.

-type ch_status() :: undefined
                   | closed
                   | #{ is_active    := aesc_channels:is_active()
                      , vsn          := chan_vsn()
                      , changed      := boolean()
                      , channel      := aesc_channels:channel()
                      , lock_period  := aesc_channels:lock_period()
                      , locked_until := aesc_channels:locked_until() }.

-type block_info() :: #{ block_hash := block_hash()
                       , prev_hash  := block_hash()
                       , block_type := key | micro
                       , height     := aec_blocks:height() }.

-type tx_log_entry_key() :: block_hash().

-type tx_log_entry_value() :: #{ txs := map() }.

-type tx_log() :: aesc_window:window(#tx_log_entry{}).

-type rpt_log() :: aesc_window:window(#rpt_log_entry{}).

-type cache() :: #{ mode := mode()
                  , tx_log := tx_log()
                  , rpt_log := rpt_log()
                  , last_block := block_hash()
                  , chan_vsn   := chan_vsn()
                  , block_hash => block_hash()
                  , scenario   := scenario()
                  , {location , tx_hash()}  => tx_location()
                  , {signed_tx, tx_hash()}  => aetx_sign:signed_tx()
                  , {ch_status    , block_hash()} => ch_status()
                  , {block_info   , block_hash()} => block_info()
                  , {height       , block_hash()} => non_neg_integer()
                  , {header       , block_hash()} => aec_headers:header()
                  , {in_main_chain, block_hash()} => boolean()
                  , {channel      , block_hash()} => aesc_channels:channel()
                  , {tx_hashes    , block_hash()} => [tx_hash()] }.

watch_req() ->
    watch_req(#{}).

watch_req(I) ->
    I#{ mode => watch }.

min_depth_req(TxHash, MinDepth, ReqType) ->
    #{ mode      => tx_hash
     , tx_hash   => TxHash
     , min_depth => MinDepth
     , type      => ReqType }.

close_req(MinDepth) ->
    #{ mode         => close
     , min_depth    => MinDepth
     , type         => close }.

unlock_req() ->
    #{ mode => unlock
     , type => closing }.

req_type(#req{ info = #{ type := Type }}) ->
    Type.

get_txs_since({all_after_tx, _Hash} = StopCond, ChId) ->
    get_txs_since_(StopCond, ChId);
get_txs_since({any_after_block, _Hash} = StopCond, ChId) ->
    get_txs_since_(StopCond, ChId).

get_txs_since_(StopCond, ChId) ->
    get_txs_since(StopCond, aec_chain:top_block_hash(), ChId, #{}).

ensure_ets_tables() ->
    [{T0,_}|_] = Tabs = tabs(),
    case ets:info(T0, type) of
        undefined ->
            [ets:new(T, Os) || {T, Os} <- Tabs];
        _ ->
            %% Tables exist
            ok
    end,
    ok.

give_away_ets_tables(Pid) ->
    Tabs = tabs(),
    [ets:give_away(T, Pid, ?MODULE) || {T,_} <- Tabs],
    ok.

inherit_ets_tables(Parent) ->
    inherit_ets_tables([T || {T,_} <- tabs()], Parent).

inherit_ets_tables([], _) ->
    ok;
inherit_ets_tables([T|Ts], Parent) ->
    receive
        {'ETS-TRANSFER', T, Parent, ?MODULE} ->
            inherit_ets_tables(Ts, Parent)
    after 5000 ->
            error(timeout)
    end.

-ifdef(TEST).
table_names() ->
    [ T || {T, _} <- tabs()].

get_cache_reinit_interval() ->
    ?CACHE_REINIT_INTERVAL.
-endif.

tabs() ->
    %% The `heir` option ensures that the initial creator of the
    %% tables (the supervisor) inherits the tables if the worker
    %% dies.
    Opts = [public, named_table, {heir, self(), ?MODULE}],
    [
      {?T_REQUESTS    , [ordered_set, {keypos, #req.key} | Opts]}
    , {?T_CLIENT2REQ  , [ordered_set, {keypos, 1} | Opts]}
    , {?T_CLIENT2CH   , [ordered_set, {keypos, 1} | Opts]}
    , {?T_MREFS       , [set, {keypos, 1} | Opts]}
    , {?T_CH_STATES   , [ordered_set, {keypos, #ch_state.key} | Opts]}
    , {?T_AT_HEIGHT   , [ordered_set | Opts]}
    , {?T_AUDIT       , [set | Opts]}
    ].

register(ChId, Mod, Reqs) when is_binary(ChId)
                             , is_atom(Mod)
                             , is_list(Reqs) ->
    gen_server:call(?SERVER, {register, ChId, Mod, Reqs}).

request(ChId, Req) ->
    lager:debug("Req = ~p", [Req]),
    gen_server:call(?SERVER, {request, ChId, Req}).

start_link() ->
    %% Create the ets tables in the parent process (the supervisor).
    %% Using the `heir` option (see `tabs()`), the parent gets the tables back
    %% if the worker dies. When this function is again called to restart the
    %% worker, the tables will already be there (this is the point), and the
    %% `ensure_ets_tables()` function needs to handle that.
    ensure_ets_tables(),
    {ok, Pid} = gen_server:start_link(
                  {local, ?SERVER}, ?MODULE, #{parent => self()}, []),
    give_away_ets_tables(Pid),
    {ok, Pid}.

init(#{parent := Parent}) ->
    process_flag(trap_exit, true),
    true = aec_events:subscribe(top_changed),
    lager:debug("subscribed to top_changed", []),
    %% When inheriting the ets tables from the parent, some messages will
    %% be generated by the runtime system, and we want to receive those in-line
    %% (otherwise, they tend to appear as unhandled `handle_info()`).
    %% Ack to the parent, then handle the table inheritance, then enter the
    %% gen_server loop.
    proc_lib:init_ack(Parent, {ok, self()}),
    inherit_ets_tables(Parent),
    gen_server:enter_loop(?MODULE, ?GEN_SERVER_OPTS, #st{}, {local, ?SERVER}).

%% ============================================================
%% Data storage

store_req(#{ mode := Mode } = R, ChanId, Pid, Module) ->
    Key = new_req_key(R, ChanId, Pid),
    ets:insert(?T_CLIENT2REQ, {{Pid, Key}}),
    write_req(#req{ key    = Key
                  , mode   = Mode
                  , ch_id  = ChanId
                  , client = Pid
                  , module = Module
                  , info   = maps:without([mode], R) }).

new_req_key(#{ mode := tx_hash
             , tx_hash := TxHash }, ChanId, Pid) ->
    new_req_key_({tx_hash, TxHash}, ChanId, Pid);
new_req_key(#{ mode := M }, ChanId, Pid) when M == watch; M == close; M == unlock ->
    new_req_key_(M, ChanId, Pid).

new_req_key_(Mode, ChanId, Pid) ->
    {ChanId, Mode, Pid}.

write_req(#req{key = Key, client = C, info = #{check_at_height := H}} = R) ->
    lager:debug("Inserting check_at_height H=~p", [H]),
    ets:insert(?T_AT_HEIGHT, {{H, Key}}),
    ets:insert(?T_REQUESTS, R),
    ets:insert(?T_CLIENT2REQ, {{C, Key}}),
    ok;
write_req(#req{key = Key, client = C} = R) ->
    ets:insert(?T_REQUESTS, R),
    ets:insert(?T_CLIENT2REQ, {{C, Key}}),
    ok.

delete_req(#req{key = Key, client = C} = R) ->
    ets:delete(?T_REQUESTS, Key),
    ets:delete(?T_CLIENT2REQ, {C, Key}),
    clear_check_at_height(R),
    ok.

delete_pid(Pid) ->
    ReqKeys = ets:select( ?T_CLIENT2REQ, [{ {{Pid,'$1'}}, [], ['$1'] }]),
    _ = ets:select_delete(?T_CLIENT2REQ, [{ {{Pid,'_' }}, [], [true] }]),
    %%
    ChIds = ets:select(?T_CLIENT2CH, [{ {{Pid,'$1'},'_'}, [], ['$1'] }]),
    ets:select_delete( ?T_CLIENT2CH, [{ {{Pid,'_' },'_'}, [], [true] }]),
    %%
    [ delete_req_by_key_(Key) || Key <- ReqKeys ],
    [ ets:delete(?T_CH_STATES, {Pid, ChId}) || ChId <- ChIds ],
    delete_monitor(Pid),
    ok.

delete_req_by_key_(Key) ->
    %% Assumes that the ?CLIENT2REQ mapping is already gone
    case ets:lookup(?T_REQUESTS, Key) of
        [] ->
            ok;
        [R] ->
            ets:delete(?T_REQUESTS, Key),
            clear_check_at_height(R),
            ok
    end.

%% ============================================================

ensure_check_at_height(#{check_at_height := _} = R) ->
    R;
ensure_check_at_height(R) when is_map(R) ->
    %% Set to something guaranteed to be < CurHeight
    R#{check_at_height => 0}.

clear_check_at_height(#req{info = #{check_at_height := H} = I, key = Key} = R) ->
    ets:delete(?T_AT_HEIGHT, {H, Key}),
    R#req{info = maps:remove(check_at_height, I)};
clear_check_at_height(R) ->
    R.

clear_locked_until(#req{info = I} = R) when is_map(I) ->
    R#req{info = maps:remove(locked_until, I)}.

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
handle_info({gproc_ps_event, top_changed, #{info := Info}}, #st{} = St) ->
    lager:debug("top_changed: ~p", [Info]),
    {noreply, check_status(Info, St)};
handle_info({gproc_ps_event, {tx_event, {channel, ChId}},
             #{info := #{ block_hash := _BlockHash
                        , tx_hash    := _TxHash
                        , type       := _TxType } = I}}, St) ->
    %% We receive tx events for channels that we subscribe to. Therefore,
    %% no need to check if the ch is relevant (and no disaster if it has
    %% just been deleted.)
    lager:debug("tx_event (ChId=~p): I = ~p", [ChId, I]),
    {noreply, log_tx(I#{chan_id => ChId}, St)};
handle_info({'DOWN', _MRef, process, Pid, _}, #st{} = St) ->
    ets:delete(?T_MREFS, Pid),
    delete_pid(Pid),
    {noreply, St};
handle_info(_Msg, St) ->
    lager:debug("got unknown Msg: ~p", [_Msg]),
    {noreply, St}.

handle_call({register, ChId, Mod, Reqs}, {Pid,_}, St) ->
    lager:debug("FSM registering with min_depth watcher. ChId = ~p, Client = ~p", [ChId, Pid]),
    case ets:member(?T_CLIENT2CH, {Pid, ChId}) of
        true ->
            {reply, {error, already_registered}, St};
        false ->
            ensure_monitor(Pid),
            ets:insert(?T_CLIENT2CH, {{Pid, ChId}, #{module => Mod}}),
            %% We may already be subscribed, since several procs may register for ChId
            true = aec_events:ensure_subscription({tx_event, {channel, ChId}}),
            _ = [store_req(ensure_check_at_height(R), ChId, Pid, Mod) || R <- Reqs],
            {reply, ok, St}
    end;
handle_call({request, ChId, Req}, {Pid, _}, St) ->
    case get_registered(ChId, Pid) of
        {ok, #{module := Mod} = Info} ->
            lager:debug("Registered (Pid=~p, ChId=~p): ~p", [Pid, ChId, Info]),
            store_req(ensure_check_at_height(Req#{info => Info}), ChId, Pid, Mod),
            {reply, ok, St};
        error ->
            lager:debug("None registered for Pid=~p, ChId=~p", [Pid, ChId]),
            {reply, {error, unknown_channel}, St}
    end;
handle_call(Req, _From, St) ->
    lager:debug("Unknown request, From=~p: ~p", [_From, Req]),
    {reply, {error, {unknown_request, Req}}, St}.

handle_cast(audit_one_chid, St) ->
    %% After a fork switch, check each ChId in turn.
    lager:debug("Got 'audit_one_chid' request", []),
    {noreply, audit_one_chid(St)};
handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

get_registered(ChId, Pid) ->
    case ets:lookup(?T_CLIENT2CH, {Pid, ChId}) of
        [{_, Info}] ->
            {ok, Info};
        [] ->
            error
    end.

audit_one_chid(St) ->
    case ets:first(?T_AUDIT) of
        '$end_of_table' ->
            St;
        ChId ->
            lager:debug("Will audit channel ~p", [ChId]),
            Reqs = requests(ChId),
            check_requests(Reqs, St)
    end.

check_status(#{ prev_hash  := PHash
              , block_hash := BHash
              , height     := Height} = I, #st{ last_top = PHash
                                              , tx_log   = TxLog } = St) ->
    %% Next successive block.
    %% With a little luck, we don't need to touch the chain
    case aesc_window:keyfind(BHash, #tx_log_entry.key, TxLog) of
        false ->
            lager:debug("No tx in top", []),
            case reqs_at_height(Height) of
                [] ->
                    lager:debug("No reqs at height ~p", [Height]),
                    St#st{ last_top = BHash };
                [_|_] = Reqs ->
                    lager:debug("Reqs at height ~p: ~p", [Height, Reqs]),
                    St1 = init_cache(I, next_block, St),
                    check_requests(Reqs, St1)
            end;
        #tx_log_entry{value = #{txs := Txs}} = LogEntry ->
            lager:debug("found ~p txs in top: ~p", [maps:size(Txs), LogEntry]),
            case reqs_for_txs(Txs) of
                {[], []} ->
                    St;
                {ChIds, Reqs} ->
                    lager:debug("Reqs for txs found, ChIds = ~p, Reqs = ~p",
                                [ChIds, [R#req.key || R <- Reqs]]),
                    St1 = init_cache(I, {has_tx, Txs}, St),
                    St2 = save_tx_info(ChIds, Txs, BHash, Height, St1),
                    St3 = log_entry_txs_to_history(ChIds, Txs, BHash, Height, St2),
                    check_requests(Reqs, St3)
            end
    end;
check_status(I, St) ->
    try check_status_on_fork_switch(I, St)
    ?_catch_(error, E, StackTrace)
         lager:error("CAUGHT ~p / ~p", [E, StackTrace]),
         lager:error("Smaller trace ~p", [compact_trace(StackTrace)]),
         error(E)
    end.

check_status_on_fork_switch(I, St) ->
    lager:debug("Assuming fork switch: I = ~p, St = ~p", [I, St]),
    St1 = init_cache(I, fork_switch, St),
    %% We need to go through all requests, but don't want to do it in a blocking
    %% action. Flag each channel id for audit and tell the watcher to check one
    %% at a time.
    %%
    %% TODO: A better strategy for handling fork switches might be to check
    %% which blocks were evicted (that we've seen) and which channel ids
    %% have been touched in those blocks.
    case all_channel_ids() of
        [] ->
            lager:debug("No channels - done", []),
            check_done(St1);
        [_|_] = ChIds ->
            [ ets:insert(?T_AUDIT, {ChId}) || ChId <- ChIds ],
            gen_server:cast(self(), audit_one_chid)
    end,
    check_done(St1).

%% May involve chain access
check_requests(Reqs, #st{ cache = C} = St) ->
    do_dirty(
      fun() ->
              check_requests(Reqs, St, C, [])
      end).

%% Trap exceptions INSIDE the db activity for meaningful error reporting
check_requests(Reqs, St, C, []) ->
    try begin
            Reqs1 = reset_if_fork_switch(Reqs, C),
            check_requests_(Reqs1, St, C)
        end
    ?_catch_(EType, E, EType==error; EType==exit, StackTrace)
        lager:error("CAUGHT ~p:~p / ~p", [EType, E, StackTrace]),
        lager:error("Smaller trace ~p", [compact_trace(StackTrace)]),
         error(E)
    end.

compact_trace([{M,F,As,Info}|T]) when is_list(As) ->
    [{M,F,length(As),Info}|compact_trace(T)];
compact_trace([H|T]) ->
    [H|compact_trace(T)];
compact_trace([]) ->
    [].


%% If there's a fork switch, don't assume channel state or depth calculations
reset_if_fork_switch(Reqs, #{scenario := S}) when ?IS_SCENARIO(S) ->
    case S of
        fork_switch ->
            reset_after_fork_switch(Reqs);
        _ ->
            Reqs
    end.

reset_after_fork_switch(Reqs) when is_list(Reqs) ->
    [reset_req_after_fork_switch(R) || R <- Reqs].

reset_req_after_fork_switch(R) ->
    clear_check_at_height( clear_locked_until(R) ).

init_cache(#{ block_hash := BHash
            , prev_hash  := PHash
            , block_type := BlockType
            , height     := Height }, Scenario, St) ->
    {C0, InitHeight} = init_cache_(Height, St),
    %% Here, we already have all the top_info, so cache it
    C1 = C0#{ scenario        => Scenario
            , top_hash        => BHash
            , top_height      => Height
            , {height, BHash} => Height
            , top_info => #{ block_hash => BHash
                           , prev_hash  => PHash
                           , block_type => BlockType
                           , height     => Height } },
    St#st{ cache = C1
         , cache_init_height = InitHeight }.

init_cache_(Height, #st{ cache_init_height = Hi
                       , cache      = SavedCache
                       , last_block = LastBlock
                       , last_top   = LastTop
                       , tx_log     = TxLog
                       , rpt_log    = RptLog }) ->
    {C0, CIH} = if is_integer(Hi), Hi < (Height - ?CACHE_REINIT_INTERVAL) ->
                        {SavedCache, Hi};
                   true ->
                        {#{ init_height => Height }, Height}
                end,
    C = C0#{ tx_log     => TxLog
           , rpt_log    => RptLog
           , last_block => LastBlock  %% Last block actually checked
           , last_top   => LastTop }, %% Last top event hash (possibly skipped)
    {C, CIH}.

%% We do an async_dirty activity for minimal overhead. Note that the analysis
%% is already fixed by the top_hash, so it should be deterministic even in
%% dirty mode. Db accesses are cached so they only need to happen once.
do_dirty(F) ->
    aec_db:ensure_activity(
      async_dirty,
      fun() ->
              try F()
              ?_catch_(EType, E, EType==error; EType==exit, StackTrace)
                  lager:error("CAUGHT ~p / ~p", [E, StackTrace]),
                  error(E)
              end
      end).

-spec check_requests_([req()], #st{}, cache()) -> #st{}.
check_requests_([], St, C) ->
    check_done(C, St);
check_requests_([#req{info = #{check_at_height := CheckAt}} = R | Reqs1], St, C) ->
    lager:debug("CheckAt: R = ~p", [R]),
    {TopHeight, C1} = top_height(C),
    if TopHeight >= CheckAt ->
            check_at_height(R, Reqs1, St, C1);
       true ->
            lager:debug("Not yet (TopHeight = ~p), skip", [TopHeight]),
            check_requests_(Reqs1, St, C1)
    end;
check_requests_([R | Reqs1], St, C) ->
    lager:debug("Req = ~p", [lager:pr(R, ?MODULE)]),
    check_cont(check_req(R, St, C), R, Reqs1, St).

check_done(#st{cache = C} = St) ->
    check_done(C, St).

check_done(C, St) ->
    C1 = update_chan_vsns(C),
    St#st{last_top = maps:get(top_hash, C1),
          tx_log = maps:get(tx_log, C1),
          rpt_log = maps:get(rpt_log, C1)}.

check_at_height(R, Reqs, St, C) ->
    lager:debug("will check at height", []),
    R1 = clear_check_at_height(R),
    check_cont(check_req(R1, St, C), R1, Reqs, St).

%% ======================================================================
%% Query functions for finding matching requests

requests(ChanId) ->
    Found = ets:select(?T_REQUESTS,
                       [{ #req{key = {ChanId,'_','_'}, _ = '_'}, [], ['$_'] }]),
    Found.

reqs_at_height(Height) ->
    lager:debug("Height=~p, All: ~p", [Height, ets:tab2list(?T_AT_HEIGHT)]),
    reqs_at_height_(ets:first(?T_AT_HEIGHT), Height, []).

reqs_at_height_({H, Key} = K, Height, Acc) when H =< Height ->
    lager:debug("K = ~p, Height = ~p", [K, Height]),
    Acc1 = case ets:lookup(?T_REQUESTS, Key) of
               [] ->
                   lager:debug("Key not found (~p)", [Key]),
                   Acc;
               [R] ->
                   lager:debug("Key exists (~p)", [Key]),
                   [R|Acc]
           end,
    reqs_at_height_(ets:next(?T_AT_HEIGHT, K), Height, Acc1);
reqs_at_height_(_K, _H, Acc) ->
    lager:debug("K = ~p, H = ~p", [_K, _H]),
    %% Arguably, reversing doesn't do much, but if there are requests
    %% that were supposed to be checked at a lower height than the current,
    %% these will at least come first.
    lists:reverse(Acc).

reqs_for_txs(Txs) ->
    maps:fold(
      fun({ch,ChId}, _Txs, {ChIds, Reqs} = Acc) ->
              case requests(ChId) of
                  [] ->
                      Acc;
                  [_|_] = ChReqs ->
                      %% order doesn't matter
                      {[ChId|ChIds], Reqs ++ ChReqs}
              end;
         (_, _, Acc) ->
              Acc
      end, {[], []}, Txs).

all_channel_ids() ->
    %% Find all channel ids that have requests registered, by skipping selectively
    %% through the ?T_REQUESTS table (ordered on channel id)
    all_channel_ids(ets:first(?T_REQUESTS), []).

all_channel_ids('$end_of_table', Acc) ->
    %% No particular point in reversing the accumulator
    Acc;
all_channel_ids({ChId,_,_}, Acc) ->
    %% Skip to next entry after the last request for ChId. The structure
    %% of the key is {binary(), tuple(), pid()}, so stuffing [] into the
    %% 2nd and 3rd positions is guaranteed to jump past all valid requests.
    all_channel_ids(ets:next(?T_REQUESTS, {ChId, [], []}), [ChId | Acc]).

%% ======================================================================

check_cont({Res, C}, Req, Reqs, St) when is_map(C) ->
    case Res of
        done ->
            delete_req(Req),
            check_requests_(Reqs, St, C);
        no_change ->
            check_requests_(Reqs, St, C);
        #req{} = R1 ->
            write_req(R1),
            check_requests_(Reqs, St, C)
    end.

remove_audit_marker(ChId) when is_binary(ChId) ->
    ets:delete(?T_AUDIT, ChId).

check_req(#req{mode = close, ch_id = ChId, info = #{locked_until := H} = I} = R, _St, C) ->
    %% Presence of locked_until means we know the channel is/was locked
    Min = maps:get(min_depth, I, 0),
    {TopHeight, C1} = top_height(C),
    if TopHeight >= (H + Min) ->
            #req{ module = Mod
                , client = Parent } = R,
            Mod:minimum_depth_achieved(Parent, ChId, req_type(R), undefined),
            {done, C1};
       true ->
            CheckAt = Min + H,
            {R#req{info = I#{check_at_height => CheckAt}}, C1}
    end;
check_req(#req{mode = close, ch_id = ChId, client = Client, info = I} = R, _St, C) ->
    lager:debug("check_req(mode = close, client = ~p)", [Client]),
    {TopHeight, C1} = top_height(C),
    {Status, C2} = channel_status(ChId, Client, C1),
    case Status of
        #{ is_active := true } ->
            {no_change, C2};
        #{ locked_until := LockedUntil } ->
            lager:debug("LockedUntil = ~p (Ch=~p)", [LockedUntil, ChId]),
            Min = maps:get(min_depth, I, 0),
            CheckAt = LockedUntil + Min,
            {R#req{ info = I#{ locked_until    => LockedUntil
                             , check_at_height => CheckAt }}, C2};
        undefined ->
            lager:debug("Channel status undefined (Ch=~p)", [ChId]),
            %% Set closing time to current top height, wait for min_depth
            Min = maps:get(min_depth, I, 0),
            lager:debug("locked_until => ~p, check_at_height => ~p",
                        [TopHeight, TopHeight + Min]),
            {R#req{ info = I#{ locked_until    => TopHeight
                             , check_at_height => TopHeight + Min }}, C2}
    end;
check_req(#req{mode = unlock, ch_id = ChId, client = Client, info = I} = R, _St, C) ->
    lager:debug("check_req(mode = unlock, client = ~p)", [Client]),
    {Status, C1} = channel_status(ChId, Client, C),
    case Status of
        #{locked_until := 0} ->
            lager:debug("locked_until = 0", []),
            report_channel_unlocked(R, Status, C1),
            {done, C1};
        #{locked_until := LockedUntil} ->
            {Height, C2} = top_height(C1),
            lager:debug("LockedUntil = ~p, Height = ~p", [LockedUntil, Height]),
            case LockedUntil < Height of
                true  ->
                    lager:debug("locked_until expired", []),
                    report_channel_unlocked(R, Status, C2),
                    {done, C2};
                false ->
                    lager:debug("still locked", []),
                    {R#req{info = I#{check_at_height => LockedUntil + 1}}, C2}
            end;
        {undefined, C1} ->
            lager:debug("couldn't find channel", []),
            {done, C1}
    end;
check_req(#req{mode = watch, ch_id = ChId, client = Client} = R, St,
          #{scenario := Scenario, top_hash := Hash } = C)
  when ?IS_SCENARIO(Scenario) ->
    lager:debug("Scenario = ~p, R = ~p", [Scenario, lager:pr(R, ?MODULE)]),
    case Scenario of
        {has_tx, Txs} ->
            case maps:find({ch,ChId}, Txs) of
                {ok, ChTxs} ->
                    lists:foldl(
                      fun(TxHash, {Rx, Cx}) ->
                              #{type := TxType} = maps:get({tx,TxHash}, Txs),
                              {Ch, C1} = get_channel(ChId, Client, Hash, Cx),
                              {#{ height := H }, C2} = top_info(Hash, C1),
                              case Ch of
                                  undefined when TxType == channel_close_mutual_tx;
                                                 TxType == channel_settle_tx ->
                                      report_closed_on_chain(#{ tx_type => TxType
                                                              , tx_hash => TxHash
                                                              , block_hash => Hash
                                                              , height  => H }, Rx, C2);
                                  _ ->
                                      {Status, C3} = channel_status(ChId, Client, C2),
                                      lager:debug("Status = ~p", [Status]),
                                      watch_for_change_in_ch_status(Status, H, Rx, St, C3)
                              end
                      end, {R, C}, ChTxs);
                error ->
                    {R, C}
            end;
        fork_switch ->
            watch_for_channel_change(R, St, C);
        _ ->
            lager:debug("Other scenario - ignore (~p)", [Scenario]),
            {no_change, C}
    end;
check_req(#req{ mode = tx_hash, client = Client
              , info = #{tx_hash := TxHash, min_depth := MinDepth} = I
              , ch_id = ChanId } = R, _St, C) ->
    lager:debug("check_req(tx_hash = ~p, client = ~p)", [TxHash, Client]),
    case current_depth(TxHash, ChanId, C) of
        {undefined, C1} ->
            {no_change, C1};
        {Depth, C1} when Depth >= MinDepth ->
            lager:debug("min_depth achieved", []),
            #req{ module = Mod } = R,
            Mod:minimum_depth_achieved(Client, ChanId, req_type(R), TxHash),
            {done, C1};
        {Depth, C1} ->
            {TopHeight, C2} = top_height(C1),
            lager:debug("min_depth not yet achieved (Top = ~p, Depth = ~p, Min = ~p)",
                        [TopHeight, Depth, MinDepth]),
            {R#req{info = I#{check_at_height => TopHeight + (MinDepth - Depth)}}, C2}
    end.

watch_for_channel_change(R, St, #{ scenario := Scenario } = C)
  when ?IS_SCENARIO(Scenario) ->
    lager:debug("Scenario = ~p", [Scenario]),
    case Scenario of
        next_block ->
            lager:debug("Will not check channel", []),
            {R, C};
        _ ->
            {CurrHeight, C1} = top_height(C),
            lager:debug("Will check channel on chain (~p)", [Scenario]),
            watch_for_channel_change(CurrHeight, R, St, C1)
    end.

watch_for_channel_change(CurrHeight, #req{ch_id = ChanId, client = Client} = R, St, C) ->
    V = case read_ch_state(ChanId, Client) of
            #{vsn := Vsn} when Vsn =/= undefined ->
                Vsn;
            _ ->
                undefined
        end,
    lager:debug("Will check channel status (V=~p)", [V]),
    {Status, C1} = channel_status(ChanId, Client, C),
    lager:debug("Status = ~p", [Status]),
    watch_for_change_in_ch_status(Status, CurrHeight, R, St, C1).

watch_for_change_in_ch_status(undefined, _CurrHeight, R, _St, C) ->
    lager:debug("No channel object yet", []),
    {R, C};
watch_for_change_in_ch_status(Status, _CurrHeight, R, St, C) ->
    case Status of
        #{changed := true} ->
            lager:debug("Channel has changed: ~p", [Status]),
            %% NextHeight = calc_next_height(Status, CurrHeight, St),
            #req{module = Mod, client = Client, ch_id = ChId} = R,
            C1 = report_status_change(Status, ChId, Mod, Client, St, C),
            {no_change, C1};
        _ ->
            lager:debug("No change in channel: ~p", [Status]),
            {no_change, C}
    end.

report_status_change(#{channel := Ch, is_active := IsActive,
                       tx := SignedTx}, ChId, Mod, Client, _St, C) ->
    Event = if IsActive -> changed_on_chain;
               true     -> closing_on_chain
            end,
    BlockHash = maps:get(top_hash, C),
    TxHash = aetx_sign:hash(SignedTx),
    RptKey = {Event, TxHash, Client},
    Info = #{ chan_id => ChId
            , tx      => SignedTx
            , tx_hash => TxHash
            , channel => Ch
            , block_hash => BlockHash },
    maybe_report(
      RptKey, Info,
      fun() ->
              case Event of
                  changed_on_chain ->
                      Mod:channel_changed_on_chain(Client, Info);
                  closing_on_chain ->
                      Mod:channel_closing_on_chain(Client, Info)
              end
      end, C).

report_closed_on_chain(#{ tx_type    := _TxType
                        , tx_hash    := TxHash
                        , block_hash := BHash
                        , height     := Height } = I, R, C) ->
    #req{ module = Mod, client = Client, ch_id = ChId, info = ReqInfo} = R,
    RptKey = {closed_on_chain, TxHash, Client},
    I1 = maps:merge(ReqInfo, I),
    C1 = maybe_report(
           RptKey, I1,
           fun(Cx) ->
                   {SignedTx, Cx1} = get_signed_tx(TxHash, Cx),
                   Mod:channel_closed_on_chain(Client, I1#{tx => SignedTx}),
                   Cx1
           end, C),
    ClosedAt = #{ block_hash => BHash
                , height     => Height },
    ChStatus = maps:get({ch_status, BHash}, C1, #{}),
    NewVsn = #{closed_at => ClosedAt},
    ChVsns = maps:get(chan_vsn, C1, #{}),
    C2 = C1#{ {ch_status, BHash}     => ChStatus#{ChId => closed}
            , chan_vsns              => ChVsns#{{Client, ChId} => NewVsn}
            , {channel, ChId, BHash} => ClosedAt },
    { R#req{ info = I#{closed_at => ClosedAt} }, C2}.

maybe_report(RptKey, Info, Rpt, C) when is_function(Rpt) ->
    lager:debug("RptKey = ~p", [RptKey]),
    RptLog = maps:get(rpt_log, C),
    case aesc_window:keyfind(RptKey, #rpt_log_entry.key, RptLog) of
        false ->
            lager:debug("not found in log, reporting", []),
            C1 = call_rpt(Rpt, C),
            LogEntry = #rpt_log_entry{key = RptKey, value = Info},
            C1#{rpt_log => aesc_window:add(LogEntry, RptLog)};
        Other ->
            lager:debug("Not reporting (Other=~p)", [Other]),
            C
    end.

call_rpt(R, C) when is_function(R, 0) ->
    R(),
    C;
call_rpt(R, C) when is_function(R, 1) ->
    R(C).

report_channel_unlocked(#req{module = Mod, client = Client, ch_id = ChId}, Ch, C) ->
    BlockHash = maps:get(top_hash, C),
    Mod:channel_unlocked(Client, #{ chan_id => ChId
                                  , channel => Ch
                                  , block_hash => BlockHash }).

channel_status(ChId, Client, #{top_hash := Hash} = C) ->
    cached_get({ch_status, Client, ChId, Hash}, C,
               fun(C1) ->
                       get_ch_status(ChId, Client, Hash, C1)
               end).

channel_status_changed(V, ChId, Client, Cache) ->
    {V0, C1} = prev_chan_vsn(ChId, Client, Cache),
    lager:debug("(~p) V = ~p; V0 = ~p", [V =/= V0, V, V0]),
    {V =/= V0, C1}.

prev_chan_vsn(ChId, Client, C) ->
    {Vsns, C1} = chan_vsns(C),
    case maps:find({Client, ChId}, Vsns) of
        {ok, V} ->
            {V, C1};
        error ->
            S = read_ch_state(ChId, Client),
            {S, C1#{chan_vsns => Vsns#{ {Client, ChId} => S }}}
    end.

update_chan_vsns(Cache) ->
    {Vsns, Cache1} = chan_vsns(Cache),
    maps:fold(fun({Client, ChId}, V, C) when is_pid(Client) ->
                      remove_audit_marker(ChId),
                      update_chan_vsn(Client, ChId, V, C)
              end, Cache1, Vsns).

update_chan_vsn(Client, ChId, V, #{ top_hash  := Hash
                                  , scenario  := S } = Cache) when ?IS_SCENARIO(S),
                                                                   is_pid(Client),
                                                                   is_binary(ChId) ->
    Key = {ChId, Client},
    case V of
        #{ closed_at := _ } = Vsn ->
            lager:debug("Update Vsn = ~p", [Vsn]),
            write_ch_state(Key, Hash, #{vsn => Vsn}),
            Cache;
        #{vsn := Vsn, changed := true} ->
            lager:debug("Update Vsn = ~p", [Vsn]),
            write_ch_state(Key, Hash, #{vsn => Vsn, last_block => Hash}),
            Cache;
        _ ->
            Cache
    end.

write_ch_state(Key, BlockHash, #{vsn := Vsn} = I) ->
    ets:insert(?T_CH_STATES, #ch_state{ key   = Key
                                      , block = BlockHash
                                      , vsn   = Vsn
                                      , info = maps:remove(vsn, I)}),
    ok.

read_ch_state(ChId, Client) when is_binary(ChId), is_pid(Client) ->
    case ets:lookup(?T_CH_STATES, {ChId, Client}) of
        [#ch_state{vsn = Vsn, info = I}] ->
            I#{vsn => Vsn};
        [] ->
            lager:debug("Ch States in ets: ~p", [ets:tab2list(?T_CH_STATES)]),
            try_copy_ch_state(ChId, Client)
    end.

try_copy_ch_state(ChId, ToPid) ->
    %% See if there is an existing state record for another pid
    %% If so, copy it, but set `changed = true`
    case ets:next(?T_CH_STATES, {ChId, 0}) of
        {ChId, _} = Key ->
            [#ch_state{info = I0} = S0] = ets:lookup(?T_CH_STATES, Key),
            S = S0#ch_state{ key  = {ChId, ToPid}
                           , info = I0#{ changed => true} },
            ets:insert(?T_CH_STATES, S),
            S;
        _ ->
            undefined
    end.

get_ch_status(ChId, Client, Hash, C) ->
    case get_basic_ch_status_(ChId, Client, Hash, C) of
        {#{changed := Changed} = Status, C1} ->
            case Changed of
                true ->
                    lager:debug("status has changed", []),
                    handle_ch_status_changed(ChId, Client, C1);
                false ->
                    lager:debug("status hasn't changed", []),
                    {Status, C1}
            end;
        Other ->
            lager:debug("Other = ~p", [Other]),
            Other
    end.

handle_ch_status_changed(ChId, Client, #{ last_block := Last
                                        , top_hash := Hash } = C) ->
    lager:debug("channel status changed, Hash=~p, Last=~p", [Hash,Last]),
    C1 = get_txs_since({any_after_block, Last}, Hash, ChId, C),
    {TxHash, #{tx := Tx, block_hash := BlockHash}, C2} =
        get_latest_tx(ChId, C1),
    lager:debug("Latest TxHash = ~p, Hash = ~p", [TxHash, BlockHash]),
    {Status, C3} = get_basic_ch_status_(ChId, Client, BlockHash, C2),
    lager:debug("ChStatus(~p) = ~p", [BlockHash, Status]),
    %% Assert that this is really a changed channel object
    {true, C4} = channel_status_changed(maps:get(vsn, Status), ChId, Client, C3),
    lager:debug("asserted status changed", []),
    Status1 = Status#{tx => Tx},
    {Status1, C4#{{channel, ChId, BlockHash} => Status1}}.

get_basic_ch_status_(ChId, Client, BlockHash, C) ->
    {Ch, C1} = get_channel(ChId, Client, BlockHash, C),
    case Ch of
        undefined ->
            {undefined, C1};
        _ ->
            Vsn = channel_vsn(Ch),
            lager:debug("Vsn = ~p (Ch = ~p)", [Vsn, Ch]),
            {Changed, C2} = channel_status_changed(Vsn, ChId, Client, C1),
            I = #{ is_active    => aesc_channels:is_active(Ch)
                 , vsn          => Vsn
                 , changed      => Changed
                 , channel      => Ch
                 , lock_period  => aesc_channels:lock_period(Ch) },
            {maybe_add_locked_until(I, Ch), C2}
    end.

maybe_add_locked_until(#{is_active := false} = I, Ch) ->
    LU = aesc_channels:locked_until(Ch),
    lager:debug("Adding locked_until => ~p (Ch = ~p)", [LU, Ch]),
    I#{ locked_until => LU };
maybe_add_locked_until(I, _) ->
    I.

chan_vsns(C) ->
    cached_get(chan_vsns, C, fun() -> #{} end).

get_channel(ChId, Client, Hash, C) when is_pid(Client) ->
    {Vsns, C1} = chan_vsns(C),
    case maps:find(ChId, C1) of
        {ok, #{hash := Hash, channel := Ch}} ->
            {Ch, C1};
        Other ->
            case aesc_chain_watcher_support:get_channel(ChId, Hash) of
                undefined when Other == error ->
                    {undefined, C1};
                undefined ->
                    {undefined, C1#{chan_vsns => maps:remove({Client, ChId}, Vsns)}};
                Ch ->
                    {Ch, C1#{chan_vsns => Vsns#{{Client,ChId} => #{ hash => Hash
                                                                  , channel =>  Ch }}}}
            end
    end.

cached_get(Key, C, Get) ->
    case maps:find(Key, C) of
        {ok, V} ->
            {V, C};
        error ->
            get_and_cache(Get, Key, C)
    end.

get_and_cache(F, Key, C) when is_function(F, 0), is_map(C) ->
    V = F(),
    {V, maps:put(Key, V, C)};
get_and_cache(F, Key, C) when is_function(F, 1), is_map(C) ->
    {V, C1} = F(C),
    {V, maps:put(Key, V, C1)}.

log_tx(TxInfo, #st{tx_log = TxLog} = St) ->
    try St#st{tx_log = log_tx_(TxInfo, TxLog)}
    ?_catch_(EType, E, EType==error; EType==exit, StackTrace)
        lager:error("CAUGHT ~p / ~p", [E, StackTrace]),
        error(E)
    end.

log_tx_(#{ tx_hash      := TxHash
         , chan_id      := ChId
         , block_hash   := BlockHash } = Info, TxLog) ->
    Key = BlockHash,
    case aesc_window:keyfind(Key, #tx_log_entry.key, TxLog) of
        #tx_log_entry{value = #{txs := Txs} = V} = Entry ->
            %% Save Info with entry points for both Tx and ChId. Erlang takes care
            %% of space-efficiency via sharing.
            %% Although unlikely, assume there might be >1 txs for ChId in same block
            case maps:is_key({tx,TxHash}, Txs) of
                true ->
                    TxLog;
                false ->
                    ChTxs0 = maps:get({ch,ChId}, Txs, []),
                    ChTxs1 = ChTxs0 ++ [TxHash],
                    Entry1 = Entry#tx_log_entry{
                               value = V#{txs => Txs#{ {tx,TxHash} => Info
                                                     , {ch,ChId}   => ChTxs1 }}},
                    aesc_window:keyreplace(
                      Key, #tx_log_entry.key, TxLog, Entry1)
            end;
        false ->
            V0 = maps:with([block_origin, type], Info),
            V1 = V0#{txs => #{ {tx,TxHash} => Info
                             , {ch,ChId}   => [TxHash] }},
            LogEntry = #tx_log_entry{ key = Key
                                    , value = V1 },
            aesc_window:add(LogEntry, TxLog)
    end.

log_entry_txs_to_history(ChIds, Txs, BlockHash, Height, #st{cache = C} = St) ->
    %% TODO Look into optimizing this
    St#st{cache = log_tx_history_(ChIds, Txs, BlockHash, Height, C)}.

log_tx_history_(ChIds, Txs, BlockHash, Height, C) when is_map(C) ->
    lists:foldl(
      fun(ChId, Cx) ->
              log_tx_history_for_chid(ChId, Txs, BlockHash, Height, Cx)
      end, C, ChIds).

log_tx_history_for_chid(ChId, Txs, BlockHash, Height, C) ->
    {TxHistories, C1} = tx_histories(C),
    lager:debug("TxHistories = ~p", [TxHistories]),
    TxHashes = maps:get({ch, ChId}, Txs),
    ChHist = maps:get(ChId, TxHistories, []),
    lager:debug("ChHist (ChId=~p): ~p", [ChId, ChHist]),
    {ChHist1, C2} =
        lists:foldl(
          fun(TxHash, {ChHx, Cx}) ->
                  TxInfo = maps:get({tx,TxHash}, Txs),
                  lager:debug("Txs: TxInfo = ~p", [TxInfo]),
                  Cx1 = set_tx_info(TxHash, Cx,
                                    fun(I0) ->
                                            Merged = maps:merge(I0, TxInfo),
                                            lager:debug("Updated TxInfo = ~p",
                                                        [Merged]),
                                            Merged
                                    end),
                  lager:debug("From cache: ~p", [maps:get({tx_info,TxHash}, Cx1)]),
                  lager:debug("tx_info(~p) -> ~p", [TxHash, tx_info(TxHash, Cx1)]),
                  {add_tx_to_history(
                     ChHx, Height, TxHash, BlockHash), Cx1}
          end, {ChHist, C1}, TxHashes),
    TxHistories1 = TxHistories#{ ChId => ChHist1 },
    lager:debug("TxHistories1 = ~p", [TxHistories1]),
    C2#{ tx_histories => TxHistories1 }.

log_tx_histories(Found, ChId, C) when is_list(Found), is_map(C) ->
    {TxHistories, C1} = tx_histories(C),
    lager:debug("TxHistories = ~p", [TxHistories]),
    ChHist = maps:get(ChId, TxHistories, []),
    {ChHist1, C2} =
        lists:foldl(
          fun(TxHash, {ChHx, Cx}) ->
                  lager:debug("TxHash = ~p", [TxHash]),
                  {#{block_hash := BHash}, Cx1} =
                      tx_info(TxHash, Cx),
                  {Height, Cx2} = height(BHash, Cx1),
                  {add_tx_to_history(
                     ChHx, Height, TxHash, BHash), Cx2}
          end, {ChHist, C1}, Found),
    C2#{ tx_histories => TxHistories#{ ChId => ChHist1 } }.

tx_histories(C) ->
    cached_get(tx_histories, C, fun() -> #{} end).

%% Sort on height, but also match on BlockHash (forks might cause different
%% cached blocks to have the same apparent height?)
add_tx_to_history([#{ height     := Height
                    , block_hash := BlockHash
                    , txs := Txs} = H|T] = L, Height, TxHash, BlockHash) ->
    case lists:member(TxHash, Txs) of
        true ->
            L;
        false ->
            %% Assumption: Txs are added first-to-last, and we usually want to pop
            %% the latest tx, so keep the list of txs LIFO
            [H#{txs => [TxHash | Txs]} | T]
    end;
add_tx_to_history([#{ height := HHeight } |_] = L, Height, TxHash, BlockHash)
  when HHeight < Height ->
    [#{ height     => Height
      , block_hash => BlockHash
      , txs        => [TxHash] } | L];
add_tx_to_history([H|T], Height, TxHash, BlockHash) ->
    [H|add_tx_to_history(T, Height, TxHash, BlockHash)];
add_tx_to_history([], Height, TxHash, BlockHash) ->
    [#{ height     => Height
      , block_hash => BlockHash
      , txs        => [TxHash] }].

update_tx_log(_, undefined, undefined, undefined, _, C) ->
    %% don't add a non-existing tx
    C;
update_tx_log(TxHash, SignedTx, BlockHash, TxType, ChId, #{tx_log := TxLog} = C)
  when is_binary(BlockHash) ->
    TxInfo = #{ tx_hash      => TxHash
              , block_hash   => BlockHash
              , type         => TxType
              , chan_id      => ChId },
    C#{ tx_log => log_tx_(TxInfo, TxLog)
      , {tx, TxHash} => SignedTx };
update_tx_log(_, _, _, _, _, C) ->
    C.

get_latest_tx(ChId, C) ->
    lager:debug("ChId = ~p", [ChId]),
    {TxHistories, C1} = tx_histories(C),
    lager:debug("TxHistories = ~p", [TxHistories]),
    [#{ block_hash := BlockHash
      , txs := [TxHash|_] } | _] = maps:get(ChId, TxHistories),
    {SignedTx, C2} = get_signed_tx(TxHash, C1),
    {TxHash, #{ tx => SignedTx
              , block_hash => BlockHash }, C2}.

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
    lager:debug("StopCond = ~p, Hash = ~p, ChId = ~p", [StopCond, Hash, ChId]),
    case verify_stop_cond(StopCond, ChId, C) of
        {ok, C1} ->
            lager:debug("StopCond = ~p", [StopCond]),
            {Found, C2} = get_txs_since(StopCond, Hash, ChId, C1, []),
            %% TODO: It's likely usually (not always?) redundant to log to history here
            log_tx_histories(Found, ChId, C2);
        {{error, Error}, _} ->
            lager:error("Bad StopCond (~p): ~p", [StopCond, Error]),
            error(bad_stop_condition)
    end.

verify_stop_cond({any_after_block, _}, _ChId, C) ->
    %% assume this is ok (it will be as long as the watcher uses it correctly)
    {ok, C};
verify_stop_cond({all_after_tx, TxHash}, ChId, C) ->
    case tx_location(TxHash, ChId, C) of
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
            lager:debug("txs in ~p: ~p", [Hash, Found]),
            PrevHash = aec_headers:prev_hash(Hdr),
            case stop_cond(StopCond, PrevHash, Found, C2) of
                {true, Found1, C3} ->
                    {Found1 ++ Acc, C3};
                {false, C3} ->
                    get_txs_since(
                      StopCond, PrevHash, ChId, C3, Found ++ Acc)
            end
    end.

stop_cond({all_after_tx, TxHash}, _PrevHash, Found, C) ->
    case Found of
        [] -> {false, C};
        [_|_] ->
            tail_after_tx(TxHash, Found, C)
    end;
stop_cond({any_after_block, Hash}, PrevHash, Found, C) ->
    if Hash == PrevHash ->
            {true, Found, C};
       true ->
            has_create_tx(Found, C)
    end.

%% Safety. If a create_tx exists in Found, it must be the first one.
has_create_tx([], C) ->
    {false, C};
has_create_tx([TxHash|_] = Found, C) ->
    {SignedTx, C1} = get_signed_tx(TxHash, C),
    case tx_type(SignedTx) of
        channel_create_tx ->
            {true, Found, C1};
        _ ->
            {false, C1}
    end.

tail_after_tx(_, [], C) ->
    {false, C};
tail_after_tx(H, [H|T], C) ->
    {true, T, C};
tail_after_tx(Hash, [_|T], C) ->
    tail_after_tx(Hash, T, C).

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
    %% Filter out txs for ChId, preserving the within-block order
    {TxHashes, C1} = tx_hashes(Hash, Hdr, C),
    lager:debug("TxHashes = ~p", [TxHashes]),
    {_Found, _C2} = lists:foldr(
                    fun(TxHash, {Acc, Cx}) ->
                            {SignedTx, Cx1} = get_signed_tx(TxHash, Cx),
                            lager:debug("SignedTx = ~p", [SignedTx]),
                            case is_tx_for_chid(SignedTx, ChId) of
                                true  ->
                                    Cx2 = set_tx_info(
                                            TxHash,
                                            Cx1,
                                            fun(I) ->
                                                    I#{ block_hash => Hash
                                                      , chan_id    => ChId }
                                            end),
                                    {[TxHash|Acc], Cx2};
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

save_tx_info(ChIds, Txs, BHash, _Height, #st{ cache = C} = St) ->
    C1 = lists:foldl(
           fun(ChId, Cx) ->
                   case maps:find({ch, ChId}, Txs) of
                       {ok, TxHashes} ->
                           lists:foldl(
                             fun(TxHash, C1x) ->
                                     Info = maps:get({tx,TxHash}, Txs),
                                     lager:debug("Info = ~p", [Info]),
                                     I1 = Info#{ block_hash => BHash
                                               , chan_id    => ChId },
                                     set_tx_info(TxHash, C1x,
                                                 fun(I0) ->
                                                         maps:merge(I0, I1)
                                                 end)
                             end, Cx, TxHashes);
                       error ->
                           Cx
                   end
           end, C, ChIds),
    St#st{ cache = C1 }.

set_tx_info(TxHash, C, F) ->
    lager:debug("TxHash = ~p", [TxHash]),
    {I, C1} = tx_info(TxHash, C),
    C1#{ {tx_info, TxHash} => F(I) }.

tx_info(TxHash, C) ->
    tx_info(TxHash, C, fun() -> #{} end).

tx_info(TxHash, C, Init) when is_binary(TxHash)
                            , is_map(C)
                            , is_function(Init, 0) ->
    cached_get({tx_info, TxHash}, C, Init).

is_tx_for_chid(undefined, _) -> false;
is_tx_for_chid(SignedTx, ChId) ->
    case aesc_utils:channel_pubkey(SignedTx) of
        {error, not_channel_tx} ->
            false;
        {ok, ChId} -> % same channel id
            lager:debug("Tx=~p has channel id ~p", [SignedTx, ChId]),
            true;
        {ok, _OtherChId} ->
            false
    end.

-spec channel_vsn(undefined | aesc_channels:channel()) -> undefined | chan_vsn().
channel_vsn(undefined) ->
    undefined;
channel_vsn(Ch) ->
    { aesc_channels:round(Ch)
    , aesc_channels:solo_round(Ch)
    , aesc_channels:is_active(Ch)
    , aesc_channels:locked_until(Ch)
    , aesc_channels:state_hash(Ch) }.

current_depth(TxHash, ChId, C) ->
    {L, C1} = tx_location(TxHash, ChId, C),
    case L of
        undefined ->
            {undefined, C1};
        _ when is_binary(L) ->
            {InChain, C2} = in_main_chain(L, C1),
            case InChain of
                true ->
                    {TxHeight, C3} = height(L, C2),
                    {TopHeight, C4} = top_height(C3),
                    {TopHeight - TxHeight, C4};
                false ->
                    {undefined, C2}
            end
    end.

tx_location(TxHash, ChId, C) ->
    cached_get({location, TxHash}, C, fun(C1) ->
                                              tx_location_(TxHash, ChId, C1)
                                      end).

tx_location_(TxHash, ChId, C) ->
    {L, TxType, SignedTx} =
        case aec_chain:find_tx_with_location(TxHash) of
            none ->
                lager:debug("couldn't find tx hash", []),
                {undefined, undefined, undefined};
            {mempool, STx} ->
                lager:debug("tx still in mempool", []),
                {undefined, tx_type(STx), STx};
            {BlockHash, STx} ->
                lager:debug("tx in Block ~p", [BlockHash]),
                {BlockHash, tx_type(STx), STx}
        end,
    {L, update_tx_log(TxHash, SignedTx, L, TxType, ChId, C)}.

tx_type(SignedTx) ->
    {TxType, _} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    TxType.

in_main_chain(BHash, C) ->
    %% We don't want to use aec_chain_state:hash_is_in_main_chain/1, since we want to
    %% stick to the top hash of the triggering event.
    cached_get({in_main_chain, BHash}, C, fun(C1) -> in_main_chain_(BHash, C1) end).

in_main_chain_(Hash, C) ->
    {TopHash, C1} = top_hash(C),
    Res = aec_chain_state:hash_is_in_main_chain(Hash, TopHash),
    {Res, C1}.

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

top_info(TopHash, C) ->
    cached_get(top_info, C, fun() -> block_info(TopHash, C) end).

block_info(BHash, C) ->
    cached_get({block_info, BHash}, C, fun() -> block_info_(BHash, C) end).

block_info_(TopHash, C0) ->
    {Hdr, C1} = get_header(TopHash, C0),
    {#{ block_hash => TopHash
      , prev_hash  => aec_headers:prev_hash(Hdr)
      , block_type => aec_headers:type(Hdr)
      , height     => aec_headers:height(Hdr) }, C1}.

ensure_monitor(Pid) ->
    case ets:member(?T_MREFS, Pid) of
        true ->
            ignore;
        false ->
            MRef = monitor(process, Pid),
            ets:insert(?T_MREFS, {Pid, MRef})
    end,
    ok.

delete_monitor(Pid) ->
    ets:delete(?T_MREFS, Pid).
