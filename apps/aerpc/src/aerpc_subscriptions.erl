%%%-------------------------------------------------------------------
%%% @doc Subscription registry for the JSON-RPC WebSocket transport.
%%%
%%% Owns the per-connection subscription state, monitors the WS handler
%%% pids, and fans `aec_events' broadcasts (new key-blocks; new logs)
%%% out to the right subscribers in the right shape.
%%%
%%% Two subscription kinds today:
%%%
%%%   * `newHeads' -- fires once per new generation; payload is the
%%%     eth-shaped block (`ae_getBlockByNumber' result with full-tx
%%%     hashes only).
%%%   * `logs'     -- fires once per matching log inside a new
%%%     generation; payload is the same map shape as one element of
%%%     `ae_getLogs'.
%%%
%%% Subscription IDs are hex `QUANTITY' (matches the eth wire
%%% convention) and allocated from a monotonic counter. They are
%%% non-guessable enough for a local API.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_subscriptions).

-behaviour(gen_server).

-export([start_link/0,
         subscribe/3,
         unsubscribe/2,
         drop_owner/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(sub, {
    id        :: binary(),
    owner     :: pid(),
    kind      :: newHeads | logs,
    criteria  :: undefined | map()  %% undefined for newHeads; the
                                    %% caller-supplied filter map
                                    %% (address/topics) for logs
}).

-record(state, {
    next_id   = 1                 :: pos_integer(),
    by_id     = #{}               :: #{binary() => #sub{}},
    by_owner  = #{}               :: #{pid()   => [binary()]},
    monitors  = #{}               :: #{pid()   => reference()}
}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec subscribe(pid(), newHeads | logs, term()) ->
    {ok, binary()} | {error, integer(), binary()}.
subscribe(OwnerPid, Kind, Criteria)
  when is_pid(OwnerPid), (Kind =:= newHeads orelse Kind =:= logs) ->
    gen_server:call(?MODULE, {subscribe, OwnerPid, Kind, Criteria}).

-spec unsubscribe(pid(), binary()) -> boolean().
unsubscribe(OwnerPid, SubId) when is_pid(OwnerPid), is_binary(SubId) ->
    gen_server:call(?MODULE, {unsubscribe, OwnerPid, SubId}).

%% Called by the WS handler on termination to release all subs it owns
%% without waiting for the DOWN monitor message (which would also work,
%% but explicit cleanup is faster + clearer in logs).
-spec drop_owner(pid()) -> ok.
drop_owner(OwnerPid) when is_pid(OwnerPid) ->
    gen_server:cast(?MODULE, {drop_owner, OwnerPid}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% Subscribe once to the top-changed event; the per-subscription
    %% fan-out happens on each notification. Wrapped in try so that
    %% the registry can boot even in test environments where aec_events
    %% isn't fully wired up.
    try aec_events:subscribe(top_changed)
    catch _:_ -> ok
    end,
    {ok, #state{}}.

handle_call({subscribe, OwnerPid, Kind, Criteria}, _From, State) ->
    {Id, State1} = allocate(State),
    Sub = #sub{id = Id, owner = OwnerPid, kind = Kind, criteria = Criteria},
    State2 = add_sub(Sub, State1),
    State3 = ensure_monitor(OwnerPid, State2),
    {reply, {ok, Id}, State3};

handle_call({unsubscribe, OwnerPid, SubId}, _From, State) ->
    case maps:get(SubId, State#state.by_id, undefined) of
        #sub{owner = OwnerPid} ->
            State1 = drop_sub(SubId, OwnerPid, State),
            {reply, true, maybe_demonitor(OwnerPid, State1)};
        _Other ->
            %% Either id is unknown or owned by another pid; eth's
            %% convention: idempotent false.
            {reply, false, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({drop_owner, OwnerPid}, State) ->
    State1 = drop_all_for(OwnerPid, State),
    {noreply, maybe_demonitor(OwnerPid, State1)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, top_changed, #{info := #{block_hash := KBHash}}},
            State) ->
    fanout(KBHash, State),
    {noreply, State};
handle_info({gproc_ps_event, top_changed, _Other}, State) ->
    %% Event shape changed across protocols; ignore rather than crash.
    {noreply, State};
handle_info({'DOWN', _MRef, process, Pid, _Reason}, State) ->
    State1 = drop_all_for(Pid, State),
    State2 = State1#state{monitors = maps:remove(Pid, State1#state.monitors)},
    {noreply, State2};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

allocate(#state{next_id = N} = State) ->
    Id = aerpc_encoding:to_quantity(N),
    {Id, State#state{next_id = N + 1}}.

add_sub(#sub{id = Id, owner = Owner} = Sub, State) ->
    Ids0   = maps:get(Owner, State#state.by_owner, []),
    State#state{
        by_id    = maps:put(Id, Sub, State#state.by_id),
        by_owner = maps:put(Owner, [Id | Ids0], State#state.by_owner)
    }.

drop_sub(Id, Owner, State) ->
    ByOwner = State#state.by_owner,
    Remaining = lists:delete(Id, maps:get(Owner, ByOwner, [])),
    NewByOwner = case Remaining of
                     [] -> maps:remove(Owner, ByOwner);
                     _  -> maps:put(Owner, Remaining, ByOwner)
                 end,
    State#state{by_id    = maps:remove(Id, State#state.by_id),
                by_owner = NewByOwner}.

drop_all_for(Owner, State) ->
    Ids = maps:get(Owner, State#state.by_owner, []),
    State#state{
        by_id    = lists:foldl(fun maps:remove/2, State#state.by_id, Ids),
        by_owner = maps:remove(Owner, State#state.by_owner)
    }.

ensure_monitor(Pid, #state{monitors = M} = State) ->
    case maps:is_key(Pid, M) of
        true  -> State;
        false ->
            Ref = erlang:monitor(process, Pid),
            State#state{monitors = maps:put(Pid, Ref, M)}
    end.

maybe_demonitor(Pid, #state{monitors = M, by_owner = O} = State) ->
    case {maps:get(Pid, M, undefined), maps:is_key(Pid, O)} of
        {undefined, _}    -> State;
        {_Ref, true}      -> State;  %% Pid still owns other subs.
        {Ref, false} ->
            erlang:demonitor(Ref, [flush]),
            State#state{monitors = maps:remove(Pid, M)}
    end.

fanout(KBHash, State) ->
    %% Build the new-head payload once; reused across all `newHeads' subs.
    BlockMap = block_for_notification(KBHash),
    HashEnc  = aerpc_encoding:format_key_block_hash(KBHash),
    maps:fold(
        fun(_Id, #sub{kind = newHeads, owner = Pid, id = Id}, _Acc) ->
                send(Pid, Id, BlockMap),
                ok;
           (_Id, #sub{kind = logs, owner = Pid, id = Id, criteria = Crit},
            _Acc) ->
                fanout_logs(Pid, Id, HashEnc, Crit),
                ok
        end, ok, State#state.by_id).

block_for_notification(KBHash) ->
    %% Use the "tx-hash-only" form (full_txs=false) to keep the
    %% notification payload small; matches the eth `newHeads' shape.
    case aerpc_block:by_hash(aerpc_encoding:format_key_block_hash(KBHash),
                             false) of
        {ok, Block} when is_map(Block) -> Block;
        _Other -> #{}
    end.

fanout_logs(Pid, SubId, HashEnc, undefined) ->
    %% No filter -> emit every log in the new generation.
    fanout_logs(Pid, SubId, HashEnc, #{});
fanout_logs(Pid, SubId, HashEnc, Crit) when is_map(Crit) ->
    %% Reuse the existing ae_getLogs filter implementation for one-block
    %% scans. The blockHash constraint keeps the walk bounded.
    Filter = Crit#{<<"blockHash">> => HashEnc},
    case aerpc_logs:get_logs(Filter) of
        {ok, Logs} ->
            [send(Pid, SubId, L) || L <- Logs];
        _Error ->
            []
    end.

send(Pid, SubId, ResultTerm) ->
    Pid ! {aerpc_notify, SubId, ResultTerm}.
