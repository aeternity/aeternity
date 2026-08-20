%%%-------------------------------------------------------------------
%%% @doc Subscription registry for the JSON-RPC WebSocket transport.
%%%
%%% Owns the per-connection subscription state, monitors the WS handler
%%% pids, and fans `aec_events' broadcasts (new key-blocks; new logs)
%%% out to the right subscribers in the right shape.
%%%
%%% Three subscription kinds:
%%%
%%%   * `newHeads' -- fires once per new generation; payload is the
%%%     eth-shaped block (`eth_getBlockByNumber' result with full-tx
%%%     hashes only).
%%%   * `logs'     -- fires once per matching log inside a new
%%%     generation; payload is the same map shape as one element of
%%%     `eth_getLogs'.
%%%   * `newPendingTransactions' -- fires once per mempool arrival;
%%%     payload is the tx hash, or the full eth transaction object when
%%%     the client passed `true' as the second parameter. Same
%%%     `aec_events:tx_received' stream `eth_newPendingTransactionFilter'
%%%     polls, so the push and poll halves cannot disagree about what
%%%     "pending" means.
%%%
%%% Unlike the two chain-derived kinds, a mempool arrival cannot be
%%% recovered from chain state, so a client that misses a frame has
%%% missed it. That is inherent to the eth semantics of this
%%% subscription, not a property of this implementation -- geth behaves
%%% the same way.
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
         drop_owner/1,
         parse_subscribe_params/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(sub, {
    id        :: binary(),
    owner     :: pid(),
    kind      :: newHeads | logs | pending_tx,
    criteria  :: undefined | map() | boolean()
                                    %% undefined for newHeads; the
                                    %% caller-supplied filter map
                                    %% (address/topics) for logs; the
                                    %% full-transactions flag for
                                    %% pending_tx
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

-spec subscribe(pid(), newHeads | logs | pending_tx, term()) ->
    {ok, binary()} | {error, integer(), binary()}.
subscribe(OwnerPid, Kind, Criteria)
  when is_pid(OwnerPid), (Kind =:= newHeads orelse Kind =:= logs
                          orelse Kind =:= pending_tx) ->
    gen_server:call(?MODULE, {subscribe, OwnerPid, Kind, Criteria}).

%% @doc Turn `eth_subscribe' params into a kind and its criteria. Lives
%% here rather than in the WebSocket handler because which kinds exist is
%% a property of this registry, not of the transport -- the same reason
%% the batch cap moved into `aerpc:dispatch/1'. It also makes the one
%% distinction that matters testable without a socket.
-spec parse_subscribe_params(term()) ->
    {ok, newHeads | logs | pending_tx, term()} | {error, integer(), binary()}.
parse_subscribe_params([<<"newHeads">>]) ->
    {ok, newHeads, undefined};
parse_subscribe_params([<<"newHeads">>, _Opts]) ->
    %% eth ignores the second arg for newHeads in practice.
    {ok, newHeads, undefined};
parse_subscribe_params([<<"logs">>]) ->
    {ok, logs, #{}};
parse_subscribe_params([<<"logs">>, Criteria]) when is_map(Criteria) ->
    {ok, logs, Criteria};
parse_subscribe_params([<<"newPendingTransactions">>]) ->
    {ok, pending_tx, false};
parse_subscribe_params([<<"newPendingTransactions">>, Full])
  when is_boolean(Full) ->
    %% geth's second parameter: `true' asks for full transaction objects
    %% rather than hashes.
    {ok, pending_tx, Full};
%% A kind we do not implement is NOT the same answer as a malformed
%% call. A client told "invalid params" retries or gives up; one told the
%% kind is unsupported falls back to polling, and for every kind on this
%% endpoint the poll filter exists and works. Splitting them costs one
%% clause and is the difference between a client that degrades and one
%% that just fails.
parse_subscribe_params([Kind | _Rest]) when is_binary(Kind) ->
    aerpc_errors:unsupported_subscription(Kind);
parse_subscribe_params(_Malformed) ->
    {error, -32602, <<"Invalid params">>}.

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
    %% The mempool stream, for `newPendingTransactions'. The handler
    %% returns immediately when nothing is subscribed to it, so a socket
    %% that only asked for newHeads costs nothing here.
    try aec_events:subscribe(tx_received)
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

handle_info({gproc_ps_event, top_changed, #{info := Info}}, State)
  when is_map(Info) ->
    %% `top_changed' carries the new TOP block hash, which is a micro
    %% block whenever one is mined. Everything below is keyed by the
    %% generation's KEY block, so feeding the raw hash through made
    %% `newHeads' emit `{}' and pointed the log fan-out at a hash
    %% `eth_getLogs' does not accept -- the third instance of the same
    %% micro-vs-key confusion as the receipts and the address index.
    case generation_hash(Info) of
        {ok, KBHash} -> fanout(KBHash, State);
        error        -> ok
    end,
    {noreply, State};
handle_info({gproc_ps_event, top_changed, _Other}, State) ->
    %% Event shape changed across protocols; ignore rather than crash.
    {noreply, State};
handle_info({gproc_ps_event, tx_received, #{info := SignedTx}}, State) ->
    fanout_pending(SignedTx, State),
    {noreply, State};
handle_info({gproc_ps_event, tx_received, _Other}, State) ->
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

%% Resolve the event's block hash to its generation's key block. A key
%% block opens a generation with no micro blocks yet, so the interesting
%% notification is the one it CLOSES -- which is also the generation a
%% micro-block event belongs to, so both types resolve through
%% `prev_key_hash'.
generation_hash(#{block_hash := Hash} = Info) ->
    case block_type(Hash, Info) of
        micro   -> prev_key(Hash);
        key     -> prev_key(Hash);
        unknown -> error
    end;
generation_hash(_Other) ->
    error.

block_type(_Hash, #{block_type := Type}) when Type =:= key; Type =:= micro ->
    Type;
block_type(Hash, _Info) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} -> aec_headers:type(Header);
        error        -> unknown
    end.

prev_key(Hash) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} -> {ok, aec_headers:prev_key_hash(Header)};
        error        -> error
    end.

%% Mempool fan-out. Only `pending_tx' subscribers care, and hashing a
%% transaction is not free, so the early return is on whether anyone is
%% listening rather than inside the per-subscriber fold -- the same
%% shape as `fanout/2' below.
fanout_pending(SignedTx, #state{by_id = Subs}) ->
    case pending_subs(Subs) of
        [] ->
            ok;
        Pending ->
            try aetx_sign:hash(SignedTx) of
                TxHash ->
                    Hash = aerpc_encoding:format_tx_hash(TxHash),
                    %% The full-transaction form is built once and only
                    %% if some subscriber asked for it.
                    Full = pending_full_tx(Pending, SignedTx),
                    [send(Owner, Id, pending_payload(Criteria, Hash, Full))
                     || #sub{id = Id, owner = Owner, criteria = Criteria}
                            <- Pending],
                    ok
            catch _:_ -> ok
            end
    end.

pending_subs(Subs) ->
    [S || #sub{kind = pending_tx} = S <- maps:values(Subs)].

%% geth's second parameter: `true' means full transaction objects,
%% anything else means hashes. Both halves of this endpoint agree on
%% what a pending transaction is because both read the same event.
pending_full_tx(Pending, SignedTx) ->
    case lists:any(fun(#sub{criteria = true}) -> true;
                      (_Other)                -> false
                   end, Pending) of
        true  -> aerpc_tx:to_eth_tx(SignedTx, null, null, null);
        false -> undefined
    end.

pending_payload(true, _Hash, Full) when is_map(Full) -> Full;
pending_payload(_Other, Hash, _Full)                 -> Hash.

fanout(_KBHash, #state{by_id = Subs}) when map_size(Subs) =:= 0 ->
    %% No subscribers: do nothing at all. The payload build below fetches
    %% the whole generation and blooms every log in it, so running it
    %% eagerly would cost a full generation walk per key-block on every
    %% node -- including the overwhelmingly common case of a node with no
    %% WebSocket clients attached.
    ok;
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
    %% Reuse the existing eth_getLogs filter implementation for one-block
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
