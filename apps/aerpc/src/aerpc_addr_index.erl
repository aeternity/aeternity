%%%-------------------------------------------------------------------
%%% @doc Reverse index from a 20-byte Ethereum-shaped address to the
%%% 32-byte æternity pubkey it was derived from.
%%%
%%% The outbound wire contract emits `first 20 bytes of the pubkey' as
%%% the address, which is lossy, so every inbound 20-byte address has to
%%% be resolved back through a lookup. This module owns that lookup.
%%%
%%% It deliberately mirrors `aerpc_log_indexer' / `aerpc_log_store':
%%% same app, ETS owned by this process, no `aecore' change. It is NOT
%%% started with the app -- `aerpc_sup:ensure_addr_index/0' starts it
%%% when the operator enables `http > endpoints > rpc', so a node with
%%% the endpoint off pays neither the backfill nor the per-block work.
%%%
%%% It is filled from two directions: a one-off walk of the accounts and
%%% contracts tries at the top block, and, from then on, each new block
%%% as `top_changed' announces it -- see `index_new_top/2', which is
%%% where the block type matters.
%%%
%%% == Why the incremental half reads the state delta ==
%%%
%%% Reading transaction fields is not enough and cannot be made enough.
%%% A `spend_tx' names its recipient, but `Chain.spend' from inside a
%%% contract call names nobody, and `Chain.create' produces a contract
%%% that appears in no field of any transaction type. Both change the
%%% tries all the same, and both were measured missing: 5 of 13 pubkeys
%%% on a lab chain were unresolvable, and every one of them answered
%%% `0x0'/`0x' on the wire -- a well-formed wrong answer. Restarting the
%%% node repaired it, because the backfill reads the tries, and fresh
%%% traffic drifted again.
%%%
%%% So the incremental path reads the same source the backfill trusts:
%%% every pubkey whose accounts- or contracts-trie entry CHANGED between
%%% the parent state and the new one. That is blind to the mechanism --
%%% it does not care whether a transaction field, a primop or a protocol
%%% upgrade put the entry there -- and it is bounded by what the block
%%% actually touched, because the diff prunes any subtree whose Merkle
%%% node is unchanged. The tx-field pass is kept in front of it: it is
%%% nearly free, and it still covers a block whose state has gone.
%%%
%%% == The invariant that matters ==
%%%
%%% Ethereum semantics say an unknown address has balance zero. That is
%%% only a correct answer here once the backfill is finished: before
%%% then, a lookup miss is indistinguishable from an account we simply
%%% have not walked to yet, and answering `0x0' would be a wrong number
%%% that looks like a right one. So `resolve/1' distinguishes the two:
%%%
%%%   {ok, Pubkey}  resolved
%%%   unknown       backfill complete, no such address on chain -> the
%%%                 caller may apply eth's zero/empty semantics
%%%   incomplete    backfill still running, a state delta is still
%%%                 catching up, or the index is not up -> the caller
%%%                 MUST return an error, never a value
%%%
%%% Crash-safety falls out of the same design: the tables are owned by
%%% this process, so a restart empties them and `resolve/1' answers
%%% `incomplete' until the rebuild finishes rather than serving a
%%% partially-populated index as if it were complete.
%%%
%%% == Derivation ==
%%%
%%% First 20 bytes of the pubkey, not a hash. ed25519 pubkeys are
%%% uniformly distributed, so truncation is as collision-safe as
%%% hashing here (~2^80 birthday bound), and grinding a collision needs
%%% the matching private key. A collision is still handled rather than
%%% assumed away: the first mapping wins, the second is logged and
%%% refused, and an overwrite never happens.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_addr_index).

-behaviour(gen_server).

-export([start_link/0,
         resolve/1,
         to_addr20/1,
         index_pubkey/1,
         rebuild/0,
         status/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(IDX,  aerpc_addr_idx).    %% {Addr20, Pubkey32}
-define(META, aerpc_addr_meta).   %% {backfill, _} | {indexed, N} | {collisions, N}

%% Entries walked per scheduling slice. The backfill runs inside this
%% gen_server, so it must yield often enough that `index_pubkey/1' and
%% the top_changed handler are not blocked behind a full trie walk.
-define(CHUNK, 2000).

%% A contracts-trie key is either a bare 32-byte contract pubkey or a
%% store key under that contract's own 32 bytes, so 64 nibbles is exactly
%% the depth at which the contracts live. Handed to the iterator as
%% `max_path_length' it is a pruning bound rather than a filter: the walk
%% stops descending at that depth and never reads a store node from the
%% store at all.
-define(CONTRACT_KEY_NIBBLES, 64).

%% Retry delay when the chain is not up yet at start.
-define(RETRY_MS, 1000).

%% Trie nodes visited by ONE SCHEDULING SLICE of a state delta -- not a
%% ceiling on the delta. A delta that fits in a slice runs exactly as it
%% always did, in the `top_changed' handler, with no extra message and
%% no visible state change; one that does not is suspended and resumed
%% from the message loop, so `index_pubkey/1' and the next block are
%% served in between rather than queued behind a 36-second walk.
%%
%% It is deliberately the same order as `?CHUNK': the number that matters
%% is how long the process is unavailable, and both loops answer to that.
%% A real mainnet generation costs ~880 nodes, so the common case never
%% reaches the slice boundary at all.
-define(DEFAULT_DELTA_SLICE_NODES, 2000).

%% One trie's half of a delta, suspended between slices. `items' is the
%% whole remaining walk: `walk_delta/4' keeps its worklist flat for
%% exactly this reason, so a suspension is a list and not a stack.
-record(walk, {which :: accounts | contracts,
               items :: list(),
               new   :: aeu_mtrees:mtree(),
               old   :: aeu_mtrees:mtree()}).

%% One in-flight state delta: the walks it still owes, and what it cost.
-record(delta, {from    :: binary(),
                to      :: binary(),
                walks   :: [#walk{}],
                nodes   = 0 :: non_neg_integer(),
                yielded = false :: boolean()}).

-record(state, {phase  :: pending | accounts | contracts | complete,
                %% A delta could not be completed while a walk was
                %% already in flight, so that walk's snapshot is behind:
                %% go round again rather than declaring `complete'.
                resync = false :: boolean(),
                %% The suspended state delta, if one is mid-walk.
                delta   = undefined :: undefined | #delta{},
                %% `{From, To}' for the walk to run once that one lands.
                %% A burst coalesces into this single pair rather than
                %% queueing one walk per block -- see `start_delta/3'.
                pending = undefined :: undefined | {binary(), binary()}}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc First 20 bytes of a 32-byte pubkey.
-spec to_addr20(binary()) -> binary().
to_addr20(<<Addr20:20/binary, _Rest/binary>>) -> Addr20.

%% @doc Resolve a 20-byte address. Read straight from ETS: this is on
%% the request path for every address-keyed method, so it must not go
%% through the gen_server.
-spec resolve(binary()) -> {ok, binary()} | unknown | incomplete.
resolve(<<Addr20:20/binary>>) ->
    case ets:info(?IDX) of
        undefined ->
            %% Not running at all -- never answer as if it were empty.
            incomplete;
        _Info ->
            case ets:lookup(?IDX, Addr20) of
                [{_, Pubkey}] -> {ok, Pubkey};
                []            -> miss()
            end
    end;
resolve(_Other) ->
    incomplete.

miss() ->
    case backfill_state() of
        complete -> unknown;
        _Other   -> incomplete
    end.

%% @doc Add one pubkey to the index. Synchronous so a caller (and a
%% test) can rely on it having landed.
-spec index_pubkey(binary()) -> ok.
index_pubkey(Pubkey) when is_binary(Pubkey) ->
    gen_server:call(?MODULE, {index_pubkey, Pubkey}).

%% @doc Drop everything and walk the tries again. The index goes back to
%% answering `incomplete' for the duration, by construction.
-spec rebuild() -> ok.
rebuild() ->
    gen_server:call(?MODULE, rebuild).

%% @doc `deltas' / `delta_misses' are what makes the incremental half
%% observable. A miss is a block whose parent or own state could not be
%% read, so its delta never ran; on a micro block the next key block's
%% generation-wide delta covers it again, on a key block it is a real
%% gap that only `rebuild/0' closes. Non-zero is a signal, not noise.
-spec status() -> map().
status() ->
    #{backfill     => backfill_state(),
      indexed      => counter(indexed),
      collisions   => counter(collisions),
      deltas       => counter(deltas),
      delta_misses => counter(delta_misses),
      delta_yields => counter(delta_yields),
      resyncs      => counter(resyncs)}.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ?IDX  = ets:new(?IDX,  [set, named_table, protected, {read_concurrency, true}]),
    ?META = ets:new(?META, [set, named_table, protected, {read_concurrency, true}]),
    reset_meta(),
    %% Keep new generations flowing into the index while the backfill is
    %% still walking; the two paths are independent and both idempotent.
    try aec_events:subscribe(top_changed)
    catch _:_ -> ok
    end,
    self() ! start_backfill,
    {ok, #state{phase = pending}}.

handle_call({index_pubkey, Pubkey}, _From, State) ->
    do_insert(Pubkey),
    {reply, ok, State};
handle_call(rebuild, _From, State) ->
    ets:delete_all_objects(?IDX),
    reset_meta(),
    self() ! start_backfill,
    {reply, ok, State#state{phase = pending}};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_backfill, State) ->
    case top_trees() of
        {ok, Trees} ->
            set_backfill(running),
            Iter = aec_accounts_trees:mtree_iterator(aec_trees:accounts(Trees)),
            self() ! {backfill_accounts, Iter, Trees},
            {noreply, State#state{phase = accounts}};
        error ->
            %% Chain not up yet. The endpoint is opt-in and answers
            %% `incomplete' meanwhile, so retrying is safe.
            erlang:send_after(?RETRY_MS, self(), start_backfill),
            {noreply, State#state{phase = pending}}
    end;
handle_info({backfill_accounts, Iter, Trees}, State) ->
    case walk_accounts(Iter, ?CHUNK) of
        {more, Iter1} ->
            self() ! {backfill_accounts, Iter1, Trees},
            {noreply, State};
        done ->
            %% Contract pubkeys share the account key space, but a
            %% contract with no account entry would still have to
            %% resolve, so walk the contracts trie too.
            %% Not `Iter': the accounts iterator is still bound by the
            %% function head, so matching `{ok, Iter}' here would compare
            %% against it rather than bind the new one.
            case contracts_iterator(Trees) of
                {ok, CtIter} -> self() ! {backfill_contracts, CtIter};
                error        -> self() ! {backfill_contracts, done}
            end,
            {noreply, State#state{phase = contracts}}
    end;
handle_info({backfill_contracts, Iter}, State) ->
    case walk_contracts(Iter, ?CHUNK) of
        {more, Iter1} ->
            self() ! {backfill_contracts, Iter1},
            {noreply, State};
        done when State#state.resync ->
            %% Do not claim `complete' on a snapshot we already know is
            %% stale -- that is the one moment a miss would turn from
            %% `incomplete' into a confident `unknown'.
            {noreply, start_resync(State#state{resync = false})};
        done ->
            set_backfill(complete),
            lager:info("aerpc address index backfill complete: ~p entries, "
                       "~p collision(s)",
                       [counter(indexed), counter(collisions)]),
            {noreply, State#state{phase = complete}}
    end;
handle_info({gproc_ps_event, top_changed, #{info := Info}}, State)
  when is_map(Info) ->
    {noreply, index_new_top(Info, State)};
handle_info({gproc_ps_event, top_changed, _Other}, State) ->
    {noreply, State};
%% Resume the suspended delta. The token carries nothing: the walk lives
%% in the state, so a token that outlives its delta -- one sent just
%% before a re-walk took the work over -- resumes nothing.
handle_info(delta_slice, #state{delta = undefined} = State) ->
    {noreply, State};
handle_info(delta_slice, #state{delta = Delta} = State) ->
    {noreply, run_delta(Delta, State)};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Backfill
%% ===================================================================

top_trees() ->
    case aec_chain:top_block_hash() of
        undefined ->
            error;
        Hash ->
            try aec_chain:get_block_state_partial(Hash, [accounts, contracts])
            catch _:_ -> error
            end
    end.

walk_accounts(Iter, 0) ->
    {more, Iter};
walk_accounts(Iter, N) ->
    case aeu_mtrees:iterator_next(Iter) of
        '$end_of_table' ->
            done;
        {Pubkey, _Value, Iter1} ->
            do_insert(Pubkey),
            walk_accounts(Iter1, N - 1)
    end.

%% The contracts trie also holds each contract's store, under keys
%% prefixed with the 32-byte contract id. Only the bare 32-byte keys are
%% contracts.
%%
%% Reading that as a list -- `aect_state_tree:to_list/1' -- was measured
%% at mainnet scale and is where this backfill spent almost all of its
%% cost: 10m37s of a 12m26s walk and every byte of a 2.12 GiB process
%% heap, to insert zero entries that the accounts walk had not already
%% inserted. Two separate faults, and both are removed here rather than
%% traded off:
%%
%%   * it materialises the WHOLE trie, stores included, onto one process
%%     heap, so the peak is the size of the trie and not of the answer;
%%   * it is the one step of the backfill outside the `?CHUNK' loop, so
%%     for its duration this gen_server answers nothing.
%%
%% An iterator bounded to `?CONTRACT_KEY_NIBBLES' fixes both. It is
%% driven `?CHUNK' entries at a time like the accounts walk, so nothing
%% accumulates and the process stays responsive throughout; and because
%% the bound prunes the descent, the store subtrees the old code read,
%% decoded and then discarded are never read at all.
%%
%% Note what is NOT claimed: that the contracts walk can be dropped. On
%% mainnet at height 1,314,674 it inserted nothing, because every
%% contract there also holds an accounts-trie entry -- but that is an
%% observation about one chain at one height, not an invariant this
%% module is entitled to assume. The walk stays; only its cost goes.
contracts_iterator(Trees) ->
    try
        {_Root, Tree} = trie(contracts, Trees),
        {ok, aeu_mtrees:iterator(Tree,
                                 [{max_path_length, ?CONTRACT_KEY_NIBBLES}])}
    catch _:_ -> error
    end.

walk_contracts(done, _N) ->
    done;
walk_contracts(Iter, 0) ->
    {more, Iter};
walk_contracts(Iter, N) ->
    case aeu_mtrees:iterator_next(Iter) of
        '$end_of_table' ->
            done;
        {Key, _Value, Iter1} ->
            %% The bound already stops the walk above every store key, so
            %% `do_insert/1' rejecting anything but 32 bytes is a belt on
            %% top of it rather than the filter it used to be.
            do_insert(Key),
            walk_contracts(Iter1, N - 1)
    end.

%% ===================================================================
%% Incremental maintenance
%% ===================================================================

%% `top_changed' carries the hash of the NEW TOP BLOCK, which is a micro
%% block whenever one is mined and a key block otherwise. Handing that
%% hash to `get_generation_by_hash/2' -- which is keyed by the key block
%% -- is the same micro-vs-key confusion that broke the receipts, one
%% layer up, and it made this path dead: on a micro block the lookup
%% returned `error', and on a key block the generation it opens has no
%% micro blocks yet. Only the beneficiary was ever indexed, so every
%% address first appearing after backfill resolved to `unknown' and was
%% served eth's zero-and-empty default -- a wrong answer that is
%% well-formed, which is exactly what the index exists to prevent.
%%
%% So dispatch on the block type the event already tells us:
%%
%%   micro -> this micro block's own transaction fields, then the state
%%            delta from its parent. Precise, and O(what this block
%%            changed) rather than O(generation) per event.
%%   key   -> the beneficiary, the transaction fields of the generation
%%            this key block just CLOSED, and the state delta across
%%            that whole generation -- from the previous key block's
%%            state to this key block's. Both are the catch-up net: a
%%            reorg or a sync burst advances the top in one event and
%%            the intervening micro blocks never raise one of their own,
%%            so without it their changes are missed until a rebuild.
%%            One diff covers the generation because trie entries are
%%            only ever added or updated, never removed, so the net
%%            difference between the two states is the union of every
%%            change in between. Re-inserting is idempotent.
index_new_top(#{block_hash := Hash} = Info, State) ->
    case block_type(Hash, Info) of
        micro   -> index_micro_block(Hash, Info, State);
        key     -> index_key_block(Hash, State);
        unknown -> State
    end;
index_new_top(_Other, State) ->
    State.

%% The event carries `block_type'; fall back to the header so a change in
%% the event shape degrades to a DB read rather than to doing nothing.
block_type(_Hash, #{block_type := Type}) when Type =:= key; Type =:= micro ->
    Type;
block_type(Hash, _Info) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} -> aec_headers:type(Header);
        error        -> unknown
    end.

index_micro_block(Hash, Info, State) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            [index_tx(STx) || STx <- aec_blocks:txs(Block)],
            ok;
        error ->
            ok
    end,
    start_delta(parent_hash(Hash, Info), Hash, State).

index_key_block(Hash, State) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} ->
            catch do_insert(aec_headers:beneficiary(Header)),
            PrevKeyHash = aec_headers:prev_key_hash(Header),
            sweep_generation(PrevKeyHash),
            start_delta(PrevKeyHash, Hash, State);
        error ->
            State
    end.

%% The event already carries the parent; the header read is only there so
%% a change in the event shape costs a lookup rather than the delta.
parent_hash(_Hash, #{prev_hash := Prev}) when is_binary(Prev) ->
    Prev;
parent_hash(Hash, _Info) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} -> aec_headers:prev_hash(Header);
        error        -> undefined
    end.

%% Re-walk a closed generation. Every insert is idempotent, so this only
%% costs a lookup per already-known pubkey.
sweep_generation(PrevKeyHash) ->
    case aec_chain:get_generation_by_hash(PrevKeyHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            [index_tx(STx) || MB <- MBs, STx <- aec_blocks:txs(MB)],
            ok;
        error ->
            ok
    end.

index_tx(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    catch do_insert(aetx:origin(Tx)),
    {Type, _Body} = aetx:specialize_type(Tx),
    index_counterparty(Type, Tx).

index_counterparty(spend_tx, Tx) ->
    insert_from_id(Tx, recipient_id);
index_counterparty(contract_call_tx, Tx) ->
    insert_from_id(Tx, contract_id);
index_counterparty(contract_create_tx, Tx) ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        do_insert(Mod:contract_pubkey(Inner))
    catch _:_ -> false
    end;
index_counterparty(_Other, _Tx) ->
    false.

insert_from_id(Tx, Accessor) ->
    try
        {Mod, Inner} = aetx:specialize_callback(Tx),
        case aeser_id:specialize(Mod:Accessor(Inner)) of
            {account,  PK} -> do_insert(PK);
            {contract, PK} -> do_insert(PK);
            _Other         -> false
        end
    catch _:_ -> false
    end.

%% ===================================================================
%% State delta
%%
%% Index every pubkey whose accounts- or contracts-trie entry differs
%% between two states. Both tries are Merkle Patricia tries over the same
%% node store, so the diff is a one-sided walk of the NEW trie that stops
%% descending the moment `has_node/3' says the old trie holds the very
%% same node at the very same path: an unchanged subtree costs one
%% comparison whatever its size. What is left is exactly the paths the
%% block touched.
%%
%% Two properties make this cheap enough to run per block:
%%
%%   * a path of 256 bits or more already names its pubkey, so the walk
%%     inserts it and stops. A contract call that wrote ten thousand
%%     store slots is one insert, not ten thousand -- the store lives in
%%     the contracts trie under the contract's own 32-byte prefix.
%%   * `unfold/3' hands back child nodes without decoding them, so a
%%     pruned subtree is never read from the store at all.
%% ===================================================================

%% @doc Begin the delta for a new top, and run its first slice inline.
%%
%% Bursts are the reason this is not a straight function call. Mainnet
%% has produced a generation creating 15,508 pubkeys -- 149,025 trie
%% nodes, ~36 s of walking -- and ten more over the crossing point inside
%% one 2,000-generation window, closer together than a re-walk takes. The
%% old code capped the walk and handed anything above the cap to a full
%% re-walk of both tries, which is both slower than the walk it replaced
%% and, at that clustering, effectively continuous.
%%
%% So the walk is suspended and resumed instead of abandoned, and raising
%% the cap is not the fix either: 149,025 nodes under the old code is 36
%% seconds during which this gen_server answers nothing at all.
start_delta(_From, To, #state{delta = InFlight} = State)
  when InFlight =/= undefined, is_binary(To) ->
    %% A walk is already in flight. Trie entries are only added or
    %% updated, never removed, so ONE later walk from that walk's own
    %% target to the newest top covers every generation in between: a
    %% burst costs one catch-up walk, not one walk per block.
    State#state{pending = coalesce(State#state.pending, InFlight#delta.to, To)};
start_delta(From, To, State) when is_binary(From), is_binary(To) ->
    case {partial_trees(From), partial_trees(To)} of
        {{ok, Old}, {ok, New}} ->
            try new_delta(From, To, New, Old) of
                #delta{walks = []} -> State;   %% both roots unchanged
                Delta              -> run_delta(Delta, State)
            catch Class:Reason:St ->
                delta_failed(setup, Class, Reason, St, State)
            end;
        _Unavailable ->
            %% The state is gone or not written yet, so this block's
            %% changes were not read. Count it: on a micro block the next
            %% key block re-diffs the whole generation and closes it, on
            %% a key block it stays open until a rebuild, and either way
            %% a silent zero is what this module exists to prevent.
            bump(delta_misses),
            State
    end;
start_delta(_From, _To, State) ->
    bump(delta_misses),
    State.

%% The `from' of a deferred walk is pinned to the in-flight walk's target
%% the first time we defer; only the target moves as more tops arrive.
coalesce(undefined, InFlightTo, To) -> {InFlightTo, To};
coalesce({From, _Superseded}, _InFlightTo, To) -> {From, To}.

partial_trees(Hash) ->
    try aec_chain:get_block_state_partial(Hash, [accounts, contracts])
    catch _:_ -> error
    end.

new_delta(From, To, NewTrees, OldTrees) ->
    #delta{from  = From,
           to    = To,
           walks = [W || Which <- [accounts, contracts],
                         W <- [mk_walk(Which, NewTrees, OldTrees)],
                         W =/= skip]}.

mk_walk(Which, NewTrees, OldTrees) ->
    {NewRoot, NewTree} = trie(Which, NewTrees),
    {OldRoot, OldTree} = trie(Which, OldTrees),
    case NewRoot =:= OldRoot of
        true  -> skip;
        false -> #walk{which = Which, new = NewTree, old = OldTree,
                       items = aeu_mp_trees:unfold(<<>>, <<>>, NewTree)}
    end.

run_delta(Delta, State) ->
    try run_slice(Delta, delta_slice_nodes()) of
        {done, Delta1} ->
            finish_delta(Delta1, State);
        {suspended, Delta1} ->
            self() ! delta_slice,
            State#state{delta = mark_yielded(Delta1)}
    catch Class:Reason:St ->
        delta_failed(walking(Delta), Class, Reason, St, State)
    end.

run_slice(#delta{walks = []} = D, _Budget) ->
    {done, D};
run_slice(#delta{walks = [W | Rest]} = D, Budget) when Budget > 0 ->
    case walk_delta(W#walk.items, W#walk.new, W#walk.old, Budget) of
        {ok, Left} ->
            bump(deltas),
            run_slice(D#delta{walks = Rest,
                              nodes = D#delta.nodes + Budget - Left}, Left);
        {suspended, Items} ->
            {suspended, D#delta{walks = [W#walk{items = Items} | Rest],
                                nodes  = D#delta.nodes + Budget}}
    end;
run_slice(D, _Exhausted) ->
    {suspended, D}.

%% The first suspension is where this delta stops being invisible: from
%% here until it lands, the index is knowingly behind the top, so a miss
%% must not become eth's confident zero.
mark_yielded(#delta{yielded = true} = D) ->
    D;
mark_yielded(D) ->
    bump(delta_yields),
    suspend_completeness(),
    D#delta{yielded = true}.

finish_delta(#delta{yielded = Yielded, nodes = Nodes}, State) ->
    case Yielded of
        true  -> lager:info("aerpc address index caught up across a large "
                            "state delta: ~p trie nodes", [Nodes]);
        false -> ok
    end,
    State1 = State#state{delta = undefined},
    case State1#state.pending of
        undefined ->
            %% Idle and caught up -- and only now, because a pending walk
            %% means the index is still behind whatever raised it.
            resume_completeness(),
            State1;
        {From, To} ->
            start_delta(From, To, State1#state{pending = undefined})
    end.

%% A delta that stops half-way has already inserted part of what it
%% found, so it can neither be replayed from the start nor written off as
%% a miss. Hand it to the re-walk, which is the one path that closes a
%% gap this module cannot see the shape of. Never crash out of it: a
%% restart would empty the table and answer `incomplete' for every
%% address until the walk finished.
delta_failed(Where, Class, Reason, St, State) ->
    lager:error("aerpc address index ~p delta failed: ~p:~p ~p",
                [Where, Class, Reason, St]),
    bump(delta_misses),
    request_resync(State#state{delta = undefined, pending = undefined}).

walking(#delta{walks = [#walk{which = Which} | _]}) -> Which;
walking(_Delta)                                     -> unknown.

%% `catching_up' is `complete' minus the promise: every entry already in
%% the table still answers, and only a MISS changes meaning, from eth's
%% zero back to `incomplete'. Both are idempotent and neither overwrites
%% `running', so a re-walk that starts mid-delta keeps its own state.
suspend_completeness() ->
    case backfill_state() of
        complete -> set_backfill(catching_up);
        _Other   -> ok
    end.

resume_completeness() ->
    case backfill_state() of
        catching_up -> set_backfill(complete);
        _Other      -> ok
    end.

%% Rebuild the bare Merkle trie from the accessors each wrapper exports,
%% rather than reaching into its record.
trie(accounts, Trees) ->
    T = aec_trees:accounts(Trees),
    mk_trie(aec_accounts_trees:root_hash(T), aec_accounts_trees:db(T));
trie(contracts, Trees) ->
    T = aec_trees:contracts(Trees),
    mk_trie(aect_state_tree:root_hash(T), aect_state_tree:db(T)).

mk_trie({error, empty}, _DB)  -> {empty, aeu_mtrees:empty()};
mk_trie({ok, Hash}, {ok, DB}) -> {Hash, aeu_mtrees:new_with_backend(Hash, DB)}.

%% Suspendable because the worklist is FLAT. Descending used to recurse
%% -- walk the children, then carry the leftover budget back to the
%% siblings -- which kept half the remaining walk on the Erlang stack
%% where nothing can store it. Pushing the children onto the front of the
%% same list keeps the depth-first order and the identical set of
%% inserts, and makes `Items' the whole of what is left to do: a
%% suspension is then just that list.
walk_delta([], _New, _Old, Budget) ->
    {ok, Budget};
walk_delta(Items, _New, _Old, Budget) when Budget =< 0 ->
    {suspended, Items};
walk_delta([{leaf, Path} | T], New, Old, Budget) ->
    _ = insert_path(Path),
    walk_delta(T, New, Old, Budget - 1);
walk_delta([{node, Path, Node} | T], New, Old, Budget) ->
    case insert_path(Path) of
        indexed ->
            walk_delta(T, New, Old, Budget - 1);
        deeper ->
            case aeu_mp_trees:has_node(Path, Node, Old) of
                yes ->
                    %% Same node, same path: the whole subtree under it
                    %% is byte-identical and can teach us nothing.
                    walk_delta(T, New, Old, Budget - 1);
                _NoOrMaybe ->
                    Children = aeu_mp_trees:unfold(Path, Node, New),
                    walk_delta(Children ++ T, New, Old, Budget - 1)
            end
    end.

%% A trie path is the key's own bits, so its first 256 name the pubkey --
%% for a bare accounts or contracts entry, and for every store key under
%% that contract, since `compute_contract_store_id/1' prefixes the store
%% with the contract's 32 bytes.
%%
%% Matching on bits rather than whole bytes is load-bearing, not tidiness.
%% `unfold/3' follows an extension node before it emits anything, so the
%% node at exactly 64 nibbles is never handed back as an item of its own:
%% a contract's children arrive at 65 nibbles, which is not byte-aligned.
%% Requiring a whole binary here would let a contract whose own entry
%% changed while its store did not slip past, because every child would
%% then be pruned as unchanged.
insert_path(Path) when is_bitstring(Path), bit_size(Path) >= 256 ->
    <<Pubkey:32/binary, _Rest/bitstring>> = Path,
    _ = do_insert(Pubkey),
    indexed;
insert_path(_Shorter) ->
    deeper.

delta_slice_nodes() ->
    case application:get_env(aerpc, addr_delta_slice_nodes,
                             ?DEFAULT_DELTA_SLICE_NODES) of
        N when is_integer(N), N > 0 -> N;
        _Other                      -> ?DEFAULT_DELTA_SLICE_NODES
    end.

%% ===================================================================
%% Resync
%%
%% A re-walk that KEEPS what is already indexed. Trie entries are only
%% added or updated, never removed, so an existing mapping cannot become
%% wrong -- and keeping it means the addresses we already know keep
%% answering while the walk catches up, instead of every address in the
%% index answering `incomplete' for the duration. `rebuild/0' is the
%% operator's hammer and still drops everything.
%% ===================================================================

request_resync(#state{phase = complete} = State) ->
    start_resync(State);
request_resync(State) ->
    State#state{resync = true}.

start_resync(State) ->
    bump(resyncs),
    lager:warning("aerpc address index re-walking the tries: a state "
                  "delta could not be completed", []),
    set_backfill(running),
    self() ! start_backfill,
    State#state{phase = pending}.

%% ===================================================================
%% Store
%% ===================================================================

do_insert(<<Pubkey:32/binary>>) ->
    Addr20 = to_addr20(Pubkey),
    case ets:lookup(?IDX, Addr20) of
        [] ->
            ets:insert(?IDX, {Addr20, Pubkey}),
            bump(indexed),
            true;
        [{_, Pubkey}] ->
            false;                       %% already mapped to this pubkey
        [{_, Existing}] ->
            %% First mapping wins. Overwriting would silently re-point
            %% an address that has already been served to a client.
            bump(collisions),
            lager:error("aerpc address index collision on ~s: keeping ~s, "
                        "refusing ~s",
                        [hex(Addr20), hex(Existing), hex(Pubkey)]),
            false
    end;
do_insert(_NotAPubkey) ->
    false.

hex(Bin) -> binary:encode_hex(Bin).

%% ===================================================================
%% Meta
%% ===================================================================

reset_meta() ->
    ets:insert(?META, [{backfill, pending}, {indexed, 0}, {collisions, 0},
                       {deltas, 0}, {delta_misses, 0}, {delta_yields, 0},
                       {resyncs, 0}]),
    ok.

set_backfill(Value) ->
    ets:insert(?META, {backfill, Value}).

backfill_state() ->
    case ets:info(?META) of
        undefined -> pending;
        _Info ->
            case ets:lookup(?META, backfill) of
                [{backfill, V}] -> V;
                []              -> pending
            end
    end.

counter(Key) ->
    case ets:info(?META) of
        undefined -> 0;
        _Info ->
            case ets:lookup(?META, Key) of
                [{Key, N}] -> N;
                []         -> 0
            end
    end.

bump(Key) ->
    ets:update_counter(?META, Key, {2, 1}, {Key, 0}).
