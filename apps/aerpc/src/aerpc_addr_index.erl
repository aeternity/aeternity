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
%%% as `top_changed' announces it -- see `index_new_top/1', which is
%%% where the block type matters.
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
%%%   incomplete    backfill still running, or the index is not up ->
%%%                 the caller MUST return an error, never a value
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

%% Retry delay when the chain is not up yet at start.
-define(RETRY_MS, 1000).

-record(state, {phase :: pending | accounts | contracts | complete}).

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

-spec status() -> map().
status() ->
    #{backfill   => backfill_state(),
      indexed    => counter(indexed),
      collisions => counter(collisions)}.

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
            Contracts = contract_pubkeys(Trees),
            self() ! {backfill_contracts, Contracts},
            {noreply, State#state{phase = contracts}}
    end;
handle_info({backfill_contracts, Pubkeys}, State) ->
    case walk_list(Pubkeys, ?CHUNK) of
        {more, Rest} ->
            self() ! {backfill_contracts, Rest},
            {noreply, State};
        done ->
            set_backfill(complete),
            lager:info("aerpc address index backfill complete: ~p entries, "
                       "~p collision(s)",
                       [counter(indexed), counter(collisions)]),
            {noreply, State#state{phase = complete}}
    end;
handle_info({gproc_ps_event, top_changed, #{info := Info}}, State)
  when is_map(Info) ->
    index_new_top(Info),
    {noreply, State};
handle_info({gproc_ps_event, top_changed, _Other}, State) ->
    {noreply, State};
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

walk_list([], _N)       -> done;
walk_list(Rest, 0)      -> {more, Rest};
walk_list([PK | T], N)  -> do_insert(PK), walk_list(T, N - 1).

%% The contracts trie also holds each contract's store, under keys
%% prefixed with the 32-byte contract id. Only the bare 32-byte keys are
%% contracts.
contract_pubkeys(Trees) ->
    try
        [PK || {PK, _V} <- aect_state_tree:to_list(aec_trees:contracts(Trees)),
               byte_size(PK) =:= 32]
    catch _:_ -> []
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
%%   micro -> index that micro block's own transactions. Precise, and
%%            O(txs in this block) rather than O(generation) per event.
%%   key   -> index the beneficiary, and sweep the generation this key
%%            block just CLOSED (its prev_key_hash). The sweep is the
%%            catch-up net: a reorg or a sync burst advances the top in
%%            one event and the intervening micro blocks never raise one
%%            of their own, so without it those txs are missed until a
%%            rebuild. Re-inserting is idempotent and bounded by the
%%            block gas limit, so the sweep is cheap.
index_new_top(#{block_hash := Hash} = Info) ->
    case block_type(Hash, Info) of
        micro   -> index_micro_block(Hash);
        key     -> index_key_block(Hash);
        unknown -> ok
    end;
index_new_top(_Other) ->
    ok.

%% The event carries `block_type'; fall back to the header so a change in
%% the event shape degrades to a DB read rather than to doing nothing.
block_type(_Hash, #{block_type := Type}) when Type =:= key; Type =:= micro ->
    Type;
block_type(Hash, _Info) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} -> aec_headers:type(Header);
        error        -> unknown
    end.

index_micro_block(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            [index_tx(STx) || STx <- aec_blocks:txs(Block)],
            ok;
        error ->
            ok
    end.

index_key_block(Hash) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} ->
            catch do_insert(aec_headers:beneficiary(Header)),
            sweep_generation(aec_headers:prev_key_hash(Header));
        error ->
            ok
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
    ets:insert(?META, [{backfill, pending}, {indexed, 0}, {collisions, 0}]),
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
