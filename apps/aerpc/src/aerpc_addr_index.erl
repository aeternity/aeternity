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
handle_info({gproc_ps_event, top_changed, #{info := #{block_hash := KBHash}}},
            State) ->
    index_generation(KBHash),
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

%% Index every pubkey this layer can emit for the new generation: the
%% beneficiary, each tx's origin, each tx's `to' counterpart and any
%% contract a create produced. That is exactly the closure of addresses
%% that can come back to us as a 20-byte input for these blocks.
index_generation(KBHash) ->
    case aec_chain:get_generation_by_hash(KBHash, forward) of
        {ok, #{key_block := KB, micro_blocks := MBs}} ->
            index_beneficiary(KB),
            [index_tx(STx) || MB <- MBs, STx <- aec_blocks:txs(MB)],
            ok;
        error ->
            ok
    end.

index_beneficiary(KB) ->
    try do_insert(aec_headers:beneficiary(aec_blocks:to_key_header(KB)))
    catch _:_ -> false
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
