%%%-------------------------------------------------------------------
%%% @doc Real-time log index for `eth_getLogs'.
%%%
%%% == What it indexes, and when ==
%%%
%%% Only CLOSED generations. A generation stays open until the next key
%%% block arrives, so indexing on every micro block and advancing the
%%% watermark with it would mark a height as covered while more micro
%%% blocks were still landing under it -- and a later query for that
%%% height would then read the index and silently miss the newer logs.
%%% So: on a key block at height H, index generation H-1 and set the
%%% watermark to H-1. Queries for the open generation fall through to
%%% `aerpc_logs`' inline walker, which is always current.
%%%
%%% No backfill. At start the floor is `top + 1', so everything at or
%%% below the startup top stays with the inline walker forever. That
%%% keeps boot free; an archive mode would be a separate decision.
%%%
%%% == Retention ==
%%%
%%% The index is a bounded sliding window, `http > rpc >
%%% log_retention_blocks' generations wide (0 disables eviction, which is
%%% unbounded memory and is not the default). After each indexed
%%% generation, entries below the window are deleted AND THE FLOOR IS
%%% RAISED to match. Raising the floor is the load-bearing half: it is
%%% what makes `aerpc_log_store:indexed/1' report the narrowed coverage,
%%% so a query reaching below the window is sent to the walker instead of
%%% being answered from an index that no longer holds those heights.
%%% Evicting without moving the floor would return a short log list that
%%% looks exactly like a complete one -- the same shape of wrong answer
%%% the address index refuses to give.
%%%
%%% The tables are owned by this process, so they vanish when it stops
%%% and a restart cannot serve stale data.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_log_indexer).

-behaviour(gen_server).

-export([start_link/0,
         status/0,
         retention/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Generations kept in the index when the operator sets nothing. Wide
%% enough that an indexer-backed query is the common case, narrow enough
%% that the table cannot grow without bound on a long-running node.
-define(DEFAULT_RETENTION, 10000).

-record(state, {started_at :: non_neg_integer()}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc `#{floor, watermark, entries, retention}'. `floor' and
%% `watermark' bound what the index actually covers; anything outside
%% that range is served by the walker.
-spec status() -> map().
status() ->
    #{floor     => aerpc_log_store:floor_height(),
      watermark => aerpc_log_store:watermark(),
      entries   => aerpc_log_store:size(),
      retention => retention()}.

%% @doc Window width in generations. 0 means "never evict".
-spec retention() -> non_neg_integer().
retention() ->
    case application:get_env(aerpc, log_retention_blocks, ?DEFAULT_RETENTION) of
        N when is_integer(N), N >= 0 -> N;
        _Other                       -> ?DEFAULT_RETENTION
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    aerpc_log_store:init(),
    Started = current_top_height(),
    %% Heights up to and including `Started' are not indexed by this
    %% process, so a query for them falls through to the walker.
    aerpc_log_store:set_floor(Started + 1),
    aerpc_log_store:set_watermark(Started),
    try aec_events:subscribe(top_changed)
    catch _:_ -> ok
    end,
    {ok, #state{started_at = Started}}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, top_changed, #{info := Info}}, State)
  when is_map(Info) ->
    maybe_index(Info, State),
    {noreply, State};
handle_info({gproc_ps_event, top_changed, _Other}, State) ->
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

current_top_height() ->
    try aec_chain:top_header() of
        undefined -> 0;
        Header    -> aec_headers:height(Header)
    catch _:_ -> 0
    end.

%% A micro block extends the open generation; nothing to close, so
%% nothing to index. A key block closes the previous one, which is the
%% only point at which a generation's log set is final.
maybe_index(#{block_hash := Hash} = Info, State) ->
    case block_type(Hash, Info) of
        key   -> index_closed_generation(Hash, State);
        _Else -> ok
    end;
maybe_index(_Info, _State) ->
    ok.

block_type(_Hash, #{block_type := Type}) when Type =:= key; Type =:= micro ->
    Type;
block_type(Hash, _Info) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} -> aec_headers:type(Header);
        error        -> unknown
    end.

index_closed_generation(KeyHash, #state{started_at = Started}) ->
    case aec_chain:get_header(KeyHash) of
        {ok, Header} ->
            PrevKey = aec_headers:prev_key_hash(Header),
            Closed  = aec_headers:height(Header) - 1,
            case Closed > Started of
                true  -> index_generation(PrevKey, Closed);
                false -> ok   %% below the floor; the walker owns it
            end;
        error ->
            ok
    end.

index_generation(KBHash, Height) ->
    case aec_chain:get_generation_by_hash(KBHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            aerpc_log_store:insert_many(collect_entries(MBs, KBHash, Height)),
            aerpc_log_store:set_watermark(Height),
            evict(Height);
        error ->
            ok
    end.

%% Slide the window forward, then tell the store how far back it can now
%% honestly answer from.
evict(Watermark) ->
    case retention() of
        0 ->
            ok;
        Keep ->
            NewFloor = max(current_floor(), Watermark - Keep + 1),
            case NewFloor > current_floor() of
                true ->
                    Removed = aerpc_log_store:evict_below(NewFloor),
                    aerpc_log_store:set_floor(NewFloor),
                    Removed > 0 andalso
                        lager:debug("aerpc log index evicted ~p entries below "
                                    "height ~p", [Removed, NewFloor]),
                    ok;
                false ->
                    ok
            end
    end.

current_floor() ->
    case aerpc_log_store:floor_height() of
        undefined -> 0;
        H         -> H
    end.

collect_entries(MBs, KBHash, Height) ->
    collect_micros(MBs, KBHash, Height, 0, 0, []).

collect_micros([], _KBH, _H, _TxIdx, _LogIdx, Acc) ->
    lists:reverse(Acc);
collect_micros([MB | Rest], KBH, H, TxIdx, LogIdx, Acc) ->
    {ok, MBHash} = aec_blocks:hash_internal_representation(MB),
    Txs = aec_blocks:txs(MB),
    {NewAcc, TxIdx1, LogIdx1} =
        walk_txs(Txs, MBHash, KBH, H, TxIdx, LogIdx, Acc),
    collect_micros(Rest, KBH, H, TxIdx1, LogIdx1, NewAcc).

walk_txs([], _MBH, _KBH, _H, TxIdx, LogIdx, Acc) ->
    {Acc, TxIdx, LogIdx};
walk_txs([STx | Rest], MBH, KBH, H, TxIdx, LogIdx, Acc) ->
    %% The call object is read at the state of the micro block holding
    %% the tx. Reading at the generation's key block -- which this did --
    %% finds nothing, because the calls trie resets per generation and at
    %% the key block that generation's calls do not exist yet. The index
    %% was therefore recording zero logs for every block.
    case logs_for_tx(STx, MBH) of
        [] ->
            walk_txs(Rest, MBH, KBH, H, TxIdx + 1, LogIdx, Acc);
        Logs ->
            TxHash = aetx_sign:hash(STx),
            {Acc1, LogIdx1} = build_entries(Logs, MBH, KBH, H, TxIdx,
                                            LogIdx, TxHash, Acc),
            walk_txs(Rest, MBH, KBH, H, TxIdx + 1, LogIdx1, Acc1)
    end.

build_entries([], _MBH, _KBH, _H, _TxIdx, LogIdx, _TxHash, Acc) ->
    {Acc, LogIdx};
build_entries([{Address, Topics, Data} | Rest], MBH, KBH, H, TxIdx, LogIdx,
              TxHash, Acc) ->
    Entry = aerpc_log_store:make_entry(Address, H, TxIdx, LogIdx,
                                        Topics, Data, KBH, MBH, TxHash),
    build_entries(Rest, MBH, KBH, H, TxIdx, LogIdx + 1, TxHash,
                  [Entry | Acc]).

%% Re-implemented locally rather than calling into aerpc_logs to avoid
%% a circular dependency once aerpc_logs starts consulting the index.
logs_for_tx(STx, MicroBlockHash) ->
    try
        Tx = aetx_sign:tx(STx),
        {Type, _} = aetx:specialize_type(Tx),
        case is_contract_tx(Type) of
            false -> [];
            true ->
                {CB, CTx} = aetx:specialize_callback(Tx),
                {ContractId, CallId} =
                    case Type of
                        contract_call_tx ->
                            {CB:ct_call_id(CTx), CB:call_id(CTx)};
                        contract_create_tx ->
                            {CB:contract_pubkey(CTx), CB:call_id(CTx)}
                    end,
                case aec_chain:get_contract_call(ContractId, CallId,
                                                 MicroBlockHash) of
                    {ok, Call}     -> aect_call:log(Call);
                    {error, _Reas} -> []
                end
        end
    catch _:_ -> []
    end.

is_contract_tx(contract_call_tx)   -> true;
is_contract_tx(contract_create_tx) -> true;
is_contract_tx(_)                  -> false.
