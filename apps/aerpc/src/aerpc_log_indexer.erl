%%%-------------------------------------------------------------------
%%% @doc Real-time log indexer for `ae_getLogs'.
%%%
%%% Subscribes to `aec_events:top_changed' and pushes the new
%%% generation's logs into [`aerpc_log_store'](aerpc_log_store.erl).
%%% No backfill: at boot the floor is set to `top+1' (so heights at-
%%% or-below-startup-top fall through to the inline walker forever).
%%% This keeps boot cheap; a backfill mode can be added later if/when
%%% archive-style indexing is needed.
%%%
%%% Aerpc applications depend on aecore so by the time this gen_server
%%% starts, `aec_chain:top_header/0' is callable.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_log_indexer).

-behaviour(gen_server).

-export([start_link/0,
         status/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    started_at :: non_neg_integer()
}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return `#{floor, watermark, indexed_count}'. Useful for health
%% checks and for tests that need to verify the indexer is current.
-spec status() -> map().
status() ->
    #{floor      => aerpc_log_store:floor_height(),
      watermark  => aerpc_log_store:watermark(),
      entries    => ets_size()}.

ets_size() ->
    case ets:info(aerpc_log_idx) of
        undefined -> 0;
        Info -> proplists:get_value(size, Info, 0)
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    aerpc_log_store:init(),
    Started = current_top_height(),
    %% Heights up to and including `Started' are NOT indexed by this
    %% process. Floor is `Started + 1' so a query for height Started
    %% falls through to the inline walker.
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

handle_info({gproc_ps_event, top_changed,
             #{info := #{block_hash := KBHash}}}, State) ->
    index_block(KBHash),
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

index_block(KBHash) ->
    case aec_chain:get_generation_by_hash(KBHash, forward) of
        {ok, #{key_block := KB, micro_blocks := MBs}} ->
            Header = aec_blocks:to_key_header(KB),
            Height = aec_headers:height(Header),
            Entries = collect_entries(MBs, KBHash, Height),
            aerpc_log_store:insert_many(Entries),
            aerpc_log_store:set_watermark(Height);
        error ->
            ok
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
    case logs_for_tx(STx, KBH) of
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
logs_for_tx(STx, BlockHash) ->
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
                case aec_chain:get_contract_call(ContractId, CallId, BlockHash) of
                    {ok, Call}     -> aect_call:log(Call);
                    {error, _Reas} -> []
                end
        end
    catch _:_ -> []
    end.

is_contract_tx(contract_call_tx)   -> true;
is_contract_tx(contract_create_tx) -> true;
is_contract_tx(_)                  -> false.
