%%%-------------------------------------------------------------------
%%% @doc Server-side filter registry for the poll-based half of the eth
%%% log API: `eth_newFilter', `eth_newBlockFilter',
%%% `eth_newPendingTransactionFilter', `eth_getFilterChanges',
%%% `eth_getFilterLogs' and `eth_uninstallFilter'.
%%%
%%% == Cursors ==
%%%
%%% `getFilterChanges' returns what is new since the last poll, so each
%%% filter carries a cursor. Following geth: a log or block filter's
%%% cursor starts at the height the filter was CREATED, so the first
%%% poll returns what has happened since creation rather than replaying
%%% the filter's `fromBlock'. `getFilterLogs' is the one that honours
%%% the original range.
%%%
%%% Log and block cursors are heights, derived from the chain on each
%%% poll rather than accumulated from events. That means a missed
%%% `top_changed' cannot leave a filter permanently behind, and it
%%% removes an unbounded per-filter buffer. Pending-transaction filters
%%% cannot work that way -- a mempool arrival is not derivable from a
%%% height -- so those do buffer, and that buffer is explicitly capped.
%%%
%%% == Bounds, all of them deliberate ==
%%%
%%%   * `http > rpc > max_filters'   -- refuse to allocate beyond this.
%%%   * `http > rpc > filter_ttl_seconds' -- a filter nobody polls is
%%%     dropped. Clients disappear without calling uninstall, and
%%%     without a TTL every one of them leaks a registry entry.
%%%   * `?MAX_PENDING' per pending-tx filter -- oldest dropped first. A
%%%     client that stops polling during a mempool burst must not be
%%%     able to grow this process's heap without limit.
%%%
%%% == One known v1 property ==
%%%
%%% Polls are served inside this gen_server, so a slow log scan delays
%%% other polls. The scan is bounded by `aerpc_logs`' own range cap and
%%% a typical poll covers one generation, so this is stated rather than
%%% engineered around; moving the scan out of the server needs the
%%% cursor advance to stay atomic and is not worth the complexity yet.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_filter_registry).

-behaviour(gen_server).

-export([start_link/0,
         new_log_filter/1,
         new_block_filter/0,
         new_pending_tx_filter/0,
         changes/1,
         logs/1,
         uninstall/1,
         status/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(DEFAULT_MAX_FILTERS, 128).
-define(DEFAULT_TTL_SECONDS, 300).
%% Mempool hashes buffered per pending-tx filter before the oldest go.
-define(MAX_PENDING, 1000).
%% How often expired filters are swept.
-define(SWEEP_MS, 60000).

-record(filter, {
    id            :: binary(),
    kind          :: logs | block | pending_tx,
    criteria      :: undefined | map(),   %% the client's original filter
    from_height   :: undefined | non_neg_integer(),
    to_height     :: undefined | non_neg_integer() | latest,
    cursor        :: non_neg_integer(),   %% highest height already returned
    pending = []  :: [binary()],          %% newest-first, pending_tx only
    dropped = 0   :: non_neg_integer(),   %% pending entries lost to ?MAX_PENDING
    last_used     :: integer()            %% erlang:monotonic_time(second)
}).

-record(state, {next_id = 1 :: pos_integer(),
                filters = #{} :: #{binary() => #filter{}}}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Allocate a log filter from an `eth_newFilter' criteria object.
-spec new_log_filter(map()) -> {ok, binary()} | {error, integer(), binary()}.
new_log_filter(Criteria) when is_map(Criteria) ->
    call({new, logs, Criteria});
new_log_filter(_) ->
    {error, -32602, <<"Invalid params">>}.

-spec new_block_filter() -> {ok, binary()} | {error, integer(), binary()}.
new_block_filter() ->
    call({new, block, undefined}).

-spec new_pending_tx_filter() -> {ok, binary()} | {error, integer(), binary()}.
new_pending_tx_filter() ->
    call({new, pending_tx, undefined}).

%% @doc Everything since the previous call for this filter.
-spec changes(binary()) -> {ok, [term()]} | {error, integer(), binary()}.
changes(Id) when is_binary(Id) ->
    call({changes, Id});
changes(_) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Every log matching a log filter's ORIGINAL range, cursor
%% untouched. Undefined for block and pending-transaction filters, which
%% is also how geth answers.
-spec logs(binary()) -> {ok, [map()]} | {error, integer(), binary()}.
logs(Id) when is_binary(Id) ->
    call({logs, Id});
logs(_) ->
    {error, -32602, <<"Invalid params">>}.

-spec uninstall(binary()) -> {ok, boolean()}.
uninstall(Id) when is_binary(Id) ->
    case call({uninstall, Id}) of
        {ok, Bool}      -> {ok, Bool};
        {error, _, _}   -> {ok, false}    %% eth: idempotent false
    end;
uninstall(_) ->
    {ok, false}.

-spec status() -> map().
status() ->
    case call(status) of
        {ok, Map}     -> Map;
        {error, _, _} -> #{filters => 0, running => false}
    end.

%% The registry is only started when the endpoint is enabled, so a call
%% with it down is a configuration answer rather than a crash.
call(Msg) ->
    try gen_server:call(?MODULE, Msg, 30000)
    catch exit:_ -> aerpc_errors:filter_registry_unavailable()
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% Only pending-transaction filters need these, and the handler drops
    %% out immediately when there are none -- but subscribing once here
    %% is simpler than managing subscription lifecycle per filter.
    %%
    %% BOTH events. `aec_tx_pool:push/1' defaults to `tx_created', which
    %% is what the node's own POST /v3/transactions uses and therefore
    %% what every SDK, wallet and dapp produces; `tx_received' covers
    %% only gossip from a peer and fork re-adds. Subscribing to
    %% `tx_received' alone meant a locally submitted transaction never
    %% reached a pending filter at all, and on a single-node deployment
    %% nothing reached it ever.
    [try aec_events:subscribe(E) catch _:_ -> ok end
     || E <- [tx_created, tx_received]],
    erlang:send_after(?SWEEP_MS, self(), sweep),
    {ok, #state{}}.

handle_call({new, Kind, Criteria}, _From, State) ->
    case map_size(State#state.filters) >= max_filters() of
        true ->
            {reply, aerpc_errors:too_many_filters(max_filters()), State};
        false ->
            case build_filter(Kind, Criteria, State) of
                {ok, Filter, State1} ->
                    Filters = maps:put(Filter#filter.id, Filter,
                                       State1#state.filters),
                    {reply, {ok, Filter#filter.id},
                     State1#state{filters = Filters}};
                {error, _, _} = Err ->
                    {reply, Err, State}
            end
    end;

handle_call({changes, Id}, _From, State) ->
    with_filter(Id, State, fun do_changes/2);

handle_call({logs, Id}, _From, State) ->
    with_filter(Id, State,
                fun(#filter{kind = logs} = F, _S) ->
                        {aerpc_logs:get_logs(original_criteria(F)), F};
                   (F, _S) ->
                        %% geth: getFilterLogs is log-filters only.
                        {aerpc_errors:filter_not_found(), F}
                end);

handle_call({uninstall, Id}, _From, State) ->
    case maps:is_key(Id, State#state.filters) of
        true  -> {reply, {ok, true},
                  State#state{filters = maps:remove(Id, State#state.filters)}};
        false -> {reply, {ok, false}, State}
    end;

handle_call(status, _From, State) ->
    Fs = maps:values(State#state.filters),
    {reply, {ok, #{running         => true,
                   filters         => map_size(State#state.filters),
                   max_filters     => max_filters(),
                   ttl_seconds     => ttl_seconds(),
                   %% Non-zero means a pending-tx client stopped polling
                   %% through a mempool burst and lost the oldest hashes.
                   %% Visible rather than silent, since the client cannot
                   %% tell from the poll result alone.
                   pending_dropped => lists:sum([F#filter.dropped || F <- Fs]),
                   by_kind         => count_kinds(
                                        [F#filter.kind || F <- Fs])}}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, -32603, <<"Internal error">>}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{info := SignedTx}}, State)
  when Event =:= tx_created; Event =:= tx_received ->
    {noreply, buffer_pending(SignedTx, State)};
handle_info({gproc_ps_event, Event, _Other}, State)
  when Event =:= tx_created; Event =:= tx_received ->
    {noreply, State};
handle_info(sweep, State) ->
    erlang:send_after(?SWEEP_MS, self(), sweep),
    {noreply, sweep_expired(State)};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Filter construction
%% ===================================================================

build_filter(logs, Criteria, State) ->
    case maps:is_key(<<"blockHash">>, Criteria) of
        true ->
            %% eth_newFilter is a range filter; a single-block query is
            %% eth_getLogs' job and has no cursor to advance.
            {error, -32602,
             <<"blockHash is not valid for eth_newFilter; use eth_getLogs">>};
        false ->
            case resolve_bounds(Criteria) of
                {ok, FromH, ToH} ->
                    case top_height() of
                        {ok, Top} ->
                            {Id, State1} = allocate(State),
                            {ok, #filter{id          = Id,
                                         kind        = logs,
                                         criteria    = Criteria,
                                         from_height = FromH,
                                         to_height   = ToH,
                                         cursor      = Top,
                                         last_used   = now_seconds()},
                             State1};
                        error ->
                            {error, -32603, <<"Chain not initialized">>}
                    end;
                {error, _, _} = Err ->
                    Err
            end
    end;
build_filter(Kind, _Criteria, State) when Kind =:= block; Kind =:= pending_tx ->
    case top_height() of
        {ok, Top} ->
            {Id, State1} = allocate(State),
            {ok, #filter{id = Id, kind = Kind, cursor = Top,
                         last_used = now_seconds()}, State1};
        error ->
            {error, -32603, <<"Chain not initialized">>}
    end.

%% Validate the range up front so a malformed filter fails at creation
%% rather than on the first poll.
resolve_bounds(Criteria) ->
    From = maps:get(<<"fromBlock">>, Criteria, <<"latest">>),
    To   = maps:get(<<"toBlock">>,   Criteria, <<"latest">>),
    case {aerpc_block:resolve_tag(From), aerpc_block:resolve_tag(To)} of
        {{ok, FromH}, {ok, _ToH}} ->
            %% A `latest' upper bound must stay open, so record the tag
            %% rather than the height it happened to resolve to.
            {ok, FromH, to_bound(To)};
        _Other ->
            {error, -32602, <<"Invalid params">>}
    end.

to_bound(<<"0x", _/binary>> = Hex) ->
    case aerpc_block:resolve_tag(Hex) of
        {ok, H}        -> H;
        {error, _, _}  -> latest
    end;
to_bound(_Tag) ->
    latest.

allocate(#state{next_id = N} = State) ->
    {aerpc_encoding:to_quantity(N), State#state{next_id = N + 1}}.

%% ===================================================================
%% Polling
%% ===================================================================

with_filter(Id, State, Fun) ->
    case maps:get(Id, State#state.filters, undefined) of
        undefined ->
            {reply, aerpc_errors:filter_not_found(), State};
        Filter ->
            {Reply, Filter1} = Fun(Filter, State),
            Touched = Filter1#filter{last_used = now_seconds()},
            {reply, Reply,
             State#state{filters = maps:put(Id, Touched, State#state.filters)}}
    end.

do_changes(#filter{kind = pending_tx, pending = Pending} = F, _State) ->
    %% Buffered newest-first; eth clients expect arrival order.
    Hashes = [aerpc_encoding:format_tx_hash(H) || H <- lists:reverse(Pending)],
    {{ok, Hashes}, F#filter{pending = [], dropped = 0}};
do_changes(#filter{kind = block, cursor = Cursor} = F, _State) ->
    case top_height() of
        {ok, Top} when Top > Cursor ->
            Hashes = [key_block_hash(H) || H <- lists:seq(Cursor + 1, Top)],
            {{ok, [H || H <- Hashes, H =/= undefined]},
             F#filter{cursor = Top}};
        {ok, _Top} ->
            {{ok, []}, F};
        error ->
            {{error, -32603, <<"Chain not initialized">>}, F}
    end;
do_changes(#filter{kind = logs, cursor = Cursor} = F, _State) ->
    case window(F, Cursor) of
        {ok, From, To} ->
            Query = (original_criteria(F))#{
                        <<"fromBlock">> => aerpc_encoding:to_quantity(From),
                        <<"toBlock">>   => aerpc_encoding:to_quantity(To)},
            case aerpc_logs:get_logs(Query) of
                {ok, Logs}          -> {{ok, Logs}, F#filter{cursor = To}};
                {error, _, _} = Err -> {Err, F}
            end;
        nothing_new ->
            {{ok, []}, F};
        error ->
            {{error, -32603, <<"Chain not initialized">>}, F}
    end.

%% The next unread slice, clamped to the filter's own upper bound so a
%% filter created with an explicit `toBlock' stops there instead of
%% following the chain forever.
window(#filter{to_height = Bound}, Cursor) ->
    case top_height() of
        {ok, Top} ->
            Ceiling = case Bound of
                          latest -> Top;
                          H      -> min(H, Top)
                      end,
            case Ceiling > Cursor of
                true  -> {ok, Cursor + 1, Ceiling};
                false -> nothing_new
            end;
        error ->
            error
    end.

%% getFilterLogs replays the client's original range, so hand back the
%% criteria exactly as it arrived.
original_criteria(#filter{criteria = Criteria}) when is_map(Criteria) ->
    Criteria;
original_criteria(_) ->
    #{}.

key_block_hash(Height) ->
    case aec_chain:get_key_block_by_height(Height) of
        {ok, KB} ->
            case aec_blocks:hash_internal_representation(KB) of
                {ok, Hash} -> aerpc_encoding:format_key_block_hash(Hash);
                _Other     -> undefined
            end;
        {error, _Reason} ->
            undefined
    end.

%% ===================================================================
%% Pending transactions
%% ===================================================================

buffer_pending(SignedTx, #state{filters = Filters} = State) ->
    case has_pending_filter(Filters) of
        false ->
            %% Nothing is watching the mempool: do no work at all rather
            %% than hash every arriving transaction.
            State;
        true ->
            try aetx_sign:hash(SignedTx) of
                Hash -> State#state{filters = push_hash(Hash, Filters)}
            catch _:_ -> State
            end
    end.

has_pending_filter(Filters) ->
    lists:any(fun(#filter{kind = pending_tx}) -> true;
                 (_Other)                     -> false
              end, maps:values(Filters)).

push_hash(Hash, Filters) ->
    maps:map(
      fun(_Id, #filter{kind = pending_tx, pending = P, dropped = D} = F) ->
              case length(P) >= ?MAX_PENDING of
                  true ->
                      %% Drop the oldest rather than the newest: a client
                      %% resuming a poll wants recent mempool state.
                      Trimmed = lists:droplast(P),
                      F#filter{pending = [Hash | Trimmed], dropped = D + 1};
                  false ->
                      F#filter{pending = [Hash | P]}
              end;
         (_Id, F) ->
              F
      end, Filters).

%% ===================================================================
%% Expiry
%% ===================================================================

sweep_expired(#state{filters = Filters} = State) ->
    Cutoff = now_seconds() - ttl_seconds(),
    Kept = maps:filter(fun(_Id, #filter{last_used = U}) -> U >= Cutoff end,
                       Filters),
    Expired = map_size(Filters) - map_size(Kept),
    Expired > 0 andalso
        lager:debug("aerpc filter registry expired ~p idle filter(s)",
                    [Expired]),
    State#state{filters = Kept}.

%% ===================================================================
%% Helpers
%% ===================================================================

top_height() ->
    try aec_chain:top_header() of
        undefined -> error;
        Header    -> {ok, aec_headers:height(Header)}
    catch _:_ -> error
    end.

now_seconds() ->
    erlang:monotonic_time(second).

max_filters() ->
    case application:get_env(aerpc, max_filters, ?DEFAULT_MAX_FILTERS) of
        N when is_integer(N), N > 0 -> N;
        _Other                      -> ?DEFAULT_MAX_FILTERS
    end.

ttl_seconds() ->
    case application:get_env(aerpc, filter_ttl_seconds, ?DEFAULT_TTL_SECONDS) of
        N when is_integer(N), N > 0 -> N;
        _Other                      -> ?DEFAULT_TTL_SECONDS
    end.

count_kinds(Kinds) ->
    lists:foldl(fun(K, Acc) -> maps:update_with(K, fun(N) -> N + 1 end, 1, Acc)
                end, #{}, Kinds).
