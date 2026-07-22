%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Shared ETS read cache for Merkle Patricia Tree backend nodes.
%%%
%%%    The cache is content-addressed (keys are `aec_hash' digests of the
%%%    stored value) and uses a two-segment generational LRU. New and
%%%    promoted entries land in generation 1; when generation 1 grows past
%%%    `max_size' entries, or the tracked payload exceeds `max_bytes',
%%%    generation 2 is dropped and generation 1 is demoted to 2.
%%%
%%%    Rotation is performed by this gen_server, never by the calling
%%%    process, so it stays off the synchronous read path, and it is
%%%    single-flight so concurrent triggers cannot interleave the two
%%%    `ets:select_*' passes.
%%%
%%%    The table is owned by this gen_server. Callers never create it:
%%%    every accessor degrades to a cache miss if the table is absent, so
%%%    a cache fault can never propagate into block execution.
%%%
%%%    Hot-path counters are `atomics' slots flushed to exometer on a
%%%    timer; the read path never touches `exometer_admin'.
%%%
%%%    Cache use is gated at the callback boundary in `aec_db_backends'.
%%%    In particular the cache is NOT consulted when garbage collection is
%%%    active for a tree: under GC a read that misses the primary table
%%%    must reach `aec_db:lookup_tree_node/2' so the node is promoted back
%%%    into the primary, and a cache hit above that boundary would skip the
%%%    promotion and lose the node at the next GC switch.
%%% @end
%%%=============================================================================
-module(aec_mpt_cache).

-behaviour(gen_server).

-export([ start_link/0
        , enabled/0
        , max_size/0
        , max_bytes/0
        , get/2
        , put/3
        , clear/0
        , size/0
        , memory/0
        , stats/0
        , gate_safe_access/1
        , reload_config/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SERVER, ?MODULE).
-define(TAB, aec_mpt_read_cache).
-define(PT_COUNTERS, {?MODULE, counters}).
-define(PT_ENABLED,  {?MODULE, enabled}).
-define(PT_MAX_SIZE, {?MODULE, max_size}).
-define(PT_MAX_BYTES, {?MODULE, max_bytes}).
-define(DEFAULT_ENABLED,   false).
-define(DEFAULT_MAX_SIZE,  100000).
-define(DEFAULT_MAX_BYTES, 268435456).

-define(IX_YOUNG,     1).
-define(IX_BYTES,     2).
-define(IX_HIT,       3).
-define(IX_MISS,      4).
-define(IX_EVICT,     5).
-define(IX_ROTATIONS, 6).
-define(IX_ROTATING,  7).
-define(IX_SAFE,      8).
-define(IX_ROTATE_US, 9).
-define(N_COUNTERS,   9).

-define(SAFE_OFF,      0).
-define(SAFE_ON,       1).
-define(SAFE_DRAINING, 2).

%% Binaries at or above this size are stored off-table as refcounted
%% payload and are not reflected in `ets:info(Tab, memory)'.
-define(REFC_LIMIT, 64).
-define(ENTRY_OVERHEAD, 120).
-define(FLUSH_INTERVAL, 10000).

-record(st, {tab, sent = #{}, timer}).

-type table() :: atom().
-type key()   :: binary().
-type value() :: term().

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec enabled() -> boolean().
enabled() ->
    case persistent_term:get(?PT_ENABLED, undefined) of
        Value when is_boolean(Value) -> Value;
        undefined -> memoise(?PT_ENABLED, <<"enabled">>, enabled, ?DEFAULT_ENABLED, boolean)
    end.

-spec max_size() -> pos_integer().
max_size() ->
    case persistent_term:get(?PT_MAX_SIZE, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        undefined -> memoise(?PT_MAX_SIZE, <<"max_size">>, max_size, ?DEFAULT_MAX_SIZE,
                             pos_integer)
    end.

-spec max_bytes() -> pos_integer().
max_bytes() ->
    case persistent_term:get(?PT_MAX_BYTES, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        undefined -> memoise(?PT_MAX_BYTES, <<"max_bytes">>, max_bytes, ?DEFAULT_MAX_BYTES,
                             pos_integer)
    end.

-spec get(table(), key()) -> {value, value()} | none.
get(Table, Key) ->
    CacheKey = {Table, Key},
    try ets:lookup(?TAB, CacheKey) of
        [{_, Val, 1}] ->
            count(?IX_HIT),
            {value, Val};
        [{_, Val, _}] ->
            _ = ets:update_element(?TAB, CacheKey, {3, 1}),
            note_young(),
            count(?IX_HIT),
            {value, Val};
        [] ->
            count(?IX_MISS),
            none
    catch
        _:_ -> none
    end.

-spec put(table(), key(), value()) -> ok.
put(Table, Key, Val) ->
    CacheKey = {Table, Key},
    try
        case ets:lookup(?TAB, CacheKey) of
            [{_, _, 1}] ->
                %% Content-addressed: same key implies same value and
                %% therefore no change in tracked payload size.
                true = ets:insert(?TAB, {CacheKey, Val, 1});
            [{_, _, _}] ->
                true = ets:insert(?TAB, {CacheKey, Val, 1}),
                note_young();
            [] ->
                true = ets:insert(?TAB, {CacheKey, Val, 1}),
                add(?IX_BYTES, entry_bytes(Val)),
                note_young()
        end,
        ok
    catch
        _:_ -> ok
    end.

-spec clear() -> ok.
clear() ->
    try gen_server:call(?SERVER, clear, 30000)
    catch exit:_ -> ok
    end.

-spec size() -> non_neg_integer().
size() ->
    table_info(size, 0).

%% Real resident bytes: the ETS `memory' datapoint is in words and
%% excludes refcounted binary payload, which is most of what we store.
-spec memory() -> non_neg_integer().
memory() ->
    table_info(memory, 0) * erlang:system_info(wordsize) + counter(?IX_BYTES).

-spec stats() -> map().
stats() ->
    #{ entries     => size()
     , memory      => memory()
     , payload     => counter(?IX_BYTES)
     , hits        => counter(?IX_HIT)
     , misses      => counter(?IX_MISS)
     , evictions   => counter(?IX_EVICT)
     , rotations   => counter(?IX_ROTATIONS)
     , rotate_us   => counter(?IX_ROTATE_US)
     , young       => counter(?IX_YOUNG)
     }.

%% Edge-triggered barrier around the `aec_db:db_safe_access/0' window.
%% Returns whether the cache may be used. Entries cached before the
%% window opened were never checksum-verified, so the cache is flushed
%% on both edges rather than resurfacing unverified values afterwards.
-spec gate_safe_access(boolean()) -> boolean().
gate_safe_access(SafeAccess) ->
    %% Called from the block-execution read path: degrade to "do not use
    %% the cache" rather than propagating any fault.
    try gate_safe_access_(SafeAccess) catch _:_ -> false end.

gate_safe_access_(true) ->
    %% Nobody serves the cache while the flag is set, so the entry flush
    %% can be asynchronous.
    case compare_exchange(?IX_SAFE, ?SAFE_OFF, ?SAFE_ON) of
        ok -> gen_server:cast(?SERVER, clear);
        _  -> ok
    end,
    false;
gate_safe_access_(false) ->
    %% Leaving the window goes through a distinct draining state, so no
    %% concurrent reader can be told the cache is usable in the gap
    %% between reopening it and the flush actually landing.
    case compare_exchange(?IX_SAFE, ?SAFE_ON, ?SAFE_DRAINING) of
        ok ->
            _ = clear(),
            set(?IX_SAFE, ?SAFE_OFF),
            false;
        ?SAFE_OFF ->
            true;
        _ ->
            false
    end.

%% Drop memoised config so the next accessor re-reads user config.
-spec reload_config() -> ok.
reload_config() ->
    _ = persistent_term:erase(?PT_ENABLED),
    _ = persistent_term:erase(?PT_MAX_SIZE),
    _ = persistent_term:erase(?PT_MAX_BYTES),
    ok.

%%%===================================================================
%%% gen_server
%%%===================================================================

%% This process is a `temporary' child of a supervisor running with
%% MaxRestarts = 0, so nothing here may raise: an escaping exception would
%% take `aecore_sup' — and with it the node — down over a cache fault.
%% Every callback body is therefore wrapped, and `init/1' tolerates an
%% existing table rather than failing on `badarg'.
init([]) ->
    process_flag(trap_exit, true),
    _ = catch counters_ref(),
    _ = catch reset_counters(),
    {ok, arm(#st{tab = new_table()})}.

new_table() ->
    try ets:new(?TAB, [ named_table
                      , public
                      , set
                      , {read_concurrency,  true}
                      , {write_concurrency, true}
                      ])
    catch
        error:badarg -> ets:whereis(?TAB)
    end.

handle_call(clear, _From, St) ->
    safe(fun do_clear/0),
    {reply, ok, St};
handle_call(flush, _From, St) ->
    {reply, ok, tick(St)};
handle_call(sync, _From, St) ->
    {reply, ok, St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(rotate, St) ->
    safe(fun do_rotate/0),
    {noreply, St};
handle_cast(clear, St) ->
    safe(fun do_clear/0),
    {noreply, St};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({flush, Ref}, #st{timer = Ref} = St) ->
    {noreply, arm(tick(St))};
handle_info(_Msg, St) ->
    {noreply, St}.

arm(St) ->
    Ref = make_ref(),
    _ = erlang:send_after(?FLUSH_INTERVAL, self(), {flush, Ref}),
    St#st{timer = Ref}.

tick(#st{sent = Sent} = St) ->
    Sent1 = try flush_metrics(Sent) catch _:_ -> Sent end,
    safe(fun maybe_enforce_budget/0),
    St#st{sent = Sent1}.

safe(F) ->
    try F() catch _:_ -> ok end.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal
%%%===================================================================

do_clear() ->
    _ = catch ets:delete_all_objects(?TAB),
    set(?IX_YOUNG, 0),
    set(?IX_BYTES, 0),
    set(?IX_ROTATING, 0),
    ok.

%% Rotation is only ever entered here, from the owning process, and only
%% one pass may be in flight: `?IX_ROTATING' is claimed by the requester
%% before the cast and released at the end of the pass. The young counter
%% is exchanged rather than reset, so increments arriving while the two
%% select passes run are preserved and the 2 x max_size bound holds.
do_rotate() ->
    T0 = erlang:monotonic_time(microsecond),
    try
        D = ets:select_delete(?TAB, [{{{'_', '_'}, '_', 2}, [], [true]}]),
        %% select_replace returns how many entries left the young
        %% generation, so the counter can be decremented by exactly
        %% that instead of reset. Increments that arrive while the
        %% two passes run are therefore preserved.
        Demoted = ets:select_replace(?TAB, [{{{'$1', '$2'}, '$3', 1},
                                             [],
                                             [{{{{'$1', '$2'}}, '$3', 2}}]}]),
        sub_floor(?IX_YOUNG, Demoted),
        scale_bytes(D),
        add(?IX_EVICT, D),
        add(?IX_ROTATIONS, 1),
        set(?IX_ROTATE_US, erlang:monotonic_time(microsecond) - T0)
    catch
        _:_ -> ok
    after
        %% Must always run: a retained claim disables rotation for good.
        catch set(?IX_ROTATING, 0)
    end,
    %% Writers can overshoot while a pass is in flight, and the claim
    %% suppresses their requests, so re-arm once if still over budget.
    case catch over_budget() of
        true -> request_rotate();
        _    -> ok
    end,
    ok.

%% select_delete does not hand back the evicted values, so the tracked
%% payload is scaled by the surviving fraction rather than recomputed.
%% An O(n) fold here would both slow the pass and widen the window in
%% which writers can overshoot the bound.
scale_bytes(0) ->
    ok;
scale_bytes(Deleted) ->
    Kept = ?MODULE:size(),
    case Kept + Deleted of
        0 ->
            set(?IX_BYTES, 0);
        Total ->
            set(?IX_BYTES, (counter(?IX_BYTES) * Kept) div Total)
    end.

sub_floor(Ix, N) ->
    case atomics:sub_get(counters_ref(), Ix, N) of
        V when V < 0 -> set(Ix, 0);
        _            -> ok
    end.

%% Rotation is asynchronous, so a burst of concurrent writers can push the
%% young generation past `max_size' while a pass is in flight. The periodic
%% tick is what makes the bound hard: it rotates until the cache is back
%% inside both the entry and the byte budget.
%% One pass per tick: a pass only drops the older generation, so looping
%% here until the bound is met would empty the cache outright after a
%% burst instead of converging on it.
maybe_enforce_budget() ->
    case over_budget() of
        true  -> do_rotate();
        false -> ok
    end.

over_budget() ->
    ?MODULE:size() > 2 * max_size() orelse memory() > max_bytes().

note_young() ->
    Young = add_get(?IX_YOUNG, 1),
    case Young >= max_size() of
        true  -> request_rotate();
        false -> ok
    end.

request_rotate() ->
    case compare_exchange(?IX_ROTATING, 0, 1) of
        ok ->
            _ = catch gen_server:cast(?SERVER, rotate),
            ok;
        _ ->
            ok
    end.

entry_bytes(Val) ->
    ?ENTRY_OVERHEAD + refc_bytes(Val).

refc_bytes(B) when is_binary(B) ->
    case byte_size(B) of
        Sz when Sz >= ?REFC_LIMIT -> Sz;
        _ -> 0
    end;
refc_bytes([H | T]) ->
    refc_bytes(H) + refc_bytes(T);
refc_bytes([]) ->
    0;
refc_bytes(T) when is_tuple(T) ->
    refc_bytes(tuple_to_list(T));
refc_bytes(_) ->
    0.

table_info(Key, Default) ->
    case ets:whereis(?TAB) of
        undefined ->
            Default;
        _Tid ->
            case ets:info(?TAB, Key) of
                undefined -> Default;
                Value     -> Value
            end
    end.

%% Validation lives here, not only on the memoised read: the application
%% env fallback bypasses JSON-schema validation entirely, so an operator
%% typo would otherwise reach arithmetic in the server loop.
memoise(PTKey, ConfigKey, EnvKey, Default, Type) ->
    Raw = config_value(ConfigKey, EnvKey, Default),
    Value = case valid(Type, Raw) of
                true  -> Raw;
                false -> Default
            end,
    persistent_term:put(PTKey, Value),
    Value.

valid(boolean, V)     -> is_boolean(V);
valid(pos_integer, V) -> is_integer(V) andalso V > 0.

%% Falls back to the application env if the user-config subsystem is not
%% up yet, so an early reader cannot crash on a cache setting.
config_value(ConfigKey, EnvKey, Default) ->
    try aeu_env:find_config([<<"chain">>, <<"mpt_read_cache">>, ConfigKey],
                            [ user_config
                            , {env, aecore, [mpt_read_cache, EnvKey]}
                            , schema_default
                            , {value, Default}
                            ]) of
        {ok, Value} -> Value;
        undefined   -> app_env(EnvKey, Default)
    catch
        _:_ -> app_env(EnvKey, Default)
    end.

app_env(EnvKey, Default) ->
    case application:get_env(aecore, mpt_read_cache) of
        {ok, L} when is_list(L) -> proplists:get_value(EnvKey, L, Default);
        _                       -> Default
    end.

%%%===================================================================
%%% Counters
%%%===================================================================

counters_ref() ->
    case persistent_term:get(?PT_COUNTERS, undefined) of
        undefined ->
            Ref = atomics:new(?N_COUNTERS, []),
            persistent_term:put(?PT_COUNTERS, Ref),
            persistent_term:get(?PT_COUNTERS);
        Ref ->
            Ref
    end.

reset_counters() ->
    Ref = counters_ref(),
    [atomics:put(Ref, I, 0) || I <- lists:seq(1, ?N_COUNTERS)],
    ok.

count(Ix) -> add(Ix, 1).

add(_Ix, 0) -> ok;
add(Ix, N)  -> atomics:add(counters_ref(), Ix, N).

add_get(Ix, N) -> atomics:add_get(counters_ref(), Ix, N).

set(Ix, N) -> atomics:put(counters_ref(), Ix, N).

counter(Ix) -> atomics:get(counters_ref(), Ix).

compare_exchange(Ix, Expected, Desired) ->
    atomics:compare_exchange(counters_ref(), Ix, Expected, Desired).

%% Counters stay cumulative so `stats/0' is meaningful; only the delta
%% since the previous tick is pushed to the exometer counters.
flush_metrics(Sent) ->
    lists:foldl(fun flush_metric/2, Sent,
                [ {hit, ?IX_HIT}, {miss, ?IX_MISS}
                , {evict, ?IX_EVICT}, {rotate, ?IX_ROTATIONS} ]).

flush_metric({Name, Ix}, Sent) ->
    Total = counter(Ix),
    case Total - maps:get(Name, Sent, 0) of
        Delta when Delta > 0 ->
            %% Reporting must never take the cache down with it.
            _ = catch aec_metrics:try_update_or_create(
                        [ae, epoch, aecore, mpt, read_cache, Name], Delta),
            Sent#{Name => Total};
        _ ->
            Sent#{Name => Total}
    end.
