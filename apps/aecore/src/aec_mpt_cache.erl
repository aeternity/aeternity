%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Shared ETS read cache for Merkle Patricia Tree backend nodes.
%%%
%%%    The cache is content-addressed (keys are `aec_hash' digests of the
%%%    stored value) and uses a two-segment generational LRU so eviction
%%%    is O(1) and stays off the read hot-path. New / promoted entries
%%%    land in generation 1; when the generation-1 size reaches
%%%    `max_size', generation 2 is dropped and generation 1 is demoted
%%%    to 2 in two `ets:select_*' calls.
%%%
%%%    `enabled/0' and `max_size/0' are memoised in `persistent_term'
%%%    (same pattern as `aec_db:db_safe_access/0'), so the per-block
%%%    lookups in `aec_db_backends:db_spec/1' stay free.
%%% @end
%%%=============================================================================
-module(aec_mpt_cache).

-export([ start/0
        , enabled/0
        , max_size/0
        , get/2
        , put/3
        , clear/0
        , size/0
        , memory/0
        ]).

-define(TAB, aec_mpt_read_cache).
-define(PT_YOUNG_COUNTER, {?MODULE, young_counter}).
-define(PT_ENABLED,       {?MODULE, enabled}).
-define(PT_MAX_SIZE,      {?MODULE, max_size}).
-define(DEFAULT_ENABLED,  true).
-define(DEFAULT_MAX_SIZE, 100000).

-type table() :: atom().
-type key()   :: binary().
-type value() :: term().

-spec start() -> ok.
start() ->
    %% Drop any previously memoised config so the next `enabled/0' /
    %% `max_size/0' call re-reads `aeternity.yaml' (and the application
    %% env for tests). In production this is a no-op because `start/0'
    %% is invoked once from `aecore_sup:init/1' before any reader.
    _ = persistent_term:erase(?PT_ENABLED),
    _ = persistent_term:erase(?PT_MAX_SIZE),
    _ = ensure_table(),
    _ = ensure_young_counter(),
    ok.

-spec enabled() -> boolean().
enabled() ->
    case persistent_term:get(?PT_ENABLED, undefined) of
        Value when is_boolean(Value) ->
            Value;
        undefined ->
            Value = config_value(<<"enabled">>, enabled, ?DEFAULT_ENABLED),
            persistent_term:put(?PT_ENABLED, Value),
            Value
    end.

-spec max_size() -> pos_integer().
max_size() ->
    case persistent_term:get(?PT_MAX_SIZE, undefined) of
        Value when is_integer(Value), Value > 0 ->
            Value;
        undefined ->
            Value = config_value(<<"max_size">>, max_size, ?DEFAULT_MAX_SIZE),
            persistent_term:put(?PT_MAX_SIZE, Value),
            Value
    end.

-spec get(table(), key()) -> {value, value()} | none.
get(Table, Key) ->
    CacheKey = {Table, Key},
    case lookup(CacheKey) of
        [{CacheKey, Val, 1}] ->
            bump(hit),
            {value, Val};
        [{CacheKey, Val, 2}] ->
            _ = ets:update_element(?TAB, CacheKey, {3, 1}),
            note_young_entry(),
            bump(hit),
            {value, Val};
        [] ->
            bump(miss),
            none
    end.

-spec put(table(), key(), value()) -> ok.
put(Table, Key, Val) ->
    CacheKey = {Table, Key},
    case lookup(CacheKey) of
        [{CacheKey, _OldVal, 1}] ->
            true = ets:insert(?TAB, {CacheKey, Val, 1});
        [{CacheKey, _OldVal, 2}] ->
            true = ets:insert(?TAB, {CacheKey, Val, 1}),
            note_young_entry();
        [] ->
            true = ets:insert(?TAB, {CacheKey, Val, 1}),
            note_young_entry()
    end,
    ok.

-spec clear() -> ok.
clear() ->
    ok = start(),
    true = ets:delete_all_objects(?TAB),
    atomics:put(young_counter(), 1, 0),
    ok.

-spec size() -> non_neg_integer().
size() ->
    table_info(size, 0).

-spec memory() -> non_neg_integer().
memory() ->
    table_info(memory, 0).

%%%===================================================================
%%% Internal
%%%===================================================================

%% `lookup/1' is the ETS-touching primitive used by both `get/2' and
%% `put/3'. If the table happens to be missing (e.g. first call before
%% `aecore_sup:init/1' has run), recreate it once and return an empty
%% result. Any subsequent `badarg' is a real bug and is allowed to
%% propagate so we never enter an infinite recovery loop.
lookup(CacheKey) ->
    try ets:lookup(?TAB, CacheKey)
    catch
        error:badarg ->
            ok = ensure_table(),
            _ = ensure_young_counter(),
            ets:lookup(?TAB, CacheKey)
    end.

ensure_table() ->
    case ets:whereis(?TAB) of
        undefined ->
            try ets:new(?TAB, [ named_table
                              , public
                              , set
                              , {read_concurrency,  true}
                              , {write_concurrency, true}
                              ]) of
                _ -> ok
            catch
                %% Another process won the race.
                error:badarg -> ok
            end;
        _Tid ->
            ok
    end.

ensure_young_counter() ->
    case persistent_term:get(?PT_YOUNG_COUNTER, undefined) of
        undefined ->
            Counter = atomics:new(1, []),
            persistent_term:put(?PT_YOUNG_COUNTER, Counter),
            %% Re-read so a concurrent caller's counter (if any) wins
            %% over our local reference and everyone agrees on the same
            %% atomics ref.
            persistent_term:get(?PT_YOUNG_COUNTER);
        Counter ->
            Counter
    end.

young_counter() ->
    ensure_young_counter().

note_young_entry() ->
    Young = atomics:add_get(young_counter(), 1, 1),
    maybe_rotate(Young).

maybe_rotate(Young) ->
    case Young >= max_size() of
        true  -> rotate();
        false -> ok
    end.

rotate() ->
    Deleted = ets:select_delete(?TAB, [{{{'_', '_'}, '_', 2}, [], [true]}]),
    %% Build the replacement record in the match-spec body: outer 3-tuple
    %% needs `{{ ... }}' to be constructed, and the embedded composite
    %% key `{'$1','$2'}' needs its own `{{ ... }}' for the same reason.
    _ = ets:select_replace(?TAB, [{{{'$1', '$2'}, '$3', 1},
                                   [],
                                   [{{{{'$1', '$2'}}, '$3', 2}}]}]),
    atomics:put(young_counter(), 1, 0),
    bump(evict, Deleted),
    ok.

config_value(ConfigKey, EnvKey, Default) ->
    case aeu_env:find_config([<<"chain">>, <<"mpt_read_cache">>, ConfigKey],
                             [ user_config
                             , {env, aecore, [mpt_read_cache, EnvKey]}
                             , schema_default
                             , {value, Default}
                             ]) of
        {ok, Value} -> Value;
        undefined   -> Default
    end.

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

bump(Name) ->
    bump(Name, 1).

bump(_Name, 0) ->
    ok;
bump(Name, Value) ->
    _ = aec_metrics:try_update_or_create(metric(Name), Value),
    ok.

metric(Name) ->
    [ae, epoch, aecore, mpt, read_cache, Name].
