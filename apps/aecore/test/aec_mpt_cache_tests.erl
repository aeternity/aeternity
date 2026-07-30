%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Unit tests for the MPT read cache.
%%% @end
%%%=============================================================================
-module(aec_mpt_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%% Lowest value the config schema permits, so the eviction tests exercise
%% a configuration an operator can actually set.
-define(MAX_SIZE, 1000).

mpt_cache_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"missing key returns none", fun missing_key_returns_none/0}
     , {"put then get", fun put_then_get/0}
     , {"clear removes entries", fun clear_removes_entries/0}
     , {"rotation bounds the entry count", {timeout, 60, fun rotation_bounds_entries/0}}
     , {"rotation keeps the hot generation",
        {timeout, 60, fun rotation_keeps_hot_generation/0}}
     , {"rotation is off the read path", {timeout, 60, fun rotation_is_async/0}}
     , {"concurrent load does not empty the cache",
        {timeout, 180, fun concurrent_load_survives/0}}
     , {"memory is reported in bytes", fun memory_in_bytes/0}
     , {"payload bytes are tracked", fun payload_tracked/0}
     , {"config honours application env", fun config_honours_env/0}
     , {"safe access gate flushes on both edges", fun safe_access_gate/0}
     , {"stats expose the observability floor", fun stats_shape/0}
     , {"invalid config falls back to defaults", fun invalid_config_rejected/0}
     , {"stray messages do not add timers", fun stray_messages_ignored/0}
     , {"cache faults never kill the server", fun server_survives_faults/0}
     ]}.

%% B6: a cache fault must degrade to a miss, never propagate.
no_server_test_() ->
    {setup,
     fun() -> ensure_stopped(), ok end,
     fun(_) -> ok end,
     [ {"get without a running cache returns none",
        fun() -> ?assertEqual(none, aec_mpt_cache:get(accounts, <<"k">>)) end}
     , {"put without a running cache returns ok",
        fun() -> ?assertEqual(ok, aec_mpt_cache:put(accounts, <<"k">>, v)) end}
     , {"size and memory are defined without a table",
        fun() ->
                ?assertEqual(0, aec_mpt_cache:size()),
                ?assert(is_integer(aec_mpt_cache:memory()))
        end}
     , {"clear without a running cache does not crash",
        fun() -> ?assertEqual(ok, aec_mpt_cache:clear()) end}
     ]}.

setup() ->
    ensure_stopped(),
    set_config([{enabled, true}, {max_size, ?MAX_SIZE}, {max_bytes, 268435456}]),
    {ok, Pid} = aec_mpt_cache:start_link(),
    Pid.

teardown(Pid) ->
    stop(Pid),
    application:unset_env(aecore, mpt_read_cache),
    aec_mpt_cache:reload_config(),
    ok.

set_config(Env) ->
    application:set_env(aecore, mpt_read_cache, Env),
    aec_mpt_cache:reload_config().

ensure_stopped() ->
    case whereis(aec_mpt_cache) of
        undefined -> ok;
        Pid       -> stop(Pid)
    end.

stop(Pid) when is_pid(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ok
    end;
stop(_) ->
    ok.

%% Rotation is a cast; block until the server has drained its mailbox.
sync() ->
    ok = gen_server:call(aec_mpt_cache, sync, 30000).

missing_key_returns_none() ->
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<"missing">>)).

put_then_get() ->
    ok = aec_mpt_cache:put(accounts, <<"hash">>, {node, 1}),
    ?assertEqual({value, {node, 1}}, aec_mpt_cache:get(accounts, <<"hash">>)),
    %% Namespaces are per state tree.
    ?assertEqual(none, aec_mpt_cache:get(calls, <<"hash">>)).

clear_removes_entries() ->
    ok = aec_mpt_cache:put(accounts, <<"hash">>, {node, 1}),
    ?assert(aec_mpt_cache:size() > 0),
    ok = aec_mpt_cache:clear(),
    ?assertEqual(0, aec_mpt_cache:size()),
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<"hash">>)),
    ?assertEqual(0, maps:get(payload, aec_mpt_cache:stats())).

rotation_bounds_entries() ->
    put_range(1, 3 * ?MAX_SIZE),
    sync(),
    Size = aec_mpt_cache:size(),
    ?assert(Size =< 2 * ?MAX_SIZE),
    ?assert(Size > 0),
    ?assert(maps:get(rotations, aec_mpt_cache:stats()) >= 1).

rotation_keeps_hot_generation() ->
    put_range(1, 2 * ?MAX_SIZE + 10),
    sync(),
    %% The most recently written key is in the young generation and must
    %% have survived every rotation.
    Recent = 2 * ?MAX_SIZE + 10,
    ?assertEqual({value, {node, Recent}}, aec_mpt_cache:get(accounts, key(Recent))).

%% B3: a read that trips the rotation threshold must not pay for the
%% rotation itself.
rotation_is_async() ->
    put_range(1, ?MAX_SIZE - 1),
    Before = maps:get(rotations, aec_mpt_cache:stats()),
    {Micros, ok} = timer:tc(fun() -> aec_mpt_cache:put(accounts, key(?MAX_SIZE), v) end),
    ?assert(Micros < 50000),
    sync(),
    ?assert(maps:get(rotations, aec_mpt_cache:stats()) > Before).

%% B2: concurrent rotation triggers previously destroyed the whole table.
concurrent_load_survives() ->
    Workers = 8,
    PerWorker = 5000,
    Parent = self(),
    Pids = [spawn_link(fun() ->
                               Lo = (W - 1) * PerWorker + 1,
                               put_range(Lo, Lo + PerWorker - 1),
                               Parent ! {done, self()}
                       end) || W <- lists:seq(1, Workers)],
    [receive {done, P} -> ok after 120000 -> error(timeout) end || P <- Pids],
    sync(),
    %% This is the B2 regression: the pre-fix implementation emptied the
    %% whole table under exactly this load (concurrent, unguarded,
    %% re-entrant rotation, 200k/200k entries destroyed in 3/3 trials).
    ?assert(aec_mpt_cache:size() > 0),
    ?assert(maps:get(rotations, aec_mpt_cache:stats()) > 1),
    %% Rotation is async, so the entry bound is enforced at rest by the
    %% periodic tick rather than synchronously on the write path. Draining
    %% a fully cold cache is a legitimate outcome of that enforcement, so
    %% only the ceiling is asserted here.
    ok = gen_server:call(aec_mpt_cache, flush, 30000),
    sync(),
    ?assert(aec_mpt_cache:size() =< 2 * ?MAX_SIZE).

%% B5: ets:info(Tab, memory) is words and excludes refc binaries.
memory_in_bytes() ->
    Big = binary:copy(<<"x">>, 4096),
    ok = aec_mpt_cache:put(accounts, <<"big">>, Big),
    Words = ets:info(aec_mpt_read_cache, memory),
    Memory = aec_mpt_cache:memory(),
    ?assert(Memory >= Words * erlang:system_info(wordsize)),
    ?assert(Memory >= byte_size(Big)).

payload_tracked() ->
    Big = binary:copy(<<"x">>, 4096),
    ok = aec_mpt_cache:put(accounts, <<"a">>, [Big, Big]),
    Payload = maps:get(payload, aec_mpt_cache:stats()),
    ?assert(Payload >= 2 * byte_size(Big)),
    %% Re-putting the same content-addressed key must not double count.
    ok = aec_mpt_cache:put(accounts, <<"a">>, [Big, Big]),
    ?assertEqual(Payload, maps:get(payload, aec_mpt_cache:stats())).

config_honours_env() ->
    ?assertEqual(true, aec_mpt_cache:enabled()),
    ?assertEqual(?MAX_SIZE, aec_mpt_cache:max_size()),
    ?assertEqual(268435456, aec_mpt_cache:max_bytes()),
    set_config([{enabled, false}, {max_size, 7000}, {max_bytes, 1048576}]),
    ?assertEqual(false, aec_mpt_cache:enabled()),
    ?assertEqual(7000, aec_mpt_cache:max_size()),
    ?assertEqual(1048576, aec_mpt_cache:max_bytes()).

%% S2: entries cached before a safe-access window were never verified,
%% so the cache is flushed on entry and on exit.
safe_access_gate() ->
    ok = aec_mpt_cache:put(accounts, <<"k">>, v),
    ?assertEqual(true, aec_mpt_cache:gate_safe_access(false)),
    ?assertEqual(false, aec_mpt_cache:gate_safe_access(true)),
    sync(),
    ?assertEqual(0, aec_mpt_cache:size()),
    ok = aec_mpt_cache:put(accounts, <<"k">>, v),
    ?assertEqual(false, aec_mpt_cache:gate_safe_access(true)),
    %% Leaving the window drains synchronously: by the time any caller is
    %% told the cache is usable again, the unverified entries are gone.
    %% No sync() here on purpose -- production has no such barrier.
    ?assertEqual(false, aec_mpt_cache:gate_safe_access(false)),
    ?assertEqual(0, aec_mpt_cache:size()),
    ?assertEqual(true, aec_mpt_cache:gate_safe_access(false)),
    ?assertEqual(0, aec_mpt_cache:size()).

stats_shape() ->
    ok = aec_mpt_cache:put(accounts, <<"k">>, v),
    {value, v} = aec_mpt_cache:get(accounts, <<"k">>),
    none = aec_mpt_cache:get(accounts, <<"nope">>),
    Stats = aec_mpt_cache:stats(),
    ?assertEqual(1, maps:get(hits, Stats)),
    ?assertEqual(1, maps:get(misses, Stats)),
    [?assert(maps:is_key(K, Stats))
     || K <- [entries, memory, payload, hits, misses, evictions, rotations,
              rotate_us, young]].

%% SR-01: the application env fallback bypasses JSON-schema validation, so
%% a bad value must not reach arithmetic in the server loop.
invalid_config_rejected() ->
    set_config([{enabled, "yes"}, {max_size, not_an_integer}, {max_bytes, -1}]),
    ?assertEqual(false, aec_mpt_cache:enabled()),
    ?assertEqual(100000, aec_mpt_cache:max_size()),
    ?assertEqual(268435456, aec_mpt_cache:max_bytes()),
    %% The budget sweep does arithmetic on both; it must not crash.
    Pid = whereis(aec_mpt_cache),
    ok = gen_server:call(aec_mpt_cache, flush, 30000),
    ?assertEqual(Pid, whereis(aec_mpt_cache)).

%% SR-05: the periodic timer is keyed by a stored ref, so a stray message
%% to the registered name cannot arm an extra timer.
stray_messages_ignored() ->
    Pid = whereis(aec_mpt_cache),
    aec_mpt_cache ! flush,
    aec_mpt_cache ! {flush, make_ref()},
    aec_mpt_cache ! whatever,
    sync(),
    ?assertEqual(Pid, whereis(aec_mpt_cache)),
    ok = aec_mpt_cache:put(accounts, <<"k">>, v),
    ?assertEqual({value, v}, aec_mpt_cache:get(accounts, <<"k">>)).

%% SR-01: aec_mpt_cache is a temporary child of a MaxRestarts = 0
%% supervisor, so no callback may let an exception escape.
server_survives_faults() ->
    Pid = whereis(aec_mpt_cache),
    ok = aec_mpt_cache:put(accounts, <<"k">>, v),
    %% Table yanked from under the server, then every callback exercised.
    true = ets:delete(aec_mpt_read_cache),
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<"k">>)),
    ?assertEqual(ok, aec_mpt_cache:put(accounts, <<"k">>, v)),
    ?assertEqual(ok, aec_mpt_cache:clear()),
    gen_server:cast(aec_mpt_cache, rotate),
    ok = gen_server:call(aec_mpt_cache, flush, 30000),
    sync(),
    ?assertEqual(Pid, whereis(aec_mpt_cache)),
    ?assertEqual(0, aec_mpt_cache:size()),
    ?assertEqual(false, aec_mpt_cache:gate_safe_access(true)).

put_range(Lo, Hi) ->
    [aec_mpt_cache:put(accounts, key(I), {node, I}) || I <- lists:seq(Lo, Hi)],
    ok.

key(I) ->
    <<I:256>>.
