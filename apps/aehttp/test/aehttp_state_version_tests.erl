%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Unit tests for the X-Ae-Height chain-state height cache
%%%    (aehttp_state_version).
%%% @end
%%%=============================================================================
-module(aehttp_state_version_tests).

-include_lib("eunit/include/eunit.hrl").

state_version_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [ {"top_height falls back to a dirty read when the cache is unavailable",
        fun fallback_dirty_read/0}
     , {"top_height is undefined when the chain top is not readable",
        fun undefined_when_no_top/0}
     , {"top_height serves the cached value without hitting the chain",
        fun cached_read_skips_dirty/0}
     , {"a top_changed event updates the cached height",
        fun top_changed_updates_cache/0}
     ]}.

setup() ->
    ok = meck:new(aec_events, [passthrough]),
    ok = meck:new(aec_chain, [passthrough]),
    ok = meck:new(aec_headers, [passthrough]),
    %% subscribe/1 returns `true' - init/1 relies on that exact value.
    meck:expect(aec_events, subscribe, fun(top_changed) -> true end),
    %% A mock header is just a tagged height; height/1 unwraps it.
    meck:expect(aec_headers, height, fun({mock_header, H}) -> H end),
    ok.

cleanup(_) ->
    stop_server(),
    meck:unload().

%%%===================================================================
%%% Test cases
%%%===================================================================

%% No server has been started, so the ETS cache table does not exist and
%% top_height/0 must fall back to a dirty chain read rather than crash.
fallback_dirty_read() ->
    set_top(42),
    ?assertEqual(42, aehttp_state_version:top_height()).

undefined_when_no_top() ->
    set_no_top(),
    ?assertEqual(undefined, aehttp_state_version:top_height()).

%% Once the server has seeded the cache, top_height/0 reads it and does not
%% consult the chain again - proven by making a fresh dirty read return a
%% different answer (undefined) while the cached value still wins.
cached_read_skips_dirty() ->
    set_top(100),
    start_server(),
    ?assertEqual(100, aehttp_state_version:top_height()),
    set_no_top(),
    ?assertEqual(100, aehttp_state_version:top_height()).

top_changed_updates_cache() ->
    set_top(100),
    start_server(),
    ?assertEqual(100, aehttp_state_version:top_height()),
    aehttp_state_version ! {gproc_ps_event, top_changed,
                            #{info => #{height => 150}}},
    %% A synchronous call flushes the mailbox: the info message above is
    %% handled before this call is answered.
    _ = gen_server:call(aehttp_state_version, sync),
    ?assertEqual(150, aehttp_state_version:top_height()).

%%%===================================================================
%%% Helpers
%%%===================================================================

set_top(Height) ->
    meck:expect(aec_chain, dirty_top_header, fun() -> {mock_header, Height} end).

set_no_top() ->
    meck:expect(aec_chain, dirty_top_header, fun() -> undefined end).

start_server() ->
    {ok, _Pid} = aehttp_state_version:start_link(),
    ok.

stop_server() ->
    case whereis(aehttp_state_version) of
        undefined -> ok;
        Pid       -> gen_server:stop(Pid)
    end.
