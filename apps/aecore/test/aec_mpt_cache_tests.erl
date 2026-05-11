%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Unit tests for the MPT read cache.
%%% @end
%%%=============================================================================
-module(aec_mpt_cache_tests).

-include_lib("eunit/include/eunit.hrl").

mpt_cache_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"start is idempotent", fun start_is_idempotent/0}
     , {"missing key returns none", fun missing_key_returns_none/0}
     , {"put then get", fun put_then_get/0}
     , {"clear removes entries", fun clear_removes_entries/0}
     , {"rotation drops cold generation", fun rotation_drops_cold_generation/0}
     , {"enabled honours application env", fun enabled_honours_application_env/0}
     , {"start re-reads config", fun start_re_reads_config/0}
     , {"lookup auto-recovers if table is gone", fun lookup_auto_recovers/0}
     ]}.

setup() ->
    application:set_env(aecore, mpt_read_cache, [{enabled, true}, {max_size, 3}]),
    ok = aec_mpt_cache:start(),
    ok = aec_mpt_cache:clear(),
    ok.

teardown(_) ->
    ok = aec_mpt_cache:clear(),
    application:unset_env(aecore, mpt_read_cache),
    ok.

start_is_idempotent() ->
    ?assertEqual(ok, aec_mpt_cache:start()),
    ?assertEqual(ok, aec_mpt_cache:start()).

missing_key_returns_none() ->
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<"missing">>)).

put_then_get() ->
    ok = aec_mpt_cache:put(accounts, <<"hash">>, {node, 1}),
    ?assertEqual({value, {node, 1}}, aec_mpt_cache:get(accounts, <<"hash">>)).

clear_removes_entries() ->
    ok = aec_mpt_cache:put(accounts, <<"hash">>, {node, 1}),
    ?assert(aec_mpt_cache:size() > 0),
    ok = aec_mpt_cache:clear(),
    ?assertEqual(0, aec_mpt_cache:size()),
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<"hash">>)).

rotation_drops_cold_generation() ->
    [ok = aec_mpt_cache:put(accounts, <<I>>, {node, I}) || I <- lists:seq(1, 7)],
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<1>>)),
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<2>>)),
    ?assertEqual({value, {node, 7}}, aec_mpt_cache:get(accounts, <<7>>)),
    ?assert(aec_mpt_cache:size() =< 6).

enabled_honours_application_env() ->
    ?assertEqual(true, aec_mpt_cache:enabled()),
    ?assertEqual(3, aec_mpt_cache:max_size()).

start_re_reads_config() ->
    ?assertEqual(true, aec_mpt_cache:enabled()),
    application:set_env(aecore, mpt_read_cache, [{enabled, false}, {max_size, 7}]),
    %% Previously memoised value is still in persistent_term:
    ?assertEqual(true, aec_mpt_cache:enabled()),
    ok = aec_mpt_cache:start(),
    ?assertEqual(false, aec_mpt_cache:enabled()),
    ?assertEqual(7, aec_mpt_cache:max_size()).

lookup_auto_recovers() ->
    ok = aec_mpt_cache:put(accounts, <<"k">>, {node, 1}),
    ?assertEqual({value, {node, 1}}, aec_mpt_cache:get(accounts, <<"k">>)),
    true = ets:delete(aec_mpt_read_cache),
    %% First lookup after the table has been wiped must not crash, and
    %% must not loop. It returns `none' (empty new table) and the next
    %% put / get cycle works normally.
    ?assertEqual(none, aec_mpt_cache:get(accounts, <<"k">>)),
    ok = aec_mpt_cache:put(accounts, <<"k">>, {node, 2}),
    ?assertEqual({value, {node, 2}}, aec_mpt_cache:get(accounts, <<"k">>)).
