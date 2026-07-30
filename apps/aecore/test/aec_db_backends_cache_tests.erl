%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Tests for the production wiring of the MPT read cache into the
%%%    `aeu_mp_trees_db' backend callbacks.
%%%
%%%    The central invariant here is B1: a tree whose context is a GC
%%%    context must never be served from, or populated into, the read
%%%    cache. Under GC, `aec_db:lookup_tree_node/2' promotes a node found
%%%    in the secondary table back into the primary. A cache hit above
%%%    that boundary skips the promotion, and the node is dropped at the
%%%    next GC switch even though it is still reachable from a live root.
%%% @end
%%%=============================================================================
-module(aec_db_backends_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%% Mirrors of the tree context records in aec_db.erl. They are defined in
%% the module body there rather than a header, so they are reconstructed
%% here. `tree_context_shape_test_' below fails if they ever drift.
-define(TREE(Tab), {tree, Tab, transaction}).
-define(TREE_GC(Prim, Sec), {tree_gc, Prim, Sec, aec_account_state, transaction}).

wiring_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {"gc context is never cacheable", fun gc_context_never_cacheable/0}
     , {"plain context is cacheable", fun plain_context_cacheable/0}
     , {"disabled cache short circuits", fun disabled_cache/0}
     , {"safe access window is not cacheable", fun safe_access_not_cacheable/0}
     , {"unknown handle is not cacheable", fun unknown_handle/0}
     , {"gc reads always reach the promoting lookup", fun gc_reads_reach_lookup/0}
     , {"plain reads are served from cache after the first miss",
        fun plain_reads_are_cached/0}
     , {"none is never cached", fun no_negative_caching/0}
     ]}.

setup() ->
    case whereis(aec_mpt_cache) of
        undefined -> ok;
        Old       -> stop(Old)
    end,
    persistent_term:put({aec_db, db_safe_access}, false),
    application:set_env(aecore, mpt_read_cache,
                        [{enabled, true}, {max_size, 1000}, {max_bytes, 268435456}]),
    aec_mpt_cache:reload_config(),
    {ok, Pid} = aec_mpt_cache:start_link(),
    Pid.

teardown(Pid) ->
    catch meck:unload(aec_db),
    stop(Pid),
    persistent_term:erase({aec_db, db_safe_access}),
    application:unset_env(aecore, mpt_read_cache),
    aec_mpt_cache:reload_config(),
    ok.

stop(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok after 5000 -> ok end.

%%%===================================================================
%%% Namespace selection
%%%===================================================================

gc_context_never_cacheable() ->
    Ctxt = ?TREE_GC(aec_account_state, aec_account_state2),
    ?assertEqual(none, aec_db:tree_context_cache_key(Ctxt)),
    ?assertEqual(none, aec_db_backends:read_cache_tab(Ctxt)).

plain_context_cacheable() ->
    Ctxt = ?TREE(aec_account_state),
    ?assertEqual({ok, aec_account_state}, aec_db:tree_context_cache_key(Ctxt)),
    ?assertEqual({ok, aec_account_state}, aec_db_backends:read_cache_tab(Ctxt)).

disabled_cache() ->
    application:set_env(aecore, mpt_read_cache, [{enabled, false}]),
    aec_mpt_cache:reload_config(),
    ?assertEqual(none, aec_db_backends:read_cache_tab(?TREE(aec_account_state))).

safe_access_not_cacheable() ->
    persistent_term:put({aec_db, db_safe_access}, true),
    ?assertEqual(none, aec_db_backends:read_cache_tab(?TREE(aec_account_state))),
    persistent_term:put({aec_db, db_safe_access}, false),
    %% First read after the window closes flushes and still bypasses.
    ?assertEqual(none, aec_db_backends:read_cache_tab(?TREE(aec_account_state))),
    ?assertEqual({ok, aec_account_state},
                 aec_db_backends:read_cache_tab(?TREE(aec_account_state))).

unknown_handle() ->
    ?assertEqual(none, aec_db:tree_context_cache_key(something_else)).

%%%===================================================================
%%% Callback behaviour
%%%===================================================================

%% B1 regression: every read on a GC context reaches aec_db:lookup_tree_node/2,
%% which is where promote_tree_node/4 lives. Repeated reads of the same hash
%% must each reach it, so a node sitting in the secondary table is always
%% promoted before the next switch can drop it.
gc_reads_reach_lookup() ->
    mock_lookup(),
    Ctxt = ?TREE_GC(aec_account_state, aec_account_state2),
    [?assertEqual({value, <<"v">>}, aec_db_backends:mpt_db_get(<<"k">>, Ctxt))
     || _ <- lists:seq(1, 5)],
    ?assertEqual(5, meck:num_calls(aec_db, lookup_tree_node, [<<"k">>, Ctxt])),
    ?assertEqual(0, aec_mpt_cache:size()).

plain_reads_are_cached() ->
    mock_lookup(),
    Ctxt = ?TREE(aec_account_state),
    [?assertEqual({value, <<"v">>}, aec_db_backends:mpt_db_get(<<"k">>, Ctxt))
     || _ <- lists:seq(1, 5)],
    ?assertEqual(1, meck:num_calls(aec_db, lookup_tree_node, [<<"k">>, Ctxt])),
    ?assertEqual({value, <<"v">>}, aec_mpt_cache:get(aec_account_state, <<"k">>)).

no_negative_caching() ->
    meck:new(aec_db, [passthrough]),
    meck:expect(aec_db, lookup_tree_node, fun(_, _) -> none end),
    Ctxt = ?TREE(aec_account_state),
    ?assertEqual(none, aec_db_backends:mpt_db_get(<<"k">>, Ctxt)),
    ?assertEqual(none, aec_db_backends:mpt_db_get(<<"k">>, Ctxt)),
    ?assertEqual(2, meck:num_calls(aec_db, lookup_tree_node, [<<"k">>, Ctxt])),
    ?assertEqual(0, aec_mpt_cache:size()).

mock_lookup() ->
    meck:new(aec_db, [passthrough]),
    meck:expect(aec_db, lookup_tree_node, fun(_Key, _Ctxt) -> {value, <<"v">>} end).

%%%===================================================================
%%% Guard against the mirrored records drifting from aec_db
%%%===================================================================

tree_context_shape_test_() ->
    {"mirrored tree context records still match aec_db",
     fun() ->
             ?assertEqual({ok, aec_account_state},
                          aec_db:tree_context_cache_key(?TREE(aec_account_state))),
             ?assertEqual(none,
                          aec_db:tree_context_cache_key(
                            ?TREE_GC(aec_account_state, aec_account_state2)))
     end}.
