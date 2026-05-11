%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Unit tests for aeu_mp_trees_db read-cache layering.
%%% @end
%%%=============================================================================
-module(aeu_mp_trees_db_tests).

-include_lib("eunit/include/eunit.hrl").

-behavior(aeu_mp_trees_db).

-export([ mpt_db_drop_cache/1
        , mpt_db_get/2
        , mpt_db_get/3
        , mpt_db_put/3
        , mpt_db_read_cache_get/2
        , mpt_db_read_cache_put/3
        ]).

mpt_db_test_() ->
    [ {"cache disabled passthrough", fun cache_disabled_passthrough/0}
    , {"cache enabled hit after miss", fun cache_enabled_hit_after_miss/0}
    , {"populate on backend write", fun populate_on_backend_write/0}
    , {"no negative caching", fun no_negative_caching/0}
    , {"write cache takes precedence", fun write_cache_takes_precedence/0}
    , {"previous db record keeps cache disabled", fun previous_db_record_keeps_cache_disabled/0}
    , {"legacy db record keeps cache disabled", fun legacy_db_record_keeps_cache_disabled/0}
    ].

cache_disabled_passthrough() ->
    with_db(false,
            fun(DB, Backend, _ReadCache) ->
                    ets:insert(Backend, {<<"k">>, <<"v">>}),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, DB)),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, DB)),
                    ?assertEqual(2, backend_gets())
            end).

cache_enabled_hit_after_miss() ->
    with_db(true,
            fun(DB, Backend, _ReadCache) ->
                    ets:insert(Backend, {<<"k">>, <<"v">>}),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, DB)),
                    ?assertEqual(1, backend_gets()),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, DB)),
                    ?assertEqual(1, backend_gets())
            end).

populate_on_backend_write() ->
    with_db(true,
            fun(DB, _Backend, _ReadCache) ->
                    DB1 = aeu_mp_trees_db:unsafe_write_to_backend(<<"k">>, <<"v">>, DB),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, DB1)),
                    ?assertEqual(0, backend_gets())
            end).

no_negative_caching() ->
    with_db(true,
            fun(DB, _Backend, ReadCache) ->
                    ?assertEqual(none, aeu_mp_trees_db:get(<<"k">>, DB)),
                    ?assertEqual([], ets:lookup(ReadCache, <<"k">>)),
                    DB1 = aeu_mp_trees_db:unsafe_write_to_backend(<<"k">>, <<"v">>, DB),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, DB1))
            end).

write_cache_takes_precedence() ->
    with_db(true,
            fun(DB, _Backend, ReadCache) ->
                    ets:insert(ReadCache, {<<"k">>, <<"stale">>}),
                    DB1 = aeu_mp_trees_db:put(<<"k">>, <<"fresh">>, DB),
                    ?assertEqual({value, <<"fresh">>}, aeu_mp_trees_db:get(<<"k">>, DB1)),
                    ?assertEqual(0, backend_gets())
            end).

previous_db_record_keeps_cache_disabled() ->
    with_db(false,
            fun(_DB, Backend, _ReadCache) ->
                    ets:insert(Backend, {<<"k">>, <<"v">>}),
                    PreviousDB = {db, ?MODULE, {backend, Backend}, {gb_trees, gb_trees:empty()}},
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, PreviousDB)),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, PreviousDB)),
                    ?assertEqual(2, backend_gets())
            end).

legacy_db_record_keeps_cache_disabled() ->
    with_db(false,
            fun(_DB, Backend, _ReadCache) ->
                    ets:insert(Backend, {<<"k">>, <<"v">>}),
                    OldDB = {db, {backend, Backend}, {gb_trees, gb_trees:empty()},
                             {?MODULE, mpt_db_drop_cache},
                             {?MODULE, mpt_db_get},
                             {?MODULE, mpt_db_put}},
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, OldDB)),
                    ?assertEqual({value, <<"v">>}, aeu_mp_trees_db:get(<<"k">>, OldDB)),
                    ?assertEqual(2, backend_gets())
            end).

with_db(UseReadCache, Fun) ->
    Backend = ets:new(?MODULE, [set]),
    ReadCache = ets:new(?MODULE, [set]),
    erlang:put(backend_gets, 0),
    Spec0 = #{ module => ?MODULE
             , cache  => {gb_trees, gb_trees:empty()}
             , handle => {backend, Backend}
             },
    Spec = case UseReadCache of
               true  -> Spec0#{read_cache => ReadCache};
               false -> Spec0
           end,
    DB = aeu_mp_trees_db:new(Spec),
    try Fun(DB, Backend, ReadCache)
    after
        ets:delete(Backend),
        ets:delete(ReadCache),
        erlang:erase(backend_gets)
    end.

backend_gets() ->
    erlang:get(backend_gets).

mpt_db_get(Key, {gb_trees, Tree}) ->
    gb_trees:lookup(Key, Tree);
mpt_db_get(Key, {backend, Tab}) ->
    erlang:put(backend_gets, backend_gets() + 1),
    case ets:lookup(Tab, Key) of
        [{Key, Val}] -> {value, Val};
        []           -> none
    end.

mpt_db_get(Key, Target, Map) when is_map(Map) ->
    Map#{result => mpt_db_get(Key, Target)}.

mpt_db_put(Key, Val, {gb_trees, Tree}) ->
    {gb_trees, gb_trees:enter(Key, Val, Tree)};
mpt_db_put(Key, Val, {backend, Tab} = Handle) ->
    ets:insert(Tab, {Key, Val}),
    Handle.

mpt_db_drop_cache({gb_trees, _Tree}) ->
    {gb_trees, gb_trees:empty()}.

mpt_db_read_cache_get(Key, Tab) ->
    case ets:lookup(Tab, Key) of
        [{Key, Val}] -> {value, Val};
        []           -> none
    end.

mpt_db_read_cache_put(Key, Val, Tab) ->
    ets:insert(Tab, {Key, Val}),
    ok.
