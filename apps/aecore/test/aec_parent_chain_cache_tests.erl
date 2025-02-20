%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_parent_chain_cache
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_parent_chain_cache).

-define(GENESIS,  <<42:32/unit:8>>).
-define(NETWORK_ID, <<"hc_eunit">>).
-define(EPOCH, 3).

-define(SIGN_MODULE, aec_preset_keys).

%%%===================================================================
%%% Test cases
%%%===================================================================


follow_child_chain_strategy_test_() ->
    {foreach,
     fun() ->
            meck:new(aec_chain, []),
            meck:expect(aec_chain, top_header, fun() -> header(0) end),
            meck:expect(aec_chain, genesis_hash, fun() -> ?GENESIS end),
            meck:new(aec_conductor, []),
            mock_parent_connector(),
            mock_events()
     end,
     fun(_) ->
            unmock_events(),
            meck:unload(aec_chain),
            meck:unload(aec_conductor),
            unmock_parent_connector()
     end,
     [  {"Cache the blocks above current child height", fun cache_above_child_height/0},
        {"Post cachable parent top", fun post_cachable_parent_top/0},
        {"Post non cachable parent top", fun post_non_cachable_parent_top/0},
        {"Post child top in the middle of caching heights", fun post_child_top_in_the_middle_of_cachable_heights/0},
        {"Configurable confirmation height", fun configurable_confirmation_height/0},
        {"Cache the blocks above current child height with finality", fun cache_above_child_height_finality/0}
     ]}.

%%%===================================================================
%%% Test cases
%%%===================================================================

cache_above_child_height() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight = ChildTop0 + StartHeight + ?EPOCH,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(ChildTop0
                                                                     +
                                                                     StartHeight
                                                                     - CacheMaxSize - 1),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(ExpectedTopHeight + 1),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

cache_above_child_height_finality() ->
    Finality = 2,
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, Finality),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight = ChildTop0 + StartHeight + ?EPOCH,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),

            %% Now ExpectedTopHeight - Finality + 1 will not be final.
            {error, not_final} = ?TEST_MODULE:get_block_by_height(ExpectedTopHeight - Finality + 1),

            %% Let's make it final
            ?TEST_MODULE:post_block(block_by_height(ExpectedTopHeight + 1)),
            {ok, _Block} = ?TEST_MODULE:get_block_by_height(ExpectedTopHeight - Finality + 1),

            ?TEST_MODULE:post_block(block_by_height(ExpectedTopHeight + Finality)),

            {ok, Res} = ?TEST_MODULE:get_state(),

            assert_child_cache_consistency(Res, Finality),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

post_cachable_parent_top() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight + ?EPOCH,
            MaxHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + ?EPOCH end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxHeight(ChildTop0),
            ?TEST_MODULE:post_block(block_by_height(ParentTop)),
            timer:sleep(20),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop0,
                    top_height         := ParentTop} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

post_non_cachable_parent_top() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight + ?EPOCH,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = ExpectedTopHeight + 10,
            ?TEST_MODULE:post_block(block_by_height(ParentTop)),
            timer:sleep(20),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop0,
                    top_height         := ParentTop} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

post_child_top_in_the_middle_of_cachable_heights() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight + ?EPOCH,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + ?EPOCH end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxCachableHeight(ChildTop0) + 10,
            ?TEST_MODULE:post_block(block_by_height(ParentTop)),
            timer:sleep(20),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop0,
                    top_height         := ParentTop}} = ?TEST_MODULE:get_state(),
            ChildTop1 = ChildTop0 + 10,
            child_new_top(CachePid, ChildTop1),
            timer:sleep(20),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop2,
                    top_height         := ParentTop} = _Res} = ?TEST_MODULE:get_state(),
            {ChildTop1, ChildTop1} = {ChildTop1, ChildTop2},
            %%assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.


configurable_confirmation_height() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight = ChildTop0 + StartHeight + ?EPOCH,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(ChildTop0 + StartHeight - CacheMaxSize - 1),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(ExpectedTopHeight + 1),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 0),
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================

start_cache(StartHeight, MaxSize) ->
    start_cache(StartHeight, MaxSize, 0).

start_cache(StartHeight, MaxSize, Finality) ->
    ParentTargetFun = fun(Height) -> lists:seq(Height + StartHeight, Height + StartHeight + ?EPOCH) end,
    Args = [StartHeight, 1000, ParentTargetFun, MaxSize, Finality],
    gen_server:start_link({local, ?TEST_MODULE}, ?TEST_MODULE, Args, []).


height_to_hash(Height) when Height < 0 -> height_to_hash(0);
height_to_hash(Height) when is_integer(Height) -> <<Height:32/unit:8>>.

block_by_height(Height) ->
    Hash = height_to_hash(Height),
    PrevHash = height_to_hash(Height - 1),
    aec_parent_chain_block:new(Hash, Height, PrevHash, 0).

mock_parent_connector() ->
    meck:new(aec_parent_connector, []),
    meck:expect(aec_parent_connector, request_block_by_height,
                fun(Height) ->
                    spawn(
                        fun() ->
                            Block = block_by_height(Height),
                            ?TEST_MODULE:post_block(Block)
                        end)
                    end),
    meck:expect(aec_parent_connector, fetch_block_by_height,
                fun(Height) ->
                    Block = block_by_height(Height),
                    {ok, Block}
                end),
    meck:expect(aec_parent_connector, request_top,
                fun() -> ok end),
    ok.


unmock_parent_connector() ->
    meck:unload(aec_parent_connector).

mock_events() ->
    meck:new(aec_events, []),
    meck:expect(aec_events, subscribe,
                fun(top_changed) -> ok;
                   (start_mining) -> ok;
                   (chain_sync) -> ok;
                   (stop_mining) -> ok
                end),
    ok.

unmock_events() ->
    meck:unload(aec_events).

child_new_top(CachePid, Height) ->
    Hash = height_to_hash(Height),
    CachePid ! {gproc_ps_event, top_changed, #{info => #{block_type => key,
                                                         block_hash => Hash,
                                                         height => Height}}}.

assert_child_cache_consistency(State) ->
    assert_child_cache_consistency(State, 0).

assert_child_cache_consistency(#{ child_start_height := StartHeight,
                                  child_top_height   := ChildTop,
                                  blocks             := Blocks0,
                                  top_height         := TopHeight}, Finality) ->
    Blocks = maps:filter(fun(_, V) -> is_tuple(V) end, Blocks0),
    CacheExpectedStart = ChildTop + StartHeight,
    ?assertEqual(CacheExpectedStart, lists:min(maps:keys(Blocks))),
    CacheExpectedEnd = CacheExpectedStart + ?EPOCH,
    ?assertEqual(CacheExpectedEnd, lists:max(maps:keys(Blocks))),
    lists:foreach(
        fun(Height) ->
            {true, Height} = {maps:is_key(Height, Blocks), Height},
            IsFinal = TopHeight - Finality >= Height,
            Block = block_by_height(Height),
            case ?TEST_MODULE:get_block_by_height(Height) of
                {ok, Block} when IsFinal -> ok;
                {error, not_final} when not IsFinal -> ok
            end,
            ok
        end,
        lists:seq(CacheExpectedStart, CacheExpectedEnd)),
    ok.

header(Height) ->
    Hash = <<Height:32/unit:8>>,
    PrevHeight =
        case Height of
            0 -> 0;
            H -> H - 1
        end,
    Hash = <<Height:32/unit:8>>,
    PrevHash = <<PrevHeight:32/unit:8>>,
    PrevKeyHash = PrevHash,
    RootHash = PrevHash,
    Miner = PrevHash,
    Beneficiary = Miner,
    Target = 0,
    KeySeal = lists:duplicate(42, 0),
    Nonce = 0,
    Time = 0,
    Info = default,
    Version = 0,
    aec_headers:new_key_header(Height, PrevHash, PrevKeyHash, RootHash, Miner, Beneficiary,
               Target, KeySeal, Nonce, Time, Info, Version).
