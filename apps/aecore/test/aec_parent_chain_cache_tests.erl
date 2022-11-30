%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_parent_chain_cache
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_parent_chain_cache).

-define(ALICE, <<123450:32/unit:8>>).
-define(BOB,   <<123451:32/unit:8>>).
-define(CAROL, <<123452:32/unit:8>>).
-define(DAVE,  <<123453:32/unit:8>>).

%%%===================================================================
%%% Test cases
%%%===================================================================


follow_child_chain_strategy_test_() ->
    {foreach,
     fun() ->
            meck:new(aec_chain, []),
            meck:expect(aec_chain, top_height, fun() -> 0 end),
            meck:new(aec_conductor, []),
            meck:expect(aec_conductor, get_mining_state, fun() -> stopped end),
            mock_parent_connector(),
            mock_events()
     end,
     fun(_) ->
            unmock_events(),
            meck:unload(aec_chain),
            meck:unload(aec_conductor),
            unmock_parent_connector()
     end,
     [  {"Cache all the blocks above current child height", fun cache_all_above_child_height/0},
        {"Post cachable parent top", fun post_cachable_parent_top/0},
        {"Post non cachable parent top", fun post_non_cachable_parent_top/0},
        {"Post child top in the middle of caching heights", fun post_child_top_in_the_middle_of_cachable_heights/0},
        {"Configurable confirmation height", fun configurable_confirmation_height/0}
     ]}.

produce_commitments_test_() ->
    {foreach,
     fun() ->
            meck:new(aec_chain, []),
            meck:expect(aec_chain, top_height, fun() -> 0 end),
            meck:expect(aec_chain, genesis_hash, fun() -> height_to_hash(0) end),
            meck:new(aec_conductor, []),
            meck:expect(aec_conductor, get_mining_state, fun() -> running end),
            meck:new(aetx_env, []),
            meck:expect(aetx_env, tx_env_and_trees_from_hash,
                        fun(_, _Hash) -> {tx_env, trees} end),
            mock_parent_connector(),
            mock_stakers(),
            mock_events()
     end,
     fun(_) ->
            unmock_events(),
            unmock_stakers(),
            meck:unload(aec_chain),
            meck:unload(aetx_env),
            meck:unload(aec_conductor),
            unmock_parent_connector()
     end,
     [  {"No commitments before the startheight", fun no_commitments_before_start/0},
        {"Post genesis commitments before start seing blocks on the child chain", fun post_initial_commitments/0},
        {"Post commitments according to child hash", fun post_commitments/0},
        {"No commitments if stopped", fun no_commitments_if_stopped/0}
     ]}.


%%%===================================================================
%%% Test cases
%%%===================================================================

cache_all_above_child_height() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, _Confirmations = 1),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight = ChildTop0 + StartHeight,
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

post_cachable_parent_top() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, _Confirmations = 1),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxCachableHeight(ChildTop0) - 2,
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
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, _Confirmations = 1),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxCachableHeight(ChildTop0) + 10,
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
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, CachePid} = start_cache(StartHeight, CacheMaxSize, _Confirmations = 1),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
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
                    top_height         := ParentTop} = Res} = ?TEST_MODULE:get_state(),
            {ChildTop1, ChildTop1} = {ChildTop1, ChildTop2},
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.


configurable_confirmation_height() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0, Confirmations) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, Confirmations),
            timer:sleep(20),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight = ChildTop0 + StartHeight,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(ChildTop0 + StartHeight - CacheMaxSize - 1),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(ExpectedTopHeight + 1),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0, 1),
    Test(20, 200, 0, 10),
    ok.

no_commitments_before_start() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    Confirmations = 10,
    ChildTop0 = 0,
    Offset = 10,
    meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
    ParentTop = StartHeight - Offset,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, Confirmations),
    %% populate the cache and start making commitments
    lists:foreach(
        fun(Idx) ->
            ParentHeight = ParentTop + Idx,
            set_parent_chain_top(ParentHeight),
            Block = block_by_height(ParentHeight),
            ?TEST_MODULE:post_block(Block),
            timer:sleep(10),
            %% ensure that the node is up to date with the parent chain
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ParentHeight,
                    child_top_height   := ChildTop0} = _Res} = ?TEST_MODULE:get_state(),
            [] = get_commitments(?ALICE),
            [] = get_commitments(?BOB),
            [] = get_commitments(?CAROL),
            [] = get_commitments(?DAVE),
            meck:reset(aec_parent_connector),
            ok
        end,
        lists:seq(0, Offset - 1)),
    ?TEST_MODULE:stop(),
    ok.

post_initial_commitments() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    Confirmations = 10,
    ChildTop0 = 0,
    meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
    ParentTop = StartHeight,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, Confirmations),
    GenesisHash = aeser_api_encoder:encode(key_block_hash, height_to_hash(0)),
    %% populate the cache and start making commitments
    lists:foreach(
        fun(Idx) ->
            ParentHeight = ParentTop + Idx,
            set_parent_chain_top(ParentHeight),
            Block = block_by_height(ParentHeight),
            ?TEST_MODULE:post_block(Block),
            timer:sleep(10),
            %% ensure that the node is up to date with the parent chain
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ParentHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            [GenesisHash] = get_commitments(?ALICE),
            [GenesisHash] = get_commitments(?BOB),
            [] = get_commitments(?CAROL),
            [] = get_commitments(?DAVE),
            meck:reset(aec_parent_connector),
            ok
        end,
        lists:seq(0, Confirmations - 1)),
    ?TEST_MODULE:stop(),
    ok.

post_commitments() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    Confirmations = 10,
    ChildTop = Confirmations,
    meck:expect(aec_chain, top_height, fun() -> ChildTop end),
    ParentTop = StartHeight + Confirmations,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, CachePid} = start_cache(StartHeight, CacheMaxSize, Confirmations),
    %% populate the cache and start making commitments
    lists:foreach(
        fun(Idx) ->
            ParentHeight = ParentTop + Idx,
            set_parent_chain_top(ParentHeight),
            ChildTop1 = ChildTop + Idx,
            meck:reset(aec_parent_connector),
            child_new_top(CachePid, ChildTop1),
            Block = block_by_height(ParentHeight),
            ?TEST_MODULE:post_block(Block),
            timer:sleep(10),
            %% ensure that the node is up to date with the parent chain and
            %% the child chain
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ParentHeight,
                    child_top_height   := ChildTop1}} = ?TEST_MODULE:get_state(),
            Hash = aeser_api_encoder:encode(key_block_hash, height_to_hash(ChildTop1)),
            [Hash] = get_commitments(?ALICE),
            [Hash] = get_commitments(?BOB),
            [] = get_commitments(?CAROL),
            [] = get_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    ?TEST_MODULE:stop(),
    ok.

no_commitments_if_stopped() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    Confirmations = 10,
    ChildTop = Confirmations,
    meck:expect(aec_chain, top_height, fun() -> ChildTop end),
    ParentTop = StartHeight + Confirmations,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    meck:expect(aec_conductor, get_mining_state, fun() -> stopped end),
    {ok, CachePid} = start_cache(StartHeight, CacheMaxSize, Confirmations),
    %% populate the cache and start making commitments
    lists:foreach(
        fun(Idx) ->
            ParentHeight = ParentTop + Idx,
            set_parent_chain_top(ParentHeight),
            ChildTop1 = ChildTop + Idx,
            meck:reset(aec_parent_connector),
            child_new_top(CachePid, ChildTop1),
            Block = block_by_height(ParentHeight),
            ?TEST_MODULE:post_block(Block),
            timer:sleep(10),
            %% ensure that the node is up to date with the parent chain and
            %% the child chain
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ParentHeight,
                    child_top_height   := ChildTop1}} = ?TEST_MODULE:get_state(),
            [] = get_commitments(?ALICE),
            [] = get_commitments(?BOB),
            [] = get_commitments(?CAROL),
            [] = get_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    ?TEST_MODULE:stop(),
    meck:expect(aec_conductor, get_mining_state, fun() -> running end),
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

start_cache(StartHeight, MaxSize, Confirmations) ->
    Args = [StartHeight, MaxSize, Confirmations],
    gen_server:start({local, ?TEST_MODULE}, ?TEST_MODULE, Args, []).

height_to_hash(Height) when Height < 0 -> height_to_hash(0);
height_to_hash(Height) when is_integer(Height) -> <<Height:32/unit:8>>.

%% hash_to_height(Hash) ->
%%     MeaningfulBytes = [B || B <- binary_to_list(Hash), B =/= 0],
%%     {Height, _} =
%%         lists:foldr( %% NB: we go right to left!
%%             fun(B, {AccumHeight, ByteIdx}) ->
%%                 {B * trunc(math:pow(8, ByteIdx)) + AccumHeight, ByteIdx + 1}
%%             end,
%%             {0, 0},
%%             MeaningfulBytes),
%%     Height.

block_by_height(Height) ->
    Hash = height_to_hash(Height),
    PrevHash = height_to_hash(Height - 1),
    aec_parent_chain_block:new(Hash, Height, PrevHash).

%% block_by_hash(Hash) ->
%%     Height = hash_to_height(Hash),
%%     block_by_height(Height).

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
    meck:expect(aec_parent_connector, post_commitment,
                fun(_Who, _What) -> ok end),
    ok.

mock_stakers() ->
    meck:new(aec_consensus_hc, []),
    meck:new(aec_preset_keys, []),
    ok.

unmock_stakers() ->
    meck:unload(aec_preset_keys),
    meck:unload(aec_consensus_hc),
    ok.

expect_stakers(StakerList) ->
    meck:expect(aec_consensus_hc, parent_chain_validators,
                fun(_TxEnv, _Trees) -> {ok, StakerList} end),
    ok.

expect_keys(PubkeyList) ->
    meck:expect(aec_preset_keys, is_key_present,
                fun(Pubkey) -> lists:member(Pubkey, PubkeyList) end),
    ok.

set_parent_chain_top(TopHeight) ->
    meck:expect(aec_parent_connector, request_block_by_height,
                fun(RequestedHeight) when RequestedHeight > TopHeight ->
                    ok;
                   (RequestedHeight) ->
                    spawn(
                        fun() ->
                            Block = block_by_height(RequestedHeight),
                            ?TEST_MODULE:post_block(Block)
                        end)
                    end),
    meck:expect(aec_parent_connector, fetch_block_by_height,
                fun(RequestedHeight) when RequestedHeight > TopHeight ->
                    {error, not_found};
                   (RequestedHeight) ->
                    Block = block_by_height(RequestedHeight),
                    {ok, Block}
                end),
    ok.

unmock_parent_connector() ->
    meck:unload(aec_parent_connector).

mock_events() ->
    meck:new(aec_events, []),
    meck:expect(aec_events, subscribe,
                fun(top_changed) -> ok end),
    ok.

unmock_events() ->
    meck:unload(aec_events).

child_new_top(CachePid, Height) ->
    Hash = height_to_hash(Height),
    CachePid ! {gproc_ps_event, top_changed, #{info => #{block_type => key,
                                                         block_hash => Hash,
                                                         height => Height}}}.

assert_child_cache_consistency(#{ child_start_height := StartHeight,
                                  child_top_height   := ChildTop,
                                  blocks             := Blocks,
                                  max_size           := CacheMaxSize,
                                  pc_confirmations   := Confirmations,
                                  top_height         := TopHeight}) ->
    ?assertEqual(CacheMaxSize, map_size(Blocks)),
    CacheExpectedStart = min(ChildTop + StartHeight, TopHeight - CacheMaxSize + 1),
    ?assertEqual(CacheExpectedStart, lists:min(maps:keys(Blocks))),
    CacheExpectedEnd = CacheExpectedStart + CacheMaxSize - 1,
    ?assertEqual(CacheExpectedEnd, lists:max(maps:keys(Blocks))),
    lists:foreach(
        fun(Height) ->
            {true, Height} = {maps:is_key(Height, Blocks), Height},
            IsMature = TopHeight - Confirmations >= Height,
            Block = block_by_height(Height),
            case ?TEST_MODULE:get_block_by_height(Height) of
                {ok, Block} when IsMature -> ok;
                {error, {not_enough_confirmations, Block}} -> ok
            end,
            ok
        end,
        lists:seq(CacheExpectedEnd, CacheExpectedEnd)),
    ok.

get_commitments(Staker) ->
    AllCommitments =
        lists:filter(
            fun({_Pid, {_M, _F, [Who, _Hash]}, _Res}) ->
                Who =:= Staker
            end,
            filter_meck_events(aec_parent_connector, post_commitment)),
    lists:map(
        fun({_Pid, {_M, _F, [_Who, Hash]}, _Res}) -> Hash end,
        AllCommitments).

filter_meck_events(Module, Function) ->
    lists:filter(
        fun({_Pid, {M, F, _Args}, _Res}) ->
            M =:= Module andalso F =:= Function
        end,
        meck:history(Module)).

