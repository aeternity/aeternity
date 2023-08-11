%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_parent_chain_cache
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_parent_chain_cache).

-define(ALICE,
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>).

-define(ALICE_PRIV,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>).

-define(BOB,
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>).
-define(BOB_PRIV,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>).

-define(CAROL,
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>).

-define(CAROL_PRIV,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>).

-define(DAVE,
    <<8,137,159,99,139,175,27,58,77,11,191,52,198,199,7,50,133,195,184,219,
        148,124,4,5,44,247,57,95,188,173,95,35>>).

-define(DAVE_PRIV,
    <<107,251,189,176,92,221,4,46,56,231,137,117,181,8,124,14,212,150,167,
        53,95,94,50,86,144,230,93,222,61,116,85,96,8,137,159,99,139,175,27,58,
        77,11,191,52,198,199,7,50,133,195,184,219,148,124,4,5,44,247,57,95,
        188,173,95,35>>).

-define(GENESIS,  <<42:32/unit:8>>).
-define(NETWORK_ID, <<"hc_eunit">>).
-define(HC_COMMITMENT_BTC, 3).

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
            meck:expect(aec_chain, top_header, fun() -> header(0) end),
            meck:expect(aec_chain, genesis_hash, fun() -> height_to_hash(0) end),
            meck:new(aec_conductor, []),
            meck:new(aetx_env, []),
            meck:expect(aetx_env, tx_env_and_trees_from_hash,
                        fun(_, _Hash) -> {tx_env, trees} end),
            mock_parent_connector(),
            mock_stakers(),
            mock_events(),
            mock_network_id(?NETWORK_ID),
            mock_sign_module()
     end,
     fun(_) ->
            unmock_events(),
            unmock_stakers(),
            meck:unload(aec_chain),
            meck:unload(aetx_env),
            meck:unload(aec_conductor),
            unmock_parent_connector(),
            unmock_network_id(),
            unmock_sign_module()
     end,
     [  {"No commitments before the startheight", fun no_commitments_before_start/0},
        {"No commitments before seing blocks on the child chain", fun do_not_post_commitments_without_child_block/0},
        {"Post commitments according to child hash", fun post_commitments/0},
        {"No commitments if stopped", fun no_commitments_if_stopped/0},
        {"Stopping and starting block production dictates commitments emmitting", fun block_production_dictates_commitments/0}
     ]}.


%%%===================================================================
%%% Test cases
%%%===================================================================

cache_all_above_child_height() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
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
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
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
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
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
            meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
            {ok, CachePid} = start_cache(StartHeight, CacheMaxSize),
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
            ExpectedTopHeight = ChildTop0 + StartHeight,
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

no_commitments_before_start() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    ChildTop0 = 0,
    Offset = 10,
    meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
    ParentTop = StartHeight - Offset,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, true),
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
            [] = collect_commitments(?ALICE),
            [] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            meck:reset(aec_parent_connector),
            ok
        end,
        lists:seq(0, Offset - 1)),
    ?TEST_MODULE:stop(),
    ok.

do_not_post_commitments_without_child_block() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    ChildTop0 = 0,
    meck:expect(aec_chain, top_header, fun() -> header(ChildTop0) end),
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(StartHeight),
    {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, true),
    ParentHeight0 = StartHeight + 1,
    set_parent_chain_top(ParentHeight0),
    ?TEST_MODULE:post_block(block_by_height(ParentHeight0)),
    timer:sleep(10),
    {ok, #{ child_start_height := StartHeight,
            top_height         := _ParentHeight0,
            child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
    AliceGenesisCommitment = encode_commitment_btc(?ALICE, height_to_hash(0)),
    BobGenesisCommitment = encode_commitment_btc(?BOB, height_to_hash(0)),
    [AliceGenesisCommitment] = collect_commitments(?ALICE),
    [BobGenesisCommitment] = collect_commitments(?BOB),
    [] = collect_commitments(?CAROL),
    [] = collect_commitments(?DAVE),
    meck:reset(aec_parent_connector),
    %% populate the cache and start making commitments
    lists:foreach(
        fun(Offset) ->
            ParentHeight = StartHeight + Offset,
            set_parent_chain_top(ParentHeight),
            Block = block_by_height(ParentHeight),
            ?TEST_MODULE:post_block(Block),
            timer:sleep(10),
            %% ensure that the node is up to date with the parent chain
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ParentHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            [] = collect_commitments(?ALICE),
            [] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            meck:reset(aec_parent_connector),
            ok
        end,
        lists:seq(2, 10)),
    ?TEST_MODULE:stop(),
    ok.

post_commitments() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    ChildTop = 10,
    meck:expect(aec_chain, top_header, fun() -> header(ChildTop) end),
    ParentTop = StartHeight,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, CachePid} = start_cache(StartHeight, CacheMaxSize, true),
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
            Hash = height_to_hash(ChildTop1),
            AliceCommitment =  aec_parent_chain_block:encode_commitment_btc(?ALICE, Hash, ?NETWORK_ID),
            [AliceCommitment] = collect_commitments(?ALICE),
            BobCommitment = aec_parent_chain_block:encode_commitment_btc(?BOB, Hash, ?NETWORK_ID),
            [BobCommitment] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    ?TEST_MODULE:stop(),
    ok.

no_commitments_if_stopped() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    ChildTop = 10,
    meck:expect(aec_chain, top_header, fun() -> header(ChildTop) end),
    ParentTop = StartHeight,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, CachePid} = start_cache(StartHeight, CacheMaxSize, false),
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
            [] = collect_commitments(?ALICE),
            [] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    ?TEST_MODULE:stop(),
    ok.

block_production_dictates_commitments() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    ChildTop = 10,
    meck:expect(aec_chain, top_header, fun() -> header(ChildTop) end),
    ParentTop = StartHeight,
    expect_stakers([?ALICE, ?BOB, ?CAROL]),
    expect_keys([?ALICE, ?BOB]),
    set_parent_chain_top(ParentTop),
    {ok, CachePid} = start_cache(StartHeight, CacheMaxSize, true),
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
            Hash = height_to_hash(ChildTop1),
            AliceCommitment = aec_parent_chain_block:encode_commitment_btc(?ALICE, Hash, ?NETWORK_ID),
            [AliceCommitment] = collect_commitments(?ALICE),
            BobCommitment = aec_parent_chain_block:encode_commitment_btc(?BOB, Hash, ?NETWORK_ID),
            [BobCommitment] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    %% stop block production
    ParentTop1 = ParentTop + 20,
    CachePid ! {gproc_ps_event, stop_mining, unused},
    lists:foreach(
        fun(Idx) ->
            ParentHeight = ParentTop1 + Idx,
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
                    child_top_height   := _ChildTop1}} = ?TEST_MODULE:get_state(),
            [] = collect_commitments(?ALICE),
            [] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    %% start block production
    ParentTop2 = ParentTop1 + 20,
    CachePid ! {gproc_ps_event, start_mining, unused},
    lists:foreach(
        fun(Idx) ->
            ParentHeight = ParentTop2 + Idx,
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
            Hash = height_to_hash(ChildTop1),
            AliceCommitment = aec_parent_chain_block:encode_commitment_btc(?ALICE, Hash, ?NETWORK_ID),
            [AliceCommitment] = collect_commitments(?ALICE),
            BobCommitment = aec_parent_chain_block:encode_commitment_btc(?BOB, Hash, ?NETWORK_ID),
            [BobCommitment] = collect_commitments(?BOB),
            [] = collect_commitments(?CAROL),
            [] = collect_commitments(?DAVE),
            ok
        end,
        lists:seq(0, 20)),
    ?TEST_MODULE:stop(),
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================

start_cache(StartHeight, MaxSize) ->
    start_cache(StartHeight, MaxSize, false).

start_cache(StartHeight, MaxSize, IsPublishingCommitments) ->
    Args = [StartHeight, MaxSize, true, IsPublishingCommitments],
    gen_server:start_link({local, ?TEST_MODULE}, ?TEST_MODULE, Args, []).

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
    ok.

unmock_stakers() ->
    meck:unload(aec_consensus_hc),
    ok.

mock_sign_module() ->
    meck:new(?SIGN_MODULE, []),
    meck:expect(?SIGN_MODULE, sign_binary,
        fun(Bin, Pubkey) when Pubkey =:= ?ALICE;
                              Pubkey =:= ?BOB;
                              Pubkey =:= ?CAROL;
                              Pubkey =:= ?DAVE
                              ->
                Privkey =
                    case Pubkey of
                        ?ALICE -> ?ALICE_PRIV;
                        ?BOB -> ?BOB_PRIV;
                        ?CAROL -> ?CAROL_PRIV;
                        ?DAVE-> ?DAVE_PRIV
                    end,
                try enacl:sign_detached(Bin, Privkey) of
                    Signature -> {ok, Signature}
                catch
                    _Type:_What -> {error, failed_sign}
                end;
           (_Bin, _Pubkey) ->
            Signature = <<123453:64/unit:8>>,
            {ok, Signature}
        end).

unmock_sign_module() ->
    meck:unload(?SIGN_MODULE).

mock_network_id(NetworkId) ->
    meck:new(aec_governance, [passthrough]),
    meck:expect(aec_governance, get_network_id,
                fun() -> NetworkId end).

unmock_network_id() ->
    meck:unload(aec_governance).

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

assert_child_cache_consistency(#{ child_start_height := StartHeight,
                                  child_top_height   := ChildTop,
                                  blocks             := Blocks,
                                  max_size           := CacheMaxSize,
                                  top_height         := TopHeight}) ->
    ?assertEqual(CacheMaxSize, map_size(Blocks)),
    CacheExpectedStart = min(ChildTop + StartHeight, TopHeight - CacheMaxSize + 1),
    ?assertEqual(CacheExpectedStart, lists:min(maps:keys(Blocks))),
    CacheExpectedEnd = CacheExpectedStart + CacheMaxSize - 1,
    ?assertEqual(CacheExpectedEnd, lists:max(maps:keys(Blocks))),
    lists:foreach(
        fun(Height) ->
            {true, Height} = {maps:is_key(Height, Blocks), Height},
            IsMature = TopHeight >= Height,
            Block = block_by_height(Height),
            case ?TEST_MODULE:get_block_by_height(Height) of
                {ok, Block} when IsMature -> ok;
                {error, {not_enough_confirmations, Block}} -> ok
            end,
            ok
        end,
        lists:seq(CacheExpectedEnd, CacheExpectedEnd)),
    ok.

collect_commitments(Staker) ->
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

mock_commitments_list(_BlockHashesMap) ->
    meck:expect(aec_parent_connector, request_block_by_height,
                fun(Height) ->
                    spawn(
                        fun() ->
                            Block = block_by_height(Height),
                            ?TEST_MODULE:post_block(Block)
                        end)
            end).

mock_commitments_list(all, L) ->
    meck:expect(aec_parent_connector, request_block_by_height,
                fun(Height) ->
                    spawn(
                        fun() ->
                            Block0 = block_by_height(Height),
                            Block = aec_parent_chain_block:set_commitments(Block0, L),
                            ?TEST_MODULE:post_block(Block)
                        end)
            end).

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

encode_commitment_btc(PubKey, Hash) ->
    PubKeyFate = aeb_fate_encoding:serialize(aeb_fate_data:make_address(PubKey)),
    <<StakerHash:8/binary, _/binary>> = aec_hash:sha256_hash(PubKeyFate),
    <<TopKeyHash:7/binary, _/binary>> = aec_hash:sha256_hash(Hash),
    NetworkIdPadded = aec_parent_chain_block:encode_network_id(?NETWORK_ID),
    Msg = aec_hash:sha256_hash(<<Hash/binary, NetworkIdPadded/binary>>),
    {ok, <<Signature:64/binary>>} = ?SIGN_MODULE:sign_binary(Msg, PubKey),
    <<?HC_COMMITMENT_BTC, Signature/binary, StakerHash/binary, TopKeyHash/binary>>.

