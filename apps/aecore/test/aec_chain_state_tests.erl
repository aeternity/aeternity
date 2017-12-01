%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_chain_state
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

-import(aec_test_utils,
        [ extend_block_chain_by_targets_with_nonce_and_coinbase/3
        , aec_keys_setup/0
        , aec_keys_cleanup/1
        ]).

-import(aec_chain_state,
        [ difficulty_at_hash/2
        , difficulty_at_top_block/1
        , difficulty_at_top_header/1
        , get_block/2
        , get_block_by_height/2
        , get_header/2
        , get_header_by_height/2
        , hash_is_connected_to_genesis/2
        , insert_block/2
        , insert_header/2
        , top_block/1
        , top_block_hash/1
        , top_header/1
        , top_header_hash/1
        ]).

-define(compareBlockResults(B1, B2),
        ?assertEqual(aec_blocks:serialize_for_network(element(2,B1)),
                     aec_blocks:serialize_for_network(element(2,B2)))).

-define(FACTOR, 1000000000).
-define(GENESIS_TARGET, 553713663).

%% GENESIS DIFFICULTY = float(trunc(?FACTOR / 553713663))
-define(GENESIS_DIFFICULTY, 1.0).

-define(assertDifficultyEq(__X__, __Y__),
        ?assertEqual(trunc(1000 *(__X__)),
                     trunc(1000*(__Y__)))).

%%%===================================================================
%%% Test cases
%%%===================================================================

%%%===================================================================
%%% Basic access tests

basic_access_test_() ->
    {setup,
     fun aec_test_utils:aec_keys_setup/0,
     fun aec_test_utils:aec_keys_cleanup/1,
     [ {"Access for header chain", fun basic_access_test_header_chain/0}
     , {"Access for block chain", fun basic_access_test_block_chain/0}
     , {"Access for missing blocks", fun basic_access_missing_blocks/0}
     ]}.

basic_access_test_header_chain() ->
    %% Create a chain that we are going to use.
    Chain = aec_test_utils:gen_block_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(B) || B <- Chain],

    %% Add a couple of headers - not blocks - to the chain.
    NewState = new_state(),
    {ok, State0} = insert_header(BH0, NewState),
    {ok, State1} = insert_header(BH1, State0),
    {ok, State2} = insert_header(BH2, State1),

    %% Check highest header.
    ?assertEqual(BH1, top_header(State1)),
    ?assertEqual(BH2, top_header(State2)),

    %% Check by hash.
    ?assertEqual({ok, BH0}, get_header(B0H, State2)),
    ?assertEqual({ok, BH1}, get_header(B1H, State2)),
    ?assertEqual({ok, BH2}, get_header(B2H, State2)),
    ?assertEqual(error, get_header(B1H, State0)),
    ?assertEqual(error, get_header(B2H, State0)),

    %% Check by height.
    ?assertEqual({ok, BH0}, get_header_by_height(0, State2)),
    ?assertEqual({ok, BH1}, get_header_by_height(1, State2)),
    ?assertEqual({ok, BH2}, get_header_by_height(2, State2)),
    ?assertEqual({error, chain_too_short},
                 get_header_by_height(3, State2)),

    %% Test getting blocks for a header chain.
    %% We only have the block for the genesis.

    %% Get by hash
    ?assertMatch(error, get_block(B0H, State2)),
    ?assertEqual(error, get_block(B1H, State2)),
    ?assertEqual(error, get_block(B2H, State2)),

    %% Get by height
    ?assertMatch({error, block_not_found},
                 get_block_by_height(0, State2)),
    ?assertMatch({error, block_not_found},
                 get_block_by_height(1, State2)),
    ?assertMatch({error, block_not_found},
                 get_block_by_height(2, State2)),
    ?assertMatch({error, chain_too_short},
                 get_block_by_height(3, State2)).

basic_access_test_block_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_block_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Add a couple of blocks to the chain.
    NewState = new_state(),
    {ok, State0} = insert_block(B0, NewState),
    {ok, State1} = insert_block(B1, State0),
    {ok, State2} = insert_block(B2, State1),

    %% Check highest header.
    ?assertEqual(BH1, top_header(State1)),
    ?assertEqual(BH2, top_header(State2)),

    %% Check highest block
    ?compareBlockResults({ok, B0}, {ok, top_block(State0)}),
    ?compareBlockResults({ok, B1}, {ok, top_block(State1)}),
    ?compareBlockResults({ok, B2}, {ok, top_block(State2)}),

    %% Check by hash.
    ?assertEqual({ok, BH0}, get_header(B0H, State2)),
    ?assertEqual({ok, BH1}, get_header(B1H, State2)),
    ?assertEqual({ok, BH2}, get_header(B2H, State2)),
    ?assertEqual(error, get_header(B1H, State0)),
    ?assertEqual(error, get_header(B2H, State0)),
    ?compareBlockResults({ok, B0}, get_block(B0H, State2)),
    ?compareBlockResults({ok, B1}, get_block(B1H, State2)),
    ?compareBlockResults({ok, B2}, get_block(B2H, State2)),
    ?assertEqual(error, get_block(B1H, State0)),
    ?assertEqual(error, get_block(B2H, State0)),

    %% Check by height
    ?assertEqual({ok, BH0}, get_header_by_height(0, State2)),
    ?assertEqual({ok, BH1}, get_header_by_height(1, State2)),
    ?assertEqual({ok, BH2}, get_header_by_height(2, State2)),
    ?compareBlockResults({ok, B0}, get_block_by_height(0, State2)),
    ?compareBlockResults({ok, B1}, get_block_by_height(1, State2)),
    ?compareBlockResults({ok, B2}, get_block_by_height(2, State2)),
    ?assertEqual({error, chain_too_short}, get_block_by_height(3, State2)).

basic_access_missing_blocks() ->
    Chain = [B0, B1, B2] = aec_test_utils:gen_block_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Add some headers
    State0 = write_headers_to_chain([BH0, BH1], new_state()),
    ?assertEqual(ordsets:from_list([B0H, B1H]),
                 ordsets:from_list(aec_chain_state:get_missing_block_hashes(State0))),
    State1 = write_headers_to_chain([BH2], State0),
    ?assertEqual(ordsets:from_list([B0H, B1H, B2H]),
                 ordsets:from_list(aec_chain_state:get_missing_block_hashes(State1))),
    State2 = write_blocks_to_chain([B2], State1),
    ?assertEqual(ordsets:from_list([B0H, B1H]),
                 ordsets:from_list(aec_chain_state:get_missing_block_hashes(State2))),
    State3 = write_blocks_to_chain([B0], State2),
    ?assertEqual(ordsets:from_list([B1H]),
                 ordsets:from_list(aec_chain_state:get_missing_block_hashes(State3))),
    State4 = write_blocks_to_chain([B1], State3),
    ?assertEqual(ordsets:from_list([]),
                 ordsets:from_list(aec_chain_state:get_missing_block_hashes(State4))),
    ok.

%%%===================================================================
%%% GC tests

gc_test_() ->
    {foreach,
     fun setup_meck_and_keys/0,
     fun teardown_meck_and_keys/1,
     [{ gc_test_slogan(Length, Max, Interval, KeepAll)
      , gc_test_gen(Length, Max, Interval, KeepAll)}
      || {Length, Max, Interval, KeepAll} <- gc_test_params()
     ]}.

gc_test_gen(Length, Max, Interval, KeepAll) ->
    fun() -> gc_test_fun(Length, Max, Interval, KeepAll)end.

gc_test_slogan(Length, Max, Interval, KeepAll) ->
    S = io_lib:format("Create chain, test that only"
                      " the intended snapshots are kept."
                      " Length: ~w, Max: ~w, Interval: ~w, KeepAll: ~w",
                      [Length, Max, Interval, KeepAll]),
    lists:flatten(S).

gc_test_params() ->
    %% {Length, Max, Interval, KeepAll}
    [ {15, 10, 5, 2}
    , {4, 3, 1, 1}
    , {100, 30, 10, 5}
    ].

gc_test_fun(Length, Max, Interval, KeepAll) ->
    %% Generate blockchain and write it to the state.
    BC = aec_test_utils:gen_block_chain(Length),
    State1 = new_state(gc_opts(KeepAll, Max, Interval)),
    State2 = write_blocks_to_chain(BC, State1),

    %% Get the state trees that we would have persisted.
    Trees = aec_chain_state:get_state_trees_for_persistance(State2),

    %% Check that the top block hash is among the persisted.
    TopHash = top_block_hash(State2),
    ?assertMatch({TopHash, _}, lists:keyfind(TopHash, 1, Trees)),

    %% Compute the hashes that always should be persisted...
    KeepAllHashes =
        [block_hash(lists:nth(X, BC))
         || X <- lists:seq(Length - KeepAll + 1, Length)],
    %% ...and check that they are indeed persisted.
    ?assertEqual(lists:duplicate(length(KeepAllHashes), true),
                 [lists:keymember(X, 1, Trees)
                  || X <- KeepAllHashes]),

    %% Compute the hashes that should be sparsely persisted...
    KeepSparseHashes =
        [block_hash(lists:nth(X, BC))
         || X <- lists:seq(Length-KeepAll, Length-Max,-Interval)],
    %% ...and check that they are indeed persisted.
    ?assertEqual(lists:duplicate(length(KeepSparseHashes), true),
                 [lists:keymember(X, 1, Trees)
                  || X <- KeepSparseHashes]),

    %% Check that we have have covered all the hashes that
    %% we will persist (and that the hashes were unique).
    HashSet = ordsets:from_list(KeepAllHashes ++ KeepSparseHashes),
    ?assertEqual(ordsets:size(HashSet), length(Trees)),
    ?assertEqual(ordsets:size(HashSet),
                 length(KeepSparseHashes) + length(KeepAllHashes)),
    ok.

%%%===================================================================
%%% Out of order tests

out_of_order_test_() ->
    {setup,
     fun aec_test_utils:aec_keys_setup/0,
     fun aec_test_utils:aec_keys_cleanup/1,
     [ {"Out of order insert of header chain",
        fun out_of_order_test_header_chain/0}
     , {"Out of order insert of block chain",
        fun out_of_order_test_block_chain/0}
     , {"Out of order insert of mixed chain",
        fun out_of_order_test_mixed_chain/0}
     , {"Out of order check if a block is connected",
        fun out_of_order_test_connected/0}
     ]}.

out_of_order_test_header_chain() ->
    %% Create a chain that we are going to use.
    Chain = aec_test_utils:gen_block_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H,_B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Insert the headers in different order and assert
    %% that the end result is the same
    S = new_state(),
    ?assertEqual(B2H, top_header_hash(write_headers_to_chain([BH0, BH1, BH2], S))),
    ?assertEqual(B2H, top_header_hash(write_headers_to_chain([BH0, BH2, BH1], S))),
    ?assertEqual(B2H, top_header_hash(write_headers_to_chain([BH1, BH0, BH2], S))),
    ?assertEqual(B2H, top_header_hash(write_headers_to_chain([BH1, BH2, BH0], S))),
    ?assertEqual(B2H, top_header_hash(write_headers_to_chain([BH2, BH0, BH1], S))),
    ?assertEqual(B2H, top_header_hash(write_headers_to_chain([BH2, BH1, BH0], S))),

    %% Check that the top header is not moving if the chain is not complete.
    ?assertEqual(B0H, top_header_hash(write_headers_to_chain([BH0, BH2], S))),
    ?assertEqual(undefined, top_header_hash(write_headers_to_chain([BH1, BH2], S))),
    ok.

out_of_order_test_block_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_block_chain(3),
    [B0H,_B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Insert the headers in different order and assert
    %% that the end result is the same
    S = new_state(),
    ?assertEqual(B2H, top_header_hash(write_blocks_to_chain([B0, B1, B2], S))),
    ?assertEqual(B2H, top_header_hash(write_blocks_to_chain([B0, B2, B1], S))),
    ?assertEqual(B2H, top_header_hash(write_blocks_to_chain([B1, B0, B2], S))),
    ?assertEqual(B2H, top_header_hash(write_blocks_to_chain([B1, B2, B0], S))),
    ?assertEqual(B2H, top_header_hash(write_blocks_to_chain([B2, B0, B1], S))),
    ?assertEqual(B2H, top_header_hash(write_blocks_to_chain([B2, B1, B0], S))),

    %% Check that the top block is not moving if the chain is not complete.
    ?assertEqual(B0H, top_block_hash(write_blocks_to_chain([B0, B2], S))),
    ?assertEqual(undefined, top_block_hash(write_blocks_to_chain([B1, B2], S))),
    ok.

out_of_order_test_mixed_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_block_chain(3),
    [BH0, BH1,_BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Check that the top header hash and the top block hash is moving
    %% in the correct way.
    {ok, State1} = insert_header(BH0, new_state()),
    ?assertEqual(B0H, top_header_hash(State1)),
    ?assertEqual(undefined, top_block_hash(State1)),
    {ok, State2} = insert_header(BH1, State1),
    ?assertEqual(B1H, top_header_hash(State2)),
    ?assertEqual(undefined, top_block_hash(State2)),
    {ok, State3} = insert_block(B0, State2),
    ?assertEqual(B1H, top_header_hash(State3)),
    ?assertEqual(B0H, top_block_hash(State3)),
    {ok, State4} = insert_block(B2, State3),
    ?assertEqual(B2H, top_header_hash(State4)),
    ?assertEqual(B0H, top_block_hash(State4)),
    {ok, State5} = insert_block(B1, State4),
    ?assertEqual(B2H, top_header_hash(State5)),
    ?assertEqual(B2H, top_block_hash(State5)),
    ok.

out_of_order_test_connected() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2, B3] = aec_test_utils:gen_block_chain(4),
    [B0H, B1H, B2H, B3H] = [block_hash(H) || H <- Chain],

    %% Insert a broken chain and test connectivity
    S1 = write_blocks_to_chain([B0, B1, B3], new_state()),
    ?assertEqual(true, hash_is_connected_to_genesis(B0H, S1)),
    ?assertEqual(true, hash_is_connected_to_genesis(B1H, S1)),
    ?assertEqual(false, hash_is_connected_to_genesis(B2H, S1)),
    ?assertEqual(false, hash_is_connected_to_genesis(B3H, S1)),

    %% Write the missing block
    S2 = write_blocks_to_chain([B2], S1),
    ?assertEqual(true, hash_is_connected_to_genesis(B0H, S2)),
    ?assertEqual(true, hash_is_connected_to_genesis(B1H, S2)),
    ?assertEqual(true, hash_is_connected_to_genesis(B2H, S2)),
    ?assertEqual(true, hash_is_connected_to_genesis(B3H, S2)),
    ok.

%%%===================================================================
%%% Broken chain tests

broken_chain_test_() ->
    {foreach,
     fun setup_meck_and_keys/0,
     fun teardown_meck_and_keys/1,
     [{"Test that an invalid block in a different fork is validated when that "
       "fork tries to take over as main chain",
       fun broken_chain_postponed_validation/0},
      {"Add a block that points to the wrong place in the main chain"
       " because of its height",
       fun broken_chain_wrong_height/0},
      {"Add a block with the wrong state hash",
       fun broken_chain_wrong_state_hash/0}
     ]}.

broken_chain_postponed_validation() ->
    MainBC = gen_block_chain_by_target([?GENESIS_TARGET, 2, 5], 111),
    AltChain = [B0, B1, B2, B3] = gen_block_chain_by_target([?GENESIS_TARGET, 2, 1], 222),

    %% Assert that we are creating a fork
    ?assertNotEqual(MainBC, AltChain),
    ?assertEqual(hd(MainBC), B0),

    %% Insert the main chain.
    State0 = write_blocks_to_chain(MainBC, new_state()),

    %% Assert that the fork would have taken over if it was ok.
    State1 = write_blocks_to_chain(AltChain, State0),
    ?assertEqual(aec_blocks:hash_internal_representation(B3),
                 aec_blocks:hash_internal_representation(top_block(State1))),

    %% Insert the first block of the fork
    {ok, State2} = insert_block(B1, State0),

    %% Insert the second block of the fork with a bad root hash
    Bad = B2#block{root_hash = <<"I'm not really a hash">>},
    {ok, State3} = insert_block(Bad, State2),

    {ok, Hash} = aec_blocks:hash_internal_representation(Bad),
    B3Bad = B3#block{prev_hash = Hash},

    %% Check that the fork is not taking over
    ?assertEqual(top_block(State0), top_block(State3)),
    ?assertEqual(top_block_hash(State3), block_hash(lists:last(MainBC))),

    %% When the fork takes over, the bad block should be found invalid.
    ?assertMatch({error, _}, insert_block(B3Bad, State3)),
    ok.


broken_chain_wrong_height() ->
    %% Create a chain that we are going to use.
    [B0, B1, B2] = aec_test_utils:gen_block_chain(3),

    %% Insert up to last block.
    State0 = write_blocks_to_chain([B0, B1], new_state()),

    %% Check that we can insert the unmodified last block
    ?assertMatch({ok, _}, insert_block(B2, State0)),

    %% Change the height of the last block to an incompatible height.
    ?assertEqual({error, height_inconsistent_with_previous_hash},
                 insert_block(B2#block{height = 4}, State0)),
    ?assertEqual({error, height_inconsistent_with_previous_hash},
                 insert_block(B2#block{height = 1}, State0)),
    ok.

broken_chain_wrong_state_hash() ->
    [B0, B1, B2] = aec_test_utils:gen_block_chain(3),

    %% Insert up to last block.
    State0 = write_blocks_to_chain([B0, B1], new_state()),

    %% Check that we can insert the unmodified last block
    ?assertMatch({ok, _}, insert_block(B2, State0)),

    %% Change the state hash to something wrong.
    Hash = B2#block.root_hash,
    Bogus = case <<1:(bit_size(Hash))>> =:= Hash of
                true  -> <<0:(bit_size(Hash))>>;
                false -> <<1:(bit_size(Hash))>>
            end,
    ?assertNotEqual(Hash, Bogus),
    ?assertMatch({error, {root_hash_mismatch, _, _}},
                 insert_block(B2#block{root_hash = Bogus}, State0)),
    ok.

%%%===================================================================
%%% Target validation tests

target_validation_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:mock_difficulty_as_target(),
             meck:new(aec_governance, [passthrough]),
             meck:new(aec_pow, [passthrough]),
             meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 3),
             meck:expect(aec_governance, expected_block_mine_rate, 0, 3000000), %% 50 mins
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:unmock_difficulty_as_target(),
             meck:unload(aec_governance),
             meck:unload(aec_pow),
             aec_test_utils:aec_keys_cleanup(TmpDir)
     end,
     [{"Ensure target is same as genesis block target"
       " in first (blocks_to_check_difficulty_count + 1) headers/blocks",
       fun constant_target_at_the_beginning_of_the_chain/0},
      {"Ensure target is verified based on calculations"
       " after (blocks_to_check_difficulty_count + 1) headers/blocks",
       fun target_verified_based_on_calculations/0},
      {"Test target is verified even for blocks coming"
       " in different order, hence fork is rejected",
       fun test_postponed_target_verification/0}
     ]}.

constant_target_at_the_beginning_of_the_chain() ->
    Chain = [_,_,_,B3,_] = gen_block_chain_by_target(
                             [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 222),
    [BH0, BH1, BH2, BH3, BH4] = [aec_blocks:to_header(B) || B <- Chain],

    %% Insert genesis
    {ok, S0} = insert_header(BH0, new_state()),

    %% Do not allow too low target
    BH1TooLowTarget = BH1#header{target = trunc(?GENESIS_TARGET / 2)},
    ?assertMatch({error, {target_not_equal_to_parent, _, _}},
                 insert_header(BH1TooLowTarget, S0)),

    {ok, S1} = insert_header(BH1, S0),
    {ok, S2} = insert_header(BH2, S1),

    %% Do not allow too high target
    BH3TooHighTarget = BH3#header{target = 2 * ?GENESIS_TARGET},
    ?assertMatch({error, {target_not_equal_to_parent, _, _}},
                 insert_header(BH3TooHighTarget, S2)),

    {ok, S3} = insert_header(BH3, S2),

    %% target_not_equal_to_parent does not kick in for header with height = 4
    %% For header with height 4, header with height 1 is taken for difficulty recalculations
    ?assertNotMatch({error, {target_not_equal_to_parent, _, _}},
                    insert_header(BH4, S3)),
    ?assertMatch({error, {target_too_high, _, _}},
                 insert_header(BH4, S3)),

    ?assertEqual(block_hash(B3), top_header_hash(S3)),
    ok.

target_verified_based_on_calculations() ->
    Chain = gen_block_chain_by_target(
              [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, 2, 1], 222),
    [BH0, BH1, BH2, BH3, BH4, BH5] = [aec_blocks:to_header(B) || B <- Chain],

    meck:expect(aec_pow, recalculate_target,
                fun(?GENESIS_TARGET, _, _) -> 5; %% Expect target of block 4 to be <=5
                   (2, _, _) -> 4
                end),

    {ok, S0} = insert_header(BH0, new_state()),
    {ok, S1} = insert_header(BH1, S0),
    {ok, S2} = insert_header(BH2, S1),
    {ok, S3} = insert_header(BH3, S2),

    %% Try to insert header with height=4 with target=8
    BadBH4 = BH4#header{target = 8},
    ?assertMatch({error, {target_too_high, _, _}},
                 insert_header(BadBH4, S3)),

    %% Insert header with height=4 with expected target
    {ok, S4} = insert_header(BH4, S3),
    {ok, S5} = insert_header(BH5, S4),

    ?assertEqual(block_hash(lists:last(Chain)), top_header_hash(S5)),
    ok.

test_postponed_target_verification() ->
    MainBC = gen_block_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET,
                                        4, 6, 5], 111),
    AltChain = [_, B1, B2, B3, B4, B5, B6] = gen_block_chain_by_target(
                                               [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET,
                                                4, 100, 1], 222),
    %% Assert that we are creating a fork
    ?assertNotEqual(MainBC, AltChain),

    meck:expect(aec_pow, recalculate_target,
                fun(4, _, _) -> 10; %% Expect target of block 5 to be <=10
                   (_, _, _) -> 1000
                end),

    %% Insert the main chain
    S0 = write_blocks_to_chain(MainBC, new_state()),
    ?assertEqual(aec_blocks:hash_internal_representation(lists:last(MainBC)),
                 {ok, top_block_hash(S0)}),

    %% Insert all blocks of alt chain chain except B4 (which prevents from target validation)
    {ok, S1} = insert_block(B1, S0),
    {ok, S2} = insert_block(B2, S1),
    {ok, S3} = insert_block(B3, S2),
    {ok, S4} = insert_block(B5, S3),
    {ok, S5} = insert_block(B6, S4),

    %% Insert B4, which should make AltChain take over,
    %% but already inserted B5 has too high target (it was mined too easily)
    ?assertMatch({error, {target_too_high, _, _}},
                 insert_block(B4, S5)),

    %% Assert alt chain did not take over
    ?assertEqual(aec_blocks:hash_internal_representation(lists:last(MainBC)),
                 {ok, top_block_hash(S5)}),

    ok.


%%%===================================================================
%%% Total difficulty test

total_difficulty_test_() ->
    {foreach,
     fun setup_meck_and_keys/0,
     fun teardown_meck_and_keys/1,
     [{"Get work in chain of only genesis",
       fun total_difficulty_only_genesis/0},
      {"Get work in header chain",
       fun total_difficulty_in_chain/0
      }]
    }.

total_difficulty_only_genesis() ->
    {ok, State0} = insert_block(genesis_block(), new_state()),
    {ok, Difficulty} = difficulty_at_top_header(State0),
    ?assertDifficultyEq(?GENESIS_DIFFICULTY, Difficulty).

total_difficulty_in_chain() ->
    %% In order to pass target validation, block after genesis has to have the same target as genesis block
    [B0, B1, B2, B3, B4] = Chain = gen_block_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    State = write_blocks_to_chain(Chain, new_state()),
    {ok, DiffTopH} = difficulty_at_top_header(State),
    {ok, DiffTopB} = difficulty_at_top_block(State),
    {ok, Diff0} = difficulty_at_hash(block_hash(B0), State),
    {ok, Diff1} = difficulty_at_hash(block_hash(B1), State),
    {ok, Diff2} = difficulty_at_hash(block_hash(B2), State),
    {ok, Diff3} = difficulty_at_hash(block_hash(B3), State),
    {ok, Diff4} = difficulty_at_hash(block_hash(B4), State),

    %% Mecked difficulty = ?FACTOR / target
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 3 * ?FACTOR, DiffTopH),
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 3 * ?FACTOR, DiffTopB),
    ?assertDifficultyEq(1 * ?GENESIS_DIFFICULTY + 0          , Diff0),
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 0          , Diff1),
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 1 * ?FACTOR, Diff2),
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 2 * ?FACTOR, Diff3),
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 3 * ?FACTOR, Diff4),

    ok.

%%%===================================================================
%%% Forking tests

forking_test_() ->
    {foreach,
     fun setup_meck_and_keys/0,
     fun teardown_meck_and_keys/1,
     [ {"Fork on genesis", fun fork_on_genesis/0}
     , {"Fork on shorter chain because of difficulty", fun fork_on_shorter/0}
     , {"Fork on last block", fun fork_on_last_block/0}
     , {"Fork and out of order", fun fork_out_of_order/0}
     ]}.

fork_on_genesis() ->
    EasyChain = gen_block_chain_by_target([?GENESIS_TARGET, 2, 2, 2], 111),
    HardChain = gen_block_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    fork_common(EasyChain, HardChain).

fork_on_last_block() ->
    CommonChain = gen_block_chain_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = extend_chain(CommonChain, [2], 111),
    HardChain = extend_chain(CommonChain, [1], 222),
    fork_common(EasyChain, HardChain).

fork_on_shorter() ->
    EasyChain = gen_block_chain_by_target([?GENESIS_TARGET, 2, 2, 4], 111),
    HardChain = gen_block_chain_by_target([?GENESIS_TARGET, 1, 1], 111),
    fork_common(EasyChain, HardChain).

fork_common(EasyChain, HardChain) ->
    TopHashEasy = block_hash(lists:last(EasyChain)),
    TopHashHard = block_hash(lists:last(HardChain)),
    %% Insert blocks
    ok = fork_common_block(EasyChain, TopHashEasy, HardChain, TopHashHard),
    %% Out of order
    ok = fork_common_block(lists:reverse(EasyChain), TopHashEasy,
                           lists:reverse(HardChain), TopHashHard),
    %% Insert headers
    EasyHeaders = [aec_blocks:to_header(X) || X <- EasyChain],
    HardHeaders = [aec_blocks:to_header(X) || X <- HardChain],
    ok = fork_common_headers(EasyHeaders, TopHashEasy, HardHeaders, TopHashHard),
    %% Out of order
    ok = fork_common_headers(lists:reverse(EasyHeaders), TopHashEasy,
                             lists:reverse(HardHeaders), TopHashHard),

    ok.

fork_common_block(EasyChain, TopHashEasy, HardChain, TopHashHard) ->
    InitState = new_state(),

    %% The second chain should take over
    State1 = write_blocks_to_chain(EasyChain, InitState),
    State2 = write_blocks_to_chain(HardChain, State1),
    ?assertEqual(TopHashEasy, top_block_hash(State1)),
    ?assertEqual(TopHashHard, top_block_hash(State2)),

    %% The second chain should not take over
    State3 = write_blocks_to_chain(HardChain, InitState),
    State4 = write_blocks_to_chain(EasyChain, State3),
    ?assertEqual(TopHashHard, top_block_hash(State3)),
    ?assertEqual(TopHashHard, top_block_hash(State4)),
    ok.

fork_common_headers(EasyChain, TopHashEasy, HardChain, TopHashHard) ->
    InitState = new_state(),

    %% The second chain should take over
    State1 = write_headers_to_chain(EasyChain, InitState),
    State2 = write_headers_to_chain(HardChain, State1),
    ?assertEqual(TopHashEasy, top_header_hash(State1)),
    ?assertEqual(TopHashHard, top_header_hash(State2)),

    %% The second chain should not take over
    State3 = write_headers_to_chain(HardChain, InitState),
    State4 = write_headers_to_chain(EasyChain, State3),
    ?assertEqual(TopHashHard, top_header_hash(State3)),
    ?assertEqual(TopHashHard, top_header_hash(State4)),
    ok.

fork_out_of_order() ->
    CommonChain = gen_block_chain_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = extend_chain(CommonChain, [2], 111),
    HardChain = extend_chain(CommonChain, [1], 222),

    %% Add the chain with the fork node as the last entry.
    InitState = new_state(),

    State1 = write_blocks_to_chain(lists:droplast(CommonChain), InitState),
    {ok, State2} = insert_block(lists:last(EasyChain), State1),
    {ok, State3} = insert_block(lists:last(HardChain), State2),

    %% The last block to enter is the last common node.
    {ok, State4} = insert_block(lists:last(CommonChain), State3),
    ?assertEqual(block_hash(lists:last(HardChain)), top_block_hash(State4)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_state() ->
    aec_chain_state:new().

new_state(Opts) ->
    aec_chain_state:new(Opts).

setup_meck_and_keys() ->
    aec_test_utils:mock_difficulty_as_target(),
    aec_test_utils:mock_block_target_validation(),
    aec_test_utils:aec_keys_setup().

teardown_meck_and_keys(TmpDir) ->
    aec_test_utils:unmock_difficulty_as_target(),
    aec_test_utils:unmock_block_target_validation(),
    aec_test_utils:aec_keys_cleanup(TmpDir).

write_blocks_to_chain([H|T], State) ->
    {ok, State1} = insert_block(H, State),
    write_blocks_to_chain(T, State1);
write_blocks_to_chain([], State) ->
    State.

write_headers_to_chain([H|T], State) ->
    {ok, State1} = insert_header(H, State),
    write_headers_to_chain(T, State1);
write_headers_to_chain([], State) ->
    State.

gc_opts(KeepAll, Max, Interval) ->
    #{ max_snapshot_height => Max
     , sparse_snapshots_interval => Interval
     , keep_all_snapshots_height => KeepAll
     }.

gen_block_chain_by_target(Targets, Nonce) ->
    B0 = genesis_block(),
    [B0 | extend_block_chain_by_targets_with_nonce_and_coinbase(B0, Targets, Nonce)].

extend_chain(Base, Targets, Nonce) ->
    B = lists:last(Base),
    Base ++
        extend_block_chain_by_targets_with_nonce_and_coinbase(B, Targets, Nonce).

genesis_block() ->
    aec_block_genesis:genesis_block().

block_hash(Block) ->
    {ok, H} = aec_blocks:hash_internal_representation(Block),
    H.
