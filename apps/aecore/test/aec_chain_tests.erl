%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

-import(aec_test_utils,
        [ extend_block_chain_with_state/3
        , blocks_only_chain/1
        , genesis_block/0
        , genesis_block_with_state/0
        ]).

-import(aec_chain_state,
        [ insert_block/1
        , insert_header/1
        ]).

-import(aec_chain,
        [ difficulty_at_hash/1
        , difficulty_at_top_block/0
        , difficulty_at_top_header/0
        , get_block/1
        , get_block_by_height/1
        , get_block_state/1
        , get_header/1
        , get_header_by_height/1
        , get_missing_block_hashes/0
        , hash_is_connected_to_genesis/1
        , top_block/0
        , top_block_hash/0
        , top_header/0
        , top_header_hash/0
        ]).

-define(compareBlockResults(B1, B2),
        ?assertEqual(aec_blocks:serialize_to_map(element(2,B1)),
                     aec_blocks:serialize_to_map(element(2,B2)))).

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
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db()
     end,
     [ {"Access for header chain", fun basic_access_test_header_chain/0}
     , {"Access for block chain", fun basic_access_test_block_chain/0}
     , {"Access for missing blocks", fun basic_access_missing_blocks/0}
     ]}.

basic_access_test_header_chain() ->
    %% Create a chain that we are going to use.
    Chain = aec_test_utils:gen_blocks_only_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(B) || B <- Chain],

    %% Add a couple of headers - not blocks - to the chain.
    ok = insert_header(BH0),
    ?assertEqual(error, get_header(B1H)),
    ?assertEqual(error, get_header(B2H)),

    ok = insert_header(BH1),
    ?assertEqual(BH1, top_header()),

    ok = insert_header(BH2),
    ?assertEqual(BH2, top_header()),

    %% Check by hash.
    ?assertEqual({ok, BH0}, get_header(B0H)),
    ?assertEqual({ok, BH1}, get_header(B1H)),
    ?assertEqual({ok, BH2}, get_header(B2H)),

    %% Check by height.
    ?assertEqual({ok, BH0}, get_header_by_height(0)),
    ?assertEqual({ok, BH1}, get_header_by_height(1)),
    ?assertEqual({ok, BH2}, get_header_by_height(2)),
    ?assertEqual({error, chain_too_short}, get_header_by_height(3)),

    %% Test getting blocks for a header chain.
    %% We only have the block for the genesis.

    %% Get by hash
    ?assertMatch(error, get_block(B0H)),
    ?assertEqual(error, get_block(B1H)),
    ?assertEqual(error, get_block(B2H)),

    %% Get by height
    ?assertMatch({error, block_not_found}, get_block_by_height(0)),
    ?assertMatch({error, block_not_found}, get_block_by_height(1)),
    ?assertMatch({error, block_not_found}, get_block_by_height(2)),
    ?assertMatch({error, chain_too_short}, get_block_by_height(3)).

basic_access_test_block_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(B) || B <- Chain],

    %% Add a couple of blocks to the chain.
    ok = insert_block(B0),

    ?assertEqual(error, get_header(B1H)),
    ?assertEqual(error, get_header(B2H)),
    ?assertEqual(error, get_block(B1H)),
    ?assertEqual(error, get_block(B2H)),

    ?compareBlockResults({ok, B0}, {ok, top_block()}),
    ok = insert_block(B1),
    ?assertEqual(BH1, top_header()),
    ?compareBlockResults({ok, B1}, {ok, top_block()}),

    ok = insert_block(B2),
    ?assertEqual(BH2, top_header()),
    ?compareBlockResults({ok, B2}, {ok, top_block()}),

    %% Check by hash.
    ?assertEqual({ok, BH0}, get_header(B0H)),
    ?assertEqual({ok, BH1}, get_header(B1H)),
    ?assertEqual({ok, BH2}, get_header(B2H)),
    ?compareBlockResults({ok, B0}, get_block(B0H)),
    ?compareBlockResults({ok, B1}, get_block(B1H)),
    ?compareBlockResults({ok, B2}, get_block(B2H)),

    %% Check by height
    ?assertEqual({ok, BH0}, get_header_by_height(0)),
    ?assertEqual({ok, BH1}, get_header_by_height(1)),
    ?assertEqual({ok, BH2}, get_header_by_height(2)),
    ?compareBlockResults({ok, B0}, get_block_by_height(0)),
    ?compareBlockResults({ok, B1}, get_block_by_height(1)),
    ?compareBlockResults({ok, B2}, get_block_by_height(2)),
    ?assertEqual({error, chain_too_short}, get_block_by_height(3)).

basic_access_missing_blocks() ->
    Chain = [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(B) || B <- Chain],

    %% Add some headers
    ok = write_headers_to_chain([BH0, BH1]),
    ?assertEqual(ordsets:from_list([B0H, B1H]),
                 ordsets:from_list(get_missing_block_hashes())),
    ok = write_headers_to_chain([BH2]),
    ?assertEqual(ordsets:from_list([B0H, B1H, B2H]),
                 ordsets:from_list(get_missing_block_hashes())),
    ok = write_blocks_to_chain([B2]),
    ?assertEqual(ordsets:from_list([B0H, B1H]),
                 ordsets:from_list(get_missing_block_hashes())),
    ok = write_blocks_to_chain([B0]),
    ?assertEqual(ordsets:from_list([B1H]),
                 ordsets:from_list(get_missing_block_hashes())),
    ok = write_blocks_to_chain([B1]),
    ?assertEqual(ordsets:from_list([]),
                 ordsets:from_list(get_missing_block_hashes())),
    ok.

%%%===================================================================
%%% Out of order tests

out_of_order_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db()
     end,
     [ {"Out of order insert of header chain",
        fun out_of_order_test_header_chain/0}
     , {"Out of order insert of block chain",
        fun out_of_order_test_block_chain/0}
     , {"Out of order insert of mixed chain",
        fun out_of_order_test_mixed_chain/0}
     , {"Out of order check if a block is connected",
        fun out_of_order_test_connected/0}
     ]}.

restart_chain_db() ->
    aec_test_utils:stop_chain_db(),
    aec_test_utils:start_chain_db().

out_of_order_test_header_chain() ->
    %% Create a chain that we are going to use.
    Chain = aec_test_utils:gen_blocks_only_chain(3),
    [BH0, BH1, BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H,_B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Insert the headers in different order and assert
    %% that the end result is the same
    write_headers_to_chain([BH0, BH1, BH2]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_headers_to_chain([BH0, BH2, BH1]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_headers_to_chain([BH1, BH0, BH2]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_headers_to_chain([BH1, BH2, BH0]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_headers_to_chain([BH2, BH0, BH1]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_headers_to_chain([BH2, BH1, BH0]),
    ?assertEqual(B2H, top_header_hash()),

    %% Check that the top header is not moving if the chain is not complete.
    restart_chain_db(),
    write_headers_to_chain([BH0, BH2]),
    ?assertEqual(B0H, top_header_hash()),

    restart_chain_db(),
    write_headers_to_chain([BH1, BH2]),
    ?assertEqual(undefined, top_header_hash()),
    ok.

out_of_order_test_block_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),
    [B0H,_B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Insert the headers in different order and assert
    %% that the end result is the same
    write_blocks_to_chain([B0, B1, B2]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B0, B2, B1]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B1, B0, B2]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B1, B2, B0]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B2, B0, B1]),
    ?assertEqual(B2H, top_header_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B2, B1, B0]),
    ?assertEqual(B2H, top_header_hash()),

    %% Check that the top block is not moving if the chain is not complete.
    restart_chain_db(),
    write_blocks_to_chain([B0, B2]),
    ?assertEqual(B0H, top_block_hash()),
    restart_chain_db(),
    write_blocks_to_chain([B1, B2]),
    ?assertEqual(undefined, top_block_hash()),
    ok.

out_of_order_test_mixed_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),
    [BH0, BH1,_BH2] = [aec_blocks:to_header(B) || B <- Chain],
    [B0H, B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Check that the top header hash and the top block hash is moving
    %% in the correct way.
    ok = insert_header(BH0),
    ?assertEqual(B0H, top_header_hash()),
    ?assertEqual(undefined, top_block_hash()),
    ok = insert_header(BH1),
    ?assertEqual(B1H, top_header_hash()),
    ?assertEqual(undefined, top_block_hash()),
    ok = insert_block(B0),
    ?assertEqual(B1H, top_header_hash()),
    ?assertEqual(B0H, top_block_hash()),
    ok = insert_block(B2),
    ?assertEqual(B2H, top_header_hash()),
    ?assertEqual(B0H, top_block_hash()),
    ok = insert_block(B1),
    ?assertEqual(B2H, top_header_hash()),
    ?assertEqual(B2H, top_block_hash()),
    ok.

out_of_order_test_connected() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2, B3] = aec_test_utils:gen_blocks_only_chain(4),
    [B0H, B1H, B2H, B3H] = [block_hash(H) || H <- Chain],

    %% Insert a broken chain and test connectivity
    ok = write_blocks_to_chain([B0, B1, B3]),
    ?assertEqual(true, hash_is_connected_to_genesis(B0H)),
    ?assertEqual(true, hash_is_connected_to_genesis(B1H)),
    ?assertEqual(false, hash_is_connected_to_genesis(B2H)),
    ?assertEqual(false, hash_is_connected_to_genesis(B3H)),

    %% Write the missing block
    ok = write_blocks_to_chain([B2]),
    ?assertEqual(true, hash_is_connected_to_genesis(B0H)),
    ?assertEqual(true, hash_is_connected_to_genesis(B1H)),
    ?assertEqual(true, hash_is_connected_to_genesis(B2H)),
    ?assertEqual(true, hash_is_connected_to_genesis(B3H)),
    ok.

%%%===================================================================
%%% Broken chain tests

broken_chain_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             teardown_meck_and_keys(TmpDir),
             aec_test_utils:stop_chain_db()
     end,
     [{"Test that an invalid block in a different fork is validated "
       "even if it arrives before the predecessor",
       fun broken_chain_postponed_validation/0},
      {"Add a block that points to the wrong place in the main chain"
       " because of its height",
       fun broken_chain_wrong_height/0},
      {"Add a block with the wrong state hash",
       fun broken_chain_wrong_state_hash/0},
      {"Add a block with invalid transaction",
       fun broken_chain_invalid_transaction/0}
     ]}.

broken_chain_postponed_validation() ->
    MainBC = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 2, 5], 111),
    AltChain = [B0, B1, B2, B3] = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 2, 1], 222),

    %% Assert that we are creating a fork
    ?assertNotEqual(MainBC, AltChain),
    ?assertEqual(hd(MainBC), B0),

    %% Insert the main chain.
    ok = write_blocks_to_chain(MainBC),
    TopHash = top_block_hash(),

    %% Insert the second block of the fork with a bad root hash
    Bad = B2#block{root_hash = <<"I'm not really a hash">>},
    ok = insert_block(Bad),

    {ok, Hash} = aec_blocks:hash_internal_representation(Bad),
    B3Bad = B3#block{prev_hash = Hash},
    ok = insert_block(B3Bad),

    %% Try to insert the first block of the fork
    %% It should succeed since the B1 block is not the faulty one...
    ok = insert_block(B1),

    %% ... but the top should not have changed...
    ?assertEqual(TopHash, top_block_hash()),

    %% ... and the faulty block should not have a stored state.
    ?assertEqual(error, get_block_state(Hash)),

    ok.

broken_chain_wrong_height() ->
    %% Create a chain that we are going to use.
    [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1]),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(B2)),

    %% Change the height of the last block to an incompatible height.
    ?assertEqual({error, height_inconsistent_with_previous_hash},
                 insert_block(B2#block{height = 4})),
    ?assertEqual({error, height_inconsistent_with_previous_hash},
                 insert_block(B2#block{height = 1})),
    ok.

broken_chain_wrong_state_hash() ->
    [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1]),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(B2)),

    %% Change the state hash to something wrong.
    Hash = B2#block.root_hash,
    Bogus = case <<1:(bit_size(Hash))>> =:= Hash of
                true  -> <<0:(bit_size(Hash))>>;
                false -> <<1:(bit_size(Hash))>>
            end,
    ?assertNotEqual(Hash, Bogus),
    ?assertMatch({error, {root_hash_mismatch, _, _}},
                 insert_block(B2#block{root_hash = Bogus})),
    ok.

broken_chain_invalid_transaction() ->
    [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1]),

    %% Add invalid transaction with negative nonce to last block
    Txs = B2#block.txs,
    BogusSpendTx = aec_test_utils:signed_spend_tx(#{recipient => <<>>, amount => 0, fee => 0, nonce => -1}),
    BogusTxs = [BogusSpendTx | Txs],

    ?assertNotEqual(Txs, BogusTxs),
    ?assertMatch({error, invalid_transactions_in_block},
                 insert_block(B2#block{txs = BogusTxs})),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(B2)),
    ok.

%%%===================================================================
%%% Block candidate test

n_headers_from_top_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db()
     end,
     [{"Ensure the right headers are returned.",
       fun n_headers_from_top/0}]}.

n_headers_from_top() ->
    Chain = gen_blocks_only_chain_by_target(
              [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 1),

    Hdrs = lists:reverse([ aec_blocks:to_header(B) || B <- Chain ]),


    ok = write_blocks_to_chain(Chain),
    TopHash = top_header_hash(),

    {ok, Hdrs0} = aec_chain:get_n_headers_from_hash(TopHash, 2),

    ?assertMatch(X when length(X) == 2, Hdrs0),
    ?assertEqual(lists:sublist(Hdrs, 2), lists:reverse(Hdrs0)),


    {ok, Hdrs1} = aec_chain:get_n_headers_from_hash(TopHash, 4),

    ?assertMatch(X when length(X) == 4, Hdrs1),
    ?assertEqual(lists:sublist(Hdrs, 4), lists:reverse(Hdrs1)).


%%%===================================================================
%%% Target validation tests

target_validation_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_difficulty_as_target(),
             meck:new(aec_governance, [passthrough]),
             meck:new(aec_pow, [passthrough]),
             meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 3),
             meck:expect(aec_governance, expected_block_mine_rate, 0, 3000000), %% 50 mins
             aec_test_utils:mock_genesis(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:unmock_difficulty_as_target(),
             meck:unload(aec_governance),
             meck:unload(aec_pow),
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db()
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
    Chain = [_,_,_,B3,_] = gen_blocks_only_chain_by_target(
                             [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 222),
    [BH0, BH1, BH2, BH3, BH4] = [aec_blocks:to_header(B) || B <- Chain],

    %% Insert genesis
    ok = insert_header(BH0),

    %% Do not allow too low target
    BH1TooLowTarget = BH1#header{target = trunc(?GENESIS_TARGET / 2)},
    ?assertMatch({error, {target_not_equal_to_parent, _, _, _}},
                 insert_header(BH1TooLowTarget)),

    ok = insert_header(BH1),
    ok = insert_header(BH2),

    %% Do not allow too high target
    BH3TooHighTarget = BH3#header{target = 2 * ?GENESIS_TARGET},
    ?assertMatch({error, {target_not_equal_to_parent, _, _, _}},
                 insert_header(BH3TooHighTarget)),

    ok = insert_header(BH3),

    %% target_not_equal_to_parent does not kick in for header with height = 4
    %% For header with height 4, header with height 1 is taken for difficulty recalculations
    ?assertNotMatch({error, {target_not_equal_to_parent, _, _, _}},
                    insert_header(BH4)),
    ?assertMatch({error, {wrong_target, _, _, _}},
                 insert_header(BH4)),

    ?assertEqual(block_hash(B3), top_header_hash()),
    ok.

target_verified_based_on_calculations() ->
    Good4 = 536926835,
    Bad4  = 536926853,
    Good5 = 520235910,
    T0    = aeu_time:now_in_msecs(),
    ChainData =
        #{ targets    => [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, Good4, Good5],
           nonce      => 12345,
           timestamps => [T0, T0 + 10000, T0 + 20000, T0 + 30000, T0 + 40000] },

    Chain = gen_blocks_only_chain(ChainData),
    [BH0, BH1, BH2, BH3, BH4, BH5] = [aec_blocks:to_header(B) || B <- Chain],

    ok = insert_header(BH0),
    ok = insert_header(BH1),
    ok = insert_header(BH2),
    ok = insert_header(BH3),

    %% Try to insert header with height=4 with an incorrect target
    BadBH4 = BH4#header{target = Bad4},
    ?assertMatch({error, {wrong_target, _, _, _}},
                 insert_header(BadBH4)),

    %% Insert header with height=4 with expected target
    ok = insert_header(BH4),
    ok = insert_header(BH5),

    ?assertEqual(block_hash(lists:last(Chain)), top_header_hash()),
    ok.

test_postponed_target_verification() ->
    CommonTargets = [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET],
    MainTargets = CommonTargets ++ [536926835, 520235910, 503676955],
    AltTargets  = CommonTargets ++ [536926835, 168427524, 503676955],

    T0 = aeu_time:now_in_msecs(),
    TS = [T0, T0 + 10000, T0 + 20000, T0 + 30000, T0 + 40000, T0 + 50000],
    MainBC = gen_blocks_only_chain(#{ targets => MainTargets, nonce => 111, timestamps => TS }),
    AltChain = [_, B1, B2, B3, B4, B5, B6] =
        gen_blocks_only_chain(#{ targets => AltTargets, nonce => 222, timestamps => TS }),

    %% Assert that we are creating a fork
    ?assertNotEqual(MainBC, AltChain),

    %% Insert the main chain
    ok = write_blocks_to_chain(MainBC),
    {ok, TopBlockHash} = aec_blocks:hash_internal_representation(lists:last(MainBC)),
    ?assertEqual(TopBlockHash, top_block_hash()),

    %% Insert all blocks of alt chain chain except B4 (which prevents from target validation)
    ok = insert_block(B1),
    ok = insert_block(B2),
    ok = insert_block(B3),
    ok = insert_block(B5),
    ok = insert_block(B6),

    %% Insert B4, which should make AltChain take over...
    ok = insert_block(B4),

    %% ... but already inserted B5 has too high target (it was mined too easily)
    ?assertEqual(TopBlockHash, top_block_hash()),

    ok.


%%%===================================================================
%%% Total difficulty test

total_difficulty_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             aec_test_utils:stop_chain_db(),
             teardown_meck_and_keys(TmpDir)
     end,
     [{"Get work in chain of only genesis",
       fun total_difficulty_only_genesis/0},
      {"Get work in header chain",
       fun total_difficulty_in_chain/0
      }]
    }.

total_difficulty_only_genesis() ->
    ok = insert_block(genesis_block()),
    {ok, Difficulty} = difficulty_at_top_header(),
    ?assertDifficultyEq(?GENESIS_DIFFICULTY, Difficulty).

total_difficulty_in_chain() ->
    %% In order to pass target validation, block after genesis has to have the same target as genesis block
    [B0, B1, B2, B3, B4] = Chain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    ok = write_blocks_to_chain(Chain),
    {ok, DiffTopH} = difficulty_at_top_header(),
    {ok, DiffTopB} = difficulty_at_top_block(),
    {ok, Diff0} = difficulty_at_hash(block_hash(B0)),
    {ok, Diff1} = difficulty_at_hash(block_hash(B1)),
    {ok, Diff2} = difficulty_at_hash(block_hash(B2)),
    {ok, Diff3} = difficulty_at_hash(block_hash(B3)),
    {ok, Diff4} = difficulty_at_hash(block_hash(B4)),

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
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             teardown_meck_and_keys(TmpDir),
             aec_test_utils:stop_chain_db()
     end,
     [ {"Fork on genesis", fun fork_on_genesis/0}
     , {"Fork on shorter chain because of difficulty", fun fork_on_shorter/0}
     , {"Fork on last block", fun fork_on_last_block/0}
     , {"Fork and out of order", fun fork_out_of_order/0}
     ]}.

fork_on_genesis() ->
    EasyChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 2, 2, 2], 111),
    HardChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    fork_common(EasyChain, HardChain).

fork_on_last_block() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = extend_chain_with_state(CommonChain, [2], 111),
    HardChain = extend_chain_with_state(CommonChain, [1], 222),
    fork_common(blocks_only_chain(EasyChain), blocks_only_chain(HardChain)).

fork_on_shorter() ->
    EasyChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 2, 2, 4], 111),
    HardChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 1, 1], 111),
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
    restart_chain_db(),
    %% The second chain should take over
    ok = write_blocks_to_chain(EasyChain),
    ?assertEqual(TopHashEasy, top_block_hash()),
    ok = write_blocks_to_chain(HardChain),
    ?assertEqual(TopHashHard, top_block_hash()),

    restart_chain_db(),
    %% The second chain should not take over
    ok = write_blocks_to_chain(HardChain),
    ?assertEqual(TopHashHard, top_block_hash()),
    ok = write_blocks_to_chain(EasyChain),
    ?assertEqual(TopHashHard, top_block_hash()),
    ok.

fork_common_headers(EasyChain, TopHashEasy, HardChain, TopHashHard) ->
    restart_chain_db(),
    %% The second chain should take over
    ok = write_headers_to_chain(EasyChain),
    ?assertEqual(TopHashEasy, top_header_hash()),
    ok = write_headers_to_chain(HardChain),
    ?assertEqual(TopHashHard, top_header_hash()),

    restart_chain_db(),
    %% The second chain should not take over
    ok = write_headers_to_chain(HardChain),
    ?assertEqual(TopHashHard, top_header_hash()),
    ok = write_headers_to_chain(EasyChain),
    ?assertEqual(TopHashHard, top_header_hash()),
    ok.

fork_out_of_order() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = extend_chain_with_state(CommonChain, [2], 111),
    HardChain = extend_chain_with_state(CommonChain, [1], 222),

    restart_chain_db(),
    %% Add the chain with the fork node as the last entry.
    ok = write_blocks_to_chain(lists:droplast(blocks_only_chain(CommonChain))),
    ok = insert_block(lists:last(blocks_only_chain(EasyChain))),
    ok = insert_block(lists:last(blocks_only_chain(HardChain))),

    %% The last block to enter is the last common node.
    ok = insert_block(lists:last(blocks_only_chain(CommonChain))),
    ?assertEqual(block_hash(lists:last(blocks_only_chain(HardChain))),
                 top_block_hash()),
    ok.


%%%===================================================================
%%% Blocks time summary tests

block_time_summary_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             aec_test_utils:stop_chain_db(),
             teardown_meck_and_keys(TmpDir)
     end,
     [ {"Empty list on no genesis", fun time_summary_no_genesis/0}
     , {"Time summary on only genesis block", fun time_summary_only_genesis/0}
     , {"Time summary N blocks", fun time_summary_N_blocks/0}
     ]}.

time_summary_no_genesis() ->
    ?assertEqual([], aec_chain:get_top_N_blocks_time_summary(30)),
    ok.

time_summary_only_genesis() ->
    Genesis = genesis_block(),
    ok = insert_block(Genesis),

    ?assertEqual([{aec_blocks:height(Genesis),
                   aec_blocks:time_in_msecs(Genesis),
                   aec_blocks:difficulty(Genesis)}],
                 aec_chain:get_top_N_blocks_time_summary(30)),
    ok.

time_summary_N_blocks() ->
    [B0, B1, B2, B3, B4] = Chain =
        gen_blocks_only_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    ok = write_blocks_to_chain(Chain),

    B0Time = aec_blocks:time_in_msecs(B0),
    B1Time = aec_blocks:time_in_msecs(B1),
    B2Time = aec_blocks:time_in_msecs(B2),
    B3Time = aec_blocks:time_in_msecs(B3),
    B4Time = aec_blocks:time_in_msecs(B4),

    Expected30Blocks = Expected5Blocks =
        [{aec_blocks:height(B4), B4Time, B4Time - B3Time, aec_blocks:difficulty(B4)},
         {aec_blocks:height(B3), B3Time, B3Time - B2Time, aec_blocks:difficulty(B3)},
         {aec_blocks:height(B2), B2Time, B2Time - B1Time, aec_blocks:difficulty(B2)},
         {aec_blocks:height(B1), B1Time, B1Time - B0Time, aec_blocks:difficulty(B1)},
         {aec_blocks:height(B0), B0Time, aec_blocks:difficulty(B0)}],

    ?assertEqual(Expected30Blocks,
                 aec_chain:get_top_N_blocks_time_summary(30)),
    ?assertEqual(Expected5Blocks,
                 aec_chain:get_top_N_blocks_time_summary(5)),

    Expected2Blocks =
        [{aec_blocks:height(B4), B4Time, B4Time - B3Time, aec_blocks:difficulty(B4)},
         {aec_blocks:height(B3), B3Time, B3Time - B2Time, aec_blocks:difficulty(B3)}],

    ?assertEqual(Expected2Blocks,
                 aec_chain:get_top_N_blocks_time_summary(2)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_meck_and_keys() ->
    aec_test_utils:mock_difficulty_as_target(),
    aec_test_utils:mock_block_target_validation(),
    aec_test_utils:mock_genesis(),
    aec_test_utils:aec_keys_setup().

teardown_meck_and_keys(TmpDir) ->
    aec_test_utils:unmock_difficulty_as_target(),
    aec_test_utils:unmock_block_target_validation(),
    aec_test_utils:unmock_genesis(),
    aec_test_utils:aec_keys_cleanup(TmpDir).

write_blocks_to_chain([H|T]) ->
    ok = insert_block(H),
    write_blocks_to_chain(T);
write_blocks_to_chain([]) ->
    ok.

write_headers_to_chain([H|T]) ->
    ok = insert_header(H),
    write_headers_to_chain(T);
write_headers_to_chain([]) ->
    ok.

gen_blocks_only_chain(Data) ->
    blocks_only_chain(gen_block_chain_with_state(Data)).

gen_block_chain_with_state(Data) ->
    {B0, S0} = genesis_block_with_state(),
    [{B0, S0} | extend_block_chain_with_state(B0, S0, Data)].

gen_blocks_only_chain_by_target(Targets, Nonce) ->
    blocks_only_chain(gen_block_chain_with_state_by_target(Targets, Nonce)).

gen_block_chain_with_state_by_target(Targets, Nonce) ->
    gen_block_chain_with_state(#{ targets => Targets, nonce => Nonce }).

extend_chain_with_state(Base, Targets, Nonce) ->
    {B, S} = lists:last(Base),
    Base ++ extend_block_chain_with_state(B, S, #{ targets => Targets, nonce => Nonce }).

block_hash(Block) ->
    {ok, H} = aec_blocks:hash_internal_representation(Block),
    H.
