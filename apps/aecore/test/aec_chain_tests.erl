%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-import(aec_test_utils,
        [ extend_block_chain_with_state/2
        , blocks_only_chain/1
        , genesis_block/0
        , genesis_block_with_state/1
        ]).

-import(aec_chain_state,
        [ insert_block/1
        ]).

-import(aec_chain,
        [ difficulty_at_hash/1
        , difficulty_at_top_block/0
        , get_block/1
        , get_key_block_by_height/1
        , get_block_state/1
        , get_header/1
        , get_key_header_by_height/1
        , top_block/0
        , top_block_hash/0
        , top_header/0
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
             aec_test_utils:mock_genesis(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db()
     end,
     [ {"Access for block chain", fun basic_access_test_block_chain/0}
     ]}.

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
    ?assertEqual({ok, BH0}, get_key_header_by_height(0)),
    ?assertEqual({ok, BH1}, get_key_header_by_height(1)),
    ?assertEqual({ok, BH2}, get_key_header_by_height(2)),
    ?compareBlockResults({ok, B0}, get_key_block_by_height(0)),
    ?compareBlockResults({ok, B1}, get_key_block_by_height(1)),
    ?compareBlockResults({ok, B2}, get_key_block_by_height(2)),
    ?assertEqual({error, chain_too_short}, get_key_block_by_height(3)).

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
     [ {"Out of order insert of block chain",
        fun out_of_order_test_block_chain/0}
     ]}.

restart_chain_db() ->
    aec_test_utils:stop_chain_db(),
    aec_test_utils:start_chain_db().

out_of_order_test_block_chain() ->
    %% Create a chain that we are going to use.
    Chain = [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),
    [B0H,_B1H, B2H] = [block_hash(H) || H <- Chain],

    %% Insert the blocks in different order and assert
    %% that the end result is the same
    write_blocks_to_chain([B0, B1, B2]),
    ?assertEqual(B2H, top_block_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B0, B2, B1]),
    ?assertEqual(B2H, top_block_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B1, B0, B2]),
    ?assertEqual(B2H, top_block_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B1, B2, B0]),
    ?assertEqual(B2H, top_block_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B2, B0, B1]),
    ?assertEqual(B2H, top_block_hash()),

    restart_chain_db(),
    write_blocks_to_chain([B2, B1, B0]),
    ?assertEqual(B2H, top_block_hash()),

    %% Check that the top block is not moving if the chain is not complete.
    restart_chain_db(),
    write_blocks_to_chain([B0, B2]),
    ?assertEqual(B0H, top_block_hash()),
    restart_chain_db(),
    write_blocks_to_chain([B1, B2]),
    ?assertEqual(undefined, top_block_hash()),
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
    ?assertEqual({error, height_inconsistent_for_keyblock_with_previous_hash},
                 insert_block(B2#block{height = 4})),
    ?assertEqual({error, height_inconsistent_for_keyblock_with_previous_hash},
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
    #{ public := SenderPubKey, secret := SenderPrivKey } = enacl:sign_keypair(),
    RecipientPubKey = <<42:32/unit:8>>,
    PresetAccounts = [{SenderPubKey, 100}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Spend = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),

    Chain0 = gen_block_chain_with_state_by_target(PresetAccounts, [?GENESIS_TARGET], 111),

    TxsFun = fun(_) -> [Spend] end,
    [B0, B1, MB1, _B2] = blocks_only_chain(extend_chain_with_state(Chain0, [?GENESIS_TARGET], 111, TxsFun)),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1]),

    %% Add invalid transaction with too high nonce to last block
    Txs = MB1#block.txs,
    BogusSpendTx = aec_test_utils:signed_spend_tx(#{recipient => <<1:32/unit:8>>,
                                                    amount => 0,
                                                    fee => 1,
                                                    nonce => 10,
                                                    payload => <<"">>}),
    BogusTxs = [BogusSpendTx | Txs],

    ?assertNotEqual(Txs, BogusTxs),
    ?assertMatch({error, invalid_transactions_in_block},
                 insert_block(MB1#block{txs = BogusTxs})),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(MB1)),
    ok.

%%%===================================================================
%%% Block candidate test

n_headers_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             aec_test_utils:stop_chain_db(),
             teardown_meck_and_keys(TmpDir)

     end,
     [ {"Get n headers backwards.", fun n_headers_backwards/0}
     , {"Get n headers forwards.", fun n_headers_forwards/0}
     , {"Get n headers forwards in fork.", fun n_headers_forwards_fork/0}
     ]}.

n_headers_backwards() ->
    Chain = gen_blocks_only_chain_by_target(
              [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 1),

    Hdrs = lists:reverse([ aec_blocks:to_header(B) || B <- Chain ]),


    ok = write_blocks_to_chain(Chain),
    TopHash = top_block_hash(),

    {ok, Hdrs0} = aec_chain:get_n_generation_headers_backwards_from_hash(TopHash, 2),

    ?assertMatch(X when length(X) == 2, Hdrs0),
    ?assertEqual(lists:sublist(Hdrs, 2), lists:reverse(Hdrs0)),


    {ok, Hdrs1} = aec_chain:get_n_generation_headers_backwards_from_hash(TopHash, 4),

    ?assertMatch(X when length(X) == 4, Hdrs1),
    ?assertEqual(lists:sublist(Hdrs, 4), lists:reverse(Hdrs1)),

    {ok, Hdrs2} = aec_chain:get_n_generation_headers_backwards_from_hash(TopHash, 5),

    ?assertMatch(X when length(X) == 5, Hdrs2),
    ?assertEqual(lists:sublist(Hdrs, 5), lists:reverse(Hdrs2)).

n_headers_forwards() ->
    Chain = gen_blocks_only_chain_by_target(
              [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 1),

    Hdrs = [ aec_blocks:to_header(B) || B <- Chain ],

    ok = write_blocks_to_chain(Chain),
    Hashes = [block_hash(B) || B <- Chain],
    ok = n_headers_forwards(Hdrs, Hashes, 5).

n_headers_forwards(_Hdrs,_Hashes, 0) ->
    ok;
n_headers_forwards(Hdrs, Hashes, M) ->
    n_headers_forwards(Hdrs, Hashes, M, 5),
    n_headers_forwards(Hdrs, Hashes, M - 1).

n_headers_forwards(_Hdrs,_Hashes,_M, 0) ->
    ok;
n_headers_forwards(Hdrs, Hashes, M, N) ->
    Hash        = lists:nth(N, Hashes),
    PrunedStart = lists:nthtail(N - 1, Hdrs),
    Expected    = lists:sublist(PrunedStart, M),
    ?assertEqual({ok, Expected},
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, M)),
    n_headers_forwards(Hdrs, Hashes, M, N - 1).


n_headers_forwards_fork() ->
    EasyChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 2, 2, 2, 1], 111),
    HardChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    EasyHdrs = [ aec_blocks:to_header(B) || B <- EasyChain ],
    HardHdrs = [ aec_blocks:to_header(B) || B <- HardChain ],

    ok = write_blocks_to_chain(EasyChain),
    Hash = aec_chain:genesis_hash(),
    %% We should now get the headers from the easy chain.
    ?assertEqual({ok, lists:sublist(EasyHdrs, 3)},
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, 3)),

    %% Write the hard chain, that will take over as main fork.
    %% We should now get the headers from the hard chain.
    ok = write_blocks_to_chain(HardChain),
    ?assertEqual({ok, lists:sublist(HardHdrs, 3)},
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, 3)),

    %% If we try to get headers forward in the easy chain, we should
    %% get an error since the function is only defined on the main chain.
    Hash1 = block_hash(lists:nth(2, EasyChain)),
    ?assertEqual(error,
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash1, 1)),

    %% A special case error is if the header has higher height than the top hash
    Hash2 = block_hash(lists:last(EasyChain)),
    ?assertEqual(error,
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash2, 1)),

    %% A special case error is if the hash doesn't exist
    Hash3 = <<123:256>>,
    ?assertEqual(error,
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash3, 1)),
    ok.

%%%===================================================================
%%% Target validation tests

target_validation_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_difficulty_as_target(),
             meck:new(aec_governance, [passthrough]),
             meck:new(aec_pow, [passthrough]),
             meck:expect(aec_governance, key_blocks_to_check_difficulty_count, 0, 3),
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
       " in first (key_blocks_to_check_difficulty_count + 1) headers/blocks",
       fun constant_target_at_the_beginning_of_the_chain/0},
      {"Ensure target is verified based on calculations"
       " after (key_blocks_to_check_difficulty_count + 1) headers/blocks",
       fun target_verified_based_on_calculations/0},
      {"Test target is verified even for blocks coming"
       " in different order, hence fork is rejected",
       fun test_postponed_target_verification/0}
     ]}.

constant_target_at_the_beginning_of_the_chain() ->
    [B0,B1,B2,B3,B4] =
        gen_blocks_only_chain_by_target(
          [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 222),

    %% Insert genesis
    ok = insert_block(B0),

    %% Do not allow too low target
    B1TooLowTarget = B1#block{target = trunc(?GENESIS_TARGET / 2)},
    ?assertMatch({error, {target_not_equal_to_parent, _, _, _}},
                 insert_block(B1TooLowTarget)),

    ok = insert_block(B1),
    ok = insert_block(B2),

    %% Do not allow too high target
    B3TooHighTarget = B3#block{target = 2 * ?GENESIS_TARGET},
    ?assertMatch({error, {target_not_equal_to_parent, _, _, _}},
                 insert_block(B3TooHighTarget)),

    ok = insert_block(B3),

    %% target_not_equal_to_parent does not kick in for block with height = 4
    %% For block with height 4, block with height 1 is taken for difficulty recalculations
    ?assertNotMatch({error, {target_not_equal_to_parent, _, _, _}},
                    insert_block(B4)),
    ?assertMatch({error, {wrong_target, _, _, _}},
                 insert_block(B4)),

    ?assertEqual(block_hash(B3), top_block_hash()),
    ok.

target_verified_based_on_calculations() ->
    Good4 = 536926835,
    Bad4  = 536926853,
    Good5 = 520235910,
    T0    = aeu_time:now_in_msecs(),
    ChainData =
        #{ targets    => [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, Good4, Good5],
           nonce      => 12345,
           timestamps => [T0, T0 + 10000, T0 + 20000, T0 + 30000, T0 + 40000],
           txs_by_height_fun => fun(_) -> [] end},

    [B0, B1, B2, B3, B4, B5] = Chain = gen_blocks_only_chain(ChainData),

    ok = insert_block(B0),
    ok = insert_block(B1),
    ok = insert_block(B2),
    ok = insert_block(B3),

    %% Try to insert block with height=4 with an incorrect target
    BadB4 = B4#block{target = Bad4},
    ?assertMatch({error, {wrong_target, _, _, _}},
                 insert_block(BadB4)),

    %% Insert block with height=4 with expected target
    ok = insert_block(B4),
    ok = insert_block(B5),

    ?assertEqual(block_hash(lists:last(Chain)), top_block_hash()),
    ok.

test_postponed_target_verification() ->
    CommonTargets = [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET],
    MainTargets = CommonTargets ++ [536926835, 520235910, 503676955],
    AltTargets  = CommonTargets ++ [536926835, 168427524, 503676955],

    T0 = aeu_time:now_in_msecs(),
    TS = [T0, T0 + 10000, T0 + 20000, T0 + 30000, T0 + 40000, T0 + 50000],
    MainBC = gen_blocks_only_chain(#{ targets => MainTargets, nonce => 111, timestamps => TS, txs_by_height_fun => fun(_) -> [] end }),
    AltChain = [_, B1, B2, B3, B4, B5, B6] =
        gen_blocks_only_chain(#{ targets => AltTargets, nonce => 222, timestamps => TS, txs_by_height_fun => fun(_) -> [] end }),

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
      {"Get work in block chain",
       fun total_difficulty_in_chain/0
      }]
    }.

total_difficulty_only_genesis() ->
    ok = insert_block(genesis_block()),
    {ok, Difficulty} = difficulty_at_top_block(),
    ?assertDifficultyEq(?GENESIS_DIFFICULTY, Difficulty).

total_difficulty_in_chain() ->
    %% In order to pass target validation, block after genesis has to have the same target as genesis block
    [B0, B1, B2, B3, B4] = Chain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, 1, 1, 1], 111),
    ok = write_blocks_to_chain(Chain),
    {ok, DiffTopB} = difficulty_at_top_block(),
    {ok, Diff0} = difficulty_at_hash(block_hash(B0)),
    {ok, Diff1} = difficulty_at_hash(block_hash(B1)),
    {ok, Diff2} = difficulty_at_hash(block_hash(B2)),
    {ok, Diff3} = difficulty_at_hash(block_hash(B3)),
    {ok, Diff4} = difficulty_at_hash(block_hash(B4)),

    %% Mecked difficulty = ?FACTOR / target
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
     , {"Get by height in forks", fun fork_get_by_height/0}
     , {"Test if hash is connected to genesis", fun fork_is_connected_to_genesis/0}
     , {"Test if hash is in main chain", fun fork_is_in_main_chain/0}
     , {"Get a transaction from the right fork", fun fork_get_transaction/0}
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


fork_get_by_height() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = blocks_only_chain(extend_chain_with_state(CommonChain, [2], 111)),
    HardChain = blocks_only_chain(extend_chain_with_state(CommonChain, [1], 222)),

    ok = write_blocks_to_chain(EasyChain),
    ?assertEqual({ok, block_hash(lists:last(EasyChain))},
                 aec_chain_state:get_key_block_hash_at_height(4)),

    ok = write_blocks_to_chain(HardChain),
    ?assertEqual({ok, block_hash(lists:last(HardChain))},
                 aec_chain_state:get_key_block_hash_at_height(4)),

    ?assertEqual(error, aec_chain_state:get_key_block_hash_at_height(5)),

    ok.

fork_is_connected_to_genesis() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = blocks_only_chain(extend_chain_with_state(CommonChain, [2, 2], 111)),
    HardChain = blocks_only_chain(extend_chain_with_state(CommonChain, [1, 1], 222)),

    %% Add the easy chain
    ok = write_blocks_to_chain(EasyChain),
    [?assertEqual(true, aec_chain:hash_is_connected_to_genesis(block_hash(B)))
     || B <- EasyChain],

    %% Add only the top of the hard chain
    HardTop = lists:last(HardChain),
    HardTopHash = block_hash(HardTop),
    ok = write_blocks_to_chain([HardTop]),
    ?assertEqual(false, aec_chain:hash_is_connected_to_genesis(HardTopHash)),

    %% Add the rest of the hard chain.
    ok = write_blocks_to_chain(HardChain),
    [?assertEqual(true, aec_chain:hash_is_connected_to_genesis(block_hash(B)))
     || B <- EasyChain],
    [?assertEqual(true, aec_chain:hash_is_connected_to_genesis(block_hash(B)))
     || B <- HardChain],
    ok.

fork_is_in_main_chain() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, 1, 1], 111),
    EasyChain = blocks_only_chain(extend_chain_with_state(CommonChain, [2, 2], 111)),
    HardChain = blocks_only_chain(extend_chain_with_state(CommonChain, [1, 1], 222)),

    ok = write_blocks_to_chain(EasyChain),
    [?assertEqual(true, aec_chain:hash_is_in_main_chain(block_hash(B)))
     || B <- EasyChain],
    ok = write_blocks_to_chain(HardChain),
    ?assertEqual(block_hash(lists:last(HardChain)), aec_chain:top_block_hash()),

    ?assertEqual({ok, block_hash(lists:last(blocks_only_chain(CommonChain)))},
                 aec_chain:find_common_ancestor(block_hash(lists:last(HardChain)),
                                                block_hash(lists:last(EasyChain)))),

    [?assertEqual(true, aec_chain:hash_is_in_main_chain(block_hash(B)))
     || B <- [hd(HardChain)]],
    [?assertEqual(false, aec_chain:hash_is_in_main_chain(block_hash(B)))
     || B <- EasyChain -- blocks_only_chain(CommonChain)],
    ?assertEqual(false, aec_chain:hash_is_in_main_chain(<<12345:256>>)),
    ok.

fork_get_transaction() ->
    #{ public := SenderPubKey, secret := SenderPrivKey } = enacl:sign_keypair(),
    RecipientPubKey = <<42:32/unit:8>>,
    PresetAccounts = [{SenderPubKey, 100}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Spend1 = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),
    Spend2 = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 2, RecipientPubKey), SenderPrivKey),
    CommonChainTargets = [?GENESIS_TARGET, 1, 1],
    EasyChainExtensionTargets = [2, 2],
    HardChainExtensionTargets = [1, 1, 1],
    EasyChainTopHeight = ?GENESIS_HEIGHT + length(CommonChainTargets ++ EasyChainExtensionTargets),
    HardChainTopHeight = ?GENESIS_HEIGHT + length(CommonChainTargets ++ HardChainExtensionTargets),
    ?assertMatch(H when H > EasyChainTopHeight, HardChainTopHeight),
    TxsFun = fun
                 (H) when H =:= EasyChainTopHeight -> [Spend1];
                 (H) when H =:= HardChainTopHeight -> [Spend2];
                 (_) -> []
             end,
    CommonChain = gen_block_chain_with_state_by_target(PresetAccounts, CommonChainTargets, 111),
    EasyChain = blocks_only_chain(extend_chain_with_state(CommonChain, EasyChainExtensionTargets, 111, TxsFun)),
    HardChain = blocks_only_chain(extend_chain_with_state(CommonChain, HardChainExtensionTargets, 222, TxsFun)),

    %% Txs is in second to last (micro-)block
    EasyTopBlock = lists:last(lists:droplast(EasyChain)),
    [EasySpend] = aec_blocks:txs(EasyTopBlock),
    EasyTxHash = aetx_sign:hash(EasySpend),

    HardTopBlock = lists:last(lists:droplast(HardChain)),
    [HardSpend] = aec_blocks:txs(HardTopBlock),
    HardTxHash = aetx_sign:hash(HardSpend),

    HardButLastBlock = lists:last(lists:sublist(HardChain, 1, length(HardChain) - 3)),
    [HardButLastSpend] = aec_blocks:txs(HardButLastBlock),
    HardButLastTxHash = aetx_sign:hash(HardButLastSpend),

    ?assertEqual(HardButLastTxHash, EasyTxHash),
    ?assertNotEqual(HardButLastTxHash, HardTxHash),

    ok = write_blocks_to_chain(EasyChain),
    ?assertEqual({block_hash(EasyTopBlock), EasySpend},
                 aec_chain:find_tx_with_location(EasyTxHash)),
    ?assertEqual(none,
                 aec_chain:find_tx_with_location(HardTxHash)),

    ok = write_blocks_to_chain(HardChain),
    ?assertEqual({block_hash(HardButLastBlock), HardButLastSpend},
                 aec_chain:find_tx_with_location(HardButLastTxHash)),
    ?assertEqual({block_hash(HardTopBlock), HardSpend},
                 aec_chain:find_tx_with_location(HardTxHash)),

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
%%% Fees test

fees_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             aec_test_utils:stop_chain_db(),
             teardown_meck_and_keys(TmpDir)
     end,
     [{"Check fee division between three beneficiaries",
       fun fees_three_beneficiaries/0}
     ]
    }.

fees_three_beneficiaries() ->
    %% Two accounts to act as sender and receiver.
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret :=_PrivKey2 } = enacl:sign_keypair(),

    PresetAccounts = [{PubKey1, 1000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% Three accounts to act as miners
    #{ public := PubKey3, secret := PrivKey3 } = enacl:sign_keypair(),
    #{ public := PubKey4, secret := PrivKey4 } = enacl:sign_keypair(),
    #{ public := PubKey5, secret := PrivKey5 } = enacl:sign_keypair(),

    %% Three accounts to act as beneficiaries
    #{ public := PubKey6, secret := _PrivKey6 } = enacl:sign_keypair(),
    #{ public := PubKey7, secret := _PrivKey7 } = enacl:sign_keypair(),
    #{ public := PubKey8, secret := _PrivKey8 } = enacl:sign_keypair(),

    %% Add two transactions in different blocks to collect fees from
    Fee1 = 10,
    Fee2 = 100,
    TxsFun = fun(2) ->
                     Tx = make_spend_tx(PubKey1, 1, PubKey2, Fee1),
                     [aec_test_utils:sign_tx(Tx, PrivKey1)];
                (3) ->
                     Tx = make_spend_tx(PubKey1, 2, PubKey2, Fee2),
                     [aec_test_utils:sign_tx(Tx, PrivKey1)];
                (_) ->
                     []
             end,

    %% Create a chain with the three different miners and beneficiaries
    Miners = [ {PubKey3, PrivKey3}
             , {PubKey4, PrivKey4}
             , {PubKey5, PrivKey5}
             ],
    Beneficiaries = [PubKey6, PubKey7, PubKey8],
    Chain0 = gen_block_chain_with_state_by_actors(PresetAccounts, Miners, Beneficiaries, TxsFun),
    Chain = blocks_only_chain(Chain0),

    %% Write all but the last key block to the chain and keep the top hash.
    ok = write_blocks_to_chain(lists:droplast(Chain)),
    Hash1 = aec_chain:top_block_hash(),
    {ok, Balances1} = aec_chain:all_accounts_balances_at_hash(Hash1),
    DictBal1 = orddict:from_list(Balances1),

    %% Write the last one as well to close the last generation.
    ok = write_blocks_to_chain([lists:last(Chain)]),
    Hash2 = aec_chain:top_block_hash(),
    {ok, Balances2} = aec_chain:all_accounts_balances_at_hash(Hash2),
    DictBal2 = orddict:from_list(Balances2),

    %% Before the last generation is closed, only the two first beneficiaries
    %% should have collected rewards
    MiningReward = aec_governance:block_mine_reward(),
    ?assertEqual(MiningReward + round(Fee1 * 0.4),
                 orddict:fetch(PubKey6, DictBal1)),
    ?assertEqual(MiningReward + round(Fee1 * 0.6),
                 orddict:fetch(PubKey7, DictBal1)),
    ?assertEqual(false, orddict:is_key(PubKey5, DictBal1)),

    %% When the last generation is closed, the last transaction fee should
    %% also have been collected.
    ?assertEqual(MiningReward + round(Fee1 * 0.4),
                 orddict:fetch(PubKey6, DictBal2)),
    ?assertEqual(MiningReward + round(Fee1 * 0.6) + round(Fee2 * 0.4),
                 orddict:fetch(PubKey7, DictBal2)),
    ?assertEqual(MiningReward + round(Fee2 * 0.6),
                 orddict:fetch(PubKey8, DictBal2)),

    %% Miners' balances did not change, since beneficiaries took the rewards.
    ?assertEqual(error, orddict:find(PubKey3, DictBal2)),
    ?assertEqual(error, orddict:find(PubKey4, DictBal2)),
    ?assertEqual(error, orddict:find(PubKey5, DictBal2)),
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

gen_blocks_only_chain(Data) ->
    gen_blocks_only_chain(aec_test_utils:preset_accounts(), Data).

gen_blocks_only_chain(PresetAccounts, Data) ->
    blocks_only_chain(gen_block_chain_with_state(PresetAccounts, Data)).

gen_block_chain_with_state(PresetAccounts, Data) ->
    {B0, S0} = genesis_block_with_state(PresetAccounts),
    extend_block_chain_with_state([{B0, S0}], Data).

gen_blocks_only_chain_by_target(Targets, Nonce) ->
    gen_blocks_only_chain_by_target(aec_test_utils:preset_accounts(), Targets, Nonce).

gen_blocks_only_chain_by_target(PresetAccounts, Targets, Nonce) ->
    blocks_only_chain(gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce)).

gen_block_chain_with_state_by_target(Targets, Nonce) ->
    gen_block_chain_with_state_by_target(aec_test_utils:preset_accounts(), Targets, Nonce).

gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce) ->
    gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce, fun(_) -> [] end).

gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce, TxsFun) ->
    gen_block_chain_with_state(PresetAccounts, #{ targets => Targets, txs_by_height_fun => TxsFun, nonce => Nonce }).

gen_block_chain_with_state_by_actors(PresetAccounts, Miners, Beneficiaries, TxsFun) ->
    Targets = lists:duplicate(length(Miners), ?GENESIS_TARGET),
    gen_block_chain_with_state(PresetAccounts,
                               #{ miners => Miners,
                                  beneficiaries => Beneficiaries,
                                  targets => Targets,
                                  nonce => 111,
                                  txs_by_height_fun => TxsFun}).

extend_chain_with_state(Base, Targets, Nonce) ->
    extend_chain_with_state(Base, Targets, Nonce, fun(_) -> [] end).

extend_chain_with_state(Base, Targets, Nonce, TxsFun) ->
    extend_block_chain_with_state(Base, #{ targets => Targets, txs_by_height_fun => TxsFun, nonce => Nonce }).

block_hash(Block) ->
    {ok, H} = aec_blocks:hash_internal_representation(Block),
    H.

make_spend_tx(Sender, SenderNonce, Recipient) ->
    make_spend_tx(Sender, SenderNonce, Recipient, 1).

make_spend_tx(Sender, SenderNonce, Recipient, Fee) ->
    {ok, SpendTx} = aec_spend_tx:new(#{sender => Sender,
                                       recipient => Recipient,
                                       amount => 1,
                                       fee => Fee,
                                       nonce => SenderNonce,
                                       payload => <<>>}),
    SpendTx.
