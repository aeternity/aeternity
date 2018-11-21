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
    %% Create a chain with both key and micro blocks
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    TxsFun = fun(1) ->
                     Tx1 = make_spend_tx(PubKey, 1, PubKey, 1),
                     Tx2 = make_spend_tx(PubKey, 2, PubKey, 1),
                     [aec_test_utils:sign_tx(Tx1, PrivKey),
                      aec_test_utils:sign_tx(Tx2, PrivKey)
                     ];
                (_) ->
                     []
             end,
    PresetAccounts = [{PubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Chain0 = gen_block_chain_with_state_by_target(
               PresetAccounts,
               [?HIGHEST_TARGET_SCI, ?HIGHEST_TARGET_SCI], 1, TxsFun),
    [KB0, KB1, MB1, MB2, KB2] = blocks_only_chain(Chain0),
    {ok, KB0Hash} = aec_blocks:hash_internal_representation(KB0),
    {ok, KB1Hash} = aec_blocks:hash_internal_representation(KB1),
    {ok, KB2Hash} = aec_blocks:hash_internal_representation(KB2),
    {ok, MB1Hash} = aec_blocks:hash_internal_representation(MB1),
    {ok, MB2Hash} = aec_blocks:hash_internal_representation(MB2),

    ?assertEqual(true , aec_blocks:is_key_block(KB0)),
    ?assertEqual(true , aec_blocks:is_key_block(KB1)),
    ?assertEqual(false, aec_blocks:is_key_block(MB1)),
    ?assertEqual(false, aec_blocks:is_key_block(MB2)),
    ?assertEqual(true,  aec_blocks:is_key_block(KB2)),

    %% We should not be able to skip ahead for key blocks
    ok = insert_block(KB0),
    ?assertEqual(KB0Hash, top_block_hash()),
    ?assertMatch({error, {illegal_orphan, _}}, insert_block(KB2)),

    %% We should not be able to skip ahead for micro blocks
    ?assertMatch({error, {illegal_orphan, _}}, insert_block(MB1)),

    %% ... even if we have the corresponding key block
    ok = insert_block(KB1),
    ?assertEqual(KB1Hash, top_block_hash()),
    ?assertMatch({error, {illegal_orphan, _}}, insert_block(MB2)),

    %% Check that we can insert them all if they are connected.
    ok = insert_block(MB1),
    ?assertEqual(MB1Hash, top_block_hash()),

    ok = insert_block(MB2),
    ?assertEqual(MB2Hash, top_block_hash()),

    ok = insert_block(KB2),
    ?assertEqual(KB2Hash, top_block_hash()),

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
     [{"Test that an invalid block in a different fork is validated",
       fun broken_chain_broken_fork/0},
      {"Add a block that points to the wrong place in the main chain"
       " because of its height",
       fun broken_chain_wrong_height/0},
      {"Add a block with the wrong state hash",
       fun broken_chain_wrong_state_hash/0},
      {"Add a block with the wrong previous key hash",
       fun broken_chain_wrong_prev_key_hash/0},
      {"Add a block with invalid transaction",
       fun broken_chain_invalid_transaction/0},
      {"Add a block with invalid micro block signature",
       fun broken_chain_invalid_micro_block_signature/0},
      {"Add a block which fails median time validation",
       fun broken_chain_median_time/0}
     ]}.

broken_chain_broken_fork() ->
    MainBC = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 5], 111),
    AltChain = [B0, B1, B2, B3] = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1], 222),

    %% Assert that we are creating a fork
    ?assertNotEqual(MainBC, AltChain),
    ?assertEqual(hd(MainBC), B0),

    %% Insert the main chain.
    ok = write_blocks_to_chain(MainBC),
    TopHash = top_block_hash(),

    %% Insert the first block of the fork
    %% It should succeed since the B1 and B2 block are not the faulty one
    ok = write_blocks_to_chain([B1, B2]),

    %% The top should not have changed
    ?assertEqual(TopHash, top_block_hash()),

    %% Try to insert the second block of the fork with a bad root hash
    Bad = aec_blocks:set_root_hash(B3, <<"I'm not really a hash">>),
    ?assertMatch({error, _}, insert_block(Bad)),

    %% ... and the faulty block should not have a stored state.
    ?assertEqual(error, get_block_state(block_hash(Bad))),

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
                 insert_block(aec_blocks:set_height(B2, 4))),
    ?assertEqual({error, height_inconsistent_for_keyblock_with_previous_hash},
                 insert_block(aec_blocks:set_height(B2, 1))),
    ok.

broken_chain_wrong_state_hash() ->
    [B0, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1]),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(B2)),

    %% Change the state hash to something wrong.
    Hash = aec_blocks:root_hash(B2),
    Bogus = case <<1:(bit_size(Hash))>> =:= Hash of
                true  -> <<0:(bit_size(Hash))>>;
                false -> <<1:(bit_size(Hash))>>
            end,
    ?assertNotEqual(Hash, Bogus),
    ?assertMatch({error, {root_hash_mismatch, _, _}},
                 insert_block(aec_blocks:set_root_hash(B2, Bogus))),
    ok.

broken_chain_wrong_prev_key_hash() ->
    #{ public := SenderPubKey, secret := SenderPrivKey } = enacl:sign_keypair(),
    RecipientPubKey = <<42:32/unit:8>>,
    PresetAccounts = [{SenderPubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Spend1 = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),
    Spend2 = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 2, RecipientPubKey), SenderPrivKey),

    Chain0 = gen_block_chain_with_state_by_target(PresetAccounts, [?GENESIS_TARGET], 111),

    TxsFun = fun(2) -> [Spend1, Spend2];
                (_) -> []
             end,
    [B0,_B1, B2, MB1, MB2, B3] = Chain =
        blocks_only_chain(extend_chain_with_state(Chain0, [?GENESIS_TARGET, ?GENESIS_TARGET], 111, TxsFun)),

    %% Insert the full chain so we know it is valid.
    ok = write_blocks_to_chain(Chain),

    B0Hash = block_hash(B0),

    %% Set the prev key hash to the wrong key block and see that it fails.
    ?assertEqual({error, prev_key_hash_inconsistency},
                 insert_block(aec_blocks:set_prev_key_hash(B2, B0Hash))),
    ?assertEqual({error, prev_key_hash_inconsistency},
                 insert_block(aec_blocks:set_prev_key_hash(B3, B0Hash))),
    ?assertEqual({error, prev_key_hash_inconsistency},
                 insert_block(aec_blocks:set_prev_key_hash(MB1, B0Hash))),
    ?assertEqual({error, prev_key_hash_inconsistency},
                 insert_block(aec_blocks:set_prev_key_hash(MB2, B0Hash))),
    ok.

broken_chain_invalid_transaction() ->
    #{ public := SenderPubKey, secret := SenderPrivKey } = enacl:sign_keypair(),
    RecipientPubKey = <<42:32/unit:8>>,
    PresetAccounts = [{SenderPubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Spend = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),

    Chain0 = gen_block_chain_with_state_by_target(PresetAccounts, [?GENESIS_TARGET], 111),

    TxsFun = fun(_) -> [Spend] end,
    [B0, B1, B2, MB2] = blocks_only_chain(extend_chain_with_state(Chain0, [?GENESIS_TARGET], 111, TxsFun)),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1, B2]),

    %% Add invalid transaction with too high nonce to last block
    Txs = aec_blocks:txs(MB2),
    BogusSpendTx = aec_test_utils:signed_spend_tx(
                     #{recipient_id => aec_id:create(account, <<1:32/unit:8>>),
                       amount => 0,
                       fee => 1,
                       nonce => 10,
                       payload => <<"">>}),
    BogusTxs = [BogusSpendTx | Txs],

    ?assertNotEqual(Txs, BogusTxs),
    ?assertMatch({error, invalid_transactions_in_block},
                 insert_block(aec_blocks:set_txs(MB2, BogusTxs))),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(MB2)),
    ok.

broken_chain_invalid_micro_block_signature() ->
    #{ public := SenderPubKey, secret := SenderPrivKey } = enacl:sign_keypair(),
    #{ secret := BogusPrivKey } = enacl:sign_keypair(),

    RecipientPubKey = <<42:32/unit:8>>,
    PresetAccounts = [{SenderPubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Spend = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),

    Chain0 = gen_block_chain_with_state_by_target(PresetAccounts, [?GENESIS_TARGET], 111),

    TxsFun = fun(_) -> [Spend] end,
    [B0, B1, B2, MB] = blocks_only_chain(extend_chain_with_state(Chain0, [?GENESIS_TARGET], 111, TxsFun)),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1, B2]),

    %% Mess up the signature of the last block
    BogusMB = aec_test_utils:sign_micro_block(MB, BogusPrivKey),

    ?assertMatch({error, signature_verification_failed}, insert_block(BogusMB)),

    %% Check that we can insert the unmodified last block
    ?assertEqual(ok, insert_block(MB)),
    ok.

broken_chain_median_time() ->
    TimeStampKeyBlocks = aec_governance:median_timestamp_key_blocks(),
    Chain = aec_test_utils:gen_blocks_only_chain(TimeStampKeyBlocks + 2),
    ButLast = lists:droplast(Chain),
    Last = lists:last(Chain),
    ?assertEqual(ok, write_blocks_to_chain(ButLast)),
    MedianTS = median([aec_blocks:time_in_msecs(B) || B <- tl(ButLast)]),
    OldBlock = aec_blocks:set_time_in_msecs(Last, MedianTS - 1),
    ?assertEqual({error, key_block_from_the_past}, insert_block(OldBlock)),
    ?assertEqual(ok, insert_block(Last)).

median(Xs) ->
    Sorted = lists:sort(Xs),
    Length = length(Sorted),
    Mid = Length div 2,
    Rem = Length rem 2,
    (lists:nth(Mid+Rem, Sorted) + lists:nth(Mid+1, Sorted)) div 2.

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
    EasyChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 2, 2, 2, 1], 111),
    HardChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1, 1], 111),
    EasyHdrs = [ aec_blocks:to_header(B) || B <- EasyChain ],
    HardHdrs = [ aec_blocks:to_header(B) || B <- HardChain ],

    ok = write_blocks_to_chain(EasyChain),
    Hash = aec_chain:genesis_hash(),
    %% We should now get the headers from the easy chain.
    ?assertEqual({ok, lists:sublist(EasyHdrs, 4)},
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, 4)),

    %% Write the hard chain, that will take over as main fork.
    %% We should now get the headers from the hard chain.
    ok = write_blocks_to_chain(HardChain),
    ?assertEqual({ok, lists:sublist(HardHdrs, 4)},
                 aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, 4)),

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
             meck:expect(aec_governance, key_blocks_to_check_difficulty_count, 0, 2),
             meck:expect(aec_governance, expected_block_mine_rate, 0, 1800000), %% 50 mins
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
       fun target_verified_based_on_calculations/0}
     ]}.

constant_target_at_the_beginning_of_the_chain() ->
    [B0,B1,B2,B3,B4] =
        gen_blocks_only_chain_by_target(
          [?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET, ?GENESIS_TARGET], 222),

    %% Insert genesis
    ok = insert_block(B0),

    %% Do not allow too low target
    B1TooLowTarget = aec_blocks:set_target(B1, trunc(?GENESIS_TARGET / 2)),
    ?assertMatch({error, {target_not_equal_to_parent, _, _, _}},
                 insert_block(B1TooLowTarget)),

    ok = insert_block(B1),
    ok = insert_block(B2),

    %% Do not allow too high target
    B3TooHighTarget = aec_blocks:set_target(B3, 2 * ?GENESIS_TARGET),
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
    Good4 = 553697371,
    Bad4  = 553697352,
    Good5 = 553690381,
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
    BadB4 = aec_blocks:set_target(B4, Bad4),
    ?assertMatch({error, {wrong_target, _, _, _}},
                 insert_block(BadB4)),

    %% Insert block with height=4 with expected target
    ok = insert_block(B4),
    ok = insert_block(B5),

    ?assertEqual(block_hash(lists:last(Chain)), top_block_hash()),
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
    [B0, B1, B2, B3, B4] = Chain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
    ok = write_blocks_to_chain(Chain),
    {ok, DiffTopB} = difficulty_at_top_block(),
    {ok, Diff0} = difficulty_at_hash(block_hash(B0)),
    {ok, Diff1} = difficulty_at_hash(block_hash(B1)),
    {ok, Diff2} = difficulty_at_hash(block_hash(B2)),
    {ok, Diff3} = difficulty_at_hash(block_hash(B3)),
    {ok, Diff4} = difficulty_at_hash(block_hash(B4)),

    %% Mecked difficulty = ?FACTOR / target
    ?assertDifficultyEq(3 * ?GENESIS_DIFFICULTY + 2 * ?FACTOR, DiffTopB),
    ?assertDifficultyEq(1 * ?GENESIS_DIFFICULTY + 0 * ?FACTOR, Diff0),
    ?assertDifficultyEq(2 * ?GENESIS_DIFFICULTY + 0 * ?FACTOR, Diff1),
    ?assertDifficultyEq(3 * ?GENESIS_DIFFICULTY + 0 * ?FACTOR, Diff2),
    ?assertDifficultyEq(3 * ?GENESIS_DIFFICULTY + 1 * ?FACTOR, Diff3),
    ?assertDifficultyEq(3 * ?GENESIS_DIFFICULTY + 2 * ?FACTOR, Diff4),

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
     , {"Get by height in forks", fun fork_get_by_height/0}
     , {"Test if hash is in main chain", fun fork_is_in_main_chain/0}
     , {"Get a transaction from the right fork", fun fork_get_transaction/0}
     , {"Fork on micro-block", fun fork_on_micro_block/0}
     , {"Fork on old fork point", fun fork_on_old_fork_point/0}
     , {"Generate candidate from top with high fork", fun fork_gen_key_candidate/0}
     ]}.

fork_on_genesis() ->
    EasyChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 2, 2], 111),
    HardChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
    fork_common(EasyChain, HardChain).

fork_on_last_block() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
    EasyChain = extend_chain_with_state(CommonChain, [2], 111),
    HardChain = extend_chain_with_state(CommonChain, [1], 222),
    fork_common(blocks_only_chain(EasyChain), blocks_only_chain(HardChain)).

fork_on_shorter() ->
    EasyChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 2, 2, 4], 111),
    HardChain = gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
    fork_common(EasyChain, HardChain).

fork_common(EasyChain, HardChain) ->
    TopHashEasy = block_hash(lists:last(EasyChain)),
    TopHashHard = block_hash(lists:last(HardChain)),
    %% Insert blocks
    ok = fork_common_block(EasyChain, TopHashEasy, HardChain, TopHashHard),
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

fork_get_by_height() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
    EasyChain = blocks_only_chain(extend_chain_with_state(CommonChain, [2], 111)),
    HardChain = blocks_only_chain(extend_chain_with_state(CommonChain, [1], 222)),

    ok = write_blocks_to_chain(EasyChain),
    ?assertEqual({ok, block_hash(lists:last(EasyChain))},
                 aec_chain_state:get_key_block_hash_at_height(5)),

    ok = write_blocks_to_chain(HardChain),
    ?assertEqual({ok, block_hash(lists:last(HardChain))},
                 aec_chain_state:get_key_block_hash_at_height(5)),

    ?assertEqual(error, aec_chain_state:get_key_block_hash_at_height(6)),

    ok.

fork_is_in_main_chain() ->
    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
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
    PresetAccounts = [{SenderPubKey, 100000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    Spend1 = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),
    Spend2 = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 2, RecipientPubKey), SenderPrivKey),
    CommonChainTargets = [?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1],
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

    %% Txs is in the last (micro-)block
    EasyTopBlock = lists:last(EasyChain),
    [EasySpend] = aec_blocks:txs(EasyTopBlock),
    EasyTxHash = aetx_sign:hash(EasySpend),

    HardTopBlock = lists:last(HardChain),
    [HardSpend] = aec_blocks:txs(HardTopBlock),
    HardTxHash = aetx_sign:hash(HardSpend),

    HardButLastBlock = lists:last(lists:sublist(HardChain, 1, length(HardChain) - 2)),
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

fork_on_micro_block() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAccounts = [{PubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% Create main chain with both key and micro blocks
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 1), PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, 1), PrivKey),
                     [Tx1, Tx2];
                (_) ->
                     []
             end,
    [B0, B1, B2, _, _, _] =
        Chain0 = gen_block_chain_with_state_by_target(
                   PresetAccounts,
                   [?HIGHEST_TARGET_SCI, ?HIGHEST_TARGET_SCI, ?HIGHEST_TARGET_SCI], 1, TxsFun),
    [KB0, KB1, MB1, MB2, KB2, KB3] = blocks_only_chain(Chain0),

    %% Create fork, which starts on a micro block
    CommonChain = [B0, B1, B2],
    Fork = extend_chain_with_state(CommonChain, [?HIGHEST_TARGET_SCI], 1),
    [KB0, KB1, MB1, KB2Fork] = blocks_only_chain(Fork),

    %% Insert fork first
    ok = insert_block(KB0),
    ok = insert_block(KB1),
    ok = insert_block(MB1),
    ok = insert_block(KB2Fork),

    {ok, KB2ForkHash} = aec_blocks:hash_internal_representation(KB2Fork),
    ?assertEqual(KB2ForkHash, aec_chain:top_block_hash()),

    %% Insert main chain
    ok = insert_block(MB2),
    ok = insert_block(KB2),
    ok = insert_block(KB3),

    %% Check main chain took over
    {ok, KB3Hash} = aec_blocks:hash_internal_representation(KB3),
    ?assertEqual(KB3Hash, aec_chain:top_block_hash()),

    ok.

fork_on_old_fork_point() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAccounts = [{PubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    CommonChain = gen_block_chain_with_state_by_target(
                    PresetAccounts, [?GENESIS_TARGET, ?GENESIS_TARGET], 111),
    OriginalChain = extend_chain_with_state(CommonChain, [2, 2, 2, 2, 2, 2, 2, 2], 111),
    ForkHeight = length(CommonChain),

    %% Create a fork chain with both micro blocks and key blocks
    TxsFun = fun(N) when N =:= ForkHeight ->
                     [aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 1), PrivKey)]
             end,
    ForkChain0 = extend_chain_with_state(CommonChain, [1], 222, TxsFun),
    ForkChain1 = lists:nthtail(ForkHeight, ForkChain0),

    [ForkBlock|MicroBlocks] = blocks_only_chain(ForkChain1),

    %% Add the original chain
    ok = write_blocks_to_chain(blocks_only_chain(OriginalChain)),

    %% If we try to add the fork block trough gossip, it should be refused.
    ?assertMatch({error, {too_far_below_top, _, _}}, insert_block(ForkBlock)),

    %% But if we add it through sync, it is allowed
    ?assertEqual(ok, insert_block(#{key_block => ForkBlock,
                                    micro_blocks => MicroBlocks,
                                    dir => forward,
                                    add_keyblock => true
                                   })),
    ok.

fork_gen_key_candidate() ->
    %% Make sure a key candidate can be generated when there is a fork
    %% of equal height. Bug found in system tests.
    meck:expect(aec_governance, beneficiary_reward_delay, 0, 4),

    CommonChain = gen_block_chain_with_state_by_target([?GENESIS_TARGET, ?GENESIS_TARGET], 111),
    HardChain = extend_chain_with_state(CommonChain, [1, 1, 1], 111),
    EasyChain = extend_chain_with_state(CommonChain, [2, 2, 2], 222),

    %% Add the easy chain first, then the hard chain
    ok = write_blocks_to_chain(blocks_only_chain(EasyChain)),
    ok = write_blocks_to_chain(blocks_only_chain(HardChain)),

    %% Verify that the hard chain took over
    [TopBlock] = blocks_only_chain([lists:last(HardChain)]),
    TopBlockHash = block_hash(TopBlock),
    ?assertEqual(TopBlockHash, aec_chain:top_block_hash()),

    %% Assert that we can make a new key block candidate.
    {ok, Pubkey} = aec_keys:pubkey(),
    ?assertMatch({ok, _},
                 aec_chain_state:calculate_state_for_new_keyblock(
                   TopBlockHash, Pubkey, Pubkey)),
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
        gen_blocks_only_chain_by_target([?GENESIS_TARGET, ?GENESIS_TARGET, 1, 1], 111),
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
       fun fees_three_beneficiaries/0},
      {"Check reward is delayed",
       fun fees_delayed_reward/0}
     ]
    }.

fees_three_beneficiaries() ->
    meck:expect(aec_governance, beneficiary_reward_delay, 0, 3),

    %% Two accounts to act as sender and receiver.
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret :=_PrivKey2 } = enacl:sign_keypair(),

    PresetAccounts = [{PubKey1, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% Three accounts to act as miners
    #{ public := PubKey3, secret := PrivKey3 } = enacl:sign_keypair(),
    #{ public := PubKey4, secret := PrivKey4 } = enacl:sign_keypair(),
    #{ public := PubKey5, secret := PrivKey5 } = enacl:sign_keypair(),

    %% Three accounts to act as beneficiaries
    #{ public := PubKey6, secret := _PrivKey6 } = enacl:sign_keypair(),
    #{ public := PubKey7, secret := _PrivKey7 } = enacl:sign_keypair(),
    #{ public := PubKey8, secret := _PrivKey8 } = enacl:sign_keypair(),

    %% Add transactions in different micro blocks to collect fees from
    Fee1 = 20000,
    Fee2 = 25000,
    Fee3 = 30000,
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey1, 1, PubKey2, Fee1) ,PrivKey1),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey1, 2, PubKey2, Fee2), PrivKey1),
                     [Tx1, Tx2];
                (2) ->
                     Tx = aec_test_utils:sign_tx(make_spend_tx(PubKey1, 3, PubKey2, Fee3), PrivKey1),
                     [Tx];
                (_) ->
                     []
             end,

    %% Create a chain with the three different miners and beneficiaries
    Miners = [ {PubKey3, PrivKey3}
             , {PubKey4, PrivKey4}
             , {PubKey5, PrivKey5}
             , {PubKey5, PrivKey5}
             , {PubKey5, PrivKey5}
             , {PubKey5, PrivKey5}
             ],
    Beneficiaries = [PubKey6, PubKey7, PubKey8, PubKey8, PubKey8, PubKey8],
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
    ?assertEqual(aec_governance:block_mine_reward(1) + reward_40(Fee1 + Fee2),
                 orddict:fetch(PubKey6, DictBal1)),
    ?assertEqual(aec_governance:block_mine_reward(2) + reward_60(Fee1 + Fee2),
                 orddict:fetch(PubKey7, DictBal1)),
    ?assertEqual(false, orddict:is_key(PubKey5, DictBal1)),

    %% When the last generation is closed, the last transaction fee should
    %% also have been collected.
    ?assertEqual(aec_governance:block_mine_reward(1) + reward_40(Fee1 + Fee2),
                 orddict:fetch(PubKey6, DictBal2)),
    ?assertEqual(aec_governance:block_mine_reward(2) + reward_60(Fee1 + Fee2) + reward_40(Fee3),
                 orddict:fetch(PubKey7, DictBal2)),
    ?assertEqual(aec_governance:block_mine_reward(3) + reward_60(Fee3),
                 orddict:fetch(PubKey8, DictBal2)),

    %% Miners' balances did not change, since beneficiaries took the rewards.
    ?assertEqual(error, orddict:find(PubKey3, DictBal2)),
    ?assertEqual(error, orddict:find(PubKey4, DictBal2)),
    ?assertEqual(error, orddict:find(PubKey5, DictBal2)),
    ok.

fees_delayed_reward() ->
    %% Delay reward by 2 key blocks / generations.
    meck:expect(aec_governance, beneficiary_reward_delay, 0, 2),

    %% Two accounts to act as sender and receiver.
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret :=_PrivKey2 } = enacl:sign_keypair(),

    PresetAccounts = [{PubKey1, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% An account to act as a beneficiary
    #{ public := PubKey3, secret := _PrivKey3 } = enacl:sign_keypair(),

    %% Add transactions in different micro blocks to collect fees from
    Fee1 = 20000,
    Fee2 = 30000,
    Fee3 = 40000,
    TxsFun = fun(1) -> [aec_test_utils:sign_tx(make_spend_tx(PubKey1, 1, PubKey2, Fee1) ,PrivKey1)];
                (2) -> [aec_test_utils:sign_tx(make_spend_tx(PubKey1, 2, PubKey2, Fee2), PrivKey1)];
                (3) -> [aec_test_utils:sign_tx(make_spend_tx(PubKey1, 3, PubKey2, Fee3), PrivKey1)];
                (4) -> [aec_test_utils:sign_tx(make_spend_tx(PubKey1, 4, PubKey2, Fee3), PrivKey1)];
                (_) -> []
             end,

    Miners = [{PubKey1, PrivKey1} || _X <- lists:seq(1, 5)],
    Beneficiaries = [PubKey3 || _X <- lists:seq(1, 5)],
    Chain0 = gen_block_chain_with_state_by_actors(PresetAccounts, Miners, Beneficiaries, TxsFun),
    [KB0, KB1, MB1, KB2, MB2, KB3, MB3, KB4, MB4, KB5] = blocks_only_chain(Chain0),

    MiningReward1 = aec_governance:block_mine_reward(1),
    MiningReward2 = aec_governance:block_mine_reward(2),
    MiningReward3 = aec_governance:block_mine_reward(3),

    %% Write first part of the chain
    ok = write_blocks_to_chain([KB0, KB1, MB1, KB2, MB2, KB3, MB3]),
    Hash1 = aec_chain:top_block_hash(),
    {ok, Balances1} = aec_chain:all_accounts_balances_at_hash(Hash1),
    DictBal1 = orddict:from_list(Balances1),

    %% Check only beneficiary of K1 gets rewards, without any rewards / fees of the next generations
    ?assertEqual({ok, MiningReward1}, orddict:find(PubKey3, DictBal1)),

    %% Insert KB4
    ok = write_blocks_to_chain([KB4]),
    Hash2 = aec_chain:top_block_hash(),
    {ok, Balances2} = aec_chain:all_accounts_balances_at_hash(Hash2),
    DictBal2 = orddict:from_list(Balances2),

    %% Check rewards are granted for the first two key blocks, with fees of first generation
    ?assertEqual({ok, MiningReward1 + MiningReward2 + Fee1},
                 orddict:find(PubKey3, DictBal2)),

    %% Insert the rest of the chain
    ok = write_blocks_to_chain([MB4, KB5]),
    Hash3 = aec_chain:top_block_hash(),
    {ok, Balances3} = aec_chain:all_accounts_balances_at_hash(Hash3),
    DictBal3 = orddict:from_list(Balances3),

    %% Check rewards are granted for the first three key blocks, with fees of first two generations
    ?assertEqual({ok, MiningReward1 + MiningReward2 + MiningReward3 + Fee1 + Fee2},
                 orddict:find(PubKey3, DictBal3)),
    ok.

%%%===================================================================
%%% PoF test

pof_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             aec_test_utils:stop_chain_db(),
             teardown_meck_and_keys(TmpDir)
     end,
     [{"Check pof is recognized on key-block as parent",
       fun pof_fork_on_key_block/0},
      {"Check pof is recognized on micro-block as parent",
       fun pof_fork_on_micro_block/0},
      {"Check pof can be reported late in generation",
       fun pof_reported_late/0}
     ]
    }.

pof_fork_on_key_block() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAccounts = [{PubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% Create main chain
    TxsFun = fun(1) -> [aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000, 2), PrivKey)];
                (_) -> []
             end,
    [B0, B1, _B2] = Chain0 = gen_block_chain_with_state_by_target(
                               PresetAccounts, [?HIGHEST_TARGET_SCI], 1, TxsFun),
    Chain = [_KB0, KB1, MB1] = blocks_only_chain(Chain0),

    %% Create fork, which starts on a key-block
    CommonChain = [B0, B1],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000, 3), PrivKey)],
    Fork = aec_test_utils:extend_block_chain_with_micro_blocks(CommonChain, Txs),
    [_, _, MB2] = blocks_only_chain(Fork),

    %% Write initial chain
    ok = write_blocks_to_chain(Chain),

    %% Write micro-block, and recognize a fraud
    FraudHeader1 = aec_blocks:to_header(MB2),
    FraudHeader2 = aec_blocks:to_header(MB1),
    {pof, PoF} = insert_block(MB2),
    ?assertEqual(FraudHeader1, aec_pof:header1(PoF)),
    ?assertEqual(FraudHeader2, aec_pof:header2(PoF)),
    ?assertEqual(aec_blocks:miner(KB1), aec_pof:pubkey(PoF)),

    ok.

pof_fork_on_micro_block() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAccounts = [{PubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% Create main chain
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000, 2), PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, 20000, 2), PrivKey),
                     [Tx1, Tx2];
                (_) ->
                     []
             end,
    [B0, B1, B2, _] = Chain0 = gen_block_chain_with_state_by_target(
                               PresetAccounts, [?HIGHEST_TARGET_SCI], 1, TxsFun),
    Chain = [_KB0, KB1, _MB1, MB2] = blocks_only_chain(Chain0),

    %% Create fork, which starts on a micro-block
    CommonChain = [B0, B1, B2],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, 20000, 3), PrivKey)],
    Fork = aec_test_utils:extend_block_chain_with_micro_blocks(CommonChain, Txs, 1),
    [_, _, _, MB3] = blocks_only_chain(Fork),

    %% Write initial chain
    ok = write_blocks_to_chain(Chain),

    %% Write micro-block, and recognize a fraud
    FraudHeader1 = aec_blocks:to_header(MB3),
    FraudHeader2 = aec_blocks:to_header(MB2),
    {pof, PoF} = insert_block(MB3),
    ?assertEqual(FraudHeader1, aec_pof:header1(PoF)),
    ?assertEqual(FraudHeader2, aec_pof:header2(PoF)),
    ?assertEqual(aec_blocks:miner(KB1), aec_pof:pubkey(PoF)),

    ok.

pof_reported_late() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAccounts = [{PubKey, 1000000}],
    meck:expect(aec_genesis_block_settings, preset_accounts, 0, PresetAccounts),

    %% Create main chain
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000, 1), PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, 20000, 1), PrivKey),
                     [Tx1, Tx2];
                (2) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 3, PubKey, 20000, 1), PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 4, PubKey, 20000, 1), PrivKey),
                     Tx3 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 5, PubKey, 20000, 1), PrivKey),
                     [Tx1, Tx2, Tx3]
             end,
    [B0, B1, B2|_] = Chain0 =
        gen_block_chain_with_state_by_target(
          PresetAccounts, [?HIGHEST_TARGET_SCI, ?HIGHEST_TARGET_SCI], 1, TxsFun),

    [ KB0, KB1, MB1, MB2, KB2, MB3, MB4, MB5] = blocks_only_chain(Chain0),
    Chain1 = [KB0, KB1, MB1, MB2, KB2, MB3],

    %% Create fork, which starts on a micro-block
    CommonChain = [B0, B1, B2],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, 20000, 3), PrivKey)],
    Fork = aec_test_utils:extend_block_chain_with_micro_blocks(CommonChain, Txs, 1),
    [_, _, _, FraudMB] = blocks_only_chain(Fork),

    %% Write initial chain
    ok = write_blocks_to_chain(Chain1),

    %% Write micro-block, and recognize a fraud
    FraudHeader1 = aec_blocks:to_header(FraudMB),
    FraudHeader2 = aec_blocks:to_header(MB2),
    {pof, PoF} = insert_block(FraudMB),
    ?assertEqual(FraudHeader1, aec_pof:header1(PoF)),
    ?assertEqual(FraudHeader2, aec_pof:header2(PoF)),
    ?assertEqual(aec_blocks:miner(KB1), aec_pof:pubkey(PoF)),

    %% Report the fraud in second block of the generation
    {ok, MinerPrivKey} = aec_keys:sign_privkey(),
    ReportMB4 = aec_test_utils:sign_micro_block(aec_blocks:set_pof(MB4, PoF), MinerPrivKey),
    ?assertEqual(ok, insert_block(ReportMB4)),

    %% Make sure it cannot be reported in the next micro block
    ReportMB4Hash = block_hash(ReportMB4),
    ReportMB5 = aec_blocks:set_prev_hash(aec_blocks:set_pof(MB5, PoF),
                                         ReportMB4Hash),
    ReportMB5Signed = aec_test_utils:sign_micro_block(ReportMB5, MinerPrivKey),
    ReportMB5Hash = block_hash(ReportMB5Signed),
    ?assertEqual({error, {double_reported_fraud, ReportMB5Hash}},
                 insert_block(ReportMB5Signed)),
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
    make_spend_tx(Sender, SenderNonce, Recipient, 20000).

make_spend_tx(Sender, SenderNonce, Recipient, Fee) ->
    make_spend_tx(Sender, SenderNonce, Recipient, Fee, 1).

make_spend_tx(Sender, SenderNonce, Recipient, Fee, Amount) ->
    SenderId = aec_id:create(account, Sender),
    RecipientId = aec_id:create(account, Recipient),
    {ok, SpendTx} = aec_spend_tx:new(#{sender_id => SenderId,
                                       recipient_id => RecipientId,
                                       amount => Amount,
                                       fee => Fee,
                                       nonce => SenderNonce,
                                       payload => <<>>}),
    SpendTx.

reward_40(Fee) -> Fee * 4 div 10.

reward_60(Fee) -> Fee - reward_40(Fee).

