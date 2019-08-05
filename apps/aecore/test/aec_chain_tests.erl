%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aeminer/include/aeminer.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-import(aec_test_utils,
        [ extend_block_chain_with_state/2
        , blocks_only_chain/1
        , genesis_block/0
        , genesis_block_with_state/1
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
        ?assertEqual(aec_blocks:serialize_to_binary(element(2,B1)),
                     aec_blocks:serialize_to_binary(element(2,B2)))).

-define(FACTOR, 1000000000).
-define(GENESIS_TARGET, 553713663).
-define(GENESIS_HEIGHT, aec_block_genesis:height()).

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
             aec_test_utils:mock_genesis_and_forks(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis_and_forks(),
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
             aec_test_utils:mock_genesis_and_forks(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis_and_forks(),
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
    PresetAccounts = [{PubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
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
    PresetAccounts = [{SenderPubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
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
    PresetAccounts = [{SenderPubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    Spend = aec_test_utils:sign_tx(make_spend_tx(SenderPubKey, 1, RecipientPubKey), SenderPrivKey),

    Chain0 = gen_block_chain_with_state_by_target(PresetAccounts, [?GENESIS_TARGET], 111),

    TxsFun = fun(_) -> [Spend] end,
    [B0, B1, B2, MB2] = blocks_only_chain(extend_chain_with_state(Chain0, [?GENESIS_TARGET], 111, TxsFun)),

    %% Insert up to last block.
    ok = write_blocks_to_chain([B0, B1, B2]),

    %% Add invalid transaction with too high nonce to last block
    Txs = aec_blocks:txs(MB2),
    BogusSpendTx = aec_test_utils:signed_spend_tx(
                     #{recipient_id => aeser_id:create(account, <<1:32/unit:8>>),
                       amount => 0,
                       fee => 18000 * aec_test_utils:min_gas_price(),
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
    PresetAccounts = [{SenderPubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
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
             meck:expect(aec_governance, key_blocks_to_check_difficulty_count, 0, 2),
             meck:expect(aec_governance, expected_block_mine_rate, 0, 1800000), %% 50 mins
             aec_test_utils:mock_genesis_and_forks(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:unmock_difficulty_as_target(),
             meck:unload(aec_governance),
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:unmock_genesis_and_forks(),
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
     , {"Fork on old fork point", fun fork_on_old_fork_point/0}]}.

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
    PresetAccounts = [{SenderPubKey, 100000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
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
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

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
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

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

    %% But if we add the blocks through sync, it is allowed
    Res = [ insert_block(B, block_synced) || B <- [ForkBlock | MicroBlocks] ],
    ?assertEqual([ok], lists:usort(Res)),
    ok.


%%% Generate candidate from top with high fork
fork_gen_key_candidate_split_disabled_test_() ->
    {setup,
     fun () ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:dev_reward_setup(false, true, 100),
             setup_meck_and_keys()
     end,
     fun (TmpDir) ->
             teardown_meck_and_keys(TmpDir),
             aec_test_utils:stop_chain_db()
     end,
     fun fork_gen_key_candidate/0}.

%%% Generate candidate from top with high fork
fork_gen_key_candidate_split_enabled_test_() ->
    {setup,
     fun () ->
             aec_test_utils:start_chain_db(),
             aec_test_utils:dev_reward_setup(true, true, 100),
             setup_meck_and_keys()
     end,
     fun (TmpDir) ->
             teardown_meck_and_keys(TmpDir),
             aec_test_utils:stop_chain_db()
     end,
     fun fork_gen_key_candidate/0}.


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

fees_test_setup(Enabled, Activated, BeneficiaryShare) ->
    aec_test_utils:dev_reward_setup(Enabled, Activated, BeneficiaryShare),
    aec_test_utils:start_chain_db(),
    setup_meck_and_keys().

fees_test_teardown(TmpDir) ->
    aec_test_utils:stop_chain_db(),
    teardown_meck_and_keys(TmpDir).


%%% Check fee division between three beneficiaries with reward split
fees_three_beneficiaries_with_split_test_() ->
    {setup,
     fun() -> fees_test_setup(true, true, 100) end,
     fun fees_test_teardown/1,
     fun fees_three_beneficiaries/0}.

%%% Check fee division between three beneficiaries without reward split
fees_three_beneficiaries_without_split_test_() ->
    {setup,
     fun() -> fees_test_setup(false, true, 100) end,
     fun fees_test_teardown/1,
     fun fees_three_beneficiaries/0}.


fees_three_beneficiaries() ->
    meck:expect(aec_governance, beneficiary_reward_delay, 0, 3),

    %% Two accounts to act as sender and receiver.
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret :=_PrivKey2 } = enacl:sign_keypair(),

    PresetAccounts = [{PubKey1, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

    %% Three accounts to act as miners
    #{ public := PubKey3, secret := PrivKey3 } = enacl:sign_keypair(),
    #{ public := PubKey4, secret := PrivKey4 } = enacl:sign_keypair(),
    #{ public := PubKey5, secret := PrivKey5 } = enacl:sign_keypair(),

    %% Three accounts to act as beneficiaries
    #{ public := PubKey6, secret := _PrivKey6 } = enacl:sign_keypair(),
    #{ public := PubKey7, secret := _PrivKey7 } = enacl:sign_keypair(),
    #{ public := PubKey8, secret := _PrivKey8 } = enacl:sign_keypair(),

    %% Add transactions in different micro blocks to collect fees from
    Fee1 = 20000 * aec_test_utils:min_gas_price(),
    Fee2 = 25000 * aec_test_utils:min_gas_price(),
    Fee3 = 30000 * aec_test_utils:min_gas_price(),
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
    ?assertEqual(split_reward(aec_governance:block_mine_reward(1) + reward_40(Fee1 + Fee2)),
                 orddict:fetch(PubKey6, DictBal1)),
    ?assertEqual(split_reward(aec_governance:block_mine_reward(2) + reward_60(Fee1 + Fee2)),
                 orddict:fetch(PubKey7, DictBal1)),
    ?assertEqual(false, orddict:is_key(PubKey5, DictBal1)),

    %% When the last generation is closed, the last transaction fee should
    %% also have been collected.
    ?assertEqual(split_reward(aec_governance:block_mine_reward(1) + reward_40(Fee1 + Fee2)),
                 orddict:fetch(PubKey6, DictBal2)),
    ?assertEqual(split_reward(aec_governance:block_mine_reward(2) + reward_60(Fee1 + Fee2)) + split_reward(reward_40(Fee3)),
                 orddict:fetch(PubKey7, DictBal2)),
    ?assertEqual(split_reward(aec_governance:block_mine_reward(3) + reward_60(Fee3)),
                 orddict:fetch(PubKey8, DictBal2)),

    [{PubKeyProtocol, _}] = aec_dev_reward:beneficiaries(),
    case aec_dev_reward:enabled() of
        true ->
            TotalRewards = aec_governance:block_mine_reward(1) + aec_governance:block_mine_reward(2) + aec_governance:block_mine_reward(3)
                + Fee1 + Fee2 + Fee3,
            ProtocolBenefits = TotalRewards - split_reward(TotalRewards),
            ?assertEqual(ProtocolBenefits, orddict:fetch(PubKeyProtocol, DictBal2));
        false ->
            ?assertEqual(false, orddict:is_key(PubKeyProtocol, DictBal2))
    end,

    %% Miners' balances did not change, since beneficiaries took the rewards.
    ?assertEqual(error, orddict:find(PubKey3, DictBal2)),
    ?assertEqual(error, orddict:find(PubKey4, DictBal2)),
    ?assertEqual(error, orddict:find(PubKey5, DictBal2)),
    ok.


%%% Check reward is delayed with reward split
fees_delayed_reward_with_split_test_() ->
    {setup,
     fun() -> fees_test_setup(true, true, 100) end,
     fun fees_test_teardown/1,
     fun fees_delayed_reward/0}.

%%% Check reward is delayed without reward split
fees_delayed_reward_without_split_test_() ->
    {setup,
     fun() -> fees_test_setup(false, true, 100) end,
     fun fees_test_teardown/1,
     fun fees_delayed_reward/0}.


fees_delayed_reward() ->
    %% Delay reward by 2 key blocks / generations.
    meck:expect(aec_governance, beneficiary_reward_delay, 0, 2),

    %% Two accounts to act as sender and receiver.
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret :=_PrivKey2 } = enacl:sign_keypair(),

    PresetAccounts = [{PubKey1, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

    %% An account to act as a beneficiary
    #{ public := PubKey3, secret := _PrivKey3 } = enacl:sign_keypair(),

    %% Add transactions in different micro blocks to collect fees from
    Fee1 = 20000 * aec_test_utils:min_gas_price(),
    Fee2 = 30000 * aec_test_utils:min_gas_price(),
    Fee3 = 40000 * aec_test_utils:min_gas_price(),
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
    ?assertEqual({ok, split_reward(MiningReward1)}, orddict:find(PubKey3, DictBal1)),

    %% Insert KB4
    ok = write_blocks_to_chain([KB4]),
    Hash2 = aec_chain:top_block_hash(),
    {ok, Balances2} = aec_chain:all_accounts_balances_at_hash(Hash2),
    DictBal2 = orddict:from_list(Balances2),

    %% Check rewards are granted for the first two key blocks, with fees of first generation
    ?assertEqual({ok, split_reward(MiningReward1 + MiningReward2 + Fee1)},
                 orddict:find(PubKey3, DictBal2)),

    %% Insert the rest of the chain
    ok = write_blocks_to_chain([MB4, KB5]),
    Hash3 = aec_chain:top_block_hash(),
    {ok, Balances3} = aec_chain:all_accounts_balances_at_hash(Hash3),
    DictBal3 = orddict:from_list(Balances3),

    %% Check rewards are granted for the first three key blocks, with fees of first two generations
    ?assertEqual({ok, split_reward(MiningReward1 + MiningReward2 + MiningReward3 + Fee1 + Fee2)},
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
    PresetAccounts = [{PubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

    %% Create main chain
    TxsFun = fun(1) -> [aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000 * aec_test_utils:min_gas_price(), 2), PrivKey)];
                (_) -> []
             end,
    [B0, B1, _B2] = Chain0 = gen_block_chain_with_state_by_target(
                               PresetAccounts, [?HIGHEST_TARGET_SCI], 1, TxsFun),
    Chain = [_KB0, KB1, MB1] = blocks_only_chain(Chain0),

    %% Create fork, which starts on a key-block
    CommonChain = [B0, B1],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, 20000 * aec_test_utils:min_gas_price(), 3), PrivKey)],
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
    PresetAccounts = [{PubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),

    %% Create main chain
    Fee = 20000 * aec_test_utils:min_gas_price(),
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, Fee, 2),
                                                  PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, Fee, 2),
                                                  PrivKey),
                     [Tx1, Tx2];
                (_) ->
                     []
             end,
    [B0, B1, B2, _] = Chain0 = gen_block_chain_with_state_by_target(
                               PresetAccounts, [?HIGHEST_TARGET_SCI], 1, TxsFun),
    Chain = [_KB0, KB1, _MB1, MB2] = blocks_only_chain(Chain0),

    %% Create fork, which starts on a micro-block
    CommonChain = [B0, B1, B2],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, Fee, 3), PrivKey)],
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
    PresetAccounts = [{PubKey, 1000000 * aec_test_utils:min_gas_price()}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    Fee = 20000 * aec_test_utils:min_gas_price(),

    %% Create main chain
    TxsFun = fun(1) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 1, PubKey, Fee, 1), PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, Fee, 1), PrivKey),
                     [Tx1, Tx2];
                (2) ->
                     Tx1 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 3, PubKey, Fee, 1), PrivKey),
                     Tx2 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 4, PubKey, Fee, 1), PrivKey),
                     Tx3 = aec_test_utils:sign_tx(make_spend_tx(PubKey, 5, PubKey, Fee, 1), PrivKey),
                     [Tx1, Tx2, Tx3]
             end,
    [B0, B1, B2|_] = Chain0 =
        gen_block_chain_with_state_by_target(
          PresetAccounts, [?HIGHEST_TARGET_SCI, ?HIGHEST_TARGET_SCI], 1, TxsFun),

    [ KB0, KB1, MB1, MB2, KB2, MB3, MB4, MB5] = blocks_only_chain(Chain0),
    Chain1 = [KB0, KB1, MB1, MB2, KB2, MB3],

    %% Create fork, which starts on a micro-block
    CommonChain = [B0, B1, B2],
    Txs = [aec_test_utils:sign_tx(make_spend_tx(PubKey, 2, PubKey, Fee, 3), PrivKey)],
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
%%% Sum of token supply test

token_supply_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             aec_test_utils:stop_chain_db(),
             teardown_meck_and_keys(TmpDir)
     end,
     [ {"Test sum of coinbase", fun token_supply_coinbase/0}
     , {"Test sum of spend", fun token_supply_spend/0}
     , {"Test sum of oracles", fun token_supply_oracles/0}
     , {"Test sum of channels", fun token_supply_channels/0}
     , {"Test sum of contracts", fun token_supply_contracts/0}
     ] ++
         [{"Test sum of generalized accounts", fun token_supply_ga/0}
          || aect_test_utils:latest_protocol_version() >= ?FORTUNA_PROTOCOL_VSN
         ]
    }.

token_supply_coinbase() ->
    TestHeight = 30,
    Delay = 10,
    PubKey = <<12345:256>>,
    PresetAmount = 1000000,
    PresetAccounts = [{PubKey, PresetAmount}],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, Delay),
    Targets = lists:duplicate(TestHeight, ?GENESIS_TARGET),
    Chain = gen_blocks_only_chain_by_target(PresetAccounts, Targets, 111),
    ok = write_blocks_to_chain(Chain),
    Heights = lists:seq(1, TestHeight),
    lists:foldl(
      fun(Height, Acc) ->
              {ok, Map} = aec_chain:sum_tokens_at_height(Height),
              Coinbase = aec_coinbase:coinbase_at_height(Height),
              PaidOut = aec_coinbase:coinbase_at_height(max(Height - Delay, 0)),
              ?assertEqual(maps:get(pending_rewards, Acc) + Coinbase,
                           maps:get(pending_rewards, Map) + PaidOut),
              ?assertEqual(maps:get(accounts, Acc) + PaidOut,
                           maps:get(accounts, Map)),
              ?assertEqual(maps:get(total, Acc) + Coinbase,
                           maps:get(total, Map)),
              Map
      end, #{accounts => PresetAmount,
             pending_rewards => 0,
             total => PresetAmount
            }, Heights).

token_supply_spend() ->
    TestHeight = 20,
    Delay = 10,
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAmount = 1000000 * aec_test_utils:min_gas_price(),
    PresetAccounts = [{PubKey, PresetAmount}],
    SpendFee = 100000 * aec_test_utils:min_gas_price(),
    SpendAmount = 3000,
    TxFun1 =
        fun(N, Receiver) ->
                make_spend_tx(PubKey, N, Receiver, SpendFee, SpendAmount)
        end,
    TxsFun = fun(1) -> [aec_test_utils:sign_tx(TxFun1(1, <<1:256>>), PrivKey)];
                (2) -> [aec_test_utils:sign_tx(TxFun1(2, <<2:256>>), PrivKey)];
                (3) -> [aec_test_utils:sign_tx(TxFun1(3, <<3:256>>), PrivKey)];
                (4) -> [aec_test_utils:sign_tx(TxFun1(4, <<4:256>>), PrivKey)];
                (5) -> [aec_test_utils:sign_tx(TxFun1(5, <<5:256>>), PrivKey)];
                (_) -> []
             end,
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, Delay),
    Targets = lists:duplicate(TestHeight, ?GENESIS_TARGET),
    Chain = gen_blocks_only_chain_by_target(PresetAccounts, Targets, 111, TxsFun),
    ok = write_blocks_to_chain(Chain),
    Heights = lists:seq(1, TestHeight),
    lists:foldl(
      fun(Height, Acc) ->
              {ok, Map} = aec_chain:sum_tokens_at_height(Height),
              Coinbase = aec_coinbase:coinbase_at_height(Height),
              RewardHeight = max(Height - Delay, 0),
              OldFees = case RewardHeight > 1 andalso RewardHeight < 7 of
                            true -> SpendFee;
                            false -> 0
                        end,
              NewFees = case Height > 1 andalso Height < 7 of
                            true -> SpendFee;
                            false -> 0
                        end,
              OldCoinbase = aec_coinbase:coinbase_at_height(RewardHeight),
              ExpectedPendingDiff = Coinbase + NewFees - OldCoinbase - OldFees,
              ?assertEqual(maps:get(pending_rewards, Acc) + ExpectedPendingDiff,
                           maps:get(pending_rewards, Map)),
              ExpectedAccountDiff = OldFees + OldCoinbase - NewFees,
              ?assertEqual(maps:get(accounts, Acc) + ExpectedAccountDiff,
                           maps:get(accounts, Map)),
              ?assertEqual(maps:get(total, Acc) + Coinbase,
                           maps:get(total, Map)),
              Map
      end, #{accounts => PresetAmount,
             pending_rewards => 0,
             total => PresetAmount
            }, Heights).

token_supply_oracles() ->
    TestHeight = 20,
    %% We don't want to care about coinbase this time.
    Delay = 1000,
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret := PrivKey2 } = enacl:sign_keypair(),
    PresetAmount = 10000000 * aec_test_utils:min_gas_price(),
    PresetAccounts = [{PubKey1, PresetAmount}, {PubKey2, PresetAmount}],
    Fee  = 100000 * aec_test_utils:min_gas_price(),
    QFee = 100000,
    RegisterFun =
        fun(Address, Nonce) ->
                make_oracle_register_tx(Address, Nonce, Fee, QFee)
        end,
    QueryFun =
        fun(From, To, Nonce) ->
                make_oracle_query_tx(From, To, Nonce, Fee, QFee)
        end,
    ResponseFun =
        fun(From, QueryId, Nonce) ->
                make_oracle_response_tx(From, Nonce, Fee, QueryId)
        end,
    QId = aeo_query:id(PubKey1, 2, PubKey2),
    TxsFun = fun(1) -> [aec_test_utils:sign_tx(RegisterFun(PubKey1, 1), PrivKey1)];
                (2) -> [aec_test_utils:sign_tx(RegisterFun(PubKey2, 1), PrivKey2)];
                (3) -> [aec_test_utils:sign_tx(QueryFun(PubKey1, PubKey2, 2), PrivKey1)];
                (4) -> [aec_test_utils:sign_tx(ResponseFun(PubKey2, QId, 2), PrivKey2)];
                (_) -> []
             end,
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, Delay),
    Targets = lists:duplicate(TestHeight, ?GENESIS_TARGET),
    Chain = gen_blocks_only_chain_by_target(PresetAccounts, Targets, 111, TxsFun),
    ok = write_blocks_to_chain(Chain),

    %% Only presets
    {ok, Map1} = aec_chain:sum_tokens_at_height(1),
    Oracle1 = 0,
    ?assertEqual(PresetAmount * 2, maps:get(accounts, Map1)),
    ?assertEqual(Oracle1, maps:get(oracles, Map1)),
    ?assertEqual(0, maps:get(oracles, Map1)),

    %% One account registered as oracle.
    {ok, Map2} = aec_chain:sum_tokens_at_height(2),
    Oracle2 = Oracle1 + PresetAmount - Fee,
    ?assertEqual(PresetAmount, maps:get(accounts, Map2)),
    ?assertEqual(Oracle2, maps:get(oracles, Map2)),

    %% Both accounts registered as oracles.
    {ok, Map3} = aec_chain:sum_tokens_at_height(3),
    Oracle3 = Oracle2 + PresetAmount - Fee,
    ?assertEqual(0, maps:get(accounts, Map3)),
    ?assertEqual(Oracle3, maps:get(oracles, Map3)),

    %% One query is pending
    {ok, Map4} = aec_chain:sum_tokens_at_height(4),
    Oracle4 = Oracle3 - Fee - QFee,
    ?assertEqual(0, maps:get(accounts, Map4)),
    ?assertEqual(Oracle4, maps:get(oracles, Map4)),
    ?assertEqual(QFee, maps:get(oracle_queries, Map4)),

    %% The query is responded
    {ok, Map5} = aec_chain:sum_tokens_at_height(5),
    Oracle5 = Oracle4 - Fee + QFee,
    ?assertEqual(0, maps:get(accounts, Map5)),
    ?assertEqual(Oracle5, maps:get(oracles, Map5)),
    ?assertEqual(0, maps:get(oracle_queries, Map5)),

    ok.

token_supply_channels() ->
    TestHeight = 10,
    %% We don't want to care about coinbase this time.
    Delay = 1000,
    #{ public := PubKey1, secret := PrivKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2, secret := PrivKey2 } = enacl:sign_keypair(),
    PresetAmount = 10000000 * aec_test_utils:min_gas_price(),
    PresetAccounts = [{PubKey1, PresetAmount}, {PubKey2, PresetAmount}],
    Fee  = 100000 * aec_test_utils:min_gas_price(),
    StartAmount = 100000 * aec_test_utils:min_gas_price(),
    CloseAmount = 50000 * aec_test_utils:min_gas_price(),
    CreateNonce = 1,
    CloseNonce = 2,
    StartChannelFun =
        fun() ->
                make_channel_create_tx(PubKey1, CreateNonce, PubKey2, StartAmount, Fee)
        end,
    ChannelPubkey = aesc_channels:pubkey(PubKey1, CreateNonce, PubKey2),
    ChannelId = aeser_id:create(channel, ChannelPubkey),
    CloseMutualFun =
        fun() ->
                make_channel_close_mutual_tx(PubKey1, CloseNonce, ChannelId, CloseAmount, Fee)
        end,
    TxsFun = fun(1) -> [aec_test_utils:sign_tx(StartChannelFun(),
                                               [PrivKey1, PrivKey2])];
                (2) -> [aec_test_utils:sign_tx(CloseMutualFun(),
                                               [PrivKey1, PrivKey2])];
                (_) -> []
             end,
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, Delay),
    Targets = lists:duplicate(TestHeight, ?GENESIS_TARGET),
    Chain = gen_blocks_only_chain_by_target(PresetAccounts, Targets, 111, TxsFun),
    ok = write_blocks_to_chain(Chain),

    %% Only presets
    {ok, Map1} = aec_chain:sum_tokens_at_height(1),
    ?assertEqual(PresetAmount * 2, maps:get(accounts, Map1)),
    ?assertEqual(0, maps:get(channels, Map1)),

    %% The channel is created
    {ok, Map2} = aec_chain:sum_tokens_at_height(2),
    ChannelAmount = StartAmount + StartAmount,
    ExpectedAccounts1 = 2 * PresetAmount - ChannelAmount - Fee,
    ?assertEqual(ExpectedAccounts1, maps:get(accounts, Map2)),
    ?assertEqual(ChannelAmount, maps:get(channels, Map2)),

    %% The channel is closed
    {ok, Map3} = aec_chain:sum_tokens_at_height(3),
    ExpectedAccounts2 = ExpectedAccounts1 + CloseAmount + CloseAmount,
    ExpectedLockedAmount = StartAmount * 2 - CloseAmount * 2 - Fee,
    ?assertEqual(ExpectedAccounts2, maps:get(accounts, Map3)),
    ?assertEqual(0, maps:get(channels, Map3)),
    ?assertEqual(ExpectedLockedAmount, maps:get(locked, Map3)),

    ok.

token_supply_contracts() ->
    TestHeight = 10,
    %% We don't want to care about coinbase this time.
    Delay = 5,
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAmount = 10000000 * aec_test_utils:min_gas_price(),
    PresetAccounts = [{PubKey, PresetAmount}],
    Deposit = 1000,
    Amount  = 3000,
    Fee     = 100000 * aec_test_utils:min_gas_price(),
    Gas     = 10000,
    GasPrice = aec_test_utils:min_gas_price(),
    {ok, Contract} = aect_test_utils:read_contract(identity),
    {ok, Code}     = aect_test_utils:compile_contract(identity),
    {ok, InitCallData} = aect_test_utils:encode_call_data(Contract, <<"init">>, []),
    CreateTx = make_contract_create_tx(PubKey, Code, InitCallData, 1,
                                       Deposit, Amount, Fee, Gas, GasPrice),
    SCreateTx = aec_test_utils:sign_tx(CreateTx, [PrivKey]),
    TxsFun = fun(1) -> [SCreateTx];
                (_) -> []
             end,
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, Delay),
    Targets = lists:duplicate(TestHeight, ?GENESIS_TARGET),
    Chain = gen_blocks_only_chain_by_target(PresetAccounts, Targets, 111, TxsFun),
    ok = write_blocks_to_chain(Chain),

    %% Only presets
    {ok, Map1} = aec_chain:sum_tokens_at_height(1),
    ?assertEqual(PresetAmount, maps:get(accounts, Map1)),
    ?assertEqual(0, maps:get(contracts, Map1)),

    %% The contract is created
    GasFee     = get_used_gas_fee(SCreateTx),
    {ok, Map2} = aec_chain:sum_tokens_at_height(2),
    ?assertEqual(PresetAmount - GasFee - Fee - Amount - Deposit, maps:get(accounts, Map2)),
    ?assertEqual(Amount + Deposit, maps:get(contracts, Map2)),

    ok.

token_supply_ga() ->
    TestHeight = 10,
    Delay = 5,
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    PresetAmount = 10000000 * aec_test_utils:min_gas_price(),
    PresetAccounts = [{PubKey, PresetAmount}],
    Fee     = 100000 * aec_test_utils:min_gas_price(),
    Gas     = 1000,
    GasPrice = aec_test_utils:min_gas_price(),
    {ok, CodeMap} = aega_test_utils:get_contract("simple_auth.aes"),
    #{ bytecode := ByteCode
     , map      := #{type_info := TypeInfo}
     , src      := Src} = CodeMap,
    {ok, InitCallData} = aect_test_utils:encode_call_data(list_to_binary(Src), <<"init">>, [<<"123">>]),
    {ok, MetaCallData} = aect_test_utils:encode_call_data(list_to_binary(Src), <<"authorize">>, [<<"123">>, <<"1">>]),
    {ok, AuthFunHash}  = aeb_aevm_abi:type_hash_from_function_name(<<"authorize">>, TypeInfo),
    AttachTx = make_ga_attach_tx(PubKey, ByteCode, AuthFunHash, InitCallData, 1,
                                 Fee, Gas, GasPrice),
    SAttachTx = aec_test_utils:sign_tx(AttachTx, [PrivKey]),
    SpendTx = make_spend_tx(PubKey, 0, <<4711:256>>, Fee),
    InnerTx = aetx_sign:new(SpendTx, []),
    MetaTx  = make_ga_meta_tx(PubKey, MetaCallData, InnerTx, Fee, Gas, GasPrice),
    SMetaTx = aetx_sign:new(MetaTx, []),
    TxsFun = fun(1) -> [SAttachTx];
                (2) -> [SMetaTx];
                (_) -> []
             end,
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, Delay),
    Targets = lists:duplicate(TestHeight, ?GENESIS_TARGET),
    Chain = gen_blocks_only_chain_by_target(PresetAccounts, Targets, 111, TxsFun),
    ok = write_blocks_to_chain(Chain),

    AttachFee    = aetx:fee(AttachTx),
    MetaFee      = aetx:fee(MetaTx),
    SpendFee     = aetx:fee(SpendTx),
    {ok, H3}     = aec_chain:get_key_header_by_height(3),
    {ok, Trees2} = aec_chain:get_block_state(aec_headers:prev_hash(H3)),
    DeepFee      = aetx:deep_fee(MetaTx, Trees2),

    %% Check the assumptions on the fees for GA attach and meta
    ?assertEqual(AttachFee, Fee),
    ?assertEqual(MetaFee, Fee),
    ?assertEqual(SpendFee, Fee),
    ?assertEqual(DeepFee, MetaFee + SpendFee),

    %% Find the used gas and gas price
    AttachGasFee = get_used_gas_fee(SAttachTx),
    MetaGasFee = get_used_gas_fee(SMetaTx),

    %% Check that the fees for GA attach and meta are put as pending.
    {ok, #{pending_rewards := Pending1}} = aec_chain:sum_tokens_at_height(1),
    {ok, #{pending_rewards := Pending2}} = aec_chain:sum_tokens_at_height(2),
    {ok, #{pending_rewards := Pending3}} = aec_chain:sum_tokens_at_height(3),

    Coinbase1 = aec_coinbase:coinbase_at_height(1),
    Coinbase2 = aec_coinbase:coinbase_at_height(2),
    Coinbase3 = aec_coinbase:coinbase_at_height(3),

    Reward1 = Pending2 - Pending1,
    Reward2 = Pending3 - Pending2,

    ?assertEqual(Pending1, Coinbase1),
    ?assertEqual(Reward1, Coinbase2 + AttachFee + AttachGasFee),
    ?assertEqual(Reward2, Coinbase3 + DeepFee + MetaGasFee),

    %% Check that the fees for GA attach and meta are given to the miner.
    RewardHeight1 = 1 + Delay,
    RewardHeight2 = 2 + Delay,
    RewardHeight3 = 3 + Delay,

    {ok, #{pending_rewards := PendingReward1}} = aec_chain:sum_tokens_at_height(RewardHeight1),
    {ok, #{pending_rewards := PendingReward2}} = aec_chain:sum_tokens_at_height(RewardHeight2),
    {ok, #{pending_rewards := PendingReward3}} = aec_chain:sum_tokens_at_height(RewardHeight3),

    ?assertEqual(PendingReward2, (PendingReward1
                                  + aec_coinbase:coinbase_at_height(RewardHeight2)
                                  - Reward1)),
    ?assertEqual(PendingReward3, (PendingReward2
                                  + aec_coinbase:coinbase_at_height(RewardHeight3)
                                  - Reward2)),

    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_meck_and_keys() ->
    aec_test_utils:mock_difficulty_as_target(),
    aec_test_utils:mock_governance(),
    aec_test_utils:mock_genesis_and_forks(),
    aec_test_utils:aec_keys_setup().

teardown_meck_and_keys(TmpDir) ->
    aec_test_utils:unmock_difficulty_as_target(),
    aec_test_utils:unmock_governance(),
    aec_test_utils:unmock_genesis_and_forks(),
    aec_test_utils:aec_keys_cleanup(TmpDir).

write_blocks_to_chain([H|T]) ->
    ok = insert_block(H),
    write_blocks_to_chain(T);
write_blocks_to_chain([]) ->
    ok.

gen_blocks_only_chain(Data) ->
    gen_blocks_only_chain(aec_test_utils:genesis_accounts(), Data).

gen_blocks_only_chain(PresetAccounts, Data) ->
    blocks_only_chain(gen_block_chain_with_state(PresetAccounts, Data)).

gen_block_chain_with_state(PresetAccounts, Data) ->
    {B0, S0} = genesis_block_with_state(PresetAccounts),
    extend_block_chain_with_state([{B0, S0}], Data).

gen_blocks_only_chain_by_target(Targets, Nonce) ->
    gen_blocks_only_chain_by_target(aec_test_utils:genesis_accounts(), Targets, Nonce).

gen_blocks_only_chain_by_target(PresetAccounts, Targets, Nonce) ->
    blocks_only_chain(gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce)).

gen_blocks_only_chain_by_target(PresetAccounts, Targets, Nonce, TxsFun) ->
    blocks_only_chain(gen_block_chain_with_state_by_target(PresetAccounts, Targets, Nonce, TxsFun)).

gen_block_chain_with_state_by_target(Targets, Nonce) ->
    gen_block_chain_with_state_by_target(aec_test_utils:genesis_accounts(), Targets, Nonce).

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
    make_spend_tx(Sender, SenderNonce, Recipient, 20000 * aec_test_utils:min_gas_price()).

make_spend_tx(Sender, SenderNonce, Recipient, Fee) ->
    make_spend_tx(Sender, SenderNonce, Recipient, Fee, 1).

make_spend_tx(Sender, SenderNonce, Recipient, Fee, Amount) ->
    SenderId = aeser_id:create(account, Sender),
    RecipientId = aeser_id:create(account, Recipient),
    {ok, SpendTx} = aec_spend_tx:new(#{sender_id => SenderId,
                                       recipient_id => RecipientId,
                                       amount => Amount,
                                       fee => Fee,
                                       nonce => SenderNonce,
                                       payload => <<>>}),
    SpendTx.

make_oracle_register_tx(Pubkey, Nonce, Fee, QFee) ->
    AccountId = aeser_id:create(account, Pubkey),
    {ok, Tx} = aeo_register_tx:new(#{account_id      => AccountId,
                                     nonce           => Nonce,
                                     query_format    => <<>>,
                                     response_format => <<>>,
                                     query_fee       => QFee,
                                     oracle_ttl      => {delta, 100},
                                     abi_version     => ?ABI_NO_VM,
                                     fee             => Fee}),
    Tx.

make_oracle_query_tx(Pubkey, OraclePubkey, Nonce, Fee, QueryFee) ->
    SenderId = aeser_id:create(account, Pubkey),
    OracleId = aeser_id:create(oracle, OraclePubkey),
    {ok, Tx} = aeo_query_tx:new(#{sender_id    => SenderId,
                                  nonce        => Nonce,
                                  oracle_id    => OracleId,
                                  query        => <<"What is your name?">>,
                                  query_fee    => QueryFee,
                                  query_ttl    => {delta, 10},
                                  response_ttl => {delta, 10},
                                  fee          => Fee}),
    Tx.

make_oracle_response_tx(OraclePubkey, Nonce, Fee, QueryId) ->
    OracleId = aeser_id:create(oracle, OraclePubkey),
    {ok, Tx} = aeo_response_tx:new(#{oracle_id    => OracleId,
                                     nonce        => Nonce,
                                     query_id     => QueryId,
                                     response_ttl => {delta, 10},
                                     response     => <<"I am Nemo">>,
                                     fee          => Fee}),
    Tx.

make_channel_create_tx(InitiatorPubkey, Nonce, ResponderPubkey, Amount, Fee) ->
    InitiatorId = aeser_id:create(account, InitiatorPubkey),
    ResponderId = aeser_id:create(account, ResponderPubkey),
    {ok, Tx} = aesc_create_tx:new(#{initiator_id       => InitiatorId,
                                    initiator_amount   => Amount,
                                    responder_id       => ResponderId,
                                    responder_amount   => Amount,
                                    channel_reserve    => Amount div 2,
                                    lock_period        => 0,
                                    fee                => Fee,
                                    state_hash         => <<123:256>>,
                                    nonce              => Nonce}),
    Tx.

make_channel_close_mutual_tx(FromPubKey, Nonce, ChannelId, Amount, Fee) ->
    FromId = aeser_id:create(account, FromPubKey),
    {ok, Tx} = aesc_close_mutual_tx:new(#{channel_id              => ChannelId,
                                          from_id                 => FromId,
                                          initiator_amount_final  => Amount,
                                          responder_amount_final  => Amount,
                                          fee                     => Fee,
                                          nonce                   => Nonce}),
    Tx.


make_contract_create_tx(Pubkey, Code, CallData, Nonce, Deposit, Amount, Fee,
                        Gas, GasPrice) ->
    OwnerId = aeser_id:create(account, Pubkey),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    VM  = aect_test_utils:latest_sophia_vm_version(),
    {ok, Tx} = aect_create_tx:new(#{owner_id   => OwnerId,
                                    nonce      => Nonce,
                                    code       => Code,
                                    abi_version => ABI,
                                    vm_version => VM,
                                    deposit    => Deposit,
                                    amount     => Amount,
                                    gas        => Gas,
                                    gas_price  => GasPrice,
                                    call_data  => CallData,
                                    fee        => Fee}),
    Tx.

make_ga_attach_tx(Pubkey, Code, AuthFun, CallData, Nonce, Fee, Gas, GasPrice) ->
    OwnerId = aeser_id:create(account, Pubkey),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    VM  = aect_test_utils:latest_sophia_vm_version(),
    {ok, Tx} = aega_attach_tx:new(#{owner_id   => OwnerId,
                                    nonce      => Nonce,
                                    code       => Code,
                                    auth_fun   => AuthFun,
                                    abi_version => ABI,
                                    vm_version => VM,
                                    gas        => Gas,
                                    gas_price  => GasPrice,
                                    call_data  => CallData,
                                    fee        => Fee}),
    Tx.

make_ga_meta_tx(Pubkey, AuthData, InnerTx, Fee, Gas, GasPrice) ->
    AccountId = aeser_id:create(account, Pubkey),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    {ok, Tx} = aega_meta_tx:new(#{ ga_id       => AccountId
                                 , auth_data   => AuthData
                                 , abi_version => ABI
                                 , gas         => Gas
                                 , gas_price   => GasPrice
                                 , fee         => Fee
                                 , tx          => InnerTx}),
    Tx.

reward_40(Fee) -> Fee * 4 div 10.

reward_60(Fee) -> Fee - reward_40(Fee).

get_used_gas_fee(STx) ->
    Hash = aec_db:find_tx_location(aetx_sign:hash(STx)),
    Tx = aetx_sign:tx(STx),
    {CB, RawTx} = aetx:specialize_callback(Tx),
    case aetx:specialize_type(Tx) of
        {ga_meta_tx, RawTx} ->
            {ok, TmpTrees} = aec_chain:get_block_state(Hash),
            CPubkey = CB:ga_pubkey(RawTx),
            CallId = CB:call_id(RawTx, TmpTrees),
            {ok, Call}  = aec_chain:get_contract_call(CPubkey, CallId, Hash),
            aect_call:gas_used(Call) * aect_call:gas_price(Call);
        _ ->
            CallId = CB:call_id(RawTx),
            CPubkey = CB:contract_pubkey(RawTx),
            {ok, Call}  = aec_chain:get_contract_call(CPubkey, CallId, Hash),
            aect_call:gas_used(Call) * aect_call:gas_price(Call)
    end.

split_reward(Fee) ->
    case aec_dev_reward:enabled() of
        true ->
            ContribFactor = aec_dev_reward:allocated_shares(),
            Fee * (1000 - ContribFactor) div 1000;
        false ->
            Fee
    end.

%%%===================================================================
%%% Hard forking tests

hard_forking_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:start_chain_db(),
             meck:new(aec_hard_forks, [passthrough]),
             setup_meck_and_keys()
     end,
     fun(TmpDir) ->
             teardown_meck_and_keys(TmpDir),
             meck:unload(aec_hard_forks),
             aec_test_utils:stop_chain_db()
     end,
     [ {"Hard fork is accepted", fun hard_fork_is_accepted/0}
     , {"Hard fork with accounts is accepted", fun hard_fork_inserts_new_accounts/0}
     ]}.

hard_fork_is_accepted() ->
    MinervaForkHeight = 10,
    meck_minerva_fork_height(MinervaForkHeight),
    %% Create a chain that we are going to use.
    % genesis has a height = 0
    Chain = aec_test_utils:gen_blocks_only_chain(MinervaForkHeight + 1),

    %% Insert all blocks.
    ok = write_blocks_to_chain(Chain),
    ok.

hard_fork_inserts_new_accounts() ->
    MinervaForkHeight = 10,
    Alice = <<42:32/unit:8>>,
    BalA = 123456,
    meck:expect(aec_fork_block_settings, minerva_accounts, 0, [{Alice, BalA}]),
    meck_minerva_fork_height(MinervaForkHeight),
    %% Create a chain that we are going to use.
    % genesis has a height = 0
    Chain = aec_test_utils:gen_blocks_only_chain(MinervaForkHeight + 1),
    [HardForkBlock | PreForkChain] = lists:reverse(Chain),

    %% Insert up to the point of the fork
    ok = write_blocks_to_chain(lists:reverse(PreForkChain)),
    %% assert that Alice is not present
    none = aec_chain:get_account(Alice),
    ok = write_blocks_to_chain([HardForkBlock]),
    %% assert that Alice is present
    {value, AliceAccount} = aec_chain:get_account(Alice),
    % ensure the account nonce and balance
    0 = aec_accounts:nonce(AliceAccount),
    BalA = aec_accounts:balance(AliceAccount),
    ok.

meck_minerva_fork_height(Height) ->
    meck:expect(aec_hard_forks, protocol_effective_at_height,
                fun(H) ->
                    case H >= Height of
                        true -> ?MINERVA_PROTOCOL_VSN;
                        false -> ?ROMA_PROTOCOL_VSN
                    end
                end).

%% ------------------------------------------------------------
%% Mapping the return value of aec_chain_state:insert_block/[1,2] to match the old API
%% This essentially throws away the tx_events generated. For tests that actually want to
%% look at those, use the aec_chain_state API directly.
%%

insert_block(Block) ->
    insert_block_ret(aec_chain_state:insert_block(Block)).

insert_block(Block, Origin) ->
    insert_block_ret(aec_chain_state:insert_block(Block, Origin)).

insert_block_ret({ok,_}     ) -> ok;
insert_block_ret({pof,Pof,_}) -> {pof,Pof};
insert_block_ret(Other      ) -> Other.
