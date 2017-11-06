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
        [ extend_block_chain_by_difficulties_with_nonce_and_coinbase/3
        , aec_keys_setup/0
        , aec_keys_cleanup/1
        ]).

%%%===================================================================
%%% Test cases
%%%===================================================================

only_genesis_test_() ->
    [{"Insert genesis header, then genesis block"
     , fun() ->
               State1 = aec_chain_state:new(),
               ?assertEqual(undefined, aec_chain_state:top_header(State1)),
               ?assertEqual(undefined, aec_chain_state:top_block(State1)),
               GenesisHeader = genesis_header(),
               {ok, State2} = aec_chain_state:insert_header(GenesisHeader, State1),
               ?assertEqual(header_hash(GenesisHeader),
                            aec_chain_state:top_header_hash(State2)),
               ?assertEqual(undefined,
                            aec_chain_state:top_block_hash(State2)),
               GenesisBlock = genesis_block(),
               {ok, State3} = aec_chain_state:insert_block(GenesisBlock, State2),
               ?assertEqual(header_hash(GenesisHeader),
                            aec_chain_state:top_header_hash(State3)),
               ?assertEqual(block_hash(GenesisBlock),
                            aec_chain_state:top_block_hash(State3)),
               ok
       end},
     {"Insert genesis block directly"
     , fun() ->
               State1 = aec_chain_state:new(),
               ?assertEqual(undefined, aec_chain_state:top_header(State1)),
               ?assertEqual(undefined, aec_chain_state:top_block(State1)),
               GenesisBlock = genesis_block(),
               {ok, State2} = aec_chain_state:insert_block(GenesisBlock, State1),
               ?assertEqual(block_hash(GenesisBlock),
                            aec_chain_state:top_header_hash(State2)),
               ?assertEqual(block_hash(GenesisBlock),
                            aec_chain_state:top_block_hash(State2)),
               ok
       end}
    ].

gc_test_() ->
    {foreach,
     fun setup_meck_and_keys/0,
     fun teardown_meck_and_keys/1,
     [{gc_test_slogan(Length, Max, Interval, KeepAll)
      , fun() ->
                %% Generate blockchain and write it to the state.
                BC = gen_block_chain(Length),
                State1 = aec_chain_state:new(gc_opts(KeepAll, Max, Interval)),
                State2 = write_blocks_to_chain(BC, State1),

                %% Get the state trees that we would have persisted.
                Trees = aec_chain_state:get_state_trees_for_persistance(State2),

                %% Check that the top block hash is among the persisted.
                TopHash = aec_chain_state:top_block_hash(State2),
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
                ok
        end}
      || {Length, Max, Interval, KeepAll} <- gc_test_params()
     ]}.

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


%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_meck_and_keys() ->
    aec_test_utils:mock_difficulty_as_target(),
    aec_test_utils:aec_keys_setup().

teardown_meck_and_keys(TmpDir) ->
    aec_test_utils:unmock_difficulty_as_target(),
    aec_test_utils:aec_keys_cleanup(TmpDir).

write_blocks_to_chain([H|T], State) ->
    {ok, State1} = aec_chain_state:insert_block(H, State),
    write_blocks_to_chain(T, State1);
write_blocks_to_chain([], State) ->
    State.

gc_opts(KeepAll, Max, Interval) ->
    #{ max_snapshot_height => Max
     , sparse_snapshots_interval => Interval
     , keep_all_snapshots_height => KeepAll
     }.

gen_block_chain(Length) ->
  gen_block_chain(Length, 1).

gen_block_chain(Length, Difficulty) ->
  gen_block_chain(Length, Difficulty, 111).

gen_block_chain(Length, Difficulty, Nounce) ->
  Ds = lists:duplicate(Length - 1, Difficulty),
  G = genesis_block(),
  [G|extend_block_chain_by_difficulties_with_nonce_and_coinbase(G, Ds, Nounce)].

genesis_block() ->
    aec_block_genesis:genesis_block().

genesis_header() ->
    aec_block_genesis:genesis_header().

header_hash(Header) ->
    {ok, H} = aec_headers:hash_header(Header),
    H.

block_hash(Block) ->
    {ok, H} = aec_blocks:hash_internal_representation(Block),
    H.
