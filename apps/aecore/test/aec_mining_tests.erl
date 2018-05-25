%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_mining module
%%% @end
%%%=============================================================================
-module(aec_mining_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_mining).
-define(LOWEST_TARGET_SCI, 16#01010000).
-define(TEST_PUB, <<4,176,10,241,172,223,229,80,244,222,165,8,198,46,
                    167,128,25,34,151,180,162,192,72,103,185,62,161,12,
                    117,147,72,68,194,188,89,248,81,212,197,21,193,74,
                    115,216,210,123,239,69,164,128,164,122,116,151,23,
                    22,56,146,73,13,29,198,110,162,145>>).

mine_block_test_() ->
    {foreach,
      fun() -> setup() end,
      fun(_) -> cleanup(unused_arg) end,
      [
       {timeout, 60,
        {"Find a new block",
         fun() ->
                 TopBlock = #block{height = ?GENESIS_HEIGHT,
                                   target = ?HIGHEST_TARGET_SCI,
                                   version = ?GENESIS_VERSION},
                 % if there is a change in the structure of the block
                 % this will result in a change in the hash of the header
                 % and will invalidate the nonce value below
                 % in order to find a proper nonce for your
                 % block uncomment the line below
                 %let_it_crash = generate_valid_test_data(TopBlock, 100000000000000),
                 meck:expect(aec_pow, pick_nonce, 0, 3650718748619880306),

                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), []),
                 HeaderBin = aec_headers:serialize_to_binary(aec_blocks:to_header(BlockCandidate)),
                 Target = aec_blocks:target(BlockCandidate),
                 {ok, {Nonce1, Evd}} = ?TEST_MODULE:mine(HeaderBin, Target, Nonce),
                 Block = aec_blocks:set_pow(BlockCandidate, Nonce1, Evd),

                 ?assertEqual(1, Block#block.height),
                 ?assertEqual(0, length(Block#block.txs)),

                 ?assertEqual(ok, aec_headers:validate(
                                    aec_blocks:to_header(Block)))
         end}},
       {timeout, 60,
        {"Proof of work fails with no_solution",
         fun() ->
                 TopBlock = #block{height = ?GENESIS_HEIGHT,
                                   target = ?LOWEST_TARGET_SCI,
                                   version = ?GENESIS_VERSION},
                 meck:expect(aec_pow, pick_nonce, 0, 18),
                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), []),
                 HeaderBin = aec_headers:serialize_to_binary(aec_blocks:to_header(BlockCandidate)),
                 Target = aec_blocks:target(BlockCandidate),
                 ?assertEqual({error, no_solution},
                              ?TEST_MODULE:mine(HeaderBin, Target, Nonce))
         end}}
      ]}.

difficulty_recalculation_test_() ->
    {foreach,
      fun() ->
              setup(),
              %% This group of tests tests the difficulty
              %% recalculation inside the aec_mining module, hence the
              %% PoW module can be mocked.
              meck:new(aec_pow_cuckoo),
              meck:expect(aec_pow_cuckoo, generate,
                          fun(_, _, Nonce) ->
                              Evd = lists:duplicate(42, 0),
                              {ok, {Nonce, Evd}}
                          end),
              meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 10)
      end,
      fun(_) ->
              meck:unload(aec_pow_cuckoo),
              cleanup(unused_arg)
      end,
      [
       {timeout, 60,
        {"For good mining speed mine block with the same difficulty",
         fun() ->
                 Now = 1504731164584,
                 OneBlockExpectedMineTime = 300000,
                 BlockHeight = 30,
                 meck:expect(aec_blocks, new, 4,
                             #block{height = BlockHeight,
                                    target = ?HIGHEST_TARGET_SCI,
                                    txs = [],
                                    time = Now,
                                    version = ?PROTOCOL_VERSION}),
                 Chain = lists:duplicate(10, #header{height = 20,
                                                     target = ?HIGHEST_TARGET_SCI,
                                                     time = Now - (10 * OneBlockExpectedMineTime),
                                                     version = ?PROTOCOL_VERSION}),
                 meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 10),
                 meck:expect(aec_governance, expected_block_mine_rate, 0, OneBlockExpectedMineTime),

                 TopBlock = #block{version = ?PROTOCOL_VERSION},
                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), Chain),
                 HeaderBin = aec_headers:serialize_to_binary(aec_blocks:to_header(BlockCandidate)),
                 Target = aec_blocks:target(BlockCandidate),
                 {ok, {Nonce1, Evd}} = ?TEST_MODULE:mine(HeaderBin, Target, Nonce),
                 Block = aec_blocks:set_pow(BlockCandidate, Nonce1, Evd),

                 ?assertEqual(30, Block#block.height),

                 ?assertEqual(?HIGHEST_TARGET_SCI, Block#block.target),
                 ?assertEqual(1, meck:num_calls(aec_governance, blocks_to_check_difficulty_count, 0))
         end}},
       {timeout, 60,
        {"Too few blocks mined in time increases new block's target threshold",
         fun() ->
                 Now = 1504731164584,
                 BlockHeight = 200,
                 TS  = [ {X, Now - X * 300001} || X <- lists:seq(1, 10) ], %% Almost perfect timing!!
                 PastTarget = aec_pow:integer_to_scientific(?HIGHEST_TARGET_INT div 2),
                 Chain = [ #header{ height = BlockHeight - I, target = PastTarget, time = T,
                                    version = ?PROTOCOL_VERSION } || {I, T} <- TS ],


                 meck:expect(aec_blocks, new, 4,
                             #block{height = BlockHeight,
                                    target = PastTarget,
                                    txs = [],
                                    time = Now,
                                    version = ?PROTOCOL_VERSION}),

                 meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 10),
                 %% One block should be mined every 5 mins
                 meck:expect(aec_governance, expected_block_mine_rate, 0, 300000),

                 TopBlock = #block{version = ?PROTOCOL_VERSION},
                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), Chain),
                 HeaderBin = aec_headers:serialize_to_binary(aec_blocks:to_header(BlockCandidate)),
                 Target = aec_blocks:target(BlockCandidate),
                 {ok, {Nonce1, Evd}} = ?TEST_MODULE:mine(HeaderBin, Target, Nonce),
                 Block = aec_blocks:set_pow(BlockCandidate, Nonce1, Evd),

                 ?assertEqual(200, Block#block.height),

                 ?assertEqual(true, PastTarget < Block#block.target),
                 ?assertEqual(true, ?HIGHEST_TARGET_SCI >= Block#block.target)
         end}}
      ]}.

setup() ->
    ok = meck:new(aeu_env, [passthrough]),
    aec_test_utils:mock_fast_and_deterministic_cuckoo_pow(),
    aec_test_utils:start_chain_db(),
    ok = application:ensure_started(erlexec),
    application:start(crypto),
    meck:new(aec_blocks, [passthrough]),
    meck:new(aec_headers, [passthrough]),
    meck:new(aetx_sign, [passthrough]),
    meck:new(aec_governance, [passthrough]),
    meck:new(aec_keys,[passthrough]),
    meck:new(aec_trees, [passthrough]),
    meck:new(aeu_time, [passthrough]),
    meck:expect(aeu_time, now_in_msecs, 0, 1519659148405),
    {ok, _} = aec_tx_pool:start_link(),
    Trees =
    aec_test_utils:create_state_tree_with_account(aec_accounts:new(?TEST_PUB, 0)),
    meck:expect(aec_trees, hash, 1, <<>>),
    meck:expect(aec_trees, apply_signed_txs, 5, {ok, [], Trees}),
    meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
    ok.


cleanup(_) ->
    application:stop(crypto),
    meck:unload(aec_blocks),
    meck:unload(aec_headers),
    meck:unload(aetx_sign),
    meck:unload(aec_governance),
    meck:unload(aec_keys),
    meck:unload(aec_trees),
    meck:unload(aeu_time),
    ok = aec_tx_pool:stop(),
    aec_test_utils:stop_chain_db(),
    ok = meck:unload(aeu_env).

generate_valid_test_data(_TopBlock, Tries) when Tries < 1 ->
    could_not_find_nonce;
generate_valid_test_data(TopBlock, Tries) ->
    {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), []),
    HeaderBin = aec_headers:serialize_to_binary(aec_blocks:to_header(BlockCandidate)),
    Target = aec_blocks:target(BlockCandidate),
    case ?TEST_MODULE:mine(HeaderBin, Target, Nonce) of
        {ok, {Nonce1, _Evd}} ->
            {ok, BlockCandidate, Nonce1};
        {error, no_solution} ->
            generate_valid_test_data(TopBlock, Tries -1)
    end.

-endif.
