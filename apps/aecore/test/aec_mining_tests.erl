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
-include("core_txs.hrl").

-define(TEST_MODULE, aec_mining).
-define(LOWEST_TARGET_SCI, 16#01010000).
-define(TEST_PUB, <<4,130,41,165,13,201,185,26,2,151,146,68,56,108,22,242,94,157,95,191,140,86,
                    145,96,71,82,28,176,23,5,128,17,245,174,170,199,54,248,167,43,185,12,108,91,
                    107,188,126,242,98,36,211,79,105,50,16,124,227,93,228,142,83,163,126,167,206>>).

mine_block_test_() ->
    {foreach,
      fun() -> setup() end,
      fun(_) -> cleanup(unused_arg) end,
      [
       {timeout, 60,
        {"Find a new block",
         fun() ->
                 TopBlock = #block{height = 0,
                                   target = ?HIGHEST_TARGET_SCI},
                 meck:expect(aec_pow, pick_nonce, 0, 54),

                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), []),
                 HeaderBin = aec_headers:serialize_for_hash(aec_blocks:to_header(BlockCandidate)),
                 Target = aec_blocks:target(BlockCandidate),
                 {ok, {Nonce1, Evd}} = ?TEST_MODULE:mine(HeaderBin, Target, Nonce),
                 Block = aec_blocks:set_pow(BlockCandidate, Nonce1, Evd),

                 ?assertEqual(1, Block#block.height),
                 ?assertEqual(1, length(Block#block.txs)),

                 ?assertEqual(ok, aec_headers:validate(
                                    aec_blocks:to_header(Block)))
         end}},
       {timeout, 60,
        {"Proof of work fails with no_solution",
         fun() ->
                 TopBlock = #block{target = ?LOWEST_TARGET_SCI},
                 meck:expect(aec_pow, pick_nonce, 0, 18),
                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), []),
                 HeaderBin = aec_headers:serialize_for_hash(aec_blocks:to_header(BlockCandidate)),
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
                 {ok, CoinbaseTx} = aec_coinbase_tx:new(#{ account => <<"pubkey">> }),
                 meck:expect(aec_blocks, new, 3,
                             #block{height = 30,
                                    target = ?HIGHEST_TARGET_SCI,
                                    txs = [aetx_sign:sign(CoinbaseTx, <<"sig1">>)],
                                    time = Now}),
                 Chain = lists:duplicate(10, #header{height = 20,
                                                     target = ?HIGHEST_TARGET_SCI,
                                                     time = Now - (10 * OneBlockExpectedMineTime)}),
                 meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 10),
                 meck:expect(aec_governance, expected_block_mine_rate, 0, OneBlockExpectedMineTime),

                 TopBlock = #block{},
                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), Chain),
                 HeaderBin = aec_headers:serialize_for_hash(aec_blocks:to_header(BlockCandidate)),
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
                 TS  = [ {X, Now - X * 300001} || X <- lists:seq(1, 10) ], %% Almost perfect timing!!
                 PastTarget = aec_pow:integer_to_scientific(?HIGHEST_TARGET_INT div 2),
                 Chain = [ #header{ height = 200 - I, target = PastTarget, time = T } || {I, T} <- TS ],

                 {ok, CoinbaseTx} = aec_coinbase_tx:new(#{ account => <<"pubkey">> }),
                 meck:expect(aec_blocks, new, 3,
                             #block{height = 200,
                                    target = PastTarget,
                                    txs = [aetx_sign:sign(CoinbaseTx, <<"sig1">>)],
                                    time = Now}),

                 meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 10),
                 %% One block should be mined every 5 mins
                 meck:expect(aec_governance, expected_block_mine_rate, 0, 300000),

                 TopBlock = #block{},
                 {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, aec_trees:new(), Chain),
                 HeaderBin = aec_headers:serialize_for_hash(aec_blocks:to_header(BlockCandidate)),
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
    ok = application:ensure_started(erlexec),
    application:start(crypto),
    meck:new(aec_blocks, [passthrough]),
    meck:new(aec_headers, [passthrough]),
    meck:new(aetx, [passthrough]),
    meck:new(aetx_sign, [passthrough]),
    meck:new(aec_governance, [passthrough]),
    meck:new(aec_keys,[passthrough]),
    meck:new(aec_trees, [passthrough]),
    meck:new(aeu_time, [passthrough]),
    meck:expect(aeu_time, now_in_msecs, 0, 1510253222889),
    {ok, _} = aec_tx_pool:start_link(),
    SignedTx = {signed_tx,{aetx, aec_coinbase_tx, {coinbase_tx,<<"pubkey">>}},
                         [<<48,69,2,32,44,5,112,89,79,175,39,38,68,238,0,83,
                            234,249,73,148,30,94,88,10,210,129,137,122,164,
                            221,55,4,187,21,52,128,2,33,0,158,123,167,116,
                            215,21,130,172,94,58,168,240,32,124,242,147,171,
                            183,186,62,21,253,155,101,132,121,17,72,89,101,
                            145,206>>]},
    Trees = aec_test_utils:create_state_tree_with_account(aec_accounts:new(<<"pubkey">>, 0, 0)),
    meck:expect(aec_trees, hash, 1, <<>>),
    meck:expect(aetx_sign, filter_invalid_signatures, fun(X) -> X end),
    meck:expect(aec_trees, apply_signed_txs, 3, {ok, [SignedTx], Trees}),
    meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
    %% We hardcode the signed_tx because crypto adds salt and gives
    %% non-deterministic result.
    meck:expect(aec_keys, sign, 1,
                {ok, SignedTx}).


cleanup(_) ->
    application:stop(crypto),
    meck:unload(aec_blocks),
    meck:unload(aec_headers),
    meck:unload(aetx),
    meck:unload(aetx_sign),
    meck:unload(aec_governance),
    meck:unload(aec_keys),
    meck:unload(aec_trees),
    meck:unload(aeu_time),
    ok = aec_tx_pool:stop(),
    ok = meck:unload(aeu_env).

-endif.
