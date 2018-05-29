%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_block_candidate module
%%% @end
%%%=============================================================================
-module(aec_block_candidate_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").
-define(TEST_PUB, <<181,14,1,149,98,104,64,190,242,52,152,159,190,216,30,49,
                    94,251,20,75,9,85,29,82,35,178,98,75,188,72,242,141>>).
-define(TEST_PRIV, <<220,220,95,208,13,103,52,54,220,60,93,149,153,25,95,67,
                     178,143,191,176,251,107,170,15,223,140,13,57,96,171,79,
                     43,181,14,1,149,98,104,64,190,242,52,152,159,190,216,30,
                     49,94,251,20,75,9,85,29,82,35,178,98,75,188,72,242,141>>).


difficulty_recalculation_test_() ->
      [ {"For good mining speed mine block with the same difficulty",
         fun() ->
                 Now = 1504731164584,
                 OneBlockExpectedMineTime = 300000,
                 BlockHeight = 30,

                 Block0 = aec_blocks:new(BlockHeight, <<0:32/unit:8>>, <<0:32/unit:8>>, <<0:32/unit:8>>,
                                         [], undefined, 12345, Now, ?PROTOCOL_VERSION, ?TEST_PUB),
                 Chain = lists:duplicate(10, #header{height = 20,
                                                     target = ?HIGHEST_TARGET_SCI,
                                                     time = Now - (10 * OneBlockExpectedMineTime),
                                                     version = ?PROTOCOL_VERSION}),

                 {ok, Block} = aec_block_candidate:adjust_target(Block0, Chain),

                 ?assertEqual(?HIGHEST_TARGET_SCI, Block#block.target)
         end},
        {"Too few blocks mined in time increases new block's target threshold",
         fun() ->
                 Now = 1504731164584,
                 BlockHeight = 200,
                 TS  = [ {X, Now - X * 300001} || X <- lists:seq(1, 10) ], %% Almost perfect timing!!
                 PastTarget = aec_pow:integer_to_scientific(?HIGHEST_TARGET_INT div 2),
                 Chain = [ #header{ height = BlockHeight - I, target = PastTarget, time = T,
                                    version = ?PROTOCOL_VERSION } || {I, T} <- TS ],
                 Block0 = aec_blocks:new(BlockHeight, <<0:32/unit:8>>, <<0:32/unit:8>>, <<0:32/unit:8>>,
                                         [], undefined, 12345, Now, ?PROTOCOL_VERSION, ?TEST_PUB),

                 {ok, Block} = aec_block_candidate:adjust_target(Block0, Chain),

                 ?assertEqual(true, PastTarget < Block#block.target),
                 ?assertEqual(true, ?HIGHEST_TARGET_SCI >= Block#block.target)
         end}
      ].

block_extension_test_() ->
    {foreach,
      fun() ->
        meck:new(aeu_time, [passthrough]),
        meck:new(aec_chain, [passthrough]),
        meck:new(aec_keys, [passthrough]),
        meck:new(aec_tx_pool, [passthrough])
      end,
      fun(_) ->
        meck:unload(aec_tx_pool),
        meck:unload(aec_keys),
        meck:unload(aec_chain),
        meck:unload(aeu_time)
      end,
      [{"Generate a block in one step, compared with two steps",
        fun() ->
          {ok, Tx} = aec_spend_tx:new(#{ sender => ?TEST_PUB, recipient => ?TEST_PUB
                                       , amount => 10, fee => 1, ttl => 100, nonce => 1, payload => <<>> }),
          STx = aetx_sign:sign(Tx, ?TEST_PRIV),

          AccMap = #{ preset_accounts => [{?TEST_PUB, 1000}] },
          {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),

          meck:expect(aeu_time, now_in_msecs, 0, 1234567890),
          meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
          meck:expect(aec_tx_pool, get_candidate, 2, {ok, [STx]}),
          meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
          {ok, Block1A, #{ trees := _Trees1A }} = aec_block_candidate:create(Block0),

          meck:expect(aec_tx_pool, get_candidate, 2, {ok, []}),
          {ok, Block1B0, BInfo} = aec_block_candidate:create(Block0),

          {ok, Block1B, #{ trees := _Trees1B }} =
                aec_block_generator:update_block_candidate(5, Block1B0, [STx], BInfo),

          ?assertEqual(Block1A, Block1B)
        end}
      ]}.


-endif.
