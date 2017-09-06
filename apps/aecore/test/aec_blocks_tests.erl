-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).

-define(DIFFICULTY, 17).

new_block_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Generate new block with given txs and 0 nonce",
       fun() ->
               PrevBlock = #block{height = 11},
               BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

               {ok, NewBlock} = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

               ?assertEqual(12, NewBlock#block.height),
               ?assertEqual(aec_sha256:hash(BlockHeader), NewBlock#block.prev_hash),
               ?assertEqual([], NewBlock#block.txs),
               ?assertEqual(?DIFFICULTY, NewBlock#block.difficulty),
               ?assertEqual(1, NewBlock#block.version)
       end}
     ]
    }.

setup() ->
    application:start(crypto),
    meck:new(aec_pow_sha256),
    meck:expect(aec_pow_sha256, recalculate_difficulty, 3, ?DIFFICULTY).

teardown(_) ->
    application:stop(crypto),
    meck:unload(aec_pow_sha256).

-endif.
