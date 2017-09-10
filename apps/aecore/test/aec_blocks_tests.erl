-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).

new_block_test_() ->
    {"Generate new block with given txs and 0 nonce",
     fun() ->
             PrevBlock = #block{height = 11, difficulty = 17},
             BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

             {ok, NewBlock} = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

             ?assertEqual(12, ?TEST_MODULE:height(NewBlock)),
             ?assertEqual(aec_sha256:hash(BlockHeader), NewBlock#block.prev_hash),
             ?assertEqual([], NewBlock#block.txs),
             ?assertEqual(17, NewBlock#block.difficulty),
             ?assertEqual(1, NewBlock#block.version)
     end
    }.

-endif.
