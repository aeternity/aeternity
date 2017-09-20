-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).

new_block_test_() ->
    {"Generate new block with given txs and 0 nonce",
     fun() ->
             PrevBlock = #block{height = 11, target = 17},
             BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

             {ok, NewBlock} = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

             ?assertEqual(12, ?TEST_MODULE:height(NewBlock)),
             ?assertEqual(aec_headers:hash_header(BlockHeader),
                          {ok, ?TEST_MODULE:prev_hash(NewBlock)}),
             ?assertEqual([], NewBlock#block.txs),
             ?assertEqual(17, NewBlock#block.target),
             ?assertEqual(1, NewBlock#block.version)
     end
    }.

network_serialization_test() ->
    Block = #block{trees = #trees{accounts = foo}},
    {ok, SerializedBlock} = ?TEST_MODULE:serialize_for_network(Block),
    {ok, DeserializedBlock} =
        ?TEST_MODULE:deserialize_from_network(SerializedBlock),
    ?assertEqual(Block#block{trees = #trees{}}, DeserializedBlock),
    ?assertEqual({ok, SerializedBlock},
                 ?TEST_MODULE:serialize_for_network(DeserializedBlock)).
-endif.
