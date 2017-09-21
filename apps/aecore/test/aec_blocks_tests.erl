-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).

new_block_test_() ->
    {setup,
     fun() ->
             meck:new(aec_trees, [passthrough]),
             meck:expect(aec_trees, all_trees_hash, 1, <<>>)
     end,
     fun(_) ->
             ?assert(meck:validate(aec_trees)),
             meck:unload(aec_trees)
     end,
     {"Generate new block with given txs and 0 nonce",
      fun() ->
              PrevBlock = #block{height = 11, target = 17},
              BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

              {ok, NewBlock} = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

              ?assertEqual(12, ?TEST_MODULE:height(NewBlock)),
              {ok, SerializedBlockHeader} =
                  aec_headers:serialize_to_binary(BlockHeader),
              ?assertEqual(aec_sha256:hash(SerializedBlockHeader),
                           ?TEST_MODULE:prev_hash(NewBlock)),
              ?assertEqual([], NewBlock#block.txs),
              ?assertEqual(17, NewBlock#block.target),
              ?assertEqual(1, NewBlock#block.version)
      end}}.

network_serialization_test() ->
    Block = #block{trees = #trees{accounts = foo}},
    {ok, SerializedBlock} = ?TEST_MODULE:serialize_for_network(Block),
    {ok, DeserializedBlock} =
        ?TEST_MODULE:deserialize_from_network(SerializedBlock),
    ?assertEqual(Block#block{trees = #trees{}}, DeserializedBlock),
    ?assertEqual({ok, SerializedBlock},
                 ?TEST_MODULE:serialize_for_network(DeserializedBlock)).
-endif.
