-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).

new_block_test_() ->
    {setup,
     fun() ->
             meck:new(aec_txs_trees, [passthrough]),
             meck:new(aec_trees, [passthrough]),
             meck:expect(aec_txs_trees, new, 1, {ok, fake_txs_tree}),
             meck:expect(
               aec_txs_trees, root_hash,
               fun(fake_txs_tree) ->
                       {ok, <<"fake_txs_tree_hash">>}
               end),
             meck:expect(aec_trees, all_trees_hash, 1, <<>>)
     end,
     fun(_) ->
             ?assert(meck:validate(aec_txs_trees)),
             ?assert(meck:validate(aec_trees)),
             meck:unload(aec_txs_trees),
             meck:unload(aec_trees)
     end,
     {"Generate new block with given txs and 0 nonce",
      fun() ->
              PrevBlock = #block{height = 11, target = 17},
              BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

              {ok, NewBlock} = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

              ?assertEqual(12, ?TEST_MODULE:height(NewBlock)),
              ?assertEqual(aec_sha256:hash(BlockHeader),
                           ?TEST_MODULE:prev_hash(NewBlock)),
              ?assertEqual(<<"fake_txs_tree_hash">>, NewBlock#block.txs_hash),
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

hash_test() ->
    Block = #block{},
    {ok, SerializedHeader} =
        aec_headers:serialize_for_network(?TEST_MODULE:to_header(Block)),
    ?assertEqual({ok, aec_sha256:hash(SerializedHeader)},
                 ?TEST_MODULE:hash_internal_representation(Block)).

-endif.
