-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

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
              {ok, SerializedBlockHeader} =
                  aec_headers:serialize_to_binary(BlockHeader),
              ?assertEqual(aec_sha256:hash(SerializedBlockHeader),
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

validate_test_() ->
    [fun() ->
             SignedCoinbase = #signed_tx{data = #coinbase_tx{}},
             Block = #block{txs = [SignedCoinbase, SignedCoinbase]},
             ?assertEqual({error, multiple_coinbase_txs}, ?TEST_MODULE:validate(Block))
     end,
     fun() ->
             SignedCoinbase = #signed_tx{data = #coinbase_tx{}},
             CorrectTxs = [SignedCoinbase],
             MalformedTxs = [SignedCoinbase, #signed_tx{data = #coinbase_tx{nonce = 123}}],
             {ok, MalformedTree} = aec_txs_trees:new(MalformedTxs),
             {ok, MalformedRootHash} = aec_txs_trees:root_hash(MalformedTree),
             Block = #block{txs = CorrectTxs, txs_hash = MalformedRootHash},
             ?assertEqual({error, malformed_txs_hash}, ?TEST_MODULE:validate(Block))
     end,
     fun() ->
             SignedCoinbase = #signed_tx{data = #coinbase_tx{}},
             Txs = [SignedCoinbase],
             {ok, Tree} = aec_txs_trees:new(Txs),
             {ok, RootHash} = aec_txs_trees:root_hash(Tree),
             Block = #block{txs = Txs, txs_hash = RootHash},
             ?assertEqual(ok, ?TEST_MODULE:validate(Block))
     end].

-endif.
