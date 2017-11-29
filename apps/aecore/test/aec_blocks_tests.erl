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

              NewBlock = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

              ?assertEqual(12, ?TEST_MODULE:height(NewBlock)),
              SerializedBlockHeader =
                  aec_headers:serialize_for_hash(BlockHeader),
              ?assertEqual(aec_sha256:hash(SerializedBlockHeader),
                           ?TEST_MODULE:prev_hash(NewBlock)),
              ?assertEqual(<<"fake_txs_tree_hash">>, NewBlock#block.txs_hash),
              ?assertEqual([], NewBlock#block.txs),
              ?assertEqual(17, NewBlock#block.target),
              ?assertEqual(1, NewBlock#block.version)
      end}}.

network_serialization_test_() ->
    [{"Serialize/deserialize block",
      fun() ->
             Block = #block{trees = #trees{accounts = foo}},
             {ok, SerializedBlock} = ?TEST_MODULE:serialize_for_network(Block),
             {ok, DeserializedBlock} =
                 ?TEST_MODULE:deserialize_from_network(SerializedBlock),
             ?assertEqual(Block#block{trees = #trees{}}, DeserializedBlock),
             ?assertEqual({ok, SerializedBlock},
                          ?TEST_MODULE:serialize_for_network(DeserializedBlock))
     end},
     {"try to deserialize a blocks with out-of-range nonce",
      fun() ->
             Block1 = #block{trees = #trees{accounts = foo}, nonce = ?MAX_NONCE + 1},
             {ok, SerializedBlock1} = ?TEST_MODULE:serialize_for_network(Block1),
             ?assertEqual({error,bad_nonce},
                          ?TEST_MODULE:deserialize_from_network(SerializedBlock1)),

              Block2 = #block{trees = #trees{accounts = foo}, nonce = -1},
             {ok, SerializedBlock2} = ?TEST_MODULE:serialize_for_network(Block2),
             ?debugFmt("serialized block: ~p~n", [SerializedBlock2]),
             ?assertEqual({error,bad_nonce},
                          ?TEST_MODULE:deserialize_from_network(SerializedBlock2))
     end}].

validate_test_() ->
    [fun() ->
             SignedCoinbase = #signed_tx{data = #coinbase_tx{}},
             Block = #block{txs = [SignedCoinbase, SignedCoinbase]},
             ?assertEqual({error, multiple_coinbase_txs}, ?TEST_MODULE:validate(Block))
     end,
     fun() ->
             SignedCoinbase = #signed_tx{data = #coinbase_tx{}},
             CorrectTxs = [SignedCoinbase],
             MalformedTxs = [SignedCoinbase, #signed_tx{data = #coinbase_tx{account = <<"malformed_account">>}}],
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
