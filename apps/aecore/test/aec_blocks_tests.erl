%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("core_txs.hrl").

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
              ?assertEqual(?PROTOCOL_VERSION, NewBlock#block.version)
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
     {"Serialize/deserialize block with min nonce",
      fun() ->
              B = #block{nonce = 0},
              {ok, SB} = ?TEST_MODULE:serialize_for_network(B),
              ?assertEqual({ok, B}, ?TEST_MODULE:deserialize_from_network(SB))
      end
     },
     {"Serialize/deserialize block with max nonce",
      fun() ->
              B = #block{nonce = ?MAX_NONCE},
              {ok, SB} = ?TEST_MODULE:serialize_for_network(B),
              ?assertEqual({ok, B}, ?TEST_MODULE:deserialize_from_network(SB))
      end
     },
     {"try to deserialize a blocks with out-of-range nonce",
      fun() ->
             Block1 = #block{trees = #trees{accounts = foo}, nonce = ?MAX_NONCE + 1},
             {ok, SerializedBlock1} = ?TEST_MODULE:serialize_for_network(Block1),
             ?assertEqual({error,bad_nonce},
                          ?TEST_MODULE:deserialize_from_network(SerializedBlock1)),

              Block2 = #block{trees = #trees{accounts = foo}, nonce = -1},
             {ok, SerializedBlock2} = ?TEST_MODULE:serialize_for_network(Block2),
             ?assertEqual({error,bad_nonce},
                          ?TEST_MODULE:deserialize_from_network(SerializedBlock2))
     end}].

validate_test_() ->
    {setup,
     fun aec_test_utils:aec_keys_setup/0,
     fun aec_test_utils:aec_keys_cleanup/1,
     [ {"Multiple coinbase txs in the block",
        fun validate_test_multiple_coinbase/0}
     , {"Malformed txs merkle tree hash",
        fun validate_test_malformed_txs_root_hash/0}
     , {"Malformed tx signature",
        fun validate_test_malformed_tx_signature/0}
     , {"Pass validation",
        fun validate_test_pass_validation/0}
     ]}.

validate_test_multiple_coinbase() ->
    SignedCoinbase = aec_test_utils:signed_coinbase_tx(),
    Block = #block{txs = [SignedCoinbase, SignedCoinbase]},

    ?assertEqual({error, multiple_coinbase_txs}, ?TEST_MODULE:validate(Block)).

validate_test_malformed_txs_root_hash() ->
    SignedCoinbase = aec_test_utils:signed_coinbase_tx(),
    MalformedTxs = [SignedCoinbase, aec_tx_sign:sign(#coinbase_tx{account = <<"malformed_account">>}, <<"pubkey">>)],
    {ok, MalformedTree} = aec_txs_trees:new(MalformedTxs),
    {ok, MalformedRootHash} = aec_txs_trees:root_hash(MalformedTree),
    Block = #block{txs = [SignedCoinbase], txs_hash = MalformedRootHash},

    ?assertEqual({error, malformed_txs_hash}, ?TEST_MODULE:validate(Block)).

validate_test_malformed_tx_signature() ->
    SignedCoinbase = aec_test_utils:signed_coinbase_tx(),
    Txs = [{signed_tx, aec_tx_sign:data(SignedCoinbase), []}],
    {ok, Tree} = aec_txs_trees:new(Txs),
    {ok, RootHash} = aec_txs_trees:root_hash(Tree),
    Block = #block{txs = Txs, txs_hash = RootHash},

    ?assertEqual({error, invalid_transaction_signature}, ?TEST_MODULE:validate(Block)).

validate_test_pass_validation() ->
    SignedCoinbase = aec_test_utils:signed_coinbase_tx(),
    Txs = [SignedCoinbase],
    {ok, Tree} = aec_txs_trees:new(Txs),
    {ok, RootHash} = aec_txs_trees:root_hash(Tree),
    Block = #block{txs = Txs, txs_hash = RootHash},

    ?assertEqual(ok, ?TEST_MODULE:validate(Block)).

-endif.
