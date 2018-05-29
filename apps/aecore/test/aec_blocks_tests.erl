%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).
-define(FAKE_TXS_TREE_HASH, <<42:?TXS_HASH_BYTES/unit:8>>).

new_block_test_() ->
    {setup,
     fun() ->
             meck:new(aec_txs_trees, [passthrough]),
             meck:new(aec_trees, [passthrough]),
             meck:expect(aec_txs_trees, from_txs, 1, fake_txs_tree),
             meck:expect(
               aec_txs_trees, root_hash,
               fun(fake_txs_tree) ->
                       {ok, ?FAKE_TXS_TREE_HASH}
               end),
             meck:expect(aec_trees, hash, 1, <<>>)
     end,
     fun(_) ->
             ?assert(meck:validate(aec_txs_trees)),
             ?assert(meck:validate(aec_trees)),
             meck:unload(aec_txs_trees),
             meck:unload(aec_trees)
     end,
     {"Generate new block with given txs and 0 nonce",
      fun() ->
              PrevBlock = #block{height = 11, target = 17,
                                 version = ?GENESIS_VERSION},
              BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

              {NewBlock, _} =
                  aec_block_candidate:create_with_state(PrevBlock, ?MINER_PUBKEY,
                                                        [], aec_trees:new()),

              ?assertEqual(12, ?TEST_MODULE:height(NewBlock)),
              SerializedBlockHeader =
                  aec_headers:serialize_to_binary(BlockHeader),
              ?assertEqual(aec_hash:hash(header, SerializedBlockHeader),
                           ?TEST_MODULE:prev_hash(NewBlock)),
              ?assertEqual(?FAKE_TXS_TREE_HASH, NewBlock#block.txs_hash),
              ?assertEqual([], NewBlock#block.txs),
              ?assertEqual(17, NewBlock#block.target),
              ?assertEqual(?GENESIS_VERSION, NewBlock#block.version)
      end}}.

network_serialization_test_() ->
    [{"Serialize/deserialize block with min nonce",
      fun() ->
              B = #block{nonce = 0,
                         version = ?PROTOCOL_VERSION},
              SB = #{} = ?TEST_MODULE:serialize_to_map(B),
              ?assertEqual({ok, B}, ?TEST_MODULE:deserialize_from_map(SB))
      end
     },
     {"Serialize/deserialize block with max nonce",
      fun() ->
              B = #block{nonce = ?MAX_NONCE,
                         version = ?PROTOCOL_VERSION},
              SB = #{} = ?TEST_MODULE:serialize_to_map(B),
              ?assertEqual({ok, B}, ?TEST_MODULE:deserialize_from_map(SB))
      end
     },
     {"try to deserialize a blocks with out-of-range nonce",
      fun() ->
             Block1 = #block{nonce = ?MAX_NONCE + 1,
                             version = ?PROTOCOL_VERSION},
             SerializedBlock1 = #{} = ?TEST_MODULE:serialize_to_map(Block1),
             ?assertEqual({error,bad_nonce},
                          ?TEST_MODULE:deserialize_from_map(SerializedBlock1)),

             Block2 = #block{nonce = -1,
                             version = ?PROTOCOL_VERSION},
             SerializedBlock2 = #{} = ?TEST_MODULE:serialize_to_map(Block2),
             ?assertEqual({error,bad_nonce},
                          ?TEST_MODULE:deserialize_from_map(SerializedBlock2))
     end}].

validate_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [ {"Malformed txs merkle tree hash",
        fun validate_test_malformed_txs_root_hash/0}
     , {"Pass validation - case no txs",
        fun validate_test_pass_validation_no_txs/0}
     , {"Pass validation - case some txs",
        fun validate_test_pass_validation/0}
     ]}.

validate_test_malformed_txs_root_hash() ->
    SignedSpend =
        aec_test_utils:signed_spend_tx(
          #{recipient => <<1:32/unit:8>>,
            amount => 1,
            fee => 1,
            nonce => 1,
            payload => <<>>}),

    {ok, Spend} = aec_spend_tx:new(#{sender => <<42:32/unit:8>>,
                                     recipient => <<4242:32/unit:8>>,
                                     amount => 1,
                                     fee => 1,
                                     nonce => 1,
                                     payload => <<>>}),
    BadSignedSpend = aetx_sign:sign(Spend, <<0:64/unit:8>>),

    MalformedTxs = [SignedSpend, BadSignedSpend],
    MalformedTree = aec_txs_trees:from_txs(MalformedTxs),
    {ok, MalformedRootHash} = aec_txs_trees:root_hash(MalformedTree),
    Block = #block{txs = [SignedSpend], txs_hash = MalformedRootHash,
                   version = ?PROTOCOL_VERSION},

    ?assertEqual({error, malformed_txs_hash}, ?TEST_MODULE:validate(Block)).

validate_test_pass_validation_no_txs() ->
    Txs = [],
    Tree = aec_txs_trees:from_txs(Txs),
    RootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(Tree)),
    Block = #block{txs = Txs, txs_hash = RootHash,
        version = ?PROTOCOL_VERSION},

    ?assertEqual(ok, ?TEST_MODULE:validate(Block)).

validate_test_pass_validation() ->
    SignedSpend =
        aec_test_utils:signed_spend_tx(
          #{recipient => <<1:32/unit:8>>,
            amount => 1,
            fee => 1,
            nonce => 1,
            payload => <<>>}),
    Txs = [SignedSpend],
    Tree = aec_txs_trees:from_txs(Txs),
    {ok, RootHash} = aec_txs_trees:root_hash(Tree),
    Block = #block{txs = Txs, txs_hash = RootHash,
                   version = ?PROTOCOL_VERSION},

    ?assertEqual(ok, ?TEST_MODULE:validate(Block)).

-endif.
