%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_SECKEY, <<42:?MINER_PUB_BYTES/unit:16>>).
-define(FAKE_TXS_TREE_HASH, <<42:?TXS_HASH_BYTES/unit:8>>).

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
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             meck:new(enacl, [passthrough]),
             meck:expect(enacl, sign_verify_detached, 3, {ok, <<>>}),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_header, 1, error),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             meck:unload(aec_chain),
             meck:unload(enacl)
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
          #{recipient => aec_id:create(account, <<1:32/unit:8>>),
            amount => 1,
            fee => 1,
            nonce => 1,
            payload => <<>>}),

    {ok, Spend} = aec_spend_tx:new(#{sender => aec_id:create(account, <<42:32/unit:8>>),
                                     recipient => aec_id:create(account, <<4242:32/unit:8>>),
                                     amount => 1,
                                     fee => 1,
                                     nonce => 1,
                                     payload => <<>>}),
    BadSignedSpend = aec_test_utils:sign_tx(Spend, <<0:64/unit:8>>),

    MalformedTxs = [SignedSpend, BadSignedSpend],
    MalformedTree = aec_txs_trees:from_txs(MalformedTxs),
    {ok, MalformedRootHash} = aec_txs_trees:root_hash(MalformedTree),
    Block = #block{txs = [SignedSpend], txs_hash = MalformedRootHash,
                   version = ?PROTOCOL_VERSION},

    ?assertEqual({error, {block, malformed_txs_hash}},
                 ?TEST_MODULE:validate_micro_block(Block, ?MINER_SECKEY)).

validate_test_pass_validation_no_txs() ->
    Txs = [],
    Tree = aec_txs_trees:from_txs(Txs),
    RootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(Tree)),
    Block = #block{txs = Txs, txs_hash = RootHash,
        version = ?PROTOCOL_VERSION},

    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, ?MINER_SECKEY)).

validate_test_pass_validation() ->
    SignedSpend =
        aec_test_utils:signed_spend_tx(
          #{recipient => aec_id:create(account, <<1:32/unit:8>>),
            amount => 1,
            fee => 1,
            nonce => 1,
            payload => <<>>}),
    Txs = [SignedSpend],
    Tree = aec_txs_trees:from_txs(Txs),
    {ok, RootHash} = aec_txs_trees:root_hash(Tree),
    Block = #block{txs = Txs, txs_hash = RootHash,
                   version = ?PROTOCOL_VERSION},

    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, ?MINER_SECKEY)).

-endif.
