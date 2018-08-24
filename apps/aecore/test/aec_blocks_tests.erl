%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-import(aec_blocks, [raw_key_block/0,
                     raw_micro_block/0
                    ]).

-define(TEST_MODULE, aec_blocks).
-define(FAKE_TXS_TREE_HASH, <<42:?TXS_HASH_BYTES/unit:8>>).

network_serialization_test_() ->
    [{"Serialize/deserialize block with min nonce",
      fun() ->
              B = aec_blocks:set_nonce(raw_key_block(), 0),
              SB = #{} = ?TEST_MODULE:serialize_to_map(B),
              ?assertEqual({ok, B}, ?TEST_MODULE:deserialize_from_map(SB))
      end
     },
     {"Serialize/deserialize block with max nonce",
      fun() ->
              B = aec_blocks:set_nonce(raw_key_block(), ?MAX_NONCE),
              SB = #{} = ?TEST_MODULE:serialize_to_map(B),
              ?assertEqual({ok, B}, ?TEST_MODULE:deserialize_from_map(SB))
      end
     },
     {"try to deserialize a blocks with out-of-range nonce",
      fun() ->
              Block1 = aec_blocks:set_nonce(raw_key_block(), ?MAX_NONCE + 1),
              SerializedBlock1 = #{} = ?TEST_MODULE:serialize_to_map(Block1),
              ?assertEqual({error,bad_nonce},
                           ?TEST_MODULE:deserialize_from_map(SerializedBlock1)),

              Block2 = aec_blocks:set_nonce(raw_key_block(), -1),
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
    {ok, MalformedTxsRootHash} = aec_txs_trees:root_hash(MalformedTree),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, MalformedTxsRootHash,
              aec_blocks:root_hash(RawBlock),
              [SignedSpend],
              aec_blocks:time_in_msecs(RawBlock)),
    ?assertEqual({error, {block, malformed_txs_hash}},
                 ?TEST_MODULE:validate_micro_block(Block)).

validate_test_pass_validation_no_txs() ->
    Txs = [],
    Tree = aec_txs_trees:from_txs(Txs),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(Tree)),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, TxsRootHash,
              aec_blocks:root_hash(RawBlock),
              [],
              aec_blocks:time_in_msecs(RawBlock)),
    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block)).

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
    {ok, TxsRootHash} = aec_txs_trees:root_hash(Tree),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, TxsRootHash,
              aec_blocks:root_hash(RawBlock),
              Txs,
              aec_blocks:time_in_msecs(RawBlock)),

    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block)).

-endif.
