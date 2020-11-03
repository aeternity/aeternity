%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aeminer/include/aeminer.hrl").
-include("blocks.hrl").

-import(aec_blocks, [raw_micro_block/0
                    ]).

-define(TEST_MODULE, aec_blocks).
-define(FAKE_TXS_TREE_HASH, <<42:?TXS_HASH_BYTES/unit:8>>).

validate_test_() ->
    {setup,
     fun() ->
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             meck:new(enacl, [passthrough]),
             meck:expect(enacl, sign_verify_detached, 3, {ok, <<>>}),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, dirty_get_header, 1, error),
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
          #{recipient_id => aeser_id:create(account, <<1:32/unit:8>>),
            amount => 1,
            fee => 1,
            nonce => 1,
            payload => <<>>}),

    {ok, Spend} = aec_spend_tx:new(#{sender_id => aeser_id:create(account, <<42:32/unit:8>>),
                                     recipient_id => aeser_id:create(account, <<4242:32/unit:8>>),
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
              [SignedSpend]),
    ?assertEqual({error, {block, malformed_txs_hash}},
                 ?TEST_MODULE:validate_micro_block(Block, aec_blocks:version(Block))).

validate_test_pass_validation_no_txs() ->
    Txs = [],
    Tree = aec_txs_trees:from_txs(Txs),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(Tree)),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, TxsRootHash,
              aec_blocks:root_hash(RawBlock),
              []),
    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, aec_blocks:version(Block))).

validate_test_pass_validation() ->
    SignedSpend =
        aec_test_utils:signed_spend_tx(
          #{recipient_id => aeser_id:create(account, <<1:32/unit:8>>),
            amount => 1,
            fee => 20000,
            nonce => 1,
            payload => <<>>}),
    Txs = [SignedSpend],
    Tree = aec_txs_trees:from_txs(Txs),
    {ok, TxsRootHash} = aec_txs_trees:root_hash(Tree),
    RawBlock = raw_micro_block(),
    Block = aec_blocks:update_micro_candidate(
              RawBlock, TxsRootHash,
              aec_blocks:root_hash(RawBlock),
              Txs),

    ?assertEqual(ok, ?TEST_MODULE:validate_micro_block(Block, aec_blocks:version(Block))).

-endif.
