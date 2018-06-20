%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(aec_block_key_candidate_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(PREV_MINER_PUBKEY, <<85:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).

new_key_block_test_() ->
    {"Create new key-block",
     fun() ->
             %% Previous block is a key block, so it
             %% has miner and height.
             PrevBlock = #block{height = 11, target = 17,
                                miner = ?PREV_MINER_PUBKEY,
                                version = ?GENESIS_VERSION},
             BlockHeader = aec_blocks:to_header(PrevBlock),

             FeesInfo = #{txs => 0, gas => 0},
             {NewBlock, _} =
                 aec_block_key_candidate:create_with_state(PrevBlock, ?MINER_PUBKEY, aec_trees:new(), FeesInfo),

             ?assertEqual(12, aec_blocks:height(NewBlock)),
             SerializedBlockHeader = aec_headers:serialize_to_binary(BlockHeader),
             ?assertEqual(aec_hash:hash(header, SerializedBlockHeader),
                          aec_blocks:prev_hash(NewBlock)),
             ?assertEqual([], NewBlock#block.txs),
             ?assertEqual(17, NewBlock#block.target),
             ?assertEqual(?GENESIS_VERSION, NewBlock#block.version)
     end}.

-endif.
