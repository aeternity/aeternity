%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(aec_block_key_candidate_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(PREV_MINER_PUBKEY, <<85:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).
-define(BENEFICIARY_PUBKEY, <<123:?MINER_PUB_BYTES/unit:8>>).

new_key_block_test_() ->
    {"Create new key-block",
     fun() ->
             %% Previous block is a key block, so it
             %% has miner and height.
             PrevBlock = #block{height = 11, target = 17,
                                miner = ?PREV_MINER_PUBKEY,
                                version = ?GENESIS_VERSION},
             BlockHeader = aec_blocks:to_header(PrevBlock),

             {NewBlock, _} = aec_test_utils:create_keyblock_with_state(
                               [{PrevBlock, aec_trees:new()}], ?MINER_PUBKEY, ?BENEFICIARY_PUBKEY),

             ?assertEqual(12, aec_blocks:height(NewBlock)),
             SerializedBlockHeader = aec_headers:serialize_to_binary(BlockHeader),
             ?assertEqual(aec_hash:hash(header, SerializedBlockHeader),
                          aec_blocks:prev_hash(NewBlock)),
             ?assertEqual([], NewBlock#block.txs),
             ?assertEqual(17, NewBlock#block.target),
             ?assertEqual(?GENESIS_VERSION, NewBlock#block.version),
             ?assertEqual(?MINER_PUBKEY, NewBlock#block.miner),
             ?assertEqual(?BENEFICIARY_PUBKEY, NewBlock#block.beneficiary)
     end}.

-endif.
