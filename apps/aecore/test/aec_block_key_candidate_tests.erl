%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(aec_block_key_candidate_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-import(aec_blocks, [raw_block/0]).

-define(PREV_MINER_PUBKEY, <<85:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).
-define(BENEFICIARY_PUBKEY, <<123:?MINER_PUB_BYTES/unit:8>>).

new_key_block_test_() ->
    {"Create new key-block",
     fun() ->
             %% Previous block is a key block, so it
             %% has miner and height.
             RawBlock  = raw_block(),
             PrevBlock1 = aec_blocks:set_height(RawBlock, 11),
             PrevBlock2 = aec_blocks:set_target(PrevBlock1, 17),
             PrevBlock = aec_blocks:set_miner(PrevBlock2, ?MINER_PUBKEY),
             BlockHeader = aec_blocks:to_header(PrevBlock),

             {NewBlock, _} = aec_test_utils:create_keyblock_with_state(
                               [{PrevBlock, aec_trees:new()}], ?MINER_PUBKEY, ?BENEFICIARY_PUBKEY),

             ?assertEqual(12, aec_blocks:height(NewBlock)),
             SerializedBlockHeader = aec_headers:serialize_to_binary(BlockHeader),
             ?assertEqual(aec_hash:hash(header, SerializedBlockHeader),
                          aec_blocks:prev_hash(NewBlock)),
             ?assertEqual([], aec_blocks:txs(NewBlock)),
             ?assertEqual(17, aec_blocks:target(NewBlock)),
             ?assertEqual(?GENESIS_VERSION, aec_blocks:version(NewBlock)),
             ?assertEqual(?MINER_PUBKEY, aec_blocks:miner(NewBlock)),
             ?assertEqual(?BENEFICIARY_PUBKEY, aec_blocks:beneficiary(NewBlock))
     end}.

-endif.
