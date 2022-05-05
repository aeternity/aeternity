-module(aeapi).

-export([
         blockchain_name/0
        , network_id/0
        , node_revision/0
        , node_version/0
        , sync_progress/0
        , connected_peers/0

        , block_height/1
        , current_block/0
        , block_time_in_msecs/1
        , block_txs/1
        , key_block_by_height/1
        , key_block_by_hash/1
        , prev_block/1
        , printable_block_hash/1
        , printable_tx_hash/1
        , top_key_block/0
        ]).

blockchain_name() ->
    <<"aeternity">>. %% TODO: check hardcoding

network_id() ->
    aec_governance:get_network_id().

node_revision() ->
    aeu_info:get_revision().

node_version() ->
    aeu_info:get_version().

top_key_block() ->
    aec_chain:top_key_block().

sync_progress() ->
    aec_sync:sync_progress().

connected_peers() ->
    aec_peers:connected_peers().

current_block() ->
    case aec_chain:top_key_block() of
        {ok, Block} ->
            Block;
        error ->
            throw(block_not_found)
    end.

prev_block(Block) ->
    PrevBlockHash = aec_blocks:prev_key_hash(Block),
    case aec_chain:get_block(PrevBlockHash) of
        {ok, PrevBlock} ->
            PrevBlock;
        error ->
            throw(block_not_found)
    end.

block_height(Block) ->
    aec_blocks:height(Block).

block_time_in_msecs(Block) ->
    aec_blocks:time_in_msecs(Block).

block_txs(Block) ->
    block_txs(aec_blocks:is_key_block(Block), Block).

block_txs(true, Block) ->
    {ok, BlockHash} = aec_headers:hash_header(aec_blocks:to_key_header(Block)),
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MicroBlocks}} ->
            Txs = lists:foldl(
              fun(MicBlock, Acc) ->
                      [aec_blocks:txs(MicBlock) | Acc]
              end, [], MicroBlocks),
            lists:flatten(lists:reverse(Txs));
        error ->
            error
    end;
block_txs(false, Block) ->
    aec_blocks:txs(Block).

printable_block_hash(Block) ->
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(Block)),
    aeser_api_encoder:encode(key_block_hash, Hash).

printable_tx_hash(SignedTx) ->
    aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)).

-spec key_block_by_height(aec_blocks:height()) -> {ok, aec_blocks:key_block()} | error.

key_block_by_height(Height) when is_integer(Height) ->
    aec_chain:get_key_block_by_height(Height).

-spec key_block_by_hash(aeser_api_encoder:encoded()) -> {ok, aec_blocks:key_block()} | error.

key_block_by_hash(Hash) when is_binary(Hash) ->
    {key_block_hash, DecodedHash} = aeser_api_encoder:decode(Hash),
    aec_chain:get_block(DecodedHash).

