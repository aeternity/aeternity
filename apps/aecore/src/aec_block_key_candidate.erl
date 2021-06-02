%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_key_candidate).

-export([ create/2
        ]).

-include("blocks.hrl").

%% -- API functions ----------------------------------------------------------
-spec create(aec_blocks:block() | aec_blocks:block_header_hash(),
             aec_keys:pubkey()) ->
                    {ok, aec_blocks:block()} | {error, term()}.
create(BlockHash, Beneficiary) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            create(Block, Beneficiary);
        error ->
            {error, block_not_found}
    end;
create(Block, Beneficiary) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    Height = aec_blocks:height(Block) + 1,
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    int_create(BlockHash, Block, Beneficiary, Protocol).

%% -- Internal functions -----------------------------------------------------

int_create(BlockHash, Block, Beneficiary, Protocol) ->
    H = aec_blocks:height(Block) + 1,
    Consensus = aec_consensus:get_consensus_module_at_height(H),
    N = Consensus:keyblocks_for_unmined_keyblock_adjust() + 1,
    case aec_blocks:height(Block) < N of
        true  ->
            int_create(BlockHash, Block, Beneficiary, [], Protocol);
        false ->
            case N of
                1 ->
                    int_create(BlockHash, Block, Beneficiary, [], Protocol);
                _ ->
                    case aec_chain:get_n_generation_headers_backwards_from_hash(BlockHash, N) of
                        {ok, Headers} ->
                            int_create(BlockHash, Block, Beneficiary, Headers, Protocol);
                        error ->
                            {error, headers_for_target_adjustment_not_found}
                    end
            end
    end.

int_create(BlockHash, Block, Beneficiary, AdjChain, Protocol) ->
    case aec_keys:candidate_pubkey() of
        {ok, Miner} ->
            int_create(BlockHash, Block, Miner, Beneficiary, AdjChain, Protocol);
        {error, _} = Error ->
            Error
    end.

int_create(PrevBlockHash, PrevBlock, Miner, Beneficiary, AdjChain, Protocol) ->
    {ok, Block, _} =
        aec_chain_state:calculate_new_unmined_keyblock(aec_blocks:to_header(PrevBlock), PrevBlockHash, Miner, Beneficiary, Protocol),
    Consensus = aec_blocks:consensus_module(Block),
    case Consensus:adjust_unmined_keyblock(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock};
        {error, Reason} -> {error, {failed_to_adjust_new_block, Reason}}
    end.
