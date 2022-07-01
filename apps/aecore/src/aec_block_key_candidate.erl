%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_key_candidate).

-export([ create/3
        ]).

-include("blocks.hrl").

%% -- API functions ----------------------------------------------------------
-spec create(aec_blocks:block() | aec_blocks:block_header_hash(),
             aec_keys:pubkey(), aec_keys:pubkey()) ->
                    {ok, aec_blocks:block()} | {error, term()}.
create(BlockHash, Beneficiary, Miner) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            create(Block, Beneficiary, Miner);
        error ->
            {error, block_not_found}
    end;
create(Block, Beneficiary, Miner) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    Height = aec_blocks:height(Block) + 1,
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    int_create(BlockHash, Block, Beneficiary, Miner, Protocol).

%% -- Internal functions -----------------------------------------------------

int_create(BlockHash, Block, Beneficiary, Miner, Protocol) ->
    H = aec_blocks:height(Block) + 1,
    Consensus = aec_consensus:get_consensus_module_at_height(H),
    N = Consensus:keyblocks_for_target_calc() + 1,
    case aec_blocks:height(Block) < N of
        true  ->
            int_create_(BlockHash, Block, Beneficiary, Miner, [], Protocol);
        false ->
            case N of
                1 ->
                    int_create_(BlockHash, Block, Beneficiary, Miner, [], Protocol);
                _ ->
                    case aec_chain:get_n_generation_headers_backwards_from_hash(BlockHash, N) of
                        {ok, Headers} ->
                            int_create_(BlockHash, Block, Beneficiary, Miner, Headers, Protocol);
                        error ->
                            {error, headers_for_target_adjustment_not_found}
                    end
            end
    end.

int_create_(PrevBlockHash, PrevBlock, Beneficiary, Miner, AdjChain, Protocol) ->
    {ok, Trees} =
        aec_chain_state:calculate_state_for_new_keyblock(PrevBlockHash, Miner, Beneficiary, Protocol),
    Block = int_create_block(PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol),
    Consensus = aec_blocks:consensus_module(Block),
    case Consensus:keyblock_create_adjust_target(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock};
        {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
    end.

int_create_block(PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol) ->
    Height = aec_blocks:height(PrevBlock) + 1,
    Consensus = aec_consensus:get_consensus_module_at_height(Height),
    PrevKeyHash = case aec_blocks:type(PrevBlock) of
                      micro -> aec_blocks:prev_key_hash(PrevBlock);
                      key   -> PrevBlockHash
                  end,
    Fork = aeu_env:get_env(aecore, fork, undefined),
    InfoField = aec_chain_state:get_info_field(Height, Fork),
    aec_blocks:new_key(Height, PrevBlockHash, PrevKeyHash,
                       aec_trees:hash(Trees), Consensus:default_target(),
                       0, aeu_time:now_in_msecs(), InfoField, Protocol,
                       Miner, Beneficiary).
