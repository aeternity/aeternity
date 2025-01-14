%%%=============================================================================
%%% @copyright 2024, Aeternity Anstalt
%%% @doc Creates key block (hole) candidate to be inserted into the chain, when the
%%%     current leader is not able to produce a key block before the cutoff time.
%%% @end
%%%=============================================================================
-module(aec_block_hole_candidate).

-export([create/3]).

-include("blocks.hrl").

%% -- API functions ----------------------------------------------------------
-spec create(BlockHash, Beneficiary, Miner) ->
    {ok, aec_blocks:block()} | {error, term()}
when
    BlockHash :: aec_blocks:block() | aec_blocks:block_header_hash(),
    Beneficiary :: aec_keys:pubkey(),
    Miner :: aec_keys:pubkey().
create(BlockHash, Beneficiary, Miner) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            create(Block, Beneficiary, Miner);
        error ->
            {error, block_not_found}
    end;
create(Block, Beneficiary, Miner) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    ConsensusModule = aec_blocks:consensus_module(Block),
    Height = ConsensusModule:key_block_height_relative_previous_block(
        aec_blocks:type(Block),
        aec_blocks:height(Block)
    ),
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    int_create(Height, BlockHash, Block, Beneficiary, Miner, Protocol).

int_create(Height, PrevBlockHash, PrevBlock, Beneficiary, Miner, Protocol) ->
    try
        {ok, Trees} = aec_chain_state:calculate_state_for_new_keyblock(
            Height,
            PrevBlockHash,
            Miner,
            Beneficiary,
            Protocol,
            <<?HOLE_FLAG:32>>
        ),
        Block = int_create_block(
            Height, PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol
        ),
        Consensus = aec_blocks:consensus_module(Block),
        case Consensus:keyblock_create_adjust_target(Block, []) of
            {ok, AdjBlock} -> {ok, AdjBlock};
            {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
        end
    catch
        error:{aborted, {Err = {leader_validation_failed, _}, _Stack1}}:_Stack2 ->
            {error, Err}
    end.

int_create_block(Height, PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol) ->
    Consensus = aec_consensus:get_consensus_module_at_height(Height),
    PrevKeyHash =
        case aec_blocks:type(PrevBlock) of
            micro -> aec_blocks:prev_key_hash(PrevBlock);
            key -> PrevBlockHash
        end,
    Fork = aeu_env:get_env(aecore, fork, undefined),
    InfoField = aec_chain_state:get_info_field(Height, Fork),
    aec_blocks:new_key(
        Height,
        PrevBlockHash,
        PrevKeyHash,
        aec_trees:hash(Trees),
        Consensus:default_target(),
        0,
        aeu_time:now_in_msecs(),
        InfoField,
        Protocol,
        Miner,
        Beneficiary,
        <<?HOLE_FLAG:32>>
    ).
