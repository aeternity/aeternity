%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_key_candidate).

-export([ create/3
        , create_hole/3
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").

%% -- API functions ----------------------------------------------------------
-spec create(BlockHash, Beneficiary, Miner) -> {ok, aec_blocks:block()} | {error, term()}
  when BlockHash :: aec_blocks:block() | aec_blocks:block_header_hash()
     , Beneficiary :: aec_keys:pubkey()
     , Miner :: aec_keys:pubkey().
create(BlockHash, Beneficiary, Miner) ->
    create(BlockHash, Beneficiary, Miner, #{hc_hole => false}).


-spec create_hole(BlockHash, Beneficiary, Miner) -> {ok, aec_blocks:block()} | {error, term()}
  when BlockHash :: aec_blocks:block() | aec_blocks:block_header_hash()
     , Beneficiary :: aec_keys:pubkey()
     , Miner :: aec_keys:pubkey().
create_hole(BlockHash, Beneficiary, Miner) ->
    create(BlockHash, Beneficiary, Miner, #{hc_hole => true}).

create(BlockHash, Beneficiary, Miner, Flags) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            create(Block, Beneficiary, Miner, Flags);
        error ->
            {error, block_not_found}
    end;
create(Block, Beneficiary, Miner, Flags) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    ConsensusModule = aec_blocks:consensus_module(Block),
    Height = ConsensusModule:key_block_height_relative_previous_block(aec_blocks:type(Block),
                                                                      aec_blocks:height(Block)),
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    int_create(Height, BlockHash, Block, Beneficiary, Miner, Protocol, Flags).

%% -- Internal functions -----------------------------------------------------

int_create(Height, BlockHash, Block, Beneficiary, Miner, Protocol, Flags) ->
    Consensus = aec_consensus:get_consensus_module_at_height(Height),
    N = Consensus:keyblocks_for_target_calc() + 1,
    case aec_blocks:height(Block) < N of
        true  ->
            int_create_(Height, BlockHash, Block, Beneficiary, Miner, [], Protocol, Flags);
        false ->
            case N of
                1 ->
                    int_create_(Height, BlockHash, Block, Beneficiary, Miner, [], Protocol, Flags);
                _ ->
                    case aec_chain:get_n_generation_headers_backwards_from_hash(BlockHash, N) of
                        {ok, Headers} ->
                            int_create_(Height, BlockHash, Block, Beneficiary, Miner, Headers, Protocol, Flags);
                        error ->
                            {error, headers_for_target_adjustment_not_found}
                    end
            end
    end.

int_create_(Height, PrevBlockHash, PrevBlock, Beneficiary, Miner, AdjChain, Protocol, Flags) ->
    {ok, Trees} = aec_chain_state:calculate_state_for_new_keyblock(Height, PrevBlockHash,
                                                                   Miner, Beneficiary, Protocol),
    Block = int_create_block(Height, PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol, Flags),
    Consensus = aec_blocks:consensus_module(Block),
    case Consensus:keyblock_create_adjust_target(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock};
        {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
    end.

int_create_block(Height, PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol, Flags) ->
    Consensus = aec_consensus:get_consensus_module_at_height(Height),
    PrevKeyHash = case aec_blocks:type(PrevBlock) of
                      micro -> aec_blocks:prev_key_hash(PrevBlock);
                      key   -> PrevBlockHash
                  end,
    Fork = aeu_env:get_env(aecore, fork, undefined),
    InfoField =
        case maps:get(hc_hole, Flags, false) of
            true ->
                %% Keep backwards comatibility for ae
                %% WIP Note: Not sure if this should live here.
                case aec_chain_state:get_info_field(Height, Fork) of
                    default when Protocol  >= ?MINERVA_PROTOCOL_VSN ->
                    PointReleaseInfo = aeu_info:block_info(),
                    Info = (1 bsl 31) bor PointReleaseInfo,
                    <<Info:?OPTIONAL_INFO_BYTES/unit:8>>;
                default ->
                    Info = (1 bsl 31),
                    <<Info:?OPTIONAL_INFO_BYTES/unit:8>>;
                BaseInfo when is_integer(BaseInfo) ->
                    Info = (1 bsl 31) bor BaseInfo,
                    <<Info:?OPTIONAL_INFO_BYTES/unit:8>>
                end;
            false ->
                aec_chain_state:get_info_field(Height, Fork)
            end,

    aec_blocks:new_key(Height, PrevBlockHash, PrevKeyHash,
        aec_trees:hash(Trees), Consensus:default_target(),
        0, aeu_time:now_in_msecs(), InfoField, Protocol,
        Miner, Beneficiary).
