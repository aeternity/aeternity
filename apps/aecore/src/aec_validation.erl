%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining blocks validation
%%% @end
%%%=============================================================================

-module(aec_validation).

%% API
-export([validate_block/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

validate_block(Block, Version) ->
    ConsensusModule = aec_blocks:consensus_module(Block),
    case ConsensusModule:dirty_validate_block_pre_conductor(Block) of
        ok ->
            case aec_blocks:is_key_block(Block) of
                true ->
                    aec_blocks:validate_key_block(Block, Version);
                false ->
                    aec_blocks:validate_micro_block(Block, Version)
            end;
        {error, Reason} ->
            {error, {consensus, Reason}}
    end.
