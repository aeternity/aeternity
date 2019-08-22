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
    case aec_blocks:is_key_block(Block) of
        true ->
            aec_blocks:validate_key_block(Block, Version);
        false ->
            aec_blocks:validate_micro_block(Block, Version)
    end.
