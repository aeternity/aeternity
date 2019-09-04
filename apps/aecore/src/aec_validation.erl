%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining blocks validation
%%% @end
%%%=============================================================================

-module(aec_validation).

%% API
-export([validate_block/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

validate_block(Block) ->
    case aec_blocks:is_key_block(Block) of
        true ->
            aec_blocks:validate_key_block(Block);
        false ->
            aec_blocks:validate_micro_block(Block)
    end.
