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

validate_block(#{ key_block := KeyBlock, micro_blocks := MicroBlocks }) ->
    case aec_blocks:validate_key_block(KeyBlock) of
        ok ->
            validate_micro_block_list(MicroBlocks);
        Err = {error, _} ->
            Err
    end;
validate_block(Block) ->
    case aec_blocks:is_key_block(Block) of
        true ->
            aec_blocks:validate_key_block(Block);
        false ->
            aec_blocks:validate_micro_block(Block)
    end.

validate_micro_block_list([Block|Left]) ->
    case aec_blocks:validate_micro_block(Block) of
        ok ->
            validate_micro_block_list(Left);
        {error, _} = Err ->
            Err
    end;
validate_micro_block_list([]) ->
    ok.
