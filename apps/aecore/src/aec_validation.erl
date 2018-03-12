%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining blocks validation
%%% @end
%%%=============================================================================

-module(aec_validation).

%% API
-export([validate_block/2,
         validate_block_no_signature/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% TODO Create envelope that tells which kind of block/header is being processed.
%% Then Move specific validation to aec_key_block.erl and aec_micro_block.erl,
%% depending on block/header kind.
%% This will be changed in/after jur0's changes.

validate_block_no_signature(#{ key_block := KeyBlock, micro_blocks := MicroBlocks }) ->
    case aec_blocks:validate_key_block(KeyBlock) of
        ok ->
            lists:foldl(fun(MB, ok) -> aec_blocks:validate_micro_block_no_signature(MB);
                           (_MB, Err = {error, _}) -> Err
                        end, ok, MicroBlocks);
        Err = {error, _} ->
            Err
    end;
validate_block_no_signature(Block) ->
    case aec_blocks:is_key_block(Block) of
        true ->
            aec_blocks:validate_key_block(Block);
        false ->
            aec_blocks:validate_micro_block_no_signature(Block)
    end.

validate_block(Block, LeaderKey) ->
    case aec_blocks:is_key_block(Block) of
        true ->
            aec_blocks:validate_key_block(Block);
        false ->
            aec_blocks:validate_micro_block(Block, LeaderKey)
    end.
