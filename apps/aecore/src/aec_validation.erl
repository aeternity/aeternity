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

validate_block(#{ key_block    := KeyBlock,
                  micro_blocks := MicroBlocks,
                  add_keyblock := AddKeyblock
                }) ->
    GenesisHeight = aec_block_genesis:height(),
    case aec_blocks:height(KeyBlock) of
        GenesisHeight ->
            case aec_blocks:to_header(KeyBlock) == aec_block_genesis:genesis_header()
                    andalso MicroBlocks == [] of
                true  -> ok;
                false -> {error, invalid_genesis_generation}
            end;
        Height when Height > GenesisHeight ->
            case validate_micro_block_list(MicroBlocks) of
                ok ->
                    case AddKeyblock of
                        true -> aec_blocks:validate_key_block(KeyBlock);
                        false -> ok
                    end;
                {error, _} = Err ->
                    Err
            end
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
