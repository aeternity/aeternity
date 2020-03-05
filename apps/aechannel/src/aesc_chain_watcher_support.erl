-module(aesc_chain_watcher_support).

-export([
          get_channel/2
        ]).



get_channel(ChId, BlockHash) ->
    case aec_chain:get_block_state(BlockHash) of
        {ok, Trees} ->
            case aec_chain:get_channel(ChId, Trees) of
                {ok, Ch} ->
                    Ch;
                {error, _} ->
                    undefined
            end;
        error ->
            undefined
    end.
