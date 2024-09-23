%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc
%%% Gather cild->parent chain pinning information for AE HC and make it 
%%% available through API
%%% @end
%%%-------------------------------------------------------------------

-module(aec_pinning_agent).

-export([get_pinning_data/0]).

-spec get_pinning_data() -> #{epoch := integer(), height => integer(), block_hash => binary(), parent_type => atom(), parent_network_id => binary()}.
get_pinning_data() ->
    get_pinning_data(aeternity, <<"dev1">>). 

-spec get_pinning_data(binary(), atom()) -> #{epoch => integer(), height => integer(), block_hash => binary(), parent_type => atom(), parent_network_id => binary()}.
get_pinning_data(Type, Id) ->
    {ok, #{epoch := Epoch,
            first := First,
            at := _At,
            last := _Last}} = aec_chain_hc:epoch_info(),
    %BlockHash = aec_chain:get_block_hash_optionally_by_hash_or_height(First-1),
    {ok, BlockHash} = aec_chain_state:get_key_block_hash_at_height(First-1),
    

    
    #{epoch => Epoch,
        height => First-1,
        block_hash => BlockHash,
        %block_hash => aeser_api_encoder:encode(key_block_hash, "123456789"),
        parent_type => Type,
        parent_network_id => Id}.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.