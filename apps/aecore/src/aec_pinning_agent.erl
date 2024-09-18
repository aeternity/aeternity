%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc
%%% Gather cild->parent chain pinning information for AE HC and make it 
%%% available through API
%%% @end
%%%-------------------------------------------------------------------

-module(aec_pinning_agent).

-export([get_pinning_data/0]).

-spec get_pinning_data() -> #{atom() := integer(), atom() := integer(), atom() := binary(), atom() := atom(), atom() := binary()}.
get_pinning_data() ->
    get_pinning_data(aeternity, <<"dev1">>). 

-spec get_pinning_data(binary(), atom()) -> #{atom() := integer(), atom() := integer(), atom() := binary(), atom() := atom(), atom() := binary()}.
get_pinning_data(Type, Id) ->
    #{epoch => 1000,
        height => 123456789,
        block_hash => aeser_api_encoder:encode(key_block_hash, "12344567"),
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