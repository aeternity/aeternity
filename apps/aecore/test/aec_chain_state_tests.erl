%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_chain_state
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").


%%%===================================================================
%%% Test cases
%%%===================================================================

only_genesis_test_() ->
    [{"Insert genesis header, then genesis block"
     , fun() ->
               State1 = aec_chain_state:new(),
               ?assertEqual(undefined, aec_chain_state:top_header(State1)),
               ?assertEqual(undefined, aec_chain_state:top_block(State1)),
               GenesisHeader = genesis_header(),
               {ok, State2} = aec_chain_state:insert_header(GenesisHeader, State1),
               ?assertEqual(header_hash(GenesisHeader),
                            aec_chain_state:top_header_hash(State2)),
               ?assertEqual(undefined,
                            aec_chain_state:top_block_hash(State2)),
               GenesisBlock = genesis_block(),
               {ok, State3} = aec_chain_state:insert_block(GenesisBlock, State2),
               ?assertEqual(header_hash(GenesisHeader),
                            aec_chain_state:top_header_hash(State3)),
               ?assertEqual(block_hash(GenesisBlock),
                            aec_chain_state:top_block_hash(State3)),
               ok
       end},
     {"Insert genesis block directly"
     , fun() ->
               State1 = aec_chain_state:new(),
               ?assertEqual(undefined, aec_chain_state:top_header(State1)),
               ?assertEqual(undefined, aec_chain_state:top_block(State1)),
               GenesisBlock = genesis_block(),
               {ok, State2} = aec_chain_state:insert_block(GenesisBlock, State1),
               ?assertEqual(block_hash(GenesisBlock),
                            aec_chain_state:top_header_hash(State2)),
               ?assertEqual(block_hash(GenesisBlock),
                            aec_chain_state:top_block_hash(State2)),
               ok
       end}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

genesis_block() ->
    aec_block_genesis:genesis_block().

genesis_header() ->
    aec_block_genesis:genesis_header().

header_hash(Header) ->
    {ok, H} = aec_headers:hash_header(Header),
    H.

block_hash(Block) ->
    {ok, H} = aec_blocks:hash_internal_representation(Block),
    H.
