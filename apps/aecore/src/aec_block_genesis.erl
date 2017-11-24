%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Genesis block definition.
%%%
%%% The genesis block does not follow the validation rules of the
%%% other blocks because:
%%% * It is unmined;
%%%   * It implies genesis block cannot be validated PoW wise.
%%% * Its time is epoch i.e. much in the past;
%%%   * It implies the time difference between genesis block and first
%%%     block is very large - that may be considered abnormal for
%%%     successive blocks (e.g. between blocks 1 and 2 - with block 0
%%%     being genesis).
%%% * It contains no transactions - not even coinbase;
%%%   * This means that validation function attempting to check that
%%%     there is at least a coinbase transaction in a block needs to
%%%     have a special case for genesis.
%%% * The hash values in it have special value i.e. all zeros.
%%%   * This means that validation function attempting to consider the
%%%     hashes in a block needs to have a special case for genesis.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_block_genesis).

%% API
-export([genesis_header/0,
         genesis_block_as_deserialized_from_network/0,
         genesis_block/0,
         height/0]).

-include("common.hrl").
-include("blocks.hrl").

genesis_header() ->
    #header{
       version = ?GENESIS_VERSION,
       height = ?GENESIS_HEIGHT,
       prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
       txs_hash = <<0:?TXS_HASH_BYTES/unit:8>>,
       root_hash = <<0:?STATE_HASH_BYTES/unit:8>>,
       target = ?HIGHEST_TARGET_SCI,
       pow_evidence = no_value,
       nonce = 0,
       time = 0 %% Epoch.
      }.

genesis_block_as_deserialized_from_network() ->
    H = genesis_header(),
    #block{
       version = H#header.version,
       height = H#header.height,
       prev_hash = H#header.prev_hash,
       txs_hash = H#header.txs_hash,
       root_hash = H#header.root_hash,
       target = H#header.target,
       pow_evidence = H#header.pow_evidence,
       nonce = H#header.nonce,
       time = H#header.time,
       txs = [],
       trees = _DummyTrees = #trees{}
      }.

%% Returns the genesis block including the state trees.
%%
%% The current implementation of state trees causes a new Erlang term,
%% representing the initial empty state trees, to be allocated in the
%% heap memory of the calling process.
genesis_block() ->
    B = genesis_block_as_deserialized_from_network(),
    {ok, T} = aec_trees:all_trees_new(),
    B#block{trees = T}.

height() ->
    ?GENESIS_HEIGHT.
