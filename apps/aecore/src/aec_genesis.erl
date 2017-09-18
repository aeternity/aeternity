%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Genesis block definition.
%%%
%%% @TODO Open questions:
%%% * Confirm initial difficulty as lowest (easy to mine initial
%%%   blocks), and defer fine tuning of initial difficulty;
%%% * Confirm unmined;
%%%   * It implies genesis block cannot be validated PoW wise.
%%% * Confirm time as epoch i.e. much in the past;
%%%   * It implies the time difference between genesis block and first
%%%     block is very large - that may be considered abnormal for
%%%     successive blocks (e.g. between blocks 1 and 2 - with block 0
%%%     being genesis).
%%% * Confirm no transactions - not even coinbase;
%%%   * This means that validation function attempting to check that
%%%     there is at least a coinbase transaction in a block needs to
%%%     have a special case for genesis.
%%% * Confirm special hash values i.e. all zeros.
%%%   * This means that validation function attempting to consider the
%%%     hashes in a block needs to have a special case for genesis.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_genesis).

%% API
-export([genesis_header/0,
         genesis_block_as_deserialized_from_network/0,
         genesis_block/0]).

-include("common.hrl").
-include("blocks.hrl").

genesis_header() ->
    #header{
       version = ?GENESIS_VERSION,
       height = ?GENESIS_HEIGHT,
       prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
       %% txs_hash = <<0:?TXS_HASH_BYTES/unit:8>>, %% TODO Refine `#header{}` record definition based on wiki https://github.com/aeternity/epoch/wiki/Block-Header
       %% state_hash = <<0:?STATE_HASH_BYTES/unit:8>>, %% TODO Refine `#header{}` record definition based on wiki https://github.com/aeternity/epoch/wiki/Block-Header
       %% TODO Delete proof_hash from wiki https://github.com/aeternity/epoch/wiki/Block-Header
       target = ?HIGHEST_TARGET_SCI, %% TODO Update wiki https://github.com/aeternity/epoch/wiki/Block-Header
       pow_evidence = no_value, %% TODO Refine type in `#header{}` record based on wiki https://github.com/aeternity/epoch/wiki/Block-Header and code.
       nonce = 0,
       time = 0 %% Epoch.
      }.

genesis_block_as_deserialized_from_network() ->
    H = genesis_header(),
    #block{
       version = H#header.version,
       height = H#header.height,
       prev_hash = H#header.prev_hash,
       %% TODO txs_hash
       %% TODO state_hash
       %% TODO Delete proof_hash from wiki https://github.com/aeternity/epoch/wiki/Block-Header
       target = H#header.target,
       pow_evidence = H#header.pow_evidence,
       nonce = H#header.nonce,
       time = H#header.time,
       %% TODO Delete balance_proofs from wiki https://github.com/aeternity/epoch/wiki/Block
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
    B#block{
      trees = todo}. %% TODO Return empty state trees. Refine module aec_trees.
