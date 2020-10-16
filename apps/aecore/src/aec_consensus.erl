%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus behavior for customizing the node
%%%      Only one consensus algorithm might be enabled at a given height
%%%      Some consensus algorithms provide a special instrumentation HTTP API
%%%      for controlling consensus specific functionality. Some consensus
%%%      modules might overwrite existing functionality in the node using plugins.
%%%      Some consensus modules cannot be disabled after enabling.
%%%      Consensus is only a small part of the overall on-chain protocol
%%%      Protocol defined features are configured separately - new features
%%%      can get enabled and introduced regardless of the used consensus.
%%%      --------------------------------------------------------------
%%%      "Dev Mode" would work in the following way:
%%%      - Disallow disabling of dev mode after it got enabled
%%%      - Optionally use state from a real block
%%%      - Disable sync, gossip, peer discovery etc...
%%%      - Start an in-ram chain simulator or even multiple ones if requested
%%%      - Mock the tx push HTTP endpoint and instantly mine transactions pushed to it
%%%      - Ignore PoW in the block headers
%%%      - Use plugins almost anywhere
%%%      - Provide an API for instrumenting the chain:
%%%        * Start from <real_block_hash> - starts a chain simulator based on real-world state
%%%          (might be taken from mainnet/testnet).
%%%        * Start empty - starts a new chain simulator
%%%        * Enable/Disable instant tx processing
%%%        * Commit pending txs to a microblock
%%%        * Clone on microblock/fork etc...
%%%        * Set account state etc...
%%%        * N new keyblocks on top of the current top block(or block with given hash)
%%%        * Set the given block hash as the new top
%%%        * Generate FATE execution traces, change contract code and state at will
%%%      --------------------------------------------------------------
%%%      PoA Consensus with one authority would work in the following way:
%%%      - Can be enabled/disabled and switched to another algorithm
%%%      - No plugins are enabled
%%%      - Keyblocks contain the signature of the chosen authority
%%%      - API for querying and changing the authority in the node
%%%     ---------------------------------------------------------------
%%%     PoA with a contract
%%%     - Can be enabled/disabled and switched to another algorithm
%%%     - No plugins are enabled
%%%     - Keyblocks contain the signature of the chosen authority taken from a contract
%%%     - The leader is chosen by a contract
%%%     - API for querying the authorities, consensus status
%%%     ---------------------------------------------------------------
%%%     HC Consensus
%%%     - Can be enabled/disabled amd switched to another algorithm
%%%     - Some plugins might get enabled
%%%     - Keyblocks contain hashes from another blockchain and a signature
%%%     - API for querying election/delegate status etc...
%%% @end
%%% -------------------------------------------------------------------

-module(aec_consensus).

%% API
-export([]).

%% Some consensus methods alter the node dramatically
%% Dev mode once enabled should never be turned off(unless the node restarts)
%% PoA or HC consensus can be turned off and changed to something else
-callback can_be_turned_off() -> boolean().

%% Some consensus implementations should ensure that it can be enabled at the given point in time
%% for instance by killing the peer pool, disabling the mining conductor etc...
-callback prepare_start() -> ok.

%% Should start the given consensus implementation - called only after prepare_start
-callback start() -> ok.
%% Should stop the given consensus implementation
-callback stop() -> ok.

%% Some consensus implementations might provide external http methods
%% Clique PoA might provide a voting API for delegates
%% HC might provide a debug endpoint for the staking contract and some insights
-callback is_providing_extra_http_endpoints() -> boolean().
-callback extra_http_endpoints() -> term().

%% Consensus algorithms might change the block structure and use the extra field
%% Extra data might only be derived from the header itself and the extra field is
%% never saved to the DB as it would be redundant. The main idea behind recycling
%% the existing fields instead of changing the serialization is to ensure no client
%% tooling breaks - middleware can just connect to a HC/PoA/Dev enabled node and
%% just work without refactoring code on the client side
-callback populate_extra(term()) -> term().

%% Callbacks for building the insertion ctx
-callback recent_cache_n() -> non_neg_integer().
-callback recent_cache_trim_header(term()) -> term().

%% This callback is called in dirty context before starting the block insertion
%% It's called only when the insertion context got properly created:
%%    - It's not a genesis block
%%    - The chain invariants were verified:
%%      * The block is not an orphan - prev block and prev key block are present and were validated
%%      * min(Height-GenesisHeight, recent_cache_n()) previous key headers are present in the DB
%%      * height deltas were checked
%%      * prev_block points to the same generation as prev_key_blocks
-callback dirty_validate_key_header(term()) -> ok | {error, term()}.
%%
%5-callback validate_key_header_with_state(term())

%% Consensus modules might define their own genesis block
-callback genesis_block_with_state() -> term().
-callback genesis_height() -> non_neg_integer().
-callback genesis_hash() -> non_neg_integer().
-callback genesis_state() -> aec_trees:trees().

-optional_callbacks([stop/0, extra_http_endpoints/0]).
