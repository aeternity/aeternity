%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus behavior for customizing the node
%%%      Consensus is responsible for validating, emitting and processing of keyblocks
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
%%%     - Can be enabled/disabled and switched to another algorithm
%%%     - Some plugins might get enabled
%%%     - Keyblocks contain hashes from another blockchain and a signature
%%%     - API for querying election/delegate status etc...
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus).

%% API
-export([]).

%% -------------------------------------------------------------------
%% Global consensus features
%% -------------------------------------------------------------------
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
%% Special consensus features - some of them are exposed via the above http endpoints
-callback client_request(term()) -> term().

%% -------------------------------------------------------------------
%% Deserialization of headers
%% -------------------------------------------------------------------
%% Consensus algorithms might change the block structure and use the extra field
%% Extra data might only be derived from the header itself and the extra field is
%% never saved to the DB as it would be redundant. The main idea behind recycling
%% the existing fields instead of changing the serialization is to ensure no client
%% tooling breaks - middleware can just connect to a HC/PoA/Dev enabled node and
%% just work without refactoring code on the client side
-callback extra_from_header(aec_headers:header()) -> map().

%% -------------------------------------------------------------------
%% Block insertion and state transitions
%% -------------------------------------------------------------------
%% Callbacks for building the db insertion ctx
-callback recent_cache_n() -> non_neg_integer().
-callback recent_cache_trim_header(aec_headers:header()) -> term().

%% This callback is called in dirty context before starting the block insertion
%% It's called only when the insertion context got properly created:
%%    - It's not a genesis block
%%    - The chain invariants were verified:
%%      * The block is not an orphan - prev block and prev key block are present and were validated
%%      * min(Height-GenesisHeight, recent_cache_n()) previous key headers are present in the DB
%%      * height deltas were checked
%%      * prev_block points to the same generation as prev_key_blocks
-callback dirty_validate_key_header(aec_headers:header()) -> ok | {error, term()}.
%%
%%-callback validate_key_header_with_state(term())

%% Consensus modules might define their own genesis block
-callback genesis_block_with_state() -> {aec_blocks:block(), aec_trees:trees()}.
-callback genesis_height() -> non_neg_integer().
-callback genesis_header() -> aec_headers:header().
-callback genesis_state() -> aec_trees:trees().

%% -------------------------------------------------------------------
%% Block sealing
%% -------------------------------------------------------------------
%% Serializes the key header to the form expected for sealing
-callback key_header_for_sealing(aec_headers:header()) -> binary().
%% Validates the crypto seal on the given key block
-callback validate_key_header_seal(aec_headers:header(), non_neg_integer()) -> ok | {error, term()}.
%% Tries to generate a crypto seal on the given key block
%% In case it's impossible to generate a seal for the given header - this callback can stop the mining
%% conductor by returning stop_mining. After the seal got properly created this callback can decide whether to continue mining
-type conductor_action() :: stop_mining | continue_mining.
-callback generate_key_header_seal(binary(), aec_headers:header(), non_neg_integer(), term(), term()) -> {conductor_action(), {ok, term()}} | {conductor_action(), {error, term()}}.
%% Inserts the generated crypto seal in the block header
-callback set_key_block_seal(aec_blocks:block(), term()) -> term().
%% Gets the nonce used for sealing - please note that this field might be used for instance for a custom voting protocol
%% Some consensus modules might have already calculated the nonce before and stored it in the header
%% PoW usually will increase the nonce after an unsuccessful mining attempt
-callback nonce_for_sealing(aec_headers:header()) -> non_neg_integer().
-callback next_nonce_for_sealing(non_neg_integer(), term()) -> non_neg_integer().
-callback trim_sealing_nonce(non_neg_integer(), term()) -> non_neg_integer().

-optional_callbacks([stop/0, extra_http_endpoints/0]).
