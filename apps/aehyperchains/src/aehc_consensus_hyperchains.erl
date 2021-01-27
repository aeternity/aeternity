%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Hyperchains consensus - https://github.com/aeternity/hyperchains-whitepaper
%%% @end
%%% -------------------------------------------------------------------
-module(aehc_consensus_hyperchains).

-behavior(aec_consensus).

-define(TAG, 13370).

%% API
-export([ can_be_turned_off/0
        , assert_config/1
        , start/1
        , stop/0
        , is_providing_extra_http_endpoints/0
        , client_request/1
        %% Deserialization
        , extra_from_header/1
        %% Building the Insertion Ctx
        , recent_cache_n/0
        , recent_cache_trim_key_header/1
        %% Target adjustment when creating keyblocks
        , keyblocks_for_target_calc/0
        , keyblock_create_adjust_target/2
        %% Preconductor hook
        , dirty_validate_block_pre_conductor/1
        %% Dirty validation before starting the state transition
        , dirty_validate_key_node_with_ctx/3
        , dirty_validate_micro_node_with_ctx/3
        %% State transition
        , state_pre_transform_key_node_consensus_switch/2
        , state_pre_transform_key_node/2
        , state_pre_transform_micro_node/2
        %% Block rewards
        , state_grant_reward/3
        %% PoGF
        , pogf_detected/2
        %% Genesis block
        , genesis_transform_trees/2
        , genesis_raw_header/0
        , genesis_difficulty/0
        %% Keyblock sealing
        , key_header_for_sealing/1
        , validate_key_header_seal/2
        , generate_key_header_seal/5
        , set_key_block_seal/2
        , nonce_for_sealing/1
        , next_nonce_for_sealing/2
        , trim_sealing_nonce/2
        %% Block target and difficulty
        , default_target/0
        , assert_key_target_range/1
        , key_header_difficulty/1 ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecore/include/blocks.hrl").

can_be_turned_off() -> true.
assert_config(_Config) -> ok.
start(_Config) -> ok.
stop() -> ok.

is_providing_extra_http_endpoints() -> false.
client_request(_) -> error(todo).

extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 1.
recent_cache_trim_key_header(_) -> ok.

keyblocks_for_target_calc() -> 0.
keyblock_create_adjust_target(Block, []) -> {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
%% Don't waste CPU cycles when we are only interested in state transitions...
dirty_validate_key_node_with_ctx(_Node, _Block, _Ctx) -> ok.
dirty_validate_micro_node_with_ctx(_Node, _Block, _Ctx) -> ok.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(_Node, Trees) -> Trees.
state_pre_transform_key_node(_Node, Trees) -> Trees.
state_pre_transform_micro_node(_Node, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Trees, Amount) -> aec_consensus_bitcoin_ng:state_grant_reward(Beneficiary, Trees, Amount).

%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok.

%% -------------------------------------------------------------------
%% Genesis block
genesis_transform_trees(Trees, #{}) -> Trees.
genesis_raw_header() ->
    aec_headers:new_key_header(
        0,
        aec_governance:contributors_messages_hash(),
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
        <<0:32/unit:8>>,
        <<0:?MINER_PUB_BYTES/unit:8>>,
        <<0:?BENEFICIARY_PUB_BYTES/unit:8>>,
        ?TAG,
        no_value,
        0,
        0,
        default,
        ?ROMA_PROTOCOL_VSN).
genesis_difficulty() -> 0.

key_header_for_sealing(Header) ->
    aec_headers:root_hash(Header).

validate_key_header_seal(_Header, _Protocol) ->
    ok.

generate_key_header_seal(_, _, ?TAG, _, _) ->
    { continue_mining, {ok, ?TAG} }.

set_key_block_seal(Block, ?TAG) ->
    aec_blocks:set_nonce_and_pow(Block, ?TAG, [?TAG]).

nonce_for_sealing(_Header) ->
    ?TAG.

next_nonce_for_sealing(?TAG, _) ->
    ?TAG.

trim_sealing_nonce(?TAG, _) ->
    ?TAG.

default_target() ->
    ?TAG.

assert_key_target_range(?TAG) ->
    ok.

key_header_difficulty(_) ->
    ?TAG.
