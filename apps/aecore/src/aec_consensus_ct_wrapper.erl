%%% @doc A wrapper module for `aec_consensus' to facilitate testing.
%%% This module proxies all calls ALMOST transparently to the wrapped module, which also
%%% implements the `aec_consensus' behaviour. The intent is to allow the test suite to orchestrate
%%% the flow of blocks production by defining the blocks flow as a list of "commands".
%%% @end
-module(aec_consensus_ct_wrapper).

-behaviour(aec_consensus).

-export([
    allow_lazy_leader/0,
    assert_config/1,
    assert_key_target_range/1,
    beneficiary/0,
    can_be_turned_off/0,
    client_request/1,
    default_target/0,
    dirty_validate_block_pre_conductor/1,
    dirty_validate_header_pre_conductor/1,
    dirty_validate_key_hash_at_height/2,
    dirty_validate_key_node_with_ctx/3,
    dirty_validate_micro_node_with_ctx/3,
    extra_from_header/1,
    generate_key_header_seal/5,
    genesis_difficulty/0,
    genesis_raw_header/0,
    genesis_transform_trees/2,
    get_block_producer_configs/0,
    get_sign_module/0,
    get_type/0,
    is_leader_valid/4,
    is_providing_extra_http_endpoints/0,
    key_header_difficulty/1,
    key_header_for_sealing/1,
    keyblock_create_adjust_target/2,
    keyblocks_for_target_calc/0,
    next_beneficiary/0,
    next_nonce_for_sealing/2,
    nonce_for_sealing/1,
    pick_lazy_leader/1,
    pogf_detected/2,
    recent_cache_n/0,
    recent_cache_trim_key_header/1,
    set_key_block_seal/2,
    start/2,
    state_grant_reward/4,
    state_pre_transform_key_node_consensus_switch/2,
    state_pre_transform_key_node/3,
    state_pre_transform_micro_node/2,
    trim_sealing_nonce/2,
    validate_key_header_seal/2
]).

%% notimpl in aec_consensus_hc: extra_http_endpoints/0
-export([stop/0]).

%% Remember to set this in process dictionary of every instance of `aec_consensus_ct_wrapper`
% -define(WRAPPED_MODULE, erlang:get(consensus_module)).
-define(WRAPPED_MODULE, aec_consensus_hc).

%% ---------------------------------------------------------------------------
%% Optional callbacks for `aec_consensus`
%% ---------------------------------------------------------------------------

stop() ->
    ?WRAPPED_MODULE:stop().

%% ---------------------------------------------------------------------------
%% Required callbacks for `aec_consensus`
%% ---------------------------------------------------------------------------

can_be_turned_off() ->
    ?WRAPPED_MODULE:can_be_turned_off().

assert_config(Config) ->
    ?WRAPPED_MODULE:assert_config(Config).

start(Config, Arg2) ->
    ?WRAPPED_MODULE:start(Config, Arg2).

is_providing_extra_http_endpoints() ->
    ?WRAPPED_MODULE:is_providing_extra_http_endpoints().

client_request(Arg1) ->
    ?WRAPPED_MODULE:client_request(Arg1).

extra_from_header(Arg1) ->
    ?WRAPPED_MODULE:extra_from_header(Arg1).

recent_cache_n() ->
    ?WRAPPED_MODULE:recent_cache_n().

recent_cache_trim_key_header(Arg1) ->
    ?WRAPPED_MODULE:recent_cache_trim_key_header(Arg1).

keyblocks_for_target_calc() ->
    ?WRAPPED_MODULE:keyblocks_for_target_calc().

keyblock_create_adjust_target(Block, Arg2) ->
    ?WRAPPED_MODULE:keyblock_create_adjust_target(Block, Arg2).

dirty_validate_block_pre_conductor(Arg1) ->
    ?WRAPPED_MODULE:dirty_validate_block_pre_conductor(Arg1).

dirty_validate_header_pre_conductor(Arg1) ->
    ?WRAPPED_MODULE:dirty_validate_header_pre_conductor(Arg1).

dirty_validate_key_hash_at_height(Arg1, Arg2) ->
    ?WRAPPED_MODULE:dirty_validate_key_hash_at_height(Arg1, Arg2).

dirty_validate_key_node_with_ctx(Node, Block, Ctx) ->
    ?WRAPPED_MODULE:dirty_validate_key_node_with_ctx(Node, Block, Ctx).

dirty_validate_micro_node_with_ctx(Node, Block, Ctx) ->
    ?WRAPPED_MODULE:dirty_validate_micro_node_with_ctx(Node, Block, Ctx).

state_pre_transform_key_node_consensus_switch(Node, Trees) ->
    ?WRAPPED_MODULE:state_pre_transform_key_node_consensus_switch(Node, Trees).

state_pre_transform_key_node(Node, PrevNode, Trees) ->
    ?WRAPPED_MODULE:state_pre_transform_key_node(Node, PrevNode, Trees).

state_pre_transform_micro_node(Node, Trees) ->
    ?WRAPPED_MODULE:state_pre_transform_micro_node(Node, Trees).

state_grant_reward(Beneficiary, Node, Trees, Amount) ->
    ?WRAPPED_MODULE:state_grant_reward(Beneficiary, Node, Trees, Amount).

pogf_detected(H1, H2) ->
    ?WRAPPED_MODULE:pogf_detected(H1, H2).

genesis_transform_trees(Trees, Arg2) ->
    ?WRAPPED_MODULE:genesis_transform_trees(Trees, Arg2).

genesis_raw_header() ->
    ?WRAPPED_MODULE:genesis_raw_header().

genesis_difficulty() ->
    ?WRAPPED_MODULE:genesis_difficulty().

beneficiary() ->
    ?WRAPPED_MODULE:beneficiary().

next_beneficiary() ->
    ?WRAPPED_MODULE:next_beneficiary().

allow_lazy_leader() ->
    ?WRAPPED_MODULE:allow_lazy_leader().

pick_lazy_leader(TopHash) ->
    ?WRAPPED_MODULE:pick_lazy_leader(TopHash).

get_sign_module() ->
    ?WRAPPED_MODULE:get_sign_module().

get_type() ->
    ?WRAPPED_MODULE:get_type().

get_block_producer_configs() ->
    ?WRAPPED_MODULE:get_block_producer_configs().

is_leader_valid(Node, Trees, TxEnv, PrevNode) ->
    ?WRAPPED_MODULE:is_leader_valid(Node, Trees, TxEnv, PrevNode).

key_header_for_sealing(Arg1) ->
    ?WRAPPED_MODULE:key_header_for_sealing(Arg1).

validate_key_header_seal(Header, Protocol) ->
    ?WRAPPED_MODULE:validate_key_header_seal(Header, Protocol).

generate_key_header_seal(Arg1, Candidate, PCHeight, Config, Arg5) ->
    ?WRAPPED_MODULE:generate_key_header_seal(Arg1, Candidate, PCHeight, Config, Arg5).

set_key_block_seal(KeyBlock, Seal) ->
    ?WRAPPED_MODULE:set_key_block_seal(KeyBlock, Seal).

nonce_for_sealing(Header) ->
    ?WRAPPED_MODULE:nonce_for_sealing(Header).

next_nonce_for_sealing(PCHeight, Arg2) ->
    ?WRAPPED_MODULE:next_nonce_for_sealing(PCHeight, Arg2).

trim_sealing_nonce(PCHeight, Arg2) ->
    ?WRAPPED_MODULE:trim_sealing_nonce(PCHeight, Arg2).

default_target() ->
    ?WRAPPED_MODULE:default_target().

assert_key_target_range(Arg1) ->
    ?WRAPPED_MODULE:assert_key_target_range(Arg1).

key_header_difficulty(H) ->
    ?WRAPPED_MODULE:key_header_difficulty(H).
