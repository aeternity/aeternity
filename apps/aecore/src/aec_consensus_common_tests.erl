%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus module for common tests - used when we are only interested in the state transitions
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_common_tests).

-export([]).

-ifdef(TEST).
-behavior(aec_consensus).

-define(TAG, 1337).

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
        %% Preconductor hook
        , dirty_validate_block_pre_conductor/1
        , dirty_validate_header_pre_conductor/1
        , dirty_validate_key_hash_at_height/2
        %% Dirty validation before starting the state transition
        , dirty_validate_key_node_with_ctx/3
        , dirty_validate_micro_node_with_ctx/3
        %% State transition
        , state_pre_transform_key_node_consensus_switch/4
        , state_pre_transform_key_node/4
        , state_pre_transform_micro_node/4
        %% Block rewards
        , state_grant_reward/3
        %% PoGF
        , pogf_detected/2
        %% Genesis block
        , genesis_transform_trees/2
        , genesis_raw_header/0
        , genesis_difficulty/0
        %% Keyblock creation
        , new_unmined_key_node/8
        , keyblocks_for_unmined_keyblock_adjust/0
        , adjust_unmined_keyblock/2
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
-include("blocks.hrl").

can_be_turned_off() -> false.
assert_config(_Config) -> ok.
start(_Config) -> ok.
stop() -> ok.

is_providing_extra_http_endpoints() -> false.
%% This is not yet dev mode but it's close :)
%% TODO: Expose via HTTP
client_request(emit_kb) ->
    TopHash = aec_chain:top_block_hash(),
    {ok, Beneficiary} = aec_conductor:get_beneficiary(),
    {ok, Block} = aec_block_key_candidate:create(TopHash, Beneficiary),
    ok = aec_conductor:add_synced_block(Block),
    Block;
client_request(emit_mb) ->
    TopHash = aec_chain:top_block_hash(),
    {ok, MicroBlock, _} = aec_block_micro_candidate:create(TopHash),
    {ok, MicroBlockS} = aec_keys:sign_micro_block(MicroBlock),
    ok = aec_conductor:post_block(MicroBlockS),
    MicroBlockS;
client_request({mine_blocks, NumBlocksToMine, Type}) ->
    case {aec_conductor:is_leader(), Type} of
        {_, any} ->
            %% Some test might expect to mine a tx - interleave KB with MB
            Pairs = NumBlocksToMine div 2,
            Rem = NumBlocksToMine rem 2,
            P = [[ client_request(emit_kb)
                 , client_request(emit_mb)
                 ] || _ <- lists:seq(1, Pairs)],
            R = [ client_request(emit_kb) || _ <- lists:seq(1, Rem)],
            {ok, lists:flatten([P,R])};
        {_, key} ->
            {ok, [client_request(emit_kb) || _ <- lists:seq(1, NumBlocksToMine)]};
        {true, micro} ->
            {ok, [client_request(emit_mb) || _ <- lists:seq(1, NumBlocksToMine)]};
        {false, micro} ->
            client_request(emit_kb),
            {ok, [client_request(emit_mb) || _ <- lists:seq(1, NumBlocksToMine)]}
    end;
client_request(mine_micro_block_emptying_mempool_or_fail) ->
    KB = client_request(emit_kb),
    MB = client_request(emit_mb),
    %% If instant mining is enabled then we can't have microforks :)
    {ok, []} = aec_tx_pool:peek(infinity),
    {ok, [KB, MB]};
client_request({mine_blocks_until_txs_on_chain, TxHashes, Max}) ->
    mine_blocks_until_txs_on_chain(TxHashes, Max, []).

mine_blocks_until_txs_on_chain(_TxHashes, 0, _Blocks) ->
    {error, max_reached};
mine_blocks_until_txs_on_chain(TxHashes, Max, Blocks) ->
    case aec_conductor:is_leader() of
        false ->
            KB = client_request(emit_kb),
            mine_blocks_until_txs_on_chain(TxHashes, Max-1, [KB|Blocks]);
        true ->
            MB = client_request(emit_mb),
            KB = client_request(emit_kb),
            NewAcc = [KB|Blocks],
            case txs_not_in_microblock(MB, TxHashes) of
                []        -> {ok, lists:reverse(NewAcc)};
                TxHashes1 -> mine_blocks_until_txs_on_chain(TxHashes1, Max - 1, NewAcc)
            end
    end.

txs_not_in_microblock(MB, TxHashes) ->
    [ TxHash || TxHash <- TxHashes, not tx_in_microblock(MB, TxHash) ].

tx_in_microblock(MB, TxHash) ->
    lists:any(fun(STx) ->
                      aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx)) == TxHash
              end, aec_blocks:txs(MB)).

extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 1.
recent_cache_trim_key_header(_) -> ok.

dirty_validate_block_pre_conductor(_) -> ok.
dirty_validate_header_pre_conductor(_) -> ok.
dirty_validate_key_hash_at_height(_, _) -> ok.
%% Don't waste CPU cycles when we are only interested in state transitions...
dirty_validate_key_node_with_ctx(_Node, _Block, _Ctx) -> ok.
dirty_validate_micro_node_with_ctx(_Node, _Block, _Ctx) -> ok.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.
state_pre_transform_key_node(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.
state_pre_transform_micro_node(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.

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

%% -------------------------------------------------------------------
%% Keyblock creation
new_unmined_key_node(PrevNode, PrevKeyNode, Height, Miner, Beneficiary, Protocol, InfoField, _TreesIn) ->
    FakeBlockHash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
    FakeStateHash = <<1337:?STATE_HASH_BYTES/unit:8>>,
    Header = aec_headers:new_key_header(
        Height,
        aec_block_insertion:node_hash(PrevNode),
        aec_block_insertion:node_hash(PrevKeyNode),
        FakeStateHash,
        Miner,
        Beneficiary,
        default_target(),
        no_value,
        0,
        aeu_time:now_in_msecs(),
        InfoField,
        Protocol),
    aec_chain_state:wrap_header(Header, FakeBlockHash).

keyblocks_for_unmined_keyblock_adjust() -> 0.
adjust_unmined_keyblock(Block, []) -> {ok, Block}.

key_header_for_sealing(Header) ->
    aec_headers:root_hash(Header).

validate_key_header_seal(_Header, _Protocol) ->
    ok.

generate_key_header_seal(_, _, ?TAG, _, _) ->
    { continue_mining, {ok, ?TAG} }.

set_key_block_seal(Block, ?TAG) ->
    aec_blocks:set_nonce_and_pow(Block, ?TAG, ?TAG).

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
-endif.
