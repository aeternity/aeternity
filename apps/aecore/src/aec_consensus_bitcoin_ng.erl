%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc BitcoinNG consensus module
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_bitcoin_ng).
-behavior(aec_consensus).

%% API
-export([ can_be_turned_off/0
        , dirty_validate_key_header/0
        , prepare_start/0
        , start/0
        , stop/0
        , is_providing_extra_http_endpoints/0
        , extra_from_header/1
        , recent_cache_n/0
        , recent_cache_trim_header/1
        , dirty_validate_key_header/1
        , genesis_block_with_state/0
        , genesis_height/0
        , genesis_header/0
        , genesis_state/0
        , genesis_target/0
        , key_header_for_sealing/1
        , validate_key_header_seal/2
        , generate_key_header_seal/5
        , set_key_block_seal/2
        , nonce_for_sealing/1
        , next_nonce_for_sealing/2
        , trim_sealing_nonce/2
        , default_target/0
        , assert_key_target_range/1
        , key_header_difficulty/1 ]).

-include_lib("aeminer/include/aeminer.hrl").

can_be_turned_off() -> true.
dirty_validate_key_header() -> error(todo).
prepare_start() -> error(todo).
start() -> error(todo).
stop() -> error(todo).

is_providing_extra_http_endpoints() -> false.
extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 10.
recent_cache_trim_header(_) -> {}.

dirty_validate_key_header(_) -> error(todo).

genesis_block_with_state() -> error(todo).
genesis_height() -> error(todo).
genesis_header() -> error(todo).
genesis_state() -> error(todo).
-ifdef(TEST).
genesis_target() ->
   ?HIGHEST_TARGET_SCI.
-else.
genesis_target() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> 16#1F1F1F1F;
        _                -> ?HIGHEST_TARGET_SCI
    end.
-endif.

key_header_for_sealing(Header) ->
    Header1 = aec_headers:set_nonce(Header, 0),
    Header2 = aec_headers:set_key_seal(Header1, no_value),
    aec_headers:serialize_to_binary(Header2).

validate_key_header_seal(Header, _Protocol) ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined block
    %% got hashed.
    Nonce = aec_headers:nonce(Header),
    Evd = aec_headers:pow(Header),
    Target = aec_headers:target(Header),
    HeaderBinary = key_header_for_sealing(Header),
    case aec_mining:verify(HeaderBinary, Nonce, Evd, Target) of
        true ->
            ok;
        false ->
            {error, incorrect_pow}
    end.

generate_key_header_seal(HeaderBin, Header, Nonce, MinerConfig, AddressedInstance) ->
    Target = aec_headers:target(Header),
    { continue_mining,
      aec_mining:generate(HeaderBin, Target, Nonce, MinerConfig, AddressedInstance)
    }.

set_key_block_seal(Block, {Nonce, Evd}) ->
    aec_blocks:set_nonce_and_pow(Block, Nonce, Evd).

nonce_for_sealing(_Header) ->
    aeminer_pow:pick_nonce().

next_nonce_for_sealing(Nonce, MinerConfig) ->
    aeminer_pow:next_nonce(Nonce, MinerConfig).

trim_sealing_nonce(Nonce, MinerConfig) ->
    aeminer_pow:trim_nonce(Nonce, MinerConfig).

default_target() ->
    ?HIGHEST_TARGET_SCI.

assert_key_target_range(Target) ->
    ok.

key_header_difficulty(Header) ->
    aeminer_pow:target_to_difficulty(aec_headers:target(Header)).
