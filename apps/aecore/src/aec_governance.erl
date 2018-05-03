-module(aec_governance).

%% API
-export([sorted_protocol_versions/0,
         protocols/0,
         blocks_to_check_difficulty_count/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         max_txs_in_block/0,
         minimum_tx_fee/0,
         name_preclaim_expiration/0,
         name_claim_burned_fee/0,
         name_claim_max_expiration/0,
         name_protection_period/0,
         name_claim_preclaim_delta/0,
         name_registrars/0,
         micro_block_cycle/0]).

-export_type([protocols/0]).

-include("common.hrl").
-include("blocks.hrl").

-define(SORTED_VERSIONS, lists:sort(maps:keys(?PROTOCOLS))).
-define(PROTOCOLS,
        #{?PROTOCOL_VERSION => ?GENESIS_HEIGHT
         }).

-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 10).
-define(EXPECTED_BLOCK_MINE_RATE, 300000). %% 60secs * 1000ms * 5 = 300000msecs
-define(BLOCK_MINE_REWARD, 10).


%% Maps consensus protocol version to minimum height at which such
%% version is effective.  The height must be strictly increasing with
%% the version.
-type protocols() :: #{Version::non_neg_integer() => height()}.


sorted_protocol_versions() ->
    ?SORTED_VERSIONS.

protocols() ->
    case aeu_env:user_map([<<"chain">>, <<"hard_forks">>]) of
        undefined -> ?PROTOCOLS;
        {ok, M} ->
            maps:from_list(
              lists:map(
                fun({K, V}) -> {binary_to_integer(K), V} end,
                maps:to_list(M)))
    end.

blocks_to_check_difficulty_count() ->
    ?BLOCKS_TO_CHECK_DIFFICULTY_COUNT.

expected_block_mine_rate() ->
    aeu_env:user_config_or_env([<<"mining">>, <<"expected_mine_rate">>],
                               aecore, expected_mine_rate, ?EXPECTED_BLOCK_MINE_RATE).

block_mine_reward() ->
    ?BLOCK_MINE_REWARD.

max_txs_in_block() ->
    %% TODO: Consider trade-offs sync latency vs pow time
    %%       Relate to transaction size expressed with gas (when we have channels)
    10946.

minimum_tx_fee() ->
    1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Naming system variables

name_preclaim_expiration() ->
    300.

name_claim_burned_fee() ->
    3.

name_claim_max_expiration() ->
    50000.

name_protection_period() ->
    2016.

name_claim_preclaim_delta() ->
    1.

name_registrars() ->
    [<<"aet">>, <<"test">>].

micro_block_cycle() ->
    %% Miliseconds
    30000.
