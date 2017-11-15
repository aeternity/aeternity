-module(aec_target).

%% API
-export([recalculate/2,
         determine_delta_header_height/1,
         verify/2]).

-include("common.hrl").
-include("blocks.hrl").

%% Return height of the header to be used as a start point for target calculations,
%% based on the following formula:
%% delta_height(Header) = Header.height - aec_governance:blocks_to_check_difficulty_count().
%% Returns {error | chain_too_short_to_recalculate_target} if initial height is a negative value
%% or it points to genesis block.
-spec determine_delta_header_height(
        header()) -> {ok, non_neg_integer()}
                         | {error, chain_too_short_to_recalculate_target}.
determine_delta_header_height(Header) ->
    Height = aec_headers:height(Header),
    BlocksCount = aec_governance:blocks_to_check_difficulty_count(),
    InitialHeight = Height - BlocksCount,
    GenesisHeight = aec_block_genesis:height(),
    case InitialHeight > GenesisHeight of
        true ->
            {ok, InitialHeight};
        false ->
            {error, chain_too_short_to_recalculate_target}
    end.

-spec recalculate(header(), header()) -> non_neg_integer().
recalculate(Header, InitialHeader) ->
    HeaderTarget = aec_headers:target(Header),
    ExpectedRate = aec_governance:expected_block_mine_rate(),
    CurrentRate = mining_rate_between(InitialHeader, Header),
    aec_pow:recalculate_target(HeaderTarget, ExpectedRate, CurrentRate).

-spec verify(header(), header()) -> ok | {error, target_too_high}.
verify(Header, InitialHeader) ->
    HeaderTarget = aec_headers:target(Header),
    ExpectedTarget = recalculate(Header, InitialHeader),
    %% Currently we don't verify if target was too low (difficulty was too high),
    %% so that someone might have tried to send a block
    %% with too high difficulty (too low target).
    %% TODO: Investigate whether we need to avoid "too difficult" blocks
    case HeaderTarget =< ExpectedTarget of
        true ->
            ok;
        false ->
            {error, target_too_high}
    end.

%% Internals

-spec mining_rate_between(header(), header()) -> integer().
mining_rate_between(InitialHeader, Header) ->
    NumOfBlocksBetween = aec_governance:blocks_to_check_difficulty_count(),
    Time1 = aec_headers:time_in_msecs(InitialHeader),
    Time2 = aec_headers:time_in_msecs(Header),
    TimeDiff = max(1, Time2 - Time1),
    TimeDiff div NumOfBlocksBetween.
