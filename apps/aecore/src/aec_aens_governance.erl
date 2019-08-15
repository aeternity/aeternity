%%%-------------------------------------------------------------------
%%% @doc Placeholder for functions instrumenting Naming auctions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_aens_governance).

%% API
-export([init_fee_at_length/1,
         bid_timeout_at_length/1,
         get_base_fee/0]).

-define(MULTIPLIER_14, 100000000000000).
-define(MULTIPLIER_DAY, 480).


get_base_fee() ->
    %% INFO: calling old governance, to make sure non-auction
    %%       is compatible with pre-lima fees
    aec_governance:name_claim_locked_fee() * ?MULTIPLIER_14.

-spec init_fee_at_length(non_neg_integer()) -> non_neg_integer().
init_fee_at_length(Length) when not is_integer(Length) orelse Length < 1 ->
    error({bad_height, Length});

init_fee_at_length(Length) when Length > 31 -> get_base_fee();

init_fee_at_length(31) -> 3 * ?MULTIPLIER_14;
init_fee_at_length(30) -> 5 * ?MULTIPLIER_14;
init_fee_at_length(29) -> 8 * ?MULTIPLIER_14;
init_fee_at_length(28) -> 13 * ?MULTIPLIER_14;
init_fee_at_length(27) -> 21 * ?MULTIPLIER_14;
init_fee_at_length(26) -> 34 * ?MULTIPLIER_14;
init_fee_at_length(25) -> 55 * ?MULTIPLIER_14;
init_fee_at_length(24) -> 89 * ?MULTIPLIER_14;
init_fee_at_length(23) -> 144 * ?MULTIPLIER_14;
init_fee_at_length(22) -> 233 * ?MULTIPLIER_14;
init_fee_at_length(21) -> 377 * ?MULTIPLIER_14;
init_fee_at_length(20) -> 610 * ?MULTIPLIER_14;
init_fee_at_length(19) -> 987 * ?MULTIPLIER_14;
init_fee_at_length(18) -> 1597 * ?MULTIPLIER_14;
init_fee_at_length(17) -> 2584 * ?MULTIPLIER_14;
init_fee_at_length(16) -> 4181 * ?MULTIPLIER_14;
init_fee_at_length(15) -> 6765 * ?MULTIPLIER_14;
init_fee_at_length(14) -> 10946 * ?MULTIPLIER_14;
init_fee_at_length(13) -> 17711 * ?MULTIPLIER_14;
init_fee_at_length(12) -> 28657 * ?MULTIPLIER_14;
init_fee_at_length(11) -> 46368 * ?MULTIPLIER_14;
init_fee_at_length(10) -> 75025 * ?MULTIPLIER_14;
init_fee_at_length(9) -> 121393 * ?MULTIPLIER_14;
init_fee_at_length(8) -> 196418 * ?MULTIPLIER_14;
init_fee_at_length(7) -> 317811 * ?MULTIPLIER_14;
init_fee_at_length(6) -> 514229 * ?MULTIPLIER_14;
init_fee_at_length(5) -> 832040 * ?MULTIPLIER_14;
init_fee_at_length(4) -> 1346269 * ?MULTIPLIER_14;
init_fee_at_length(3) -> 2178309 * ?MULTIPLIER_14;
init_fee_at_length(2) -> 3524578 * ?MULTIPLIER_14;
init_fee_at_length(1) -> 5702887 * ?MULTIPLIER_14.


bid_timeout_at_length(Length) when not is_integer(Length) orelse Length < 1 ->
    error({bad_height, Length});
bid_timeout_at_length(Length) when Length > 32 -> 1;
bid_timeout_at_length(Length) when Length > 8 -> 1 * ?MULTIPLIER_DAY;  %% 480 blocks
bid_timeout_at_length(Length) when Length > 4 -> 31 * ?MULTIPLIER_DAY; %% 14880 blocks
bid_timeout_at_length(Length) when Length > 0 -> 62 * ?MULTIPLIER_DAY. %% 29760 blocks
