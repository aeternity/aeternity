%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% @doc Epoch-length delta arithmetic of the end-of-epoch length vote.
%%%
%%% The delta a validator proposes at the end of child epoch N is derived from
%%% the wall-clock distance between the two parent blocks that seed epochs N and
%%% N+1, measured against `CurrentLength * BlockTime'. The quotient is bucketed
%%% by `ceil/1' below the expectation and `floor/1' above it, so the delta only
%%% moves in whole `BlockTime' steps and a window sitting on a bucket edge is
%%% decided by a single millisecond.
%%%
%%% The measured values below are the epoch-5..7 windows recorded by
%%% aehttp_hyperchains_SUITE:epochs_with_fast_parent/1 on the nightly gate; they
%%% are pinned here so the bucket edges stay observable without a multi-node run.
-module(aec_eoe_length_vote_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_eoe_length_vote).

%% The suite's constants: ?CHILD_EPOCH_LENGTH and ?CHILD_BLOCK_TIME.
-define(LEN, 10).
-define(BLOCK_TIME, 800).

%% First epoch whose delta is computed rather than hard-zeroed.
-define(LIVE_EPOCH, 5).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% The delta the module would propose for `Epoch' when the parent blocks seeding
%% `Epoch' and `Epoch + 1' are `TimeDiff' milliseconds apart.
delta(Epoch, TimeDiff, Len, BlockTime) ->
    ParentBlocks = #{Epoch     => parent_block(1, 0),
                     Epoch + 1 => parent_block(2, TimeDiff)},
    delta_(Epoch, ParentBlocks, Len, BlockTime).

%% Same, but with the parent blocks supplied directly - used for the
%% missing-block cases, which cannot be expressed as a TimeDiff.
delta_(Epoch, ParentBlocks, Len, BlockTime) ->
    State = ?TEST_MODULE:init_state(Epoch, <<"seed">>, ParentBlocks, Len,
                                    BlockTime, ?TEST_MODULE:init([])),
    %% #data{length, length_delta} - private to the module under test, so reach
    %% for it positionally rather than duplicating the record definition.
    ?assertEqual(data, element(1, State)),
    ?assertEqual(Len, element(2, State)),
    element(3, State).

parent_block(N, Time) ->
    aec_parent_chain_block:new(<<N:256>>, 80 + N, <<(N - 1):256>>, Time).

%%%===================================================================
%%% Tests
%%%===================================================================

%% A parent keeping pace asks for no change, and the expectation itself is the
%% floor branch rather than the ceil branch: 8000 is not < 8000.
on_pace_proposes_no_change_test() ->
    ?assertEqual(0, delta(?LIVE_EPOCH, ?LEN * ?BLOCK_TIME, ?LEN, ?BLOCK_TIME)).

%% The -1/0 bucket edge. `ceil/1' makes the boundary itself shorten, so exactly
%% nine nominal block times still proposes -1 while one millisecond more does
%% not. A test that arranges a nine-block window is therefore deciding its own
%% outcome on sub-millisecond block timing.
minus_one_bucket_edge_test_() ->
    NineBlocks = 9 * ?BLOCK_TIME,
    [?_assertEqual(-1, delta(?LIVE_EPOCH, NineBlocks, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual( 0, delta(?LIVE_EPOCH, NineBlocks + 1, ?LEN, ?BLOCK_TIME))].

%% Windows measured on the nightly gate, cycle 20260816T220042Z and its
%% neighbours. 7144 ms is the epoch-5 window of a passing ct-ceres run and 7919
%% ms the epoch-5 window of the failing ct-arcus run: both are stretches the
%% parent produced at its *default* rate, and they straddle the edge above.
measured_default_rate_windows_test_() ->
    [?_assertEqual(-1, delta(?LIVE_EPOCH, 7066, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(-1, delta(?LIVE_EPOCH, 7144, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(-1, delta(?LIVE_EPOCH, 7193, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual( 0, delta(?LIVE_EPOCH, 7919, ?LEN, ?BLOCK_TIME))].

%% The epoch-6 and epoch-7 windows from the same runs - a parent genuinely
%% producing at twice the rate. These sit whole buckets away from the edge, so
%% they are what a fast-parent assertion should be reading.
measured_doubled_rate_windows_test_() ->
    [?_assertEqual(-4, delta(?LIVE_EPOCH, 4005, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(-5, delta(?LIVE_EPOCH, 3957, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(-5, delta(?LIVE_EPOCH, 3941, ?LEN, ?BLOCK_TIME))].

%% A parent falling behind lengthens the epoch, bucketed by floor/1.
slow_parent_lengthens_test_() ->
    [?_assertEqual(0, delta(?LIVE_EPOCH, 11 * ?BLOCK_TIME - 1, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(1, delta(?LIVE_EPOCH, 11 * ?BLOCK_TIME, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(8, delta(?LIVE_EPOCH, 18 * ?BLOCK_TIME, ?LEN, ?BLOCK_TIME))].

%% Shortening is clamped so an epoch never drops below a single block.
shortening_is_clamped_to_one_block_test_() ->
    [?_assertEqual(1 - ?LEN, delta(?LIVE_EPOCH, 0, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(1 - ?LEN, delta(?LIVE_EPOCH, ?BLOCK_TIME, ?LEN, ?BLOCK_TIME))].

%% The first four epochs share a seed, so their delta is hard-zero whatever the
%% parent did. An assertion on epoch =< 4 can never observe an adjustment.
early_epochs_are_hard_zero_test_() ->
    [?_assertEqual(0, delta(E, 0, ?LEN, ?BLOCK_TIME)) || E <- lists:seq(1, 4)].

%% Either parent block missing makes the vote a silent no-op rather than an
%% error: the module substitutes the expectation, which buckets to 0.
missing_parent_block_is_a_silent_no_op_test_() ->
    Present = parent_block(1, 0),
    E = ?LIVE_EPOCH,
    [?_assertEqual(0, delta_(E, #{}, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(0, delta_(E, #{E => Present}, ?LEN, ?BLOCK_TIME)),
     ?_assertEqual(0, delta_(E, #{E + 1 => Present}, ?LEN, ?BLOCK_TIME))].
