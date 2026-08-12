-module(aehttp_logic_tests).

%% Tests for the parts of aehttp_logic that do not need a running node.

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aehttp_logic).

%% The knob under test is http:dry_run:min_gas_price_override, read here through
%% the aehttp app env - the same path aeu_env falls back to when the user config
%% file does not set the key.
set_override(undefined) ->
    application:unset_env(aehttp, dry_run);
set_override(V) ->
    application:set_env(aehttp, dry_run, [{min_gas_price_override, V}]).

override_setup() ->
    Prev = application:get_env(aehttp, dry_run),
    Prev.

override_teardown(undefined) ->
    application:unset_env(aehttp, dry_run);
override_teardown({ok, Prev}) ->
    application:set_env(aehttp, dry_run, Prev).

with_override(Tests) ->
    {foreach, fun override_setup/0, fun override_teardown/1, Tests}.

%%%===================================================================
%%% Default state: absent means off, and off means today's behaviour
%%%===================================================================

default_off_test_() ->
    with_override(
      [ {"an unset knob reads as disabled",
         fun() ->
             set_override(undefined),
             ?assertEqual(undefined, ?TEST_MODULE:min_gas_price_override())
         end}
      , {"with the knob unset every reported price passes through untouched",
         fun() ->
             set_override(undefined),
             [ ?assertEqual(P, ?TEST_MODULE:apply_min_gas_price_override(P))
               || P <- [0, 1, 1000000000, 1000000000000] ]
         end}
      ]).

%%%===================================================================
%%% Enabled: a floor, never a substitution
%%%===================================================================

floor_semantics_test_() ->
    with_override(
      [ {"a price below the floor is raised to it",
         fun() ->
             set_override(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_gas_price_override(1000000000))
         end}
      , {"a price already above the floor is left alone - the override may only "
         "raise the reported figure, never lower it below what the chain shows",
         fun() ->
             set_override(500000000000),
             ?assertEqual(900000000000, ?TEST_MODULE:apply_min_gas_price_override(900000000000))
         end}
      , {"a price exactly at the floor is unchanged",
         fun() ->
             set_override(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_gas_price_override(500000000000))
         end}
      , {"an empty window reports the floor rather than 0",
         %% get_top_blocks_gas_price_summary/1 yields 0 when no micro block fell
         %% inside the window (aehttp_logic:min_gas_price/1), which is the case an
         %% operator setting a floor most wants covered.
         fun() ->
             set_override(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_gas_price_override(0))
         end}
      ]).

%%%===================================================================
%%% A malformed value degrades to off, not to some other number
%%%===================================================================

malformed_reads_as_off_test_() ->
    with_override(
      [ {"anything that is not a positive integer disables the override",
         fun() ->
             lists:foreach(
               fun(V) ->
                   set_override(V),
                   ?assertEqual(undefined, ?TEST_MODULE:min_gas_price_override()),
                   ?assertEqual(1000000000, ?TEST_MODULE:apply_min_gas_price_override(1000000000))
               end,
               %% `null' is what yamerl hands back for an explicit `null' in the
               %% YAML config, which the schema permits and which means off.
               [null, undefined, 0, -1, <<"500">>, "500", 1.5])
         end}
      , {"a dry_run env that is not a proplist at all disables the override",
         fun() ->
             application:set_env(aehttp, dry_run, not_a_proplist),
             ?assertEqual(undefined, ?TEST_MODULE:min_gas_price_override())
         end}
      , {"a dry_run env carrying only unrelated keys disables the override",
         fun() ->
             application:set_env(aehttp, dry_run, [{timeout_ms, 3000}]),
             ?assertEqual(undefined, ?TEST_MODULE:min_gas_price_override())
         end}
      ]).

%%%===================================================================
%%% Reporting only: the mempool's own floor must not move
%%%===================================================================

%% The whole safety argument for this knob is that it is enforced nowhere. The
%% floor that IS enforced - at mempool admission in
%% aec_tx_pool:check_minimum_miner_gas_price/6 and at candidate selection in
%% aec_tx_pool:check_candidate/10 - is read from mining:min_miner_gas_price via
%% aec_tx_pool:minimum_miner_gas_price/0, a different config path entirely. If a
%% later refactor ever folds the two together, this is the test that fails.
does_not_move_the_enforced_floor_test_() ->
    with_override(
      [ {"setting the reporting override leaves aec_tx_pool's enforced floor "
         "exactly where it was",
         fun() ->
             set_override(undefined),
             Before = aec_tx_pool:minimum_miner_gas_price(),
             set_override(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:min_gas_price_override()),
             ?assertEqual(Before, aec_tx_pool:minimum_miner_gas_price())
         end}
      , {"and the enforced floor is genuinely the mining: key, not this one",
         fun() ->
             set_override(500000000000),
             application:set_env(aecore, mining_min_miner_gas_price, 12345),
             try
                 ?assertEqual(12345, aec_tx_pool:minimum_miner_gas_price()),
                 ?assertEqual(500000000000, ?TEST_MODULE:min_gas_price_override())
             after
                 application:unset_env(aecore, mining_min_miner_gas_price)
             end
         end}
      ]).
