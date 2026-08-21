-module(aehttp_logic_tests).

%% Tests for the parts of aehttp_logic that do not need a running node.

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aehttp_logic).

%% http:gas_price:min_relay_gas_price, read here through the aehttp app env - the
%% same path aeu_env falls back to when the user config file does not set it.
set_min_relay(undefined) ->
    application:unset_env(aehttp, gas_price);
set_min_relay(V) ->
    application:set_env(aehttp, gas_price, [{min_relay_gas_price, V}]).

set_min_relay(V, UtilV) ->
    application:set_env(aehttp, gas_price,
                        [{min_relay_gas_price, V},
                         {reporting_utilization_override, UtilV}]).

gas_price_env_setup() ->
    application:get_env(aehttp, gas_price).

gas_price_env_teardown(undefined) ->
    application:unset_env(aehttp, gas_price);
gas_price_env_teardown({ok, Prev}) ->
    application:set_env(aehttp, gas_price, Prev).

with_gas_price_env(Tests) ->
    {foreach, fun gas_price_env_setup/0, fun gas_price_env_teardown/1, Tests}.

%%%===================================================================
%%% Default state: absent means off, and off means today's behaviour
%%%===================================================================

default_off_test_() ->
    with_gas_price_env(
      [ {"an unset key reads as disabled",
         fun() ->
             set_min_relay(undefined),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price())
         end}
      , {"with the key unset every reported price passes through untouched",
         fun() ->
             set_min_relay(undefined),
             [ ?assertEqual(P, ?TEST_MODULE:apply_min_relay_gas_price(P))
               || P <- [0, 1, 1000000000, 1000000000000] ]
         end}
      , {"with the key unset the reported {price, utilization} pair passes "
         "through untouched - off has to stay byte-identical on the wire",
         fun() ->
             set_min_relay(undefined),
             [ ?assertEqual({P, U}, ?TEST_MODULE:apply_min_relay_gas_price(P, U))
               || P <- [0, 1, 1000000000, 1000000000000],
                  U <- [0, 1, 10, 69, 70, 71, 100] ]
         end}
      ]).

%%%===================================================================
%%% Enabled: a floor, never a substitution
%%%===================================================================

floor_semantics_test_() ->
    with_gas_price_env(
      [ {"a price below the floor is raised to it",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_relay_gas_price(1000000000))
         end}
      , {"a price already above the floor is left alone - the floor may only "
         "raise the reported figure, never lower it below what the chain shows",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(900000000000, ?TEST_MODULE:apply_min_relay_gas_price(900000000000))
         end}
      , {"a price exactly at the floor is unchanged",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_relay_gas_price(500000000000))
         end}
      , {"an empty window reports the floor rather than 0",
         %% get_top_blocks_gas_price_summary/1 yields 0 when no micro block fell
         %% inside the window (aehttp_logic:min_gas_price/1).
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_relay_gas_price(0))
         end}
      ]).

%%%===================================================================
%%% Utilization: raised only where the floor actually moved the price
%%%===================================================================

%% THE OPT-IN. Setting the price floor alone must not start publishing a
%% congestion figure the chain did not show - that is a second, deliberate
%% decision, and the default of 0 is what keeps the two apart.
utilization_default_test_() ->
    with_gas_price_env(
      [ {"the default is off: the price floor alone reports the utilization the "
         "chain actually showed",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override()),
             ?assertEqual({500000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      , {"and it stays off on the cheapest window the chain can actually show, "
         "where a substituted figure would be least visible",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual({500000000000, 0},
                          ?TEST_MODULE:apply_min_relay_gas_price(1, 0))
         end}
      , {"an explicit figure is what gets reported, so an operator who wants one "
         "has asked for it",
         fun() ->
             set_min_relay(500000000000, 85),
             ?assertEqual(85, ?TEST_MODULE:reporting_utilization_override()),
             ?assertEqual({500000000000, 85},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      ]).

%% Configured, the override reaches exactly the windows whose price the floor
%% actually raised - and never lowers a figure the chain did show.
utilization_raised_when_the_floor_moves_test_() ->
    with_gas_price_env(
      [ {"on a quiet window the configured figure is reported alongside the "
         "raised price",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      , {"a genuinely cheap window - 1 aetto observed, 0 utilization - gets "
         "both figures",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(1, 0))
         end}
      , {"a window whose real utilization is already higher keeps its real "
         "figure - the override may only raise it",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 85},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 85))
         end}
      ]).

%% THE NO-DATA CASE. A 0 price is min_gas_price/1's no-observation marker - no
%% micro block fell inside the window. It is still a window sitting below the
%% floor, and it is raised like any other: a quiet chain is precisely when an
%% operator who configured a floor needs it advertised. An enabled floor is at
%% least 1 aetto, so a no-observation window is always below it.
empty_window_is_floored_test_() ->
    with_gas_price_env(
      [ {"with the feature off an empty window passes through untouched in "
         "BOTH fields - off has to stay byte-identical on the wire",
         fun() ->
             set_min_relay(undefined),
             ?assertEqual({0, 0}, ?TEST_MODULE:apply_min_relay_gas_price(0, 0))
         end}
      , {"configured, an empty window reports the floor and the configured "
         "utilization alongside it",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(0, 0))
         end}
      , {"the price floor ALONE reports the utilization the window arrived "
         "with - flooring an empty window is not an opt-in to the override",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override()),
             ?assertEqual({500000000000, 0},
                          ?TEST_MODULE:apply_min_relay_gas_price(0, 0))
         end}
      , {"1 aetto is the smallest floor an operator can enable, and even that "
         "is above a no-observation 0",
         fun() ->
             set_min_relay(1, 71),
             ?assertEqual({1, 71}, ?TEST_MODULE:apply_min_relay_gas_price(0, 0))
         end}
      , {"an empty window keeps whatever utilization it arrived with where "
         "that already exceeds the override - the override may only raise it. "
         "The endpoint only ever pairs a 0 price with 0 (stats_to_data/2), so "
         "this pins the contract rather than a reachable response",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 85},
                          ?TEST_MODULE:apply_min_relay_gas_price(0, 85))
         end}
      , {"no data is not a low price: ONE aetto really seen on chain is an "
         "observation, and is still floored",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(1, 0))
         end}
      ]).

%% THE NO-OP CASE. Where the chain's own price already meets the floor, the floor
%% brings about no outcome, so it buys no licence to report a utilization the
%% chain did not show.
%% The override is set throughout this group on purpose: with it at its default
%% of 0 every assertion below would hold vacuously, and would keep holding if the
%% "only where the floor moved the price" condition were dropped entirely.
utilization_untouched_when_the_floor_does_nothing_test_() ->
    with_gas_price_env(
      [ {"real price above the floor - price and utilization both as observed",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({900000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(900000000000, 10))
         end}
      , {"real price exactly AT the floor is the boundary: it is not moving the "
         "price, so utilization stays as observed",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(500000000000, 10))
         end}
      , {"one aetto below the floor is the other side of the boundary",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(499999999999, 10))
         end}
      , {"a floor below what the chain shows raises nothing, in either field",
         fun() ->
             set_min_relay(1, 71),
             [ ?assertEqual({P, U}, ?TEST_MODULE:apply_min_relay_gas_price(P, U))
               || P <- [1, 1000000000, 1000000000000], U <- [0, 10, 71, 100] ]
         end}
      ]).

utilization_figure_can_be_disabled_test_() ->
    with_gas_price_env(
      [ {"0 advertises the floor and reports utilization as observed",
         fun() ->
             set_min_relay(500000000000, 0),
             ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override()),
             ?assertEqual({500000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      , {"and 0 leaves a genuinely busy window alone too - it is max/2, not a "
         "substitution, so it can never lower a real figure",
         fun() ->
             set_min_relay(500000000000, 0),
             ?assertEqual({500000000000, 85},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 85))
         end}
      ]).

%% Absent and unreadable land in the same place: the node reports what it
%% observed rather than guessing at what the operator meant.
utilization_malformed_reads_as_observed_test_() ->
    with_gas_price_env(
      [ {"anything that is not an integer in 0..100 reports utilization as observed",
         fun() ->
             lists:foreach(
               fun(V) ->
                   set_min_relay(500000000000, V),
                   ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override()),
                   %% the price floor still applies
                   ?assertEqual({500000000000, 10},
                                ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
               end,
               %% 101 and up cannot come from a config file - the schema caps the
               %% key at 100 - but can still arrive through the app env.
               [null, -1, 101, 1000, 70.5, <<"71">>, "71", true])
         end}
      , {"a gas_price env carrying only the price half reports utilization as "
         "observed - the override does not follow the floor on",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override())
         end}
      , {"a gas_price env that is not a proplist at all reports utilization as "
         "observed and advertises no floor either",
         fun() ->
             application:set_env(aehttp, gas_price, not_a_proplist),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price()),
             ?assertEqual({1000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      ]).

%% A half-configured node must not start reporting congestion it did not see
%% alongside an untouched price.
utilization_inert_without_the_price_floor_test_() ->
    with_gas_price_env(
      [ {"the utilization figure alone reports nothing the chain did not show",
         fun() ->
             application:set_env(aehttp, gas_price,
                                 [{reporting_utilization_override, 71}]),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price()),
             [ ?assertEqual({P, U}, ?TEST_MODULE:apply_min_relay_gas_price(P, U))
               || P <- [0, 1000000000], U <- [0, 10, 100] ]
         end}
      , {"and it is inert under the price key's own off value, 0, as well",
         fun() ->
             set_min_relay(0, 71),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price()),
             ?assertEqual({1000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      ]).

%% GasPrices.utilization in apps/aehttp/priv/oas3.yaml is an integer 0..100, and
%% no configuration may take the reported value outside it.
utilization_stays_within_the_published_schema_test_() ->
    with_gas_price_env(
      [ {"no configuration can make the reported utilization leave 0..100",
         fun() ->
             lists:foreach(
               fun({FloorV, UtilV}) ->
                   set_min_relay(FloorV, UtilV),
                   lists:foreach(
                     fun({P, U}) ->
                         {_GP, Rep} = ?TEST_MODULE:apply_min_relay_gas_price(P, U),
                         ?assert(is_integer(Rep)),
                         ?assert(Rep >= 0 andalso Rep =< 100)
                     end,
                     [ {P, U} || P <- [0, 1, 1000000000, 500000000000, 900000000000],
                                 U <- [0, 10, 70, 71, 100] ])
               end,
               [ {V, UV} || V  <- [1, 1000000000, 500000000000],
                            UV <- [0, 1, 70, 71, 100, 101, -1, null] ])
         end}
      ]).

%%%===================================================================
%%% A malformed value degrades to off, not to some other number
%%%===================================================================

malformed_reads_as_off_test_() ->
    with_gas_price_env(
      [ {"anything that is not a positive integer disables the floor",
         fun() ->
             lists:foreach(
               fun(V) ->
                   set_min_relay(V),
                   ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price()),
                   ?assertEqual(1000000000, ?TEST_MODULE:apply_min_relay_gas_price(1000000000))
               end,
               %% `null' cannot come from a config file - the schema type is a
               %% plain integer - but can arrive through the app env.
               [null, undefined, 0, -1, <<"500">>, "500", 1.5])
         end}
      , {"a gas_price env that is not a proplist at all disables the floor",
         fun() ->
             application:set_env(aehttp, gas_price, not_a_proplist),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price())
         end}
      , {"a gas_price env carrying only the other key disables the floor",
         fun() ->
             application:set_env(aehttp, gas_price,
                                 [{reporting_utilization_override, 71}]),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price())
         end}
      ]).

%%%===================================================================
%%% Reporting only: the mempool's own floor must not move
%%%===================================================================

%% The enforced floor - aec_tx_pool:check_minimum_miner_gas_price/6 at admission,
%% check_candidate/10 at selection - reads mining:min_miner_gas_price. This is the
%% test that fails if a later refactor folds the two settings together.
does_not_move_the_enforced_floor_test_() ->
    with_gas_price_env(
      [ {"setting the relay price leaves aec_tx_pool's enforced floor exactly "
         "where it was",
         fun() ->
             set_min_relay(undefined),
             Before = aec_tx_pool:minimum_miner_gas_price(),
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:min_relay_gas_price()),
             ?assertEqual(Before, aec_tx_pool:minimum_miner_gas_price())
         end}
      , {"and the enforced floor is genuinely the mining: key, not this one",
         fun() ->
             set_min_relay(500000000000),
             application:set_env(aecore, mining_min_miner_gas_price, 12345),
             try
                 ?assertEqual(12345, aec_tx_pool:minimum_miner_gas_price()),
                 ?assertEqual(500000000000, ?TEST_MODULE:min_relay_gas_price())
             after
                 application:unset_env(aecore, mining_min_miner_gas_price)
             end
         end}
      , {"reporting a raised utilization does not move it either - the endpoint "
         "can report a busy chain without the pool believing it",
         fun() ->
             set_min_relay(undefined),
             Before = aec_tx_pool:minimum_miner_gas_price(),
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 5)),
             ?assertEqual(Before, aec_tx_pool:minimum_miner_gas_price())
         end}
      ]).
