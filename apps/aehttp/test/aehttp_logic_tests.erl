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
%%% Price (dry-run call objects, apply_min_relay_gas_price/1): a bottom cap
%%%===================================================================

%% This form adjusts an individual call object's own gas price in a public
%% dry-run result - it can only ever raise a figure the caller already set,
%% never replace it outright.
floor_semantics_test_() ->
    with_gas_price_env(
      [ {"a price below the floor is raised to it",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_relay_gas_price(1000000000))
         end}
      , {"a price already above the floor is left alone - the floor may only "
         "raise the reported figure, never lower it below what the caller set",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(900000000000, ?TEST_MODULE:apply_min_relay_gas_price(900000000000))
         end}
      , {"a price exactly at the floor is unchanged",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_relay_gas_price(500000000000))
         end}
      , {"0 is raised the same as any other low figure",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(500000000000, ?TEST_MODULE:apply_min_relay_gas_price(0))
         end}
      ]).

%%%===================================================================
%%% recent-gas-prices (apply_min_relay_gas_price/2): price is an outright
%%% override, utilization is an independent bottom cap
%%%===================================================================

%% THE PRICE. Configured, it is reported exactly as set, in every window,
%% regardless of what the chain actually showed there. A bottom cap (max)
%% would only ever surface the configured figure on a window that already
%% reads BELOW it - if the real price is already higher, a max would show
%% that instead, never the value the operator asked to advertise. An
%% operator who set this wants it advertised outright, so it is a plain
%% override, not a max.
price_is_reported_exactly_as_configured_test_() ->
    with_gas_price_env(
      [ {"a real price below the configured value is overridden to it",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual({500000000000, 0},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 0))
         end}
      , {"a real price ABOVE the configured value is still overridden to it - "
         "this is the case a bottom cap could never produce",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual({500000000000, 0},
                          ?TEST_MODULE:apply_min_relay_gas_price(900000000000, 0))
         end}
      , {"a real price exactly at the configured value is unaffected either way",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual({500000000000, 0},
                          ?TEST_MODULE:apply_min_relay_gas_price(500000000000, 0))
         end}
      , {"a 0 price - min_gas_price/1's no-observation marker for a window no "
         "micro block fell inside, not a cheap window - is overridden the same "
         "as any other value",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual({500000000000, 0},
                          ?TEST_MODULE:apply_min_relay_gas_price(0, 0))
         end}
      ]).

%% THE UTILIZATION. Off by default - setting the price alone must not start
%% publishing a congestion figure the chain did not show, since that is a
%% second, deliberate decision.
utilization_default_test_() ->
    with_gas_price_env(
      [ {"the default is off: the price override alone reports the "
         "utilization the chain actually showed",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override()),
             ?assertEqual({500000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      , {"an explicit figure is what gets reported, so an operator who wants "
         "one has asked for it",
         fun() ->
             set_min_relay(500000000000, 85),
             ?assertEqual(85, ?TEST_MODULE:reporting_utilization_override()),
             ?assertEqual({500000000000, 85},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      ]).

%% Configured, the override is a bottom cap applied to every window alike -
%% independent of the price override, and independent of whether that window
%% had any observation of its own.
utilization_is_a_bottom_cap_test_() ->
    with_gas_price_env(
      [ {"a real utilization below the override is raised to it",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      , {"a real utilization ABOVE the override keeps its real figure - the "
         "override may only raise it, unlike the price which is an outright "
         "override",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 85},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 85))
         end}
      , {"a real utilization exactly AT the override is unaffected either way",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 71))
         end}
      , {"a no-observation window (0 price, 0 utilization) is raised on both "
         "counts - the price by the override, the utilization by the bottom cap",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(0, 0))
         end}
      ]).

%% THE INDEPENDENCE. A window whose real price is already above the
%% configured value - a no-op for a bottom cap - still gets its utilization
%% raised: the two settings do not gate one another.
utilization_is_capped_even_where_price_would_be_a_no_op_test_() ->
    with_gas_price_env(
      [ {"real price already above the configured value is still overridden "
         "to it (see price_is_reported_exactly_as_configured_test_), and "
         "utilization is independently raised to the override",
         fun() ->
             set_min_relay(500000000000, 71),
             ?assertEqual({500000000000, 71},
                          ?TEST_MODULE:apply_min_relay_gas_price(900000000000, 10))
         end}
      , {"across a spread of real prices and utilizations, utilization always "
         "lands at max(Observed, Override) regardless of the price",
         fun() ->
             set_min_relay(1, 71),
             [ ?assertEqual({1, max(U, 71)}, ?TEST_MODULE:apply_min_relay_gas_price(P, U))
               || P <- [0, 1, 1000000000, 1000000000000], U <- [0, 10, 71, 100] ]
         end}
      ]).

%% 0 is utilization's own off value - a bottom cap of 0 is a no-op, so this
%% is the one case where the override cannot raise a real figure.
utilization_figure_can_be_disabled_test_() ->
    with_gas_price_env(
      [ {"0 advertises the price override and reports utilization as observed",
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
                   %% the price override still applies
                   ?assertEqual({500000000000, 10},
                                ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
               end,
               %% 101 and up cannot come from a config file - the schema caps the
               %% key at 100 - but can still arrive through the app env.
               [null, -1, 101, 1000, 70.5, <<"71">>, "71", true])
         end}
      , {"a gas_price env carrying only the price half reports utilization as "
         "observed - the override does not follow the price on",
         fun() ->
             set_min_relay(500000000000),
             ?assertEqual(0, ?TEST_MODULE:reporting_utilization_override())
         end}
      , {"a gas_price env that is not a proplist at all reports utilization as "
         "observed and advertises no price override either",
         fun() ->
             application:set_env(aehttp, gas_price, not_a_proplist),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price()),
             ?assertEqual({1000000000, 10},
                          ?TEST_MODULE:apply_min_relay_gas_price(1000000000, 10))
         end}
      ]).

%% A half-configured node must not start reporting congestion it did not see
%% alongside an untouched price.
utilization_inert_without_the_price_override_test_() ->
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
      [ {"anything that is not a positive integer disables the override",
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
      , {"a gas_price env that is not a proplist at all disables the override",
         fun() ->
             application:set_env(aehttp, gas_price, not_a_proplist),
             ?assertEqual(undefined, ?TEST_MODULE:min_relay_gas_price())
         end}
      , {"a gas_price env carrying only the other key disables the override",
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
