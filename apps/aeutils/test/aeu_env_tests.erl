%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the example configuration in doc/examples
%%% @end
%%%=============================================================================
-module(aeu_env_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% logger handler callback, see config_error_reported_before_lager_starts_test_
-export([log/2]).

schema_test_() ->
    {setup,
     fun() -> setup() end,
     fun(SavedFork) -> teardown(SavedFork) end,
     [{"Example user configuration files pass schema validation",
       [fun() ->
                ?assertMatch({ok, _}, aeu_env:check_config(Config))
        end || Config <- test_data_config_files()]
      }]
    }.

extra_checks_test_() ->
    {setup,
     fun() ->
             ok = meck:new(setup, [passthrough]),
             setup()
     end,
     fun(SavedFork) ->
             teardown(SavedFork),
             ok = meck:unload(setup)
     end,
     [{"User configuration cannot contain both 'mining > cuckoo > edge_bits' and deprecated 'mining > cuckoo > miner'",
       fun deprecated_miner_section_conflicting_with_edge_bits/0},
      {"User configuration cannot contain both 'mining > cuckoo > miners' and deprecated 'mining > cuckoo > miner'",
       fun deprecated_miner_section_conflicting_with_miners/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_start_height' greater or equal to 'fork_management > fork > signalling_end_height'",
       fun invalid_fork_signalling_interval/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_start_height' lower or equal to the last scheduled hard fork height",
       fun invalid_fork_signalling_start_height/0},
      {"User configuration cannot contain 'fork_management > fork > signalling_block_count' greater than signalling interval",
       fun invalid_fork_signalling_block_count/0},
      {"User configuration cannot contain 'fork_management > fork > version' lower or equal to Minerva protocol version (2)",
       fun invalid_fork_signalling_version/0},
      {"User configuration cannot contain 'fork_management > fork > version' lower or equal to the last scheduled hard fork version",
       fun invalid_fork_signalling_version2/0}]
     ++ positive_extra_checks_tests()}.

extra_network_id_checks_test_() ->
    {setup,
     fun() ->
             ok = meck:new(setup, [passthrough]),
             ok = meck:new(aec_governance, [passthrough]),
             setup()
     end,
     fun(SavedFork) ->
             teardown(SavedFork),
             ok = meck:unload(aec_governance),
             ok = meck:unload(setup)
     end,
     [{"User configuration cannot contain more config properties for ae_uat than 'fork_management > fork > enabled'",
       fun() -> invalid_fork_signalling_network_config(<<"ae_uat">>) end},
      {"User configuration cannot contain more config properties for ae_mainnet than 'fork_management > fork > enabled'",
       fun() -> invalid_fork_signalling_network_config(<<"ae_mainnet">>) end}]
    }.

config_cache_test_() ->
    {foreach,
     fun() ->
             %% A config cached by an earlier test in this VM must not feed
             %% into the checks below.
             ok = aeu_env:invalidate_config_cache(),
             setup()
     end,
     fun(SavedFork) ->
             %% Drop the cache before anything that can fail: a leaked config
             %% would be served to every later test module sharing this VM.
             ok = aeu_env:invalidate_config_cache(),
             teardown(SavedFork)
     end,
     [{"setup's expansion is the identity for every example config file",
       fun expansion_is_identity/0},
      {"is_expandable/1 finds a '$' at any depth, in keys as well as values",
       fun is_expandable_covers_nested_terms/0},
      {"a config with nothing to expand is cached; reads skip setup",
       fun plain_config_is_cached/0},
      {"a config with an expandable value is not cached; reads stay live",
       fun expandable_config_is_not_cached/0},
      {"caching an expandable config drops the cache left by a plain one",
       fun expandable_config_drops_stale_cache/0},
      {"a value setup expands later is not frozen by the cache",
       fun expandable_value_follows_setup/0}]}.

%% The cache serves the stored config only when setup's value expansion cannot
%% change it. Assert both halves of that: the example configs have nothing to
%% expand, and for them expansion really is a no-op.
expansion_is_identity() ->
    lists:foreach(
      fun(File) ->
              {ok, {UserMap, _UserConfig}} = aeu_env:check_config(File),
              ?assertEqual(false, aeu_env:is_expandable(UserMap)),
              ok = aeu_env:cache_config(UserMap),
              ?assertEqual(setup:get_env(aeutils, '$user_map', #{}),
                           application:get_env(aeutils, '$user_map', #{})),
              ?assertEqual(setup:get_env(aeutils, '$user_config', []),
                           application:get_env(aeutils, '$user_config', [])),
              %% And the readers really do serve it. Without this the checks
              %% above would hold with the cache never consulted at all: they
              %% compare setup against the app env, not against user_map/0.
              ?assertEqual(setup:get_env(aeutils, '$user_map', #{}),
                           aeu_env:user_map()),
              ?assertEqual(setup:get_env(aeutils, '$user_config', []),
                           aeu_env:user_config())
      end, test_data_config_files()).

is_expandable_covers_nested_terms() ->
    ?assertEqual(false, aeu_env:is_expandable(#{<<"a">> => [1, 2, 3],
                                                <<"b">> => <<"plain">>})),
    ?assertEqual(false, aeu_env:is_expandable([104, 105])),
    ?assertEqual(true, aeu_env:is_expandable(#{<<"a">> => <<"$HOME">>})),
    %% Keys are expanded too, not just values.
    ?assertEqual(true, aeu_env:is_expandable(#{<<"$HOME">> => <<"v">>})),
    ?assertEqual(true, aeu_env:is_expandable([#{<<"a">> => [<<"x">>, <<"$Y">>]}])),
    %% setup's special forms carry the '$' in a string, not a binary.
    ?assertEqual(true, aeu_env:is_expandable({'$value', "$HOME"})),
    ?assertEqual(true, aeu_env:is_expandable("a$b")).

plain_config_is_cached() ->
    Map = #{<<"mempool">> => #{<<"tx_ttl">> => 123}},
    ok = aeu_env:cache_config(Map),
    %% Served from the cache now: reaching for setup must not happen.
    ok = meck:new(setup, [passthrough]),
    try
        ok = meck:expect(setup, get_env,
                         fun(_, _) -> error(unexpected_expansion) end),
        ok = meck:expect(setup, get_env,
                         fun(_, _, _) -> error(unexpected_expansion) end),
        ?assertEqual(Map, aeu_env:user_map()),
        ?assertEqual({ok, 123}, aeu_env:user_map([<<"mempool">>, <<"tx_ttl">>])),
        ?assertEqual({ok, 123}, aeu_env:user_config([<<"mempool">>, <<"tx_ttl">>])),
        %% The binary key clause reads the tree form rather than the map.
        ?assertEqual({ok, [{<<"tx_ttl">>, 123}]}, aeu_env:user_config(<<"mempool">>))
    after
        ok = meck:unload(setup)
    end.

expandable_config_is_not_cached() ->
    Map = #{<<"system">> => #{<<"plugin_path">> => <<"$DATA_DIR/plugins">>}},
    ?assertEqual(true, aeu_env:is_expandable(Map)),
    ok = aeu_env:cache_config(Map),
    ?assertEqual(undefined, cached_user_map()),
    %% Falling back is load bearing here, not merely conservative: for this
    %% config setup really does rewrite the value, so serving the stored term
    %% would return something other than what the old code path returned.
    ?assertNotEqual(application:get_env(aeutils, '$user_map', #{}),
                    setup:get_env(aeutils, '$user_map', #{})),
    %% Uncached, so the read must be exactly what the old code path returned.
    ?assertEqual(setup:get_env(aeutils, '$user_map', #{}), aeu_env:user_map()).

expandable_config_drops_stale_cache() ->
    Plain = #{<<"mempool">> => #{<<"tx_ttl">> => 1}},
    ok = aeu_env:cache_config(Plain),
    ?assertEqual(Plain, cached_user_map()),
    Expandable = Plain#{<<"system">> => #{<<"plugin_path">> => <<"$DATA_DIR/p">>}},
    ok = aeu_env:cache_config(Expandable),
    ?assertEqual(undefined, cached_user_map()),
    ?assertEqual(setup:get_env(aeutils, '$user_map', #{}), aeu_env:user_map()).

%% Why cache_expanded_config/2 refuses to cache an expandable config rather
%% than expanding it once and caching the result. aeu_logging_env rewrites
%% `setup > log_dir' at setup hook 102, after read_config/0 has already stored
%% the config at hook 100, so $LOG_DIR resolves to one directory before that
%% hook and another after it. A read has to follow setup, not a value frozen at
%% cache time - which is exactly what a config with a '$' in it gives up.
expandable_value_follows_setup() ->
    Saved = application:get_env(setup, log_dir),
    try
        ok = application:set_env(setup, log_dir, "/tmp/log-before"),
        ok = aeu_env:cache_config(#{<<"a">> => <<"$LOG_DIR/x">>}),
        ?assertEqual(undefined, cached_user_map()),
        ?assertEqual({ok, <<"/tmp/log-before/x">>}, aeu_env:user_map([<<"a">>])),
        %% The later hook moves it. The next read must see the new directory.
        ok = application:set_env(setup, log_dir, "/tmp/log-after"),
        ?assertEqual({ok, <<"/tmp/log-after/x">>}, aeu_env:user_map([<<"a">>]))
    after
        case Saved of
            {ok, Dir} -> application:set_env(setup, log_dir, Dir);
            undefined -> application:unset_env(setup, log_dir)
        end
    end.

cached_user_map() ->
    persistent_term:get({aeu_env, user_map}, undefined).

positive_extra_checks_tests() ->
    [{"Example user configuration file passes checks further to the schema: " ++ Config, %% For enabling files to be linked from wiki as examples.
      fun() ->
              {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
              ok = mock_user_config(UserMap, UserConfig),
              ?assertEqual(ok, aec_hard_forks:ensure_env()),
              ?assertEqual(ok, aec_mining:check_env())
      end
     } || Config <- test_data_config_files()].

deprecated_miner_section_conflicting_with_edge_bits() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_deprecated_miner_with_edge_bits.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertExit(cuckoo_config_validation_failed, aec_mining:check_env()).

deprecated_miner_section_conflicting_with_miners() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_deprecated_miner_with_miners.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertExit(cuckoo_config_validation_failed, aec_mining:check_env()).

invalid_fork_signalling_interval() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_interval.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError({illegal_fork_signalling_interval, _, _}, aec_hard_forks:ensure_env()).

invalid_fork_signalling_start_height() ->
    case aec_governance:get_network_id() of
        <<"local_roma_testnet">> ->
            %% Roma started with height 0, so signalling height cannot be lower.
            ok;
        _Other ->
            {Dir, DataDir} = get_test_config_base(),
            Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_start_height.yaml"]),
            {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
            ok = mock_user_config(UserMap, UserConfig),
            ?assertError({illegal_fork_signalling_interval, _, _}, aec_hard_forks:ensure_env())
    end.

invalid_fork_signalling_block_count() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_block_count.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError({illegal_fork_signalling_block_count, _}, aec_hard_forks:ensure_env()).

invalid_fork_signalling_version() ->
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_version.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError({illegal_fork_version, _}, aec_hard_forks:ensure_env()).

invalid_fork_signalling_version2() ->
    case aec_governance:get_network_id() of
        <<"local_lima_testnet">> ->
            {Dir, DataDir} = get_test_config_base(),
            Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_version2.yaml"]),
            {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
            ok = mock_user_config(UserMap, UserConfig),
            ?assertError({illegal_fork_version, _}, aec_hard_forks:ensure_env());
        _Other ->
            ok
    end.

invalid_fork_signalling_network_config(NetworkId) ->
    ok = meck:expect(aec_governance, get_network_id, fun() -> NetworkId end),
    {Dir, DataDir} = get_test_config_base(),
    Config = filename:join([Dir, DataDir, "epoch_invalid_fork_signalling_network_config.yaml"]),
    {ok, {UserMap, UserConfig}} = aeu_env:check_config(Config),
    ok = mock_user_config(UserMap, UserConfig),
    ?assertError(illegal_fork_signalling_config, aec_hard_forks:ensure_env()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_data_config_files() ->
    {Dir, DataDir} = get_test_config_base(),
    [filename:join([Dir, DataDir, "epoch_full.yaml"]),
     filename:join([Dir, DataDir, "epoch_no_peers.yaml"]),
     filename:join([Dir, DataDir, "epoch_no_newline.yaml"]),
     filename:join([Dir, DataDir, "epoch_prebuilt_miner.yaml"]),
     filename:join([Dir, DataDir, "epoch_deprecated_miner.yaml"]),
     filename:join([Dir, DataDir, "epoch_deprecated_debug_api.yaml"]),
     filename:join([Dir, DataDir, "epoch_monitoring.yaml"]),
     filename:join([Dir, DataDir, "epoch_nice.yaml"])].

get_test_config_base() ->
    %% differentiate between Eunit run in top directory and
    %% common test run in _build/test/logs/...
    %% This should be rebar3 thingy, if only one would know how.
    DataDir = "aeutils/test/data/",
    Dir =
        case filelib:is_dir(filename:join("apps", DataDir)) of
            true -> "apps/";
            false -> "../../lib/"
        end,
    {Dir, DataDir}.

setup() ->
    %% A config installed by an earlier test in this VM must not feed into the
    %% checks below.
    ok = forget_user_config(),
    application:ensure_all_started(jesse),
    application:ensure_all_started(yamerl),
    application:ensure_all_started(jsx),
    %% Several of the checks below run aec_hard_forks:ensure_env/0, which
    %% rewrites `aecore > fork` from whatever config they installed - and
    %% unsets it for a config that does not configure fork signalling, which
    %% every config here is. That key decides which protocol a block falls
    %% under, for every block, so hand it to teardown/1 to put back.
    application:get_env(aecore, fork).

teardown(SavedFork) ->
    %% The checks above store a config and a community fork; they must not leak
    %% into other tests sharing this VM.
    case SavedFork of
        {ok, Fork} -> application:set_env(aecore, fork, Fork);
        undefined  -> application:unset_env(aecore, fork)
    end,
    ok = forget_user_config(),
    application:stop(rfc3339),
    application:stop(jesse),
    application:stop(yamerl),
    application:stop(jsx).

mock_user_config(UserMap, UserConfig) ->
    %% This installs a config without going through aeu_env:cache_config/1, so
    %% a config left by an earlier read_config/0 in this VM would shadow it.
    ok = forget_user_config(),
    %% setup:get_env/2 answers {ok, V} | undefined, setup:get_env/3 answers the
    %% bare value or the default. Both are mocked from one clause set, so that
    %% user_config/0 and user_map/0 - which read the arity 3 form - see the
    %% value itself rather than an {ok, _} wrapper around it.
    F = fun
            (aeutils, '$user_config') -> {ok, UserConfig};
            (aeutils, '$user_map')    -> {ok, UserMap};
            (A, K)                    -> meck:passthrough([A, K])
        end,
    ok = meck:expect(setup, get_env, F),
    ok = meck:expect(setup, get_env,
                     fun(A, K, Default) ->
                             case F(A, K) of
                                 {ok, V}   -> V;
                                 undefined -> Default
                             end
                     end),
    ok.

%% aeu_env:cache_config/1 writes the config to the aeutils app env as well as
%% to the cache, and invalidate_config_cache/0 drops only the cache. Both have
%% to go: a config left in the app env is served to every later test module
%% sharing this VM, and aec_tx_pool reads `mempool > tx_ttl` from it.
forget_user_config() ->
    ok = aeu_env:invalidate_config_cache(),
    ok = application:unset_env(aeutils, '$user_map'),
    ok = application:unset_env(aeutils, '$user_config').
%%%=============================================================================
%%% AE__ environment variables must not be able to kill the node at boot
%%%=============================================================================

%% aeu_env:coerce_type/3 has no catch-all: a schema type it does not know is a
%% case_clause that kills application_controller at boot. Pinned rather than
%% asserted empty because three keys predating this test are already bad.
-define(NOT_COERCIBLE_BY_OS_ENV,
        [ {<<"sync:peer_pool:select_verified_peer_probability">>, <<"number">>}
        , {<<"stratum:session:share_target_diff_threshold">>,     <<"number">>}
        , {<<"stratum:session:edge_bits">>,                       <<"number">>}
        ]).

coercible_schema_types_test_() ->
    {setup, fun setup/0, fun(SavedFork) -> teardown(SavedFork) end,
     [{"no schema type is added that aeu_env:coerce_type/3 cannot handle",
       fun() ->
           %% keep in step with coerce_type/3 in aeu_env.erl
           Known = [<<"integer">>, <<"string">>, <<"boolean">>,
                    <<"array">>, <<"object">>],
           Bad = [ PT || {_P, T} = PT <- schema_leaf_types(),
                         not lists:member(T, Known) ],
           ?assertEqual(lists:sort(?NOT_COERCIBLE_BY_OS_ENV), lists:sort(Bad))
       end},
      {"the relay gas price keys in particular are coercible, so AE__ cannot kill boot",
       fun() ->
           Types = schema_leaf_types(),
           [ ?assertEqual({Key, {ok, <<"integer">>}},
                          {Key,
                           case lists:keyfind(Key, 1, Types) of
                               {_, T} -> {ok, T};
                               false  -> not_in_schema
                           end})
             || Key <- [<<"http:gas_price:min_relay_gas_price">>,
                        <<"http:gas_price:reporting_utilization_override">>] ]
       end}]}.

%% Walk the schema and yield {ColonPath, Type} for every property carrying a
%% "type", so a bad one is reported by name rather than as a bare count.
schema_leaf_types() ->
    #{<<"properties">> := Props} = aeu_env:schema(),
    schema_leaf_types(Props, []).

schema_leaf_types(Props, Path) when is_map(Props) ->
    maps:fold(
      fun(Name, #{} = Sub, Acc) ->
              Path1 = Path ++ [Name],
              Acc1 = case maps:find(<<"type">>, Sub) of
                         {ok, T} ->
                             [{iolist_to_binary(lists:join(<<":">>, Path1)), T} | Acc];
                         error ->
                             Acc
                     end,
              case maps:find(<<"properties">>, Sub) of
                  {ok, SubProps} -> schema_leaf_types(SubProps, Path1) ++ Acc1;
                  error          -> Acc1
              end;
         (_Name, _NotAMap, Acc) -> Acc
      end, [], Props).

%% Asserted against the real schema because the bound moved: `minimum: 1' with
%% type ["integer","null"] rejected 0; `minimum: 0' with a plain integer accepts
%% it as the off value. Opposite verdicts on the same input.
min_relay_gas_price_schema_bound_test_() ->
    {setup, fun setup/0, fun(SavedFork) -> teardown(SavedFork) end,
     [{"the schema accepts the off value, the working multiples, and nothing else",
       fun() ->
           Accept = [ {"absent - the default off state", absent}
                    , {"0 - the explicit off value",     0}
                    , {"1 - the smallest live floor",    1}
                    , {"500000000000 (x500)",            500000000000}
                    , {"1000000000000 (x1000)",          1000000000000}
                    ],
           Reject = [ {"-1 - negative",                  -1}
                    , {"1.5 - not an integer",           1.5}
                    , {"\"500\" - a string",             <<"500">>}
                    , {"true - a boolean",               true}
                      %% null was valid under the old union type, which is what
                      %% made an AE__ variable naming this key kill the node at
                      %% boot, so its rejection here is load-bearing
                    , {"null - the removed union member", null}
                    ],
           Cases = [ {N, V, accept} || {N, V} <- Accept ]
                ++ [ {N, V, reject} || {N, V} <- Reject ],
           Bad = lists:filtermap(
                   fun({Name, V, Want}) ->
                       case validate_min_relay(V) of
                           Want -> false;
                           Got  -> {true, {Name, {expected, Want}, {got, Got}}}
                       end
                   end, Cases),
           ?assertEqual([], Bad)
       end}]}.

%% 0..100 is what GasPrices.utilization declares in apps/aehttp/priv/oas3.yaml;
%% a wider schema would let an operator configure the node into violating it.
reporting_utilization_override_schema_bound_test_() ->
    {setup, fun setup/0, fun(SavedFork) -> teardown(SavedFork) end,
     [{"the schema accepts 0..100 and refuses everything outside it",
       fun() ->
           Accept = [ {"absent - takes the documented default of 0",  absent}
                    , {"0 - the default, report utilization as observed", 0}
                    , {"70 - inside the range",                       70}
                    , {"100 - the top of the published range",        100}
                    ],
           Reject = [ {"-1 - below the range",                         -1}
                    , {"101 - one past the OpenAPI maximum",           101}
                    , {"1000 - well past it",                          1000}
                    , {"70.5 - not an integer",                        70.5}
                    , {"\"71\" - a string",                            <<"71">>}
                    , {"true - a boolean",                             true}
                    , {"null - not a union member here either",        null}
                    ],
           Cases = [ {N, V, accept} || {N, V} <- Accept ]
                ++ [ {N, V, reject} || {N, V} <- Reject ],
           Bad = lists:filtermap(
                   fun({Name, V, Want}) ->
                       case validate_reporting_utilization_override(V) of
                           Want -> false;
                           Got  -> {true, {Name, {expected, Want}, {got, Got}}}
                       end
                   end, Cases),
           ?assertEqual([], Bad)
       end}]}.

validate_min_relay(V) ->
    validate_gas_price_key(<<"min_relay_gas_price">>, V).

validate_reporting_utilization_override(V) ->
    validate_gas_price_key(<<"reporting_utilization_override">>, V).

validate_gas_price_key(Key, V) ->
    GasPrice = case V of
                   absent -> #{};
                   _      -> #{Key => V}
               end,
    Cfg = #{<<"http">> => #{<<"gas_price">> => GasPrice}},
    case catch jesse:validate_with_schema(aeu_env:schema(), Cfg, []) of
        {ok, _}    -> accept;
        {error, _} -> reject;
        Other      -> {crash, Other}
    end.

%% The concrete regression: drive the real boot-time call with the variable set,
%% and assert it both survives and lands the coerced integer in the config map.
min_relay_gas_price_settable_by_os_env_test_() ->
    Var = "AE__HTTP__GAS_PRICE__MIN_RELAY_GAS_PRICE",
    {setup,
     % setup/0's return value is the saved `aecore > fork' env and teardown/1
     % switches on it - returning a bare ok here aborts cleanup.
     fun() -> SavedFork = setup(), os:putenv(Var, "500000000000"), SavedFork end,
     fun(SavedFork) -> os:unsetenv(Var), teardown(SavedFork) end,
     {"setting http:gas_price:min_relay_gas_price via AE__ does not crash config load",
      fun() ->
          Res = aeu_env:apply_os_env("AE", aeu_env:schema(), #{}),
          ?assertMatch(#{<<"http">> := #{<<"gas_price">> := #{}}}, Res),
          #{<<"http">> := #{<<"gas_price">> := GasPrice}} = Res,
          %% coerced to an integer, not left as the "500000000000" string
          ?assertEqual(500000000000,
                       maps:get(<<"min_relay_gas_price">>, GasPrice))
      end}}.

%% Same regression for the utilization half: a container deployment sets both
%% keys the same way.
reporting_utilization_override_settable_by_os_env_test_() ->
    Var = "AE__HTTP__GAS_PRICE__REPORTING_UTILIZATION_OVERRIDE",
    {setup,
     % setup/0's return value is the saved `aecore > fork' env and teardown/1
     % switches on it - returning a bare ok here aborts cleanup.
     fun() -> SavedFork = setup(), os:putenv(Var, "71"), SavedFork end,
     fun(SavedFork) -> os:unsetenv(Var), teardown(SavedFork) end,
     {"setting http:gas_price:reporting_utilization_override via AE__ does "
      "not crash config load",
      fun() ->
          Res = aeu_env:apply_os_env("AE", aeu_env:schema(), #{}),
          ?assertMatch(#{<<"http">> := #{<<"gas_price">> := #{}}}, Res),
          #{<<"http">> := #{<<"gas_price">> := GasPrice}} = Res,
          ?assertEqual(71, maps:get(<<"reporting_utilization_override">>, GasPrice))
      end}}.

%%%=============================================================================
%%% Config errors reported by the hooks that run before lager
%%%=============================================================================

-define(CAPTURE, ?MODULE).

%% error_format/3 must not lose the message when hooks 100-101 run before lager.
config_error_reported_before_lager_starts_test_() ->
    {setup,
     fun() ->
             SavedFork = setup(),
             LagerUp = lists:keymember(lager, 1, application:which_applications()),
             _ = application:stop(lager),
             %% A table, not messages: eunit runs each test in a process of
             %% its own, which the handler has no way of knowing about.
             ?CAPTURE = ets:new(?CAPTURE, [named_table, public, duplicate_bag]),
             ok = logger:add_handler(?CAPTURE, ?MODULE, #{level => error}),
             {SavedFork, LagerUp}
     end,
     fun({SavedFork, LagerUp}) ->
             ok = logger:remove_handler(?CAPTURE),
             true = ets:delete(?CAPTURE),
             case LagerUp of true -> ok = lager:start(); false -> ok end,
             teardown(SavedFork)
     end,
     {"a validation failure names the offending setting with lager down",
      fun() ->
              Bad = #{<<"mining">> =>
                          #{<<"expected_mine_rate">> => <<"not-an-integer">>}},
              ?assertError(validation_failed,
                           aeu_env:validate(Bad, config_error_test,
                                            aeu_env:schema_filename(), report)),
              [Reported] = captured(),
              ?assertNotEqual(nomatch, string:find(Reported, "expected_mine_rate")),
              ?assertNotEqual(nomatch, string:find(Reported, "not-an-integer")),
              ?assertNotEqual(nomatch, string:find(Reported, "Wrong type"))
      end}}.

log(Event, _Config) ->
    ets:insert(?CAPTURE, {msg, logger_formatter:format(Event, #{template => [msg]})}),
    ok.

captured() ->
    [lists:flatten(Msg) || {msg, Msg} <- ets:tab2list(?CAPTURE)].

-endif.
