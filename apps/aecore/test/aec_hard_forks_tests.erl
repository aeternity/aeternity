%%%============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_hard_forks.
%%% @end
%%%============================================================================

-module(aec_hard_forks_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_hard_forks).

-define(FORKS1, #{1 => 0}).
-define(RESULTS1, [{0, 1}, {1, 1}, {2, 1}, {1000, 1}, {9999, 1}]).

-define(FORKS2, #{1 => 0, 2 => 1, 3 => 2, 4 => 3}).
-define(RESULTS2, [{0, 1}, {1, 2}, {2, 3}, {3, 4}, {4, 4}, {5, 4}, {3132, 4}]).

-define(FORKS3, #{1 => 0, 3 => 100}).
-define(RESULTS3, [{0, 1}, {1, 1}, {2, 1}, {99, 1}, {100, 3}, {101, 3}]).

-define(FORK_HEIGHT, 650).
-define(NEW_VERSION, 10).

-define(FORK_SIGNALLING,
        #{signalling_start_height => 500,
          signalling_end_height   => 600,
          signalling_block_count  => 90,
          fork_height             => ?FORK_HEIGHT,
          info_field              => 1234,
          version                 => ?NEW_VERSION}).

-define(FORK_SIGNALLING_RESULTS1,
        [
%%         {0, 1, undefined}, {1, 1, undefined},
%%         {?FORK_HEIGHT - 1, 1, undefined},
         {?FORK_HEIGHT, 1, false},
         {?FORK_HEIGHT, ?NEW_VERSION, true}
%%         {1000, 1, false}
%%         {1000, ?NEW_VERSION, true}
        ]).

protocol_effective_at_height_test_() ->
    [protocol_effective_at_height(no_fork_signalling_config, ?FORKS1, ?RESULTS1),
     protocol_effective_at_height(no_fork_signalling_config, ?FORKS2, ?RESULTS2),
     protocol_effective_at_height(no_fork_signalling_config, ?FORKS3, ?RESULTS3)].

protocol_effective_at_height_with_signalling_config_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_fork_signalling, [passthrough]),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_key_block_by_height, fun(_) -> {ok, aec_blocks:raw_key_block()} end)
     end,
     fun(_) ->
             meck:unload(aec_chain),
             meck:unload(aec_fork_signalling)
     end,
     [fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {0, 1, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {1, 1, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {?FORK_HEIGHT - 1, 1, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {?FORK_HEIGHT, 1, false})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {?FORK_HEIGHT, ?NEW_VERSION, true})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {?FORK_HEIGHT + 1, 1, false})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS1, ?FORK_SIGNALLING, {?FORK_HEIGHT + 1, ?NEW_VERSION, true})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {0, 1, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {1, 2, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {2, 3, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {3, 4, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {?FORK_HEIGHT - 1, 4, undefined})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {?FORK_HEIGHT, 4, false})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {?FORK_HEIGHT, ?NEW_VERSION, true})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {?FORK_HEIGHT + 100, 4, false})
      end,
      fun(_) ->
              protocol_effective_at_height(
                with_fork_signalling_config, ?FORKS2, ?FORK_SIGNALLING, {?FORK_HEIGHT + 100, ?NEW_VERSION, true})
      end]}.

protocol_effective_at_height(no_fork_signalling_config, Forks, Results) ->
    [?_assertEqual({ok, V}, ?TEST_MODULE:protocol_effective_at_height(H, Forks))
     || {H, V} <- Results].

protocol_effective_at_height(with_fork_signalling_config, Forks, ForkSignallingCfg, {H, V, R}) ->
    application:set_env(aecore, fork, ForkSignallingCfg),
    case R of
        X when is_boolean(X) ->
            meck:expect(aec_fork_signalling, get_fork_result, fun(_, _, _) -> {ok, X} end);
        X when X =/= undefined ->
            meck:expect(aec_fork_signalling, get_fork_result, fun(_, _, _) -> {error, X} end);
        undefined ->
            ok
    end,
    case V of
        Y when is_integer(Y) ->
            [?_assertEqual({ok, Y}, ?TEST_MODULE:protocol_effective_at_height(H, Forks))];
        Y when is_atom(Y) ->
            [?_assertEqual({error, Y}, ?TEST_MODULE:protocol_effective_at_height(H, Forks))]
    end.
