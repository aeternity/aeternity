%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the community fork configuration read by aec_hard_forks
%%% @end
%%%=============================================================================
-module(aec_hard_forks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Shaped as ensure_env/0 writes it: the atom keys conv_fork_config_key/1
%% produces, with the integer values the schema constrains them to.
-define(FORK_CFG,
        #{signalling_start_height => 100,
          signalling_end_height   => 200,
          signalling_block_count  => 50,
          info_field              => 1234,
          version                 => 6}).

fork_config_test_() ->
    {foreach,
     fun() -> application:get_env(aecore, fork) end,
     fun({ok, Fork})  -> application:set_env(aecore, fork, Fork);
        (undefined)   -> application:unset_env(aecore, fork)
     end,
     [{"setup's expansion is the identity for `aecore > fork`, set and unset",
       fun fork_expansion_is_identity/0}]}.

%% protocol_effective_at_height/1 reads `aecore > fork` with
%% application:get_env/2 rather than aeu_env:get_env/2 - it runs for every
%% block, and the expansion rebuilds the whole term on every call. That is only
%% sound while the expansion cannot change the term, so assert exactly that,
%% against the real setup, in both the unset and the configured case.
fork_expansion_is_identity() ->
    ok = application:unset_env(aecore, fork),
    ?assertEqual(undefined, application:get_env(aecore, fork)),
    ?assertEqual(aeu_env:get_env(aecore, fork),
                 application:get_env(aecore, fork)),
    ok = application:set_env(aecore, fork, ?FORK_CFG),
    ?assertEqual({ok, ?FORK_CFG}, application:get_env(aecore, fork)),
    ?assertEqual(aeu_env:get_env(aecore, fork),
                 application:get_env(aecore, fork)).

-endif.
