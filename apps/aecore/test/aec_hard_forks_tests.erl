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

stale_fork_test_() ->
    {foreach,
     fun() ->
             Saved = application:get_env(aecore, fork),
             SavedCfg = take_user_config(),
             ok = meck:new(aec_governance, [passthrough]),
             {Saved, SavedCfg}
     end,
     fun({Saved, SavedCfg}) ->
             %% Restore before anything that can fail: `aecore > fork` decides
             %% which protocol a block falls under, for every block, and must
             %% not leak into other test modules sharing this VM.
             case Saved of
                 {ok, Fork} -> application:set_env(aecore, fork, Fork);
                 undefined  -> application:unset_env(aecore, fork)
             end,
             restore_user_config(SavedCfg),
             ok = meck:unload(aec_governance)
     end,
     [{"ensure_env/0 drops a community fork left by an earlier run",
       fun ensure_env_drops_stale_fork/0}]}.

%% ae_mainnet configures no fork signalling - mainnet_fork_config/0 returns
%% undefined - so re-running the hook must clear a fork left behind by an
%% earlier run, rather than leave it for protocol_effective_at_height/1 to keep
%% reading on every block.
ensure_env_drops_stale_fork() ->
    ok = meck:expect(aec_governance, get_network_id,
                     fun() -> <<"ae_mainnet">> end),
    ok = application:set_env(aecore, fork, ?FORK_CFG),
    ?assertEqual(ok, aec_hard_forks:ensure_env()),
    ?assertEqual(undefined, application:get_env(aecore, fork)).

%% ensure_env/0 reads `fork_management > fork' from the user config, and
%% assert_fork/3 rejects any fork configured for ae_mainnet, which the test
%% above mocks the network id to. A config left in the aeutils app env by
%% another test module in this VM - or by the node itself, under
%% aec_eunit_SUITE - would therefore raise illegal_fork_signalling_config
%% instead of exercising the stale fork. Take it out, and put back what
%% was there.
take_user_config() ->
    Saved = [{K, application:get_env(aeutils, K)}
             || K <- ['$user_map', '$user_config']],
    ok = aeu_env:invalidate_config_cache(),
    ok = application:unset_env(aeutils, '$user_map'),
    ok = application:unset_env(aeutils, '$user_config'),
    Saved.

restore_user_config(Saved) ->
    lists:foreach(
      fun({K, {ok, V}})   -> application:set_env(aeutils, K, V);
         ({K, undefined}) -> application:unset_env(aeutils, K)
      end, Saved),
    %% Restored to the app env only. aeu_env falls back to reading it through
    %% setup, so this is correct, just uncached - and cache_config/1 is the
    %% only thing entitled to decide whether this config may be cached.
    ok = aeu_env:invalidate_config_cache().

-endif.
