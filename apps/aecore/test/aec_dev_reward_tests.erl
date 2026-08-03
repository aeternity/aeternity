%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the boot derived reward configuration in aec_dev_reward
%%% @end
%%%=============================================================================
-module(aec_dev_reward_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Read by split/3 on the per key block reward path. ensure_env/0 writes all of
%% them but dev_reward_activated, which only test setup ever writes - see
%% expansion_is_identity/1.
-define(KEYS, [dev_reward_enabled,
               dev_reward_activated,
               dev_reward_allocated_shares,
               dev_reward_beneficiaries]).

reward_config_test_() ->
    {foreach,
     fun() ->
             Saved = [{K, application:get_env(aecore, K)} || K <- ?KEYS],
             SavedCfg = take_user_config(),
             ok = meck:new(aec_governance, [passthrough]),
             {Saved, SavedCfg}
     end,
     fun({Saved, SavedCfg}) ->
             %% Restore before anything that can fail: the reward config of a
             %% mocked network must not leak into other test modules sharing
             %% this VM, and grant_fees reads it for every key block.
             lists:foreach(
               fun({K, {ok, V}})   -> application:set_env(aecore, K, V);
                  ({K, undefined}) -> application:unset_env(aecore, K)
               end, Saved),
             restore_user_config(SavedCfg),
             ok = meck:unload(aec_governance)
     end,
     [{"setup's expansion is the identity for the reward config of " ++ Name,
       fun() -> expansion_is_identity(NetworkId) end}
      || {Name, NetworkId} <- [{"ae_mainnet", <<"ae_mainnet">>},
                               {"ae_uat", <<"ae_uat">>}]]}.

%% split/3 reads these four keys with application:get_env/2,3 rather than
%% aeu_env:get_env/2,3. That is only sound while setup's value expansion cannot
%% change them, so assert exactly that - against the real setup, on the
%% configuration ensure_env/0 actually derives, not on a hand written term.
expansion_is_identity(NetworkId) ->
    ok = meck:expect(aec_governance, get_network_id, fun() -> NetworkId end),
    ok = aec_dev_reward:ensure_env(),
    %% ensure_env/0 does not write dev_reward_activated - only test setup does,
    %% as aec_test_utils:dev_reward_setup/3 - so set it here. Left unset, the
    %% assertion below would compare undefined to undefined for that key.
    ok = application:set_env(aecore, dev_reward_activated, true),
    %% Not vacuous: the keys really are populated, and the beneficiaries really
    %% do carry the raw public keys that the expansion would otherwise walk
    %% byte by byte on every read.
    ?assertEqual({ok, true}, application:get_env(aecore, dev_reward_enabled)),
    {ok, Beneficiaries} = application:get_env(aecore, dev_reward_beneficiaries),
    ?assert(lists:any(fun({PubKey, Share}) ->
                              is_binary(PubKey) andalso is_integer(Share)
                      end,
                      lists:append(maps:values(Beneficiaries)))),
    lists:foreach(
      fun(Key) ->
              ?assertEqual(aeu_env:get_env(aecore, Key),
                           application:get_env(aecore, Key))
      end, ?KEYS).

%% ensure_env/0 reads `chain > protocol_beneficiaries_enabled' and
%% `chain > protocol_beneficiaries' from the user config, so a config left in
%% the aeutils app env by another test module in this VM - or by the node
%% itself, under aec_eunit_SUITE - would decide what it writes, and
%% protocol_beneficiaries_enabled: false alone would sink the assertions above.
%% Take it out of the picture, and put back exactly what was there.
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
