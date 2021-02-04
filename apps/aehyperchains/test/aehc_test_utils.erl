-module(aehc_test_utils).

-export([ hc_from_genesis/0
        , hc_consensus/0
        , cuckoo_pow_consensus/0
        , cuckoo_pow_from_genesis/0
        , enable_hc_from_genesis/0
        , enable_pow_cuckoo_from_genesis/0
        , with_mocked_fork_settings/1
        ]).

-include_lib("eunit/include/eunit.hrl").

hc_consensus() -> #{<<"name">> => <<"hyperchains">>}.
cuckoo_pow_consensus() -> #{<<"name">> => <<"pow_cuckoo">>}.

hc_from_genesis() -> #{<<"0">> => hc_consensus()}.
cuckoo_pow_from_genesis() -> #{<<"0">> => cuckoo_pow_consensus()}.

enable_hc_from_genesis() ->
    application:set_env(aecore, consensus, hc_from_genesis()),
    aec_consensus:set_consensus(),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

enable_pow_cuckoo_from_genesis() ->
    application:set_env(aecore, consensus, cuckoo_pow_from_genesis()),
    aec_consensus:set_consensus(),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

with_mocked_fork_settings(What) ->
    { foreach,
      fun() ->
          ?assertEqual(false, aehc_utils:hc_enabled()),
          aec_test_utils:mock_genesis_and_forks(),
          ok
      end,
      fun(_) ->
          aec_test_utils:unmock_genesis_and_forks(),
          ?assertEqual(false, aehc_utils:hc_enabled())
      end, What
    }.
