-module(aehc_test_utils).

-export([ hc_from_genesis/0
        , hc_consensus/0
        , pow_to_hc_switch/1
        , cuckoo_pow_consensus/0
        , cuckoo_pow_from_genesis/0
        , enable_hc_from_genesis/0
        , enable_pow_cuckoo_from_genesis/0
        , with_mocked_fork_settings/1
        , hc_chain_eunit_testcase/2
        ]).

-include_lib("eunit/include/eunit.hrl").

hc_consensus() -> #{<<"name">> => <<"hyperchains">>}.
cuckoo_pow_consensus() -> #{<<"name">> => <<"pow_cuckoo">>}.

hc_from_genesis() -> #{<<"0">> => hc_consensus()}.
cuckoo_pow_from_genesis() -> #{<<"0">> => cuckoo_pow_consensus()}.
pow_to_hc_switch(Height) ->
    #{<<"0">> => cuckoo_pow_consensus()
     , integer_to_binary(Height) => hc_consensus()
     }.

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

hc_chain_eunit_testcase(Consensus, What) ->
    aehc_test_utils:with_mocked_fork_settings([
    aec_test_utils:eunit_with_consensus(Consensus, [
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             {ok, _} = aec_db_error_store:start_link(),
             aec_test_utils:start_chain_db(),
             %% somehow setup:find_env_vars can't find this hook in eunit tests
             aehc_db:create_tables(ram),
             Tabs = [Tab || {Tab, _} <- aehc_parent_db:table_specs(ram)],
             ok = mnesia:wait_for_tables(Tabs, 10000),

             meck:new(aec_mining, [passthrough]),
             meck:expect(aec_mining, verify, fun(_, _, _, _) -> true end),
             meck:new(aec_events, [passthrough]),
             meck:expect(aec_events, publish, fun(_, _) -> ok end),
             TmpDir = aec_test_utils:aec_keys_setup(),
             {ok, PubKey} = aec_keys:pubkey(),
             ok = application:set_env(aecore, beneficiary, aeser_api_encoder:encode(account_pubkey, PubKey)),
             {ok, _} = aec_tx_pool:start_link(),
             aec_consensus:set_genesis_hash(),
             {ok, _} = aec_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_conductor:stop(),
             ok = aec_tx_pool:stop(),
             ok = application:stop(gproc),
             meck:unload(aec_mining),
             meck:unload(aec_events),
             aec_test_utils:stop_chain_db(),
             ok = aec_db_error_store:stop(),
             aec_test_utils:aec_keys_cleanup(TmpDir)
     end, What}])]).
