-module(aehc_test_utils).

-export([ hc_from_genesis/0
        , hc_consensus/0
        , pow_to_hc_switch/1
        , cuckoo_pow_consensus/0
        , cuckoo_pow_from_genesis/0
        , enable_hc_from_genesis/0
        , enable_pow_cuckoo_from_genesis/0
        , enable_consensus/1
        , with_mocked_fork_settings/1
        , hc_chain_eunit_testcase/2
        , patron/0
        , genesis_accounts/0
        , gen_blocks_only_chain/1
        , gen_blocks_only_chain/2
        , gen_block_chain_with_state/2
        , gen_block_chain_with_state/3
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
    enable_consensus(hc_from_genesis()).

enable_pow_cuckoo_from_genesis() ->
    enable_consensus(cuckoo_pow_from_genesis()).

enable_consensus(Consensus) ->
    application:set_env(aecore, consensus, Consensus),
    aec_consensus:set_consensus(),
    ?assertEqual(ok, aec_consensus:check_env()),
    ok.

genesis_accounts() ->
    #{pubkey := PubKey} = patron(),
    [{PubKey, 10000000000000000000 * aec_test_utils:min_gas_price()}].

patron() ->
    #{pubkey  => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,
                   73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
      privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,
                   197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,
                   167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,
                   187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>}.

with_mocked_fork_settings(What) ->
    { foreach,
      fun() ->
          ?assertEqual(false, aehc_utils:hc_enabled()),
          aec_test_utils:mock_genesis_and_forks(genesis_accounts()),
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
                 ok = lager:start(),
                 aefa_fate_op:load_pre_iris_map_ordering(),
                 ok = application:ensure_started(gproc),
                 {ok, _} = aec_db_error_store:start_link(),
                 ok = aec_test_utils:start_chain_db(),
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
                 ok = aec_conductor:stop(),
                 ok = aec_tx_pool:stop(),
                 ok = application:unset_env(aecore, beneficiary),
                 aec_test_utils:aec_keys_cleanup(TmpDir),
                 meck:unload(aec_mining),
                 meck:unload(aec_events),
                 ok = aec_db_error_store:stop(),
                 ok = aec_test_utils:stop_chain_db(),
                 ok = application:stop(gproc)
         end,
         What}
        ])
    ]).

gen_blocks_only_chain(Count) ->
    aec_test_utils:gen_blocks_only_chain(Count, genesis_accounts()).

gen_blocks_only_chain(Count, BlockCfg) ->
    aec_test_utils:gen_blocks_only_chain(Count, genesis_accounts(), BlockCfg).

gen_block_chain_with_state(Targets, TxsFun) -> gen_block_chain_with_state(Targets, TxsFun, lists:duplicate(length(Targets), undefined)).

gen_block_chain_with_state(Targets, TxsFun, Timestamps) ->
    {B0, S0} = aec_block_genesis:genesis_block_with_state(#{preset_accounts => genesis_accounts()}),
    aec_test_utils:extend_block_chain_with_state([{B0, S0}],
        #{ targets => Targets
         , txs_by_height_fun => TxsFun
         , timestamps => Timestamps
         }).
