%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% Tests for deploying the hyperchains staking contract, using a predeployed staking contract, switching from PoW to HC
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_consensus_tests).

-include_lib("eunit/include/eunit.hrl").

%% PoW genesis - aec_conductor is not started
pow_from_genesis_test_() ->
    aec_test_utils:eunit_with_consensus(aehc_test_utils:cuckoo_pow_from_genesis(),
        [{foreach,
             fun() ->
                     aec_test_utils:mock_genesis_and_forks(),
                     aec_test_utils:start_chain_db(),
                     aec_consensus_bitcoin_ng:load_whitelist(),
                     aec_test_utils:aec_keys_setup()
             end,
             fun(TmpDir) ->
                     aec_test_utils:aec_keys_cleanup(TmpDir),
                     aec_test_utils:unmock_genesis_and_forks(),
                     aec_test_utils:stop_chain_db()
             end,
             [ {"HC specific methods don't work when we use only PoW",
                 fun() ->
                     ?assertEqual(undefined, aec_chain:top_block()),
                     F = fun() ->
                            ?assertException(error, badarg, aehc_consensus_hyperchains:get_staking_contract_aci()),
                            not_deployed = aehc_consensus_hyperchains:get_staking_contract_address()
                         end,
                     Chain = aec_test_utils:gen_blocks_only_chain(3),
                     F(),
                     {error, missing_top_block} = aehc_consensus_hyperchains:static_staking_contract_call_on_top_block("enabled()"),
                     [begin
                          {ok, _} = aec_chain_state:insert_block(B),
                          ?assertNotEqual(undefined, aec_chain:top_block()),
                          F(),
                          ?assertException(exit, {badarg, _}, aehc_consensus_hyperchains:static_staking_contract_call_on_top_block("enabled()"))
                      end || B <- Chain]
                 end}
             ]}
        ]).

%% HC genesis - aec_conductor is started
hc_from_genesis_test_() ->
    aehc_test_utils:hc_chain_eunit_testcase(aehc_test_utils:hc_from_genesis(),
        [ {"Can mine some blocks",
            fun() ->
                [GB | Blocks] = aec_test_utils:gen_blocks_only_chain(10),
                GB = aec_chain:top_block(),
                [begin
                     {ok, _} = aec_chain_state:insert_block(B),
                     B = aec_chain:top_block()
                 end || B <- Blocks],
                ok
            end}
        , {"Staking contract gets deployed at genesis and is harmeless in beginning",
            fun() ->
                error = aehc_consensus_hyperchains:get_predeploy_address(),
                aehc_consensus_hyperchains:get_staking_contract_aci(),
                {ok, ContractAddress} = aehc_consensus_hyperchains:get_staking_contract_address(),
                [GB | Blocks] = aec_test_utils:gen_blocks_only_chain(10),
                %% Without stake the HC consensus fallbacks to PoW
                false = aehc_consensus_hyperchains:is_hc_pos_header(aec_blocks:to_header(GB)), %% The genesis case is separate
                [false = aehc_consensus_hyperchains:is_hc_pos_header(aec_blocks:to_header(B)) || B <- Blocks],
                GB = aec_chain:top_block(),
                %% Sanity check the system deployed staking contract object
                ContractObj = sanity_check_staking_contract_deployment(ContractAddress),
                F = fun() ->
                        %% Sanity check contract calls
                        assert_static_staking_call_result({ok, false}, "enabled()"),
                        assert_static_staking_call_result({ok, 0}, "balance()"),
                        assert_static_staking_call_result({ok, {address, <<2:32/unit:8>>}}, "restricted_address()"),
                        %% Static calls never mutate the state of the contract
                        ensure_same_staking_contract(ContractAddress, ContractObj)
                    end,
                F(),
                [begin
                     {ok, _} = aec_chain_state:insert_block(B),
                     B = aec_chain:top_block(),
                     F()
                 end || B <- Blocks],
                ok
            end}
        ]).

hc_switchover_at_10_test_() ->
    aehc_test_utils:hc_chain_eunit_testcase(aehc_test_utils:pow_to_hc_switch(10),
        [ {"Can mine some blocks. Accepts PoW blocks even with the global contract address available",
            fun() ->
                not_deployed = aehc_consensus_hyperchains:get_staking_contract_address(),
                Chain = [GB | Blocks] = aec_test_utils:gen_blocks_only_chain(20),
                {ok, _} = aehc_consensus_hyperchains:get_staking_contract_address(),
                GB = aec_chain:top_block(),
                [false = aehc_consensus_hyperchains:is_hc_pos_header(aec_blocks:to_header(B)) || B <- Chain],
                [begin
                     {ok, _} = aec_chain_state:insert_block(B),
                     B = aec_chain:top_block()
                 end || B <- Blocks],
                ok
            end},
          {"When no predeployment was done then a system account will deploy the staking contract",
            fun() ->
                aehc_consensus_hyperchains:load_staking_contract_address(),
                error = aehc_consensus_hyperchains:get_predeploy_address(),
                not_deployed = aehc_consensus_hyperchains:get_staking_contract_address(),
                Chain = aec_test_utils:gen_blocks_only_chain(21), %% This will set the predeploy address
                %% Simulate syncing by erasing the persisted staking contract address
                aec_db:delete_hc_staking_contract_address(),
                aehc_consensus_hyperchains:load_staking_contract_address(),
                [_ | PowBlocks] = lists:sublist(Chain, 10),
                [TransientBlock | HCBlocks] = lists:sublist(Chain, 11, 10),
                [begin
                     {ok, _} = aec_chain_state:insert_block(B),
                     B = aec_chain:top_block(),
                     not_deployed = aehc_consensus_hyperchains:get_staking_contract_address(),
                     error = aehc_consensus_hyperchains:get_predeploy_address()
                 end || B <- PowBlocks],
                {ok, _} = aec_chain_state:insert_block(TransientBlock),
                TransientBlock = aec_chain:top_block(),
                {ok, ContractAddress} = aehc_consensus_hyperchains:get_staking_contract_address(),
                ContractObj = sanity_check_staking_contract_deployment(ContractAddress),
                [begin
                     {ok, _} = aec_chain_state:insert_block(B),
                     B = aec_chain:top_block(),
                     {ok, ContractAddress} = aehc_consensus_hyperchains:get_staking_contract_address(),
                     error = aehc_consensus_hyperchains:get_predeploy_address(),
                     ensure_same_staking_contract(ContractAddress, ContractObj)
                 end || B <- HCBlocks]
            end}
        ]).

sanity_check_staking_contract_deployment(ContractAddress) ->
    {_, Trees} = aec_chain:top_block_with_state(),
    {value, ContractObj} = aect_state_tree:lookup_contract(ContractAddress, aec_trees:contracts(Trees)),
    ?assertMatch(<<2:32/unit:8>>, aect_contracts:owner_pubkey(ContractObj)),
    {value, OwnerObj} = aec_accounts_trees:lookup(<<2:32/unit:8>>, aec_trees:accounts(Trees)),
    ?assertMatch(1, aec_accounts:nonce(OwnerObj)),
    ?assertMatch(0, aec_accounts:balance(OwnerObj)),
    assert_static_staking_call_result({ok, {address, <<2:32/unit:8>>}}, "restricted_address()"),
    ContractObj.

ensure_same_staking_contract(ContractAddress, ContractObj) ->
    {_, Trees} = aec_chain:top_block_with_state(),
    ?assertMatch({value, ContractObj}, aect_state_tree:lookup_contract(ContractAddress, aec_trees:contracts(Trees))).

assert_static_staking_call_result(M, Query) ->
    ?assertEqual(M, aehc_consensus_hyperchains:static_staking_contract_call_on_top_block(Query)),
    ?assertEqual(M, aehc_consensus_hyperchains:static_staking_contract_call_on_block_hash(aec_chain:top_block_hash(), Query)).
