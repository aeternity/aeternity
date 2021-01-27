%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Tests for aehc_db
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_db_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

-define(PARENT_GENESIS_HASH, <<"GENESIS">>).
-define(PARENT_HASH1, <<"Parent block 1">>).
-define(PARENT_HASH2, <<"Parent block 2">>).
-define(PARENT_GENESIS_HEADER,
    aehc_parent_block:new_header(?PARENT_GENESIS_HASH, ?PARENT_GENESIS_HASH, 1)).

hyperchains_unable_to_use_normal_db_test_() ->
    {foreach,
     fun() ->
             InitialApps = {running_apps(), loaded_apps()},

             % Mock the data dir
             {ok, RootDir} = file:get_cwd(),
             DataDir = filename:absname_join(RootDir, "data/aecore"),
             meck:new(aeu_env, [passthrough]),
             meck:expect(aeu_env, data_dir, fun(aecore) -> DataDir end),

             meck:new(aec_db, [passthrough]),
             meck:expect(aec_db, load_database, 0, ok),
             meck:new(aecore_sup, [passthrough]),
             meck:expect(aecore_sup, start_link, 0, {ok, pid}),
             meck:new(aec_jobs_queues, [passthrough]),
             meck:expect(aec_jobs_queues, start, 0, ok),
             meck:new(aec_chain_state, [passthrough]),
             meck:expect(aec_chain_state, ensure_chain_ends, 0, ok),
             meck:new(aec_consensus, [passthrough]),
             ok = lager:start(),

             InitialApps
     end,
     fun({OldRunningApps, OldLoadedApps}) ->
             meck:unload(aec_consensus),
             meck:unload(aec_chain_state),
             meck:unload(aec_jobs_queues),
             meck:unload(aecore_sup),
             meck:unload(aec_db),
             meck:unload(aeu_env),
             ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
end,
     [{"HC Genesis block != Mainnet Genesis block",
       fun() ->
            try
                % Get genesis hashes
                aehc_test_utils:enable_pow_cuckoo_from_genesis(),
                {ok, MainnetGenesisHash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
                aehc_test_utils:enable_hc_from_genesis(),
                {ok, HyperchainGenesisHash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
                aehc_test_utils:enable_pow_cuckoo_from_genesis(),

                % If enabling HC didn't change the genesis hash then something must be broken
                ?assertNotEqual(MainnetGenesisHash, HyperchainGenesisHash),

                % No HC, No DB - starts normally
                meck:expect(aec_db, get_genesis_hash, 0, undefined),
                meck:expect(aec_consensus, get_genesis_hash, 0, MainnetGenesisHash),
                ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

                % No HC, Mainnet DB present - starts normally
                meck:expect(aec_db, get_genesis_hash, 0, MainnetGenesisHash),
                meck:expect(aec_consensus, get_genesis_hash, 0, MainnetGenesisHash),
                ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

                % No HC, Hyperchain DB present - fails to start
                meck:expect(aec_db, get_genesis_hash, 0, HyperchainGenesisHash),
                meck:expect(aec_consensus, get_genesis_hash, 0, MainnetGenesisHash),
                ?assertEqual({error, inconsistent_database}, aecore_app:start(normal, [])),

                % HC Enabled, No DB - starts normally
                meck:expect(aec_db, get_genesis_hash, 0, undefined),
                meck:expect(aec_consensus, get_genesis_hash, 0, HyperchainGenesisHash),
                ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

                % HC Enabled, Mainnet DB present - fails to start
                meck:expect(aec_db, get_genesis_hash, 0, MainnetGenesisHash),
                meck:expect(aec_consensus, get_genesis_hash, 0, HyperchainGenesisHash),
                ?assertEqual({error, inconsistent_database}, aecore_app:start(normal, [])),

                % HC Enabled, Hyperchain DB present - starts normally
                meck:expect(aec_db, get_genesis_hash, 0, HyperchainGenesisHash),
                meck:expect(aec_consensus, get_genesis_hash, 0, HyperchainGenesisHash),
                ?assertEqual({ok, pid}, aecore_app:start(normal, []))
            after
                aehc_test_utils:enable_pow_cuckoo_from_genesis()
            end,
            ok
       end}
     ]}.

write_parent_chain_test_() ->
    aec_test_utils:eunit_with_consensus(aehc_test_utils:hc_from_genesis(), [
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
             aec_test_utils:mock_genesis_and_forks(),
             TmpDir = aec_test_utils:aec_keys_setup(),
             {ok, PubKey} = aec_keys:pubkey(),
             ok = application:set_env(aecore, beneficiary, aeser_api_encoder:encode(account_pubkey, PubKey)),
             {ok, _} = aec_tx_pool:start_link(),
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
             aec_test_utils:unmock_genesis_and_forks(),
             aec_test_utils:stop_chain_db(),
             ok = aec_db_error_store:stop(),
             aec_test_utils:aec_keys_cleanup(TmpDir)
     end,
     [{"Write and read back pinpointed genesis block",
       fun() ->
             ParentBlock = aehc_parent_block:new_block(?PARENT_GENESIS_HEADER, []),
             aehc_parent_db:write_parent_block(ParentBlock),
             ?assertEqual(ParentBlock, aehc_parent_db:get_parent_block(?PARENT_GENESIS_HASH)),
             ?assertEqual([], aehc_parent_db:get_candidates_in_election_cycle(1337, ?PARENT_GENESIS_HASH))
       end},
      {"Write a chain with commitments",
       fun() ->
            C1 = aehc_commitment:new(aehc_commitment_header:new(<<"D1">>, <<"BLOCK 1">>), no_pogf),
            C2 = aehc_commitment:new(aehc_commitment_header:new(<<"D2">>, <<"BLOCK 2">>), no_pogf),
            C3 = aehc_commitment:new(aehc_commitment_header:new(<<"D3">>, <<"BLOCK 3">>), no_pogf),
            C4 = aehc_commitment:new(aehc_commitment_header:new(<<"D4">>, <<"BLOCK 4">>), no_pogf),
            CList = [C1, C2, C3, C4],
            CHList = [aehc_commitment:hash(C) || C <- CList],
            ParentBlockHeader = aehc_parent_block:new_header(?PARENT_GENESIS_HASH, ?PARENT_GENESIS_HASH, 1, CHList),
            ParentBlock = aehc_parent_block:new_block(ParentBlockHeader, CList),
            aehc_parent_db:write_parent_block(ParentBlock),
            ?assertEqual(ParentBlock, aehc_parent_db:get_parent_block(?PARENT_GENESIS_HASH)),
            ?assertEqual(CList, aehc_parent_db:get_candidates_in_election_cycle(1337, ?PARENT_GENESIS_HASH))
       end}
     ]}]).
