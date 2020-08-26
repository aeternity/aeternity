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
             ok = lager:start(),

             InitialApps
     end,
     fun({OldRunningApps, OldLoadedApps}) ->
             meck:unload(aec_jobs_queues),
             meck:unload(aecore_sup),
             meck:unload(aec_db),
             meck:unload(aeu_env),
             ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
end,
     [{"HC Genesis block != Mainnet Genesis block",
       fun() ->
            % Get genesis hashes
            {ok, MainnetGenesisHash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
            aehc_utils:hc_install(),
            {ok, HyperchainGenesisHash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
            aehc_utils:hc_uninstall(),

            % If enabling HC didn't change the genesis hash then something must be broken
            ?assertNotEqual(MainnetGenesisHash, HyperchainGenesisHash),

            % No HC, No DB - starts normally
            meck:expect(aec_db, get_genesis_hash, 0, undefined),
            ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

            % No HC, Mainnet DB present - starts normally
            meck:expect(aec_db, get_genesis_hash, 0, MainnetGenesisHash),
            ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

            % No HC, Hyperchain DB present - fails to start
            meck:expect(aec_db, get_genesis_hash, 0, HyperchainGenesisHash),
            ?assertEqual({error, inconsistent_database}, aecore_app:start(normal, [])),

            % Enable hyperchains
            aehc_utils:hc_install(),

            % HC Enabled, No DB - starts normally
            meck:expect(aec_db, get_genesis_hash, 0, undefined),
            ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

            % HC Enabled, Mainnet DB present - fails to start
            meck:expect(aec_db, get_genesis_hash, 0, MainnetGenesisHash),
            ?assertEqual({error, inconsistent_database}, aecore_app:start(normal, [])),

            % HC Enabled, Hyperchain DB present - starts normally
            meck:expect(aec_db, get_genesis_hash, 0, HyperchainGenesisHash),
            ?assertEqual({ok, pid}, aecore_app:start(normal, [])),

            % Disable hyperchains
            aehc_utils:hc_install(),
            ok
       end}
     ]}.
