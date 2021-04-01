-module(aecore_app_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

persisted_valid_gen_block_test_() ->
    {foreach,
     fun() ->
             InitialApps = {running_apps(), loaded_apps()},
             meck:new(aec_db, [passthrough]),
             meck:expect(aec_db, load_database, 0, ok),
             meck:new(aecore_sup, [passthrough]),
             meck:expect(aecore_sup, start_link, 0, {ok, pid}),
             meck:new(aec_jobs_queues, [passthrough]),
             meck:expect(aec_jobs_queues, start, 0, ok),
             meck:new(aec_chain_state, [passthrough]),
             meck:expect(aec_chain_state, ensure_chain_ends, 0, ok),
             meck:expect(aec_chain_state, ensure_key_headers_height_store, 0, ok),
             ok = lager:start(),
             InitialApps
     end,
     fun({OldRunningApps, OldLoadedApps}) ->
             meck:unload(aec_chain_state),
             meck:unload(aec_jobs_queues),
             meck:unload(aecore_sup),
             meck:unload(aec_db),
             ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
end,
     [{"Check persisted genesis block",
       fun() ->
            meck:expect(aec_db, persisted_valid_genesis_block, 0, false),
            ?assertEqual({error, inconsistent_database},
                         aecore_app:start(normal, [])),

            meck:expect(aec_db, persisted_valid_genesis_block, 0, true),
            ?assertEqual({ok, pid}, aecore_app:start(normal, [])),
            ok
       end}
     ]}.
