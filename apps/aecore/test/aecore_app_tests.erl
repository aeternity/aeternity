-module(aecore_app_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

persisted_valid_gen_block_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_db, [passthrough]),
             meck:expect(aec_db, load_database, 0, ok),
             meck:new(aecore_sup, [passthrough]),
             meck:expect(aecore_sup, start_link, 0, {ok, pid}),
             lager:start(),
             ok
     end,
     fun(ok) ->
             ok = application:stop(lager),
             ok = application:stop(mnesia),
             meck:unload(aecore_sup),
             meck:unload(aec_db)
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

