-module(aec_persistence_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

-define(compareBlockResults(B1, B2),
        ?assertEqual(aec_blocks:serialize_for_network(B1),
                     aec_blocks:serialize_for_network(B2))).

-define(GENESIS_DIFFICULTY, 553713663.0).

genesis_block() ->
    aec_block_genesis:genesis_block().

block_hash(B) ->
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(B)),
    Hash.

header_hash(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

ts() -> integer_to_list(erlang:system_time()).

init_persistence() ->
    put('__persist__', application:get_env(aecore, persist, false)),
    application:set_env(aecore, persist, true),
    Path = ".test_" ++ ts(),
    file:make_dir(Path),
    {ok, _} = aec_persistence:start_link(Path),
    Path.

cleanup_persistence(Path) ->
    Persist = get('__persist__'),
    application:set_env(aecore, persist, Persist),
    ok = aec_persistence:stop_and_clean(),
    file:del_dir(Path),
    ok.

kill_and_restart_chain_server() ->
    %% Stop server
    ok = aec_chain:stop(),
    %% check that it is dead.
    dead =
        try aec_chain:top()
        catch exit:{noproc, _} -> dead
        end,
    %% Restart server
    {ok, _} = aec_chain:start_link(),
    server_up = wait_for_chain(),
    ok.

wait_for_chain() ->
    case
        try aec_chain:top()
        catch exit:{noproc, _} -> dead
        end
    of
        dead ->
            timer:sleep(10),
            wait_for_chain();
        _ ->
            server_up
    end.

write_test_() ->
    {foreach,
     fun() ->
             init_persistence()
     end,
     fun(Path) ->
             cleanup_persistence(Path)
     end,
     [{"Write a block to storage and read it back.",
       fun() ->
               GB = genesis_block(),
               ok = aec_persistence:write_block(GB),
               ok = aec_persistence:sync(),
               Hash = block_hash(GB),

               Block = aec_persistence:get_block(Hash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),
               ok
       end}]}.

write_chain_test_() ->
    {foreach,
     fun() ->
             Path = init_persistence(),
             {ok, _} = aec_chain:start_link(genesis_block()),
             {Path, aec_test_utils:aec_keys_setup()}
     end,
     fun({Path, TmpDir}) ->
             cleanup_persistence(Path),
             aec_test_utils:aec_keys_cleanup(TmpDir),
             ok = aec_chain:stop()
     end,
     [{"Write a block to chain and read it back.",
       fun() ->
               GB = genesis_block(),
               ok = aec_persistence:write_block(GB),

               Hash = block_hash(GB),

               Block = aec_persistence:get_block(Hash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),

               %% Genesis should be top header
               Header = aec_persistence:get_top_header(),
               ?assertEqual(Header, Hash),

               %% Genesis should be top block
               TopBlockHash = aec_persistence:get_top_block(),
               ?assertEqual(Header, TopBlockHash),

               ok
       end},
      {"Build chain with genesis block plus 2 headers, then store block corresponding to top header",
       fun() ->
               [GB, B1, B2] = aec_test_utils:gen_block_chain(3),
               %% Add a couple of headers - not blocks - to the chain.
               BH1 = aec_blocks:to_header(B1),
               ?assertEqual(ok, aec_chain:insert_header(BH1)),
               BH2 = aec_blocks:to_header(B2),
               ?assertEqual(ok, aec_chain:insert_header(BH2)),
               aec_persistence:sync(),

               GHash = block_hash(GB),

               Block = aec_persistence:get_block(GHash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),

               %% BH2 should be top header
               Header = aec_persistence:get_top_header(),
               B2Hash = header_hash(BH2),
               ?assertEqual(B2Hash, Header),

               %% Genesis should be top block
               TopBlockHash = aec_persistence:get_top_block(),
               ?assertEqual(GHash, TopBlockHash),

               %% Add one block corresponding to a header already in the chain.
               ?assertEqual(ok, aec_chain:write_block(B2)),
               aec_persistence:sync(),

               %% GB should still be top block
               NewTopBlockHash = aec_persistence:get_top_block(),
               ?assertEqual(GHash, NewTopBlockHash),

               %% Add missing block corresponding to a header already in the chain.
               ?assertEqual(ok, aec_chain:write_block(B1)),
               aec_persistence:sync(),
               %% Now B2 should be the top block
               LastTopBlockHash = aec_persistence:get_top_block(),
               ?assertEqual(B2Hash, LastTopBlockHash),

               ok
       end}
     ]}.


restart_test_() ->
    {foreach,
     fun() ->
             Path = init_persistence(),
             {ok, _} = aec_chain:start_link(genesis_block()),
             {Path, aec_test_utils:aec_keys_setup()}
     end,
     fun({Path, TmpDir}) ->
             %% ok = aec_chain:stop(),
             cleanup_persistence(Path),
             aec_test_utils:aec_keys_cleanup(TmpDir)
     end,
     [{"Build chain, then kill server, check that chain is read back.",
       fun() ->
               [_GB, B1, B2] = aec_test_utils:gen_block_chain(3),
               BH2 = aec_blocks:to_header(B2),
               ?assertEqual(ok, aec_chain:write_block(B1)),
               ?assertEqual(ok, aec_chain:write_block(B2)),
               aec_persistence:sync(),
               %% Now B2 should be the top block
               TopBlockHash = aec_persistence:get_top_block(),
               B2Hash = header_hash(BH2),
               ?assertEqual(B2Hash, TopBlockHash),
               {ok, ChainTop1} = aec_chain:top(),
               ?compareBlockResults(B2, ChainTop1),

               %% Check the state trees from persistence
               ?assertEqual(aec_persistence:get_block_state(TopBlockHash),
                            aec_blocks:trees(ChainTop1)),

               %% Kill chain server
               kill_and_restart_chain_server(),
               aec_persistence:sync(),
               NewTopBlockHash = aec_persistence:get_top_block(),
               ?assertEqual(B2Hash, NewTopBlockHash),

               {ok, ChainTop2} = aec_chain:top(),
               ?compareBlockResults(B2, ChainTop2),

               %% Compare the trees after restart
               %% Check the state trees from persistence
               ?assertEqual(aec_blocks:trees(ChainTop2),
                            aec_blocks:trees(ChainTop1)),

               ok
       end}
     ]}.
