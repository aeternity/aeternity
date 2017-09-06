-module(aec_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

fake_genesis_block() ->
    #block{height = 0, prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>}.

genesis_test_() ->
    {setup,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     fun() ->
             GB = fake_genesis_block(),
             ?assertEqual({ok, GB}, aec_chain:top_block()),
             GH = aec_blocks:to_header(GB),
             ?assertEqual({ok, GH}, aec_chain:top_header()),

             ?assertEqual({ok, GH}, aec_chain:get_header_by_height(0)),
             ?assertEqual({ok, GB}, aec_chain:get_block_by_height(0)),

             {ok, GHH} = aec_blocks:hash_internal_representation(GB),
             ?assertEqual({ok, GH}, aec_chain:get_header_by_hash(GHH)),
             ?assertEqual({ok, GB}, aec_chain:get_block_by_hash(GHH))
     end}.

header_chain_test_() ->
    {setup,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     fun() ->
             %% Check chain is at genesis.
             B0 = fake_genesis_block(),
             BH0 = aec_blocks:to_header(B0),
             ?assertEqual({ok, BH0}, aec_chain:top_header()),

             %% Add a couple of headers - not blocks - to the chain.
             {ok, B0H} = aec_blocks:hash_internal_representation(B0),
             BH1 = #header{height = 1, prev_hash = B0H},
             ?assertEqual(ok, aec_chain:insert_header(BH1)),
             {ok, B1H} = aec_headers:hash_internal_representation(BH1),
             BH2 = #header{height = 2, prev_hash = B1H},
             ?assertEqual(ok, aec_chain:insert_header(BH2)),

             %% Check highest header.
             ?assertEqual({ok, BH2}, aec_chain:top_header()),
             %% Check heighest known block - still genesis.
             ?assertEqual({ok, B0}, aec_chain:top_block()),

             %% Check by hash.
             ?assertEqual({ok, BH0}, aec_chain:get_header_by_hash(B0H)),
             ?assertEqual({ok, B0}, aec_chain:get_block_by_hash(B0H)),
             ?assertEqual({ok, BH1}, aec_chain:get_header_by_hash(B1H)),
             ?assertEqual({error, block_not_found},
                          aec_chain:get_block_by_hash(B1H)),
             {ok, B2H} = aec_headers:hash_internal_representation(BH2),
             ?assertEqual({ok, BH2}, aec_chain:get_header_by_hash(B2H)),
             ?assertEqual({error, block_not_found},
                          aec_chain:get_block_by_hash(B2H)),

             %% Check by height.
             ?assertEqual({ok, BH0}, aec_chain:get_header_by_height(0)),
             ?assertEqual({ok, B0}, aec_chain:get_block_by_height(0)),
             ?assertEqual({ok, BH1}, aec_chain:get_header_by_height(1)),
             ?assertEqual({error, {block_not_found, {top_header, BH2}
                                  }}, aec_chain:get_block_by_height(1)),
             ?assertEqual({ok, BH2}, aec_chain:get_header_by_height(2)),
             ?assertEqual({error, {block_not_found, {top_header, BH2}
                                  }}, aec_chain:get_block_by_height(2)),
             ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                     {top_header, BH2}}
                                  }}, aec_chain:get_header_by_height(3)),
             ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                     {top_header, BH2}}
                                  }}, aec_chain:get_block_by_height(3))
     end}.

block_chain_test_() ->
    {foreach,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     [{"Build chain with genesis block plus 2 headers, then store block corresponding to top header",
       fun() ->
               %% Check chain is at genesis.
               B0 = fake_genesis_block(),
               BH0 = aec_blocks:to_header(B0),
               ?assertEqual({ok, BH0}, aec_chain:top_header()),

               %% Add a couple of headers - not blocks - to the chain.
               {ok, B0H} = aec_blocks:hash_internal_representation(B0),
               B1 = #block{height = 1, prev_hash = B0H},
               BH1 = aec_blocks:to_header(B1),
               ?assertEqual(ok, aec_chain:insert_header(BH1)),
               {ok, B1H} = aec_headers:hash_internal_representation(BH1),
               B2 = #block{height = 2, prev_hash = B1H},
               BH2 = aec_blocks:to_header(B2),
               ?assertEqual(ok, aec_chain:insert_header(BH2)),

               %% Add one block corresponding to a header already in the chain.
               ?assertEqual(ok, aec_chain:write_block(B2)),

               %% Check highest header.
               ?assertEqual({ok, BH2}, aec_chain:top_header()),
               %% Check heighest known block.
               ?assertEqual({ok, B2}, aec_chain:top_block()),

               %% Check by hash.
               ?assertEqual({ok, BH0}, aec_chain:get_header_by_hash(B0H)),
               ?assertEqual({ok, B0}, aec_chain:get_block_by_hash(B0H)),
               ?assertEqual({ok, BH1}, aec_chain:get_header_by_hash(B1H)),
               ?assertEqual({error, block_not_found},
                            aec_chain:get_block_by_hash(B1H)),
               {ok, B2H} = aec_headers:hash_internal_representation(BH2),
               ?assertEqual({ok, BH2}, aec_chain:get_header_by_hash(B2H)),
               ?assertEqual({ok, B2}, aec_chain:get_block_by_hash(B2H)),

               %% Check by height.
               ?assertEqual({ok, BH0}, aec_chain:get_header_by_height(0)),
               ?assertEqual({ok, B0}, aec_chain:get_block_by_height(0)),
               ?assertEqual({ok, BH1}, aec_chain:get_header_by_height(1)),
               ?assertEqual({error, {block_not_found, {top_header, BH2}
                                    }}, aec_chain:get_block_by_height(1)),
               ?assertEqual({ok, BH2}, aec_chain:get_header_by_height(2)),
               ?assertEqual({ok, B2}, aec_chain:get_block_by_height(2)),
               ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                       {top_header, BH2}}
                                    }}, aec_chain:get_header_by_height(3)),
               ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                       {top_header, BH2}}
                                    }}, aec_chain:get_block_by_height(3))
       end},
     {"Build chain with genesis block plus 2 headers, then store block corresponding to header before top header",
       fun() ->
               %% Check chain is at genesis.
               B0 = fake_genesis_block(),
               BH0 = aec_blocks:to_header(B0),
               ?assertEqual({ok, BH0}, aec_chain:top_header()),

               %% Add a couple of headers - not blocks - to the chain.
               {ok, B0H} = aec_blocks:hash_internal_representation(B0),
               B1 = #block{height = 1, prev_hash = B0H},
               BH1 = aec_blocks:to_header(B1),
               ?assertEqual(ok, aec_chain:insert_header(BH1)),
               {ok, B1H} = aec_headers:hash_internal_representation(BH1),
               B2 = #block{height = 2, prev_hash = B1H},
               BH2 = aec_blocks:to_header(B2),
               ?assertEqual(ok, aec_chain:insert_header(BH2)),

               %% Add one block corresponding to a header already in the chain.
               ?assertEqual(ok, aec_chain:write_block(B1)),

               %% Check highest header.
               ?assertEqual({ok, BH2}, aec_chain:top_header()),
               %% Check heighest known block.
               ?assertEqual({ok, B1}, aec_chain:top_block()),

               %% Check by hash.
               ?assertEqual({ok, BH0}, aec_chain:get_header_by_hash(B0H)),
               ?assertEqual({ok, B0}, aec_chain:get_block_by_hash(B0H)),
               ?assertEqual({ok, BH1}, aec_chain:get_header_by_hash(B1H)),
               ?assertEqual({ok, B1}, aec_chain:get_block_by_hash(B1H)),
               {ok, B2H} = aec_headers:hash_internal_representation(BH2),
               ?assertEqual({ok, BH2}, aec_chain:get_header_by_hash(B2H)),
               ?assertEqual({error, block_not_found},
                            aec_chain:get_block_by_hash(B2H)),

               %% Check by height.
               ?assertEqual({ok, BH0}, aec_chain:get_header_by_height(0)),
               ?assertEqual({ok, B0}, aec_chain:get_block_by_height(0)),
               ?assertEqual({ok, BH1}, aec_chain:get_header_by_height(1)),
               ?assertEqual({ok, B1}, aec_chain:get_block_by_height(1)),
               ?assertEqual({ok, BH2}, aec_chain:get_header_by_height(2)),
               ?assertEqual({error, {block_not_found, {top_header, BH2}
                                    }}, aec_chain:get_block_by_height(2)),
               ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                       {top_header, BH2}}
                                    }}, aec_chain:get_header_by_height(3)),
               ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                       {top_header, BH2}}
                                    }}, aec_chain:get_block_by_height(3))
       end}]}.
