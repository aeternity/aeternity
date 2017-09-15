-module(aec_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

fake_genesis_block() ->
    #block{height = 0,
           prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
           difficulty = 1,
           nonce = 0}.

top_test_() ->
    {foreach,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     [{"Initialize chain with genesis block, then check top block with related state trees",
       fun() ->
               GB = fake_genesis_block(),
               ?assertEqual({ok, GB}, aec_chain:top_block()),

               {ok, Top} = aec_chain:top(),
               %% Check block apart from state trees.
               ?assertEqual(GB, Top#block{trees = GB#block.trees}),
               %% Check state trees in block.
               _ = Top#block.trees %% TODO Check.
       end}]}.

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

             %% Check height of genesis - for readability of the test.
             0 = aec_headers:height(BH0),

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
             ?assertEqual({error, {block_not_found, {top_header, BH2}}},
                          aec_chain:get_block_by_hash(B1H)),
             {ok, B2H} = aec_headers:hash_internal_representation(BH2),
             ?assertEqual({ok, BH2}, aec_chain:get_header_by_hash(B2H)),
             ?assertEqual({error, {block_not_found, {top_header, BH2}}},
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

               %% Check height of genesis - for readability of the test.
               0 = aec_headers:height(BH0),

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
               ?assertEqual({error, {block_not_found, {top_header, BH2}}},
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

               %% Check height of genesis - for readability of the test.
               0 = aec_headers:height(BH0),

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
               ?assertEqual({error, {block_not_found, {top_header, BH2}}},
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

get_work_at_top_test_() ->
    {foreach,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     [{"Get work in chain of only genesis",
       fun() ->
               %% Check chain is at genesis.
               B0 = fake_genesis_block(),
               BH0 = aec_blocks:to_header(B0),
               ?assertEqual({ok, BH0}, aec_chain:top_header()),

               %% Check difficulty of genesis - for readability of the test.
               1.0 = aec_headers:linear_difficulty(BH0),

               %% Check work of chain at top.
               ?assertEqual({ok, {1.0, {top_header, BH0}}},
                            aec_chain:get_work_at_top())
       end},
      {"Get work in chain of genesis block plus 2 headers",
       fun() ->
               %% Check chain is at genesis.
               B0 = fake_genesis_block(),
               BH0 = aec_blocks:to_header(B0),
               ?assertEqual({ok, BH0}, aec_chain:top_header()),

               %% Check difficulty of genesis - for readability of the test.
               1.0 = aec_headers:linear_difficulty(BH0),

               %% Add a couple of headers to the chain.
               {ok, B0H} = aec_blocks:hash_internal_representation(B0),
               BH1 = #header{height = 1, prev_hash = B0H, difficulty = 2},
               ?assertEqual(ok, aec_chain:insert_header(BH1)),
               {ok, B1H} = aec_headers:hash_internal_representation(BH1),
               BH2 = #header{height = 2, prev_hash = B1H, difficulty = 5},
               ?assertEqual(ok, aec_chain:insert_header(BH2)),

               %% Check work of chain at top.
               ?assertEqual({ok, {8.0, {top_header, BH2}}},
                            aec_chain:get_work_at_top())
       end}]}.

%% Cover unhappy paths not covered in any other tests.
unhappy_paths_test_() ->
    {foreach,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     [{"Get header by hash - case not found",
       fun() ->
               %% Check chain is at genesis.
               B0 = fake_genesis_block(),
               BH0 = aec_blocks:to_header(B0),
               ?assertEqual({ok, BH0}, aec_chain:top_header()),

               %% Check height of genesis - for readability of the test.
               0 = aec_headers:height(BH0),

               %% Add a header to the chain.
               {ok, B0H} = aec_blocks:hash_internal_representation(B0),
               BH1 = #header{height = 1, prev_hash = B0H},
               ?assertEqual(ok, aec_chain:insert_header(BH1)),

               %% Attempt to lookup header not added to chain.
               {ok, B1H} = aec_headers:hash_internal_representation(BH1),
               BH2 = #header{height = 2, prev_hash = B1H},
               {ok, B2H} = aec_headers:hash_internal_representation(BH2),

               %% Attempt to get by hash header not added to chain.
               ?assertEqual({error, {header_not_found, {top_header, BH1}}},
                            aec_chain:get_header_by_hash(B2H))
       end}]}.

generate_block_chain_by_difficulties_with_nonce(
  GenesisBlock, [GenesisDifficulty | OtherDifficulties], Nonce) ->
    %% Check height of genesis - for readability.
    0 = aec_blocks:height(GenesisBlock),
    %% Check difficulty of genesis - for readability.
    GenesisDifficulty = aec_blocks:difficulty(GenesisBlock),
    lists:reverse(
      lists:foldl(
        fun(D, [PrevB | _] = BC) ->
                {ok, PrevHH} = aec_blocks:hash_internal_representation(PrevB),
                B = #block{height = 1 + aec_blocks:height(PrevB),
                           prev_hash = PrevHH,
                           difficulty = D,
                           nonce = Nonce},
                [B | BC]
        end,
        [GenesisBlock],
        OtherDifficulties)).

header_chain_from_block_chain(BC) ->
    lists:map(fun aec_blocks:to_header/1, BC).

longest_header_chain_test_() ->
    {foreach,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     [{"The alternative header chain has a different genesis hence its amount of work cannot be compared",
       fun() ->
               %% Check chain is at genesis.
               B0 = fake_genesis_block(),
               BH0 = aec_blocks:to_header(B0),
               ?assertEqual({ok, BH0}, aec_chain:top_header()),

               %% Check height of genesis - for readability of the test.
               0 = aec_headers:height(BH0),
               %% Check nonce of genesis - for readability of the test.
               0 = aec_headers:nonce(BH0),

               %% Generate the alternative header chain from a
               %% different genesis.
               HA0 = BH0#header{nonce = 1},
               {ok, HA0H} = aec_headers:hash_internal_representation(HA0),
               HA1 = #header{height = 1, prev_hash = HA0H},
               AltHC = [HA0, HA1],

               %% Attempt to determine chain with more work -
               %% specifying full header chain.
               ?assertEqual({error, {different_genesis, {genesis_header, BH0}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({error, {no_common_ancestor, {top_header, BH0}}},
                            aec_chain:has_more_work(
                              [HA1] = lists:nthtail(1, AltHC))),

               %% Check top.
               ?assertEqual({ok, BH0}, aec_chain:top_header())
       end},
      {"The alternative header chain does not have more work - case alternative chain is less high",
       fun() ->
               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 2, 2], 111),
               AltBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 3], 222),
               MainHC = [H0, _, HM2] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),

               %% Check top is main chain.
               ?assertEqual({ok, HM2}, aec_chain:top_header()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {false, {{{top_chain_work, 5.0},
                                           {alt_chain_work, 4.0}},
                                          {top_header, HM2}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {false, {{{top_chain_work, 5.0},
                                           {alt_chain_work, 4.0}},
                                          {top_header, HM2}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Give up updating chain because existing chain has more work.
               ok
       end},
      {"The alternative header chain does not have more work - case alternative chain is higher",
       fun() ->
               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 3], 111),
               AltBC = [B0, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1], 222),
               MainHC = [H0, HM1] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),

               %% Check top is main chain.
               ?assertEqual({ok, HM1}, aec_chain:top_header()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {false, {{{top_chain_work, 4.0},
                                           {alt_chain_work, 3.0}},
                                          {top_header, HM1}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {false, {{{top_chain_work, 4.0},
                                           {alt_chain_work, 3.0}},
                                          {top_header, HM1}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Give up updating chain because existing chain has more work.
               ok
       end},
      {"The alternative chain has the same amount of work, hence is to be ignored because received later",
       fun() ->
               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 2], 111),
               AltBC = [B0, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1], 222),
               MainHC = [H0, HM1] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),

               %% Check top is main chain.
               ?assertEqual({ok, HM1}, aec_chain:top_header()),

               %% Attempt to determine chain with more work -
               %% specifying full header chain.
               ?assertEqual({ok, {false, {{{top_chain_work, 3.0},
                                           {alt_chain_work, 3.0}},
                                          {top_header, HM1}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {false, {{{top_chain_work, 3.0},
                                           {alt_chain_work, 3.0}},
                                          {top_header, HM1}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Give up updating chain because existing chain has same work.
               ok
       end},
      {"The alternative header chain has more work - case alternative chain is higher. Force chain excluding genesis.",
       fun() ->
               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 2], 111),
               AltBC = [B0, _, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1, 1], 222),
               MainHC = [H0, HM1] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _, HA3] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),

               %% Check top is main chain.
               ?assertEqual({ok, HM1}, aec_chain:top_header()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM1}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM1}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Update chain because existing chain has less work.
               ?assertEqual({ok, {{old_top_header, HM1},
                                  {new_top_header, HA3}}},
                            aec_chain:force_insert_headers(
                              lists:nthtail(1, AltHC))),

               %% Check top changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),

               %% Check that headers in previous chain cannot be
               %% retrieved by hash; i.e. chain service minimizes used
               %% storage while exposing consistent view of chain.
               lists:foreach(
                 fun(H) ->
                         {ok, HH} = aec_headers:hash_internal_representation(H),
                         ?assertEqual({error, {header_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_header_by_hash(HH))
                 end,
                 lists:nthtail(1, MainHC))
       end},
      {"The alternative header chain has more work - case alternative chain is higher. Force chain including genesis.",
       fun() ->
               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 2], 111),
               AltBC = [B0, _, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1, 1], 222),
               MainHC = [H0, HM1] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _, HA3] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),

               %% Check top is main chain.
               ?assertEqual({ok, HM1}, aec_chain:top_header()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM1}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM1}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Update chain because existing chain has less work.
               ?assertEqual({ok, {{old_top_header, HM1},
                                  {new_top_header, HA3}}},
                            aec_chain:force_insert_headers(AltHC)),

               %% Check top changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),

               %% Check that headers in previous chain cannot be
               %% retrieved by hash; i.e. chain service minimizes used
               %% storage while exposing consistent view of chain.
               lists:foreach(
                 fun(H) ->
                         {ok, HH} = aec_headers:hash_internal_representation(H),
                         ?assertEqual({error, {header_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_header_by_hash(HH))
                 end,
                 lists:nthtail(1, MainHC))
       end},
      {"The alternative header chain has more work - case alternative chain is less high",
       fun() ->
               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1], 111),
               AltBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 3], 222),
               MainHC = [H0, _, HM2] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, HA1] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),

               %% Check top is main chain.
               ?assertEqual({ok, HM2}, aec_chain:top_header()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Update chain because existing chain has less work.
               ?assertEqual({ok, {{old_top_header, HM2},
                                  {new_top_header, HA1}}},
                            aec_chain:force_insert_headers(AltHC)),

               %% Check top changed.
               ?assertEqual({ok, HA1}, aec_chain:top_header()),

               %% Check that headers in previous chain cannot be
               %% retrieved by hash; i.e. chain service minimizes used
               %% storage while exposing consistent view of chain.
               lists:foreach(
                 fun(H) ->
                         {ok, HH} = aec_headers:hash_internal_representation(H),
                         ?assertEqual({error, {header_not_found,
                                               {top_header, HA1}}},
                                      aec_chain:get_header_by_hash(HH))
                 end,
                 lists:nthtail(1, MainHC))
       end},
      {"The alternative header chain has more work, but results in sub-optimal choice because of concurrent insertion",
       fun() ->
               %% This test has the main aim of clarifying design of
               %% whether chain service shall reject forcing chain
               %% with less work.

               %% Generate the two header chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _, _, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1, 2], 111),
               AltBC = [B0, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 3], 222),
               _MainHC = [H0, HM1, HM2, HM3] = header_chain_from_block_chain(MainBC),
               InitialMainHC = [H0, HM1, HM2],
               AltHC = [H0, HA1] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),

               %% Insert the initial part of the main chain.
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, InitialMainHC)),
               ?assertEqual({ok, HM2}, aec_chain:top_header()),

               %% Determine chain with more work.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(AltHC)),

               %% Decide to update chain because existing chain has
               %% less work.

               %% Concurrent actor increases amount of work in tracked
               %% chain.
               ok = aec_chain:insert_header(HM3),
               ?assertEqual({ok, HM3}, aec_chain:top_header()),

               %% Based on past - now obsolete - decision, update
               %% chain.  Chain tracked in chain service is
               %% sub-optimal.
               ?assertEqual({ok, {{old_top_header, HM3},
                                  {new_top_header, HA1}}},
                            aec_chain:force_insert_headers(AltHC)),
               ?assertEqual({ok, HA1}, aec_chain:top_header())
       end}]}.

longest_block_chain_test_() ->
    {foreach,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok = aec_chain:stop() end,
     [{"The alternative block chain has more work - case alternative chain with all blocks",
       fun() ->
               %% Generate the two block chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _, B2] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1], 111),
               AltBC = [B0, _, _, BA3] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1, 1], 222),
               MainHC = [H0, _, HM2] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _, HA3] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),
               ?assertEqual({ok, B0}, aec_chain:top_block()),

               %% Insert the main header chain...
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),
               %% ... and a block corresponding to a header already in
               %% the chain.
               ?assertEqual(ok, aec_chain:write_block(B2)),

               %% Check top is main chain.
               ?assertEqual({ok, HM2}, aec_chain:top_header()),
               ?assertEqual({ok, B2}, aec_chain:top_block()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Update header chain because existing chain has less
               %% work.
               ?assertEqual({ok, {{old_top_header, HM2},
                                  {new_top_header, HA3}}},
                            aec_chain:force_insert_headers(AltHC)),

               %% Check top changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),
               ?assertEqual({ok, B0}, aec_chain:top_block()),

               %% Insert all blocks for new chain.
               lists:foreach(
                 fun(B) -> ?assertEqual(ok, aec_chain:write_block(B)) end,
                 lists:nthtail(1, AltBC)),

               %% Check top block changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),
               ?assertEqual({ok, BA3}, aec_chain:top_block()),

               %% Check that headers and blocks in previous chain
               %% cannot be retrieved by hash; i.e. chain service
               %% minimizes used storage while exposing consistent
               %% view of chain."
               lists:foreach(
                 fun(H) ->
                         {ok, HH} = aec_headers:hash_internal_representation(H),
                         ?assertEqual({error, {header_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_header_by_hash(HH)),
                         ?assertEqual({error, {block_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_block_by_hash(HH))
                 end,
                 lists:nthtail(1, MainHC))
       end},
      {"The alternative block chain has more work - case alternative chain with only block corresponding to top header",
       fun() ->
               %% Generate the two block chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _, B2] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1], 111),
               AltBC = [B0, _, _, BA3] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1, 1], 222),
               MainHC = [H0, _, HM2] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _, HA3] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),
               ?assertEqual({ok, B0}, aec_chain:top_block()),

               %% Insert the main header chain...
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),
               %% ... and a block corresponding to a header already in
               %% the chain.
               ?assertEqual(ok, aec_chain:write_block(B2)),

               %% Check top is main chain.
               ?assertEqual({ok, HM2}, aec_chain:top_header()),
               ?assertEqual({ok, B2}, aec_chain:top_block()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Update header chain because existing chain has less
               %% work.
               ?assertEqual({ok, {{old_top_header, HM2},
                                  {new_top_header, HA3}}},
                            aec_chain:force_insert_headers(AltHC)),

               %% Check top changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),
               ?assertEqual({ok, B0}, aec_chain:top_block()),

               %% Insert only block corresponding to top header.
               ?assertEqual(ok, aec_chain:write_block(BA3)),

               %% Check top block changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),
               ?assertEqual({ok, BA3}, aec_chain:top_block()),

               %% Check that headers and blocks in previous chain
               %% cannot be retrieved by hash; i.e. chain service
               %% minimizes used storage while exposing consistent
               %% view of chain."
               lists:foreach(
                 fun(H) ->
                         {ok, HH} = aec_headers:hash_internal_representation(H),
                         ?assertEqual({error, {header_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_header_by_hash(HH)),
                         ?assertEqual({error, {block_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_block_by_hash(HH))
                 end,
                 lists:nthtail(1, MainHC))
       end},
      {"The alternative block chain has more work - case alternative chain with only block corresponding to header before top header",
       fun() ->
               %% Generate the two block chains.
               B0 = fake_genesis_block(),
               MainBC = [B0, _, B2] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1], 111),
               AltBC = [B0, _, BA2, _] = generate_block_chain_by_difficulties_with_nonce(B0, [1, 1, 1, 1], 222),
               MainHC = [H0, _, HM2] = header_chain_from_block_chain(MainBC),
               AltHC = [H0, _, _, HA3] = header_chain_from_block_chain(AltBC),

               %% Check chain is at genesis.
               ?assertEqual({ok, H0}, aec_chain:top_header()),
               ?assertEqual({ok, B0}, aec_chain:top_block()),

               %% Insert the main header chain...
               lists:foreach(
                 fun(H) -> ok = aec_chain:insert_header(H) end,
                 lists:nthtail(1, MainHC)),
               %% ... and a block corresponding to a header already in
               %% the chain.
               ?assertEqual(ok, aec_chain:write_block(B2)),

               %% Check top is main chain.
               ?assertEqual({ok, HM2}, aec_chain:top_header()),
               ?assertEqual({ok, B2}, aec_chain:top_block()),

               %% Determine chain with more work - specifying full
               %% header chain.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(AltHC)),
               %% Attempt to determine chain with more work -
               %% specifying header chain removing old ancestors.
               ?assertEqual({ok, {true, {{{top_chain_work, 3.0},
                                          {alt_chain_work, 4.0}},
                                         {top_header, HM2}}}},
                            aec_chain:has_more_work(lists:nthtail(1, AltHC))),

               %% Update header chain because existing chain has less
               %% work.
               ?assertEqual({ok, {{old_top_header, HM2},
                                  {new_top_header, HA3}}},
                            aec_chain:force_insert_headers(AltHC)),

               %% Check top changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),
               ?assertEqual({ok, B0}, aec_chain:top_block()),

               %% Insert only block corresponding to header before top
               %% header.
               ?assertEqual(ok, aec_chain:write_block(BA2)),

               %% Check top block changed.
               ?assertEqual({ok, HA3}, aec_chain:top_header()),
               ?assertEqual({ok, BA2}, aec_chain:top_block()),

               %% Check that headers and blocks in previous chain
               %% cannot be retrieved by hash; i.e. chain service
               %% minimizes used storage while exposing consistent
               %% view of chain."
               lists:foreach(
                 fun(H) ->
                         {ok, HH} = aec_headers:hash_internal_representation(H),
                         ?assertEqual({error, {header_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_header_by_hash(HH)),
                         ?assertEqual({error, {block_not_found,
                                               {top_header, HA3}}},
                                      aec_chain:get_block_by_hash(HH))
                 end,
                 lists:nthtail(1, MainHC))
       end}]}.
