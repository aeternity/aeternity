%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_mining module
%%% @end
%%%=============================================================================
-module(aec_mining_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(TEST_MODULE, aec_mining).
-define(LOWEST_TARGET_SCI, 16#01010000).
-define(TEST_PUB, <<4,130,41,165,13,201,185,26,2,151,146,68,56,108,22,242,94,157,95,191,140,86,
                    145,96,71,82,28,176,23,5,128,17,245,174,170,199,54,248,167,43,185,12,108,91,
                    107,188,126,242,98,36,211,79,105,50,16,124,227,93,228,142,83,163,126,167,206>>).

mine_block_test_() ->
    PoWModules = [aec_pow_sha256, aec_pow_cuckoo],
    [{foreach,
      fun() ->
              application:start(crypto),
              meck:new(aec_blocks, [passthrough]),
              meck:new(aec_chain, [passthrough]),
              meck:new(aec_headers, [passthrough]),
              meck:new(aec_pow, [passthrough]),
              meck:new(aec_tx, [passthrough]),
              meck:new(aec_governance, [passthrough]),
              meck:new(aec_keys,[passthrough]),
              meck:new(aec_trees, [passthrough]),
              meck:expect(aec_pow, pow_module, 0, PoWMod),
              {ok, _} = aec_tx_pool:start_link()
      end,
      fun(_) ->
              application:stop(crypto),
              meck:unload(aec_blocks),
              meck:unload(aec_chain),
              meck:unload(aec_headers),
              meck:unload(aec_pow),
              meck:unload(aec_tx),
              meck:unload(aec_governance),
              meck:unload(aec_keys),
              meck:unload(aec_trees),
              ok = aec_tx_pool:stop()
      end,
      [
       {timeout, 60,
        {"Find a new block (PoW module " ++ atom_to_list(PoWMod) ++ ")",
         fun() ->
                 Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
                 meck:expect(aec_trees, all_trees_hash, 1, <<>>),
                 meck:expect(aec_chain, top, 0, {ok, #block{target = ?HIGHEST_TARGET_SCI}}),
                 meck:expect(aec_pow, pick_nonces, 0, {1, 400}),
                 meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
                 meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
                 meck:expect(aec_keys, sign, 1,
                             {ok, #signed_tx{data = #coinbase_tx{account = <<"pubkey">>},
                                             signatures = [<<"sig1">>]}}),

                 {ok, BlockCandidate, InitialNonce, MaxNonce} = ?TEST_MODULE:create_block_candidate(),
                 {ok, Block} = ?TEST_MODULE:mine(BlockCandidate, 400, InitialNonce, MaxNonce),

                 ?assertEqual(1, Block#block.height),
                 ?assertEqual(1, length(Block#block.txs))
         end}},
       {timeout, 60,
        {"Proof of work fails with generation_count_exhausted (PoW module " ++
             atom_to_list(PoWMod) ++ ")",
         fun() ->
                 Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
                 meck:expect(aec_trees, all_trees_hash, 1, <<>>),
                 meck:expect(aec_chain, top, 0, {ok, #block{target = ?LOWEST_TARGET_SCI}}),
                 meck:expect(aec_pow, pick_nonces, 0, {1, 100}),
                 meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
                 meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
                 meck:expect(aec_keys, sign, 1,
                             {ok, #signed_tx{data = #coinbase_tx{account = <<"pubkey">>},
                                             signatures = [<<"sig1">>]}}),

                 {ok, BlockCandidate, InitialNonce, MaxNonce} = ?TEST_MODULE:create_block_candidate(),
                 ?assertEqual({error, generation_count_exhausted},
                              ?TEST_MODULE:mine(BlockCandidate, 10, InitialNonce, MaxNonce))
         end}},
       {timeout, 60,
        {"Proof of work fails with nonce_range_exhausted (PoW module " ++
             atom_to_list(PoWMod) ++ ")",
         fun() ->
                 Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
                 meck:expect(aec_trees, all_trees_hash, 1, <<>>),
                 meck:expect(aec_chain, top, 0, {ok, #block{target = ?LOWEST_TARGET_SCI}}),
                 meck:expect(aec_pow, pick_nonces, 0, {1, 2}),
                 meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
                 meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
                 meck:expect(aec_keys, sign, 1,
                             {ok, #signed_tx{data = #coinbase_tx{account = <<"pubkey">>},
                                             signatures = [<<"sig1">>]}}),

                 {ok, BlockCandidate, InitialNonce, MaxNonce} = ?TEST_MODULE:create_block_candidate(),
                 ?assertEqual({error, nonce_range_exhausted},
                              ?TEST_MODULE:mine(BlockCandidate, 10, InitialNonce, MaxNonce))
         end}},
       {"Cannot apply signed tx (PoW module " ++ atom_to_list(PoWMod) ++ ")",
        fun() ->
                meck:expect(aec_chain, top, 0, {ok, #block{}}),
                meck:expect(aec_tx, apply_signed, 3, {error, tx_failed}),
                meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
                meck:expect(aec_keys, sign, 1,
                            {ok, #signed_tx{data = #coinbase_tx{account = <<"pubkey">>},
                                            signatures = [<<"sig1">>]}}),
                ?assertEqual({error, tx_failed}, ?TEST_MODULE:create_block_candidate())
        end},
       {timeout, 60,
        {"For good mining speed mine block with the same difficulty (PoW module " ++
             atom_to_list(PoWMod) ++ ")",
         fun() ->
                 Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
                 meck:expect(aec_trees, all_trees_hash, 1, <<>>),
                 Now = 1504731164584,
                 meck:expect(aec_chain, top, 0, {ok, #block{}}),
                 meck:expect(aec_blocks, new, 3,
                             {ok, #block{height = 30,
                                         target = ?HIGHEST_TARGET_SCI,
                                         time = Now}}),
                 meck:expect(aec_chain, get_header_by_height, 1,
                             {ok, #header{height = 20,
                                          time = Now - 50000}}),
                 meck:expect(aec_pow, pick_nonces, 0, {1, 500}),
                 meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
                 meck:expect(aec_governance, recalculate_difficulty_frequency, 0, 10),
                 meck:expect(aec_governance, expected_block_mine_rate, 0, 5),
                 meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
                 meck:expect(aec_keys, sign, 1,
                             {ok, #signed_tx{data = #coinbase_tx{account = <<"pubkey">>},
                                             signatures = [<<"sig1">>]}}),

                 {ok, BlockCandidate, InitialNonce, MaxNonce} = ?TEST_MODULE:create_block_candidate(),
                 {ok, Block} = ?TEST_MODULE:mine(BlockCandidate, 400, InitialNonce, MaxNonce),

                 ?assertEqual(30, Block#block.height),
                 case PoWMod of
                     aec_pow_cuckoo ->
                         ?assertEqual(42, length(Block#block.pow_evidence));
                     aec_pow_sha256 ->
                         ?assertEqual(no_value, Block#block.pow_evidence)
                 end,

                 %% Verify block
                 Block2 = aec_blocks:set_nonce(Block, 0, no_value),
                 {ok, BlockBin} = aec_headers:serialize_to_binary(aec_blocks:to_header(Block2)),
                 ?assertEqual(true, PoWMod:verify(BlockBin, Block#block.nonce,
                                                  Block#block.pow_evidence, ?HIGHEST_TARGET_SCI)),

                 ?assertEqual(?HIGHEST_TARGET_SCI, Block#block.target),
                 ?assertEqual(2, meck:num_calls(aec_governance, recalculate_difficulty_frequency, 0)),
                 ?assertEqual(1, meck:num_calls(aec_governance, expected_block_mine_rate, 0))
         end}},
       {timeout, 60,
        {"Too few blocks mined in time increases new block's target threshold (PoW module " ++
             atom_to_list(PoWMod) ++ ")",
         fun() ->
                 Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
                 meck:expect(aec_trees, all_trees_hash, 1, <<>>),
                 Now = 1504731164584,
                 Target = aec_pow:integer_to_scientific(?HIGHEST_TARGET_INT div 2),

                 meck:expect(aec_chain, top, 0, {ok, #block{}}),
                 meck:expect(aec_blocks, new, 3,
                             {ok, #block{height = 200,
                                         target = Target,
                                         time = Now}}),
                 meck:expect(aec_chain, get_header_by_height, 1,
                             {ok, #header{height = 190,
                                          target = Target,
                                          time = Now - 11000}}),
                 meck:expect(aec_pow, pick_nonces, 0, {1, 500}),
                 meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
                 meck:expect(aec_governance, recalculate_difficulty_frequency, 0, 10),
                 meck:expect(aec_governance, expected_block_mine_rate, 0, 100000),
                 meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
                 meck:expect(aec_keys, sign, 1,
                             {ok, #signed_tx{data = #coinbase_tx{account = <<"pubkey">>},
                                             signatures = [<<"sig1">>]}}),

                 {ok, BlockCandidate, InitialNonce, MaxNonce} = ?TEST_MODULE:create_block_candidate(),
                 {ok, Block} = ?TEST_MODULE:mine(BlockCandidate, 400, InitialNonce, MaxNonce),

                 ?assertEqual(200, Block#block.height),
                 case PoWMod of
                     aec_pow_cuckoo ->
                         ?assertEqual(42, length(Block#block.pow_evidence));
                     _ ->
                         ?assertEqual(no_value, Block#block.pow_evidence)
                 end,

                 %% Verify block
                 Block2 = aec_blocks:set_nonce(Block, 0, no_value),
                 {ok, BlockBin} = aec_headers:serialize_to_binary(aec_blocks:to_header(Block2)),
                 ?assertEqual(true, PoWMod:verify(BlockBin, Block#block.nonce,
                                                  Block#block.pow_evidence, ?HIGHEST_TARGET_SCI)),

                 ?assertEqual(true, Target < Block#block.target),
                 ?assertEqual(true, ?HIGHEST_TARGET_SCI >= Block#block.target)
         end}}
      ]
     } || PoWMod <- PoWModules].

mine_block_from_genesis_test_() ->
    PoWModules = [aec_pow_sha256, aec_pow_cuckoo],
    [{setup,
      fun() ->
              meck:new(aec_pow, [passthrough]),
              meck:expect(aec_pow, pow_module, 0, PoWMod),
              meck:expect(aec_pow, pick_nonces, 0, {1, 400}),
              {ok, _} = aec_tx_pool:start_link(),
              {ok, _} = aec_chain:start_link(aec_block_genesis:genesis_block()),
              TmpKeysDir = mktempd(),
              ok = application:ensure_started(crypto),
              {ok, _} = aec_keys:start_link(["mypassword", TmpKeysDir]),
              TmpKeysDir
      end,
      fun(TmpKeysDir) ->
              ok = aec_keys:stop(),
              ok = application:stop(crypto),
              {ok, KeyFiles} = file:list_dir(TmpKeysDir),
              %% Expect two filenames - private and public keys.
              [_KF1, _KF2] = KeyFiles,
              lists:foreach(
                fun(F) ->
                        AbsF = filename:absname_join(TmpKeysDir, F),
                        {ok, _} = {file:delete(AbsF), {F, AbsF}}
                end,
                KeyFiles),
              ok = file:del_dir(TmpKeysDir),
              ok = aec_chain:stop(),
              ok = aec_tx_pool:stop(),
              ?assert(meck:validate(aec_pow)),
              meck:unload(aec_pow),
              file:delete(TmpKeysDir)
      end,
      fun(_) ->
              [
               {timeout, 60,
                {"Find first block after genesis (PoW module " ++ atom_to_list(PoWMod) ++ ")",
                 fun() ->
                         {ok, BlockCandidate, InitialNonce, MaxNonce} = ?TEST_MODULE:create_block_candidate(),
                         {ok, Block} = ?TEST_MODULE:mine(BlockCandidate, 400, InitialNonce, MaxNonce),
                         ?assertEqual(1, aec_blocks:height(Block)),
                         ?assertEqual(1, length(Block#block.txs)),
                         ?assertMatch(<<H:?TXS_HASH_BYTES/unit:8>> when H > 0,
                                      Block#block.txs_hash)
                 end}}
              ]
      end} || PoWMod <- PoWModules].

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

-endif.
