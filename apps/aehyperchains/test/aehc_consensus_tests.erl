%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% Tests for deploying the hyperchains staking contract, using a predeployed staking contract, switching from PoW to HC
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_consensus_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").

%% Magic nonces
-define(NONCE_HC_ENABLED, 16#ffffffffffffffff - 1).
-define(NONCE_HC_POGF, 16#ffffffffffffffff).
-define(AE, 1000000000000000000).
-define(GENESIS_TARGET, 553713663).

-define(PARENT_GENESIS_HASH, <<"GENESIS_AAAAAAAAAAAAAAAAAAAAAAAA">>).
-define(PARENT_HASH1, <<"Parent block 1_AAAAAAAAAAAAAAAAA">>).
-define(PARENT_HASH2, <<"Parent block 2_AAAAAAAAAAAAAAAAA">>).
-define(PARENT_GENESIS_HEADER,
    aehc_parent_block:new_header(?PARENT_GENESIS_HASH, ?PARENT_GENESIS_HASH, 1)).

-define(PROTOCOL_GATE(X), case init:get_argument(network_id) of
                              {ok,[["local_iris_testnet"]]} -> X;
                              _ -> []
                          end).

%% PoW genesis - aec_conductor is not started
pow_from_genesis_test_() ->
    aec_test_utils:eunit_with_consensus(aehc_test_utils:cuckoo_pow_from_genesis(), ?PROTOCOL_GATE(
        [{foreach,
             fun() ->
                     persistent_term:erase({aehc_consensus_hyperchains, staking_contract}),
                     aec_test_utils:mock_genesis_and_forks(aehc_test_utils:genesis_accounts()),
                     aec_test_utils:start_chain_db(),
                     aec_consensus_bitcoin_ng:load_whitelist(),
                     aec_test_utils:aec_keys_setup()
             end,
             fun(TmpDir) ->
                     aec_test_utils:aec_keys_cleanup(TmpDir),
                     aec_test_utils:stop_chain_db(),
                     aec_test_utils:unmock_genesis_and_forks()
             end,
             [ {"HC specific methods don't work when we use only PoW",
                 fun() ->
                     ?assertEqual(undefined, aec_chain:top_block()),
                     F = fun() ->
                            ?assertException(error, badarg, aehc_consensus_hyperchains:get_staking_contract_aci()),
                            not_deployed = aehc_consensus_hyperchains:get_staking_contract_address()
                         end,
                     Chain = aehc_test_utils:gen_blocks_only_chain(3),
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
        ])).

dummy_key_header_with_nonce_and_seal(Nonce, Seal) ->
    aec_headers:new_key_header(1337,
               <<1337:32/unit:8>>,
               <<13371337:32/unit:8>>,
               <<133713371337:32/unit:8>>,
               <<1337133713371337:32/unit:8>>,
               <<13371337133713371337:32/unit:8>>,
               1337,
               Seal,
               Nonce,
               aeu_time:now_in_msecs(),
               default,
               3).

%% Check the structure of a hyperchains block
pos_block_structure_test_() ->
    aehc_test_utils:hc_chain_eunit_testcase(aehc_test_utils:hc_from_genesis(), ?PROTOCOL_GATE(
        [ {"Parent hash and miner signature roundtrip",
            fun() ->
                error = aehc_consensus_hyperchains:deserialize_pos_pow_field(no_value),
                error = aehc_consensus_hyperchains:deserialize_pos_pow_field(0),
                RoundtripF = fun(H, S) ->
                        Seal = aehc_consensus_hyperchains:create_pos_pow_field(H, S),
                        {ok, H, S} = aehc_consensus_hyperchains:deserialize_pos_pow_field(Seal),
                        error = aehc_consensus_hyperchains:deserialize_pos_pow_field([1|Seal]),
                        error = aehc_consensus_hyperchains:deserialize_pos_pow_field(tl(Seal) ++ [1])
                    end,
                RoundtripF(<<0:32/unit:8>>, <<0:64/unit:8>>),
                RoundtripF(<<0:32/unit:8>>, <<1337:64/unit:8>>),
                RoundtripF(<<1337:32/unit:8>>, <<0:64/unit:8>>),
                Test1 = list_to_binary(lists:seq(1,32)),
                Test2 = list_to_binary(lists:reverse(lists:seq(1,64))),
                RoundtripF(Test1, Test2),
                ok
            end},
          {"Key header with special nonce but no special key seal is a PoW block",
           fun() ->
               [false = aehc_consensus_hyperchains:is_hc_pos_header(dummy_key_header_with_nonce_and_seal(Nonce, no_value)) || Nonce <- [?NONCE_HC_ENABLED, ?NONCE_HC_POGF]],
               [key_pow = aehc_consensus_hyperchains:hc_header_type(dummy_key_header_with_nonce_and_seal(Nonce, no_value)) || Nonce <- [?NONCE_HC_ENABLED, ?NONCE_HC_POGF]],
               ok
           end},
          {"Key header with special nonce and special key seal is a PoS block",
           fun() ->
               ParentHash = list_to_binary(lists:seq(1,32)),
               MinerSignature = list_to_binary(lists:reverse(lists:seq(1,64))),
               Seal = aehc_consensus_hyperchains:create_pos_pow_field(ParentHash, MinerSignature),
               [true = aehc_consensus_hyperchains:is_hc_pos_header(dummy_key_header_with_nonce_and_seal(Nonce, Seal)) || Nonce <- [?NONCE_HC_ENABLED, ?NONCE_HC_POGF]],
               key_pos = aehc_consensus_hyperchains:hc_header_type(dummy_key_header_with_nonce_and_seal(?NONCE_HC_ENABLED, Seal)),
               key_pos_pogf = aehc_consensus_hyperchains:hc_header_type(dummy_key_header_with_nonce_and_seal(?NONCE_HC_POGF, Seal)),
               ok
           end},
           {"PoS keyheader binary roundtrip",
            fun() ->
               ParentHash = list_to_binary(lists:seq(1,32)),
               MinerSignature = list_to_binary(lists:reverse(lists:seq(1,64))),
               Seal = aehc_consensus_hyperchains:create_pos_pow_field(ParentHash, MinerSignature),
               [begin
                    Header = dummy_key_header_with_nonce_and_seal(Nonce, Seal),
                    true = aehc_consensus_hyperchains:is_hc_pos_header(Header),
                    Type = aehc_consensus_hyperchains:hc_header_type(Header),
                    Serialized = aec_headers:serialize_to_binary(Header),
                    Header = aec_headers:deserialize_from_binary(Serialized)
                end || {Nonce, Type} <- [ {?NONCE_HC_ENABLED, key_pos}
                                        , {?NONCE_HC_POGF, key_pos_pogf}
                                        ]],
                ok
            end},
           {"Parent hash and miner signature setter/getter",
            fun() ->
                ParentHash1 = list_to_binary(lists:seq(1,32)),
                ParentHash2 = list_to_binary(lists:seq(33,64)),
                MinerSignature1 = list_to_binary(lists:reverse(lists:seq(1,64))),
                MinerSignature2 = list_to_binary(lists:reverse(lists:seq(65,128))),
                Seal = aehc_consensus_hyperchains:create_pos_pow_field(ParentHash1, MinerSignature1),
                [begin
                    Header1 = dummy_key_header_with_nonce_and_seal(Nonce, Seal),
                    ParentHash1 = aehc_consensus_hyperchains:get_pos_header_parent_hash(Header1),
                    MinerSignature1 = aehc_consensus_hyperchains:get_pos_header_miner_signature(Header1),

                    Header2 = aehc_consensus_hyperchains:set_pos_header_parent_hash(Header1, ParentHash2),
                    ParentHash2 = aehc_consensus_hyperchains:get_pos_header_parent_hash(Header2),
                    MinerSignature1 = aehc_consensus_hyperchains:get_pos_header_miner_signature(Header2),

                    Header3 = aehc_consensus_hyperchains:set_pos_header_miner_signature(Header1, MinerSignature2),
                    ParentHash1 = aehc_consensus_hyperchains:get_pos_header_parent_hash(Header3),
                    MinerSignature2 = aehc_consensus_hyperchains:get_pos_header_miner_signature(Header3),

                    Header4 = aehc_consensus_hyperchains:set_pos_header_miner_signature(Header2, MinerSignature2),
                    Header4 = aehc_consensus_hyperchains:set_pos_header_parent_hash(Header3, ParentHash2),
                    ParentHash2 = aehc_consensus_hyperchains:get_pos_header_parent_hash(Header4),
                    MinerSignature2 = aehc_consensus_hyperchains:get_pos_header_miner_signature(Header4)
                end || Nonce <- [?NONCE_HC_ENABLED, ?NONCE_HC_POGF]],
                ok
            end}
        ])).

%% HC genesis - aec_conductor is started
hc_from_genesis_test_() ->
    aehc_test_utils:hc_chain_eunit_testcase(aehc_test_utils:hc_from_genesis(), ?PROTOCOL_GATE(
        [ {"Can mine some blocks",
            fun() ->
                [GB | Blocks] = aehc_test_utils:gen_blocks_only_chain(10),
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
                [GB | Blocks] = aehc_test_utils:gen_blocks_only_chain(10),
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
        , {"One delegate, HC activation at 10. Activation criteria (1AE, 1, 5, 0)",
            fun() ->
                try
                    meck:new(aehc_utils, [passthrough]),
                    aehc_consensus_hyperchains:set_hc_activation_criteria(1 * ?AE, 1, 5, 0),
                    %% Make the patron the delegate
                    #{pubkey := PatronPubkey} = Patron = aehc_test_utils:patron(),
                    {ok, ContractAddress} = aehc_consensus_hyperchains:get_staking_contract_address(),
                    Aci = aehc_consensus_hyperchains:get_staking_contract_aci(),
                    Fee = 1 bsl 60,
                    Gas = 1 bsl 30,
                    GasPrice = 1 bsl 30,
                    MkCallF = fun(#{ pubkey := Pub, privkey := Priv }, Nonce, Amount, Call) ->
                                Tx = make_contract_call_tx(Pub, ContractAddress, Call, Nonce, Amount, Fee, Gas, GasPrice),
                                aec_test_utils:sign_tx(Tx, Priv)
                             end,
                    {ok, CallDepositStake} = aeaci_aci:encode_call_data(Aci, "deposit_stake()"),
                    %% The overall picture of what's going on in the chain
                    TxFuns =
                        fun %% Deposit 1 AE at height 5
                            (5) -> [ MkCallF(Patron, 1, 1 * ?AE, CallDepositStake) ];
                            (_) -> []
                        end,
                    Targets = [?GENESIS_TARGET || _ <- lists:seq(1,9)],
                    Chain1 = aehc_test_utils:gen_block_chain_with_state(Targets, TxFuns),
                    insert_blocks(aec_test_utils:blocks_only_chain(tl(Chain1))),
                    assert_static_staking_call_result({ok, false}, "enabled()"),
                    assert_static_staking_call_result({ok, 1 * ?AE}, "balance()"),
                    %% Now we create the block at 10 - the first HC block :)
                    meck:expect(aehc_utils, submit_commitment,
                        fun(KeyNode, Delegate) ->
                            PatronPubkey = Delegate,
                            C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
                            CList = [C],
                            CHList = [aehc_commitment:hash(C) || C <- CList],
                            ParentBlockHeader = aehc_parent_block:new_header(?PARENT_GENESIS_HASH, ?PARENT_GENESIS_HASH, 1, CHList),
                            ParentBlock = aehc_parent_block:new_block(ParentBlockHeader, CList),
                            aehc_parent_db:write_parent_block(ParentBlock),
                            ParentBlock
                        end),
                    Chain2 = aec_test_utils:extend_block_chain_with_key_blocks(Chain1, 1, PatronPubkey, PatronPubkey, #{}),
                    insert_blocks(aec_test_utils:blocks_only_chain(Chain2)),
                    assert_static_staking_call_result({ok, true}, "enabled()"),
                    assert_static_staking_call_result({ok, {variant, [0, 1], 1, {{address, PatronPubkey}}}}, "get_computed_leader()"),
                    assert_static_staking_call_result({ok, 1 * ?AE}, "balance()"),
                    %% Great! We're a hyperchain!
                    %% Emit another block to show that everything works :)
                    meck:expect(aehc_utils, submit_commitment,
                        fun(KeyNode, Delegate) ->
                            PatronPubkey = Delegate,
                            C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
                            CList = [C],
                            CHList = [aehc_commitment:hash(C) || C <- CList],
                            ParentBlockHeader = aehc_parent_block:new_header(?PARENT_HASH1, ?PARENT_GENESIS_HASH, 2, CHList),
                            ParentBlock = aehc_parent_block:new_block(ParentBlockHeader, CList),
                            aehc_parent_db:write_parent_block(ParentBlock),
                            ParentBlock
                        end),
                    Chain3 = aec_test_utils:extend_block_chain_with_key_blocks(Chain2, 1, PatronPubkey, PatronPubkey, #{}),
                    insert_blocks(aec_test_utils:blocks_only_chain(Chain3)),
                    assert_static_staking_call_result({ok, true}, "enabled()"),
                    assert_static_staking_call_result({ok, {variant, [0, 1], 1, {{address, PatronPubkey}}}}, "get_computed_leader()"),
                    assert_static_staking_call_result({ok, 1 * ?AE}, "balance()"),
                    ok
                after
                    meck:unload(aehc_utils),
                    aehc_consensus_hyperchains:unset_hc_activation_criteria()
                end
            end}
        , {"Two delegates, HC activation at 10. Activation criteria (1AE, 1, 5, 0)",
            fun() ->
                try
                    meck:new(aehc_utils, [passthrough]),
                    aehc_consensus_hyperchains:set_hc_activation_criteria(1 * ?AE, 1, 5, 0),
                    assert_static_staking_call_result({ok, false}, "enabled()"),
                    assert_static_staking_call_result({ok, 0}, "balance()"),
                    %% Use the patron to fund 2 accounts
                    #{ pubkey := PatronPubKey, privkey := PatronPrivKey } = aehc_test_utils:patron(),
                    #{public := D1Pub} = Delegate1 = enacl:sign_keypair(),
                    #{public := D2Pub} = Delegate2 = enacl:sign_keypair(),
                    {ok, ContractAddress} = aehc_consensus_hyperchains:get_staking_contract_address(),
                    Aci = aehc_consensus_hyperchains:get_staking_contract_aci(),
                    Fee = 1 bsl 60,
                    Gas = 1 bsl 30,
                    GasPrice = 1 bsl 30,
                    MkCallF = fun(#{ public := Pub, secret := Priv }, Nonce, Amount, Call) ->
                                Tx = make_contract_call_tx(Pub, ContractAddress, Call, Nonce, Amount, Fee, Gas, GasPrice),
                                aec_test_utils:sign_tx(Tx, Priv)
                             end,
                    MkFundingF = fun(#{ public := Dest }, Nonce, Amount) -> aec_test_utils:sign_tx(make_spend_tx(PatronPubKey, Nonce, Dest, Amount), PatronPrivKey) end,
                    {ok, CallDepositStake} = aeaci_aci:encode_call_data(Aci, "deposit_stake()"),
                    %% The overall picture of what's going on in the chain
                    TxFuns =
                        fun %% Fund the accounts with 2 AE
                            (2) -> [ MkFundingF(Delegate1, 1, 4 * ?AE)
                                   , MkFundingF(Delegate2, 2, 4 * ?AE)
                                   ];
                            %% Delegate1 stakes 1AE, Delegate2 stakes 1AE
                            (5) -> [ MkCallF(Delegate1, 1, 1 * ?AE, CallDepositStake)
                                   , MkCallF(Delegate2, 1, 1 * ?AE, CallDepositStake)
                                   ];
                            (_) -> []
                        end,
                    Targets = [?GENESIS_TARGET || _ <- lists:seq(1,9)],
                    Chain1 = aehc_test_utils:gen_block_chain_with_state(Targets, TxFuns),
                    insert_blocks(aec_test_utils:blocks_only_chain(tl(Chain1))),
                    assert_static_staking_call_result({ok, false}, "enabled()"),
                    assert_static_staking_call_result({ok, 2 * ?AE}, "balance()"),
                    %% Now we create the block at 10 - the first HC block :)
                    %% Only delegate1 commits to the block at 9
                    meck:expect(aehc_utils, submit_commitment,
                        fun(KeyNode, Delegate) ->
                            PatronPubkey = Delegate,
                            C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
                            CList = [C],
                            CHList = [aehc_commitment:hash(C) || C <- CList],
                            ParentBlockHeader = aehc_parent_block:new_header(?PARENT_GENESIS_HASH, ?PARENT_GENESIS_HASH, 1, CHList),
                            ParentBlock = aehc_parent_block:new_block(ParentBlockHeader, CList),
                            aehc_parent_db:write_parent_block(ParentBlock),
                            ParentBlock
                        end),
                    Chain2 = aec_test_utils:extend_block_chain_with_key_blocks(Chain1, 1, D1Pub, D1Pub, #{}),
                    insert_blocks(aec_test_utils:blocks_only_chain(Chain2)),
                    assert_static_staking_call_result({ok, true}, "enabled()"),
                    assert_static_staking_call_result({ok, {variant, [0, 1], 1, {{address, D1Pub}}}}, "get_computed_leader()"),
                    assert_static_staking_call_result({ok, 2 * ?AE}, "balance()"),
                    %% Emit a block by delegate2
                    meck:expect(aehc_utils, submit_commitment,
                        fun(KeyNode, Delegate) ->
                            PatronPubkey = Delegate,
                            C = aehc_commitment:new(aehc_commitment_header:new(Delegate, aec_block_insertion:node_hash(KeyNode)), no_pogf),
                            CList = [C],
                            CHList = [aehc_commitment:hash(C) || C <- CList],
                            ParentBlockHeader = aehc_parent_block:new_header(?PARENT_HASH1, ?PARENT_GENESIS_HASH, 2, CHList),
                            ParentBlock = aehc_parent_block:new_block(ParentBlockHeader, CList),
                            aehc_parent_db:write_parent_block(ParentBlock),
                            ParentBlock
                        end),
                    Chain3 = aec_test_utils:extend_block_chain_with_key_blocks(Chain2, 1, D2Pub, D2Pub, #{}),
                    insert_blocks(aec_test_utils:blocks_only_chain(Chain3)),
                    assert_static_staking_call_result({ok, true}, "enabled()"),
                    assert_static_staking_call_result({ok, {variant, [0, 1], 1, {{address, D2Pub}}}}, "get_computed_leader()"),
                    assert_static_staking_call_result({ok, 2 * ?AE}, "balance()"),
                    %% Test that if D2 got elected D1 cannot insert a block
                    [B1] = aec_test_utils:blocks_only_chain(Chain3),
                    B2 = aec_blocks:set_miner(B1, D1Pub),
                    {error,miner_not_leader} = aec_chain_state:insert_block(B2),
                    %% Time to make a more complicated election
                    %% Commit 1 and 2
                    meck:expect(aehc_utils, submit_commitment,
                        fun(KeyNode, Delegate) ->
                            PatronPubkey = Delegate,
                            C1 = aehc_commitment:new(aehc_commitment_header:new(D1Pub, aec_block_insertion:node_hash(KeyNode)), no_pogf),
                            C2 = aehc_commitment:new(aehc_commitment_header:new(D2Pub, aec_block_insertion:node_hash(KeyNode)), no_pogf),
                            CList = [C1, C2],
                            CHList = [aehc_commitment:hash(C) || C <- CList],
                            ParentBlockHeader = aehc_parent_block:new_header(?PARENT_HASH2, ?PARENT_HASH1, 3, CHList),
                            ParentBlock = aehc_parent_block:new_block(ParentBlockHeader, CList),
                            aehc_parent_db:write_parent_block(ParentBlock),
                            ParentBlock
                        end),
                    Chain4 = aec_test_utils:extend_block_chain_with_key_blocks(Chain3, 1, D2Pub, D2Pub, #{}),
                    insert_blocks(aec_test_utils:blocks_only_chain(Chain4)),
                    ok
                after
                    meck:unload(aehc_utils),
                    aehc_consensus_hyperchains:unset_hc_activation_criteria()
                end
            end
          }
        ])).

hc_switchover_at_10_test_() ->
    aehc_test_utils:hc_chain_eunit_testcase(aehc_test_utils:pow_to_hc_switch(10), ?PROTOCOL_GATE(
        [ {"Can mine some blocks. Accepts PoW blocks even with the global contract address available",
            fun() ->
                not_deployed = aehc_consensus_hyperchains:get_staking_contract_address(),
                Chain = [GB | Blocks] = aehc_test_utils:gen_blocks_only_chain(20),
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
                Chain = aehc_test_utils:gen_blocks_only_chain(21), %% This will set the predeploy address
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
                 end || B <- HCBlocks],
                ok
            end},
         {"Staking contract can be deployed by an ordinary user, until activated HC does not deviate from PoW",
            fun() ->
                %% Ok this test will be slightly messy - CT suites are more suited for this but well unit tests are important!
                %% Pick the deployer of the staking contract :)
                #{ public := DeployerPubKey, secret := DeployerPrivKey } = Deployer = enacl:sign_keypair(),
                %% And a random person which will help us with testing
                #{ public := RandomPubKey} = Random = enacl:sign_keypair(),
                %% The sponsor of the accounts
                #{ pubkey := PatronPubKey, privkey := PatronPrivKey } = aehc_test_utils:patron(),
                ContractAddress = aect_contracts:compute_contract_pubkey(DeployerPubKey, 1),
                Aci = aehc_consensus_hyperchains:get_staking_contract_aci(),
                Fee = 1 bsl 60,
                Gas = 1 bsl 30,
                GasPrice = 1 bsl 30,
                MkCallF = fun(#{ public := Pub, secret := Priv }, Nonce, Amount, Call) ->
                            Tx = make_contract_call_tx(Pub, ContractAddress, Call, Nonce, Amount, Fee, Gas, GasPrice),
                            aec_test_utils:sign_tx(Tx, Priv)
                         end,
                CallResF = fun(Address, Nonce) ->
                              CallPubkey     = aect_call:id(Address, Nonce, ContractAddress),
                              {_, Trees}     = aec_chain:top_block_with_state(),
                              CallTree       = aec_trees:calls(Trees),
                              {value, Call}  = aect_call_state_tree:lookup_call(ContractAddress, CallPubkey, CallTree),
                              case aect_call:return_type(Call) of
                                  ok ->
                                      {ok, aeb_fate_encoding:deserialize(aect_call:return_value(Call))};
                                  What ->
                                      {error, {What, aect_call:return_type(Call)}}
                              end
                           end,
                {ok, CallEnabled} = aeaci_aci:encode_call_data(Aci, "enabled()"),
                {ok, CallProtocolEnable} = aeaci_aci:encode_call_data(Aci, "protocol_enable()"),
                {ok, CallRestrictedAddress} = aeaci_aci:encode_call_data(Aci, "restricted_address()"),
                {ok, CallDepositStake} = aeaci_aci:encode_call_data(Aci, "deposit_stake()"),
                %% The overall picture of what's going on in the chain
                TxFuns =
                    fun %% Fund the deployer and the random account at 5
                        (5) ->
                            SpendTx1 = make_spend_tx(PatronPubKey, 1, DeployerPubKey, Fee * 40),
                            SpendTx2 = make_spend_tx(PatronPubKey, 2, RandomPubKey, Fee * 40),
                            [ aec_test_utils:sign_tx(SpendTx1, PatronPrivKey)
                            , aec_test_utils:sign_tx(SpendTx2, PatronPrivKey)
                            ];
                        %% Deploy the staking contract at 6
                        (6) ->
                            Bytecode = aehc_consensus_hyperchains:get_staking_contract_bytecode(),
                            Args = lists:flatten(io_lib:format("init({deposit_delay = ~p, stake_retraction_delay = ~p, withdraw_delay = ~p}, {}, ~s)",
                                [5, 5, 5, aeser_api_encoder:encode(account_pubkey, <<2:32/unit:8>>)])),
                            {ok, CtorCall} = aeaci_aci:encode_call_data(Aci, Args),
                            CreateTx = make_contract_create_tx(DeployerPubKey, Bytecode, CtorCall, 1, 0, 0, Fee, Gas, GasPrice),
                            [aec_test_utils:sign_tx(CreateTx, DeployerPrivKey)];
                        %% Make some stateful and stateless staking contract calls BEFORE the switchover
                        (8) ->
                            Arg4 = lists:flatten(io_lib:format("staked_tokens(~s)", [aeser_api_encoder:encode(account_pubkey, DeployerPubKey)])),
                            {ok, Call4} = aeaci_aci:encode_call_data(Aci, Arg4),
                            [ MkCallF(Deployer, 2, 0, CallEnabled)
                            , MkCallF(Random, 1, 0, CallRestrictedAddress)
                            , MkCallF(Deployer, 3, Fee, CallDepositStake)
                            , MkCallF(Random, 2, 0, Call4)
                            ];
                        (9) ->
                            Arg1 = lists:flatten(io_lib:format("staked_tokens(~s)", [aeser_api_encoder:encode(account_pubkey, RandomPubKey)])),
                            {ok, Call1} = aeaci_aci:encode_call_data(Aci, Arg1),
                            [ MkCallF(Deployer, 4, 0, Call1)
                            , MkCallF(Random, 3, Fee, CallDepositStake)
                            , MkCallF(Deployer, 5, 0, Call1)
                            , MkCallF(Deployer, 6, 0, CallProtocolEnable)
                            , MkCallF(Random, 4, 0, CallProtocolEnable)
                            ];
                        (10) -> []; %% HC should activate here
                        %% Make some stateful and stateless staking contract calls AFTER the switchover
                        (12) ->
                            [ MkCallF(Deployer, 7, Fee, CallDepositStake)
                            , MkCallF(Random, 5, Fee, CallDepositStake)
                            , MkCallF(Deployer, 8, 0, CallEnabled)
                            ];
                        (16) ->
                            [ MkCallF(Deployer, 9, 0, CallEnabled)
                            , MkCallF(Random, 6, 0, CallEnabled)
                            , MkCallF(Deployer, 10, 0, CallProtocolEnable)
                            , MkCallF(Random, 7, 0, CallProtocolEnable)
                            ];
                        (_) -> []
                    end,
                %% Ensure that both old nodes and new nodes can cooperate until the time comes
                Targets = [?GENESIS_TARGET || _ <- lists:seq(1,18)],
                ExpectedChain = try
                    aehc_test_utils:enable_pow_cuckoo_from_genesis(),
                    false = aehc_utils:hc_enabled(),
                    aec_test_utils:blocks_only_chain(aehc_test_utils:gen_block_chain_with_state(Targets, TxFuns))
                after
                    aehc_test_utils:enable_consensus(aehc_test_utils:pow_to_hc_switch(10))
                end,
                true = aehc_utils:hc_enabled(),
                [_ | Timestamps] = [aec_blocks:time_in_msecs(B) || B <- ExpectedChain, key =:= aec_blocks:type(B)],
                try
                    error = aehc_consensus_hyperchains:get_hc_activation_criteria(),
                    aehc_consensus_hyperchains:set_predeploy_address(ContractAddress),
                    %% Check compatibility with plain PoW
                    ensure_same_chains(ExpectedChain,
                        aec_test_utils:blocks_only_chain(aehc_test_utils:gen_block_chain_with_state(Targets, TxFuns, Timestamps))),
                    %% Insert the generated chain to the DB and verify call results along the way
                    %% TODO: Either make more assertions at height 5 or use lists:sublist to make this match cleaner...
                    %%       Right now just match the entire chain...
                    [ _GB
                    , B1, B2, B3, B4, B5
                    , M1, M2 %% Funding
                    , B6
                    , M3 %% Staking contract creation
                    , B7, B8
                    , M4, M5, M6, M7 %% Some static calls + Deployer deposits funds
                    , B9
                    , M8, M9, M10, M11, M12 %% More static calls + Random deposits funds + attempt to call a system function
                    , B10 %% Switchover block! HC nodes will now "pretend" to be PoW nodes ;)
                    , B11, B12
                    , M13, M14, M15 %% Check enabled state and deposit more funds
                    , B13, B14, B15, B16
                    , M16, M17, M18, M19 %% Try enabling the protocol by an ordinary user, stateless calls
                    , B17, B18 %% Just to ensure everything is fine
                    ] = ExpectedChain,
                    8 = aec_blocks:height(B8),
                    %% Check the first batch of calls pre switchover
                    insert_blocks([B1, B2, B3, B4, B5, M1, M2, B6, M3, B7, B8, M4, M5, M6, M7]),
                    ?assertMatch({ok, false}, CallResF(DeployerPubKey, 2)), %% enabled()
                    ?assertMatch({ok, {tuple, {}}}, CallResF(DeployerPubKey, 3)),  %% deposit_stake()
                    ?assertMatch({ok, {address, <<2:32/unit:8>>}}, CallResF(RandomPubKey, 1)),  %% restricted_address()
                    ?assertMatch({ok, Fee}, CallResF(RandomPubKey, 2)),  %% staked_tokens(DeployerPubKey)
                    %% Check the second batch of calls pre switchover
                    insert_blocks([B9, M8, M9, M10, M11, M12]),
                    ?assertMatch({ok, 0}, CallResF(DeployerPubKey, 4)), %% staked_tokens(RandomPubKey)
                    ?assertMatch({ok, {tuple, {}}}, CallResF(RandomPubKey, 3)),  %% deposit_stake()
                    ?assertMatch({ok, Fee}, CallResF(DeployerPubKey, 5)), %% staked_tokens(RandomPubKey)
                    ?assertMatch({error,{revert,revert}}, CallResF(DeployerPubKey, 6)), %% protocol_enable()
                    ?assertMatch({error,{revert,revert}}, CallResF(RandomPubKey, 4)), %% protocol_enable()
                    %% Now we made the switchover and consensus features should be available
                    insert_blocks([B10]),
                    {ok, false} = aehc_consensus_hyperchains:static_staking_contract_call_on_top_block("enabled()"),
                    {ok, Balance1} = aehc_consensus_hyperchains:static_staking_contract_call_on_top_block("balance()"),
                    ?assertMatch(Balance1, 2 * Fee),
                    %% Time to make deposits after the switchover but before protocol activation
                    insert_blocks([B11, B12, M13, M14, M15]),
                    ?assertMatch({ok, {tuple, {}}}, CallResF(DeployerPubKey, 7)),  %% deposit_stake()
                    ?assertMatch({ok, false}, CallResF(DeployerPubKey, 8)), %% enabled()
                    ?assertMatch({ok, {tuple, {}}}, CallResF(RandomPubKey, 5)),  %% deposit_stake()
                    %% Do some more calls
                    insert_blocks([B13, B14, B15, B16, M16, M17, M18, M19]),
                    ?assertMatch({ok, false}, CallResF(DeployerPubKey, 9)), %% enabled()
                    ?assertMatch({ok, false}, CallResF(RandomPubKey, 6)), %% enabled()
                    ?assertMatch({error,{revert,revert}}, CallResF(DeployerPubKey, 10)), %% protocol_enable()
                    ?assertMatch({error,{revert,revert}}, CallResF(RandomPubKey, 7)), %% protocol_enable()
                    %% And check the end state
                    insert_blocks([B17, B18]),
                    {ok, false} = aehc_consensus_hyperchains:static_staking_contract_call_on_top_block("enabled()"),
                    {ok, Balance2} = aehc_consensus_hyperchains:static_staking_contract_call_on_top_block("balance()"),
                    ?assertMatch(Balance2, 4 * Fee)
                after
                    aehc_consensus_hyperchains:unset_predeploy_address()
                end,
                ok
            end}
        ])).

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

ensure_same_chains([], []) -> [];
ensure_same_chains([], _) -> error("Different number of blocks");
ensure_same_chains(H, []) -> ensure_same_chains([], H);
ensure_same_chains([B1|C1], [B2|C2]) ->
    NormF = fun(B) -> aec_blocks:strip_extra(B) end,
    ?assertEqual(NormF(B1), NormF(B2), "Chains diverged"),
    ensure_same_chains(C1, C2).

%% ------------------------------------------------------------------
%% Helpers

insert_blocks([]) -> ok;
insert_blocks([H|T]) ->
    {ok, _} = aec_chain_state:insert_block(H),
    insert_blocks(T).

min_gas_price() ->
    aec_test_utils:min_gas_price() * 1000000000.

make_spend_tx(Sender, SenderNonce, Recipient, Amount) ->
    SenderId = aeser_id:create(account, Sender),
    RecipientId = aeser_id:create(account, Recipient),
    {ok, SpendTx} = aec_spend_tx:new(#{sender_id => SenderId,
                                       recipient_id => RecipientId,
                                       amount => Amount,
                                       fee => 20000 * min_gas_price(),
                                       nonce => SenderNonce,
                                       payload => <<>>}),
    SpendTx.

staking_contract_vm() -> ?VM_FATE_SOPHIA_2.
staking_contract_abi() -> ?ABI_FATE_SOPHIA_1.

make_contract_create_tx(Pubkey, Code, CallData, Nonce, Deposit, Amount, Fee,
                        Gas, GasPrice) ->
    OwnerId = aeser_id:create(account, Pubkey),
    {ok, Tx} = aect_create_tx:new(#{owner_id   => OwnerId,
                                    nonce      => Nonce,
                                    code       => Code,
                                    abi_version => staking_contract_abi(),
                                    vm_version => staking_contract_vm(),
                                    deposit    => Deposit,
                                    amount     => Amount,
                                    gas        => Gas,
                                    gas_price  => GasPrice,
                                    call_data  => CallData,
                                    fee        => Fee}),
    Tx.

make_contract_call_tx(Pubkey, ContractPubkey, CallData, Nonce, Amount, Fee,
                        Gas, GasPrice) ->
    {ok, Tx} = aect_call_tx:new(#{ caller_id   => aeser_id:create(account, Pubkey)
                                 , nonce       => Nonce
                                 , contract_id => aeser_id:create(contract, ContractPubkey)
                                 , abi_version => staking_contract_abi()
                                 , fee         => Fee
                                 , amount      => Amount
                                 , gas         => Gas
                                 , gas_price   => GasPrice
                                 , call_data   => CallData
                                 }),
    Tx.
