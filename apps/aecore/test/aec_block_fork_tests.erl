-module(aec_block_fork_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("aecontract/include/aecontract.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================
-define(UAT_ROOT_HASH, "0BA96447352BA2C14D1F9AD03D6085A270FDC49704B7D2A190C60161B54207BF").
-define(MAIN_ROOT_HASH, "E4DBC69BF2783B81B0423DA3F5B684C1D37CCFAE798474525C4001DB42C67669").

-define(UAT_TOKENS, 2448618414302482322).
-define(MAIN_TOKENS, 29622067581238053773524138).

apply_minerva_test_() ->
    [{foreach,
      fun() ->
              meck:new(aec_fork_block_settings, [passthrough]),
              ok
      end,
      fun(ok) ->
              meck:unload(aec_fork_block_settings),
              ok
      end,
      [ {"Empty minerva migration does not change balances",
         fun() ->
                 InitialAccounts =
                     [{Alice, BalA},
                      {Bob,   BalB},
                      {Carol, BalC}] = generate_accounts(3),
                 T0 = make_trees(InitialAccounts), % Alice, Bob and Carol
                 meck_minerva_accounts([]), % no accounts migrated
                 T1 = aec_block_fork:apply_minerva(T0),
                 ?assertEqual(aec_trees:hash(T0), aec_trees:hash(T1)),
                 assert_balance(T1, Alice, BalA),
                 assert_balance(T1, Bob,   BalB),
                 assert_balance(T1, Carol, BalC),
                 ok
         end},
        {"Minerva migration changes balances",
         fun() ->
                 [{Alice,  BalA} = A,
                  {Bob,   BalB} = B,
                  {Carol, BalC},
                  {David, _}] = generate_accounts(4),
                 T0 = make_trees([A, B]), % only Alice and Bob are present pre-minerva
                 meck_minerva_accounts([{Alice, DeltaA = 10}, % Alice had migrated more
                                        {Carol, BalC}, % Carol is new
                                        {David, 0}]), % David has a balance of 0
                 T1 = aec_block_fork:apply_minerva(T0),
                 assert_only_accounts_tree_changed(T0, T1),
                 assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
                 assert_balance(T1, Bob,   BalB), % Bob is unchanged
                 assert_balance(T1, Carol, BalC), % Carol is inserted
                 assert_balance(T1, David, 0),    % David is present
                 ok
         end}]}
     || aect_test_utils:latest_protocol_version() >= ?MINERVA_PROTOCOL_VSN ].

apply_lima_test_() ->
    [{foreach,
      fun() ->
              meck:new(aec_fork_block_settings, [passthrough]),
              ok
      end,
      fun(ok) ->
              meck:unload(aec_fork_block_settings),
              ok
      end,
      [ {"Lima migration changes balances if no contracts are given",
         fun() ->
                 [{Alice,  BalA} = A,
                  {Bob,   BalB} = B,
                  {Carol, BalC},
                  {Extra, BalE},
                  {David, _}] = generate_accounts(5),
                 T0 = make_trees([A, B]), % only Alice and Bob are present pre-minerva
                 meck_lima_accounts_extra_accounts_and_contracts([{Alice, DeltaA = 10},%% Alice had migrated more
                                                                  {Carol, BalC},       %% Carol is new
                                                                  {David, 0}],         %% David has a balance of 0
                                                                 [{Extra, BalE}],      %% Extra accounts
                                                                 []                    %% No contracts
                                                                ),
                 T1 = aec_block_fork:apply_lima(T0, tx_env()),
                 assert_only_accounts_tree_changed(T0, T1),
                 assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
                 assert_balance(T1, Bob,   BalB), % Bob is unchanged
                 assert_balance(T1, Carol, BalC), % Carol is inserted
                 assert_balance(T1, David, 0),    % David is present
                 assert_balance(T1, Extra, BalE), % Check the extra_accounts.
                 ok
         end},
        {"Lima migration changes balances when contracts _are_ given",
         fun() ->
                 [{Alice,  BalA} = A,
                  {Bob,   BalB} = B,
                  {Carol, BalC},
                  {Extra, BalE},
                  {David, _}] = generate_accounts(5),
                 T0 = make_trees([A, B]), % only Alice and Bob are present pre-minerva
                 ContractAmount1 = 10000,
                 ContractAmount2 = 40000,
                 ContractSpecs = [lima_contract(2, ContractAmount2),
                                  lima_contract(1, ContractAmount1)
                                 ],
                 meck_lima_accounts_extra_accounts_and_contracts([{Alice, DeltaA = 10},%% Alice had migrated more
                                                                  {Carol, BalC},       %% Carol is new
                                                                  {David, 0}],         %% David has a balance of 0
                                                                 [{Extra, BalE}],      %% Extra accounts
                                                                 ContractSpecs
                                                                ),
                 T1 = aec_block_fork:apply_lima(T0, tx_env()),
                 assert_only_accounts_and_contracts_trees_changed(T0, T1),
                 assert_balance(T1, Alice, BalA + DeltaA), % Alice balance is increased
                 assert_balance(T1, Bob,   BalB), % Bob is unchanged
                 assert_balance(T1, Carol, BalC), % Carol is inserted
                 assert_balance(T1, David, 0),    % David is present
                 assert_balance(T1, Extra, BalE), % Check the extra_accounts.
                 %% Make sure the locked account balance didn't change.
                 LockedAccount = aec_governance:locked_coins_holder_account(),
                 assert_balance(T0, LockedAccount, 0),
                 assert_balance(T1, LockedAccount, 0),
                 Contracts0 = aec_trees:contracts(T0),
                 Contracts1 = aec_trees:contracts(T1),
                 %% Make sure the contracts didn't exist before
                 [?assert(not aect_state_tree:is_contract(maps:get(pubkey, S), Contracts0))
                  || S <- ContractSpecs],
                 %% Make sure the contracts now are present and have the right amount
                 [begin
                      PK = maps:get(pubkey, S),
                      ?assert(aect_state_tree:is_contract(PK, Contracts1)),
                      Bal = maps:get(amount, S),
                      assert_balance(T1, PK, Bal)
                  end || S <- ContractSpecs],
                 %% Make sure the total coin amount increased properly.
                 Sums0 = aec_trees:sum_total_coin(T0),
                 Sums1 = aec_trees:sum_total_coin(T1),
                 ExpectedContractDelta = ContractAmount1 + ContractAmount2,
                 ExpectedTotalDelta = ExpectedContractDelta + DeltaA + BalC + BalE,
                 Total0 = maps:fold(fun(_, X, Acc) -> Acc + X end, 0, Sums0),
                 Total1 = maps:fold(fun(_, X, Acc) -> Acc + X end, 0, Sums1),
                 ?assertEqual(Total0 + ExpectedTotalDelta,
                              Total1),
                 ?assertEqual(maps:get(contracts, Sums0) + ExpectedContractDelta,
                              maps:get(contracts, Sums1)),
                 ok
         end},
        {"Lima migration tokens",
         fun() ->
                 CodeDir = filename:join(code:lib_dir(aecontract), "../../extras/test/"),

                 UATTokens = mtree:total_sum_from_json(CodeDir ++ "/json/uat_contracts_accounts.json"),
                 ?assertEqual(?UAT_TOKENS, UATTokens),

                 MainTokens = mtree:total_sum_from_json(CodeDir ++ "/json/contracts_accounts.json"),
                 ?assertEqual(?MAIN_TOKENS, MainTokens),

                 ok
         end},
        {"Lima migration root-hashes",
         fun() ->
                 CodeDir = filename:join(code:lib_dir(aecontract), "../../extras/test/"),

                 %% We got the data unsorted for some unit tests - so stick with that...
                 UATTree = mtree:tree_from_json(CodeDir ++ "/json/uat_contracts_accounts.json", [no_sort]),
                 ?assertEqual(?UAT_ROOT_HASH, mtree:root_hash(UATTree)),

                 MainTree = mtree:tree_from_json(CodeDir ++ "/json/contracts_accounts.json"),
                 ?assertEqual(?MAIN_ROOT_HASH, mtree:root_hash(MainTree)),

                 ok
         end},
        {"Lima migration using real contract and local data",
         fun() ->
                 lima_migration_test(mock)
         end},
        {"Lima migration using real contract - from UAT-JSON",
         fun() ->
                 lima_migration_test(json)
         end},
        {"Lima migration testing real contract - from Mainnet-JSON",
         fun() ->
                 ContractId = <<"ct_eJhrbPPS4V97VLKEVbSCJFpdA4uyXiZujQyLqMFoYV88TzDe6">>,

                 [{Account, _}] = generate_accounts(1),
                 T0 = make_trees([{Account, 1000000000000000000000000}]),
                 ContractSpecs = lima_contract_json("contracts.json"),
                 meck_lima_accounts_extra_accounts_and_contracts([], [], ContractSpecs),
                 T1 = aec_block_fork:apply_lima(T0, tx_env()),

                 %% Before any account migrated, the contract should have all the tokens
                 assert_balance(T1, contract_pubkey(1), ?MAIN_TOKENS),

                 {ok, Tx} = root_hash_tx(Account, 1),
                 STx = aetx_sign:new(Tx, [<<0:64/unit:8>>]),
                 {ok, [STx], [], T2, _} =
                     aec_trees:apply_txs_on_state_trees([STx], T1, tx_env(),[strict, dont_verify_signature]),
                 {ok, ContractKey} = aeser_api_encoder:safe_decode(contract_pubkey, ContractId),

                 CallId = aect_call:id(Account, 1, ContractKey),
                 Call   = aect_call_state_tree:get_call(ContractKey, CallId, aec_trees:calls(T2)),
                 ok     = aect_call:return_type(Call),
                 ?assertEqual(?MAIN_ROOT_HASH,
                              binary_to_list(aeb_fate_encoding:deserialize(aect_call:return_value(Call)))),
                 ok
         end}
      ]}
     || aect_test_utils:latest_protocol_version() == ?LIMA_PROTOCOL_VSN ].

tx_env() ->
    aetx_env:tx_env(42).

lima_migration_test(Source) ->
     AccountSpecs = migration_account_specs(),
     [{Account, _}] = generate_accounts(1),
     T0 = make_trees([{Account, 1000000000000000000000000}]),
     CNonce = 1,
     CAmount = lists:sum([A || #{amount := A} <- AccountSpecs]),
     ContractSpecs = case Source of
                         mock -> [lima_contract(CNonce, CAmount)];
                         json -> lima_contract_json("contracts_uat.json")
                     end,
     meck_lima_accounts_extra_accounts_and_contracts([], [], ContractSpecs),
     T1 = aec_block_fork:apply_lima(T0, tx_env()),
     {Txs, _} = lists:mapfoldl(
                  fun(Spec, Nonce) ->
                          {ok, Tx} = migrate_tx(Account, Nonce, Spec),
                          %% Fake signatures that won't be checked
                          {aetx_sign:new(Tx, [<<0:64/unit:8>>]),
                           Nonce + 1}
                  end, 1, AccountSpecs),
     %% Migrate all accounts in one go
     {ok, Txs, [], T2, _} =
         aec_trees:apply_txs_on_state_trees(Txs, T1, tx_env(),[strict, dont_verify_signature]),
     CPubkey = contract_pubkey(CNonce),
     %% Before any account migrated, the contract should have all the tokens
     assert_balance(T1, CPubkey, CAmount),
     %% After all accounts migrated, the contract should have nothing left
     assert_balance(T2, CPubkey, 0),
     %% Check all individual balances
     lists:foreach(fun(#{ ae_pubkey := AEPubkey
                        , amount    := AEAmount}) ->
                           assert_balance(T2, AEPubkey, AEAmount)
                   end, AccountSpecs),
     ok.

lima_contract(Nonce, Amount) ->
    put('$abi_version', ?ABI_FATE_SOPHIA_1),
    {ok, Code}     = aect_test_utils:compile_contract(?SOPHIA_LIMA_FATE, token_migration),
    {ok, Contract} = aect_test_utils:read_contract(?SOPHIA_LIMA_FATE, token_migration),
    RootHash       = "\"" ++ ?UAT_ROOT_HASH ++ "\"",
    {ok, CallData} = aect_test_utils:encode_call_data(?SOPHIA_LIMA_FATE, Contract,
                                                      <<"init">>,
                                                      [RootHash, "0"]),
    Owner          = aec_governance:locked_coins_holder_account(),
    Pubkey         = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    #{ amount      => Amount
     , vm_version  => ?VM_FATE_SOPHIA_1
     , abi_version => ?ABI_FATE_SOPHIA_1
     , code        => Code
     , call_data   => CallData
     , pubkey      => Pubkey
     , nonce       => Nonce
     }.

lima_contract_json(File) ->
    {ok, WorkDir} = file:get_cwd(),
    DataAecoreDir = WorkDir ++ "/data/aecore/",
    meck:expect(aec_fork_block_settings, contracts_file_name,
                fun(_) -> DataAecoreDir ++ ".lima/" ++ File end),

    Res = aec_fork_block_settings:lima_contracts(),

    meck:expect(aec_fork_block_settings, contracts_file_name,
                fun(Name) -> meck:passthrough([Name]) end),

    Res.

load_files_smoke_test_() ->
    [{foreach,
     fun() ->
         meck:new(aec_fork_block_settings, [passthrough]),
         {ok, WorkDir} = file:get_cwd(),
         DataAecoreDir =  WorkDir ++ "/data/aecore/",
         meck:expect(aec_fork_block_settings, accounts_file_name,
            fun(?ROMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".genesis/" ++ AFile;
                (?MINERVA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".minerva/" ++ AFile;
                (?FORTUNA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".fortuna/" ++ AFile;
                (?LIMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".lima/" ++ AFile
            end),
         meck:expect(aec_fork_block_settings, contracts_file_name,
            fun(?LIMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".lima/" ++ CFile
            end),
         meck:expect(aec_fork_block_settings, extra_accounts_file_name,
            fun(?LIMA_PROTOCOL_VSN) ->
                    DataAecoreDir ++ ".lima/" ++ AIFile
            end),
         ok
     end,
     fun(ok) ->
         meck:unload(aec_fork_block_settings),
         ok
     end,
     [ {"Load the real account and contract files",
        fun() ->
            T0 = make_trees(aec_fork_block_settings:genesis_accounts()),
            T1 = aec_block_fork:apply_minerva(T0),
            T2 = aec_block_fork:apply_fortuna(T1),
            case aect_test_utils:latest_protocol_version() == ?LIMA_PROTOCOL_VSN of
                true -> _T3 = aec_block_fork:apply_lima(T2, tx_env());
                false -> ok
            end,
            ok
        end}
     ]} || {AFile, CFile, AIFile} <- [{"accounts.json", "contracts.json", "extra_accounts.json"},
                                      {"accounts_uat.json", "contracts_uat.json", "extra_accounts_uat.json"},
                                      {"accounts_test.json", "contracts_test.json", "extra_accounts_test.json"}]
    ].

meck_minerva_accounts(AccountsList) ->
    meck:expect(aec_fork_block_settings, minerva_accounts,
                fun() -> AccountsList end).

meck_lima_accounts_extra_accounts_and_contracts(AccountsList, ExtraAccountsList, ContractsList) ->
    meck:expect(aec_fork_block_settings, lima_accounts,
                fun() -> AccountsList end),
    meck:expect(aec_fork_block_settings, lima_extra_accounts,
                fun() -> ExtraAccountsList end),
    meck:expect(aec_fork_block_settings, lima_contracts,
                fun() -> ContractsList end).



assert_only_accounts_tree_changed(Trees0, Trees1) ->
    ?assertEqual(calls_hash(Trees0)     , calls_hash(Trees1)),
    ?assertEqual(channels_hash(Trees0)  , channels_hash(Trees1)),
    ?assertEqual(contracts_hash(Trees0) , contracts_hash(Trees1)),
    ?assertEqual(names_hash(Trees0)     , names_hash(Trees1)),
    ?assertEqual(oracles_hash(Trees0)   , oracles_hash(Trees1)),

    ?assertNotEqual(accounts_hash(Trees0), accounts_hash(Trees1)),
    ok.

assert_only_accounts_and_contracts_trees_changed(Trees0, Trees1) ->
    ?assertEqual(calls_hash(Trees0)     , calls_hash(Trees1)),
    ?assertEqual(channels_hash(Trees0)  , channels_hash(Trees1)),
    ?assertEqual(names_hash(Trees0)     , names_hash(Trees1)),
    ?assertEqual(oracles_hash(Trees0)   , oracles_hash(Trees1)),

    ?assertNotEqual(accounts_hash(Trees0) , accounts_hash(Trees1)),
    ?assertNotEqual(contracts_hash(Trees0), contracts_hash(Trees1)),
    ok.


names_hash(Trees) ->
    aens_state_tree:root_hash(aec_trees:ns(Trees)).

channels_hash(Trees) ->
    aesc_state_tree:root_hash(aec_trees:channels(Trees)).

oracles_hash(Trees) ->
    aeo_state_tree:root_hash(aec_trees:oracles(Trees)).

calls_hash(Trees) ->
    aect_call_state_tree:root_hash(aec_trees:calls(Trees)).

contracts_hash(Trees) ->
    aect_state_tree:root_hash(aec_trees:contracts(Trees)).

accounts_hash(Trees) ->
    aec_accounts_trees:root_hash(aec_trees:accounts(Trees)).

make_trees(AccountsList) ->
    Trees = aec_trees:new_without_backend(),
    AccTrees =
        lists:foldl(
            fun({Pubkey, Balance}, AccumAccTrees) when Balance >= 0 ->
                Account = aec_accounts:new(Pubkey, Balance),
                aec_accounts_trees:enter(Account, AccumAccTrees)
            end,
            aec_trees:accounts(Trees),
            AccountsList ++ [{aec_governance:locked_coins_holder_account(), 0}]),
    aec_trees:set_accounts(Trees, AccTrees).

generate_accounts(Count) ->
    generate_accounts(Count, []).

generate_accounts(Count, Accum) when Count < 1 ->
    Accum;
generate_accounts(Count, Accum) ->
    Pubkey = <<Count:32/unit:8>>,
    generate_accounts(Count - 1 ,[{Pubkey, Count} | Accum]).


assert_balance(Trees, Pubkey, Balance) ->
    Accounts = aec_trees:accounts(Trees),
    Account = aec_accounts_trees:get(Pubkey, Accounts),
    ?assertEqual(Balance, aec_accounts:balance(Account)).

migrate_tx(Account, Nonce, #{receiver  := Receiver,
                             amount    := Amount,
                             index     := Index,
                             siblings  := Siblings,
                             signature := Signature}) ->
    put('$abi_version', ?ABI_FATE_SOPHIA_1),
    {ok, Contract} = aect_test_utils:read_contract(?SOPHIA_LIMA_FATE, token_migration),
    Args = [integer_to_list(Amount),
            Receiver,
            integer_to_list(Index),
            iolist_to_binary([$[, lists:join(",", [[$\", S, $\"] || S <- Siblings ]), $]]),
            iolist_to_binary([$#, Signature])],
    {ok, CallData} = aect_test_utils:encode_call_data(?SOPHIA_LIMA_FATE, Contract, <<"migrate">>, Args),
    aect_call_tx:new(#{caller_id   => aeser_id:create(account, Account),
                       nonce       => Nonce,
                       contract_id => contract_id(1),
                       abi_version => ?ABI_FATE_SOPHIA_1,
                       fee         => 1000000000000000,
                       amount      => 0,
                       gas         => aec_governance:block_gas_limit() div 2,
                       gas_price   => aec_governance:minimum_gas_price(?LIMA_PROTOCOL_VSN),
                       call_data   => CallData}).

root_hash_tx(Account, Nonce) ->
    put('$abi_version', ?ABI_FATE_SOPHIA_1),
    {ok, Contract} = aect_test_utils:read_contract(?SOPHIA_LIMA_FATE, token_migration),
    {ok, CallData} = aect_test_utils:encode_call_data(?SOPHIA_LIMA_FATE, Contract, <<"root_hash">>, []),
    aect_call_tx:new(#{caller_id   => aeser_id:create(account, Account),
                       nonce       => Nonce,
                       contract_id => contract_id(1),
                       abi_version => ?ABI_FATE_SOPHIA_1,
                       fee         => 1000000000000000,
                       amount      => 0,
                       gas         => 10000,
                       gas_price   => aec_governance:minimum_gas_price(?LIMA_PROTOCOL_VSN),
                       call_data   => CallData}).



contract_id(N) ->
    CPubkey = contract_pubkey(N),
    aeser_id:create(contract, CPubkey).

contract_pubkey(N) ->
    aect_contracts:compute_contract_pubkey(aec_governance:locked_coins_holder_account(), N).

eth_sign("0x" ++ Sender, "0x" ++ PrivKey, Receiver) ->
    Size      = integer_to_list(byte_size(Receiver)),
    Msg       = ["\x19Ethereum Signed Message:\n", Size, Receiver],
    MsgHash   = aec_hash:hash(evm, iolist_to_binary(Msg)),
    BinKey    = aeu_hex:hex_to_bin(PrivKey),
    BinSender = aeu_hex:hex_to_bin(Sender),
    DerSig    = crypto:sign(ecdsa, sha256, {digest, MsgHash}, [BinKey, secp256k1]),
    ShortSig  = aeu_crypto:ecdsa_from_der_sig(DerSig),
    BinSig    = aeu_crypto:ecdsa_recoverable_from_ecdsa(MsgHash, ShortSig, BinSender),
    aeu_hex:bin_to_hex(BinSig).

migration_account_specs() ->
  [ begin
      AePubkey = aec_hash:sha256_hash(iolist_to_binary([Sender, PrivKey])),
      Receiver = aeser_api_encoder:encode(account_pubkey, AePubkey),
      S#{signature => eth_sign(Sender, PrivKey, Receiver),
         ae_pubkey => AePubkey,
         receiver  => Receiver
        }
    end
    || S = #{sender   := Sender,
             priv_key := PrivKey} <- migration_account_specs_()].

migration_account_specs_() ->
    [
     #{ sender => "0x015ae5A39E9811875D1e57e4E6e7b3ed83b97a4d",
        priv_key => "0xd6a5e842e7410a52e0b5346c5f906052e236ce66744e9da827df43249b25414a",
        index    => 0,
        amount   => 1000000000000000000,
        siblings => ["620DF112731586BF723BD9E479FFAE60CF616617583DAD0651E21EF384C2CD77",
                     "079DF4A87160F73777A4C04EEA17F0130B5EF15F299DD102F85CBA3F0FDF67C1",
                     "BAE2CE2392924336CDA7C17E0E24E242C73635E6E57566679BC41926A9E44F17",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"]
      },
     #{ sender => "0xe39A80679c3aF15e207378D7Ab65d931ebf26253",
        priv_key => "0x398eddb094f9bfe2f579538208142df7fced2d1fe2dc79ee3ed66e84203cdd13",
        index => 1,
        amount => 123573389000000000,
        siblings => ["959EC57E848ED968C347B3623EE2060FAAFF980B72BB2FC6E2049AED49D0172D",
                     "079DF4A87160F73777A4C04EEA17F0130B5EF15F299DD102F85CBA3F0FDF67C1",
                     "BAE2CE2392924336CDA7C17E0E24E242C73635E6E57566679BC41926A9E44F17",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"]
        },
     #{ sender => "0xdCfea837b9C9eFDaD2E7aF27179e09619fb982d0",
        priv_key => "0x706aad5ca7cf2591d2c1f2073eb0535daa6f5a9fe5f32c48cc43e9ef39143786",
        index => 2,
        amount => 2045020302482322,
        siblings => ["2B5E3E76C36ADB712752C184A0816ACDD7768B1046EC630A2654D34F5E6D85C5",
                     "4498EF836D51BB97571F64661901B347ACDB7DDBD1535565611A3EE69CA23FBC",
                     "BAE2CE2392924336CDA7C17E0E24E242C73635E6E57566679BC41926A9E44F17",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"
                    ]
      },
     #{ sender => "0x03B9bCaf77B0B48618cDD61d86886C3e85f2a352",
        priv_key => "0x2f91c22a3b457097c2245c949ef9194971543d891522e93d3e1bcc8086c94e79",
        index => 3,
        amount => 40000000000000000,
        siblings => ["DC2095A482B1ABE7433CBD02D60AF71F9E76460A69B84A9B30FC0AA08E23F850",
                     "4498EF836D51BB97571F64661901B347ACDB7DDBD1535565611A3EE69CA23FBC",
                     "BAE2CE2392924336CDA7C17E0E24E242C73635E6E57566679BC41926A9E44F17",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"
                    ]
      },
     #{ sender => "0x5a0431783387718957A54D38aad31bA8D1404833",
        priv_key => "0xdd4cfa444e085495353faf88e59e6a9cfc05720600d29a6e8c121bd5e4fedcc0",
        index => 4,
        amount => 50000000000000000,
        siblings => ["2E70DB0466A0F42900F2E71C4E6558F0A8EB32528A39A5ADC56282731E684E8E",
                     "0DF11527FA456E1BF4CC24120831BD70CCCAB387A082FB48DE82F9213C7A7BFA",
                     "7FC35778A2F455C631C954BC4C8DA27C90630182DEC4BDB8030CA847F96A2C3B",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"
                    ]
      },
     #{ sender => "0x025b8302F4C7Cf2D52966104Ada6F07FC31f91C3",
        priv_key => "0xc49bddc8d1765316ab42fb9b690d0a6bead860f046ef0684b610c9ac2595e3b7",
        index => 5,
        amount => 60000000000000000,
        siblings => ["D83FD07EDB12AEAFC0E885CE331EE1B7B760113CCC0AAE4B7D4A75FE577686E3",
                     "0DF11527FA456E1BF4CC24120831BD70CCCAB387A082FB48DE82F9213C7A7BFA",
                     "7FC35778A2F455C631C954BC4C8DA27C90630182DEC4BDB8030CA847F96A2C3B",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"
                    ]
      },
     #{ sender => "0xbbaAabe6F5abEE05380413420dAd35f32c1D683f",
        priv_key => "0x0ffb86dd9d3270efc14ff43578a9f79f4c500f866bf6d2d2b76da2637d8494de",
        index => 6,
        amount => 70000000000000000,
        siblings => ["CD602824814BEDADD95964ED28247D96A5146614F46F88CD86AD306FBC498D10",
                     "4ECC55D2DA5912AB924CA1FA9035C2C24CEA3F6E4EE5015AF30132419111E0C1",
                     "7FC35778A2F455C631C954BC4C8DA27C90630182DEC4BDB8030CA847F96A2C3B",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"
                    ]
      },
     #{ sender => "0x201ac57463b9a6cD4485F75d8eDdFA60f9BAa65a",
        priv_key => "0xd30dd1144062a776b4348280b243587d6a61ca464b13e9afd0caf851cbfd37da",
        index => 7,
        amount => 80000000000000000,
        siblings => ["8511AB803305E9534EED1CCEB057C8924C7A570393B39FB000952E85B19F77C2",
                     "4ECC55D2DA5912AB924CA1FA9035C2C24CEA3F6E4EE5015AF30132419111E0C1",
                     "7FC35778A2F455C631C954BC4C8DA27C90630182DEC4BDB8030CA847F96A2C3B",
                     "C415147EE0E4439A1913F74DE54B1EAF6A63F1FD6D041E76C529C22DA6EBCD30"
                    ]
      },
     #{ sender => "0xC1d14222B373C3FaC50C556880DDAb260e711EF8",
        priv_key => "0x13a145b48179e7603504f82f53fbf0710347ae1fedce541f57939d5815c20406",
        index => 8,
        amount => 900000000000000000,
        siblings => ["FE978BB8893D4064A8DEAB694A6B185BEAB82254FBD5AA589114CBF8B55BB166",
                     "3A0C5760330AA8077F15EDD11CED64D900BA0CFDEB0D0F41792ACEFAC131A1F8",
                     "22BD952B1DAE145C4BC70CED7F4128A524E5A747C38880E09982543034AE9870",
                     "87744BE3E1904920374CFDD5783E55C8057F5AE40B1533557949E551A939CA5C"
                    ]
      },
     #{ sender => "0x42a375e8e91Bdb621996950893161015cA9cCAca",
        priv_key => "0xc63610d14407775346ddb68bb25e797d615101e0fcb4e0b6143f7cfa1ffa2817",
        index => 9,
        amount => 123000005000000000,
        siblings => ["C0E059E7F7F531B9786ABC366E82BD17DBA4B9A377355896775D0F95DF8DC2C7",
                     "3A0C5760330AA8077F15EDD11CED64D900BA0CFDEB0D0F41792ACEFAC131A1F8",
                     "22BD952B1DAE145C4BC70CED7F4128A524E5A747C38880E09982543034AE9870",
                     "87744BE3E1904920374CFDD5783E55C8057F5AE40B1533557949E551A939CA5C"
                    ]
      }].
