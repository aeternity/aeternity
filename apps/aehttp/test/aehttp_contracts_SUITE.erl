-module(aehttp_contracts_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").
-include_lib("aecore/include/aec_crypto.hrl").
%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% Endpoint calls
-export([]).

%% test case exports
%% external endpoints
-export([
	 spending_1/1,
	 spending_2/1,
	 spending_3/1,
	 identity_contract_1/1,
	 identity_contract_2/1,
	 simple_storage/1,
	 dutch_auction_contract_1/1,
	 dutch_auction_contract_2/1,
	 null/1
	]).

-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).

%% Contract return values for some predefined numbers. Conversion
%% functions will make this unnecessary.

-define(VALUE_STATEP, <<"0x0000000000000000000000000000000000000000000000000000000000000020">>).
-define(VALUE_21, <<"0x0000000000000000000000000000000000000000000000000000000000000015">>).
-define(VALUE_42, <<"0x000000000000000000000000000000000000000000000000000000000000002A">>).
-define(VALUE_84, <<"0x0000000000000000000000000000000000000000000000000000000000000054">>).
-define(VALUE_126, <<"0x000000000000000000000000000000000000000000000000000000000000007E">>).
-define(VALUE_168, <<"0x00000000000000000000000000000000000000000000000000000000000000A8">>).

all() ->
    [
     {group, contracts}
    ].

groups() ->
    [
     {contracts, [sequence],
      [
       spending_1,
       %% spending_2,
       spending_3,
       identity_contract_1,
       identity_contract_2,
       simple_storage,
       %% dutch_auction_contract_1,
       %% dutch_auction_contract_2,
       null					%This allows to end with ,
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.http_endpoints"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    Forks = aecore_suite_utils:forks(),

    aecore_suite_utils:create_configs(Config1, #{<<"chain">> =>
                                                 #{<<"persist">> => true,
                                                   <<"hard_forks">> => Forks}}),
    aecore_suite_utils:make_multi(Config1, [?NODE]),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(contracts, Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),

    %% Prepare accounts.
    {APubkey, APrivkey} = generate_key_pair(),
    {BPubkey, BPrivkey} = generate_key_pair(),
    {CPubkey, CPrivkey} = generate_key_pair(),
    {DPubkey, DPrivkey} = generate_key_pair(),
    AStartAmt = 50,				%We don't give them much!
    BStartAmt = 50,
    CStartAmt = 50,
    DStartAmt = 50,
    Fee = 1,
    BlocksToMine = 1,				%Just a few

    %% Mine someblocks
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
				   BlocksToMine),
    %% Add 4 accounts and mine
    {ok, 200, _} = post_spend_tx(APubkey, AStartAmt, Fee),
    {ok, 200, _} = post_spend_tx(BPubkey, BStartAmt, Fee),
    {ok, 200, _} = post_spend_tx(CPubkey, CStartAmt, Fee),
    {ok, 200, _} = post_spend_tx(DPubkey, DStartAmt, Fee),
    {ok, [Block]} = aecore_suite_utils:mine_blocks(NodeName, 1),
    [_Spend1, _Spend2, _Spend3, _Spend4] = aec_blocks:txs(Block),
    assert_balance(APubkey, AStartAmt),
    assert_balance(BPubkey, BStartAmt),
    assert_balance(CPubkey, CStartAmt),
    assert_balance(DPubkey, DStartAmt),
    %% Save account information.
    Accounts = #{acc_a => #{pub_key => APubkey,
			    priv_key => APrivkey,
			    start_amt => AStartAmt},
		 acc_b => #{pub_key => BPubkey,
			    priv_key => BPrivkey,
			    start_amt => BStartAmt},
		 acc_c => #{pub_key => CPubkey,
			    priv_key => CPrivkey,
			    start_amt => CStartAmt},
		 acc_d => #{pub_key => DPubkey,
			    priv_key => DPrivkey,
			    start_amt => DStartAmt}},
    [{accounts,Accounts},{node_name,NodeName}|Config];
init_per_group(_Group, Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),
    ToMine = aecore_suite_utils:latest_fork_height(),
    ct:pal("ToMine ~p\n", [ToMine]),
    aecore_suite_utils:mine_blocks(NodeName, ToMine),
    [{node_name,NodeName}|Config].

end_per_group(all_endpoints, _Config) ->
    ok;
end_per_group(_Group, Config) ->
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    ok.

init_per_testcase(_Case, Config) ->
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% null(Config)
%%  Does nothing an always succeeds.

null(_Config) ->
    ok.

%% spending_1(Config)
%%  A simple test of tokens from acc_a to acc_b.

spending_1(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey}} = proplists:get_value(accounts, Config),

    %% ct:pal("Top 1 ~p\n", [get_top()]),

    %% Check initial balances.
    ABal0 = get_balance(APubkey),
    BBal0 = get_balance(BPubkey),
    ct:pal("Balances 0: ~p, ~p\n", [ABal0,BBal0]),

    %% Add tokens to both accounts.
    {ok,200,_} = post_spend_tx(APubkey, 500, 1),
    {ok,200,_} = post_spend_tx(BPubkey, 500, 1),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Get balances after mining.
    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    ct:pal("Balances 1: ~p, ~p\n", [ABal1,BBal1]),

    %% Transfer money from Alice to Bert.
    TxHash = spend_tokens(APubkey, APrivkey, BPubkey, 200, 5),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),
    
    %% ct:pal("Top 2 ~p\n", [get_top()]),

    %% Check that tx has succeeded.
    ?assert(tx_in_block(TxHash)),

    %% Check balances after sending.
    ABal2 = get_balance(APubkey),
    BBal2 = get_balance(BPubkey),
    ct:pal("Balances 2: ~p, ~p\n", [ABal2,BBal2]),

    %% Check that the balances are correct, don't forget the fee.
    ABal2 = ABal1 - 200 - 5,
    BBal2 = BBal1 + 200,

    ok.

%% spending_2(Config)
%%  A simple test of tokens from acc_a to acc_b. There are not enough
%%  tokens in acc_a so tx suspends until TTL runs out.

spending_2(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey}} = proplists:get_value(accounts, Config),

    %% Check initial balances.
    ABal0 = get_balance(APubkey),
    BBal0 = get_balance(BPubkey),
    ct:pal("Balances 0: ~p, ~p\n", [ABal0,BBal0]),

    %% Add tokens to both accounts.
    %% {ok,200,_} = post_spend_tx(APubkey, 500, 1),
    %% {ok,200,_} = post_spend_tx(BPubkey, 500, 1),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Get balances after mining.
    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    ct:pal("Balances 1: ~p, ~p\n", [ABal1,BBal1]),

    {ok,200,#{<<"height">> := Height}} = get_top(),
    ct:pal("Height ~p\n", [Height]),

    %% Transfer money from Alice to Bert, but more than Alice has.
    TTL =  #{ttl => Height + 2},
    TxHash = spend_tokens(APubkey, APrivkey, BPubkey, ABal1 + 100, 5, TTL),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),
    
    %% Check that tx has failed.
    ct:pal("TxHash1 ~p\n", [tx_in_block(TxHash)]),

    %% Check that there has been no transfer.
    ABal2 = get_balance(APubkey),
    BBal2 = get_balance(BPubkey),
    ABal2 = ABal1,
    BBal2 = BBal1,
    ct:pal("Balances 2: ~p, ~p\n", [ABal2,BBal2]),

    %% Wait until TTL has been passed.
    {ok,[_,_]} = aecore_suite_utils:mine_blocks(NodeName, 2),
    
    %% Check that tx has failed.
    ct:pal("TxHash2 ~p\n", [tx_in_block(TxHash)]),

    ok.

%% spending_3(Config)
%%  A simple test of tokens from acc_a to acc_b. There are not enough
%%  tokens in acc_a so tx suspends until acc_a gets enough.

spending_3(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey}} = proplists:get_value(accounts, Config),

    %% Check initial balances.
    ABal0 = get_balance(APubkey),
    BBal0 = get_balance(BPubkey),
    ct:pal("Balances 0: ~p, ~p\n", [ABal0,BBal0]),

    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Get balances after mining.
    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    ct:pal("Balances 1: ~p, ~p\n", [ABal1,BBal1]),

    %% Transfer money from Alice to Bert, but more than Alice has.
    TxHash = spend_tokens(APubkey, APrivkey, BPubkey, ABal1 + 200, 5),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),
    
    %% Check that tx has failed.
    ?assertNot(tx_in_block(TxHash)),

    %% Check that there has been no transfer.
    ABal2 = get_balance(APubkey),
    BBal2 = get_balance(BPubkey),
    ABal2 = ABal1,
    BBal2 = BBal1,
    ct:pal("Balances 2: ~p, ~p\n", [ABal2,BBal2]),

    %% Now we add enough tokens to acc_a so it can do the spend tx.
    {ok,200,_} = post_spend_tx(APubkey, 500, 1),
    {ok,[_,_]} = aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Check the balance to see what happened.
    ABal3 = get_balance(APubkey),
    BBal3 = get_balance(BPubkey),
    ct:pal("Balances 3: ~p, ~p\n", [ABal3,BBal3]),

    %% Check that tx has succeeded.
    ?assert(tx_in_block(TxHash)),

    ok.

%% identity_contract_1(Config)
%%  Create the Identity contract by miner and call by miner.

identity_contract_1(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get miner balance.
    {ok, 200, _Btop} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    %% {ok,MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),

    %% Compile contract "identity.aes"
    ContractString = aeso_test_utils:read_contract("identity"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract, owned by Miner.
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitData} = aect_sophia:encode_call_data(HexCode, InitFunction,
                                                         InitArgument),
    %% DecodedInitData = aeu_hex:hexstring_decode(EncodedInitData),

    ContractInitEncoded = #{ owner => MinerAddress,
                             code => HexCode,
                             vm_version => 1,
                             deposit => 2,
                             amount => 1,
                             gas => 1000,               %Need a lot of gas
                             gas_price => 1,
                             fee => 1,
                             call_data => EncodedInitData},
    %% ContractInitDecoded = maps:merge(ContractInitEncoded,
    %%                                  #{owner => MinerPubkey,
    %%                                    code => BinCode,
    %%                                    call_data => DecodedInitData}),

    {ok,200,#{<<"tx">> := EncodedUnsignedContractCreateTx,
              <<"contract_address">> := EncodedContractPubkey}} =
        get_contract_create(ContractInitEncoded),
    %% {ok, DecodedContractPubkey} = aec_base58c:safe_decode(contract_pubkey,
    %%                                                       EncodedContractPubkey),
    ContractCreateTxHash = sign_and_post_tx(MinerAddress,
                                            EncodedUnsignedContractCreateTx),

    %% Mine blocks.
    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    CallFunction = <<"main">>,
    CallArgument = <<"(42)">>,
    {ok, EncodedCallData} = aect_sophia:encode_call_data(HexCode, CallFunction,
                                                         CallArgument),
    %% DecodedCallData = aeu_hex:hexstring_decode(EncodedCallData),

    ContractCallEncoded = #{ caller => MinerAddress,
                             contract => EncodedContractPubkey,
                             vm_version => 1,
                             amount => 1,
                             gas => 600,        %Need a log of gas
                             gas_price => 1,
                             fee => 1,
                             call_data => EncodedCallData},
    %% ContractCallDecoded = maps:merge(ContractCallEncoded,
    %%                                  #{caller => MinerPubkey,
    %%                                    contract => DecodedContractPubkey,
    %%                                    call_data => DecodedCallData}),

    {ok,200,#{<<"tx">> := EncodedUnsignedContractCallTx}} =
        get_contract_call(ContractCallEncoded),
    ContractCallTxHash = sign_and_post_tx(MinerAddress,
                                          EncodedUnsignedContractCallTx),

    %% Try to get the call object while in mempool
    {ok,400,#{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCallTxHash),

    %% Mine blocks.
    aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(ContractCallTxHash)),

    %% Get the call object
    {ok,200,#{<<"return_type">> := <<"ok">>,
              <<"return_value">> := Value}} =
        get_contract_call_object(ContractCallTxHash),

    ct:pal("Return value ~p\n", [Value]),
    ok.

%% identity_contract_2(Config)
%%  Create the Identity contract by account acc_c and call by accounts
%%  acc_c and acc_d.

identity_contract_2(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_c := #{pub_key := CPubkey,
                 priv_key := CPrivkey},
      acc_d := #{pub_key := DPubkey,
                 priv_key := DPrivkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    CBal0 = ensure_balance(CPubkey, 50000),
    DBal0 = ensure_balance(DPubkey, 50000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "identity.aes"
    ContractString = aeso_test_utils:read_contract("identity"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract, owned by Carl.
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok,EncodedInitData} = aect_sophia:encode_call_data(HexCode, InitFunction,
                                                        InitArgument),
    {ContractCreateTxHash,EncodedContractPubkey,_} =
        contract_create_tx(CPubkey, CPrivkey, HexCode, EncodedInitData),

    %% Mine blocks check that it is in the chain.
    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Set up call.
    CallFunction = <<"main">>,
    CallArgument = <<"(42)">>,
    {ok,EncodedCallData} = aect_sophia:encode_call_data(HexCode, CallFunction,
                                                        CallArgument),

    %% Call contract by Carl.
    CContractCallTxHash = contract_call_tx(CPubkey, CPrivkey,
                                           EncodedContractPubkey,
                                           EncodedCallData),

    ct:pal("C ContractCallTxHash ~p\n", [CContractCallTxHash]),

    %% Try to get the call object while in mempool
    {ok,400,#{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(CContractCallTxHash),

    %% Mine blocks.
    aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(CContractCallTxHash)),

    %% Get the call object
    {ok,200,#{<<"return_type">> := <<"ok">>,
              <<"return_value">> := CValue}} =
        get_contract_call_object(CContractCallTxHash),

    %% Call contract by Diana.
    DContractCallTxHash = contract_call_tx(DPubkey, DPrivkey,
                                           EncodedContractPubkey,
                                           EncodedCallData),

    ct:pal("D ContractCallTxHash ~p\n", [DContractCallTxHash]),

    %% Mine blocks.
    aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(DContractCallTxHash)),

    %% Get the call object
    {ok,200,#{<<"return_type">> := <<"ok">>,
              <<"return_value">> := DValue}} =
        get_contract_call_object(DContractCallTxHash),

    ct:pal("C Return value ~p\n", [CValue]),
    ct:pal("D Return value ~p\n", [DValue]),
    ct:pal("C Balances ~p, ~p\n", [CBal0,get_balance(CPubkey)]),
    ct:pal("D Balances ~p, ~p\n", [DBal0,get_balance(DPubkey)]),

    ok.

%% simple_storage(Config)
%%  Create the SimpleStorage contract by acc_a and test and set its
%%  state data by acc_a, acc_b, acc_c and finally by acc_d.

simple_storage(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
                 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
                 priv_key := BPrivkey},
      acc_c := #{pub_key := CPubkey,
                 priv_key := CPrivkey},
      acc_d := #{pub_key := DPubkey,
                 priv_key := DPrivkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    ABal0 = ensure_balance(APubkey, 50000),
    BBal0 = ensure_balance(BPubkey, 50000),
    CBal0 = ensure_balance(CPubkey, 50000),
    DBal0 = ensure_balance(DPubkey, 50000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "simple_storage.aes"
    ContractString = aeso_test_utils:read_contract("simple_storage"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract, owned by Alice.
    InitFunction = <<"init">>,
    InitArgument = <<"(21)">>,
    {ok,EncodedInitData} = aect_sophia:encode_call_data(HexCode, InitFunction,
                                                        InitArgument),
    {ContractCreateTxHash,EncodedContractPubkey,_} =
        contract_create_tx(APubkey, APrivkey, HexCode, EncodedInitData),

    %% Mine blocks and check that it is in the chain.
    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Setup call to get which can be used by all gets.
    {ok,EncodedGetData} = aect_sophia:encode_call_data(HexCode,
						       <<"get">>, <<"()">>),

    %% Call get contract by Alice.
    AGetValue1 = call_get(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			  EncodedGetData),
    ?assertEqual(AGetValue1, ?VALUE_21),

    %% Call set contract by Alice.
    ASetValue1 = call_set(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			  HexCode, <<"(42)">>),
    %% ct:pal("A set value ~p\n", [ASetValue1]),
    ?assertEqual(ASetValue1, ?VALUE_STATEP),

    %% Call get contract by Bert.
    BGetValue1 = call_get(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			  EncodedGetData),
    ?assertEqual(BGetValue1, ?VALUE_42),
    %% Call set contract by Bert.
    BSetValue1 = call_set(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			  HexCode, <<"(84)">>),
    ?assertEqual(BSetValue1, ?VALUE_STATEP),

    %% Call get and set contract by Carl.
    {CGetValue1,CSetValue1} = call_get_set(NodeName, CPubkey, CPrivkey,
					   EncodedContractPubkey,
					   EncodedGetData,
					   HexCode, <<"(126)">>),
    ?assertEqual(CGetValue1, ?VALUE_84),
    ?assertEqual(CSetValue1, ?VALUE_STATEP),

    %% Call get contract by Diana.
    DGetValue1 = call_get(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			  EncodedGetData),
    ?assertEqual(DGetValue1, ?VALUE_126),

    %% Call set contract by Diana.
    DSetValue1 = call_set(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			  HexCode, <<"(168)">>),
    ct:pal("C set value ~p\n", [DSetValue1]),

    %% Call get contract by Alice.
    AGetValue2 = call_get(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			  EncodedGetData),
    ?assertEqual(AGetValue2, ?VALUE_168),

    ct:pal("A Balances ~p, ~p\n", [ABal0,get_balance(APubkey)]),
    ct:pal("B Balances ~p, ~p\n", [BBal0,get_balance(BPubkey)]),
    ct:pal("C Balances ~p, ~p\n", [CBal0,get_balance(CPubkey)]),
    ct:pal("D Balances ~p, ~p\n", [DBal0,get_balance(DPubkey)]),

    ok.

call_get_set(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedGetData,
	     HexCode, SetVal) ->
    GetValue = call_get(NodeName, Pubkey, Privkey, EncodedContractPubkey,
			EncodedGetData),
    SetValue = call_set(NodeName, Pubkey, Privkey, EncodedContractPubkey,
			HexCode, SetVal),
    {GetValue,SetValue}.

call_get(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedGetData) ->
    %% Call get contract.
    ContractGetTxHash = contract_call_tx(Pubkey, Privkey,
					 EncodedContractPubkey,
					 EncodedGetData),
    %% Mine blocks and check that it is in the chain.
    aecore_suite_utils:mine_blocks(NodeName, 2),
    ?assert(tx_in_chain(ContractGetTxHash)),
    %% Get the call object and return value.
    {ok,200,#{<<"return_type">> := <<"ok">>,
              <<"return_value">> := GetValue}} =
        get_contract_call_object(ContractGetTxHash),
    GetValue.

call_set(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode, SetVal) ->
    %% Call set contract.
    {ok,EncodedSetData} = aect_sophia:encode_call_data(HexCode,
						       <<"set">>, SetVal),
    ContractSetTxHash = contract_call_tx(Pubkey, Privkey,
					 EncodedContractPubkey,
					 EncodedSetData),
    %% Mine blocks.
    aecore_suite_utils:mine_blocks(NodeName, 2),
    %% Get the call object and return value.
    {ok,200,#{<<"return_type">> := <<"ok">>,
              <<"return_value">> := SetValue}} =
        get_contract_call_object(ContractSetTxHash),
    SetValue.

contract_create_tx(Pubkey, Privkey, HexCode, EncodedInitData) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    {ok,Nonce} = rpc(aec_next_nonce, pick_for_account, [Pubkey]),

    ContractInitEncoded = #{ owner => Address,
                             code => HexCode,
                             vm_version => 1,
                             deposit => 2,
                             amount => 1,
                             gas => 1000,               %Need a lot of gas
                             gas_price => 1,
                             fee => 1,
                             nonce => Nonce,
                             call_data => EncodedInitData},
    %% ContractInitDecoded = maps:merge(ContractInitEncoded,
    %%                                  #{owner => Pubkey,
    %%                                    code => BinCode,
    %%                                    call_data => DecodedInitData}),
    sign_and_post_create_tx(Privkey, ContractInitEncoded).


contract_call_tx(Pubkey, Privkey, EncodedContractPubkey, EncodedCallData) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    {ok,Nonce} = rpc(aec_next_nonce, pick_for_account, [Pubkey]),

    ContractCallEncoded = #{ caller => Address,
                             contract => EncodedContractPubkey,
                             vm_version => 1,
                             amount => 1,
                             gas => 1000,       %Need a lot of gas
                             gas_price => 1,
                             fee => 1,
                             nonce => Nonce,
                             call_data => EncodedCallData},
    %% ContractCallDecoded = maps:merge(ContractCallEncoded,
    %%                                  #{caller => Pubkey,
    %%                                    contract => DecodedContractPubkey,
    %%                                    call_data => DecodedCallData}),
    sign_and_post_call_tx(Privkey, ContractCallEncoded).

dutch_auction_contract_1(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
		 priv_key := BPrivkey}} = proplists:get_value(accounts, Config),
    % Get miner balance.
    {ok, 200, _Btop} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),

    %% Compile contract "dutch_auction.aes"
    ContractString = aeso_test_utils:read_contract("dutch_auction"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    Function = <<"init">>,
    Argument = <<"(42,500,5)">>,
    {ok, EncodedCallData} = aect_sophia:encode_call_data(HexCode, Function,
                                                         Argument),
    DecodedCallData = aeu_hex:hexstring_decode(EncodedCallData),

    %% Owned by Alice.
    AAddress = aec_base58c:encode(account_pubkey, APubkey),
    {ok,ANonce} = rpc(aec_next_nonce, pick_for_account, [APubkey]),
    ct:pal("ANonce ~p\n", [ANonce]),

    ValidEncoded = #{ owner => AAddress,
                      code => HexCode,
                      vm_version => 1,
                      deposit => 2,
                      amount => 1,
                      gas => 30,
                      gas_price => 1,
                      fee => 1,
		      nonce => ANonce,
                      call_data => EncodedCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner => APubkey,
                                code => BinCode,
                                call_data => DecodedCallData}),

    %% Create transaction and sign
    {ok, 200, #{<<"tx_hash">> := Ctx}} = sign_and_post_create_tx(APrivkey,
								 ValidDecoded),
    
    ct:pal("Ctx ~p\n", [Ctx]),

    %% Mine a block.
    NodeName = proplists:get_value(node_name, Config),
    {ok, [Cblock]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Mine some more blocks.
    aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Call the contract in another transaction.

    ContractPubkey = aect_contracts:compute_contract_pubkey(APubkey, ANonce),

    ContractPubkeyHash = aec_base58c:encode(transaction, ContractPubkey),
    
    ct:pal("Ckey ~p\n", [ContractPubkey]),
    ct:pal("Ckeyhash ~p\n", [ContractPubkeyHash]),

    %% Called by Bert.
    BAddress = aec_base58c:encode(account_pubkey, BPubkey),
    {ok,BNonce} = rpc(aec_next_nonce, pick_for_account, [BPubkey]),
    ct:pal("BNonce ~p\n", [BNonce]),

    ContractCallEncoded = #{ caller => BAddress,
                             contract => ContractPubkeyHash,
                             vm_version => 1,
                             amount => 1,
                             gas => 10,
                             gas_price => 1,
                             fee => 1,
			     nonce => BNonce,
                             call_data => EncodedCallData},

    ContractCallDecoded = maps:merge(ContractCallEncoded,
                              #{caller => BPubkey,
				contract => ContractPubkey,
                                call_data => DecodedCallData}),

    %% Create call transaction and sign
    CallRet = sign_and_post_call_tx(BPrivkey, ContractCallDecoded),
    
    ct:pal("Callret ~p\n", [CallRet]),

    %% Mine a block
    aecore_suite_utils:mine_blocks(NodeName, 2),

    ok.

dutch_auction_contract_2(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
		 priv_key := BPrivkey}} = proplists:get_value(accounts, Config),
    % Get miner balance.
    {ok, 200, Btop} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),

    %% Compile contract "dutch_auction.aes"
    ContractString = aeso_test_utils:read_contract("dutch_auction"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    ct:pal("hexcode ~p\n", [HexCode]),

    %% Owned by Alice.
    AAddress = aec_base58c:encode(account_pubkey, APubkey),
    {ok,ANonce} = rpc(aec_next_nonce, pick_for_account, [APubkey]),
    ct:pal("ANonce ~p\n", [ANonce]),

    Function = <<"init">>,
    Argument = <<"(42,500,5)">>,
    {ok, EncodedCallData} = aect_sophia:encode_call_data(HexCode, Function,
                                                         Argument),
    DecodedCallData = aeu_hex:hexstring_decode(EncodedCallData),

    ValidEncoded = #{ owner => AAddress,
                      code => HexCode,
                      vm_version => 1,
                      deposit => 2,
                      amount => 1,
                      gas => 30,
                      gas_price => 1,
                      fee => 1,
		      nonce => ANonce,
                      call_data => EncodedCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner => APubkey,
                                code => BinCode,
                                call_data => DecodedCallData}),

    %% Prepare a contract_create_tx and post it.
    {ok,200,#{<<"tx">> := EncodedUnsignedContractCreateTx,
	      <<"contract_address">> := _ContractPubkey}} =
	get_contract_create(ValidEncoded),
    Ctx = sign_and_post_tx(AAddress, EncodedUnsignedContractCreateTx),

    %% Create transaction and sign
    %% {ok, 200, #{<<"tx_hash">> := Ctx}} = sign_and_post_create_tx(APrivkey,
    %% 								 ValidDecoded),
    
    ct:pal("Ctx ~p\n", [{Ctx,AAddress}]),

    %% Mine a block.
    NodeName = proplists:get_value(node_name, Config),
    {ok, [Cblock]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Mine some more blocks.
    aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Call the contract in another transaction.

    ContractPubkey = aect_contracts:compute_contract_pubkey(APubkey, ANonce),

    ContractPubkeyHash = aec_base58c:encode(transaction, ContractPubkey),
    
    ct:pal("Ckey ~p\n", [ContractPubkey]),
    ct:pal("Ckeyhash ~p\n", [ContractPubkeyHash]),

    %% Called by Bert.
    BAddress = aec_base58c:encode(account_pubkey, BPubkey),
    {ok,BNonce} = rpc(aec_next_nonce, pick_for_account, [BPubkey]),
    ct:pal("BNonce ~p\n", [BNonce]),

    ContractCallEncoded = #{ caller => BAddress,
                             contract => ContractPubkeyHash,
                             vm_version => 1,
                             amount => 1,
                             gas => 10,
                             gas_price => 1,
                             fee => 1,
			     nonce => BNonce,
                             call_data => EncodedCallData},

    ContractCallDecoded = maps:merge(ContractCallEncoded,
                              #{caller => BPubkey,
				contract => ContractPubkey,
                                call_data => DecodedCallData}),

    %% Create call transaction and sign
    CallRet = sign_and_post_call_tx(BPrivkey, ContractCallDecoded),
    
    ct:pal("Callret ~p\n", [CallRet]),

    %% Mine a block
    aecore_suite_utils:mine_blocks(NodeName, 2),

    ok.

get_balance(Pubkey) ->
    Addr = aec_base58c:encode(account_pubkey, Pubkey),
    {ok,200,#{<<"balance">> := Balance}} = get_balance_at_top(Addr),
    Balance.

ensure_balance(Pubkey, NewBalance) ->
    Balance = get_balance(Pubkey),              %Get current balance
    if Balance >= NewBalance ->                 %Enough already, do nothing
            Balance;
       true ->
            %% Get more tokens from the miner.
            Fee = 1,
            Incr = NewBalance - Balance + Fee,  %Include the fee
            {ok,200,_} = post_spend_tx(Pubkey, Incr, Fee),
            NewBalance
    end.

assert_balance(Pubkey, ExpectedBalance) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    {ok,200,#{<<"balance">> := ExpectedBalance}} = get_balance_at_top(Address).

%% tests the following
%% GET contract_create_tx unsigned transaction
%% GET contract_call_tx unsigned transaction
%% due to complexity of contract_call_tx (needs a contract in the state tree)
%% both positive and negative cases are tested in this test

%% ============================================================
%% HTTP Requests
%% ============================================================

get_top() ->
    Host = external_address(),
    http_request(Host, get, "top", []).

get_contract_create(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/create", Data).

call_contract_directly(Data) ->
    Host = external_address(),
    http_request(Host, post, "contract/call", Data).

get_contract_call(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/call", Data).

%% get_contract_call_compute(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/contract/call/compute", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "tx/"++binary_to_list(TxHash)++"/contract-call", []).

%% get_spend(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/spend", Data).

%% get_name_preclaim(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/name/preclaim", Data).

%% get_name_claim(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/name/claim", Data).

%% get_name_update(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/name/update", Data).

%% get_name_transfer(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/name/transfer", Data).

%% get_name_revoke(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/name/revoke", Data).

%% get_channel_create(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/create", Data).

%% get_channel_deposit(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/deposit", Data).

%% get_channel_withdrawal(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/withdrawal", Data).

%% get_channel_close_mutual(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/close/mutual", Data).

%% get_channel_close_solo(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/close/solo", Data).

%% get_channel_slash(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/slash", Data).

%% get_channel_settle(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/channel/settle", Data).

%% get_block_by_height(Height, TxObjects) ->
%%     Params = tx_encoding_param(TxObjects),
%%     Host = external_address(),
%%     http_request(Host, get, "block/height/" ++ integer_to_list(Height), Params).

%% get_block_by_height(Height) ->
%%     Host = external_address(),
%%     http_request(Host, get, "block/height/" ++ integer_to_list(Height), []).

%% get_block_by_hash(Hash, TxObjects) ->
%%     Params = tx_encoding_param(TxObjects),
%%     Host = external_address(),
%%     http_request(Host, get, "block/hash/" ++ http_uri:encode(Hash), Params).

%% get_header_by_hash(Hash) ->
%%     Host = external_address(),
%%     http_request(Host, get, "header-by-hash", [{hash, Hash}]).

%% get_transactions() ->
%%     Host = external_address(),
%%     http_request(Host, get, "transactions", []).

get_tx(TxHash, TxEncoding) ->
    Params = tx_encoding_param(TxEncoding),
    Host = external_address(),
    http_request(Host, get, "tx/" ++ binary_to_list(TxHash), Params).

post_spend_tx(Recipient, Amount, Fee) ->
    post_spend_tx(Recipient, Amount, Fee, <<"post spend tx">>).

post_spend_tx(Recipient, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "spend-tx",
                 #{recipient_pubkey => aec_base58c:encode(account_pubkey,
							  Recipient),
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

%% post_name_preclaim_tx(Commitment, Fee) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "name-preclaim-tx",
%%                  #{commitment => aec_base58c:encode(commitment, Commitment),
%%                    fee        => Fee}).

%% post_name_claim_tx(Name, NameSalt, Fee) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "name-claim-tx",
%%                  #{name      => Name,
%%                    name_salt => NameSalt,
%%                    fee       => Fee}).

%% post_name_update_tx(NameHash, NameTTL, Pointers, TTL, Fee) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "name-update-tx",
%%                  #{name_hash => aec_base58c:encode(name, NameHash),
%%                    name_ttl  => NameTTL,
%%                    pointers  => Pointers,
%%                    ttl       => TTL,
%%                    fee       => Fee}).

%% post_name_transfer_tx(NameHash, RecipientPubKey, Fee) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "name-transfer-tx",
%%                  #{name_hash        => aec_base58c:encode(name, NameHash),
%%                    recipient_pubkey => aec_base58c:encode(account_pubkey, RecipientPubKey),
%%                    fee              => Fee}).

%% post_name_revoke_tx(NameHash, Fee) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "name-revoke-tx",
%%                  #{name_hash => aec_base58c:encode(name, NameHash),
%%                    fee       => Fee}).

%% get_commitment_hash(Name, Salt) ->
%%     Host = external_address(),
%%     http_request(Host, get, "commitment-hash", [{name, Name}, {salt, Salt}]).

%% get_name(Name) ->
%%     Host = external_address(),
%%     http_request(Host, get, "name", [{name, Name}]).

get_balance_at_top() ->
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    get_balance_at_top(EncodedPubKey).

get_balance_at_top(EncodedPubKey) ->
    get_balance(EncodedPubKey, []).

get_balance(EncodedPubKey, Params) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/balance",
                 Params).

%% get_account_transactions(EncodedPubKey, Params) ->
%%     Host = external_address(),
%%     http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/txs",
%%                  Params).

%% post_block(Block) ->
%%     post_block_map(aehttp_api_parser:encode(block, Block)).

%% post_block_map(BlockMap) ->
%%     Host = external_address(),
%%     http_request(Host, post, "block", BlockMap).

post_tx(TxSerialized) ->
    Host = external_address(),
    http_request(Host, post, "tx", #{tx => TxSerialized}).

%% get_all_accounts_balances() ->
%%     Host = external_address(),
%%     http_request(Host, get, "balances", []).

get_miner_pub_key() ->
    Host = internal_address(),
    http_request(Host, get, "account/pub-key", []).

%% get_peer_pub_key() ->
%%     Host = external_address(),
%%     http_request(Host, get, "peer/key", []).

%% get_version() ->
%%     Host = external_address(),
%%     http_request(Host, get, "version", []).

%% get_info() ->
%%     Host = external_address(),
%%     http_request(Host, get, "info", []).

%% get_block_number() ->
%%     Host = internal_address(),
%%     http_request(Host, get, "block/number", []).

%% get_internal_block_preset(Segment, TxObjects) ->
%%     Params = tx_encoding_param(TxObjects),
%%     Host = external_address(),
%%     http_request(Host, get, "block/" ++ Segment, Params).

tx_encoding_param(default) -> #{};
tx_encoding_param(json) -> #{tx_encoding => <<"json">>};
tx_encoding_param(message_pack) -> #{tx_encoding => <<"message_pack">>}.

%% get_block_txs_count_by_height(Height) ->
%%     Host = internal_address(),
%%     http_request(Host, get, "block/txs/count/height/" ++ integer_to_list(Height),
%%                  []).

%% get_block_txs_count_by_hash(Hash) ->
%%     Host = internal_address(),
%%     http_request(Host, get, "block/txs/count/hash/" ++ http_uri:encode(Hash),
%%                  []).

%% get_block_txs_count_preset(Segment) ->
%%     Host = internal_address(),
%%     http_request(Host, get, "block/txs/count/" ++ Segment, []).

%% get_block_tx_by_index_height(Height, Index, TxObjects) ->
%%     Params = tx_encoding_param(TxObjects),
%%     Host = internal_address(),
%%     http_request(Host, get, "block/tx/height/" ++ integer_to_list(Height) ++
%%                                        "/" ++ integer_to_list(Index), Params).

%% get_block_tx_by_index_hash(Hash, Index, TxObjects) when is_binary(Hash) ->
%%     get_block_tx_by_index_hash(binary_to_list(Hash), Index, TxObjects);
%% get_block_tx_by_index_hash(Hash, Index, TxObjects) ->
%%     Params = tx_encoding_param(TxObjects),
%%     Host = internal_address(),
%%     http_request(Host, get, "block/tx/hash/" ++ http_uri:encode(Hash) ++
%%                                        "/" ++ integer_to_list(Index), Params).

%% get_block_tx_by_index_latest(Index, TxObjects) ->
%%     Params = tx_encoding_param(TxObjects),
%%     Host = internal_address(),
%%     http_request(Host, get, "block/tx/latest/" ++ integer_to_list(Index), Params).

%% get_block_txs_list_by_height(From, To, TxObjects, TxTypes) ->
%%     Params0 = tx_encoding_param(TxObjects),
%%     Filter = make_tx_types_filter(TxTypes),
%%     Params = maps:merge(Params0, maps:merge(Filter, #{from => From, to => To})),
%%     Host = internal_address(),
%%     http_request(Host, get, "block/txs/list/height", Params).

%% get_block_txs_list_by_hash(From, To, TxObjects, TxTypes) ->
%%     Params0 = tx_encoding_param(TxObjects),
%%     Filter = make_tx_types_filter(TxTypes),
%%     Params = maps:merge(Params0, maps:merge(Filter, #{from => From, to => To})),
%%     Host = internal_address(),
%%     http_request(Host, get, "block/txs/list/hash", Params).

%% make_tx_types_filter(Filter) ->
%%     Includes = maps:get(include, Filter, []),
%%     Excludes = maps:get(exclude, Filter, []),
%%     Encode =
%%         fun(_, [], Res) -> Res;
%%         (Key, TypesBin, Res) ->
%%             Types = lists:map(fun binary_to_list/1, TypesBin),
%%             T = list_to_binary(lists:join(",", Types)),
%%             maps:put(Key, T, Res)
%%         end,
%%     R0 = Encode(tx_types, Includes, #{}),
%%     R = Encode(exclude_tx_types, Excludes, R0),
%%     R.

%% get_list_oracles(Max) ->
%%     get_list_oracles(undefined, Max).

%% get_list_oracles(From, Max) ->
%%     Host = internal_address(),
%%     Params0 = #{ max => Max },
%%     Params = case From of undefined -> Params0; _ -> Params0#{ from => From } end,
%%     {ok, 200, Oracles} = http_request(Host, get, "oracles", Params),
%%     Oracles.

%% get_list_oracle_queries(Oracle, Max) ->
%%     get_list_oracle_queries(Oracle, undefined, Max).

%% get_list_oracle_queries(Oracle, From, Max) ->
%%     Host = internal_address(),
%%     Params0 = #{ max => Max, oracle_pub_key => aec_base58c:encode(oracle_pubkey, Oracle) },
%%     Params = case From of undefined -> Params0; _ -> Params0#{ from => From } end,
%%     {ok, 200, Queries} = http_request(Host, get, "oracle-questions", Params),
%%     Queries.

%% get_peers() ->
%%     Host = internal_address(),
%%     http_request(Host, get, "debug/peers", []).


%% ============================================================
%% private functions
%% ============================================================
rpc(Mod, Fun, Args) ->
    rpc(?NODE, Mod, Fun, Args).

rpc(Node, Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"external">>, <<"port">>],
                aehttp, [external, port], 8043]),
    "http://127.0.0.1:" ++ integer_to_list(Port).     % good enough for requests

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"internal">>, <<"port">>],
                aehttp, [internal, port], 8143]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

%% ws_host_and_port() ->
%%     Port = rpc(aeu_env, user_config_or_env,
%%               [ [<<"websocket">>, <<"internal">>, <<"port">>],
%%                 aehttp, [internal, websocket, port], 8144]),
%%     {"127.0.0.1", Port}.

%% channel_ws_host_and_port() ->
%%     Port = rpc(aeu_env, user_config_or_env,
%%               [ [<<"websocket">>, <<"channel">>, <<"port">>],
%%                 aehttp, [channel, websocket, port], 8045]),
%%     {"localhost", Port}.

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary([Host, "/v2/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "/v2/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

header_to_endpoint_top(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    maps:put(<<"hash">>, aec_base58c:encode(block_hash, Hash),
             aehttp_api_parser:encode(header, Header)).

random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 65)),
    list_to_binary(HList).

populate_block(Txs) ->
    lists:foreach(
        fun(#{recipient := R, amount := A, fee := F}) ->
            {ok, 200, _} = post_spend_tx(R, A, F)
        end,
        maps:get(spend_txs, Txs, [])),
    ok.

%% spend_tokens(SenderPubkey, SenderPrivkey, Recipient, Amount, Fee) ->
%% spend_tokens(SenderPubkey, SenderPrivkey, Recipient, Amount, Fee, CallerSet) ->
%%     TxHash
%%  This is based on post_correct_tx/1 in aehttp_integration_SUITE.

spend_tokens(SenderPub, SenderPriv, Recip, Amount, Fee) ->
    DefaultSet = #{ttl => 0},                   %Defaut fields set by caller
    spend_tokens(SenderPub, SenderPriv, Recip, Amount, Fee, DefaultSet).

spend_tokens(SenderPub, SenderPriv, Recip, Amount, Fee, CallerSet) ->
    %% Generate a nonce.
    {ok,Nonce} = rpc(aec_next_nonce, pick_for_account, [SenderPub]),
    Params0 = #{sender => SenderPub,
                recipient => Recip,
                amount => Amount,
                fee => Fee,
                nonce => Nonce,
                payload => <<"spend tokens">>},
    Params1 = maps:merge(Params0, CallerSet),   %Set caller defaults
    {ok, UnsignedTx} = aec_spend_tx:new(Params1),
    SignedTx = aetx_sign:sign(UnsignedTx, SenderPriv),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    %% Check that we get the correct hash.
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    EncodedSerializedTx = aec_base58c:encode(transaction, SerializedTx),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_tx(EncodedSerializedTx),
    TxHash.

sign_and_post_create_tx(Privkey, CreateEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx,
              <<"contract_address">> := EncodedPubkey}} =
        get_contract_create(CreateEncoded),
    {ok,DecodedPubkey} = aec_base58c:safe_decode(contract_pubkey,
                                                 EncodedPubkey),
    {ok,SerializedUnsignedTx} = aec_base58c:safe_decode(transaction,
                                                        EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aetx_sign:sign(UnsignedTx, Privkey),
    SendTx = aec_base58c:encode(transaction,
                                aetx_sign:serialize_to_binary(SignedTx)),
    %% ct:pal("sap_create_tx ~p\b", [{UnsignedTx,SignedTx,SendTx}]),
    {ok,200,#{<<"tx_hash">> := TxHash}} = post_tx(SendTx),
    {TxHash,EncodedPubkey,DecodedPubkey}.

sign_and_post_call_tx(Privkey, CallEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx}} = get_contract_call(CallEncoded),
    {ok,SerializedUnsignedTx} = aec_base58c:safe_decode(transaction,
                                                        EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aetx_sign:sign(UnsignedTx, Privkey),
    SendTx = aec_base58c:encode(transaction,
                                aetx_sign:serialize_to_binary(SignedTx)),
    %% ct:pal("sap_call_tx ~p\b", [{UnsignedTx,SignedTx,SendTx}]),
    {ok,200,#{<<"tx_hash">> := TxHash}} = post_tx(SendTx),
    TxHash.

%% sign_and_post_create_tx(Privkey, Decoded) ->
%%     {ok,CreateTx} = aect_create_tx:new(Decoded),
%%     SignedCreateTx = aetx_sign:sign(CreateTx, Privkey),
%%     %% Add to block candiatate.
%%     %% aec_tx_pool:push(SignedTrans),
%%     SendTx = aec_base58c:encode(transaction,
%% 				aetx_sign:serialize_to_binary(SignedCreateTx)),
%%     ct:pal("sap_create_tx ~p\b", [{CreateTx,SignedCreateTx,SendTx}]),
%%     post_tx(SendTx).

%% sign_and_post_call_tx(Privkey, CallDecoded) ->
%%     {ok,CallTx} = aect_call_tx:new(CallDecoded),
%%     SignedCallTx = aetx_sign:sign(CallTx, Privkey),
%%     %% Add to block candiatate.
%%     %% aec_tx_pool:push(SignedCall),
%%     SendCallTx = aec_base58c:encode(transaction,
%%     				    aetx_sign:serialize_to_binary(SignedCallTx)),
%%     post_tx(SendCallTx).

sign_and_post_tx(AccountPubKey, EncodedUnsignedTx) ->
    {ok, SerializedUnsignedTx} = aec_base58c:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} = rpc(aec_keys, sign, [UnsignedTx]),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    #{<<"hash">> := TxHash} = aetx_sign:serialize_for_client_pending(json, SignedTx),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_tx(aec_base58c:encode(transaction, SerializedTx)),
    %% Check tx is in mempool.
    %% Fun = fun() ->
    %%               tx_in_mempool_for_account(AccountPubKey, TxHash)
    %%       end,
    %% {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    TxHash.

tx_in_block(TxHash) ->
    {ok,200,#{<<"transaction">> := #{<<"block_hash">> := BlockHash}}} =
	get_tx(TxHash, json),
    BlockHash =/= <<"none">>.

tx_in_chain(TxHash) ->
    case get_tx(TxHash, json) of
        {ok, 200, #{<<"transaction">> :=
                        #{<<"block_hash">> := <<"none">>}}} ->
            ct:log("Tx not mined, but in mempool"),
            false;
        {ok, 200, #{<<"transaction">> := #{<<"block_hash">> := _}}} -> true;
        {ok, 404, _} -> false
    end.

%% make_params(L) ->
%%     make_params(L, []).

%% make_params([], Accum) ->
%%     maps:from_list(Accum);
%% make_params([H | T], Accum) when is_map(H) ->
%%     make_params(T, maps:to_list(H) ++ Accum);
%% make_params([{K, V} | T], Accum) ->
%%     make_params(T, [{K, V} | Accum]).

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.
