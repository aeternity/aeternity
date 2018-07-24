-module(aehttp_contracts_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% common_test exports
-export([
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
         counter_contract/1,
         dutch_auction_contract/1,
         environment_contract/1,
         identity_contract/1,
	 maps_contract/1,
         simple_storage_contract/1,
	 spend_test_contract/1,
         null/1
        ]).

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).

%% Contract return values for some predefined numbers. Conversion
%% functions will make this unnecessary.

-define(VALUE_STATEP, <<"0x0000000000000000000000000000000000000000000000000000000000000020">>).

all() ->
    [
     {group, contracts}
    ].

groups() ->
    [
     {contracts, [sequence],
      [
       spending_1,
       spending_2,
       spending_3,
       identity_contract,
       simple_storage_contract,
       counter_contract,
       maps_contract,
       environment_contract,
       spend_test_contract,
       dutch_auction_contract,
       null                                     %This allows to end with ,
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.http_contracts"},
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

    aecore_suite_utils:mine_key_blocks(NodeName, 1),

    %% Prepare accounts, Alice, Bert, Carl and Diana.

    StartAmt = 5000,
    {APubkey, APrivkey, _} = new_account(StartAmt),
    {BPubkey, BPrivkey, _} = new_account(StartAmt),
    {CPubkey, CPrivkey, _} = new_account(StartAmt),
    {DPubkey, DPrivkey, _} = new_account(StartAmt),

    {ok, [_KeyBlock,Block]} = aecore_suite_utils:mine_blocks(NodeName, 2),
    Txs = [_Spend1,_Spend2,_Spend3,_Spend4] = aec_blocks:txs(Block),
    ct:pal("Block txs ~p\n", [Txs]),

    %% Save account information.
    Accounts = #{acc_a => #{pub_key => APubkey,
			    priv_key => APrivkey,
			    start_amt => StartAmt},
		 acc_b => #{pub_key => BPubkey,
			    priv_key => BPrivkey,
			    start_amt => StartAmt},
		 acc_c => #{pub_key => CPubkey,
			    priv_key => CPrivkey,
			    start_amt => StartAmt},
		 acc_d => #{pub_key => DPubkey,
			    priv_key => DPrivkey,
			    start_amt => StartAmt}},
    [{accounts,Accounts},{node_name,NodeName}|Config];
init_per_group(test_group, Config) ->		%Do nothing
    Config;
init_per_group(_Group, Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),
    ToMine = aecore_suite_utils:latest_fork_height(),
    ct:pal("ToMine ~p\n", [ToMine]),
    aecore_suite_utils:mine_blocks(NodeName, ToMine),
    [{node_name,NodeName}|Config].

end_per_group(test_group, _Config) ->
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
%%  Does nothing and always succeeds.

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

    %% Check initial balances.
    ABal0 = get_balance(APubkey),
    BBal0 = get_balance(BPubkey),
    ct:pal("Balances 0: ~p, ~p\n", [ABal0,BBal0]),

    %% Add tokens to both accounts and wait until done.
    {ok,200,#{<<"tx_hash">> := ATxHash}} = post_spend_tx(APubkey, 500, 1),
    {ok,200,#{<<"tx_hash">> := BTxHash}} = post_spend_tx(BPubkey, 500, 1),
    MineUntilPosts = fun () ->
			     tx_in_chain(ATxHash) and tx_in_chain(BTxHash)
		     end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntilPosts, 10),

    %% Get balances after mining.
    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    ct:pal("Balances 1: ~p, ~p\n", [ABal1,BBal1]),

    %% Amount transfered and fee.
    Amount = 200,
    Fee = 5,

    %% Transfer money from Alice to Bert.
    TxHash = spend_tokens(APubkey, APrivkey, BPubkey, Amount, Fee),
    MineUntilSpend = fun () -> tx_in_chain(TxHash) end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntilSpend, 10),

    %% Check that tx has succeeded.
    ?assert(tx_in_chain(TxHash)),

    %% Check balances after sending.
    ABal2 = get_balance(APubkey),
    BBal2 = get_balance(BPubkey),
    ct:pal("Balances 2: ~p, ~p\n", [ABal2,BBal2]),

    %% Check that the balances are correct, don't forget the fee.
    ABal2 = ABal1 - Amount - Fee,
    BBal2 = BBal1 + Amount,

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
    ct:pal("TxHash1 ~p\n", [tx_in_chain(TxHash)]),

    %% Check that there has been no transfer.
    ABal2 = get_balance(APubkey),
    BBal2 = get_balance(BPubkey),
    ABal2 = ABal1,
    BBal2 = BBal1,
    ct:pal("Balances 2: ~p, ~p\n", [ABal2,BBal2]),

    %% Wait until TTL has been passed.
    {ok,[_,_,_]} = aecore_suite_utils:mine_blocks(NodeName, 3),

    %% Check that tx has failed.
    ct:pal("TxHash2 ~p\n", [tx_in_chain(TxHash)]),

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

    {ok,[_,_]} = aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Get balances after mining.
    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    ct:pal("Balances 1: ~p, ~p\n", [ABal1,BBal1]),

    %% Transfer money from Alice to Bert, but more than Alice has.
    SpendTxHash = spend_tokens(APubkey, APrivkey, BPubkey, ABal1 + 200, 5),
    {ok,[_,_]} = aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Check that tx has failed.
    ?assertNot(tx_in_chain(SpendTxHash)),

    %% Check that there has been no transfer.
    ABal2 = get_balance(APubkey),
    BBal2 = get_balance(BPubkey),
    ABal2 = ABal1,
    BBal2 = BBal1,
    ct:pal("Balances 2: ~p, ~p\n", [ABal2,BBal2]),

    %% Now we add enough tokens to acc_a so it can do the spend tx.
    {ok,200,#{<<"tx_hash">> := PostTxHash}} = post_spend_tx(APubkey, 500, 1),
    MineUntil = fun () -> tx_in_chain(PostTxHash) end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntil, 10),

    %% Check that tx has succeeded.
    ?assert(tx_in_chain(SpendTxHash)),

    %% Check the balance to see what happened.
    ABal3 = get_balance(APubkey),
    BBal3 = get_balance(BPubkey),
    ct:pal("Balances 3: ~p, ~p\n", [ABal3,BBal3]),

    ok.

%% identity_contract(Config)
%%  Create the Identity contract by account acc_c and call by accounts
%%  acc_c and acc_d.

identity_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_c := #{pub_key := CPubkey,
                 priv_key := CPrivkey},
      acc_d := #{pub_key := DPubkey,
                 priv_key := DPrivkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    ensure_balance(CPubkey, 50000),
    ensure_balance(DPubkey, 50000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "identity.aes"
    ContractString = aeso_test_utils:read_contract("identity"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract, owned by Carl.
    {EncodedContractPubkey,_,_} = create_contract(NodeName, CPubkey, CPrivkey,
						  HexCode, <<"()">>),

    %% Set up call to main which can be used by all calls.
    MainFunction = <<"main">>,
    MainArgument = <<"(42)">>,
    {ok,EncodedMainData} = aect_sophia:encode_call_data(HexCode, MainFunction,
                                                        MainArgument),

    %% Call contract main function by Carl.
    {CReturn,_} = call_encoded(NodeName, CPubkey, CPrivkey,
			       EncodedContractPubkey, EncodedMainData),
    #{<<"value">> := 42} = decode_data(<<"int">>, CReturn),

    %% Call contract main function by Diana.
    {DReturn,_} = call_encoded(NodeName, DPubkey, DPrivkey,
			       EncodedContractPubkey, EncodedMainData),
    #{<<"value">> := 42} = decode_data(<<"int">>, DReturn),

    ok.

%% simple_storage_contract(Config)
%%  Create the SimpleStorage contract by acc_a and test and set its
%%  state data by acc_a, acc_b, acc_c and finally by acc_d.

simple_storage_contract(Config) ->
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
    {EncodedContractPubkey,_,_} = create_contract(NodeName, APubkey, APrivkey,
						  HexCode, <<"(21)">>),

    %% Setup call to get which can be used by all calls to get.
    {ok,EncodedGetData} = aect_sophia:encode_call_data(HexCode,
						       <<"get">>, <<"()">>),

    %% Call contract get function by Alice.
    AGetValue1 = call_get(NodeName, APubkey, APrivkey,
			  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 21} = decode_data(<<"int">>, AGetValue1),

    %% Call contract set function by Alice.
    ASetValue1 = call_set(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			  HexCode, <<"(42)">>),
    ?assertEqual(ASetValue1, ?VALUE_STATEP),

    %% Call contract get function by Bert.
    BGetValue1 = call_get(NodeName, BPubkey, BPrivkey,
			  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 42 } = decode_data(<<"int">>, BGetValue1),

    %% Call contract set function by Bert.
    BSetValue1 = call_set(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			  HexCode, <<"(84)">>),
    ?assertEqual(BSetValue1, ?VALUE_STATEP),

    %% Call contract get function by Carl.
    CGetValue1 = call_get(NodeName, CPubkey, CPrivkey,
			  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 84 } = decode_data(<<"int">>, CGetValue1),

    %% Call contract set function by Carl.
    CSetValue1 = call_set(NodeName, CPubkey, CPrivkey, EncodedContractPubkey,
			  HexCode, <<"(126)">>),
    ?assertEqual(CSetValue1, ?VALUE_STATEP),

    %% Call contract get function by Diana.
    DGetValue1 = call_get(NodeName, DPubkey, DPrivkey,
			  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 126 } = decode_data(<<"int">>, DGetValue1),

    %% Call contract set function by Diana.
    DSetValue1 = call_set(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			  HexCode, <<"(168)">>),
    ?assertEqual(DSetValue1, ?VALUE_STATEP),

    %% Call contract get function by Alice.
    AGetValue2 = call_get(NodeName, APubkey, APrivkey,
			  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 168 } = decode_data(<<"int">>, AGetValue2),

    %% ct:pal("A Balances ~p, ~p\n", [ABal0,get_balance(APubkey)]),
    %% ct:pal("B Balances ~p, ~p\n", [BBal0,get_balance(BPubkey)]),
    %% ct:pal("C Balances ~p, ~p\n", [CBal0,get_balance(CPubkey)]),
    %% ct:pal("D Balances ~p, ~p\n", [DBal0,get_balance(DPubkey)]),

    ok.

call_get(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedGetData) ->
    {Value,_Return} = call_encoded(NodeName, Pubkey, Privkey,
				   EncodedContractPubkey, EncodedGetData),
    Value.

call_set(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode, SetVal) ->
    {Value,_Return} = call_func(NodeName, Pubkey, Privkey,
				EncodedContractPubkey, HexCode,
				<<"set">>, SetVal),
    Value.

%% counter_contract(Config)
%%  Create the Counter contract by acc_b, tick it by acc_a and then
%%  check value by acc_a.

counter_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
                 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
                 priv_key := BPrivkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    _ABal0 = ensure_balance(APubkey, 50000),
    _BBal0 = ensure_balance(BPubkey, 50000),

    %% Compile contract "counter.aes"
    ContractString = aeso_test_utils:read_contract("counter"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract, owned by Bert.
    {EncodedContractPubkey,_,_} = create_contract(NodeName, BPubkey, BPrivkey,
						  HexCode, <<"(21)">>),

    %% Setup call to tick which can be used by all calls.
    {ok,EncodedTickData} = aect_sophia:encode_call_data(HexCode,
						       <<"tick">>, <<"()">>),

    %% Setup call to get which can be used by all calls.
    {ok,EncodedGetData} = aect_sophia:encode_call_data(HexCode,
						       <<"get">>, <<"()">>),

    %% Call contract get function by Bert.
    {BGetValue1,_} = call_encoded(NodeName, BPubkey, BPrivkey,
				  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 21} = decode_data(<<"int">>, BGetValue1),

    %% Call contract tick function 5 times by Alice.
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),

    %% Call contract get function by Bert and check we have 26 ticks.
    {BGetValue2,_} = call_encoded(NodeName, BPubkey, BPrivkey,
				  EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 26 } = decode_data(<<"int">>, BGetValue2),

    ok.

%% maps_contract(Config)
%%  Check the Maps contract but we are still working on it.

maps_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
		 priv_key := BPrivkey},
      acc_c := #{pub_key := CPubkey,
		 priv_key := CPrivkey},
      acc_d := #{pub_key := DPubkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    ABal0 = ensure_balance(APubkey, 500000),
    BBal0 = ensure_balance(BPubkey, 500000),
    CBal0 = ensure_balance(CPubkey, 500000),
    DBal0 = ensure_balance(DPubkey, 500000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "maps.aes" but a simple test first.
    ContractString = aeso_test_utils:read_contract("maps"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract owned by Alice.
    {EncodedContractPubkey,_,_} =
	create_contract(NodeName, APubkey, APrivkey, HexCode, <<"()">>),

    %% get_state
    ct:pal("Calling get_state\n"),
    GetState1 = call_get_state(NodeName, BPubkey, BPrivkey,
			       EncodedContractPubkey, HexCode),
    ct:pal("Get state 1 ~p\n", [GetState1]),

    %% Set state {[k] = v}
    call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey, HexCode,
			      <<"map_state_i">>, <<"()">>),
    call_func(NodeName, CPubkey, CPrivkey, EncodedContractPubkey, HexCode,
			      <<"map_state_s">>, <<"()">>),

    %% get_state
    ct:pal("Calling get_state\n"),
    GetState2 = call_get_state(NodeName, APubkey, APrivkey,
			       EncodedContractPubkey, HexCode),
    ct:pal("Get state 2 ~p\n", [GetState2]),

    ok.

call_get_state(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode) ->
    StateType = <<"( map(int, (int, int)), map(string, (int, int)) )">>,
    {GSValue,_} = call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
			    HexCode, <<"get_state">>, <<"()">>),
    #{<<"value">> := GetState} = decode_data(StateType, GSValue),
    GetState.

%% enironment_contract(Config)
%%  Check the Environment contract. We don't always check values and
%%  the nested calls don't seem to work yet.

environment_contract(Config) ->
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

    %% Compile contract "environment.aes"
    ContractString = aeso_test_utils:read_contract("environment"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    ContractBalance = 10000,

    %% Initialise contract owned by Alice setting balance to 10000.
    {EncodedContractPubkey,_,_} =
	create_contract(NodeName, APubkey, APrivkey,
			HexCode, <<"()">>, #{amount => ContractBalance}),

    ABal1 = get_balance(APubkey),

    %% Address.
    ct:pal("Calling contract_address\n"),
    call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey, HexCode,
	      <<"contract_address">>, <<"()">>),
    %% ct:pal("Calling nested_address\n"),
    %% call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey, HexCode,
    %% 	      <<"nested_address">>,
    %% 	      list_to_binary([$(,aect_utils:hex_bytes(BPubkey),$)])),

    %% Balance.
    ct:pal("Calling contract_balance\n"),
    {CBValue,_} = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			    HexCode, <<"contract_balance">>, <<"()">>),
    #{<<"value">> := ContractBalance} = decode_data(<<"int">>, CBValue),

    %% Origin.
    ct:pal("Calling call_origin\n"),
    call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
	      HexCode, <<"call_origin">>, <<"()">>),

    %% ct:pal("Calling nested_origin\n"),
    %% NestedOrigin = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
    %% 			     HexCode, <<"nested_origin">>, <<"()">>),

    %% Caller.
    ct:pal("Calling call_caller\n"),
    call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
	      HexCode, <<"call_caller">>, <<"()">>),
    ct:pal("Calling nested_caller\n"),
    call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
	      HexCode, <<"nested_caller">>, <<"()">>),

    %% Value.
    ct:pal("Calling call_value\n"),
    {CVValue,_} = call_func(NodeName, BPubkey, BPrivkey,
			    EncodedContractPubkey,
			    HexCode, <<"call_value">>, <<"()">>),
    #{<<"value">> := CallValue} = decode_data(<<"int">>, CVValue),
    ct:pal("Call value ~p\n", [CallValue]),
    %% ct:pal("Calling nested_value\n"),
    %% NestedValue = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
    %% 			    HexCode, <<"nested_value">>, <<"(42)">>),
    %% ct:pal("Nested value ~p\n", [NestedValue]),

    %% Gas price.
    ct:pal("Calling call_gas_price\n"),
    {GPValue,_} = call_func(NodeName, BPubkey, BPrivkey,
			    EncodedContractPubkey,
			    HexCode, <<"call_gas_price">>, <<"()">>),
    #{<<"value">> := GasPrice} = decode_data(<<"int">>, GPValue),
    ct:pal("Gas price ~p\n", [GasPrice]),

    %% Account balances.
    ct:pal("Calling get_balance twice\n"),
    {BBalValue,_} = call_func(NodeName, BPubkey, BPrivkey,
			      EncodedContractPubkey,
			      HexCode, <<"get_balance">>,
			      args_to_binary([BPubkey])),
    #{<<"value">> := BBalance} = decode_data(<<"int">>, BBalValue),
    ct:pal("Balance B ~p\n", [BBalance]),

    {DBalValue,_} = call_func(NodeName, BPubkey, BPrivkey,
			      EncodedContractPubkey,
			      HexCode, <<"get_balance">>,
			      args_to_binary([DPubkey])),
    #{<<"value">> := DBalance} = decode_data(<<"int">>, DBalValue),
    ct:pal("Balance D ~p\n", [DBalance]),

    %% Block hash.
    ct:pal("Calling block_hash\n"),
    {BHValue,_} = call_func(NodeName, CPubkey, CPrivkey,
			    EncodedContractPubkey,
			    HexCode, <<"block_hash">>, <<"(21)">>),
    #{<<"value">> := BlockHash} = decode_data(<<"int">>, BHValue),
    ct:pal("Block hash ~p\n", [BlockHash]),

    %% Coinbase.
    %% ct:pal("Calling coinbase\n"),
    %% call_func(NodeName, CPubkey, CPrivkey, EncodedContractPubkey, HexCode,
    %% 	      <<"coinbase">>, <<"()">>),

    %% Block timestamp.
    %% ct:pal("Calling timestamp\n"),
    %% call_func(NodeName, CPubkey, CPrivkey, EncodedContractPubkey, HexCode,
    %% 	      <<"timestamp">>, <<"()">>),

    %% Block height.
    ct:pal("Calling block_height\n"),
    {HeightValue,_} = call_func(NodeName, DPubkey, DPrivkey,
				EncodedContractPubkey,
				HexCode, <<"block_height">>, <<"()">>),
    #{<<"value">> := BlockHeight} = decode_data(<<"int">>, HeightValue),
    ct:pal("Block height ~p\n", [BlockHeight]),

    %% Difficulty.
    ct:pal("Calling difficulty\n"),
    {DiffValue,_} = call_func(NodeName, DPubkey, DPrivkey,
			      EncodedContractPubkey,
			      HexCode, <<"difficulty">>, <<"()">>),
    #{<<"value">> := Difficulty} = decode_data(<<"int">>, DiffValue),
    ct:pal("Difficulty ~p\n", [Difficulty]),

    %% Gas limit.
    ct:pal("Calling gas_limit\n"),
    {GLValue,_} = call_func(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			    HexCode, <<"gas_limit">>, <<"()">>),
    #{<<"value">> := GasLimit} = decode_data(<<"int">>, GLValue),
    ct:pal("Gas limit ~p\n", [GasLimit]),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    ct:pal("A Balances ~p, ~p, ~p\n", [ABal0,ABal1,get_balance(APubkey)]),
    ct:pal("B Balances ~p, ~p\n", [BBal0,get_balance(BPubkey)]),
    ct:pal("C Balances ~p, ~p\n", [CBal0,get_balance(CPubkey)]),
    ct:pal("D Balances ~p, ~p\n", [DBal0,get_balance(DPubkey)]),

    ok.

%% spend_test_contract(Config)
%%  Check the SpendTest contract.

spend_test_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),

    %% Create 2 new accounts, Alice and Bert.
    {APubkey,APrivkey,ATxHash} = new_account(1000000),
    {BPubkey,_BPrivkey,BTxHash} = new_account(2000000),
    MineUntil = fun () ->
			tx_in_chain(ATxHash) and tx_in_chain(BTxHash)
		end,
    {ok,_} = aecore_suite_utils:mine_blocks_until(NodeName, MineUntil, 10),

    %% Compile contract "spend_test.aes"
    ContractString = aeso_test_utils:read_contract("spend_test"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contracts owned by Alice with balance set to 10000 and 20000.
    {EncodedC1Pubkey,DecodedC1Pubkey,_} =
	create_contract(NodeName, APubkey, APrivkey, HexCode,
			<<"()">>, #{amount => 10000}),
    {EncodedC2Pubkey,DecodedC2Pubkey,_} =
	create_contract(NodeName, APubkey, APrivkey, HexCode,
			<<"()">>, #{amount => 20000}),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    %% Alice does all the operations on the contract and spends on Bert.
    %% Check the contract balances.
    {GB1Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			     HexCode, <<"get_balance">>, <<"()">>),
    #{<<"value">> := 10000} = decode_data(<<"int">>, GB1Value),
    {GB2Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC2Pubkey,
			     HexCode, <<"get_balance">>, <<"()">>),
    #{<<"value">> := 20000} = decode_data(<<"int">>, GB2Value),

    %% Spend 15000 on to Bert.
    Sp1Arg = args_to_binary([BPubkey,15000]),
    {Sp1Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC2Pubkey,
			     HexCode, <<"spend">>, Sp1Arg),
    #{<<"value">> := 5000} = decode_data(<<"int">>, Sp1Value),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    %% Check that contract spent it.
    GBO1Arg = args_to_binary([DecodedC2Pubkey]),
    {GBO1Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			      HexCode, <<"get_balance_of">>, GBO1Arg),
    #{<<"value">> := 5000} = decode_data(<<"int">>, GBO1Value),

    %% Check that Bert got it.
    GBO2Arg = args_to_binary([BPubkey]),
    {GBO2Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			      HexCode, <<"get_balance_of">>, GBO2Arg),
    #{<<"value">> := 2015000} = decode_data(<<"int">>, GBO2Value),

    %% Spend 6000 explicitly from contract 1 to Bert.
    SF1Arg = args_to_binary([DecodedC1Pubkey,BPubkey,6000]),
    {SF1Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC2Pubkey,
			     HexCode, <<"spend_from">>, SF1Arg),
    #{<<"value">> := 2021000} = decode_data(<<"int">>, SF1Value),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    %% Check that Bert got it.
    GBO3Arg = args_to_binary([BPubkey]),
    {GBO3Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			      HexCode, <<"get_balance_of">>, GBO3Arg),
    #{<<"value">> := 2021000} = decode_data(<<"int">>, GBO3Value),

    %% Check contract 2 balance.
    GBO4Arg = args_to_binary([DecodedC2Pubkey]),
    {GBO4Value,_} = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			      HexCode, <<"get_balance_of">>, GBO4Arg),
    #{<<"value">> := 5000} = decode_data(<<"int">>, GBO4Value),

    ok.

%% dutch_auction_contract(Config)
%%  Check the DutchAuction contract. We use 3 accounts here, Alice for
%%  setting up the account, Carl as beneficiary and Bert as
%%  bidder. This makes it a bit easier to keep track of the values as
%%  we have gas loses as well.

dutch_auction_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
		 priv_key := BPrivkey},
      acc_c := #{pub_key := CPubkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    ABal0 = ensure_balance(APubkey, 500000),
    BBal0 = ensure_balance(BPubkey, 500000),
    CBal0 = ensure_balance(CPubkey, 500000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "dutch_auction.aes"
    ContractString = aeso_test_utils:read_contract("dutch_auction"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Set auction start amount and decrease per mine and fee.
    StartAmt = 50000,
    Decrease = 500,
    Fee = 100,

    %% Initialise contract owned by Alice with Carl as benficiary.
    InitArgument = args_to_binary([CPubkey,StartAmt,Decrease]),
    {EncodedContractPubkey,_,InitReturn} =
	create_contract(NodeName, APubkey, APrivkey, HexCode, InitArgument),
    #{<<"height">> := Height0} = InitReturn,

    %% Mine 10 times to decrement value.
    {ok,_} = aecore_suite_utils:mine_blocks(NodeName, 10),

    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    CBal1 = get_balance(CPubkey),

    %% Call the contract bid function by Bert.
    {_,BidReturn} = call_func(NodeName, BPubkey, BPrivkey,
			      EncodedContractPubkey, HexCode,
			      <<"bid">>, <<"()">>,
			      #{amount => 100000,fee => Fee}),
    #{<<"gas_used">> := GasUsed,<<"height">> := Height1} = BidReturn,

    %% Set the cost from the amount, decrease and diff in height.
    Cost = StartAmt - (Height1 - Height0) * Decrease,

    BBal2 = get_balance(BPubkey),
    CBal2 = get_balance(CPubkey),

    %% ct:pal("B Balances ~p, ~p, ~p\n", [BBal0,BBal1,BBal2]),
    %% ct:pal("Cost ~p, GasUsed ~p, Fee ~p\n", [Cost,GasUsed,Fee]),
    %% ct:pal("C Balances ~p, ~p, ~p\n", [CBal0,CBal1,CBal2]),

    %% Check that the balances are correct, don't forget the gas and the fee.
    BBal2 = BBal1 - Cost - GasUsed - Fee,
    CBal2 = CBal1 + Cost,

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

decode_data(Type, EncodedData) ->
    {ok,200,#{<<"data">> := DecodedData}} =
	 get_contract_decode_data(#{'sophia-type' => Type,
				    data => EncodedData}),
    DecodedData.

%% Contract interface functions.

%% create_contract(NodeName, Pubkey, Privkey, HexCode, InitArgument) ->
%%     {EncodedContractPubkey,DecodedContractPubkey,InitReturn}.
%%  Create contract and mine blocks until in chain.

create_contract(NodeName, Pubkey, Privkey, HexCode, InitArgument) ->
    create_contract(NodeName, Pubkey, Privkey, HexCode, InitArgument, #{}).

create_contract(NodeName, Pubkey, Privkey, HexCode, InitArgument, CallerSet) ->
    {ok,EncodedInitData} = aect_sophia:encode_call_data(HexCode, <<"init">>,
                                                        InitArgument),
    {ContractCreateTxHash,EncodedContractPubkey,DecodedContractPubkey} =
        contract_create_tx(Pubkey, Privkey, HexCode, EncodedInitData, CallerSet),

    %% Mine blocks and check that it is in the chain.
    MineUntil = fun () -> tx_in_chain(ContractCreateTxHash) end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntil, 10),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Get value of last call.
    {ok,200,InitReturn} = get_contract_call_object(ContractCreateTxHash),
    ct:pal("Init return ~p\n", [InitReturn]),

    {EncodedContractPubkey,DecodedContractPubkey,InitReturn}.

%% call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey, Code,
%%           Function, Arguments)
%% call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey, Code,
%%           Function, Arguments, CallerSet)
%% call_encoded(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedData)
%% call_encoded(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedData,
%%              CallerSet)
%%  Call contract function with arguments and mine blocks until in chain.

call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode,
	  Function, Arguments) ->
    call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode,
	      Function, Arguments, #{}).

call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode,
	  Function, Arguments, CallerSet) ->
    %% Call function contract.
    {ok,EncodedCallData} = aect_sophia:encode_call_data(HexCode,
							Function, Arguments),
    call_encoded(NodeName, Pubkey, Privkey, EncodedContractPubkey,
		 EncodedCallData, CallerSet).

call_encoded(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedData) ->
    call_encoded(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedData,
		 #{}).

call_encoded(NodeName, Pubkey, Privkey, EncodedContractPubkey, EncodedData,
	     CallerSet) ->
    %% Call get contract.
    ContractCallTxHash = contract_call_tx(Pubkey, Privkey,
					  EncodedContractPubkey,
					  EncodedData, CallerSet),
    %% Mine blocks and check that it is in the chain.
    MineUntil = fun () -> tx_in_chain(ContractCallTxHash) end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntil, 10),
    ?assert(tx_in_chain(ContractCallTxHash)),

    %% Get the call object and return value.
    {ok,200,CallReturn} = get_contract_call_object(ContractCallTxHash),
    ct:pal("Call return ~p\n", [CallReturn]),

    #{<<"return_type">> := <<"ok">>,<<"return_value">> := Value} = CallReturn,
    {Value,CallReturn}.

contract_create_tx(Pubkey, Privkey, HexCode, EncodedInitData) ->
    contract_create_tx(Pubkey, Privkey, HexCode, EncodedInitData, #{}).

contract_create_tx(Pubkey, Privkey, HexCode, EncodedInitData, CallerSet) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_nonce(Address),
    Nonce = Nonce0 + 1,

    %% The default init contract.
    ContractInitEncoded0 = #{ owner => Address,
			      code => HexCode,
			      vm_version => 1,	%?AEVM_01_Sophia_01
			      deposit => 2,
			      amount => 0,	%Initial balance
			      gas => 20000,	%May need a lot of gas
			      gas_price => 1,
			      fee => 1,
			      nonce => Nonce,
			      call_data => EncodedInitData,
			      payload => <<"create contract">>},
    ContractInitEncoded = maps:merge(ContractInitEncoded0, CallerSet),
    sign_and_post_create_tx(Privkey, ContractInitEncoded).


contract_call_tx(Pubkey, Privkey, EncodedContractPubkey, EncodedCallData) ->
    contract_call_tx(Pubkey, Privkey, EncodedContractPubkey, EncodedCallData, #{}).

contract_call_tx(Pubkey, Privkey, EncodedContractPubkey, EncodedCallData, CallerSet) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_nonce(Address),
    Nonce = Nonce0 + 1,
 
    %% The default call contract.
    ContractCallEncoded0 = #{ caller => Address,
			      contract => EncodedContractPubkey,
			      vm_version => 1,	%?AEVM_01_Sophia_01
			      amount => 0,
			      gas => 30000,	%May need a lot of gas
			      gas_price => 1,
			      fee => 1,
			      nonce => Nonce,
			      call_data => EncodedCallData,
			      payload => <<"call function">>},
    ContractCallEncoded = maps:merge(ContractCallEncoded0, CallerSet),
    sign_and_post_call_tx(Privkey, ContractCallEncoded).

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

get_contract_call(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/call", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "tx/"++binary_to_list(TxHash)++"/contract-call", []).

get_contract_decode_data(Request) ->
    Host = external_address(),
    http_request(Host, post, "contract/decode-data", Request).

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

get_balance_at_top(EncodedPubKey) ->
    get_balance(EncodedPubKey, []).

get_balance(EncodedPubKey, Params) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/balance",
                 Params).

get_nonce(EncodedPubKey) ->
    get_nonce(EncodedPubKey, []).

get_nonce(EncodedPubKey, Params) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/nonce", Params).

post_tx(TxSerialized) ->
    Host = external_address(),
    http_request(Host, post, "tx", #{tx => TxSerialized}).

tx_encoding_param(default) -> #{};
tx_encoding_param(json) -> #{tx_encoding => <<"json">>};
tx_encoding_param(message_pack) -> #{tx_encoding => <<"message_pack">>}.

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
                Result = case iolist_to_binary(Body) of
			     <<>> -> #{};
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

new_account(Balance) ->
    {Pubkey,Privkey} = generate_key_pair(),
    Fee = 1,
    {ok,200,#{<<"tx_hash">> := TxHash}} = post_spend_tx(Pubkey, Balance, Fee),
    {Pubkey,Privkey,TxHash}.

%% spend_tokens(SenderPubkey, SenderPrivkey, Recipient, Amount, Fee) ->
%% spend_tokens(SenderPubkey, SenderPrivkey, Recipient, Amount, Fee, CallerSet) ->
%%     TxHash
%%  This is based on post_correct_tx/1 in aehttp_integration_SUITE.

spend_tokens(SenderPub, SenderPriv, Recip, Amount, Fee) ->
    DefaultSet = #{ttl => 0},                   %Defaut fields set by caller
    spend_tokens(SenderPub, SenderPriv, Recip, Amount, Fee, DefaultSet).

spend_tokens(SenderPub, SenderPriv, Recip, Amount, Fee, CallerSet) ->
    %% Generate a nonce.
    Address = aec_base58c:encode(account_pubkey, SenderPub),
    {ok,200,#{<<"nonce">> := Nonce0}} = get_nonce(Address),
    Nonce = Nonce0 + 1,

    Params0 = #{sender => aec_id:create(account, SenderPub),
                recipient => aec_id:create(account, Recip),
                amount => Amount,
                fee => Fee,
                nonce => Nonce,
                payload => <<"spend tokens">>},
    Params1 = maps:merge(Params0, CallerSet),   %Set caller defaults
    {ok, UnsignedTx} = aec_spend_tx:new(Params1),
    SignedTx = aec_test_utils:sign_tx(UnsignedTx, SenderPriv),
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
    TxHash = sign_and_post_tx(Privkey, EncodedUnsignedTx),
    {TxHash,EncodedPubkey,DecodedPubkey}.

sign_and_post_call_tx(Privkey, CallEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx}} = get_contract_call(CallEncoded),
    sign_and_post_tx(Privkey, EncodedUnsignedTx).

sign_and_post_tx(PrivKey, EncodedUnsignedTx) ->
    {ok,SerializedUnsignedTx} = aec_base58c:safe_decode(transaction,
							EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aec_test_utils:sign_tx(UnsignedTx, PrivKey),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    SendTx = aec_base58c:encode(transaction, SerializedTx),
    {ok,200,#{<<"tx_hash">> := TxHash}} = post_tx(SendTx),
    TxHash.

tx_in_chain(TxHash) ->
    case get_tx(TxHash, json) of
        {ok, 200, #{<<"transaction">> := #{<<"block_hash">> := <<"none">>}}} ->
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

%% args_to_list(Args) -> string().
%%  Take a list of arguments in "erlang format" and generate an
%%  argument binary string.

args_to_binary(Args) ->
    list_to_binary([$(,args_to_list(Args),$)]).

args_to_list([A]) -> [arg_to_list(A)];		%The last one
args_to_list([A1|Rest]) ->
    [arg_to_list(A1),$,|args_to_list(Rest)];
args_to_list([]) -> [].

%%arg_to_list(<<N:256>>) -> integer_to_list(N);
arg_to_list(N) when is_integer(N) -> integer_to_list(N);
arg_to_list(<<$",_/binary>>=B) -> 		%A string
    binary_to_list(B);
arg_to_list(B) when is_binary(B) ->		%A key
    aect_utils:hex_bytes(B);
arg_to_list(T) when is_tuple(T) ->
    [$(,args_to_list(tuple_to_list(T)),$)];
arg_to_list(M) when is_map(M) ->
    [${,map_to_list(maps:to_list(M)),$}].

map_to_list([{K,V}]) -> [$[,arg_to_list(K),"] = ",arg_to_list(V)];
map_to_list([{K,V},Fields]) ->
    [$[,arg_to_list(K),"] = ",arg_to_list(V),$,|map_to_list(Fields)];
map_to_list([]) -> [].
