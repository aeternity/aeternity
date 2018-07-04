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

    {ok, _} = aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Prepare accounts, Alice, Bert, Carl and Diana.

    StartAmt = 5000,
    {APubkey, APrivkey} = new_account(StartAmt),
    {BPubkey, BPrivkey} = new_account(StartAmt),
    {CPubkey, CPrivkey} = new_account(StartAmt),
    {DPubkey, DPrivkey} = new_account(StartAmt),

    {ok, [Block|_]} = aecore_suite_utils:mine_blocks(NodeName, 2),
    ct:pal("Block ~p\n", [aec_blocks:txs(Block)]),

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

    ct:pal("Top 1 ~p\n", [get_top()]),

    %% Check initial balances.
    ABal0 = get_balance(APubkey),
    BBal0 = get_balance(BPubkey),
    ct:pal("Balances 0: ~p, ~p\n", [ABal0,BBal0]),

    %% Add tokens to both accounts.
    {ok,200,_} = post_spend_tx(APubkey, 500, 1),
    {ok,200,_} = post_spend_tx(BPubkey, 500, 1),
    aecore_suite_utils:mine_blocks(NodeName, 2),

    %% Get balances after mining.
    ABal1 = get_balance(APubkey),
    BBal1 = get_balance(BPubkey),
    ct:pal("Balances 1: ~p, ~p\n", [ABal1,BBal1]),

    %% Transfer money from Alice to Bert.
    TxHash = spend_tokens(APubkey, APrivkey, BPubkey, 200, 5),
    MineUntil = fun () -> tx_in_block(TxHash) end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntil, 10),

    ct:pal("Top 2 ~p\n", [get_top()]),

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
    TxHash = spend_tokens(APubkey, APrivkey, BPubkey, ABal1 + 200, 5),
    {ok,[_,_]} = aecore_suite_utils:mine_blocks(NodeName, 2),

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
    MineUntil = fun () -> tx_in_chain(TxHash) end,
    aecore_suite_utils:mine_blocks_until(NodeName, MineUntil, 10),

    %% Check the balance to see what happened.
    ABal3 = get_balance(APubkey),
    BBal3 = get_balance(BPubkey),
    ct:pal("Balances 3: ~p, ~p\n", [ABal3,BBal3]),

    %% Check that tx has succeeded.
    ?assert(tx_in_block(TxHash)),

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
    CReturn = call_encoded(NodeName, CPubkey, CPrivkey, EncodedContractPubkey,
			   EncodedMainData),
    #{<<"value">> := 42} = decode_data(<<"int">>, CReturn),

    %% Call contract main function by Diana.
    DReturn = call_encoded(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			   EncodedMainData),
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

    %% Setup call to get which can be used by all calls.
    {ok,EncodedGetData} = aect_sophia:encode_call_data(HexCode,
						       <<"get">>, <<"()">>),

    %% Call contract get function by Alice.
    AGetValue1 = call_encoded(NodeName, APubkey, APrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 21} = decode_data(<<"int">>, AGetValue1),

    %% Call contract set function by Alice.
    ASetValue1 = call_set(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			  HexCode, <<"(42)">>),
    ?assertEqual(ASetValue1, ?VALUE_STATEP),

    %% Call contract get function by Bert.
    BGetValue1 = call_encoded(NodeName, BPubkey, BPrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 42 } = decode_data(<<"int">>, BGetValue1),

    %% Call contract set function by Bert.
    BSetValue1 = call_set(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			  HexCode, <<"(84)">>),
    ?assertEqual(BSetValue1, ?VALUE_STATEP),

    %% Call contract get function by Carl.
    CGetValue1 = call_encoded(NodeName, CPubkey, CPrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 84 } = decode_data(<<"int">>, CGetValue1),

    %% Call contract set function by Carl.
    CSetValue1 = call_set(NodeName, CPubkey, CPrivkey, EncodedContractPubkey,
			  HexCode, <<"(126)">>),
    ?assertEqual(CSetValue1, ?VALUE_STATEP),

    %% Call contract get function by Diana.
    DGetValue1 = call_encoded(NodeName, DPubkey, DPrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 126 } = decode_data(<<"int">>, DGetValue1),

    %% Call contract set function by Diana.
    DSetValue1 = call_set(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			  HexCode, <<"(168)">>),
    ?assertEqual(DSetValue1, ?VALUE_STATEP),

    %% Call contract get function by Alice.
    AGetValue2 = call_encoded(NodeName, APubkey, APrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 168 } = decode_data(<<"int">>, AGetValue2),

    ct:pal("A Balances ~p, ~p\n", [ABal0,get_balance(APubkey)]),
    ct:pal("B Balances ~p, ~p\n", [BBal0,get_balance(BPubkey)]),
    ct:pal("C Balances ~p, ~p\n", [CBal0,get_balance(CPubkey)]),
    ct:pal("D Balances ~p, ~p\n", [DBal0,get_balance(DPubkey)]),

    ok.

call_set(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode, SetVal) ->
    call_func(NodeName, Pubkey, Privkey, EncodedContractPubkey, HexCode,
	      <<"set">>, SetVal).

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
    BGetValue1 = call_encoded(NodeName, BPubkey, BPrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 21} = decode_data(<<"int">>, BGetValue1),

    %% Call contract tick function 5 times by Alice.
    TGetValue1 = call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    ct:pal("Tick val ~p\n", [TGetValue1]),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),
    call_encoded(NodeName, APubkey, APrivkey, EncodedContractPubkey,
		 EncodedTickData),

    %% Call contract get function by Bert and check we have 26 ticks.
    BGetValue2 = call_encoded(NodeName, BPubkey, BPrivkey,
			      EncodedContractPubkey, EncodedGetData),
    #{<<"value">> := 26 } = decode_data(<<"int">>, BGetValue2),

    ok.

%% maps_contract(Config)
%%  Check the Maps contract.

maps_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
		 priv_key := BPrivkey},
      acc_c := #{pub_key := CPubkey},
      acc_d := #{pub_key := DPubkey}} = proplists:get_value(accounts, Config),

    %% Make sure accounts have enough tokens.
    ABal0 = ensure_balance(APubkey, 50000),
    BBal0 = ensure_balance(BPubkey, 50000),
    CBal0 = ensure_balance(CPubkey, 50000),
    DBal0 = ensure_balance(DPubkey, 50000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "maps.aes" but a simple test first.
    %% ContractString = aeso_test_utils:read_contract("maps"),
    ContractString =
	"contract Maps =\n"
	"  function main(x : int, y : int, z : string) : map(int, string) =\n"
	"    { [x] = z, [y] = z }\n",
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract owned by Alice.
    {EncodedContractPubkey,_,_} = create_contract(NodeName, APubkey, APrivkey,
						  HexCode, <<"()">>),

    %% Call contract get_state function by Bert.
    StateRet = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			 HexCode, <<"main">>, <<"(42,84,\"howdy\")">>),
    #{<<"value">> := StateVal} = decode_data(<<"map(int, string)">>, StateRet),

    ct:pal("State ~p\n", [StateVal]),

    ok.

%% enironment_contract(Config)
%%  Check the Environment contract.

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

    %% Initialise contract owned by Alice.
    {EncodedContractPubkey,_,_} =
	create_contract(NodeName, APubkey, APrivkey,
			HexCode, <<"()">>, #{amount => 10000}),

    ABal1 = get_balance(APubkey),

    %% Address.
    ct:pal("Calling contract_address\n"),
    call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey, HexCode,
	      <<"contract_address">>, <<"()">>),
    ct:pal("Calling nested_address\n"),
    %% call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey, HexCode,
    %% 	      <<"nested_address">>,
    %% 	      list_to_binary([$(,aect_utils:hex_bytes(BPubkey),$)])),

    %% Balance.
    ct:pal("Calling contract_balance\n"),
    CBReturn = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			 HexCode, <<"contract_balance">>, <<"()">>),
    #{<<"value">> := 10000} = decode_data(<<"int">>, CBReturn),

    %% Origin.
    ct:pal("Calling call_origin\n"),
    CallOrigin = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			   HexCode, <<"call_origin">>, <<"()">>),
    ct:pal("Calling nested_origin\n"),
    %% NestedOrigin = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
    %% 			     HexCode, <<"nested_origin">>, <<"()">>),

    %% Caller.
    ct:pal("Calling call_caller\n"),
    CallCaller = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			   HexCode, <<"call_caller">>, <<"()">>),
    ct:pal("Calling nested_caller\n"),
    NestedCaller = call_func(NodeName, APubkey, APrivkey, EncodedContractPubkey,
			     HexCode, <<"nested_caller">>, <<"()">>),

    %% Value.
    ct:pal("Calling call_value\n"),
    CallReturn = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			   HexCode, <<"call_value">>, <<"()">>),
    #{<<"value">> := CallValue} = decode_data(<<"int">>, CallReturn),
    ct:pal("Call value ~p\n", [CallValue]),
    %% ct:pal("Calling nested_value\n"),
    %% NestedValue = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
    %% 			    HexCode, <<"nested_value">>, <<"(42)">>),
    %% ct:pal("Nested value ~p\n", [NestedValue]),

    %% Gas price.
    ct:pal("Calling call_gas_price\n"),
    GasPriceRet = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			    HexCode, <<"call_gas_price">>, <<"()">>),
    #{<<"value">> := GasPriceVal} = decode_data(<<"int">>, GasPriceRet),
    ct:pal("Gas price ~p\n", [GasPriceVal]),

    %% Account balances.
    ct:pal("Calling get_balance twice\n"),
    BalRetB = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			HexCode, <<"get_balance">>,
			args_to_binary([BPubkey])),
    #{<<"value">> := BalValB} = decode_data(<<"int">>, BalRetB),
    ct:pal("Balance B ~p\n", [BalValB]),
    BalRetD = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			HexCode, <<"get_balance">>,
			args_to_binary([DPubkey])),
    #{<<"value">> := BalValD} = decode_data(<<"int">>, BalRetD),
    ct:pal("Balance D ~p\n", [BalValD]),

    %% Block hash.
    ct:pal("Calling block_hash\n"),
    BlockHash = call_func(NodeName, CPubkey, CPrivkey, EncodedContractPubkey,
			  HexCode, <<"block_hash">>, <<"(21)">>),
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
    HeightRet = call_func(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			  HexCode, <<"block_height">>, <<"()">>),
    #{<<"value">> := BlockHeight} = decode_data(<<"int">>, HeightRet),
    ct:pal("Block height ~p\n", [BlockHeight]),

    %% Difficulty.
    ct:pal("Calling difficulty\n"),
    DiffReturn = call_func(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			   HexCode, <<"difficulty">>, <<"()">>),
    #{<<"value">> := Difficulty} = decode_data(<<"int">>, DiffReturn),
    ct:pal("Difficulty ~p\n", [Difficulty]),

    %% Gas limit.
    ct:pal("Calling gas_limit\n"),
    GLReturn = call_func(NodeName, DPubkey, DPrivkey, EncodedContractPubkey,
			 HexCode, <<"gas_limit">>, <<"()">>),
    #{<<"value">> := GasLimit} = decode_data(<<"int">>, GLReturn),
    ct:pal("Gas limit ~p\n", [GasLimit]),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    ct:pal("A Balances ~p, ~p ~p\n", [ABal0,ABal1,get_balance(APubkey)]),
    ct:pal("B Balances ~p, ~p\n", [BBal0,get_balance(BPubkey)]),
    ct:pal("C Balances ~p, ~p\n", [CBal0,get_balance(CPubkey)]),
    ct:pal("D Balances ~p, ~p\n", [DBal0,get_balance(DPubkey)]),

    ok.

%% spend_test_contract(Config)
%%  Check the SpendTest contract.

spend_test_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),

    %% Create 2 new accounts, Alice and Bert.
    {APubkey,APrivkey} = new_account(1000000),
    {BPubkey,BPrivkey} = new_account(2000000),
    {ok,_} = aecore_suite_utils:mine_blocks(NodeName, 3),

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

    %% Alice does all the operations on the contract and Bert.
    %% Check the contract balances.
    GB1Ret = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
		       HexCode, <<"get_balance">>, <<"()">>),
    #{<<"value">> := 10000} = decode_data(<<"int">>, GB1Ret),
    GB2Ret = call_func(NodeName, APubkey, APrivkey, EncodedC2Pubkey,
		       HexCode, <<"get_balance">>, <<"()">>),
    #{<<"value">> := 20000} = decode_data(<<"int">>, GB2Ret),

    %% Spend 15000 on to Bert.
    Sp1Arg = args_to_binary([BPubkey,15000]),
    Sp1Ret = call_func(NodeName, APubkey, APrivkey, EncodedC2Pubkey,
		       HexCode, <<"spend">>, Sp1Arg),
    #{<<"value">> := 5000} = decode_data(<<"int">>, Sp1Ret),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    %% Check that contract spent it.
    GBO1Arg = args_to_binary([DecodedC2Pubkey]),
    GBO1Ret = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			HexCode, <<"get_balance_of">>, GBO1Arg),
    #{<<"value">> := 5000} = decode_data(<<"int">>, GBO1Ret),

    %% Check that Bert got it.
    GBO2Arg = args_to_binary([BPubkey]),
    GBO2Ret = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			HexCode, <<"get_balance_of">>, GBO2Arg),
    #{<<"value">> := 2015000} = decode_data(<<"int">>, GBO2Ret),

    %% Spend 6000 explicitly from contract 1 to Bert.
    SF1Arg = args_to_binary([DecodedC1Pubkey,BPubkey,6000]),
    SF1Ret = call_func(NodeName, APubkey, APrivkey, EncodedC2Pubkey,
		       HexCode, <<"spend_from">>, SF1Arg),
    #{<<"value">> := 2021000} = decode_data(<<"int">>, SF1Ret),

    aecore_suite_utils:mine_blocks(NodeName, 3),

    %% Check that Bert got it.
    GBO3Arg = args_to_binary([BPubkey]),
    GBO3Ret = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			HexCode, <<"get_balance_of">>, GBO3Arg),
    #{<<"value">> := 2021000} = decode_data(<<"int">>, GBO3Ret),

    %% Check contract 2 balance.
    GBO4Arg = args_to_binary([DecodedC2Pubkey]),
    GBO4Ret = call_func(NodeName, APubkey, APrivkey, EncodedC1Pubkey,
			HexCode, <<"get_balance_of">>, GBO4Arg),
    #{<<"value">> := 5000} = decode_data(<<"int">>, GBO4Ret),

    ok.

%% dutch_auction_contract(Config)
%%  Check the DutchAuction contract. This doesn't work yet.

dutch_auction_contract(Config) ->
    NodeName = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APubkey,
		 priv_key := APrivkey},
      acc_b := #{pub_key := BPubkey,
		 priv_key := BPrivkey}} = proplists:get_value(accounts, Config),

    ct:pal("APubkey ~p\n", [APubkey]),

    %% Make sure accounts have enough tokens.
    ABal0 = ensure_balance(APubkey, 500000),
    BBal0 = ensure_balance(BPubkey, 500000),
    {ok,[_]} = aecore_suite_utils:mine_blocks(NodeName, 1),

    %% Compile contract "dutch_auction.aes"
    ContractString = aeso_test_utils:read_contract("dutch_auction"),
    BinCode = aeso_compiler:from_string(ContractString, []),
    HexCode = aeu_hex:hexstring_encode(BinCode),

    %% Initialise contract owned by Alice.
    InitArgument = args_to_binary([APubkey,50000,500]),
    {EncodedContractPubkey,_,_} = create_contract(NodeName, APubkey, APrivkey,
						  HexCode, InitArgument,
						  #{amount => 50000}),

    ABal1 = get_balance(APubkey),

    %% Call the contract bid function by Bert.
    BidValue = call_func(NodeName, BPubkey, BPrivkey, EncodedContractPubkey,
			 HexCode, <<"bid">>, <<"()">>, #{amount => 50000}),

    ct:pal("Bid value ~p\n", [BidValue]),
    ct:pal("A Balances ~p, ~p, ~p\n", [ABal0,ABal1,get_balance(APubkey)]),
    ct:pal("B Balances ~p, ~p\n", [BBal0,get_balance(BPubkey)]),

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
%%     EncodedContractPubkey.

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
%%  Call contract function with arguments and mine 2 blocks .

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
    Value.

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

%% call_contract_directly(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "contract/call", Data).

get_contract_call(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/call", Data).

%% get_contract_call_compute(Data) ->
%%     Host = external_address(),
%%     http_request(Host, post, "tx/contract/call/compute", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "tx/"++binary_to_list(TxHash)++"/contract-call", []).

get_contract_decode_data(Request) ->
    Host = external_address(),
    http_request(Host, post, "contract/decode-data", Request).

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

get_nonce(EncodedPubKey) ->
    get_nonce(EncodedPubKey, []).

get_nonce(EncodedPubKey, Params) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/nonce", Params).

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

populate_block(Txs) ->
    lists:foreach(
      fun (#{recipient := R, amount := A, fee := F}) ->
	      {ok, 200, #{<<"tx_hash">> := TxHash}} = post_spend_tx(R, A, F),
	      TxHash
      end,
      maps:get(spend_txs, Txs, [])),
    ok.

new_account(Balance) ->
    {Pubkey,Privkey} = generate_key_pair(),
    Fee = 1,
    {ok,200,_} = post_spend_tx(Pubkey, Balance, Fee),
    {Pubkey,Privkey}.

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

    Params0 = #{sender => SenderPub,
                recipient => Recip,
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

tx_in_block(TxHash) -> tx_in_chain(TxHash).

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

