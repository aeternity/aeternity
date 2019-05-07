-module(aehttp_contracts_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%
%% A single test can be ran using:
%% make ct SUITE=apps/aehttp/test/aehttp_contracts GROUP=contracts TEST=acm_dutch_auction_contract
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/test/aect_sophia_vsn.hrl").


%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

%% Endpoint calls
-export([http_request/4, internal_address/0, external_address/0, new_account/1, rpc/4]).

%% test case exports
%% external endpoints
-export([
         abort_test_contract/1,
         counter_contract/1,
         dutch_auction_contract/1,
         acm_dutch_auction_contract/1,
         environment_contract/1,
         environment_contract_fate/1,
         erc20_token_contract/1,
         factorial_contract/1,
         fundme_contract/1,
         identity_contract/1,
         maps_contract/1,
         polymorphism_test_contract/1,
         simple_storage_contract/1,
         spend_test_contract/1,
         stack_contract/1,
         remote_gas_test_contract/1,
         events_contract/1,
         null/1
        ]).

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).

-define(MAX_MINED_BLOCKS, 20).

all() ->
    [
     {group, contracts},
     {group, fate}
    ].

groups() ->
    [
     {fate, [
             environment_contract_fate
             ]},
     {contracts, [],
      [
       identity_contract,
       abort_test_contract,
       simple_storage_contract,
       counter_contract,
       stack_contract,
       polymorphism_test_contract,
       factorial_contract,
       maps_contract,
       environment_contract,
       spend_test_contract,
       dutch_auction_contract,
       acm_dutch_auction_contract,
       fundme_contract,
       erc20_token_contract,
       remote_gas_test_contract,
       events_contract,
       null                                     %This allows to end with ,
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks}},
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [{symlink_name, "latest.http_contracts"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.



init_for_contracts(Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),

    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ aecore_suite_utils:mine_key_blocks(NodeName, ToMine) || ToMine > 0 ],

    %% Prepare accounts, Alice, Bert, Carl and Diana with balance of 10T.

    StartAmt = 10*1000*1000000 * ?DEFAULT_GAS_PRICE, %That is a lot of tokens!
    {APubkey, APrivkey, STx1} = new_account(StartAmt),
    {BPubkey, BPrivkey, STx2} = new_account(StartAmt),
    {CPubkey, CPrivkey, STx3} = new_account(StartAmt),
    {DPubkey, DPrivkey, STx4} = new_account(StartAmt),

    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(
                                    NodeName, [STx1, STx2, STx3, STx4], ?MAX_MINED_BLOCKS),

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
    [{accounts,Accounts},{node_name,NodeName}|Config].

init_per_group(contracts, Config) -> init_for_contracts(Config);
init_per_group(fate, Config) -> init_for_contracts(Config).

end_per_group(_Group, Config) ->
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    ok.

init_per_testcase(_Case, Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% null(Config)
%%  Does nothing, prints the balances and always succeeds.

null(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub},
      acc_b := #{pub_key := BPub},
      acc_c := #{pub_key := CPub},
      acc_d := #{pub_key := DPub}} = proplists:get_value(accounts, Config),
    ABal = get_balance(APub),
    BBal = get_balance(BPub),
    CBal = get_balance(CPub),
    DBal = get_balance(DPub),
    ct:pal("ABal ~p, BBal ~p, CBal ~p, DBal ~p\n", [ABal,BBal,CBal,DBal]),

    ok.

%% identity_contract(Config)
%%  Create the Identity contract by account acc_c and call by accounts
%%  acc_c and acc_d. Encode create and call data in server.

identity_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_c := #{pub_key := CPub,
                 priv_key := CPriv},
      acc_d := #{pub_key := DPub,
                 priv_key := DPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "identity.aes"
    Contract = compile_test_contract("identity"),

    init_fun_calls(),

    %% Initialise contract, owned by Carl.
    {EncCPub,_,_} = create_contract(Node, CPub, CPriv, Contract, []),

    %% Call contract main function by Carl.
    call_func(CPub, CPriv, EncCPub, Contract, "main", ["42"], {"int", 42}),

    %% Call contract main function by Diana.
    call_func(DPub, DPriv, EncCPub, Contract, "main", ["42"], {"int", 42}),

    force_fun_calls(Node),

    ok.

%% abort_test_contract(Config)
%%  Test the built-in abort function.

abort_test_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv}} = proplists:get_value(accounts, Config),

    %% Compile contracts "abort_test.aes" and "abort_test_int.aes".
    TCode = compile_test_contract("abort_test"),
    ICode = compile_test_contract("abort_test_int"),

    %% Create chain of int contracts to test contract.
    {EncodedTestPub,DecodedTestPub,_} =
        create_contract(Node, APub, APriv, TCode, ["42"]),
    {_EncodedInt1Pub,DecodedInt1Pub,_} =
        create_contract(Node, APub, APriv, ICode,
                        [aeser_api_encoder:encode(contract_pubkey, DecodedTestPub), "42"]),
    {_EncodedInt2Pub,DecodedInt2Pub,_} =
        create_contract(Node, APub, APriv, ICode,
                        [aeser_api_encoder:encode(contract_pubkey, DecodedInt1Pub), "42"]),
    {EncodedInt3Pub,_,_} =
        create_contract(Node, APub, APriv, ICode,
                        [aeser_api_encoder:encode(contract_pubkey, DecodedInt2Pub), "42"]),

    %% This should set state values to 17, 1017, 2017, 3017.
    call_compute_func(Node, APub, APriv, EncodedInt3Pub, ICode, "put_values", ["17"]),
    BeforeList = call_func_decode(Node, APub, APriv, EncodedInt3Pub, ICode,
                                  "get_values", [], "list(int)"),
    ct:pal("Before Values ~p\n", [BeforeList]),

    ABal0 = get_balance(APub),

    %% Do abort where we have put values 42, 142, 242, 342,
    %% then check value in abort_test contract, should still be 3017!
    %% Check that we get the abort string back.
    %% First check when we call the full contract call stack.
    AbortString = "\"yogi bear\"",
    AbortBin    = <<"yogi bear">>,
    GivenGas = 100000,
    {RevertValue1, Return1} =
        revert_call_compute_func(Node, APub, APriv, EncodedInt3Pub, ICode,
                                 "do_abort", ["42", AbortString], #{gas => GivenGas}),
    #{<<"value">> := AbortBin} = decode_data(<<"string">>, RevertValue1),
    #{<<"gas_used">> := GasUsed1} = Return1,
    ?assert(GasUsed1 < GivenGas), %% Make sure we actually saved some gas

    %% Also check that we get the same behaviour when calling directly to the
    %% leaf contract.
    {RevertValue2, Return2} =
        revert_call_compute_func(Node, APub, APriv, EncodedTestPub, TCode,
                                 "do_abort", ["42", AbortString], #{gas => GivenGas}),
    #{<<"value">> := AbortBin} = decode_data("string", RevertValue2),
    #{<<"gas_used">> := GasUsed2} = Return2,
    ?assert(GasUsed2 < GivenGas), %% Make sure we actually saved some gas

    ABal1 = get_balance(APub),

    %% The contract values should be the same, 17, 1017, 2017, 3017.
    AfterList = call_func_decode(Node, APub, APriv, EncodedInt3Pub, ICode,
                                 "get_values", [], "list(int)"),
    ct:pal("After Values ~p\n", [AfterList]),
    AfterList = BeforeList,

    ct:pal("Balance ~p ~p \n", [ABal0,ABal1]),

    ok.

%% simple_storage_contract(Config)
%%  Create the SimpleStorage contract by acc_a and test and set its
%%  state data by acc_a, acc_b, acc_c and finally by acc_d.

simple_storage_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "simple_storage.aes"
    Contract = compile_test_contract("simple_storage"),

    %% Initialise contract, owned by Alice.
    {EncCPub,_,_} = create_contract(Node, APub, APriv, Contract, ["21"]),

    init_fun_calls(),

    %% Call contract get function by Alice. Check initial value.
    call_get(APub, APriv, EncCPub, Contract, 21),

    %% Call contract set function by Alice.
    call_set(APub, APriv, EncCPub, Contract, "42"),

    %% Call contract get function by Alice.
    call_get(APub, APriv, EncCPub, Contract, 42),

    %% Call contract set function by Alice.
    call_set(APub, APriv, EncCPub, Contract, "84"),

    force_fun_calls(Node), %% enforce calls above to be made.

    %% Call contract get function by Bert.
    call_get(BPub, BPriv, EncCPub, Contract, 84),

    %% Call contract set function by Bert.
    call_set(BPub, BPriv, EncCPub, Contract, "126"),

    %% Call contract get function by Bert.
    call_get(BPub, BPriv, EncCPub, Contract, 126),

    force_fun_calls(Node),

    ok.

call_get(Pub, Priv, EncCPub, Contract, ExpValue) ->
    call_func(Pub, Priv, EncCPub, Contract, "get", [], {"int", ExpValue}).

call_set(Pub, Priv, EncCPub, Contract, SetArg) ->
    call_func(Pub, Priv, EncCPub, Contract, "set", [SetArg]).

%% counter_contract(Config)
%%  Create the Counter contract by acc_b, tick it by acc_a and then
%%  check value by acc_a.

counter_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "counter.aes"
    Contract = compile_test_contract("counter"),

    %% Initialise contract, owned by Bert.
    {EncCPub,_,_} = create_contract(Node, BPub, BPriv, Contract, ["21"]),

    init_fun_calls(),

    %% Call contract get function by Bert.

    call_func(BPub, BPriv, EncCPub, Contract, "get", [], {"int", 21}),

    force_fun_calls(Node),

    %% Call contract tick function 5 times by Alice.
    call_tick(APub, APriv, EncCPub, Contract),
    call_tick(APub, APriv, EncCPub, Contract),
    call_tick(APub, APriv, EncCPub, Contract),
    call_tick(APub, APriv, EncCPub, Contract),
    call_tick(APub, APriv, EncCPub, Contract),

    force_fun_calls(Node),

    %% Call contract get function by Bert and check we have 26 ticks.
    call_func(BPub, BPriv, EncCPub, Contract, "get", [], {"int", 26}),

    force_fun_calls(Node),

    ok.

call_tick(Pub, Priv, EncCPub, Contract) ->
    call_func(Pub, Priv, EncCPub, Contract, "tick", []).

%% stack(Config)
%%  Create the Stack contract by acc_a and push and pop elements by acc_b

stack_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "stack.aes"
    Contract = compile_test_contract("stack"),

    %% Create the contract with 2 elements in the stack.
    {EncCPub,_,_} = create_contract(Node, APub, APriv, Contract, ["[\"two\", \"one\"]"]),

    init_fun_calls(), % setup call handling
    String = fun(Val) -> #{<<"type">> => <<"string">>, <<"value">> => Val} end,

    %% Test the size.
    call_func(BPub, BPriv, EncCPub, Contract, "size", [], {"int", 2}),

    %% Push 2 more elements.
    call_func(BPub, BPriv, EncCPub, Contract, "push", ["\"three\""]),
    call_func(BPub, BPriv, EncCPub, Contract, "push", ["\"four\""]),

    %% Test the size.
    call_func(BPub, BPriv, EncCPub, Contract, "size", [], {"int", 4}),

    %% Check the stack.
    call_func(BPub, BPriv, EncCPub, Contract, "all", [],
              {"list(string)", [String(<<"four">>), String(<<"three">>),
                                String(<<"two">>), String(<<"one">>)]}),

    %% Pop the values and check we get them in the right order.\
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"string", <<"four">>}),
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"string", <<"three">>}),
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"string", <<"two">>}),
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"string", <<"one">>}),

    %% The resulting stack is empty.
    call_func(BPub, BPriv, EncCPub, Contract, "size", [], {"int", 0}),

    force_fun_calls(Node),

    ok.

%% polymorphism_test_contract(Config)
%%  Check the polymorphism_test contract.

polymorphism_test_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "polymorphism_test.aes".
    Contract = compile_test_contract("polymorphism_test"),

    %% Initialise contract owned by Alice.
    {EncCPub,_,_} = create_contract(Node, APub, APriv, Contract, []),

    %% Test the polymorphism.
    init_fun_calls(), % setup call handling

    call_func(APub, APriv, EncCPub, Contract, "foo", [],
              {"list(int)", [word(5), word(7), word(9)]}),
    call_func(APub, APriv, EncCPub, Contract, "bar", [],
              {"list(int)", [word(1), word(0), word(3)]}),

    force_fun_calls(Node),

    ok.

%% factorial_contract(Config)
%%  Check the factorial contract.

factorial_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "factorial.aes".
    Contract = compile_test_contract("factorial"),

    %% We'll compute 10 factorial
    N = 10,

    %% Initialise contracts owned by Alice. We need N + 1 contracts, one for
    %% each call to factorial.
    CFun = fun (_, {EP,_DP,_IR}) ->
                   create_contract(Node, APub, APriv, Contract, [EP])
           end,
    {EncCPubkey, _DecCPubkey,_} =
        lists:foldl(CFun, {aeser_api_encoder:encode(contract_pubkey, <<0:256>>),0,0}, lists:seq(0, N)),

    init_fun_calls(), % setup call handling

    %% Compute fac(10) = 3628800.
    call_func(APub, APriv, EncCPubkey, Contract, "fac", [integer_to_list(N)], {"int", 3628800}),

    force_fun_calls(Node),

    ok.

%% maps_contract(Config)
%%  Check the Maps contract. We need an interface contract here as
%%  there is no way pass record as an argument over the http API.

maps_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub, priv_key := BPriv}}
        = proplists:get_value(accounts, Config),

    %% Compile test contract "maps.aes".
    Contract = compile_test_contract("maps"),

    %% Initialise contract owned by Alice.
    {EncMapsPub, _DecMapsPub, _} = create_contract(Node, APub, APriv, Contract, []),

    init_fun_calls(), % setup call handling

    %% Set state {[k] = v}
    %% State now {map_i = {[1]=>{x=1,y=2},[2]=>{x=3,y=4},[3]=>{x=5,y=6}},
    %%            map_s = ["one"]=> ... , ["two"]=> ... , ["three"] => ...}
    %%
    call_func(BPub, BPriv, EncMapsPub, Contract, "map_state_i", []),
    call_func(BPub, BPriv, EncMapsPub, Contract, "map_state_s", []),

    force_fun_calls(Node), %% We need to force here to do the debug print

    %% Print current state
    ct:pal("State ~p\n", [call_get_state(Node, APub, APriv, EncMapsPub, Contract)]),

    %% m[k]
    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_i",  ["2"],
              {"(int, int)", [word(3), word(4)]}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_s",  ["\"three\""],
              {"(int, int)", [word(5), word(6)]}),

    %% m{[k] = v}
    %% State now {map_i = {[1]=>{x=11,y=22},[2]=>{x=3,y=4},[3]=>{x=5,y=6}},
    %%            map_s = ["one"]=> ... , ["two"]=> ... , ["three"] => ...}
    %% Need to call interface functions as cannot create record as argument.
    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_i", ["1", "{x = 11, y = 22}"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_s", ["\"one\"", "{x = 11, y = 22}"]),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_i", ["1"],
              {"(int, int)", [word(11), word(22)]}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_s",  ["\"one\""],
              {"(int, int)", [word(11), word(22)]}),

    %% m{f[k].x = v}
    call_func(BPub, BPriv, EncMapsPub, Contract, "setx_state_i", ["2, 33"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "setx_state_s", ["\"two\", 33"]),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_i",  ["2"],
              {<<"(int, int)">>, [word(33), word(4)]}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_s",  ["\"two\""],
              {<<"(int, int)">>, [word(33), word(4)]}),

    %% Map.member
    %% Check keys 1 and "one" which exist and 10 and "ten" which don't.
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["1"], {"bool", 1}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["10"], {"bool", 0}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"one\""], {"bool", 1}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"ten\""], {"bool", 0}),

    %% Map.lookup
    %% The values of map keys 3 and "three" are unchanged, keys 10 and
    %% "ten" don't exist.
    Pair = fun (X, Y) -> tuple([word(X), word(Y)]) end,
    SomePair = fun ({some,{X, Y}}) -> [1, Pair(X, Y)];
                   (none) -> [0]
               end,

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_i", ["3"],
              {"option((int, int))", SomePair({some,{5, 6}})}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_i", ["10"],
              {"option((int, int))", SomePair(none)}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_s", ["\"three\""],
              {"option((int, int))", SomePair({some,{5, 6}})}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_s", ["\"ten\""],
              {"option((int, int))", SomePair(none)}),

    %% Map.lookup_default
    %% The values of map keys 3 and "three" are unchanged, keys 10 and
    %% "ten" don't exist.

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_i",
              ["3", "{x = 47, y = 11}"], {"(int, int)", [word(5), word(6)]}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_i",
              ["10", "{x = 47, y = 11}"], {"(int, int)", [word(47), word(11)]}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_s",
              ["\"three\"", "{x = 47, y = 11}"], {"(int, int)", [word(5), word(6)]}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_s",
              ["\"ten\"", "{x = 47, y = 11}"], {"(int, int)", [word(47), word(11)]}),

    %% Map.delete
    %% Check map keys 3 and "three" exist, delete them and check that
    %% they have gone, then put them back for future use.
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["3"], {"bool", 1}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"three\""], {"bool", 1}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "delete_state_i", ["3"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "delete_state_s", ["\"three\""]),

    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["3"], {"bool", 0}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"three\""], {"bool", 0}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_i", ["3", "{x = 5, y = 6}"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_s", ["\"three\"", "{x = 5, y = 6}"]),

    %% Map.size
    %% Both of these still contain 3 elements.
    call_func(BPub, BPriv, EncMapsPub, Contract,  "size_state_i", [], {"int", 3}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "size_state_s", [], {"int", 3}),

    %% Map.to_list, Map.from_list then test if element is there.
    call_func(BPub, BPriv, EncMapsPub, Contract, "fromlist_state_i", ["[(242424, {x = 43, y = 44})]"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["242424"], {"bool", 1}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "fromlist_state_s", ["[(\"xxx\", {x = 43, y = 44})]"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"xxx\""], {"bool", 1}),

    force_fun_calls(Node, 10 * ?MAX_MINED_BLOCKS), %% Many txs, so wait for longer.

    ok.

call_get_state(Node, Pub, Priv, EncMapsPub, Contract) ->
    StateType = <<"( map(int, (int, int)), map(string, (int, int)) )">>,
    {Return,_} = call_compute_func(Node, Pub, Priv, EncMapsPub, Contract, "get_state", []),
    #{<<"value">> := GetState} = decode_data(StateType, Return),
    GetState.

%% enironment_contract(Config)
%%  Check the Environment contract. We don't always check values and
%%  the nested calls don't seem to work yet.

environment_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv}} = proplists:get_value(accounts, Config),

    BBal0 = get_balance(BPub),

    %% Compile test contract "environment.aes"
    Contract = compile_test_contract("environment"),

    ContractBalance = 10000,

    %% Initialise contract owned by Alice setting balance to 10000.
    ZeroContract = aeser_api_encoder:encode(contract_pubkey, <<0:256>>),
    {EncCPub, _, _} =
        create_contract(Node, APub, APriv, Contract, [ZeroContract], #{amount => ContractBalance}),
    %% Second contract for remote calls
    {EncRPub, _DecRPub, _} =
        create_contract(Node, APub, APriv, Contract, [ZeroContract], #{amount => ContractBalance}),

    %% Get the initial balance.
    BBal1 = get_balance(BPub),

    init_fun_calls(),

    call_func(BPub, BPriv, EncCPub, Contract, "set_remote", [EncRPub]),

    %% Address.
    ct:pal("Calling contract_address\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "contract_address", []),
    ct:pal("Calling nested_address\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "nested_address", [EncRPub]),

    %% Balance.
    ct:pal("Calling contract_balance\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "contract_balance", [], {"int", ContractBalance}),

    %% Origin.
    ct:pal("Calling call_origin\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "call_origin", []),

    ct:pal("Calling nested_origin\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "nested_origin", []),

    %% Caller.
    ct:pal("Calling call_caller\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "call_caller", []),
    ct:pal("Calling nested_caller\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "nested_caller", []),

    %% Value.
    ct:pal("Calling call_value\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "call_value", [],
              #{amount => 5}, {"int", 5}),
    ct:pal("Calling nested_value\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "nested_value", ["42"]),

    %% Gas price.
    ct:pal("Calling call_gas_price\n"),
    ExpectedGasPrice = 2 * ?DEFAULT_GAS_PRICE,
    call_func(BPub, BPriv, EncCPub, Contract, "call_gas_price", [],
              #{gas_price => ExpectedGasPrice}, {"int", ExpectedGasPrice}),

    %% Account balances.
    ct:pal("Calling get_balance\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "get_balance",
              [aeser_api_encoder:encode(account_pubkey, BPub)]),

    %% Block hash.
    ct:pal("Calling block_hash\n"),
    {ok, 200, #{<<"hash">> := ExpectedBlockHash}} = get_key_block_at_height(2),
    {_, <<BHInt:256/integer-unsigned>>} = aeser_api_encoder:decode(ExpectedBlockHash),

    call_func(BPub, BPriv, EncCPub, Contract, "block_hash", ["2"], {"int", BHInt}),

    %% Block hash. With value out of bounds
    ct:pal("Calling block_hash out of bounds\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "block_hash", ["10000000"], {"int", 0}),

    %% Coinbase.
    ct:pal("Calling coinbase\n"),

    Beneficiary = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"beneficiary">> := B}} = get_key_block(KeyHash),
                    {_, <<BInt:256/integer-unsigned>>} = aeser_api_encoder:decode(B),
                    BInt
                  end,
    call_func(BPub, BPriv, EncCPub, Contract, "coinbase", [], {"address", Beneficiary}),

    %% Block timestamp.
    ct:pal("Calling timestamp\n"),

    Timestamp = fun(Hdr) -> maps:get(<<"time">>, Hdr) end,
    call_func(BPub, BPriv, EncCPub, Contract, "timestamp", [], {"int", Timestamp}),

    %% Block height.
    ct:pal("Calling block_height\n"),
    BlockHeight = fun(Hdr) -> maps:get(<<"height">>, Hdr) end,
    call_func(BPub, BPriv, EncCPub, Contract, "block_height", [], {"int", BlockHeight}),

    %% Difficulty.
    ct:pal("Calling difficulty\n"),
    Difficulty = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"target">> := T}} = get_key_block(KeyHash),
                    aeminer_pow:target_to_difficulty(T)
                 end,
    call_func(BPub, BPriv, EncCPub, Contract, "difficulty", [], {"int", Difficulty}),

    %% Gas limit.
    ct:pal("Calling gas_limit\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "gas_limit", [],
              {"int", aec_governance:block_gas_limit()}),

    force_fun_calls(Node),

    ct:pal("B Balances ~p, ~p, ~p\n", [BBal0, BBal1, get_balance(BPub)]),

    ok.

environment_contract_fate(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv}} = proplists:get_value(accounts, Config),

    BBal0 = get_balance(BPub),

    %% Compile test contract "environment.aes"
    Contract = compile_test_contract(fate, "environment"),

    ContractBalance = 10000,

    %% Initialise contract owned by Alice setting balance to 10000.
    {EncCPub, DecCPub, _} =
        create_contract(Node, APub, APriv, Contract, "()", #{amount => ContractBalance}),

    %% Get the initial balance.
    BBal1 = get_balance(BPub),

    init_fun_calls(),

    %% Address.
    ct:pal("Calling contract_address\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "contract_address", "()",
              {fate, {address, binary:decode_unsigned(DecCPub)}}),

    %% Balance.
    ct:pal("Calling contract_balance\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "contract_balance", "()",
              {fate, ContractBalance}),

    %% Origin.
    ct:pal("Calling call_origin\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "call_origin", "()",
              {fate, {address, binary:decode_unsigned(BPub)}}),
    ct:pal("Calling remote_call_origin\n"),
    <<"ct_", Rest/binary>> = EncCPub,
    Address = binary_to_list(<<"@ak_", Rest/binary>>),
    call_func(BPub, BPriv, EncCPub, Contract, "remote_call_origin",
              "(" ++ Address ++ ")", {fate, {address, binary:decode_unsigned(BPub)}}),

    %% Caller.
    ct:pal("Calling call_caller\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "call_caller", "(" ++ Address ++ ")"),
    ct:pal("Calling remote_call_caller\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "remote_call_caller",
              "(" ++ Address ++ ")", {fate, {address, binary:decode_unsigned(DecCPub)}}),

    %% %% Value.
    %% ct:pal("Calling call_value\n"),
    %% call_func(BPub, BPriv, EncCPub, Contract, "call_value", [],
    %%           #{amount => 5}, {"int", 5}),
    %% ct:pal("Calling nested_value\n"),
    %% call_func(BPub, BPriv, EncCPub, Contract, "nested_value", ["42"]),

    %% Gas price.
    ct:pal("Calling call_gas_price\n"),
    ExpectedGasPrice = 2 * ?DEFAULT_GAS_PRICE,
    call_func(BPub, BPriv, EncCPub, Contract, "call_gas_price", "()",
              #{gas_price => ExpectedGasPrice}, {fate, ExpectedGasPrice}),

    %% Account balances.
    ct:pal("Calling get_balance\n"),
    ABal = get_balance(APub),
    EncAPub = aeser_api_encoder:encode(account_pubkey, APub),
    AAddress = "@" ++ binary_to_list(EncAPub),
    call_func(BPub, BPriv, EncCPub, Contract, "get_balance", "(" ++ AAddress ++ ")",
              {fate, ABal}),
    call_func(BPub, BPriv, EncCPub, Contract, "contract_balance", "()",
              {fate, ContractBalance}),

    %% Block hash.
    ct:pal("Calling block_hash\n"),
    {ok, 200, #{<<"hash">> := ExpectedBlockHash}} = get_key_block_at_height(2),
    {_, <<BHInt:256/integer-unsigned>>} = aeser_api_encoder:decode(ExpectedBlockHash),

    call_func(BPub, BPriv, EncCPub, Contract, "block_hash", "(2)", {fate, BHInt}),

    %% Block hash. With value out of bounds
    ct:pal("Calling block_hash out of bounds\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "block_hash", "(10000000)", {fate, 0}),

    %% Beneficiary.
    ct:pal("Calling beneficiary\n"),

    Beneficiary = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"beneficiary">> := B}} = get_key_block(KeyHash),
                    {_, <<BInt:256/integer-unsigned>>} = aeser_api_encoder:decode(B),
                    {address, BInt}
                  end,
    call_func(BPub, BPriv, EncCPub, Contract, "beneficiary", "()", {fate, Beneficiary}),

    %% Block timestamp.
    ct:pal("Calling timestamp\n"),

    Timestamp = fun(Hdr) -> maps:get(<<"time">>, Hdr) end,
    call_func(BPub, BPriv, EncCPub, Contract, "timestamp", "()", {fate, Timestamp}),

    %% Block height.
    ct:pal("Calling block_height\n"),
    BlockHeight = fun(Hdr) -> maps:get(<<"height">>, Hdr) end,
    call_func(BPub, BPriv, EncCPub, Contract, "generation", "()", {fate, BlockHeight}),

    %% Difficulty.
    ct:pal("Calling difficulty\n"),
    Difficulty = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"target">> := T}} = get_key_block(KeyHash),
                    aeminer_pow:target_to_difficulty(T)
                 end,
    call_func(BPub, BPriv, EncCPub, Contract, "difficulty", "()", {fate, Difficulty}),

    %% Gas limit.
    ct:pal("Calling gas_limit\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "gas_limit", "()",
              {fate, aec_governance:block_gas_limit()}),

    force_fun_calls(Node),

    ct:pal("B Balances ~p, ~p, ~p\n", [BBal0, BBal1, get_balance(BPub)]),

    ok.

%% spend_test_contract(Config)
%%  Check the SpendTest contract.

spend_test_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := _BPriv}} = proplists:get_value(accounts, Config),

    BBal0 = get_balance(BPub),

    %% Compile test contract "spend_test.aes"
    Contract = compile_test_contract("spend_test"),

    %% Initialise contracts owned by Alice with balance set to 10000 and 20000.
    {EncC1Pub,_DecC1Pub,_} =
        create_contract(Node, APub, APriv, Contract, [], #{amount => 10000}),
    {EncC2Pub,DecC2Pub,_} =
        create_contract(Node, APub, APriv, Contract, [], #{amount => 20000}),

    init_fun_calls(),

    %% Alice does all the operations on the contract and spends on Bert.
    %% Check the contract balances.
    call_func(APub, APriv, EncC1Pub, Contract, "get_balance", [], {"int", 10000}),
    call_func(APub, APriv, EncC2Pub, Contract, "get_balance", [], {"int", 20000}),

    %% Spend 15000 on to Bert.
    Sp1Arg = [aeser_api_encoder:encode(account_pubkey, BPub), "15000"],
    call_func(APub, APriv, EncC2Pub, Contract, "spend", Sp1Arg, {"int", 5000}),

    %% Check that contract spent it.
    GBO1Arg = [aeser_api_encoder:encode(account_pubkey, DecC2Pub)],
    call_func(APub, APriv, EncC1Pub, Contract, "get_balance_of", GBO1Arg, {"int", 5000}),

    %% Check that Bert got it.
    GBO2Arg = [aeser_api_encoder:encode(account_pubkey, BPub)],
    call_func(APub, APriv, EncC1Pub, Contract, "get_balance_of", GBO2Arg, {"int", BBal0 + 15000}),

    %% Spend 6000 explicitly from contract 1 to Bert.
    SF1Arg = [EncC1Pub, aeser_api_encoder:encode(account_pubkey, BPub), "6000"],
    call_func(APub, APriv, EncC2Pub, Contract, "spend_from", SF1Arg, {"int", BBal0 + 21000}),

    %% Check that Bert got it.
    GBO3Arg = [aeser_api_encoder:encode(account_pubkey, BPub)],
    call_func(APub, APriv, EncC1Pub, Contract, "get_balance_of", GBO3Arg, {"int", BBal0 + 21000}),

    %% Check contract 2 balance.
    GBO4Arg = [aeser_api_encoder:encode(account_pubkey, DecC2Pub)],
    call_func(APub, APriv, EncC1Pub, Contract, "get_balance_of", GBO4Arg, {"int", 5000}),

    force_fun_calls(Node),

    ok.

%% dutch_auction_contract(Config)
%%  Check the DutchAuction contract. We use 3 accounts here, Alice for
%%  setting up the account, Carl as beneficiary and Bert as
%%  bidder. This makes it a bit easier to keep track of the values as
%%  we have gas loses as well.

dutch_auction_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv},
      acc_c := #{pub_key := CPub},
      acc_d := #{pub_key := DPub,
                 priv_key := DPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "dutch_auction.aes"
    Contract = compile_test_contract("dutch_auction"),

    %% Set auction start amount and decrease per mine and fee.
    StartAmt = 50000,
    Decrease = 1000,
    Fee      = 800000 * ?DEFAULT_GAS_PRICE,

    %% Initialise contract owned by Alice with Carl as benficiary.
    InitArgument = [aeser_api_encoder:encode(account_pubkey, CPub),
                    integer_to_list(StartAmt), integer_to_list(Decrease)],
    {EncCPub, _, InitReturn} = create_contract(Node, APub, APriv, Contract, InitArgument),
    #{<<"height">> := Height0} = InitReturn,

    %% Mine 5 times to decrement value.
    {ok,_} = aecore_suite_utils:mine_key_blocks(Node, 5),

    _ABal1 = get_balance(APub),
    BBal1 = get_balance(BPub),
    CBal1 = get_balance(CPub),

    %% Call the contract bid function by Bert.
    {_,#{return := BidReturn}} =
        call_compute_func(Node, BPub, BPriv, EncCPub, Contract,
                          "bid", [], #{amount => 100000,fee => Fee}),
    #{<<"gas_used">> := GasUsed,<<"height">> := Height1} = BidReturn,

    %% Set the cost from the amount, decrease and diff in height.
    Cost = StartAmt - (Height1 - Height0) * Decrease,

    BBal2 = get_balance(BPub),
    CBal2 = get_balance(CPub),

    %% Check that the balances are correct, don't forget the gas and the fee.
    BBal2 = BBal1 - Cost - GasUsed * ?DEFAULT_GAS_PRICE - Fee,
    CBal2 = CBal1 + Cost,

    %% Now make a bid which should fail as auction has closed.
    revert_call_compute_func(Node, DPub, DPriv, EncCPub, Contract,
                             "bid", [], #{amount => 100000,fee => Fee}),
    ok.

%% acm_dutch_auction_contract(Config)
%%  Check the DutchAuction contract in the ACM form. We use 3 accounts here, Alice for
%%  setting up the account, Carl and Bert as
%%  bidder. This makes it a bit easier to keep track of the values as
%%  we have gas loses as well.

acm_dutch_auction_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv},
      acc_c := #{pub_key := CPub,
                 priv_key := CPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract
    Contract = compile_test_contract("acm_dutch_auction"),

    %% Set auction start amount and decrease per mine and fee.
    StartAmt = 50000,
    Decrease = 1000,
    Fee      = 800000 * ?DEFAULT_GAS_PRICE,

    %% Initialise contract owned by Alice .
    InitArgument = [integer_to_list(StartAmt), integer_to_list(Decrease)],
    {EncCPub,_,InitReturn} = create_contract(Node, APub, APriv, Contract, InitArgument),
    #{<<"height">> := Height0} = InitReturn,

    %% Mine 5 times to decrement value.
    {ok,_} = aecore_suite_utils:mine_key_blocks(Node, 5),

    ABal1 = get_balance(APub),
    BBal1 = get_balance(BPub),
    CBal1 = get_balance(CPub),

    %% Call the contract bid function by Bert.
    {_,#{return := BidReturn}} =
        call_compute_func(Node, BPub, BPriv, EncCPub, Contract,
                          "bid", [], #{amount => 100000,fee => Fee}),
    #{<<"gas_used">> := GasUsed,<<"height">> := Height1} = BidReturn,

    %% Set the cost from the amount, decrease and diff in height.
    Cost = StartAmt - (Height1 - Height0) * Decrease,

    ABal2 = get_balance(APub),
    BBal2 = get_balance(BPub),
    CBal1 = get_balance(CPub),

    %% Check that the balances are correct, don't forget the gas and the fee.
    BBal2 = BBal1 - Cost - GasUsed * ?DEFAULT_GAS_PRICE - Fee,
    ABal2 = ABal1 + Cost,

    %% Now make a bid which should fail as auction has closed.
    {_, SecondBidReturn} =
         revert_call_compute_func(Node, CPub, CPriv, EncCPub, Contract,
                                  "bid", [], #{amount => 100000,fee => Fee}),
    ABal2 = get_balance(APub),
    BBal2 = get_balance(BPub),
    CBal3 = get_balance(CPub),

    #{<<"gas_used">> := GasUsed2} = SecondBidReturn,
    %% The second bidder only pays the fee and
    %% the little gas (170) that the aborted call cost

    ?assertEqual(CBal1 - CBal3, GasUsed2 * ?DEFAULT_GAS_PRICE + Fee),
    ok.

%% fundme_contract(Config)
%%  Check the FundMe contract. We use 4 accounts here, Alice to set up
%%  the account, Bert and beneficiary, and Carl and Diana as
%%  contributors.

fundme_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv},
      acc_c := #{pub_key := CPub,
                 priv_key := CPriv},
      acc_d := #{pub_key := DPub,
                 priv_key := DPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "fundme.aes"
    Contract = case aect_test_utils:latest_protocol_version() of
                   Vsn when Vsn < ?FORTUNA_PROTOCOL_VSN -> compile_test_contract("fundme");
                   _                                    -> compile_test_contract("aevm_3/fundme")
               end,

    %% Get the current height.
    {ok,200,#{<<"height">> := StartHeight}} = get_key_blocks_current_height(),

    %% Set deadline and goal.
    Duration = 10,
    Deadline = StartHeight + Duration,
    Goal     = 150000,

    %% Initialise contract owned by Alice with Bert as benficiary.
    InitArg = [aeser_api_encoder:encode(account_pubkey, BPub),
               integer_to_list(Deadline), integer_to_list(Goal)],
    {EncCPub,_,_} = create_contract(Node, APub, APriv, Contract, InitArg),

    init_fun_calls(),

    %% Let Carl and Diana contribute and check if we can withdraw early.
    call_func(CPub, CPriv, EncCPub, Contract, "contribute", [], #{<<"amount">> => 100000}, none),

    %% This should fail as we have not reached the goal.
    call_func(BPub, BPriv, EncCPub, Contract, "withdraw", [], revert),

    call_func(DPub, DPriv, EncCPub, Contract, "contribute", [], #{<<"amount">> => 100000}, none),

    %% This should fail as we have not reached the deadline.
    call_func(BPub, BPriv, EncCPub, Contract, "withdraw", [], revert),

    force_fun_calls(Node),

    {ok,200,#{<<"height">> := TmpHeight}} = get_key_blocks_current_height(),
    ct:log("Now at ~p started at ~p, need ~p to pass deadline!", [TmpHeight, StartHeight, Duration - (TmpHeight - StartHeight)]),

    %% Mine enough times to get past deadline.
    {ok,_} = aecore_suite_utils:mine_key_blocks(Node, Duration - (TmpHeight - StartHeight)),

    %% Now withdraw the amount
    call_func(BPub, BPriv, EncCPub, Contract, "withdraw", []),

    force_fun_calls(Node),

    ok.

%% erc20_token_contract(Config)

erc20_token_contract(Config) ->
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv},
      acc_b := #{pub_key := BPub,
                 priv_key := BPriv},
      acc_c := #{pub_key := CPub,
                 priv_key := CPriv},
      acc_d := #{pub_key := DPub,
                 priv_key := _DPriv}} = proplists:get_value(accounts, Config),

    %% Compile test contract "erc20_token.aes"
    Contract = compile_test_contract("erc20_token"),

    %% Default values, 100000, 10, "Token Name", "TKN".
    Total = 100000,
    Decimals = 10,
    Name = <<"Token Name">>,
    Symbol = <<"TKN">>,

    %% Initialise contract owned by Alice.
    InitArg = [integer_to_list(Total), integer_to_list(Decimals), "\"Token Name\"", "\"TKN\""],
    {EncCPub,_,_} = create_contract(Node, APub, APriv, Contract, InitArg),

    init_fun_calls(),

    [EAPub, EBPub, ECPub, EDPub] =
        [ aeser_api_encoder:encode(account_pubkey, Pub) || Pub <- [APub, BPub, CPub, DPub] ],

    %% Test state record fields.
    call_func(APub, APriv, EncCPub, Contract, "totalSupply", [], {"int", Total}),
    call_func(APub, APriv, EncCPub, Contract, "decimals", [],    {"int", Decimals}),
    call_func(APub, APriv, EncCPub, Contract, "name", [],        {"string", Name}),
    call_func(APub, APriv, EncCPub, Contract, "symbol", [],      {"string", Symbol}),

    %% Setup balances for Bert to 20000 and Carl to 25000 and check balances.
    call_func(APub, APriv, EncCPub, Contract, "transfer",  [EBPub, "20000"]),
    call_func(APub, APriv, EncCPub, Contract, "transfer",  [ECPub, "25000"]),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EAPub], {"int", 55000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EBPub], {"int", 20000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [ECPub], {"int", 25000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EDPub], {"int", 0}),

    %% Bert and Carl approve transfering 15000 to Alice.
    call_func(BPub, BPriv, EncCPub, Contract, "approve", [EAPub,"15000"]),
    force_fun_calls(Node), %% We need to ensure ordering, so force here!
    call_func(CPub, CPriv, EncCPub, Contract, "approve", [EAPub,"15000"]),
    force_fun_calls(Node), %% We need to ensure ordering, so force here!

    %% Alice transfers 10000 from Bert and 15000 Carl to Diana.
    call_func(APub, APriv, EncCPub, Contract, "transferFrom", [EBPub, EDPub, "10000"]),
    call_func(APub, APriv, EncCPub, Contract, "transferFrom", [ECPub, EDPub, "15000"]),

    %% Check the balances.
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EBPub], {"int", 10000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [ECPub], {"int", 10000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EDPub], {"int", 25000}),

    %% Check transfer and approval logs.
    TrfLog = [tuple([addr(CPub), addr(DPub), word(15000)]),
              tuple([addr(BPub), addr(DPub), word(10000)]),
              tuple([addr(APub), addr(CPub), word(25000)]),
              tuple([addr(APub), addr(BPub), word(20000)])],
    call_func(APub, APriv, EncCPub, Contract, "getTransferLog", [],
              {"list((address,address,int))", TrfLog}),

    AppLog = [tuple([addr(CPub), addr(APub), word(15000)]),
              tuple([addr(BPub), addr(APub), word(15000)])],
    call_func(APub, APriv, EncCPub, Contract, "getApprovalLog", [],
              {"list((address,address,int))", AppLog}),

    force_fun_calls(Node),

    %% Request a bad transfer and check that abort restores the state.
    revert_call_compute_func(Node, APub, APriv, EncCPub, Contract,
                             "transferFrom", [EBPub, EDPub, "100000"]),    %% new-style calldata creation

    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EBPub], {"int", 10000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [ECPub], {"int", 10000}),
    call_func(APub, APriv, EncCPub, Contract, "balanceOf", [EDPub], {"int", 25000}),

    call_func(APub, APriv, EncCPub, Contract, "getTransferLog", [],
              {"list((address,address,int))", TrfLog}),
    call_func(APub, APriv, EncCPub, Contract, "getApprovalLog", [],
              {"list((address,address,int))", AppLog}),

    force_fun_calls(Node),

    ok.

events_contract(Config) ->
    %% Set an account.
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv}} = proplists:get_value(accounts, Config),

    init_fun_calls(),
    Contract = compile_test_contract("events"),

    {EncCPub, _, _} = create_contract(Node, APub, APriv, Contract, []),

    force_fun_calls(Node),

    F1Check = fun([#{<<"address">> := Addr, <<"topics">> := Ts, <<"data">> := Data}]) ->
                <<E1:256>> = aec_hash:hash(evm, <<"Event1">>),
                ?assertEqual(Addr, EncCPub),
                ?assertEqual(Data, aeser_api_encoder:encode(contract_bytearray, <<"bar">>)),
                ?assertMatch([E1, 1, 2], Ts)
              end,
    call_func(APub, APriv, EncCPub, Contract, "f1", ["1", "\"bar\""], {log, F1Check}),

    F2Check = fun([#{<<"address">> := Addr, <<"topics">> := Ts, <<"data">> := Data}]) ->
                <<E2:256>> = aec_hash:hash(evm, <<"Event2">>),
                <<A:256>>  = APub,
                ?assertEqual(Addr, EncCPub),
                ?assertEqual(Data, aeser_api_encoder:encode(contract_bytearray, <<"foo">>)),
                ?assertEqual([E2, A], Ts)
              end,
    call_func(APub, APriv, EncCPub, Contract, "f2", ["\"foo\""], {log, F2Check}),

    force_fun_calls(Node).


remote_gas_test_contract(Config) ->
    %% Set an account.
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv}} = proplists:get_value(accounts, Config),

    init_fun_calls(),

    %% Set up two contracts.
    %% Compile test contract "remote_gas_test.aes".
    Contract = compile_test_contract("remote_gas_test"),

    {EncC1Pub,_,_} = create_contract(Node, APub, APriv, Contract, ["0"], #{amount => 200}),

    {EncC2Pub, _DecC2Pub, _} = create_contract(Node, APub, APriv, Contract, ["100"]),


    force_fun_calls(Node),
    Balance0 = get_balance(APub),
    call_get(APub, APriv, EncC1Pub, Contract, 0),
    call_get(APub, APriv, EncC2Pub, Contract, 100),
    force_fun_calls(Node),
    Balance1 = get_balance(APub),
    ?assertEqual(1600476 * ?DEFAULT_GAS_PRICE, Balance0 - Balance1),

    %% Test remote call with limited gas
    %% Call contract remote set function with limited gas
    [] = call_func(APub, APriv, EncC1Pub, Contract, "call", [EncC2Pub, "1", "80000"]),
    call_get(APub, APriv, EncC2Pub, Contract, 1),
    force_fun_calls(Node),
    Balance2 = get_balance(APub),
    ?assertEqual(1610687 * ?DEFAULT_GAS_PRICE, Balance1 - Balance2),

    %% Test remote call with limited gas (3) that fails (out of gas).
    [] = call_func(APub, APriv, EncC1Pub, Contract, "call", [EncC2Pub, "2", "3"], error),
    force_fun_calls(Node),
    Balance3 = get_balance(APub),
    ?assertEqual(809933 * ?DEFAULT_GAS_PRICE, Balance2 - Balance3),

    %% Check that store/state not changed (we tried to write 2).
    call_get(APub, APriv, EncC2Pub, Contract, 1),
    force_fun_calls(Node),
    Balance4 = get_balance(APub),

    %% Test remote call with limited gas that fails (invald call).
    ZeroContract = aeser_api_encoder:encode(contract_pubkey, <<0:256>>),
    [] = call_func(APub, APriv, EncC1Pub, Contract, "call", [ZeroContract, "2", "1"], error),
    force_fun_calls(Node),
    Balance5 = get_balance(APub),
    ?assertEqual(900000 * ?DEFAULT_GAS_PRICE, Balance4 - Balance5),

    ok.

%% Data structure functions.
word(Val) -> #{<<"type">> => <<"word">>, <<"value">> => Val}.

tuple(Vals) -> #{<<"type">> => <<"tuple">>, <<"value">> => Vals}. %Sneaky

addr(Addr) -> <<Int:256>> = Addr, word(Int).

%% Internal access functions.

get_balance(Pubkey) ->
    Addr = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok,200,#{<<"balance">> := Balance}} = get_account_by_pubkey(Addr),
    Balance.

decode_data(Type, EncodedData) ->
    {ok, DecodedData} = aect_test_utils:decode_data(Type, EncodedData),
    DecodedData.

call_func_decode(NodeName, Pubkey, Privkey, EncCPubkey,
                 Contract, Function, Arg, Type) ->
    call_func_decode(NodeName, Pubkey, Privkey, EncCPubkey,
                     Contract, Function, Arg, #{}, Type).

call_func_decode(NodeName, Pubkey, Privkey, EncCPubkey,
                 Contract, Function, Arg, CallerSet, Type) ->
    {Return,_} = call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                                   Contract, Function, Arg, CallerSet),
    #{<<"value">> := Value} = decode_data(Type, Return),
    Value.

%% Contract interface functions.
compile_test_contract(Name) ->
    compile_test_contract(aevm, Name).

compile_test_contract(aevm, Name) ->
    FileName = filename:join("contracts", lists:concat([Name, ".aes"])),
    {ok, BinSrc} = aect_test_utils:read_contract(FileName),
    {ok, Code} = aect_test_utils:compile_contract(FileName),
    #{ bytecode => aeser_api_encoder:encode(contract_bytearray, Code),
       vm       => aect_test_utils:latest_sophia_vm_version(),
       abi      => aect_test_utils:latest_sophia_abi_version(),
       code     => Code,
       src      => binary_to_list(BinSrc) };
compile_test_contract(fate, Name) ->
    FileName = filename:join(["contracts", "fate_asm", lists:concat([Name, ".fate"])]),
    {ok, Code} = aect_test_utils:compile_contract(?SOPHIA_FORTUNA_FATE, FileName),
    #{ bytecode => aeser_api_encoder:encode(contract_bytearray, Code),
       vm       => ?VM_FATE_SOPHIA_1,
       abi      => ?ABI_FATE_SOPHIA_1,
       code     => Code
     }.

encode_calldata(Contract, Fun, Args) ->
    case maps:get(abi, Contract) of
        ?ABI_AEVM_SOPHIA_1 ->
            {ok, CData} = aect_test_utils:encode_call_data(maps:get(src, Contract), Fun, Args),
            {ok, aeser_api_encoder:encode(contract_bytearray, CData)};
        ?ABI_FATE_SOPHIA_1 ->
            CData = aeb_fate_asm:function_call(lists:concat([Fun, Args])),
            {ok, aeser_api_encoder:encode(contract_bytearray, CData)}
    end.

create_contract(NodeName, Pubkey, Privkey, Contract, InitArgs) ->
    create_contract(NodeName, Pubkey, Privkey, Contract, InitArgs, #{}).

create_contract(NodeName, Pubkey, Privkey, Contract, InitArgs, CallerSet) ->
    {ok, CallData} = encode_calldata(Contract, "init", InitArgs),

    {#{tx_hash := TxHash}, EncodedContractPubkey, DecodedContractPubkey} =
        contract_create_tx(Pubkey, Privkey, Contract, CallData, CallerSet),

    %% Mine blocks and check that it is in the chain.
    ok = wait_for_tx_hash_on_chain(NodeName, TxHash),
    ?assert(tx_in_chain(TxHash)),

    %% Get value of last call.
    {ok,200, #{<<"call_info">> := InitReturn}} = get_contract_call_object(TxHash),
    ct:pal("Init return ~p\n", [InitReturn]),

    {EncodedContractPubkey, DecodedContractPubkey, InitReturn}.

%% init_fun_calls()
%% force_fun_calls(Node)
%% call_func(Pubkey, Privkey, EncodedContractPubkey, Function, Arguments)
%% call_func(Pubkey, Privkey, EncodedContractPubkey, Function, Arguments, Check)

init_fun_calls() ->
    put(fun_calls, []), put(nonces, []).

force_fun_calls(Node) ->
    force_fun_calls(Node, ?MAX_MINED_BLOCKS).

force_fun_calls(Node, MaxMinedBlocks) ->
    Calls = put(fun_calls, []),
    put(nonces, []),

    %% First try them with dry run...
    dry_run_txs(Calls),

    %% Then really put them on the chain
    TxHashes = [ TxHash || {#{tx_hash := TxHash}, _} <- Calls ],
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, TxHashes, MaxMinedBlocks),
    check_calls(Calls).

dry_run_txs(Calls) ->
    Txs = [ Tx || {#{tx_encoded := Tx}, _} <- Calls ],
    {ok, 200, #{ <<"results">> := Results }} = dry_run(Txs),
    check_dry_calls(Calls, Results).

check_dry_calls(Calls, Results) ->
    [ check_dry_call(Check, Result) || {{_, Check}, Result} <- lists:zip(Calls, Results) ].

check_dry_call(Check, Result) ->
    #{ <<"call_obj">> := CallReturn } = Result,
    check_call_(Check, CallReturn, undefined).

check_calls(Calls) ->
    [ check_call(Call) || Call <- Calls ].

check_call({#{tx_hash := TxHash}, Check}) ->
    ct:log("Checking: ~p", [TxHash]),
    {ok, 200, #{<<"call_info">> := CallReturn}} = get_contract_call_object(TxHash),
    ct:pal("Call return ~p\n", [CallReturn]),
    check_call_(Check, CallReturn, TxHash).

check_call_([], _CallObject, _TxHash) ->
    ok;
check_call_([Check | Checks], CallObject, TxHash) ->
    check_call_(Check, CallObject, TxHash),
    check_call_(Checks, CallObject, TxHash);
check_call_(Check, CallObject, TxHash) ->
    #{<<"return_type">> := RetType,
      <<"return_value">> := Value,
      <<"log">> := Log } = CallObject,

    %% Get the block where the tx was included
    case Check of
        none -> ?assertEqual(<<"ok">>, RetType);
        {log, Fun} ->
            ?assertEqual(<<"ok">>, RetType),
            Fun(Log);
        {Type, Fun} when is_function(Fun) ->
            ?assertEqual(<<"ok">>, RetType),
            case TxHash of
                undefined ->
                    ok;
                _ ->
                    {ok, 200, #{<<"block_hash">> := BlockHash}} = get_tx(TxHash),
                    {ok, 200, BlockHeader} = get_micro_block_header(BlockHash),
                    check_value(Value, {Type, Fun(BlockHeader)})
            end;
        {_, _} ->
            ?assertEqual(<<"ok">>, RetType),
            check_value(Value, Check);
        error ->
            ?assertEqual(<<"error">>, RetType);
        revert ->
            ?assertEqual(<<"revert">>, RetType)
    end.

check_value(Val0, {fate, ExpVal}) ->
    Val = decode_data(fate, Val0),
    ct:log("~p decoded\nas ~p\ninto ~p =??= ~p", [Val0, fate, Val, ExpVal]),
    ?assertEqual(ExpVal, Val);
check_value(Val0, {Type, ExpVal}) ->
    #{<<"value">> := Val} = decode_data(Type, Val0),
    ct:log("~p decoded\nas ~p\ninto ~p =??= ~p", [Val0, Type, Val, ExpVal]),
    ?assertEqual(ExpVal, Val).


call_func(Pub, Priv, EncCPub, Contract, Fun, Args) ->
    call_func(Pub, Priv, EncCPub, Contract, Fun, Args, #{}, none).

call_func(Pub, Priv, EncCPub, Contract, Fun, Args, Check) ->
    call_func(Pub, Priv, EncCPub, Contract, Fun, Args, #{}, Check).

call_func(Pub, Priv, EncCPub, Contract, Fun, Args, CallerSet, Check) ->
    Nonce = get_nonce(Pub),
    {ok, CallData} = encode_calldata(Contract, Fun, Args),
    ABI = maps:get(abi, Contract),
    Tx = contract_call_tx(Pub, Priv, Nonce, EncCPub, ABI, CallData, CallerSet),

    Calls = get(fun_calls),
    put(fun_calls, Calls ++ [{Tx, Check}]).

get_nonce(Pub) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pub),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_account_by_pubkey(Address),
    Nonces = get(nonces),
    case lists:keyfind(Pub, 1, Nonces) of
        false ->
            put(nonces, [{Pub, 1} | Nonces]),
            Nonce0 + 1;
        {Pub, NonceOff} ->
            put(nonces, lists:keyreplace(Pub, 1, Nonces, {Pub, NonceOff + 1})),
            Nonce0 + NonceOff + 1
    end.

call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                  Contract, Function, Argument) ->
    call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                      Contract, Function, Argument, #{}).

call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                  Contract, Function, Argument, CallerSet) ->
    {CallReturn,ContractCallTxHash} =
        basic_call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                                Contract, Function, Argument, CallerSet),

    #{<<"return_type">> := <<"ok">>,
      <<"return_value">> := Value} = CallReturn,

    %% Get the block where the tx was included
    {ok, 200, #{<<"block_hash">> := BlockHash}} = get_tx(ContractCallTxHash),
    {ok, 200, BlockHeader} = get_micro_block_header(BlockHash),

    {Value, #{header => BlockHeader, return => CallReturn}}.

revert_call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                         Contract, Function, Argument) ->
    revert_call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                             Contract, Function, Argument, #{}).

revert_call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                         Contract, Function, Argument, CallerSet) ->
    {CallReturn,_} = basic_call_compute_func(NodeName, Pubkey, Privkey,
                                             EncCPubkey, Contract,
                                             Function, Argument, CallerSet),

    #{<<"return_type">> := <<"revert">>,
      <<"return_value">> := Value} = CallReturn,
    {Value,CallReturn}.

basic_call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                        Contract, Function, Argument, CallerSet) ->
    {ok, CallData} = encode_calldata(Contract, Function, Argument),
    ABI = maps:get(abi, Contract),
    #{tx_hash := ContractCallTxHash} =
        contract_call_tx(Pubkey, Privkey, EncCPubkey, ABI, CallData, CallerSet),

    %% Mine blocks and check that it is in the chain.
    ok = wait_for_tx_hash_on_chain(NodeName, ContractCallTxHash),
    ?assert(tx_in_chain(ContractCallTxHash)),

    %% Get the call object and return value.
    {ok,200, #{<<"call_info">> := CallReturn}} = get_contract_call_object(ContractCallTxHash),
    ct:pal("Call return ~p\n", [CallReturn]),

    {CallReturn,ContractCallTxHash}.

contract_create_tx(Pubkey, Privkey, Contract, CallData, CallerSet) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_account_by_pubkey(Address),
    Nonce = Nonce0 + 1,

    %% The default init contract.
    ContractInitEncoded0 = #{ owner_id => Address,
                              code => maps:get(bytecode, Contract),
                              call_data => CallData,
                              vm_version => maps:get(vm, Contract),
                              abi_version => maps:get(abi, Contract),
                              deposit => 0,
                              amount => 0,      %Initial balance
                              gas => 100000,   %May need a lot of gas
                              gas_price => ?DEFAULT_GAS_PRICE,
                              fee => 1400000 * ?DEFAULT_GAS_PRICE,
                              nonce => Nonce },
    ContractInitEncoded = maps:merge(ContractInitEncoded0, CallerSet),
    sign_and_post_create_tx(Privkey, ContractInitEncoded).

contract_call_tx(Pubkey, Privkey, EncCPubkey, ABI, CallData, CallerSet) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_account_by_pubkey(Address),
    Nonce = Nonce0 + 1,
    contract_call_tx(Pubkey, Privkey, Nonce, EncCPubkey, ABI, CallData, CallerSet).

contract_call_tx(Pubkey, Privkey, Nonce, EncCPubkey, ABI, CallData, CallerSet) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),

    ContractCallEncoded0 = #{ caller_id => Address,
                              contract_id => EncCPubkey,
                              call_data   => CallData,
                              abi_version => ABI,
                              amount => 0,
                              gas => 100000,    %May need a lot of gas
                              gas_price => ?DEFAULT_GAS_PRICE,
                              fee => 800000 * ?DEFAULT_GAS_PRICE,
                              nonce => Nonce },
    ContractCallEncoded = maps:merge(ContractCallEncoded0, CallerSet),
    sign_and_post_call_tx(Privkey, ContractCallEncoded).


%% ============================================================
%% HTTP Requests
%% Note that some are internal and some are external!
%% ============================================================

get_micro_block_header(Hash) ->
    Host = external_address(),
    http_request(Host, get,
                 "micro-blocks/hash/"
                 ++ binary_to_list(Hash)
                 ++ "/header", []).

dry_run(Txs) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/dry-run", #{txs => Txs}).

get_key_block(Hash) ->
    Host = external_address(),
    http_request(Host, get,
                 "key-blocks/hash/"
                 ++ binary_to_list(Hash), []).

get_key_blocks_current_height() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current/height", []).

get_key_block_at_height(Height) ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/height/" ++ integer_to_list(Height), []).

get_contract_create(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create", Data).

get_contract_call(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/call", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

get_tx(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ binary_to_list(TxHash), []).

create_spend_tx(RecipientId, Amount, Fee) ->
    Sender = maps:get(pubkey, aecore_suite_utils:patron()),
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
    create_spend_tx(SenderId, RecipientId, Amount, Fee, <<"post spend tx">>).

create_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

get_account_by_pubkey(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

post_tx(TxSerialized) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => TxSerialized}).

sign_tx(Tx) ->
    {ok, TxSer} = aeser_api_encoder:safe_decode(transaction, Tx),
    UTx = aetx:deserialize_from_binary(TxSer),
    STx = aec_test_utils:sign_tx(UTx, [maps:get(privkey, aecore_suite_utils:patron())]),
    aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(STx)).

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
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    {ok, 200, #{<<"tx">> := SpendTx}} =
        create_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Balance, Fee),
    SignedSpendTx = sign_tx(SpendTx),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),
    {Pubkey,Privkey,SpendTxHash}.

sign_and_post_create_tx(Privkey, CreateEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx,
              <<"contract_id">> := EncodedPubkey}} =
        get_contract_create(CreateEncoded),
    {ok,DecodedPubkey} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                       EncodedPubkey),
    Tx = sign_and_post_tx(Privkey, EncodedUnsignedTx),
    {Tx,EncodedPubkey,DecodedPubkey}.

sign_and_post_call_tx(Privkey, CallEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx}} =
        get_contract_call(CallEncoded),
    sign_and_post_tx(Privkey, EncodedUnsignedTx).

sign_and_post_tx(PrivKey, EncodedUnsignedTx) ->
    {ok,SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction,
                                                        EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aec_test_utils:sign_tx(UnsignedTx, PrivKey),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    SendTx = aeser_api_encoder:encode(transaction, SerializedTx),
    {ok,200,#{<<"tx_hash">> := TxHash}} = post_tx(SendTx),
    #{tx_hash => TxHash, tx_encoded => EncodedUnsignedTx}.

tx_in_chain(TxHash) ->
    case get_tx(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} ->
            ct:log("Tx not mined, but in mempool"),
            false;
        {ok, 200, #{<<"block_hash">> := _}} -> true;
        {ok, 404, _} -> false
    end.

wait_for_tx_hash_on_chain(Node, TxHash) ->
    case tx_in_chain(TxHash) of
        true -> ok;
        false ->
            case aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TxHash], ?MAX_MINED_BLOCKS) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
    end.

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.
