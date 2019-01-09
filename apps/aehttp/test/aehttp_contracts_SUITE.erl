-module(aehttp_contracts_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

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
         compiler_error_contract/1,
         counter_contract/1,
         dutch_auction_contract/1,
         environment_contract/1,
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
         null/1
        ]).

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).

all() ->
    [
     {group, contracts}
    ].

groups() ->
    [
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
       fundme_contract,
       erc20_token_contract,
       compiler_error_contract,
       remote_gas_test_contract,
       null                                     %This allows to end with ,
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
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



init_for_contracts(Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),

    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ aecore_suite_utils:mine_key_blocks(NodeName, ToMine) || ToMine > 0 ],

    %% Prepare accounts, Alice, Bert, Carl and Diana with balance of 10T.

    StartAmt = 10*1000*1000000,                 %That is a lot of tokens!
    {APubkey, APrivkey, STx1} = new_account(StartAmt),
    {BPubkey, BPrivkey, STx2} = new_account(StartAmt),
    {CPubkey, CPrivkey, STx3} = new_account(StartAmt),
    {DPubkey, DPrivkey, STx4} = new_account(StartAmt),

    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(
                                    NodeName, [STx1, STx2, STx3, STx4], 10),

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
init_per_group(remote, Config) -> init_for_contracts(Config).

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
    Code = compile_test_contract("identity"),

    init_fun_calls(),

    %% Initialise contract, owned by Carl.
    {EncCPub,_,_} =
        create_compute_contract(Node, CPub, CPriv, Code, call_code("init", [])),

    %% Call contract main function by Carl.
    call_func(CPub, CPriv, EncCPub,  <<"main">>, <<"(42)">>, {<<"int">>, 42}),

    %% Call contract main function by Diana.
    call_func(DPub, DPriv, EncCPub,  <<"main">>, <<"(42)">>, {<<"int">>, 42}),

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
        create_compute_contract(Node, APub, APriv, TCode, <<"(42)">>),
    {_EncodedInt1Pub,DecodedInt1Pub,_} =
        create_compute_contract(Node, APub, APriv, ICode,
                                args_to_binary([DecodedTestPub,42])),
    {_EncodedInt2Pub,DecodedInt2Pub,_} =
        create_compute_contract(Node, APub, APriv, ICode,
                                args_to_binary([DecodedInt1Pub,42])),
    {EncodedInt3Pub,_,_} =
        create_compute_contract(Node, APub, APriv, ICode,
                                args_to_binary([DecodedInt2Pub,42])),

    %% This should set state values to 17, 1017, 2017, 3017.
    call_compute_func(Node, APub, APriv, EncodedInt3Pub,
                      <<"put_values">>, <<"(17)">>),
    BeforeList = call_func_decode(Node, APub, APriv,
                                  EncodedInt3Pub,
                                  <<"get_values">>, <<"()">>, <<"list(int)">>),
    ct:pal("Before Values ~p\n", [BeforeList]),

    ABal0 = get_balance(APub),

    %% Do abort where we have put values 42, 142, 242, 342,
    %% then check value in abort_test contract, should still be 3017!
    %% Check that we get the abort string back.
    %% First check when we call the full contract call stack.
    AbortString = <<"yogi bear">>,
    GivenGas = 100000,
    {RevertValue1, Return1} =
        revert_call_compute_func(Node, APub, APriv, EncodedInt3Pub,
                                 <<"do_abort">>,
                                 args_to_binary([42,{string,AbortString}]),
                                 #{gas => GivenGas}
                                ),
    #{<<"value">> := AbortString} = decode_data(<<"string">>, RevertValue1),
    #{<<"gas_used">> := GasUsed1} = Return1,
    ?assert(GasUsed1 < GivenGas), %% Make sure we actually saved some gas

    %% Also check that we get the same behaviour when calling directly to the
    %% leaf contract.
    {RevertValue2, Return2} =
        revert_call_compute_func(Node, APub, APriv, EncodedTestPub,
                                 <<"do_abort">>,
                                 args_to_binary([42,{string,AbortString}]),
                                 #{gas => GivenGas}
                                ),
    #{<<"value">> := AbortString} = decode_data(<<"string">>, RevertValue2),
    #{<<"gas_used">> := GasUsed2} = Return2,
    ?assert(GasUsed2 < GivenGas), %% Make sure we actually saved some gas

    ABal1 = get_balance(APub),

    %% The contract values should be the same, 17, 1017, 2017, 3017.
    AfterList = call_func_decode(Node, APub, APriv,
                                 EncodedInt3Pub,
                                 <<"get_values">>, <<"()">>, <<"list(int)">>),
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
    Code = compile_test_contract("simple_storage"),

    %% Initialise contract, owned by Alice.
    {EncCPub,_,_} =
        create_compute_contract(Node, APub, APriv, Code, <<"(21)">>),

    init_fun_calls(),

    %% Call contract get function by Alice. Check initial value.
    call_get(APub, APriv, EncCPub, 21),

    %% Call contract set function by Alice.
    call_set(APub, APriv, EncCPub, <<"(42)">>),

    %% Call contract get function by Alice.
    call_get(APub, APriv, EncCPub, 42),

    %% Call contract set function by Alice.
    call_set(APub, APriv, EncCPub, <<"(84)">>),

    force_fun_calls(Node), %% enforce calls above to be made.

    %% Call contract get function by Bert.
    call_get(BPub, BPriv, EncCPub, 84),

    %% Call contract set function by Bert.
    call_set(BPub, BPriv, EncCPub, <<"(126)">>),

    %% Call contract get function by Bert.
    call_get(BPub, BPriv, EncCPub, 126),

    force_fun_calls(Node),

    ok.

call_get(Pub, Priv, EncCPub, ExpValue) ->
    call_func(Pub, Priv, EncCPub, <<"get">>, <<"()">>, {<<"int">>, ExpValue}).

call_set(Pub, Priv, EncCPub, SetArg) ->
    call_func(Pub, Priv, EncCPub,<<"set">>, SetArg).

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
    Code = compile_test_contract("counter"),

    %% Initialise contract, owned by Bert.
    {EncCPub,_,_} =
        create_compute_contract(Node, BPub, BPriv, Code, <<"(21)">>),

    init_fun_calls(),

    %% Call contract get function by Bert.

    call_func(BPub, BPriv, EncCPub, <<"get">>, <<"()">>, {<<"int">>, 21}),

    force_fun_calls(Node),

    %% Call contract tick function 5 times by Alice.
    call_tick(APub, APriv, EncCPub),
    call_tick(APub, APriv, EncCPub),
    call_tick(APub, APriv, EncCPub),
    call_tick(APub, APriv, EncCPub),
    call_tick(APub, APriv, EncCPub),

    force_fun_calls(Node),

    %% Call contract get function by Bert and check we have 26 ticks.
    call_func(BPub, BPriv, EncCPub, <<"get">>, <<"()">>, {<<"int">>, 26}),

    force_fun_calls(Node),

    ok.

call_tick(Pub, Priv, EncCPub) ->
    call_func(Pub, Priv, EncCPub, <<"tick">>, <<"()">>).

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
    Code = compile_test_contract("stack"),

    %% Create the contract with 2 elements in the stack.
    {EncCPub,_,_} = create_compute_contract(Node, APub, APriv, Code,
                                            <<"([\"two\",\"one\"])">>),

    init_fun_calls(), % setup call handling
    String = fun(Val) -> #{<<"type">> => <<"string">>, <<"value">> => Val} end,

    %% Test the size.
    call_func(BPub, BPriv, EncCPub, <<"size">>, <<"()">>, {<<"int">>, 2}),

    %% Push 2 more elements.
    call_func(BPub, BPriv, EncCPub, <<"push">>, <<"(\"three\")">>),
    call_func(BPub, BPriv, EncCPub, <<"push">>, <<"(\"four\")">>),

    %% Test the size.
    call_func(BPub, BPriv, EncCPub, <<"size">>, <<"()">>, {<<"int">>, 4}),

    %% Check the stack.
    call_func(BPub, BPriv, EncCPub, <<"all">>, <<"()">>,
              {<<"list(string)">>, [String(<<"four">>), String(<<"three">>),
                                    String(<<"two">>), String(<<"one">>)]}),

    %% Pop the values and check we get them in the right order.\
    call_func(BPub, BPriv, EncCPub, <<"pop">>, <<"()">>, {<<"string">>, <<"four">>}),
    call_func(BPub, BPriv, EncCPub, <<"pop">>, <<"()">>, {<<"string">>, <<"three">>}),
    call_func(BPub, BPriv, EncCPub, <<"pop">>, <<"()">>, {<<"string">>, <<"two">>}),
    call_func(BPub, BPriv, EncCPub, <<"pop">>, <<"()">>, {<<"string">>, <<"one">>}),

    %% The resulting stack is empty.
    call_func(BPub, BPriv, EncCPub, <<"size">>, <<"()">>, {<<"int">>, 0}),

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
    Code = compile_test_contract("polymorphism_test"),

    %% Initialise contract owned by Alice.
    {EncodedContractPub,_,_} =
       create_compute_contract(Node, APub, APriv, Code, <<"()">>),

    %% Test the polymorphism.
    init_fun_calls(), % setup call handling

    call_func(APub, APriv, EncodedContractPub, <<"foo">>, <<"()">>,
              {<<"list(int)">>, [word(5), word(7), word(9)]}),
    call_func(APub, APriv, EncodedContractPub, <<"bar">>, <<"()">>,
              {<<"list(int)">>, [word(1), word(0), word(3)]}),

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
    Code = compile_test_contract("factorial"),

    %% We'll compute 10 factorial
    N = 10,

    %% Initialise contracts owned by Alice. We need N + 1 contracts, one for
    %% each call to factorial.
    CFun = fun (_, {_EP,DP,_IR}) ->
                   create_compute_contract(Node, APub, APriv, Code,
                                           args_to_binary([DP]))
           end,
    {EncodedContractPubkey, _DecodedContractPubkey,_} =
        lists:foldl(CFun, {0,0,0}, lists:seq(0, N)),

    init_fun_calls(), % setup call handling

    %% Compute fac(10) = 3628800.
    call_func(APub, APriv, EncodedContractPubkey,
              <<"fac">>, args_to_binary([N]),
              {<<"int">>, 3628800}),

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
    MCode = compile_test_contract("maps"),

    %% Initialise contract owned by Alice.
    {EncMapsPub,DecodedMapsPub,_} =
       create_compute_contract(Node, APub, APriv, MCode, <<"()">>),

    %% Compile the interface contract "test_maps.aes".
    TCode = compile_test_contract(proplists:get_value(data_dir, Config), "test_maps"),
    %% TestMapsFile = proplists:get_value(data_dir, Config) ++ "test_maps.aes",
    %% {ok, SophiaCode} = file:read_file(TestMapsFile),
    %% {ok, 200, #{<<"bytecode">> := TCode}} = get_contract_bytecode(SophiaCode),

    {EncTestPub,_,_} =
        create_compute_contract(Node, APub, APriv, TCode,
                                args_to_binary([DecodedMapsPub])),

    init_fun_calls(), % setup call handling

    %% Set state {[k] = v}
    %% State now {map_i = {[1]=>{x=1,y=2},[2]=>{x=3,y=4},[3]=>{x=5,y=6}},
    %%            map_s = ["one"]=> ... , ["two"]=> ... , ["three"] => ...}
    %%
    call_func(BPub, BPriv, EncMapsPub, <<"map_state_i">>, <<"()">>),
    call_func(BPub, BPriv, EncMapsPub, <<"map_state_s">>, <<"()">>),

    force_fun_calls(Node), %% We need to force here to do the debug print

    %% Print current state
    ct:pal("State ~p\n", [call_get_state(Node, APub, APriv, EncMapsPub)]),

    %% m[k]
    call_func(BPub, BPriv, EncMapsPub, <<"get_state_i">>,  <<"(2)">>,
              {<<"(int, int)">>, [word(3), word(4)]}),

    call_func(BPub, BPriv, EncMapsPub, <<"get_state_s">>,  <<"(\"three\")">>,
              {<<"(int, int)">>, [word(5), word(6)]}),

    %% m{[k] = v}
    %% State now {map_i = {[1]=>{x=11,y=22},[2]=>{x=3,y=4},[3]=>{x=5,y=6}},
    %%            map_s = ["one"]=> ... , ["two"]=> ... , ["three"] => ...}
    %% Need to call interface functions as cannot create record as argument.
    call_func(BPub, BPriv, EncTestPub, <<"set_state_i">>, <<"(1, 11, 22)">>),
    call_func(BPub, BPriv, EncTestPub, <<"set_state_s">>, <<"(\"one\", 11, 22)">>),

    call_func(BPub, BPriv, EncMapsPub, <<"get_state_i">>, <<"(1)">>,
              {<<"(int, int)">>, [word(11), word(22)]}),
    call_func(BPub, BPriv, EncMapsPub, <<"get_state_s">>,  <<"(\"one\")">>,
              {<<"(int, int)">>, [word(11), word(22)]}),

    %% m{f[k].x = v}
    call_func(BPub, BPriv, EncMapsPub, <<"setx_state_i">>, <<"(2, 33)">>),
    call_func(BPub, BPriv, EncMapsPub, <<"setx_state_s">>, <<"(\"two\", 33)">>),

    call_func(BPub, BPriv, EncMapsPub, <<"get_state_i">>,  <<"(2)">>,
              {<<"(int, int)">>, [word(33), word(4)]}),

    call_func(BPub, BPriv, EncMapsPub, <<"get_state_s">>,  <<"(\"two\")">>,
              {<<"(int, int)">>, [word(33), word(4)]}),

    %% Map.member
    %% Check keys 1 and "one" which exist and 10 and "ten" which don't.
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_i">>, <<"(1)">>, {<<"bool">>, 1}),
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_i">>, <<"(10)">>, {<<"bool">>, 0}),

    call_func(BPub, BPriv, EncMapsPub, <<"member_state_s">>, <<"(\"one\")">>, {<<"bool">>, 1}),
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_s">>, <<"(\"ten\")">>, {<<"bool">>, 0}),

    %% Map.lookup
    %% The values of map keys 3 and "three" are unchanged, keys 10 and
    %% "ten" don't exist.
    Pair = fun (X, Y) -> tuple([word(X), word(Y)]) end,
    SomePair = fun ({some,{X, Y}}) -> [1, Pair(X, Y)];
                   (none) -> [0]
               end,

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_state_i">>, <<"(3)">>,
              {<<"option((int, int))">>, SomePair({some,{5, 6}})}),

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_state_i">>, <<"(10)">>,
              {<<"option((int, int))">>, SomePair(none)}),

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_state_s">>, <<"(\"three\")">>,
              {<<"option((int, int))">>, SomePair({some,{5, 6}})}),

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_state_s">>, <<"(\"ten\")">>,
              {<<"option((int, int))">>, SomePair(none)}),

    %% Map.lookup_default
    %% The values of map keys 3 and "three" are unchanged, keys 10 and
    %% "ten" don't exist.

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_def_state_i">>,
              <<"(3, (47, 11))">>,
              {<<"(int, int)">>, [word(5), word(6)]}),

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_def_state_i">>,
              <<"(10, (47, 11))">>,
              {<<"(int, int)">>, [word(47), word(11)]}),

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_def_state_s">>,
              <<"(\"three\", (47, 11))">>,
              {<<"(int, int)">>, [word(5), word(6)]}),

    call_func(BPub, BPriv, EncMapsPub,  <<"lookup_def_state_s">>,
              <<"(\"ten\", (47, 11))">>,
              {<<"(int, int)">>, [word(47), word(11)]}),

    %% Map.delete
    %% Check map keys 3 and "three" exist, delete them and check that
    %% they have gone, then put them back for future use.
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_i">>, <<"(3)">>, {<<"bool">>, 1}),
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_s">>, <<"(\"three\")">>, {<<"bool">>, 1}),

    call_func(BPub, BPriv, EncMapsPub, <<"delete_state_i">>, <<"(3)">>),
    call_func(BPub, BPriv, EncMapsPub, <<"delete_state_s">>, <<"(\"three\")">>),

    call_func(BPub, BPriv, EncMapsPub, <<"member_state_i">>, <<"(3)">>, {<<"bool">>, 0}),
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_s">>, <<"(\"three\")">>, {<<"bool">>, 0}),

    call_func(BPub, BPriv, EncTestPub, <<"set_state_i">>, <<"(3, 5, 6)">>),
    call_func(BPub, BPriv, EncTestPub, <<"set_state_s">>, <<"(\"three\", 5, 6)">>),

    %% Map.size
    %% Both of these still contain 3 elements.
    call_func(BPub, BPriv, EncMapsPub,  <<"size_state_i">>, <<"()">>, {<<"int">>, 3}),

    call_func(BPub, BPriv, EncMapsPub,  <<"size_state_s">>, <<"()">>, {<<"int">>, 3}),

    %% Map.to_list, Map.from_list then test if element is there.

    call_func(BPub, BPriv, EncTestPub, <<"list_state_i">>, <<"(242424)">>),
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_i">>, <<"(242424)">>, {<<"bool">>, 1}),
    call_func(BPub, BPriv, EncTestPub, <<"list_state_s">>, <<"(\"xxx\")">>),
    call_func(BPub, BPriv, EncMapsPub, <<"member_state_s">>, <<"(\"xxx\")">>, {<<"bool">>, 1}),

    force_fun_calls(Node),

    ok.

call_get_state(Node, Pub, Priv, EncodedMapsPub) ->
    StateType = <<"( map(int, (int, int)), map(string, (int, int)) )">>,
    {Return,_} = call_compute_func(Node, Pub, Priv,
                                   EncodedMapsPub,
                                   <<"get_state">>, <<"()">>),
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
    Code = compile_test_contract("environment"),

    ContractBalance = 10000,

    %% Initialise contract owned by Alice setting balance to 10000.
    {EncCPub, _, _} =
        create_compute_contract(Node, APub, APriv,
                                Code, <<"(0)">>, #{amount => ContractBalance}),
    %% Second contract for remote calls
    {_, DecRPub, _} =
        create_compute_contract(Node, APub, APriv,
                                Code, <<"(0)">>, #{amount => ContractBalance}),

    %% Get the initial balance.
    BBal1 = get_balance(BPub),

    init_fun_calls(),

    call_func(BPub, BPriv, EncCPub, <<"set_remote">>, args_to_binary([DecRPub])),

    %% Address.
    ct:pal("Calling contract_address\n"),
    call_func(BPub, BPriv, EncCPub, <<"contract_address">>, <<"()">>),
    ct:pal("Calling nested_address\n"),
    call_func(BPub, BPriv, EncCPub, <<"nested_address">>, args_to_binary([DecRPub])),

    %% Balance.
    ct:pal("Calling contract_balance\n"),
    call_func(BPub, BPriv, EncCPub, <<"contract_balance">>, <<"()">>,
              {<<"int">>, ContractBalance}),

    %% Origin.
    ct:pal("Calling call_origin\n"),
    call_func(BPub, BPriv, EncCPub, <<"call_origin">>, <<"()">>),

    ct:pal("Calling nested_origin\n"),
    call_func(BPub, BPriv, EncCPub, <<"nested_origin">>, <<"()">>),

    %% Caller.
    ct:pal("Calling call_caller\n"),
    call_func(BPub, BPriv, EncCPub, <<"call_caller">>, <<"()">>),
    ct:pal("Calling nested_caller\n"),
    call_func(BPub, BPriv, EncCPub, <<"nested_caller">>, <<"()">>),

    %% Value.
    ct:pal("Calling call_value\n"),
    call_func(BPub, BPriv, EncCPub, <<"call_value">>, <<"()">>,
              #{amount => 5}, {<<"int">>, 5}),
    ct:pal("Calling nested_value\n"),
    call_func(BPub, BPriv, EncCPub, <<"nested_value">>, <<"(42)">>),

    %% Gas price.
    ct:pal("Calling call_gas_price\n"),
    ExpectedGasPrice = 2,
    call_func(BPub, BPriv, EncCPub, <<"call_gas_price">>, <<"()">>,
              #{gas_price => ExpectedGasPrice}, {<<"int">>, ExpectedGasPrice}),

    %% Account balances.
    ct:pal("Calling get_balance\n"),
    call_func(BPub, BPriv, EncCPub, <<"get_balance">>, args_to_binary([BPub])),

    %% Block hash.
    ct:pal("Calling block_hash\n"),
    {ok, 200, #{<<"hash">> := ExpectedBlockHash}} = get_key_block_at_height(2),
    {_, <<BHInt:256/integer-unsigned>>} = aehttp_api_encoder:decode(ExpectedBlockHash),

    call_func(BPub, BPriv, EncCPub, <<"block_hash">>, <<"(2)">>, {<<"int">>, BHInt}),

    %% Block hash. With value out of bounds
    ct:pal("Calling block_hash out of bounds\n"),
    call_func(BPub, BPriv, EncCPub, <<"block_hash">>, <<"(10000000)">>, {<<"int">>, 0}),

    %% Coinbase.
    ct:pal("Calling coinbase\n"),

    Beneficiary = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"beneficiary">> := B}} = get_key_block(KeyHash),
                    {_, <<BInt:256/integer-unsigned>>} = aehttp_api_encoder:decode(B),
                    BInt
                  end,
    call_func(BPub, BPriv, EncCPub, <<"coinbase">>, <<"()">>, {<<"address">>, Beneficiary}),

    %% Block timestamp.
    ct:pal("Calling timestamp\n"),

    Timestamp = fun(Hdr) -> maps:get(<<"time">>, Hdr) end,
    call_func(BPub, BPriv, EncCPub, <<"timestamp">>, <<"()">>, {<<"int">>, Timestamp}),

    %% Block height.
    ct:pal("Calling block_height\n"),
    BlockHeight = fun(Hdr) -> maps:get(<<"height">>, Hdr) end,
    call_func(BPub, BPriv, EncCPub, <<"block_height">>, <<"()">>, {<<"int">>, BlockHeight}),

    %% Difficulty.
    ct:pal("Calling difficulty\n"),
    Difficulty = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"target">> := T}} = get_key_block(KeyHash),
                    aec_pow:target_to_difficulty(T)
                 end,
    call_func(BPub, BPriv, EncCPub, <<"difficulty">>, <<"()">>, {<<"int">>, Difficulty}),

    %% Gas limit.
    ct:pal("Calling gas_limit\n"),
    call_func(BPub, BPriv, EncCPub, <<"gas_limit">>, <<"()">>,
              {<<"int">>, aec_governance:block_gas_limit()}),

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
    Code = compile_test_contract("spend_test"),

    %% Initialise contracts owned by Alice with balance set to 10000 and 20000.
    {EncC1Pub,DecC1Pub,_} =
        create_compute_contract(Node, APub, APriv, Code, <<"()">>, #{amount => 10000}),
    {EncC2Pub,DecC2Pub,_} =
        create_compute_contract(Node, APub, APriv, Code, <<"()">>, #{amount => 20000}),

    init_fun_calls(),

    %% Alice does all the operations on the contract and spends on Bert.
    %% Check the contract balances.
    call_func(APub, APriv, EncC1Pub, <<"get_balance">>, <<"()">>, {<<"int">>, 10000}),
    call_func(APub, APriv, EncC2Pub, <<"get_balance">>, <<"()">>, {<<"int">>, 20000}),

    %% Spend 15000 on to Bert.
    Sp1Arg = args_to_binary([BPub,15000]),
    call_func(APub, APriv, EncC2Pub, <<"spend">>, Sp1Arg, {<<"int">>, 5000}),

    %% Check that contract spent it.
    GBO1Arg = args_to_binary([DecC2Pub]),
    call_func(APub, APriv, EncC1Pub, <<"get_balance_of">>, GBO1Arg, {<<"int">>, 5000}),

    %% Check that Bert got it.
    GBO2Arg = args_to_binary([BPub]),
    call_func(APub, APriv, EncC1Pub, <<"get_balance_of">>, GBO2Arg, {<<"int">>, BBal0 + 15000}),

    %% Spend 6000 explicitly from contract 1 to Bert.
    SF1Arg = args_to_binary([DecC1Pub,BPub,6000]),
    call_func(APub, APriv, EncC2Pub, <<"spend_from">>, SF1Arg, {<<"int">>, BBal0 + 21000}),

    %% Check that Bert got it.
    GBO3Arg = args_to_binary([BPub]),
    call_func(APub, APriv, EncC1Pub, <<"get_balance_of">>, GBO3Arg, {<<"int">>, BBal0 + 21000}),

    %% Check contract 2 balance.
    GBO4Arg = args_to_binary([DecC2Pub]),
    call_func(APub, APriv, EncC1Pub, <<"get_balance_of">>, GBO4Arg, {<<"int">>, 5000}),

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
    Code = compile_test_contract("dutch_auction"),

    %% Set auction start amount and decrease per mine and fee.
    StartAmt = 50000,
    Decrease = 1000,
    Fee      = 800000,

    %% Initialise contract owned by Alice with Carl as benficiary.
    InitArgument = args_to_binary([CPub,StartAmt,Decrease]),
    {EncCPub,_,InitReturn} =
        create_compute_contract(Node, APub, APriv, Code, InitArgument),
    #{<<"height">> := Height0} = InitReturn,

    %% Mine 5 times to decrement value.
    {ok,_} = aecore_suite_utils:mine_key_blocks(Node, 5),

    _ABal1 = get_balance(APub),
    BBal1 = get_balance(BPub),
    CBal1 = get_balance(CPub),

    %% Call the contract bid function by Bert.
    {_,#{return := BidReturn}} =
        call_compute_func(Node, BPub, BPriv,
                          EncCPub,
                          <<"bid">>, <<"()">>,
                          #{amount => 100000,fee => Fee}),
    #{<<"gas_used">> := GasUsed,<<"height">> := Height1} = BidReturn,

    %% Set the cost from the amount, decrease and diff in height.
    Cost = StartAmt - (Height1 - Height0) * Decrease,

    BBal2 = get_balance(BPub),
    CBal2 = get_balance(CPub),

    %% Check that the balances are correct, don't forget the gas and the fee.
    BBal2 = BBal1 - Cost - GasUsed - Fee,
    CBal2 = CBal1 + Cost,

    %% Now make a bid which should fail as auction has closed.
    revert_call_compute_func(Node, DPub, DPriv, EncCPub,
                             <<"bid">>, <<"()">>,
                             #{amount => 100000,fee => Fee}),
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
    Code = compile_test_contract("fundme"),

    %% Get the current height.
    {ok,200,#{<<"height">> := StartHeight}} = get_key_blocks_current_height(),

    %% Set deadline and goal.
    Duration = 10,
    Deadline = StartHeight + Duration,
    Goal     = 150000,

    %% Initialise contract owned by Alice with Bert as benficiary.
    InitArg = args_to_binary([BPub,Deadline,Goal]),
    {EncCPub,_,_} = create_compute_contract(Node, APub, APriv, Code, InitArg),

    init_fun_calls(),

    %% Let Carl and Diana contribute and check if we can withdraw early.
    call_func(CPub, CPriv, EncCPub, <<"contribute">>, <<"()">>, #{<<"amount">> => 100000}, none),

    %% This should fail as we have not reached the goal.
    call_func(BPub, BPriv, EncCPub, <<"withdraw">>, <<"()">>, revert),

    call_func(DPub, DPriv, EncCPub, <<"contribute">>, <<"()">>, #{<<"amount">> => 100000}, none),

    %% This should fail as we have not reached the deadline.
    call_func(BPub, BPriv, EncCPub, <<"withdraw">>, <<"()">>, revert),

    force_fun_calls(Node),

    {ok,200,#{<<"height">> := TmpHeight}} = get_key_blocks_current_height(),
    ct:log("Now at ~p started at ~p, need ~p to pass deadline!", [TmpHeight, StartHeight, Duration - (TmpHeight - StartHeight)]),

    %% Mine enough times to get past deadline.
    {ok,_} = aecore_suite_utils:mine_key_blocks(Node, Duration - (TmpHeight - StartHeight)),

    %% Now withdraw the amount
    call_func(BPub, BPriv, EncCPub, <<"withdraw">>, <<"()">>),

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
    Code = compile_test_contract("erc20_token"),

    %% Default values, 100000, 10, "Token Name", "TKN".
    Total = 100000,
    Decimals = 10,
    Name = <<"Token Name">>,
    Symbol = <<"TKN">>,

    %% Initialise contract owned by Alice.
    InitArg = args_to_binary([Total,Decimals,{string,Name},{string,Symbol}]),
    {EncCPub,_,_} =
        create_compute_contract(Node, APub, APriv, Code, InitArg),

    init_fun_calls(),

    %% Test state record fields.
    call_func(APub, APriv, EncCPub, <<"totalSupply">>, <<"()">>, {<<"int">>, Total}),
    call_func(APub, APriv, EncCPub, <<"decimals">>, <<"()">>,    {<<"int">>, Decimals}),
    call_func(APub, APriv, EncCPub, <<"name">>, <<"()">>,        {<<"string">>, Name}),
    call_func(APub, APriv, EncCPub, <<"symbol">>, <<"()">>,      {<<"string">>, Symbol}),

    %% Setup balances for Bert to 20000 and Carl to 25000 and check balances.
    call_func(APub, APriv, EncCPub, <<"transfer">>, args_to_binary([BPub, 20000])),
    call_func(APub, APriv, EncCPub, <<"transfer">>, args_to_binary([CPub, 25000])),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([APub]), {<<"int">>, 55000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([BPub]), {<<"int">>, 20000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([CPub]), {<<"int">>, 25000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([DPub]), {<<"int">>, 0}),

    %% Bert and Carl approve transfering 15000 to Alice.
    call_func(BPub, BPriv, EncCPub, <<"approve">>, args_to_binary([APub,15000])),
    force_fun_calls(Node), %% We need to ensure ordering, so force here!
    call_func(CPub, CPriv, EncCPub, <<"approve">>, args_to_binary([APub,15000])),
    force_fun_calls(Node), %% We need to ensure ordering, so force here!

    %% Alice transfers 10000 from Bert and 15000 Carl to Diana.
    call_func(APub, APriv, EncCPub, <<"transferFrom">>, args_to_binary([BPub,DPub,10000])),
    call_func(APub, APriv, EncCPub, <<"transferFrom">>, args_to_binary([CPub,DPub,15000])),

    %% Check the balances.
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([BPub]), {<<"int">>, 10000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([CPub]), {<<"int">>, 10000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([DPub]), {<<"int">>, 25000}),

    %% Check transfer and approval logs.
    TrfLog = [tuple([addr(CPub), addr(DPub), word(15000)]),
              tuple([addr(BPub), addr(DPub), word(10000)]),
              tuple([addr(APub), addr(CPub), word(25000)]),
              tuple([addr(APub), addr(BPub), word(20000)])],
    call_func(APub, APriv, EncCPub, <<"getTransferLog">>, <<"()">>,
              {<<"list((address,address,int))">>, TrfLog}),

    AppLog = [tuple([addr(CPub), addr(APub), word(15000)]),
              tuple([addr(BPub), addr(APub), word(15000)])],
    call_func(APub, APriv, EncCPub, <<"getApprovalLog">>, <<"()">>,
              {<<"list((address,address,int))">>, AppLog}),

    force_fun_calls(Node),

    %% Request a bad transfer and check that abort restores the state.
    revert_call_compute_func(Node, APub, APriv, EncCPub,
                             <<>>,
                             call_code("transferFrom", [BPub,DPub,100000])),    %% new-style calldata creation

    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([BPub]), {<<"int">>, 10000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([CPub]), {<<"int">>, 10000}),
    call_func(APub, APriv, EncCPub, <<"balanceOf">>, args_to_binary([DPub]), {<<"int">>, 25000}),

    call_func(APub, APriv, EncCPub, <<"getTransferLog">>, <<"()">>,
              {<<"list((address,address,int))">>, TrfLog}),
    call_func(APub, APriv, EncCPub, <<"getApprovalLog">>, <<"()">>,
              {<<"list((address,address,int))">>, AppLog}),

    force_fun_calls(Node),

    ok.

%% compiler_error_contract(Config)
%%  Check HTTP output from compiler errors.

compiler_error_contract(_Config) ->
    %% Test syntax error.
    {ok,403,#{<<"reason">> := SyntaxError}} =
        get_contract_bytecode(<<"contract Fail = function fail(x : string) == x + 1">>),
    <<"** Parse errors\n\n",_/binary>> = SyntaxError,

    %% Test type error.
    {ok,403,#{<<"reason">> := TypeError}} =
        get_contract_bytecode(<<"contract Fail = function fail(x : string) = x + 1">>),
    TypeError = <<"** Type errors\n\n"
                  "Cannot unify string\n"
                  "         and int\n"
                  "when checking the application at line 1, column 47 of\n"
                  "  (+) : (int, int) => int\n"
                  "to arguments\n"
                  "  x : string\n"
                  "  1 : int\n">>,

    %% Test code gen error.
    {ok,403,#{<<"reason">> := GenError}} =
        get_contract_bytecode(<<"contract Fail =\n  function fail(x : address) = Chain.balance">>),
    <<"** Code errors\n\n",_/binary>> = GenError,

    %% This generates a function_clause error.
    {ok,403,#{<<"reason">> := DeclError}} =
        get_contract_bytecode(<<"contract Fail =\n  type sune\n  function fail(x : address) = Chain.balance">>),
    <<"Error: function_clause",_/binary>> = DeclError,

    ok.


remote_gas_test_contract(Config) ->
    %% Set an account.
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub,
                 priv_key := APriv}} = proplists:get_value(accounts, Config),

    init_fun_calls(),

    %% Set up two contracts.
    %% Compile test contract "remote_gas_test.aes".
    Code = compile_test_contract("remote_gas_test"),

    {EncC1Pub,_,_} =
        create_compute_contract(Node, APub, APriv, Code, args_to_binary([0]),
                                #{amount => 200}),


    {EncC2Pub,DecC2Pub,_} =
        create_compute_contract(Node, APub, APriv, Code, args_to_binary([100])),


    force_fun_calls(Node),
    Balance0 = get_balance(APub),
    call_get(APub, APriv, EncC1Pub,  0),
    call_get(APub, APriv, EncC2Pub,  100),
    force_fun_calls(Node),
    Balance1 = get_balance(APub),
    1600476 = Balance0 - Balance1,

    %% Test remote call with limited gas
    %% Call contract remote set function with limited gas
    [] = call_func(APub, APriv, EncC1Pub, <<"call">>,
                   args_to_binary([DecC2Pub, 1, 80000])),
    call_get(APub, APriv, EncC2Pub,  1),
    force_fun_calls(Node),
    Balance2 = get_balance(APub),
    1610687 = Balance1 - Balance2,

    %% Test remote call with limited gas (3) that fails (out of gas).
    [] = call_func(APub, APriv, EncC1Pub, <<"call">>,
                   args_to_binary([DecC2Pub, 2, 3]), error),
    force_fun_calls(Node),
    Balance3 = get_balance(APub),
    809933 = Balance2 - Balance3,

    %% Check that store/state not changed (we tried to write 2).
    call_get(APub, APriv, EncC2Pub,  1),
    force_fun_calls(Node),
    Balance4 = get_balance(APub),

    %% Test remote call with limited gas that fails (invald call).
    [] = call_func(APub, APriv, EncC1Pub, <<"call">>,
                   args_to_binary([APub, 2, 1]), error),
    force_fun_calls(Node),
    Balance5 = get_balance(APub),
    900000 = Balance4 - Balance5,

    ok.

%% Data structure functions.
word(Val) -> #{<<"type">> => <<"word">>, <<"value">> => Val}.

tuple(Vals) -> #{<<"type">> => <<"tuple">>, <<"value">> => Vals}. %Sneaky

addr(Addr) -> <<Int:256>> = Addr, word(Int).

%% Internal access functions.

get_balance(Pubkey) ->
    Addr = aehttp_api_encoder:encode(account_pubkey, Pubkey),
    {ok,200,#{<<"balance">> := Balance}} = get_account_by_pubkey(Addr),
    Balance.

decode_data(Type, EncodedData) ->
    {ok,200,#{<<"data">> := DecodedData}} =
         get_contract_decode_data(#{'sophia-type' => Type,
                                    data => EncodedData}),
    DecodedData.

%% call_func_decode(NodeName, Pubkey, Privkey, EncodedContractPubkey,
%%                  Function, Arguments, ReturnType)
%% call_func_decode(NodeName, Pubkey, Privkey, EncodedContractPubkey,
%%                  Function, Arguments, CallerSet, ReturnType)

call_func_decode(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                 Function, Arg, Type) ->
    call_func_decode(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                     Function, Arg, #{}, Type).

call_func_decode(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                 Function, Arg, CallerSet, Type) ->
    {Return,_} = call_compute_func(NodeName, Pubkey, Privkey,
                                   EncodedContractPubkey,
                                   Function, Arg, CallerSet),
    #{<<"value">> := Value} = decode_data(Type, Return),
    Value.

%% Contract interface functions.

compile_test_contract(Name) ->
    CodeDir = filename:join(code:lib_dir(aehttp), "../../extras/test/contracts"),
    compile_test_contract(CodeDir, Name).

%% For testing with contracts not part of the aesophia repository
compile_test_contract(Dir, Name) ->
    {ok, SophiaCode} = file:read_file(filename:join([Dir, lists:concat([Name, ".aes"])])),
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),
    Code.

%% create_compute_contract(NodeName, Pubkey, Privkey, Code, InitArgument) ->
%%     {EncodedContractPubkey,DecodedContractPubkey,InitReturn}.
%%  Create contract and mine blocks until in chain.

create_compute_contract(NodeName, Pubkey, Privkey, Code, InitArgument) ->
    create_compute_contract(NodeName, Pubkey, Privkey, Code, InitArgument, #{}).

create_compute_contract(NodeName, Pubkey, Privkey, Code, InitArgument, CallerSet) ->
    {#{tx_hash := TxHash},EncodedContractPubkey,DecodedContractPubkey} =
        contract_create_compute_tx(Pubkey, Privkey, Code, InitArgument, CallerSet),

    %% Mine blocks and check that it is in the chain.
    ok = wait_for_tx_hash_on_chain(NodeName, TxHash),
    ?assert(tx_in_chain(TxHash)),

    %% Get value of last call.
    {ok,200,InitReturn} = get_contract_call_object(TxHash),
    ct:pal("Init return ~p\n", [InitReturn]),

    {EncodedContractPubkey,DecodedContractPubkey,InitReturn}.

%% init_fun_calls()
%% force_fun_calls(Node)
%% call_func(Pubkey, Privkey, EncodedContractPubkey, Function, Arguments)
%% call_func(Pubkey, Privkey, EncodedContractPubkey, Function, Arguments, Check)

init_fun_calls() ->
    put(fun_calls, []), put(nonces, []).

force_fun_calls(Node) ->
    Calls = put(fun_calls, []),
    put(nonces, []),

    %% First try them with dry run...
    dry_run_txs(Calls),

    %% Then really put them on the chain
    TxHashes = [ TxHash || {#{tx_hash := TxHash}, _} <- Calls ],
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, TxHashes, 20),
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
    {ok, 200, CallReturn} = get_contract_call_object(TxHash),
    ct:pal("Call return ~p\n", [CallReturn]),
    check_call_(Check, CallReturn, TxHash).

check_call_(Check, CallObject, TxHash) ->
    #{<<"return_type">> := RetType, <<"return_value">> := Value} = CallObject,

    %% Get the block where the tx was included
    case Check of
        none -> ?assertEqual(<<"ok">>, RetType);
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

check_value(Val0, {Type, ExpVal}) ->
    #{<<"value">> := Val} = decode_data(Type, Val0),
    ct:log("~p decoded\nas ~p\ninto ~p =??= ~p", [Val0, Type, Val, ExpVal]),
    ?assertEqual(ExpVal, Val).


call_func(Pub, Priv, EncCPub, Fun, Args) ->
    call_func(Pub, Priv, EncCPub, Fun, Args, #{}, none).

call_func(Pub, Priv, EncCPub, Fun, Args, Check) ->
    call_func(Pub, Priv, EncCPub, Fun, Args, #{}, Check).

call_func(Pub, Priv, EncCPub, Fun, Args, CallerSet, Check) ->
    Nonce = get_nonce(Pub),
    Tx =
        contract_call_compute_tx(Pub, Priv, Nonce, EncCPub, Fun, Args, CallerSet),

    Calls = get(fun_calls),
    put(fun_calls, Calls ++ [{Tx, Check}]).

get_nonce(Pub) ->
    Address = aehttp_api_encoder:encode(account_pubkey, Pub),
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

%% call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
%%                   Function, Arguments)
%% call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
%%                   Function, Arguments, CallerSet)

call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                  Function, Argument) ->
    call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                      Function, Argument, #{}).

call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                  Function, Argument, CallerSet) ->
    {CallReturn,ContractCallTxHash} =
        basic_call_compute_func(NodeName, Pubkey, Privkey,
                                EncodedContractPubkey,
                                 Function, Argument, CallerSet),

    #{<<"return_type">> := <<"ok">>,
      <<"return_value">> := Value} = CallReturn,

    %% Get the block where the tx was included
    {ok, 200, #{<<"block_hash">> := BlockHash}} = get_tx(ContractCallTxHash),
    {ok, 200, BlockHeader} = get_micro_block_header(BlockHash),

    {Value, #{header => BlockHeader, return => CallReturn}}.

%% revert_call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
%%                          Function, Arguments)
%% revert_call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
%%                          Function, Arguments, CallerSet)

revert_call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                         Function, Argument) ->
    revert_call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                             Function, Argument, #{}).

revert_call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                         Function, Argument, CallerSet) ->
    {CallReturn,_} = basic_call_compute_func(NodeName, Pubkey, Privkey,
                                             EncodedContractPubkey,
                                             Function, Argument, CallerSet),

    #{<<"return_type">> := <<"revert">>,
      <<"return_value">> := Value} = CallReturn,
    {Value,CallReturn}.

basic_call_compute_func(NodeName, Pubkey, Privkey, EncodedContractPubkey,
                        Function, Argument, CallerSet) ->
    #{tx_hash := ContractCallTxHash} =
        contract_call_compute_tx(Pubkey, Privkey, EncodedContractPubkey,
                                 Function, Argument, CallerSet),

    %% Mine blocks and check that it is in the chain.
    ok = wait_for_tx_hash_on_chain(NodeName, ContractCallTxHash),
    ?assert(tx_in_chain(ContractCallTxHash)),

    %% Get the call object and return value.
    {ok,200,CallReturn} = get_contract_call_object(ContractCallTxHash),
    ct:pal("Call return ~p\n", [CallReturn]),

    {CallReturn,ContractCallTxHash}.

contract_create_compute_tx(Pubkey, Privkey, Code, InitArgument, CallerSet) ->
    Address = aehttp_api_encoder:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_account_by_pubkey(Address),
    Nonce = Nonce0 + 1,

    CallInput =
        case InitArgument of
            B when is_binary(B) ->
                #{ arguments => InitArgument }; %% old calldata creation
            {code, CallCode} ->
                #{ call => CallCode }
        end,

    %% The default init contract.
    ContractInitEncoded0 = #{ owner_id => Address,
                              code => Code,
                              vm_version => ?CURRENT_AEVM_SOPHIA,
                              deposit => 2,
                              amount => 0,      %Initial balance
                              gas => 100000,   %May need a lot of gas
                              gas_price => 1,
                              fee => 1400000,
                              nonce => Nonce,
                              payload => <<"create contract">>},
    ContractInitEncoded = maps:merge(maps:merge(ContractInitEncoded0, CallInput), CallerSet),
    sign_and_post_create_compute_tx(Privkey, ContractInitEncoded).

contract_call_compute_tx(Pubkey, Privkey, EncodedContractPubkey,
                         Function, Argument, CallerSet) ->
    Address = aehttp_api_encoder:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok,200,#{<<"nonce">> := Nonce0}} = get_account_by_pubkey(Address),
    Nonce = Nonce0 + 1,
    contract_call_compute_tx(Pubkey, Privkey, Nonce, EncodedContractPubkey,
                             Function, Argument, CallerSet).

contract_call_compute_tx(Pubkey, Privkey, Nonce, EncodedContractPubkey,
                         Function, Argument, CallerSet) ->
    Address = aehttp_api_encoder:encode(account_pubkey, Pubkey),

    CallInput =
        case Argument of
            B when is_binary(B) ->
                #{ function => Function
                 , arguments => Argument }; %% old calldata creation
            {code, CallCode} ->
                #{ call => CallCode }
        end,

    ContractCallEncoded0 = #{ caller_id => Address,
                              contract_id => EncodedContractPubkey,
                              vm_version => ?CURRENT_AEVM_SOPHIA,
                              amount => 0,
                              gas => 100000,    %May need a lot of gas
                              gas_price => 1,
                              fee => 800000,
                              nonce => Nonce,
                              payload => <<"call compute function">> },
    ContractCallEncoded = maps:merge(maps:merge(ContractCallEncoded0, CallInput), CallerSet),
    sign_and_post_call_compute_tx(Privkey, ContractCallEncoded).

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

get_contract_bytecode(SourceCode) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/compile",
                 #{ <<"code">> => SourceCode, <<"options">> => <<>> }).

%% get_contract_create(Data) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "debug/contracts/create", Data).

get_contract_create_compute(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create/compute", Data).

%% get_contract_call(Data) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "debug/contracts/call", Data).

get_contract_call_compute(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/call/compute", Data).

get_contract_decode_data(Request) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/decode-data", Request).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

get_tx(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ binary_to_list(TxHash), []).

create_spend_tx(RecipientId, Amount, Fee) ->
    Sender = maps:get(pubkey, aecore_suite_utils:patron()),
    SenderId = aehttp_api_encoder:encode(account_pubkey, Sender),
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
    {ok, TxSer} = aehttp_api_encoder:safe_decode(transaction, Tx),
    UTx = aetx:deserialize_from_binary(TxSer),
    STx = aec_test_utils:sign_tx(UTx, [maps:get(privkey, aecore_suite_utils:patron())]),
    aehttp_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(STx)).

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
    Fee = 20000,
    {ok, 200, #{<<"tx">> := SpendTx}} =
        create_spend_tx(aehttp_api_encoder:encode(account_pubkey, Pubkey), Balance, Fee),
    SignedSpendTx = sign_tx(SpendTx),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),
    {Pubkey,Privkey,SpendTxHash}.

sign_and_post_create_compute_tx(Privkey, CreateEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx,
              <<"contract_id">> := EncodedPubkey}} =
        get_contract_create_compute(CreateEncoded),
    {ok,DecodedPubkey} = aehttp_api_encoder:safe_decode(contract_pubkey,
                                                 EncodedPubkey),
    Tx = sign_and_post_tx(Privkey, EncodedUnsignedTx),
    {Tx,EncodedPubkey,DecodedPubkey}.

sign_and_post_call_compute_tx(Privkey, CallEncoded) ->
    {ok,200,#{<<"tx">> := EncodedUnsignedTx}} =
        get_contract_call_compute(CallEncoded),
    sign_and_post_tx(Privkey, EncodedUnsignedTx).

sign_and_post_tx(PrivKey, EncodedUnsignedTx) ->
    {ok,SerializedUnsignedTx} = aehttp_api_encoder:safe_decode(transaction,
                                                        EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aec_test_utils:sign_tx(UnsignedTx, PrivKey),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    SendTx = aehttp_api_encoder:encode(transaction, SerializedTx),
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
            case aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TxHash], 20) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
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

call_code(Fun, Args) ->
    Type    = ["(", string:join([ type_of_arg(Arg) || Arg <- Args ], ", "), ")"],
    BinArgs = args_to_list(Args),
    {code, list_to_binary(
        [ "contract Call =\n"
        , "  function ", Fun, " : ", Type, " => _\n"
        , "  function __call() = ", Fun, "(", BinArgs, ")\n" ])}.

type_of_arg(N) when is_integer(N) -> "int";
type_of_arg({string, _}) -> "string";
type_of_arg([H | _]) -> ["list(", type_of_arg(H), ")"];
type_of_arg([]) -> "list(int)"; %% Don't know the element type
type_of_arg(B) when is_binary(B), byte_size(B) == 32 -> "address";
type_of_arg(B) when is_binary(B), byte_size(B) == 64 -> "signature";
type_of_arg(T) when is_tuple(T) ->
    ["(", string:join([ type_of_arg(X) || X <- tuple_to_list(T) ], ","), ")"];
type_of_arg(M) when is_map(M) ->
    case maps:to_list(M) of
        []  -> %% empty map: can't infer type, default to int/int
            "map(int, int)";
        [{K, V} | _] ->
            ["map(", type_of_arg(K), ", ", type_of_arg(V), ")"]
    end.

%% args_to_binary(Args) -> binary_string().
%%  Take a list of arguments in "erlang format" and generate an
%%  argument binary string. Strings are handled naively now.

args_to_binary(Args) ->
    %% ct:pal("Args ~tp\n", [Args]),
    BinArgs = list_to_binary([$(,args_to_list(Args),$)]),
    %% ct:pal("BinArgs ~tp\n", [BinArgs]),
    BinArgs.

args_to_list([A]) -> [arg_to_list(A)];          %The last one
args_to_list([A1|Rest]) ->
    [arg_to_list(A1),$,|args_to_list(Rest)];
args_to_list([]) -> [].

%%arg_to_list(<<N:256>>) -> integer_to_list(N);
arg_to_list(N) when is_integer(N) -> integer_to_list(N);
arg_to_list(B) when is_binary(B) ->             %A key
    <<"0x", Enc/binary>> = aeu_hex:hexstring_encode(B),
    ["#", binary_to_list(Enc)];
arg_to_list({string,S}) -> ["\"",S,"\""];
arg_to_list(L) when is_list(L) ->
    [$[,args_to_list(L),$]];
arg_to_list(T) when is_tuple(T) ->
    [$(,args_to_list(tuple_to_list(T)),$)];
arg_to_list(M) when is_map(M) ->
    [${,map_to_list(maps:to_list(M)),$}].

map_to_list([{K,V}]) -> [$[,arg_to_list(K),"] = ",arg_to_list(V)];
map_to_list([{K,V},Fields]) ->
    [$[,arg_to_list(K),"] = ",arg_to_list(V),$,|map_to_list(Fields)];
map_to_list([]) -> [].
