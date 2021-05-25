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
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").


%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0, rpc/4]).
-export([new_account/1]).

%% test case exports
%% external endpoints
-export([
         abort_test_contract/1,
         counter_contract/1,
         dutch_auction_contract/1,
         acm_dutch_auction_contract/1,
         environment_contract/1,
         %% environment_contract_fate/1,
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
         payable_contract/1,
         paysplit_contract/1,
         paying_for_contract/1
        ]).

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).

-define(MAX_MINED_BLOCKS, 20).

-define(skipRest(Res, Reason),
    case Res of
        true  -> throw({skip, {skip_rest, Reason}});
        false -> ok
    end).


all() ->
    [ {group, aevm}
    , {group, fate}
    ].

-define(ALL_TESTS,
        [ identity_contract
        , abort_test_contract
        , simple_storage_contract
        , counter_contract
        , stack_contract
        , polymorphism_test_contract
        , factorial_contract
        , maps_contract
        , environment_contract
        , spend_test_contract
        , dutch_auction_contract
        , acm_dutch_auction_contract
        , fundme_contract
        , erc20_token_contract
        , remote_gas_test_contract
        , events_contract
        , payable_contract
        , paysplit_contract
        , paying_for_contract
        ]).

groups() ->
    [ {aevm, [], ?ALL_TESTS}
    , {fate, [], ?ALL_TESTS}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => false,
                     <<"hard_forks">> => Forks}},
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [ {instant_mining, true}
                                                                 , {symlink_name, "latest.http_contracts"}
                                                                 , {test_module, ?MODULE}] ++ Config),
    aecore_suite_utils:start_node(?NODE, Config1),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    [ {node_name, Node}
    , {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
    ] ++ Config1.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    ok.

init_per_group(VM, Config) when VM == aevm; VM == fate ->
    aect_test_utils:init_per_group(VM, Config, fun(Cfg) -> init_per_vm(Cfg) end).

init_per_vm(Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),

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
    [{accounts,Accounts}|Config].


end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    aect_test_utils:setup_testcase(Config),
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
    call_func(CPub, CPriv, EncCPub, Contract, "main_", ["42"], {"identity", "main_", 42}),

    %% Call contract main function by Diana.
    call_func(DPub, DPriv, EncCPub, Contract, "main_", ["42"], {"identity", "main_", 42}),

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
    BeforeList = call_func_decode(Node, APub, APriv, EncodedInt3Pub, ICode, "get_values", []),
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
    #{<<"abort">> := [AbortBin]} = decode_call_result(ICode, "do_abort", revert, RevertValue1),
    #{<<"gas_used">> := GasUsed1} = Return1,
    ?assert(GasUsed1 < GivenGas), %% Make sure we actually saved some gas

    %% Also check that we get the same behaviour when calling directly to the
    %% leaf contract.
    {RevertValue2, Return2} =
        revert_call_compute_func(Node, APub, APriv, EncodedTestPub, TCode,
                                 "do_abort", ["42", AbortString], #{gas => GivenGas}),
    #{<<"abort">> := [AbortBin]} = decode_call_result(TCode, "do_abort", revert, RevertValue2),
    #{<<"gas_used">> := GasUsed2} = Return2,
    ?assert(GasUsed2 < GivenGas), %% Make sure we actually saved some gas

    ABal1 = get_balance(APub),

    %% The contract values should be the same, 17, 1017, 2017, 3017.
    AfterList = call_func_decode(Node, APub, APriv, EncodedInt3Pub, ICode,
                                 "get_values", []),
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
    call_func(Pub, Priv, EncCPub, Contract, "get", [], {maps:get(name, Contract), "get", ExpValue}).

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

    %% Test the size.
    call_func(BPub, BPriv, EncCPub, Contract, "size", [], {"stack", "size", 2}),

    %% Push 2 more elements.
    call_func(BPub, BPriv, EncCPub, Contract, "push", ["\"three\""]),
    call_func(BPub, BPriv, EncCPub, Contract, "push", ["\"four\""]),

    %% Test the size.
    call_func(BPub, BPriv, EncCPub, Contract, "size", [], {"stack", "size", 4}),

    %% Check the stack.
    call_func(BPub, BPriv, EncCPub, Contract, "all", [],
              {"stack", "all", [<<"four">>, <<"three">>, <<"two">>, <<"one">>]}),

    %% Pop the values and check we get them in the right order.\
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"stack", "pop", <<"four">>}),
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"stack", "pop", <<"three">>}),
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"stack", "pop", <<"two">>}),
    call_func(BPub, BPriv, EncCPub, Contract, "pop", [], {"stack", "pop", <<"one">>}),

    %% The resulting stack is empty.
    call_func(BPub, BPriv, EncCPub, Contract, "size", [], {"stack", "size", 0}),

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

    call_func(APub, APriv, EncCPub, Contract, "foo", [], {"polymorphism_test", "foo", [5, 7, 9]}),
    call_func(APub, APriv, EncCPub, Contract, "bar", [], {"polymorphism_test", "foo", [1, 0, 3]}),

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

    Pt   = fun(X, Y) -> #{<<"x">> => X, <<"y">> => Y} end,
    Some = fun(V) -> #{<<"Some">> => [V]} end,
    None = <<"None">>,
    %% m[k]
    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_i",  ["2"],
              {"maps", "get_state_i", Pt(3, 4)}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_s",  ["\"three\""],
              {"maps", "get_state_i", Pt(5, 6)}),

    %% m{[k] = v}
    %% State now {map_i = {[1]=>{x=11,y=22},[2]=>{x=3,y=4},[3]=>{x=5,y=6}},
    %%            map_s = ["one"]=> ... , ["two"]=> ... , ["three"] => ...}
    %% Need to call interface functions as cannot create record as argument.
    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_i", ["1", "{x = 11, y = 22}"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_s", ["\"one\"", "{x = 11, y = 22}"]),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_i", ["1"],
              {"maps", "get_state_i", Pt(11, 22)}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_s",  ["\"one\""],
              {"maps", "get_state_s", Pt(11, 22)}),

    %% m{f[k].x = v}
    call_func(BPub, BPriv, EncMapsPub, Contract, "setx_state_i", ["2, 33"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "setx_state_s", ["\"two\", 33"]),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_i",  ["2"],
              {"maps", "get_state_i", Pt(33, 4)}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "get_state_s",  ["\"two\""],
              {"maps", "get_state_s", Pt(33, 4)}),

    %% Map.member
    %% Check keys 1 and "one" which exist and 10 and "ten" which don't.
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["1"], {"maps", "member_state_i", true}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["10"], {"maps", "member_state_i", false}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"one\""], {"maps", "member_state_s", true}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"ten\""], {"maps", "member_state_s", false}),

    %% Map.lookup
    %% The values of map keys 3 and "three" are unchanged, keys 10 and
    %% "ten" don't exist.
    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_i", ["3"],
              {"maps", "lookup_state_i", Some(Pt(5, 6))}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_i", ["10"],
              {"maps", "lookup_state_i", None}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_s", ["\"three\""],
              {"maps", "lookup_state_s", Some(Pt(5, 6))}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_state_s", ["\"ten\""],
              {"maps", "lookup_state_s", None}),

    %% Map.lookup_default
    %% The values of map keys 3 and "three" are unchanged, keys 10 and
    %% "ten" don't exist.

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_i",
              ["3", "{x = 47, y = 11}"], {"maps", "lookup_def_state_i", Pt(5, 6)}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_i",
              ["10", "{x = 47, y = 11}"], {"maps", "lookup_def_state_i", Pt(47, 11)}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_s",
              ["\"three\"", "{x = 47, y = 11}"], {"maps", "lookup_def_state_s", Pt(5, 6)}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "lookup_def_state_s",
              ["\"ten\"", "{x = 47, y = 11}"], {"maps", "lookup_def_state_s", Pt(47, 11)}),

    %% Map.delete
    %% Check map keys 3 and "three" exist, delete them and check that
    %% they have gone, then put them back for future use.
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["3"], {"maps", "member_state_i", true}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"three\""], {"maps", "member_state_s", true}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "delete_state_i", ["3"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "delete_state_s", ["\"three\""]),

    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["3"], {"maps", "member_state_i", false}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"three\""], {"maps", "member_state_s", false}),

    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_i", ["3", "{x = 5, y = 6}"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "set_state_s", ["\"three\"", "{x = 5, y = 6}"]),

    %% Map.size
    %% Both of these still contain 3 elements.
    call_func(BPub, BPriv, EncMapsPub, Contract,  "size_state_i", [], {"maps", "size_state_i", 3}),

    call_func(BPub, BPriv, EncMapsPub, Contract,  "size_state_s", [], {"maps", "size_state_s", 3}),

    %% Map.to_list, Map.from_list then test if element is there.
    call_func(BPub, BPriv, EncMapsPub, Contract, "fromlist_state_i", ["[(242424, {x = 43, y = 44})]"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_i", ["242424"], {"maps", "member_state_i", true}),
    call_func(BPub, BPriv, EncMapsPub, Contract, "fromlist_state_s", ["[(\"xxx\", {x = 43, y = 44})]"]),
    call_func(BPub, BPriv, EncMapsPub, Contract, "member_state_s", ["\"xxx\""], {"maps", "member_state_s", true}),

    force_fun_calls(Node, 10 * ?MAX_MINED_BLOCKS), %% Many txs, so wait for longer.

    ok.

call_get_state(Node, Pub, Priv, EncMapsPub, Contract) ->
    {Return,_} = call_compute_func(Node, Pub, Priv, EncMapsPub, Contract, "get_state", []),
    decode_call_result(Contract, "get_state", ok, Return).

%% enironment_contract(Config)
%%  Check the Environment contract. We don't always check values and
%%  the nested calls don't seem to work yet.

environment_contract(Config) ->
    with_trace(fun environment_contract_/1, Config, "environment_contract").

environment_contract_(Config) ->
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

    CallF = fun(Pub, Priv, EC, C, F, As, Exp) -> call_func(Pub, Priv, EC, C, F, As, {"environment", F, Exp}) end,

    call_func(BPub, BPriv, EncCPub, Contract, "set_remote", [EncRPub]),

    %% Address.
    ct:pal("Calling contract_address\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "contract_address", []),
    ct:pal("Calling nested_address\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "nested_address", [EncRPub]),

    %% Balance.
    ct:pal("Calling contract_balance\n"),
    CallF(BPub, BPriv, EncCPub, Contract, "contract_balance", [], ContractBalance),

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
              #{gas_price => ExpectedGasPrice}, {"environment", "call_gas_price", ExpectedGasPrice}),

    %% Account balances.
    ct:pal("Calling get_balance\n"),
    call_func(BPub, BPriv, EncCPub, Contract, "get_balance",
              [aeser_api_encoder:encode(account_pubkey, BPub)]),

    %% Block hash.
    ct:pal("Calling block_hash\n"),
    {ok, 200, #{<<"hash">> := ExpectedBlockHash}} = get_key_block_at_height(2),
    {_, Hash} = aeser_api_encoder:decode(ExpectedBlockHash),
    HexHash = string:to_lower(aeu_hex:bin_to_hex(Hash)),

    case aect_test_utils:sophia_version() of
        VM1 when VM1 == ?SOPHIA_ROMA; VM1 == ?SOPHIA_MINERVA; VM1 == ?SOPHIA_FORTUNA ->
            %% It returns an int here, but we can't decode that... So just check for ok
            call_func(BPub, BPriv, EncCPub, Contract, "block_hash", ["2"]);
        _ ->
            CallF(BPub, BPriv, EncCPub, Contract, "block_hash", ["2"],
                  #{<<"Some">> => [list_to_binary([$# | HexHash])]})
    end,

    %% Block hash. With value out of bounds
    ct:pal("Calling block_hash out of bounds\n"),
    case aect_test_utils:sophia_version() of
        VM2 when VM2 == ?SOPHIA_ROMA; VM2 == ?SOPHIA_MINERVA; VM2 == ?SOPHIA_FORTUNA ->
            %% It returns an int here, but we can't decode that... So just check for ok
            call_func(BPub, BPriv, EncCPub, Contract, "block_hash", ["10000000"]);
        _ ->
            CallF(BPub, BPriv, EncCPub, Contract, "block_hash", ["10000000"], <<"None">>)
    end,

    %% Coinbase.
    ct:pal("Calling coinbase\n"),

    Beneficiary = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    {ok, 200, #{<<"beneficiary">> := B}} = get_key_block(KeyHash),
                    B
                  end,
    CallF(BPub, BPriv, EncCPub, Contract, "coinbase", [], Beneficiary),

    %% Block timestamp.
    ct:pal("Calling timestamp\n"),

    Timestamp = fun(Hdr) -> maps:get(<<"time">>, Hdr) end,
    CallF(BPub, BPriv, EncCPub, Contract, "timestamp", [], Timestamp),

    %% Block height.
    ct:pal("Calling block_height\n"),
    BlockHeight = fun(Hdr) -> maps:get(<<"height">>, Hdr) end,
    CallF(BPub, BPriv, EncCPub, Contract, "block_height", [], BlockHeight),

    %% Difficulty.
    ct:pal("Calling difficulty\n"),
    Difficulty = fun(Hdr) ->
                    #{<<"prev_key_hash">> := KeyHash} = Hdr,
                    %% There are other tests which check the target with bitcoin_ng consensus
                    {ok, 200, #{<<"target">> := 1337}} = get_key_block(KeyHash),
                    1337
                 end,
    CallF(BPub, BPriv, EncCPub, Contract, "difficulty", [], Difficulty),

    %% Gas limit.
    ct:pal("Calling gas_limit\n"),
    CallF(BPub, BPriv, EncCPub, Contract, "gas_limit", [], aec_governance:block_gas_limit()),

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
                          "bid", [], #{amount => 100000, fee => Fee}),
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
                             "bid", [], #{amount => 100000, fee => Fee}),
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
    Contract = compile_test_contract("fundme"),

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

    CallF = fun(Pub, Priv, EC, C, F, As, Exp) ->
                call_func(Pub, Priv, EC, C, F, As, {"erc20_token", F, Exp}) end,

    %% Test state record fields.
    CallF(APub, APriv, EncCPub, Contract, "totalSupply", [],  Total),
    CallF(APub, APriv, EncCPub, Contract, "decimals", [],     Decimals),
    CallF(APub, APriv, EncCPub, Contract, "name", [],         Name),
    CallF(APub, APriv, EncCPub, Contract, "symbol", [],       Symbol),

    %% Setup balances for Bert to 20000 and Carl to 25000 and check balances.
    call_func(APub, APriv, EncCPub, Contract, "transfer",  [EBPub, "20000"]),
    call_func(APub, APriv, EncCPub, Contract, "transfer",  [ECPub, "25000"]),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EAPub],  55000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EBPub],  20000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [ECPub],  25000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EDPub],  0),

    %% Bert and Carl approve transfering 15000 to Alice.
    call_func(BPub, BPriv, EncCPub, Contract, "approve", [EAPub,"15000"]),
    force_fun_calls(Node), %% We need to ensure ordering, so force here!
    call_func(CPub, CPriv, EncCPub, Contract, "approve", [EAPub,"15000"]),
    force_fun_calls(Node), %% We need to ensure ordering, so force here!

    %% Alice transfers 10000 from Bert and 15000 Carl to Diana.
    call_func(APub, APriv, EncCPub, Contract, "transferFrom", [EBPub, EDPub, "10000"]),
    call_func(APub, APriv, EncCPub, Contract, "transferFrom", [ECPub, EDPub, "15000"]),

    %% Check the balances.
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EBPub],  10000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [ECPub],  10000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EDPub],  25000),

    %% Check transfer and approval logs.
    Addr = fun(A) -> aeser_api_encoder:encode(account_pubkey, A) end,
    TrfLog = [[Addr(CPub), Addr(DPub), 15000],
              [Addr(BPub), Addr(DPub), 10000],
              [Addr(APub), Addr(CPub), 25000],
              [Addr(APub), Addr(BPub), 20000]],
    CallF(APub, APriv, EncCPub, Contract, "getTransferLog", [], TrfLog),

    AppLog = [[Addr(CPub), Addr(APub), 15000],
              [Addr(BPub), Addr(APub), 15000]],
    CallF(APub, APriv, EncCPub, Contract, "getApprovalLog", [], AppLog),

    force_fun_calls(Node),

    %% Request a bad transfer and check that abort restores the state.
    revert_call_compute_func(Node, APub, APriv, EncCPub, Contract,
                             "transferFrom", [EBPub, EDPub, "100000"]),    %% new-style calldata creation

    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EBPub],  10000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [ECPub],  10000),
    CallF(APub, APriv, EncCPub, Contract, "balanceOf", [EDPub],  25000),

    CallF(APub, APriv, EncCPub, Contract, "getTransferLog", [], TrfLog),
    CallF(APub, APriv, EncCPub, Contract, "getApprovalLog", [], AppLog),

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
                <<E1:256>> = case aect_test_utils:latest_protocol_version() >= ?FORTUNA_PROTOCOL_VSN of
                                true -> aec_hash:blake2b_256_hash(<<"Event1">>);
                                false -> aec_hash:hash(evm, <<"Event1">>)
                             end,
                ?assertEqual(Addr, EncCPub),
                ?assertEqual(Data, aeser_api_encoder:encode(contract_bytearray, <<"bar">>)),
                ?assertMatch([E1, 1, 2], Ts)
              end,
    call_func(APub, APriv, EncCPub, Contract, "f1", ["1", "\"bar\""], {log, F1Check}),

    F2Check = fun([#{<<"address">> := Addr, <<"topics">> := Ts, <<"data">> := Data}]) ->
                <<E2:256>> = case aect_test_utils:latest_protocol_version() >= ?FORTUNA_PROTOCOL_VSN of
                                true -> aec_hash:blake2b_256_hash(<<"Event2">>);
                                false -> aec_hash:hash(evm, <<"Event2">>)
                             end,
                <<A:256>>  = APub,
                ?assertEqual(Addr, EncCPub),
                ?assertEqual(Data, aeser_api_encoder:encode(contract_bytearray, <<"foo">>)),
                ?assertEqual([E2, A], Ts)
              end,
    call_func(APub, APriv, EncCPub, Contract, "f2", ["\"foo\""], {log, F2Check}),

    force_fun_calls(Node).

-define(assertMatchVM(AEVM, FATE, Res),
    case ?IS_AEVM_SOPHIA(aect_test_utils:vm_version()) of
        true  -> ?assertMatch(AEVM, Res);
        false -> ?assertMatch(FATE, Res)
    end).

-define(assertMatchVM(ExpVm1, ExpVm2, ExpVm3, ExpVm4, ExpFate1, ExpFate2, Res),
    case aect_test_utils:vm_version() of
        ?VM_AEVM_SOPHIA_1 -> ?assertMatch(ExpVm1, Res);
        ?VM_AEVM_SOPHIA_2 -> ?assertMatch(ExpVm2, Res);
        ?VM_AEVM_SOPHIA_3 -> ?assertMatch(ExpVm3, Res);
        ?VM_AEVM_SOPHIA_4 -> ?assertMatch(ExpVm4, Res);
        ?VM_FATE_SOPHIA_1 -> ?assertMatch(ExpFate, Res);
        ?VM_FATE_SOPHIA_2 -> ?assertMatch(ExpFate, Res)
    end).

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
    ?assertMatchVM(1600596, 1600596, 1600596, 1600916, 1600022, 1600024, (Balance0 - Balance1) div ?DEFAULT_GAS_PRICE),

    %% Test remote call with limited gas
    %% Call contract remote set function with limited gas
    [] = call_func(APub, APriv, EncC1Pub, Contract, "call", [EncC2Pub, "1", "80000"]),
    call_get(APub, APriv, EncC2Pub, Contract, 1),
    force_fun_calls(Node),
    Balance2 = get_balance(APub),
    ?assertMatchVM(1610855, 1610855, 1610855, 1611335, 1600231, 1600231, (Balance1 - Balance2) div ?DEFAULT_GAS_PRICE),

    %% Test remote call with limited gas (3) that fails (out of gas).
    [] = call_func(APub, APriv, EncC1Pub, Contract, "call", [EncC2Pub, "2", "3"], error),
    force_fun_calls(Node),
    Balance3 = get_balance(APub),
    ?assertMatchVM(809981, 809981, 809981, 809981, 800147, 800150, (Balance2 - Balance3) div ?DEFAULT_GAS_PRICE),

    %% Check that store/state not changed (we tried to write 2).
    call_get(APub, APriv, EncC2Pub, Contract, 1),
    force_fun_calls(Node),
    Balance4 = get_balance(APub),

    %% Test remote call with limited gas that fails (invald call).
    ZeroContract = aeser_api_encoder:encode(contract_pubkey, <<0:256>>),
    [] = call_func(APub, APriv, EncC1Pub, Contract, "call", [ZeroContract, "2", "1"], error),
    force_fun_calls(Node),
    Balance5 = get_balance(APub),
    ?assertMatchVM(900000, 900000, 900000, 900000, 800145, 800148, (Balance4 - Balance5) div ?DEFAULT_GAS_PRICE),

    ok.

payable_contract(Config) ->
    ?skipRest(aect_test_utils:vm_version() =< ?VM_AEVM_SOPHIA_3, payable_not_pre_lima),
    %% Set an account.
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub, priv_key := BPriv}} = proplists:get_value(accounts, Config),

    init_fun_calls(),
    Payable = compile_test_contract("payable"),
    NonPayable = compile_test_contract("non_payable"),

    {_EncCPubP, DecP, _} = create_contract(Node, APub, APriv, Payable, []),
    {_EncCPubNP, DecNP, _} = create_contract(Node, APub, APriv, NonPayable, []),

    force_fun_calls(Node),

    EAccP = aeser_api_encoder:encode(account_pubkey, DecP),
    EAccNP = aeser_api_encoder:encode(account_pubkey, DecNP),
    EAPub = aeser_api_encoder:encode(account_pubkey, APub),
    EBPub = aeser_api_encoder:encode(account_pubkey, BPub),

    {ok, 200, #{<<"tx">> := SpendTx1}} = create_spend_tx(EAPub, EAccP, 10000, 20000 * ?DEFAULT_GAS_PRICE, <<"good">>),
    {ok, 200, #{<<"tx">> := SpendTx2}} = create_spend_tx(EBPub, EAccNP, 10000, 20000 * ?DEFAULT_GAS_PRICE, <<"bad">>),

    #{tx_hash := TxHash1} = sign_and_post_tx(APriv, SpendTx1),
    #{tx_hash := TxHash2} = sign_and_post_tx(BPriv, SpendTx2),

    %% If both are mineable they _will_ end up in the same block.
    wait_for_tx_hash_on_chain(Node, TxHash1),

    ?assert(tx_in_chain(TxHash1)),
    ?assert(not tx_in_chain(TxHash2)),

    ok.

paysplit_contract(Config) ->
    ?skipRest(aect_test_utils:latest_protocol_version() < ?LIMA_PROTOCOL_VSN, payable_not_pre_lima),
    %% Set an account.
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub},
      acc_c := #{pub_key := CPub},
      acc_d := #{pub_key := DPub}} = proplists:get_value(accounts, Config),

    init_fun_calls(),
    Paysplit = compile_test_contract("paysplit"),

    EAPub = aeser_api_encoder:encode(account_pubkey, APub),
    EBPub = aeser_api_encoder:encode(account_pubkey, BPub),
    ECPub = aeser_api_encoder:encode(account_pubkey, CPub),
    EDPub = aeser_api_encoder:encode(account_pubkey, DPub),
    SplitMap = "{ [" ++ binary_to_list(EBPub) ++ "] = 30, "
               "  [" ++ binary_to_list(EAPub) ++ "] = 10, "
               "  [" ++ binary_to_list(EDPub) ++ "] = 10, "
               "  [" ++ binary_to_list(ECPub) ++ "] = 50 }",
    {EncCPub, _Dec, _} = create_contract(Node, APub, APriv, Paysplit, [SplitMap]),

    ECheck = fun([_]) -> ok end,

    call_func(APub, APriv, EncCPub, Paysplit, "payAndSplit", [], #{<<"amount">> => 100000}, {log, ECheck}),
    call_func(APub, APriv, EncCPub, Paysplit, "payAndSplit", [], #{}, revert),
    call_func(APub, APriv, EncCPub, Paysplit, "payAndSplit", [], #{<<"amount">> => 100000, <<"gas">> => 1000}, error),

    force_fun_calls(Node),

    ok.

paying_for_contract(Config) ->
    ?skipRest(aect_test_utils:latest_protocol_version() < ?IRIS_PROTOCOL_VSN, paying_for_not_pre_iris),
    Node = proplists:get_value(node_name, Config),
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub, priv_key := BPriv}} = proplists:get_value(accounts, Config),

    {BrokePub1, BrokePriv1} = aecore_suite_utils:generate_key_pair(),
    %% {BrokePub2, BrokePriv2} = aecore_suite_utils:generate_key_pair(),

    MakeSignSerialize =
      fun(SignedInnerTx) ->
        {ok, PayingForTx} = aec_paying_for_tx:new(#{payer_id => aeser_id:create(account, APub),
                                                    nonce    => get_online_nonce(APub),
                                                    fee      => 10000 * aec_test_utils:min_gas_price(),
                                                    tx       => SignedInnerTx}),
        SignedPayingForTx = aec_test_utils:sign_tx(PayingForTx, APriv),
        aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(SignedPayingForTx))
      end,

    SpendTx = aega_test_utils:spend_tx(#{ sender_id => aeser_id:create(account, BPub)
                                        , recipient_id => aeser_id:create(account, BrokePub1)
                                        , nonce => get_online_nonce(BPub) }),
    SignSpendTx = aec_test_utils:sign_pay_for_inner_tx(SpendTx, BPriv),

    SerSignPayingForTx1 = MakeSignSerialize(SignSpendTx),

    {ok, 200, #{<<"tx_hash">> := SignPayingForTx1Hash}} = post_tx(SerSignPayingForTx1),

    %% Negative test
    BadSignSpendTx = aec_test_utils:sign_tx_hash(SpendTx, BPriv),
    BadSerialTx = MakeSignSerialize(BadSignSpendTx),

    {ok,400,#{<<"reason">> := <<"Invalid tx">>}} = post_tx(BadSerialTx),


    APre = get_balance(APub),
    BPre = get_balance(BPub),

    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [SignPayingForTx1Hash], ?MAX_MINED_BLOCKS),

    APost = get_balance(APub),
    BPost = get_balance(BPub),
    Broke1Post = get_balance(BrokePub1),
    ?assertEqual(123456, Broke1Post),

    ct:pal("Spend:\n  APub: ~p ~p\n  BPub: ~p ~p\n  Bro1: ~p", [APre, APost, BPre, BPost, Broke1Post]),

    init_fun_calls(),
    Payable = compile_test_contract("payable"),

    {_EncCPubP, DecP, _} = create_contract(Node, APub, APriv, Payable, []),
    force_fun_calls(Node),

    {ok, Calldata1} = aect_test_utils:encode_call_data(maps:get(src, Payable), "foo", ["42"]),
    CallSpec1 = #{call_data => Calldata1, abi_version => aect_test_utils:abi_version(),
                  amount => 100,
                  nonce => get_online_nonce(BrokePub1) },
    Call1Tx = aect_test_utils:call_tx(BrokePub1, DecP, CallSpec1, #{}),

    SignCall1Tx = aec_test_utils:sign_pay_for_inner_tx(Call1Tx, BrokePriv1),

    SerSignPayingForTx2 = MakeSignSerialize(SignCall1Tx),
    {ok, 200, #{<<"tx_hash">> := SignPayingForTx2Hash}} = post_tx(SerSignPayingForTx2),

    APre2 = get_balance(APub),
    Broke1Pre2 = get_balance(BrokePub1),

    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [SignPayingForTx2Hash], ?MAX_MINED_BLOCKS),

    APost2 = get_balance(APub),
    Broke1Post2 = get_balance(BrokePub1),

    ct:pal("Spend:\n  APub: ~p ~p\n  Bro1: ~p ~p", [APre2, APost2, Broke1Pre2, Broke1Post2]),

    ?assertEqual(Broke1Post2, Broke1Pre2 - 100),

    ok.


%% Internal access functions.

get_balance(Pubkey) ->
    Addr = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok,200,#{<<"balance">> := Balance}} = get_account_by_pubkey(Addr),
    Balance.

decode_data(Type, EncodedData) ->
    case aect_test_utils:decode_data(Type, EncodedData) of
        {ok, #{ <<"value">> := Val }} -> Val;
        {ok, Val}                     -> Val
    end.

call_func_decode(NodeName, Pubkey, Privkey, EncCPubkey,
                 Contract, Function, Arg) ->
    call_func_decode(NodeName, Pubkey, Privkey, EncCPubkey,
                     Contract, Function, Arg, #{}).

call_func_decode(NodeName, Pubkey, Privkey, EncCPubkey,
                 Contract, Function, Arg, CallerSet) ->
    {Return,_} = call_compute_func(NodeName, Pubkey, Privkey, EncCPubkey,
                                   Contract, Function, Arg, CallerSet),
    decode_call_result(Contract, Function, ok, Return).

%% Contract interface functions.
compile_test_contract(Name) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    #{ bytecode => aeser_api_encoder:encode(contract_bytearray, Code),
       vm       => aect_test_utils:vm_version(),
       abi      => aect_test_utils:abi_version(),
       code     => Code,
       name     => Name,
       src      => binary_to_list(BinSrc) }.

encode_calldata(Contract, Fun, Args) ->
    {ok, Calldata} = aect_test_utils:encode_call_data(maps:get(src, Contract), Fun, Args),
    {ok, aeser_api_encoder:encode(contract_bytearray, Calldata)}.

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
    Txs = [ #{tx => Tx} || {#{tx_encoded := Tx}, _} <- Calls ],
    {ok, 200, #{ <<"results">> := Results }} = dry_run(Txs),
    {ok, 200, #{ <<"results">> := Results
               , <<"tx_events">> := TxEvents }} = dry_run_w_events(Txs),
    [#{ <<"tx_hash">> := <<"th_", _/binary>> } = E || E <- TxEvents],
    ct:log("TxEvents = ~p", [TxEvents]),
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
            case RetType of
                <<"ok">> ->
                    check_value(Value, Check);
                Other ->
                    error({unexpected, Other, aeser_api_encoder:safe_decode(contract_bytearray, Value)})
            end;
        {Contract, Fun, BFun} when is_function(BFun) ->
            ?assertEqual(<<"ok">>, RetType),
            case TxHash of
                undefined ->
                    ok;
                _ ->
                    {ok, 200, #{<<"block_hash">> := BlockHash}} = get_tx(TxHash),
                    {ok, 200, BlockHeader} = get_micro_block_header(BlockHash),
                    check_value(Contract, Fun, ok, Value, BFun(BlockHeader))
            end;
        {Contract, Fun, Exp} ->
            case RetType of
                <<"ok">> ->
                    check_value(Contract, Fun, ok, Value, Exp);
                Other ->
                    error({unexpected, Other, aeser_api_encoder:safe_decode(contract_bytearray, Value)})
            end;
        error ->
            ?assertEqual(<<"error">>, RetType);
        revert ->
            ?assertEqual(<<"revert">>, RetType)
    end.

check_value(Val0, {Type, ExpVal}) ->
    Val = decode_data(Type, Val0),
    ct:log("~p decoded\nas ~p\ninto ~p =??= ~p", [Val0, Type, Val, ExpVal]),
    ?assertEqual(ExpVal, Val).

check_value(Contract, Fun, ResType, ResValue, ExpVal) ->
    Val = decode_call_result(Contract, Fun, ResType, ResValue),
    ct:log("~p decoded\ninto ~p =??= ~p", [ResValue, Val, ExpVal]),
    ?assertEqual(ExpVal, Val).

decode_call_result(Contract = #{}, Fun, ResType, ResValue) ->
    decode_call_result(maps:get(name, Contract), Fun, ResType, ResValue);
decode_call_result(ContractName, Fun, ResType, ResValue) ->
    {ok, BinCode} = aect_test_utils:read_contract(?SOPHIA_IRIS_FATE, ContractName),
    aect_test_utils:decode_call_result(binary_to_list(BinCode), Fun, ResType, ResValue).

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

get_online_nonce(Pub) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pub),
    case get_account_by_pubkey(Address) of
        {ok,200,#{<<"nonce">> := Nonce}} -> Nonce + 1;
        {ok, _, _} -> 1
    end.

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
    {CallReturn, ContractCallTxHash} =
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

dry_run_w_events(Txs) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/dry-run", #{ txs => Txs
                                                            , tx_events => true }).

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
                   payload => Payload,
                   %% Just to test serialize_for_client with some TTL other than 0
                   ttl => 100000}).

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
new_account(Balance) ->
    {Pubkey,Privkey} = aecore_suite_utils:generate_key_pair(),
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

with_trace(F, Config, File) ->
    with_trace(F, Config, File, on_error).

with_trace(F, Config, File, When) ->
    ct:log("with_trace ...", []),
    TTBRes = aesc_ttb:on_nodes([node()|get_nodes()], File),
    ct:log("Trace set up: ~p", [TTBRes]),
    try F(Config)
    catch E:R:Stack ->
        case E of
            error ->
                ct:pal("Error ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                erlang:error(R);
            exit ->
                ct:pal("Exit ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                exit(R);
            throw ->
                ct:pal("Caught throw:~p", [R]),
                throw(R)
        end
    end,
    case When of
        on_error ->
            ct:log("Discarding trace", []),
            aesc_ttb:stop_nofetch();
        always ->
            ttb_stop()
    end,
    ok.

ttb_stop() ->
    Dir = aesc_ttb:stop(),
    Out = filename:join(filename:dirname(Dir),
                        filename:basename(Dir) ++ ".txt"),
    case aesc_ttb:format(Dir, Out, #{limit => 30000}) of
        {error, Reason} ->
            ct:pal("TTB formatting error: ~p", [Reason]);
        _ ->
            ok
    end,
    ct:pal("Formatted trace log in ~s~n", [Out]).

get_nodes() ->
    [aecore_suite_utils:node_name(?NODE)].
