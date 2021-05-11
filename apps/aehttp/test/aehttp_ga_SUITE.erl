-module(aehttp_ga_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% use include_lib for aecontract to compile under system test
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-import(aecore_suite_utils, [http_request/4]).

%% test case exports
%% external endpoints
-export([ attach/1
        , attach_fail/1
        , get_account_by_pubkey/1
        , get_account_by_pubkey_and_height/1
        , dry_run/1
        , dry_run_fail/1
        , meta_fail/1
        , meta_fail_auth/1
        , meta_spend/1
        , meta_oracle_register/1
        , meta_meta_fail_auth/1
        , meta_meta_fail/1
        , meta_meta_spend/1
        , attach_second/1
        , meta_sc_create/1
        , meta_sc_create_fail/1
        , meta_4_fail/1
        , mempool/1
        , mempool_rejects_normal_tx/1
        ]).

-define(NODE, dev1).
-define(NODENAME, aecore_suite_utils:node_name(?NODE)).
-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(MAX_MINED_BLOCKS, 20).
-define(MINE_BLOCKS(N), aecore_suite_utils:mine_key_blocks(?NODENAME, N)).
-define(MINE_TXS(Txs), aecore_suite_utils:mine_blocks_until_txs_on_chain(?NODENAME, Txs, ?MAX_MINED_BLOCKS)).

-define(assertMatchABI(AEVM, FATE, Res),
    case abi_version() of
        ?ABI_AEVM_SOPHIA_1 -> ?assertMatch(AEVM, Res);
        ?ABI_FATE_SOPHIA_1 -> ?assertMatch(FATE, Res)
    end).

all() ->
    [{group, swagger2},
     {group, oas3}
    ].

groups() ->
    [{swagger2, [sequence], [{group, aevm}, {group, fate}]},
     {oas3, [sequence], [{group, aevm}, {group, fate}]},
     {aevm, [sequence], [{group, ga_txs}, {group, ga_info}, {group, ga_mempool}]},
     {fate, [sequence], [{group, ga_txs}, {group, ga_info}, {group, ga_mempool}]},
     {ga_txs, [sequence],
      [ attach_fail
      , attach
      , dry_run
      , dry_run_fail
      , meta_fail_auth
      , meta_fail
      , meta_spend
      , meta_oracle_register
      , meta_meta_fail_auth
      , meta_meta_fail
      , meta_meta_spend
      , meta_4_fail
      , attach_second
      , meta_sc_create
      , meta_sc_create_fail
      ]},

     {ga_info, [sequence],
      [ attach
      , get_account_by_pubkey
      , get_account_by_pubkey_and_height
      ]},

     {ga_mempool, [sequence],
      [ attach
      , mempool
      , mempool_rejects_normal_tx ]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Forks = aecore_suite_utils:forks(),
    %% We want to run some suites with and some suites without client side cache
    %% Arbitrary run this suite with cache disabled.
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => false,
                     <<"hard_forks">> => Forks},
              <<"http">> =>
                   #{<<"cache">> => #{<<"enabled">> => false}}},
    Config1 = [{instant_mining, true}, {symlink_name, "latest.http_ga"}, {test_module, ?MODULE}] ++ Config0,
    Config2 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, Config1),
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config2),
    aecore_suite_utils:connect(NodeName, []),
    [{node_name, NodeName}, {nodes, [aecore_suite_utils:node_tuple(?NODE)]}] ++ Config2.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    ok.

init_per_group(SwaggerVsn, Config) when SwaggerVsn =:= swagger2;
                                        SwaggerVsn =:= oas3 ->
    [{swagger_version, SwaggerVsn} | Config];
init_per_group(VMGroup, Config) when VMGroup == aevm; VMGroup == fate ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        _ -> aect_test_utils:init_per_group(VMGroup, Config)
    end;
init_per_group(_GAGroup, Config) ->
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ ?MINE_BLOCKS(ToMine) || ToMine > 0 ],

    %% Prepare accounts
    StartAmt = 1000 * 1000 * 1000000 * ?DEFAULT_GAS_PRICE,
    {APK, ASK, STx1} = new_account(StartAmt),
    {BPK, BSK, STx2} = new_account(StartAmt),

    {ok, _} = ?MINE_TXS([STx1, STx2]),

    %% Save account information
    Accounts = #{acc_a => #{pub_key => APK, priv_key => ASK, start_amt => StartAmt},
                 acc_b => #{pub_key => BPK, priv_key => BSK, start_amt => StartAmt}},
    [{accounts, Accounts} | Config].

end_per_group(_VMGroup, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    SwaggerVsn = proplists:get_value(swagger_version, Config),
    aecore_suite_utils:use_swagger(SwaggerVsn),
    put('$vm_version',     ?config(vm_version,     Config)),
    put('$abi_version',    ?config(abi_version,    Config)),
    put('$sophia_version', ?config(sophia_version, Config)),
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_, N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% Attach a basic authentication contract to Account A
attach(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),
    attach_account(APub, APriv, Config).

attach_second(Config) ->
    %% Get account information.
    #{acc_b := #{pub_key := BPub, priv_key := BPriv}} = proplists:get_value(accounts, Config),
    attach_account(BPub, BPriv, Config).

attach_account(Pub, Priv, _Config) ->
    Addr = aeser_api_encoder:encode(account_pubkey, Pub),
    {ok, 200, Account0} = account_by_pubkey(Addr),
    Bal0 = maps:get(<<"balance">>, Account0),
    ?assertEqual(<<"basic">>, maps:get(<<"kind">>, Account0)),

    #{tx_hash := AttachTx, sign_tx := STx} = post_attach_tx(Pub, Priv),

    %% Check that we can dry-run GAAttachTx
    do_dry_run(STx, ok),

    ?MINE_TXS([AttachTx]),

    {ok, 200, Account1} = account_by_pubkey(Addr),
    Bal1 = maps:get(<<"balance">>, Account1),

    %% test that we can return GAAttachTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAAttachTx">>}}} = get_tx(AttachTx),
    {ok, 200, #{<<"call_info">> := #{<<"gas_used">> := Gas}}} = get_contract_call_object(AttachTx),

    ct:pal("Cost: ~p", [Bal0 - Bal1]),
    MGP = aec_test_utils:min_gas_price(),
    ExpBal = Bal0 - 1000000 * MGP - Gas * MGP,
    ?assertEqual(ExpBal, Bal1),
    ok.

attach_fail(Config) ->
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),

    AttachTxMap = make_attach_tx_map(APub),

    Fail = fun(ATMap) ->
                Tx     = aega_test_utils:ga_attach_tx(APub, ATMap),
                STx    = aec_test_utils:sign_tx(Tx, APriv),
                SerTx  = aetx_sign:serialize_to_binary(STx),
                SendTx = aeser_api_encoder:encode(transaction, SerTx),
                {ok, 400, #{<<"reason">> := _}} = post_tx(SendTx)
           end,

    Fail(AttachTxMap#{ nonce     => 123 }),
    Fail(AttachTxMap#{ fee       => 123 }),
    Fail(AttachTxMap#{ gas_price => 123 }),

    ok.

%% Dry-running GA transactions
dry_run(Config) ->
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),
    #{public := XPub, secret := _XPriv} = enacl:sign_keypair(),

    MGP = aec_test_utils:min_gas_price(),
    SpendTx = spend_tx(APub, APriv, 0, XPub, 1234, 20000 * MGP),

    do_dry_run(SpendTx, ok),

    ok.


dry_run_fail(Config) ->
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),
    #{public := XPub, secret := _XPriv} = enacl:sign_keypair(),

    %% Ensure we cant dry-run a Meta transaction...
    MGP = aec_test_utils:min_gas_price(),
    SpendTx = spend_tx(APub, APriv, 0, XPub, 1234, 20000 * MGP),
    SMetaTx = ga_meta_tx(["1"], APub, APriv, SpendTx, 100000 * MGP, 10000),
    _MetaTx = aetx_sign:tx(SMetaTx),

    do_dry_run(SMetaTx, error),

    ok.


%% A Meta with a failing authentication
meta_fail_auth(Config) ->
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["100"], BPub, 12345, 20000 * MGP, MetaFee),

    ?MINE_BLOCKS(3),

    %% test that we can return GAMetaTx from mempool
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 404, #{<<"reason">> := _NotMined}} = get_contract_call_object(MetaTx),
    ok.

%% A meta with a failing inner TX
meta_fail(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    %% Fail inner tx by spending (far) too much
    #{tx_hash := MetaTx} =
        post_ga_spend_tx(APub, APriv, ["1"], BPub, 1000 * MGP * MGP, MGP * 20000, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"error">>,
                                  <<"gas_used">> := Gas}}} = get_contract_call_object(MetaTx),

    ct:pal("Cost failing inner: ~p", [ABal0 - ABal1]),
    ExpBal = ABal0 - (MetaFee + Gas * 1000 * MGP),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_spend(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["2"], BPub, 10000, 20000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"ok">>,
                                   <<"gas_used">> := Gas}}} =
        get_contract_call_object(MetaTx),

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ExpBal = ABal0 - (MetaFee + Gas * 1000 * MGP + 20000 * MGP + 10000),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_oracle_register(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    MGP = aec_test_utils:min_gas_price(),

    InnerTx = aeo_test_utils:register_tx(APub, #{}), 
    MetaTx = ga_meta_tx(["1"], APub, APriv, InnerTx, 100000 * MGP, 10000),
    #{tx_hash := MetaTxHash} = post_aetx(MetaTx),

    ?MINE_TXS([MetaTxHash]),

    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTxHash),

    ok.

meta_meta_fail_auth(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["3", "5"], BPub, 10000, 20000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"error">>,
                                  <<"gas_used">> := Gas}}} =
        get_contract_call_object(MetaTx),

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ExpBal = ABal0 - (MetaFee + Gas * 1000 * MGP),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_meta_fail(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["4", "5"], BPub, 1000 * MGP * MGP, 20000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}} = JSONTx} = get_tx(MetaTx),
    ct:log("Transaction: ~p", [JSONTx]),

    {ok, 200, #{<<"ga_info">> := GAInfo}} =
        get_contract_call_object(MetaTx),

    ct:log("Transaction info: ~p", [GAInfo]),
    #{<<"return_type">> := <<"ok">>, <<"inner_object">> := InnerObj,
      <<"gas_used">> := Gas} = GAInfo,

    #{<<"ga_info">> := #{<<"return_type">> := <<"error">>,
                         <<"inner_object">> := #{<<"tx_info">> := <<"spend_tx">>}}} = InnerObj,

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ExpBal = ABal0 - 2 * (MetaFee + Gas * 1000 * MGP),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_meta_spend(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,


    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["6", "7"], BPub, 10000, 20000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}} = JSONTx} = get_tx(MetaTx),
    ct:log("Transaction: ~p", [JSONTx]),

    {ok, 200, #{<<"ga_info">> := #{<<"gas_used">> := Gas} = GAInfo}} =
        get_contract_call_object(MetaTx),

    ct:log("Transaction info: ~p", [GAInfo]),
    #{<<"return_type">> := <<"ok">>} = GAInfo,

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ExpBal = ABal0 - (2 * (MetaFee + Gas * 1000 * MGP) + 20000 * MGP + 10000),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_4_fail(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["8", "9", "10", "12"], APub, 10000, 20000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}} = JSONTx} = get_tx(MetaTx),
    ct:log("Transaction: ~p", [JSONTx]),

    {ok, 200, #{<<"ga_info">> := #{<<"gas_used">> := Gas} = GAInfo}} =
        get_contract_call_object(MetaTx),

    ct:log("Transaction info: ~p", [GAInfo]),
    #{<<"return_type">> := <<"ok">>, <<"inner_object">> := InnerObj} = GAInfo,

    #{<<"ga_info">> := #{<<"inner_object">> :=
        #{<<"ga_info">> := #{<<"return_type">> := <<"error">>}}}} = InnerObj,

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ExpBal = ABal0 - 3 * (MetaFee + Gas * 1000 * MGP),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_sc_create(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub, priv_key := BPriv}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),

    #{tx_hash := MetaTx} = post_ga_sc_create_tx(APub, APriv, "11", BPub, BPriv, "1"),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"ok">>,
                                   <<"gas_used">> := Gas} = GI}} =
        get_contract_call_object(MetaTx),

    ct:pal("GAS: ~p\n", [Gas]),
    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ct:pal("~p + ~p + ~p", [100000 * MGP, Gas * MGP, 20000 * MGP]),
    ct:pal("GInfo: ~p\n", [GI]),
    ExpBal = ABal0 - (100000 * MGP + Gas * 1000 * MGP + 20000 * MGP + 20000),
    ?assertEqual(ExpBal, ABal1),
    ok.

meta_sc_create_fail(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub, priv_key := BPriv}} = proplists:get_value(accounts, Config),

    #{tx_hash := MetaTx} = post_ga_sc_create_fail_tx(APub, APriv, "12", BPub, BPriv),

    ?MINE_TXS([MetaTx]),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := ReturnType}}} =
        get_contract_call_object(MetaTx),

    %% Until LIMA it was possible to use a GA account (as responder)
    %% and sign with the private key...
    case aect_test_utils:latest_protocol_version() of
      Vsn when Vsn < ?LIMA_PROTOCOL_VSN ->
        ?assertEqual(<<"ok">>, ReturnType);
      _ ->
        ?assertEqual(<<"error">>, ReturnType)
    end,

    ok.

%% Test the minimum gas price check
mempool(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 20000) * MGP,
    TooLowMetaFee = (1 * 15000 + 20000) * MGP,

    %% Test with too low fee
    not_accepted = post_ga_spend_tx(APub, APriv, ["1"], BPub, 10001, MGP * 15000, TooLowMetaFee),
    %% Test with too low fee in inner Tx
    not_accepted = post_ga_spend_tx(APub, APriv, ["1"], BPub, 10001, MGP * 10000, MetaFee),

    %% Test with too much gas for auth function
    not_accepted = post_ga_spend_tx(APub, APriv, ["1"], APub, 10001, 15000 * MGP,
                                    MetaFee, aec_tx_pool:maximum_auth_fun_gas() + 1),

    %% Test with exactly the lowest possible fee... Note that there isn't any size gas!
    %% This only works pre-IRIS
    case aect_test_utils:latest_protocol_version() of
        Vsn when Vsn =< ?LIMA_PROTOCOL_VSN ->
            #{tx_hash := _MetaTx} = post_ga_spend_tx(APub, APriv, ["1"], BPub, 10001, MGP * 15000, MetaFee);
        _ ->
            ok
    end,

    ok.

mempool_rejects_normal_tx(Config) ->
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    MGP     = aec_test_utils:min_gas_price(),
    SpendTx = spend_tx(APub, APriv, 2, BPub, 10000, 20000 * MGP),
    SignTx  = aec_test_utils:sign_tx(SpendTx, [APriv]),
    ?assertEqual(not_accepted, post_aetx(SignTx)),

    ok.

get_account_by_pubkey(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    Addr = aeser_api_encoder:encode(account_pubkey, APub),
    {ok, 200, AccountA1} = account_by_pubkey(Addr),
    ?assertEqual(<<"generalized">>, maps:get(<<"kind">>, AccountA1)),

    ContractPubkey = maps:get(<<"contract_id">>, AccountA1),
    {ok, 200, #{<<"owner_id">> := Owner}} = get_contract(ContractPubkey),
    ?assertEqual(Addr, Owner),
    ok.


get_account_by_pubkey_and_height(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    Addr = aeser_api_encoder:encode(account_pubkey, APub),
    Header = aecore_suite_utils:rpc(?NODE, aec_chain, top_header, []),
    Height = aec_headers:height(Header),
    {ok, Hash} = aec_headers:hash_header(Header),
    PrevHash = aec_headers:prev_key_hash(Header),
    EncodedHash = aeser_api_encoder:encode(key_block_hash, Hash),
    EncodedPrevHash = aeser_api_encoder:encode(key_block_hash, PrevHash),
    {ok, 200, Account1} = get_account_by_pubkey_and_height(Addr, Height - 1),
    {ok, 200, Account2} = get_account_by_pubkey_and_height(Addr, Height),
    {ok, 200, Account1} = get_account_by_pubkey_and_hash(Addr, EncodedPrevHash),
    {ok, 200, Account2} = get_account_by_pubkey_and_hash(Addr, EncodedHash),
    ?assertEqual(Addr, maps:get(<<"id">>, Account1)),
    ?assertEqual(Addr, maps:get(<<"id">>, Account2)),
    ?assert(maps:get(<<"balance">>, Account1) > 0),
    ?assertEqual(<<"basic">>, maps:get(<<"kind">>, Account1)),
    ?assertEqual(<<"generalized">>, maps:get(<<"kind">>, Account2)),
    ok.

%% Internal access functions.

get_balance(Pubkey) ->
    Addr = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := Balance}} = account_by_pubkey(Addr),
    Balance.

%% Attach
post_attach_tx(AccPK, AccSK) ->
    AttachTxMap = make_attach_tx_map(AccPK),
    AttachTx    = aega_test_utils:ga_attach_tx(AccPK, AttachTxMap),
    sign_and_post_aetx(AccSK, AttachTx).

make_attach_tx_map(AccPK) ->
    AccId = aeser_api_encoder:encode(account_pubkey, AccPK),
    {ok, 200, #{<<"nonce">> := Nonce0}} = account_by_pubkey(AccId),
    Nonce = Nonce0 + 1,

    {ok, #{bytecode := Code, src := Src, map := #{type_info := TI}}} =
        aega_test_utils:get_contract("basic_auth"),

    CallData = aega_test_utils:make_calldata(Src, "init", []),

    {ok, AuthFun} = aega_test_utils:auth_fun_hash(<<"authorize">>, TI),

    #{ nonce => Nonce, code => Code, auth_fun => AuthFun, call_data => CallData }.

%% GA spend
spend_tx (AccPK, _AccSK, Nonce, Recipient, Amount, Fee) ->
    SpendTxMap = #{ sender_id => aeser_id:create(account, AccPK)
                  , recipient_id => aeser_id:create(account, Recipient)
                  , amount => Amount
                  , fee => Fee
                  , nonce => Nonce },
    SpendTx    = aega_test_utils:spend_tx(SpendTxMap),
    SpendTx.

post_ga_spend_tx(AccPK, AccSK, Nonces, Recipient, Amount, Fee, MetaFee) ->
    post_ga_spend_tx(AccPK, AccSK, Nonces, Recipient, Amount, Fee, MetaFee, 20000).

post_ga_spend_tx(AccPK, AccSK, Nonces, Recipient, Amount, Fee, MetaFee, AuthGas) ->
    InnerTx = spend_tx(AccPK, AccSK, 0, Recipient, Amount, Fee),
    SMetaTx = ga_meta_tx(lists:reverse(Nonces), AccPK, AccSK, InnerTx, MetaFee, AuthGas),
    post_aetx(SMetaTx).

ga_meta_tx([], _AccPK, _AccSK, InnerTx, _MetaFee, _AuthGas) ->
    aetx_sign:new(InnerTx, []);
ga_meta_tx([Nonce|Nonces], AccPK, AccSK, InnerTx, MetaFee, AuthGas) ->
    TxHash    = aec_hash:hash(tx, aec_governance:add_network_id(aetx:serialize_to_binary(InnerTx))),
    Signature = aega_test_utils:basic_auth_sign(list_to_integer(Nonce), TxHash, AccSK),
    AuthData  = aega_test_utils:make_calldata("basic_auth", "authorize",
                    [Nonce, aega_test_utils:to_hex_lit(64, Signature)]),
    MetaTx    = aega_test_utils:ga_meta_tx(AccPK,
                    #{ gas => AuthGas, auth_data => AuthData,
                       tx => aetx_sign:new(InnerTx, []), fee => MetaFee }),
    ga_meta_tx(Nonces, AccPK, AccSK, MetaTx, MetaFee, AuthGas).

sc_create_tx(APK, BPK) ->
    Delegates =
        case aect_test_utils:latest_protocol_version() of
            PostIris when PostIris >= ?IRIS_PROTOCOL_VSN -> {[], []};
            _ -> []
        end,
    SCMap = #{ initiator_id => aeser_id:create(account, APK),
               responder_id => aeser_id:create(account, BPK),
               nonce => 0,
               initiator_amount => 20000,
               responder_amount => 20000,
               fee => 20000 * aec_test_utils:min_gas_price(),
               channel_reserve => 100,
               state_hash => <<0:32/unit:8>>,
               lock_period => 42,
               delegate_ids => Delegates},
    {ok, Tx} = aesc_create_tx:new(SCMap),
    Tx.

post_ga_sc_create_tx(APK, ASK, NonceA, BPK, BSK, NonceB) ->
    InnerTx = sc_create_tx(APK, BPK),
    SMetaTx1 = ga_meta_tx(APK, ASK, NonceA, InnerTx, []),
    SMetaTx2 = ga_meta_tx(BPK, BSK, NonceB, SMetaTx1, []),
    post_aetx(aetx_sign:new(SMetaTx2, [])).

%% Incorrectly use signature for "responder"
post_ga_sc_create_fail_tx(APK, ASK, NonceA, BPK, BSK) ->
    InnerTx = sc_create_tx(APK, BPK),
    SigTx   = aec_test_utils:sign_tx(InnerTx, BSK),
    [Sig]   = aetx_sign:signatures(SigTx),
    SMetaTx1 = ga_meta_tx(APK, ASK, NonceA, InnerTx, [Sig]),
    post_aetx(aetx_sign:new(SMetaTx1, [])).

ga_meta_tx(PK, SK, Nonce, InnerTx, Sigs) ->
    TxHash = aec_hash:hash(tx, aec_governance:add_network_id(aetx:serialize_to_binary(InnerTx))),
    Sig    = aega_test_utils:basic_auth_sign(list_to_integer(Nonce), TxHash, SK),
    Auth   = aega_test_utils:make_calldata("basic_auth", "authorize",
                    [Nonce, aega_test_utils:to_hex_lit(64, Sig)]),
    aega_test_utils:ga_meta_tx(PK, #{ gas => 10000, auth_data => Auth,
                                      tx => aetx_sign:new(InnerTx, Sigs),
                                      fee => 100000 * aec_test_utils:min_gas_price() }).


sign_and_post_aetx(PrivKey, Tx) ->
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    post_aetx(SignedTx).

post_aetx(SignedTx) ->
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    SendTx       = aeser_api_encoder:encode(transaction, SerializedTx),
    case post_tx(SendTx) of
        {ok, 200, #{<<"tx_hash">> := TxHash}} ->
            #{tx_hash => TxHash, sign_tx => SignedTx};
        {ok, 400, _} ->
            not_accepted
    end.


%% ============================================================
%% HTTP Requests
%% Note that some are internal and some are external!
%% ============================================================

get_contract(PubKey) ->
    Host = aecore_suite_utils:external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(PubKey), []).

get_contract_call_object(TxHash) ->
    Host = aecore_suite_utils:external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

get_tx(TxHash) ->
    Host = aecore_suite_utils:external_address(),
    http_request(Host, get, "transactions/" ++ binary_to_list(TxHash), []).

create_spend_tx(RecipientId, Amount, Fee) ->
    Sender = maps:get(pubkey, aecore_suite_utils:patron()),
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
    create_spend_tx(SenderId, RecipientId, Amount, Fee, <<"post spend tx">>).

create_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = aecore_suite_utils:internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

account_by_pubkey(Id) ->
    Host = aecore_suite_utils:external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

get_account_by_pubkey_and_height(Id, Height) ->
    Host = aecore_suite_utils:external_address(),
    IdS = binary_to_list(http_uri:encode(Id)),
    HeightS = integer_to_list(Height),
    http_request(Host, get, "accounts/" ++ IdS ++ "/height/" ++ HeightS, []).

get_account_by_pubkey_and_hash(Id, Hash) ->
    Host = aecore_suite_utils:external_address(),
    IdS = binary_to_list(http_uri:encode(Id)),
    http_request(Host, get, "accounts/" ++ IdS ++ "/hash/" ++ Hash, []).

post_tx(TxSerialized) ->
    Host = aecore_suite_utils:external_address(),
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
    {Pubkey, Privkey} = aecore_suite_utils:generate_key_pair(),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    {ok, 200, #{<<"tx">> := SpendTx}} =
        create_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Balance, Fee),
    SignedSpendTx = sign_tx(SpendTx),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),
    {Pubkey, Privkey, SpendTxHash}.

do_dry_run(STx, ExpRes) ->
    Tx = try aetx_sign:tx(STx)
         catch _:_ -> STx end,
    EncTx = aeser_api_encoder:encode(transaction, aetx:serialize_to_binary(Tx)),
    Host = aecore_suite_utils:internal_address(),
    case http_request(Host, post, "debug/transactions/dry-run", #{ txs => [#{tx => EncTx}] }) of
        {ok, 200, #{ <<"results">> := [#{ <<"result">> := Res } = ResObj] }} ->
            ct:pal("ResObj: ~p", [ResObj]),
            ?assertMatch(ExpRes, binary_to_atom(Res, utf8));
        {ok, 403, #{<<"reason">> := Reason}} ->
            ct:pal("Dry-run call failed with reason: ~s", [Reason]),
            ?assertMatch(ExpRes, error)
    end.
