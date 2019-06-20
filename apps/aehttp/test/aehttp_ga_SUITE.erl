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

%% test case exports
%% external endpoints
-export([ attach/1
        , attach_fail/1
        , get_account_by_pubkey/1
        , get_account_by_pubkey_and_height/1
        , meta_fail/1
        , meta_fail_auth/1
        , meta_spend/1
        , meta_meta_fail_auth/1
        , meta_meta_fail/1
        , meta_meta_spend/1
        , attach_second/1
        , meta_4_fail/1
        , mempool/1
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
    [{group, aevm},
     {group, fate}
    ].

groups() ->
    [{aevm, [sequence], [{group, ga_txs}, {group, ga_info}, {group, ga_mempool}]},
     {fate, [sequence], [{group, ga_txs}, {group, ga_info}, {group, ga_mempool}]},

     {ga_txs, [sequence],
      [ attach_fail
      , attach
      , meta_fail_auth
      , meta_fail
      , meta_spend
      , meta_meta_fail_auth
      , meta_meta_fail
      , meta_meta_spend
      , meta_4_fail
      ]},

     {ga_info, [sequence],
      [ attach
      , get_account_by_pubkey
      , get_account_by_pubkey_and_height
      ]},

     {ga_mempool, [sequence],
      [ attach
      , mempool ]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks}},
    Config1 = [{symlink_name, "latest.http_ga"}, {test_module, ?MODULE}] ++ Config0,
    Config2 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, Config1),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}] ++ Config2.

end_per_suite(_Config) ->
    ok.

init_per_group(VMGroup, Config) when VMGroup == aevm; VMGroup == fate ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN when VMGroup == aevm ->
            [{sophia_version, ?SOPHIA_FORTUNA}, {vm_version, ?VM_AEVM_SOPHIA_3},
             {abi_version, ?ABI_AEVM_SOPHIA_1} | Config];
        ?LIMA_PROTOCOL_VSN when VMGroup == aevm ->
            [{sophia_version, ?SOPHIA_LIMA_AEVM}, {vm_version, ?VM_AEVM_SOPHIA_3},
             {abi_version, ?ABI_AEVM_SOPHIA_1} | Config];
        ?FORTUNA_PROTOCOL_VSN when VMGroup == fate ->
            {skip, generalized_accounts_with_fate_not_in_fortuna};
        ?LIMA_PROTOCOL_VSN when VMGroup == fate ->
            [{sophia_version, ?SOPHIA_LIMA_FATE}, {vm_version, ?VM_FATE_SOPHIA_1},
             {abi_version, ?ABI_FATE_SOPHIA_1} | Config]
    end;
init_per_group(_GAGroup, Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),

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
    [{accounts, Accounts}, {node_name, NodeName} | Config].

end_per_group(VMGroup, _Config) when VMGroup == aevm; VMGroup == fate ->
    ok;
end_per_group(_GAGroup, Config) ->
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    ok.

init_per_testcase(_Case, Config) ->
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

    #{tx_hash := AttachTx} = post_attach_tx(Pub, Priv),

    ?MINE_TXS([AttachTx]),

    {ok, 200, Account1} = account_by_pubkey(Addr),
    Bal1 = maps:get(<<"balance">>, Account1),

    %% test that we can return GAAttachTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAAttachTx">>}}} = get_tx(AttachTx),

    ct:pal("Cost: ~p", [Bal0 - Bal1]),
    MGP = aec_test_utils:min_gas_price(),
    AEVMBal = Bal0 - 1000000 * MGP - 411 * MGP,
    FATEBal = Bal0 - 1000000 * MGP - 10 * MGP,
    ?assertMatchABI(AEVMBal, FATEBal, Bal1),
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
    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["1"], BPub, 1000 * MGP * MGP, MGP * 15000, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}}} = get_tx(MetaTx),

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"error">>}}} = get_contract_call_object(MetaTx),

    ct:pal("Cost failing inner: ~p", [ABal0 - ABal1]),
    AEVMBal = ABal0 - (MetaFee + 4711 * 1000 * MGP),
    FATEBal = ABal0 - (MetaFee + 1421 * 1000 * MGP),
    ?assertMatchABI(AEVMBal, FATEBal, ABal1),
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

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"ok">>}}} =
        get_contract_call_object(MetaTx),

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    AEVMBal = ABal0 - (MetaFee + 4711 * 1000 * MGP + 20000 * MGP + 10000),
    FATEBal = ABal0 - (MetaFee + 1421 * 1000 * MGP + 20000 * MGP + 10000),
    ?assertMatchABI(AEVMBal, FATEBal, ABal1),
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

    {ok, 200, #{<<"ga_info">> := #{<<"return_type">> := <<"error">>}}} =
        get_contract_call_object(MetaTx),

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    AEVMBal = ABal0 - (MetaFee + 4711 * 1000 * MGP),
    FATEBal = ABal0 - (MetaFee + 1421 * 1000 * MGP),
    ?assertMatchABI(AEVMBal, FATEBal, ABal1),
    ok.

meta_meta_fail(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["4", "5"], BPub, 1000 * MGP * MGP, 15000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}} = JSONTx} = get_tx(MetaTx),
    ct:log("Transaction: ~p", [JSONTx]),

    {ok, 200, #{<<"ga_info">> := GAInfo}} =
        get_contract_call_object(MetaTx),

    ct:log("Transaction info: ~p", [GAInfo]),
    #{<<"return_type">> := <<"ok">>, <<"inner_object">> := InnerObj} = GAInfo,
    #{<<"ga_info">> := #{<<"return_type">> := <<"error">>,
                         <<"inner_object">> := #{<<"tx_info">> := <<"spend_tx">>}}} = InnerObj,

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    AEVMBal = ABal0 - 2 * (MetaFee + 4711 * 1000 * MGP),
    FATEBal = ABal0 - 2 * (MetaFee + 1421 * 1000 * MGP),
    ?assertMatchABI(AEVMBal, FATEBal, ABal1),
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

    {ok, 200, #{<<"ga_info">> := GAInfo}} =
        get_contract_call_object(MetaTx),

    ct:log("Transaction info: ~p", [GAInfo]),
    #{<<"return_type">> := <<"ok">>} = GAInfo,

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    AEVMBal = ABal0 - (2 * (MetaFee + 4711 * 1000 * MGP) + 20000 * MGP + 10000),
    FATEBal = ABal0 - (2 * (MetaFee + 1421 * 1000 * MGP) + 20000 * MGP + 10000),
    ?assertMatchABI(AEVMBal, FATEBal, ABal1),
    ok.

meta_4_fail(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),
    MetaFee = (5 * 15000 + 30000) * MGP,

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, ["8", "9", "10", "12"], APub, 10000, 15000 * MGP, MetaFee),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    %% test that we can return GAMetaTx via http interface
    {ok, 200, #{<<"tx">> := #{<<"type">> := <<"GAMetaTx">>}} = JSONTx} = get_tx(MetaTx),
    ct:log("Transaction: ~p", [JSONTx]),

    {ok, 200, #{<<"ga_info">> := GAInfo}} =
        get_contract_call_object(MetaTx),

    ct:log("Transaction info: ~p", [GAInfo]),
    #{<<"return_type">> := <<"ok">>, <<"inner_object">> := InnerObj} = GAInfo,

    #{<<"ga_info">> := #{<<"inner_object">> :=
        #{<<"ga_info">> := #{<<"return_type">> := <<"error">>}}}} = InnerObj,

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    AEVMBal = ABal0 - 3 * (MetaFee + 4711 * 1000 * MGP),
    FATEBal = ABal0 - 3 * (MetaFee + 1421 * 1000 * MGP),
    ?assertMatchABI(AEVMBal, FATEBal, ABal1),
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
    #{tx_hash := _MetaTx} = post_ga_spend_tx(APub, APriv, ["1"], BPub, 10001, MGP * 15000, MetaFee),

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
    Header = rpc(?NODE, aec_chain, top_header, []),
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
    SMetaTx = ga_spend_tx(lists:reverse(Nonces), AccPK, AccSK, InnerTx, MetaFee, AuthGas),
    post_aetx(SMetaTx).

ga_spend_tx([], _AccPK, _AccSK, InnerTx, _MetaFee, _AuthGas) ->
    aetx_sign:new(InnerTx, []);
ga_spend_tx([Nonce|Nonces], AccPK, AccSK, InnerTx, MetaFee, AuthGas) ->
    TxHash    = aec_hash:hash(tx, aec_governance:add_network_id(aetx:serialize_to_binary(InnerTx))),
    Signature = aega_test_utils:basic_auth_sign(list_to_integer(Nonce), TxHash, AccSK),
    AuthData  = aega_test_utils:make_calldata("basic_auth", "authorize",
                    [Nonce, aega_test_utils:to_hex_lit(64, Signature)]),
    MetaTx    = aega_test_utils:ga_meta_tx(AccPK,
                    #{ gas => AuthGas, auth_data => AuthData,
                       tx => aetx_sign:new(InnerTx, []), fee => MetaFee }),
    ga_spend_tx(Nonces, AccPK, AccSK, MetaTx, MetaFee, AuthGas).

sign_and_post_aetx(PrivKey, Tx) ->
    SignedTx     = aec_test_utils:sign_tx(Tx, PrivKey),
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
    Host = external_address(),
    http_request(Host, get, "contracts/" ++ binary_to_list(PubKey), []).

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

account_by_pubkey(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

get_account_by_pubkey_and_height(Id, Height) ->
    Host = external_address(),
    IdS = binary_to_list(http_uri:encode(Id)),
    HeightS = integer_to_list(Height),
    http_request(Host, get, "accounts/" ++ IdS ++ "/height/" ++ HeightS, []).

get_account_by_pubkey_and_hash(Id, Hash) ->
    Host = external_address(),
    IdS = binary_to_list(http_uri:encode(Id)),
    http_request(Host, get, "accounts/" ++ IdS ++ "/hash/" ++ Hash, []).

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
encode_get_params([{K, V}|T]) ->
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
    {Pubkey, Privkey} = generate_key_pair(),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    {ok, 200, #{<<"tx">> := SpendTx}} =
        create_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Balance, Fee),
    SignedSpendTx = sign_tx(SpendTx),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),
    {Pubkey, Privkey, SpendTxHash}.

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.

abi_version() ->
    case get('$abi_version') of
        undefined -> aect_test_utils:latest_sophia_abi_version();
        X         -> X
    end.

