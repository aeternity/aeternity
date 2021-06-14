-module(aehttp_dryrun_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/include/aecontract.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-export([ spend_txs/1
        , identity_contract/1
        , authenticate_contract/1
        , authenticate_contract_tx/1
        , accounts/1
        , a_lot_of_gas_limit_passes/1
        , a_lot_of_gas_limit_fails/1
        ]).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0, rpc/4]).
-import(aehttp_contracts_SUITE, [new_account/1]).

-define(NODE, dev1).

-define(MAX_MINED_BLOCKS, 20).
-define(API_GAS_LIMIT, 6000000).

-define(assertMatchVM(AEVM, FATE, Res),
    case ?IS_AEVM_SOPHIA(aect_test_utils:vm_version()) of
        true  -> ?assertMatch(AEVM, Res);
        false -> ?assertMatch(FATE, Res)
    end).

all() ->
    [
      {group, all}
    ].

groups() ->
    [ {all, [sequence], [{group, swagger2}, {group, oas3}]}
    , {swagger2, [sequence], [{group, aevm}, {group, fate}]}
    , {oas3, [sequence], [{group, aevm}, {group, fate}]}
    , {aevm, [], [{group, internal}, {group, external}]}
    , {fate, [], [{group, internal}, {group, external}]}
    , {internal, [], [{group, dry_run}, a_lot_of_gas_limit_passes]}
    , {external, [], [{group, dry_run}, a_lot_of_gas_limit_fails]}
    , {dry_run, [],
        [ spend_txs
        , identity_contract
        , authenticate_contract
        , accounts
        , authenticate_contract_tx
        ]}
    ].

suite() -> [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> => #{<<"persist">> => false,
                                <<"hard_forks">> => Forks},
               <<"http">> => #{<<"endpoints">> => #{<<"dry-run">> => true},
                               <<"external">> => #{<<"gas_limit">> => ?API_GAS_LIMIT}}},
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [ {instant_mining, true}
                                                                 , {symlink_name, "latest.http_dryrun"}
                                                                 , {test_module, ?MODULE}] ++ Config),
    aecore_suite_utils:start_node(?NODE, Config1),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    ok.

init_per_group(SwaggerVsn, Config) when SwaggerVsn =:= swagger2;
                                        SwaggerVsn =:= oas3 ->
    [{swagger_version, SwaggerVsn} | Config];
init_per_group(VM, Config) when VM == aevm; VM == fate ->
    aect_test_utils:init_per_group(VM, Config);
init_per_group(Interface, Config) when Interface =:= internal;
                                       Interface =:= external ->
    [{interface, Interface} | Config];
init_per_group(all, Config) ->
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    NodeName = aecore_suite_utils:node_name(?NODE),

    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ aecore_suite_utils:mine_key_blocks(NodeName, ToMine) || ToMine > 0 ],

    %% Prepare accounts, Alice, Bert, Carl and Diana.

    StartAmt = 25000000 * aec_test_utils:min_gas_price(),
    {APubkey, APrivkey, STx1} = new_account(StartAmt),
    {BPubkey, BPrivkey, STx2} = new_account(StartAmt),
    {CPubkey, CPrivkey, STx3} = new_account(StartAmt),
    {DPubkey, DPrivkey, STx4} = new_account(StartAmt),

    {ok, KBs} = aecore_suite_utils:mine_blocks_until_txs_on_chain(
                                    NodeName, [STx1, STx2, STx3, STx4], ?MAX_MINED_BLOCKS),

    Top = lists:last(KBs),

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
    {ok, TopHash} = aec_blocks:hash_internal_representation(Top),
    [{top_hash, TopHash}, {accounts, Accounts}, {node_name, NodeName} | Config];
init_per_group(_, Config) -> Config.

end_per_group(_VM, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    SwaggerVsn = proplists:get_value(swagger_version, Config),
    aecore_suite_utils:use_swagger(SwaggerVsn),
    put('$vm_version',     ?config(vm_version,     Config)),
    put('$abi_version',    ?config(abi_version,    Config)),
    put('$sophia_version', ?config(sophia_version, Config)),
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    aect_test_utils:setup_testcase(Config),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

spend_txs(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    #{ public := EPub } = enacl:sign_keypair(),

    Tx1 = {tx, create_spend_tx(APub, EPub, 100000 * aec_test_utils:min_gas_price(), 20000 * aec_test_utils:min_gas_price(), 1, 100)},
    Tx2 = {tx, create_spend_tx(EPub, APub, 100, 20000 * aec_test_utils:min_gas_price(), 1, 100)},

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"spend">> },
                                   #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [Tx1, Tx2]),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [Tx2, Tx1]),

    ok.

identity_contract(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    {ok, Code}   = aect_test_utils:compile_contract(identity),

    InitCallData = make_call_data(identity, <<"init">>, []),
    CallCallData = make_call_data(identity, <<"main_">>, [<<"42">>]),

    CreateTx  = {tx, create_contract_tx(APub, 1, Code, InitCallData)},
    CPub      = contract_id(element(2, CreateTx)),
    CallTx    = {tx, call_contract_tx(APub, CPub, 2, CallCallData)},
    BadCallTx = {tx, call_contract_tx(APub, CPub, 1, CallCallData)},

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_create">>,
                                      <<"call_obj">> := #{ <<"gas_used">> := _ } },
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := #{ <<"gas_used">> := _ } }
                                  ] }} =
        dry_run(Config, TopHash, [CreateTx, CallTx]),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [BadCallTx, CreateTx]),

    ok.

authenticate_contract_tx(Config) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN -> {skip, generalized_accounts_in_dry_run_not_in_fortuna};
        ?LIMA_PROTOCOL_VSN    -> {skip, generalized_accounts_auth_tx_in_dry_run_not_in_lima};
        _ -> case aect_test_utils:backend() of
                 aevm -> {skip, generalized_accounts_auth_tx_not_in_aevm};
                 fate -> authenticate_contract_tx_(Config)
             end
    end.

authenticate_contract_tx_(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    {ok, Code}   = aect_test_utils:compile_contract(tx_auth),

    InitCallData = make_call_data(tx_auth, "init", []),
    CallCallData1 = make_call_data(tx_auth, "test_no_auth_tx", []),
    CallCallData2 = make_call_data(tx_auth, "test_auth_tx_spend", [aeser_api_encoder:encode(account_pubkey, APub), "100", "\"hello\""]),
    {ok, SpendTx} = aec_spend_tx:new(#{ sender_id => aeser_id:create(account, APub),
                                        recipient_id => aeser_id:create(account, APub),
                                        amount => 100, fee => 5, nonce => 1, payload => <<"hello">> }),

    CreateTx  = {tx, create_contract_tx(APub, 1, Code, InitCallData)},
    CPub      = contract_id(element(2, CreateTx)),
    CallTx    = {tx, call_contract_tx(APub, CPub, 2, CallCallData1)},

    DecodeRes =
        fun(SerRVal, Type) ->
            {ok, RValBin} = aeser_api_encoder:safe_decode(contract_bytearray, SerRVal),
            aefa_test_utils:decode(aeb_fate_encoding:deserialize(RValBin), Type)
        end,

    {ok, 200, #{ <<"results">> := [_CreateRes,
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj }
                                  ] }} =
        dry_run(Config, TopHash, [CreateTx, CallTx]),

    ?assertEqual(true, DecodeRes(maps:get(<<"return_value">>, CallObj), bool)),

    CallReq  = #{<<"contract">> => aeser_api_encoder:encode(contract_pubkey, CPub),
                 <<"calldata">> => aeser_api_encoder:encode(contract_bytearray, CallCallData2),
                 <<"abi_version">> => aect_test_utils:abi_version()},
    EncSpend = aeser_api_encoder:encode(transaction, aetx:serialize_to_binary(SpendTx)),
    CallReq1 = {call_req, CallReq#{<<"context">> => #{tx => EncSpend,
                                                      stateful => true},
                                   <<"gas">> => 100000}},
    CallReq2 = {call_req, CallReq#{<<"context">> => #{tx => EncSpend,
                                                      stateful => false},
                                   <<"gas">> => 100000,
                                   <<"nonce">> => 2}},
    {ok, 200, #{ <<"results">> := [_CreateRes,
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj2 },
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj3 },
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj3 }
                                  ] }} =
        dry_run(Config, TopHash, [CreateTx, CallReq1, CallReq2, CallReq2]),

    ?assertEqual(true, DecodeRes(maps:get(<<"return_value">>, CallObj2), bool)),
    ?assertEqual(true, DecodeRes(maps:get(<<"return_value">>, CallObj3), bool)),

    ok.

authenticate_contract(Config) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN -> {skip, generalized_accounts_in_dry_run_not_in_fortuna};
        _ -> authenticate_contract_(Config)
    end.

authenticate_contract_(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    {ok, Code}   = aect_test_utils:compile_contract(basic_auth),

    InitCallData = make_call_data(basic_auth, "init", []),
    CallCallData = make_call_data(basic_auth, "get_auth_tx_hash", []),

    CreateTx  = {tx, create_contract_tx(APub, 1, Code, InitCallData)},
    CPub      = contract_id(element(2, CreateTx)),
    CallTx    = {tx, call_contract_tx(APub, CPub, 2, CallCallData)},

    DecodeOption =
        fun(SerRVal) ->
            {ok, RValBin} = aeser_api_encoder:safe_decode(contract_bytearray, SerRVal),
            {ok, RVal} =
                case aect_test_utils:abi_version() of
                    ?ABI_AEVM_SOPHIA_1 ->
                        aeb_heap:from_binary({option, word}, RValBin);
                    ?ABI_FATE_SOPHIA_1 ->
                        {ok, aefa_test_utils:decode(aeb_fate_encoding:deserialize(RValBin), {option, int})}
                end,
            RVal
        end,

    {ok, 200, #{ <<"results">> := [_CreateRes,
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj }
                                  ] }} =
        dry_run(Config, TopHash, [CreateTx, CallTx]),

    ?assertEqual(none, DecodeOption(maps:get(<<"return_value">>, CallObj))),

    CallReq  = #{<<"contract">> => aeser_api_encoder:encode(contract_pubkey, CPub),
                 <<"calldata">> => aeser_api_encoder:encode(contract_bytearray, CallCallData),
                 <<"abi_version">> => aect_test_utils:abi_version()},
    CallReq1 = {call_req, CallReq#{<<"context">> => #{tx_hash => aeser_api_encoder:encode(tx_hash, <<12345:32/unit:8>>),
                                                      stateful => true},
                                   <<"gas">> => 100000}},
    CallReq2 = {call_req, CallReq#{<<"context">> => #{tx_hash => aeser_api_encoder:encode(tx_hash, <<12345:32/unit:8>>),
                                                      stateful => false},
                                   <<"gas">> => 100000,
                                   <<"nonce">> => 2}},
    {ok, 200, #{ <<"results">> := [_CreateRes,
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj2 },
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj3 },
                                   #{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_call">>,
                                      <<"call_obj">> := CallObj3 }
                                  ] }} =
        dry_run(Config, TopHash, [CreateTx, CallReq1, CallReq2, CallReq2]),

    ?assertMatchVM({some, 12345}, {some, {bytes, <<12345:256>>}}, DecodeOption(maps:get(<<"return_value">>, CallObj2))),
    ?assertMatchVM({some, 12345}, {some, {bytes, <<12345:256>>}}, DecodeOption(maps:get(<<"return_value">>, CallObj3))),

    ok.

accounts(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),
    GenHash = get_genesis_hash(),

    #{ public := EPub } = enacl:sign_keypair(),

    Tx1 = {tx, create_spend_tx(APub, EPub, 100000 * aec_test_utils:min_gas_price(), 20000 * aec_test_utils:min_gas_price(), 1, 100)},
    Tx2 = {tx, create_spend_tx(EPub, APub, 100, 20000 * aec_test_utils:min_gas_price(), 1, 100)},

    %% Should work on TopHash
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [Tx1, Tx2]),

    %% Should not work on GenHash
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"error">> }] }} =
        dry_run(Config, GenHash, [Tx1, Tx2]),

    %% Should work on GenHash with APub
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [Tx1, Tx2], [#{ pub_key => APub, amount => 100000000}]),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [Tx2, Tx1]),

    %% Should work on GenHash if we create and top up EPub
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(Config, TopHash, [Tx2, Tx1], [#{ pub_key => EPub, amount => 1000000000000000}]),

    ok.

%% --- Internal functions ---

make_call_data(Contract, FunName, Args) ->
    {ok, Code}     = aect_test_utils:read_contract(Contract),
    {ok, CallData} = aect_test_utils:encode_call_data(Code, FunName, Args),
    CallData.

contract_id(Tx) ->
    {_, CTx} = aetx:specialize_callback(Tx),
    aect_create_tx:contract_pubkey(CTx).

dry_run(Config, TopHash, Txs) ->
    dry_run(Config, TopHash, Txs, []).

dry_run(Config, TopHash, Txs, Accounts) ->
    EncTx = fun(Tx) -> try aeser_api_encoder:encode(transaction, aetx:serialize_to_binary(Tx))
                       catch _:_ -> Tx end end,
    {Host, URI} =
        case ?config(interface, Config) of
            internal ->
                {internal_address(), "debug/transactions/dry-run"};
            external ->
                {external_address(), "dry-run"}
        end,
    http_request(Host, post, URI,
                 #{ top => aeser_api_encoder:encode(key_block_hash, TopHash),
                    accounts => [ A#{pub_key => aeser_api_encoder:encode(account_pubkey, PK)}
                                  || A = #{pub_key := PK } <- Accounts ],
                    txs => [#{Type => EncTx(Tx)} || {Type, Tx} <- Txs] }).

get_genesis_hash() ->
    {ok, 200, #{<<"genesis_key_block_hash">> := EncGenesisHash}} = get_status(),
    {ok, GenesisHash} = aeser_api_encoder:safe_decode(key_block_hash, EncGenesisHash),
    GenesisHash.

get_status() ->
    http_request(external_address(), get, "status", #{}).

create_spend_tx(Sender, Recipient, Amount, Fee, Nonce, TTL) ->
    SenderId = aeser_id:create(account, Sender),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => <<>>,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    Tx.

create_contract_tx(Owner, Nonce, Code, CallData) ->
    create_contract_tx(Owner, Nonce, Code, CallData, 100000).

create_contract_tx(Owner, Nonce, Code, CallData, Gas) ->
    OwnerId = aeser_id:create(account, Owner),
    Params = #{ owner_id => OwnerId,
                code => Code,
                call_data => CallData,
                vm_version => aect_test_utils:vm_version(),
                abi_version => aect_test_utils:abi_version(),
                deposit => 0,
                amount => 0,      %Initial balance
                gas => Gas,     %May need a lot of gas
                gas_price => aec_test_utils:min_gas_price(),
                fee => 1400000 * aec_test_utils:min_gas_price(),
                nonce => Nonce },
    {ok, Tx} = aect_create_tx:new(Params),
    Tx.

call_contract_tx(Caller, Contract, Nonce, CallData) ->
    CallerId = aeser_id:create(account, Caller),
    ContractId = aeser_id:create(contract, Contract),
    Params = #{ caller_id => CallerId,
                contract_id => ContractId,
                call_data => CallData,
                abi_version => aect_test_utils:abi_version(),
                amount => 0,
                gas => 100000,     %May need a lot of gas
                gas_price => aec_test_utils:min_gas_price(),
                fee => 800000 * aec_test_utils:min_gas_price(),
                nonce => Nonce },
    {ok, Tx} = aect_call_tx:new(Params),
    Tx.

a_lot_of_gas_limit_passes(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    {ok, Code}   = aect_test_utils:compile_contract(identity),

    InitCallData = make_call_data(identity, <<"init">>, []),

    CreateTx  = {tx, create_contract_tx(APub, 1, Code, InitCallData, ?API_GAS_LIMIT + 1)},

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">>,
                                      <<"type">> := <<"contract_create">>,
                                      <<"call_obj">> := #{ <<"gas_used">> := _ } }
                                  ] }} =
        dry_run(Config, TopHash, [CreateTx]),

    ok.

a_lot_of_gas_limit_fails(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    {ok, Code}   = aect_test_utils:compile_contract(identity),

    InitCallData = make_call_data(identity, <<"init">>, []),

    CreateTx  = {tx, create_contract_tx(APub, 1, Code, InitCallData, ?API_GAS_LIMIT + 1)},

    {ok, 403, #{<<"reason">> := <<"Over the gas limit">>}} =
        dry_run(Config, TopHash, [CreateTx]),
    HalfLimit = ?API_GAS_LIMIT div 2,

    CreateTx1 = {tx, create_contract_tx(APub, 1, Code, InitCallData, HalfLimit + 1)},
    CreateTx2 = {tx, create_contract_tx(APub, 1, Code, InitCallData, HalfLimit + 1)},
    {ok, 403, #{<<"reason">> := <<"Over the gas limit">>}} =
        dry_run(Config, TopHash, [CreateTx1, CreateTx2]),

    CreateGas = 100000,
    CreateTx3 = {tx, create_contract_tx(APub, 1, Code, InitCallData, CreateGas)},
    CallCallData = make_call_data(identity, <<"main_">>, [<<"42">>]),
    CPub      = contract_id(element(2, CreateTx3)),

    CallReqData = #{<<"contract">> => aeser_api_encoder:encode(contract_pubkey, CPub),
                    <<"calldata">> => aeser_api_encoder:encode(contract_bytearray, CallCallData),
                    <<"abi_version">> => aect_test_utils:abi_version(),
                    <<"gas">> => ?API_GAS_LIMIT - CreateGas + 1},
    CallReq = {call_req, CallReqData},
    {ok, 403, #{<<"reason">> := <<"Over the gas limit">>}} =
        dry_run(Config, TopHash, [CreateTx3, CallReq]),
    ok.
