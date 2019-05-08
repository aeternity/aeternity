%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc CT test suite for Generalized accounts
%%% @end
%%%-------------------------------------------------------------------
-module(aega_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        ]).

-include_lib("aecontract/include/hard_forks.hrl").

%% test case exports
-export([ simple_attach/1
        , simple_double_attach_fail/1
        , simple_spend_to/1
        , simple_spend_from/1
        , simple_failed_auth/1
        , simple_contract_create/1
        , simple_contract_call/1
        , simple_re_attach_fail/1
        , simple_spend_from_fail/1

        , basic_attach/1
        , basic_spend_from/1
        , basic_contract_create/1
        , basic_contract_call/1
        , basic_minimum_fee/1

        , bitcoin_attach/1
        , bitcoin_spend_from/1
        , bitcoin_contract_create/1
        , bitcoin_contract_call/1

        , oracle_register/1
        , oracle_query/1
        , oracle_query_x2/1
        , oracle_respond/1
        , oracle_extend/1

        , multi_wrap_spend/1
        , multi_wrap_sc_create/1
        , multi_wrap_sc_close_mutual/1
        , multi_wrap_sc_solo_snapshot/1

        , channel_create/1
        , channel_deposit/1
        , channel_withdraw/1
        , channel_snapshot_solo/1
        , channel_close_mutual/1
        , channel_close_solo/1
        , channel_close_solo_snapshot/1
        , channel_close_solo_w_update/1
        , channel_slash/1
        , channel_force_progress/1

        , wrap_unrelated_tx/1
        , cripple_auth/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../../aecore/include/blocks.hrl").
-include("../../aecontract/include/aecontract.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).
-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).

-define(CHAIN_RELATIVE_TTL_MEMORY_ENCODING(X), {variant, 0, [X]}).
-define(CHAIN_ABSOLUTE_TTL_MEMORY_ENCODING(X), {variant, 1, [X]}).

-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all}].

groups() ->
    [ {all, [], [ {group, simple}
                , {group, basic}
                , {group, bitcoin}
                , {group, oracle}
                , {group, multi_wrap}
                , {group, channel}
                , {group, negative}
                ]}

    , {simple, [], [ simple_attach
                   , simple_double_attach_fail
                   , simple_spend_to
                   , simple_spend_from
                   , simple_failed_auth
                   , simple_contract_create
                   , simple_contract_call
                   , simple_re_attach_fail
                   , simple_spend_from_fail
                   ]}

    , {basic, [], [ basic_attach
                  , basic_spend_from
                  , basic_contract_create
                  , basic_contract_call
                  , basic_minimum_fee
                  ]}

    , {bitcoin, [], [ bitcoin_attach
                    , bitcoin_spend_from
                    , bitcoin_contract_create
                    , bitcoin_contract_call
                    ]}

    , {oracle, [], [ oracle_register
                   , oracle_query
                   , oracle_query_x2
                   , oracle_respond
                   , oracle_extend
                   ]}

    , {multi_wrap, [], [ multi_wrap_spend
                       , multi_wrap_sc_create
                       , multi_wrap_sc_close_mutual
                       , multi_wrap_sc_solo_snapshot
                       ]}

    , {channel, [], [ channel_create
                    , channel_deposit
                    , channel_withdraw
                    , channel_snapshot_solo
                    , channel_close_mutual
                    , channel_close_solo
                    , channel_close_solo_snapshot
                    , channel_close_solo_w_update
                    , channel_slash
                    , channel_force_progress
                    ]}

    , {negative, [], [ wrap_unrelated_tx
                     , cripple_auth
                     ]}
    ].

init_per_group(all, Cfg) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN ->
            [{sophia_version, ?SOPHIA_FORTUNA_AEVM}, {vm_version, ?VM_AEVM_SOPHIA_3},
             {protocol, fortuna} | Cfg]
    end;
init_per_group(_Grp, Cfg) ->
    Cfg.

end_per_group(_Grp, Cfg) ->
    Cfg.

%% Process dict magic in the right process ;-)
init_per_testcase(_TC, Config) ->
    VmVersion = ?config(vm_version, Config),
    SophiaVersion = ?config(sophia_version, Config),
    ProtocolVersion = case ?config(protocol, Config) of
                          roma    -> ?ROMA_PROTOCOL_VSN;
                          minerva -> ?MINERVA_PROTOCOL_VSN;
                          fortuna -> ?FORTUNA_PROTOCOL_VSN
                      end,
    put('$vm_version', VmVersion),
    put('$sophia_version', SophiaVersion),
    put('$protocol_version', ProtocolVersion),
    Config.

-define(skipRest(Res, Reason),
    case Res of
        true  -> throw({skip, {skip_rest, Reason}});
        false -> ok
    end).

-define(call(Fun, X),                      call(Fun, fun Fun/2, [X])).
-define(call(Fun, X, Y),                   call(Fun, fun Fun/3, [X, Y])).
-define(call(Fun, X, Y, Z),                call(Fun, fun Fun/4, [X, Y, Z])).
-define(call(Fun, X, Y, Z, U),             call(Fun, fun Fun/5, [X, Y, Z, U])).
-define(call(Fun, X, Y, Z, U, V),          call(Fun, fun Fun/6, [X, Y, Z, U, V])).
-define(call(Fun, X, Y, Z, U, V, W),       call(Fun, fun Fun/7, [X, Y, Z, U, V, W])).
-define(call(Fun, X, Y, Z, U, V, W, R),    call(Fun, fun Fun/8, [X, Y, Z, U, V, W, R])).
-define(call(Fun, X, Y, Z, U, V, W, R, S), call(Fun, fun Fun/9, [X, Y, Z, U, V, W, R, S])).

%%%===================================================================
%%% Simple GA tests
%%%===================================================================

simple_attach(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),
    ok.

simple_double_attach_fail(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    {failed, not_a_basic_account} =
        ?call(attach, Acc1, "simple_auth", "authorize", ["0"], #{fail => true}),

    ok.

simple_spend_to(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    PreBalance  = ?call(account_balance, Acc1),
    ok          = ?call(spend, Acc2, Acc1, 500,  20000 * MinGP),
    PostBalance = ?call(account_balance, Acc1),
    ?assertMatch({X, Y} when X + 500 == Y, {PreBalance, PostBalance}),

    ok.

simple_spend_from(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    AuthOpts    = #{ prep_fun => fun(_) -> simple_auth(["123", "1"]) end },
    PreBalance  = ?call(account_balance, Acc2),
    {ok, #{tx_res := ok}} = ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000 * MinGP),
    PostBalance = ?call(account_balance, Acc2),
    ?assertMatch({X, Y} when X + 500 == Y, {PreBalance, PostBalance}),

    ok.

simple_failed_auth(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    AuthOpts = #{ prep_fun => fun(_) -> simple_auth(["1234", "1"]) end },
    {failed, authentication_failed} =
        ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000 * MinGP, #{fail => true}),

    ok.

simple_contract_create(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    AuthOpts = #{ prep_fun => fun(_) -> simple_auth(["123", "1"]) end },
    {ok, #{tx_res := ok, init_res := ok}} = ?call(ga_create, Acc1, AuthOpts, "identity", []),

    ok.

simple_contract_call(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    AuthOpts = #{ prep_fun => fun(_) -> simple_auth(["123", "1"]) end },
    {ok, #{tx_res := ok, init_res := ok, ct_pubkey := Ct}} =
        ?call(ga_create, Acc1, AuthOpts, "identity", []),

    AuthOpts2 = #{ prep_fun => fun(_) -> simple_auth(["123", "2"]) end },
    {ok, #{tx_res := ok, call_res := ok, call_val := Val}} =
        ?call(ga_call, Acc1, AuthOpts2, Ct, "identity", "main", ["42"]),
    ?assertMatch("42", decode_call_result("identity", "main", ok, Val)),

    ok.

simple_re_attach_fail(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    AuthOpts = #{ prep_fun => fun(_) -> simple_auth(["123", "1"]) end },
    {ok, #{tx_res := error, tx_value := <<"not_a_basic_account">>}} =
        ?call(ga_attach, Acc1, AuthOpts, "simple_auth", "authorize", ["123"]),

    ok.

simple_spend_from_fail(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    AuthOpts   = #{ prep_fun => fun(_) -> simple_auth(["123", "1"]) end },
    PreBalance  = ?call(account_balance, Acc2),
    {ok, #{tx_res := error, tx_value := <<"too_low_fee">>}} =
        ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000),
    PostBalance = ?call(account_balance, Acc2),
    ?assertEqual(PreBalance, PostBalance),

    ok.

%%%===================================================================
%%% Basic GA tests
%%%===================================================================
basic_attach(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    ok.

basic_spend_from(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    AuthOpts    = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },
    PreBalance  = ?call(account_balance, Acc2),
    {ok, #{tx_res := ok}} =
        ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000 * MinGP),
    PostBalance = ?call(account_balance, Acc2),
    ?assertMatch({X, Y} when X + 500 == Y, {PreBalance, PostBalance}),

    ok.

basic_contract_create(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    AuthOpts = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },
    {ok, #{init_res := ok}} = ?call(ga_create, Acc1, AuthOpts, "identity", []),

    ok.

basic_contract_call(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    AuthOpts = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },
    {ok, #{init_res := ok, ct_pubkey := Ct}} =
        ?call(ga_create, Acc1, AuthOpts, "identity", []),

    AuthOpts2 = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "2", TxHash) end },
    {ok, #{call_res := ok, call_val := Val}} =
        ?call(ga_call, Acc1, AuthOpts2, Ct, "identity", "main", ["42"]),
    ?assertMatch("42", decode_call_result("identity", "main", ok, Val)),

    ok.

basic_minimum_fee(_Cfg) ->
    state(aect_test_utils:new_state()),
    Height = 1,
    MinGP = aec_governance:minimum_gas_price(Height),
    Acc1 = ?call(new_account, 1000000000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    %% First find out what the minimum fee is for a spend tx.
    From = Acc1,
    To   = <<4711:256>>,
    DummyTx = aega_test_utils:spend_tx(#{sender_id    => aeser_id:create(account, From),
                                         recipient_id => aeser_id:create(account, To),
                                         amount       => 4711,
                                         height       => Height,
                                         nonce        => 0}),
    MinimumSpendFee = aetx:min_fee(DummyTx, Height),

    %% When the transaction is wrapped in a meta transaction, the size of the
    %% inner transaction is paid for by the meta transaction.
    SizeFee = aetx:size(DummyTx) * aec_governance:byte_gas() * MinGP,
    MinimumInnerFee = MinimumSpendFee - SizeFee,

    %% Make sure that we actually are setting a fee that is lower than
    %% what should pass if the tx wasn't an inner tx.
    ?assert(MinimumSpendFee > MinimumInnerFee),

    %% Make sure we are right at the limit of the minimum fee for the inner tx.
    SpendTx1 = aega_test_utils:spend_tx(#{sender_id    => aeser_id:create(account, From),
                                          recipient_id => aeser_id:create(account, To),
                                          amount       => 4711,
                                          height       => Height,
                                          nonce        => 0,
                                          fee          => MinimumInnerFee - 1
                                         }),
    AuthOpts1 = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },
    ?assertMatch({ok, #{tx_res := error,
                        tx_value := <<"too_low_fee">>}},
                 ?call(meta, From, AuthOpts1, SpendTx1, #{})),

    %% But if we use just the right fee, the tx should be accepted.
    SpendTx2 = aega_test_utils:spend_tx(#{sender_id    => aeser_id:create(account, From),
                                          recipient_id => aeser_id:create(account, To),
                                          amount       => 4711,
                                          height       => Height,
                                          nonce        => 0,
                                          fee          => MinimumInnerFee
                                         }),

    AuthOpts2 = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "2", TxHash) end },
    ?assertMatch({ok, #{tx_res := ok}},
                 ?call(meta, From, AuthOpts2, SpendTx2, #{})),

    ok.

%%%===================================================================
%%% Bitcoin GA tests
%%%===================================================================
-define(SECP256K1_PRIV, <<129,67,4,165,74,42,181,117,141,36,184,24,52,32,144,252,
                          23,236,21,76,171,79,8,23,32,235,57,139,176,168,252,102>>).
-define(SECP256K1_PUB,  <<80,178,23,109,30,43,94,53,192,188,114,212,49,16,33,
                          105,167,148,214,246,21,27,109,98,205,82,189,171,145,130,
                          125,240,129,214,86,90,80,15,173,39,24,188,245,26,101,87,
                          79,253,25,20,6,201,7,238,228,119,14,10,72,126,210,21,198,125>>).
-define(B_OWNER, aega_test_utils:to_hex_lit(64, ?SECP256K1_PUB)).

bitcoin_attach(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "bitcoin_auth", "authorize", [?B_OWNER]),
    ok.

bitcoin_spend_from(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "bitcoin_auth", "authorize", [?B_OWNER]),

    AuthOpts    = #{ prep_fun => fun(TxHash) -> ?call(bitcoin_auth, Acc1, "1", TxHash) end },
    PreBalance  = ?call(account_balance, Acc2),
    {ok, #{tx_res := ok}} =
        ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000 * MinGP),
    PostBalance = ?call(account_balance, Acc2),
    ?assertMatch({X, Y} when X + 500 == Y, {PreBalance, PostBalance}),

    ok.

bitcoin_contract_create(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "bitcoin_auth", "authorize", [?B_OWNER]),

    AuthOpts = #{ prep_fun => fun(TxHash) -> ?call(bitcoin_auth, Acc1, "1", TxHash) end },
    {ok, #{init_res := ok}} = ?call(ga_create, Acc1, AuthOpts, "identity", []),

    ok.

bitcoin_contract_call(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "bitcoin_auth", "authorize", [?B_OWNER]),

    AuthOpts = #{ prep_fun => fun(TxHash) -> ?call(bitcoin_auth, Acc1, "1", TxHash) end },
    {ok, #{init_res := ok, ct_pubkey := Ct}} =
        ?call(ga_create, Acc1, AuthOpts, "identity", []),

    AuthOpts2 = #{ prep_fun => fun(TxHash) -> ?call(bitcoin_auth, Acc1, "2", TxHash) end },
    {ok, #{call_res := ok, call_val := Val}} =
        ?call(ga_call, Acc1, AuthOpts2, Ct, "identity", "main", ["42"]),
    ?assertMatch("42", decode_call_result("identity", "main", ok, Val)),

    ok.

%%%===================================================================
%%% Oracle GA tests
%%%===================================================================
oracle_register(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    AuthOpts = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },
    {ok, #{tx_res := ok, oracle_id := _}} =
        ?call(ga_oracle_register, Acc1, AuthOpts, <<"Question...">>, <<"Answer...">>, 50),

    ok.

oracle_query(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    {ok, #{tx_res := ok, oracle_id := _OracleId}} =
        ?call(ga_oracle_register, Acc1, Auth("1"), <<"Question...">>, <<"Answer...">>, 50),

    {ok, #{tx_res := ok, oracle_query_id := _QueryId}} =
        ?call(ga_oracle_query, Acc1, Auth("2"), <<"How are you?">>),

    ok.

oracle_query_x2(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    {ok, #{tx_res := ok, oracle_id := _OracleId}} =
        ?call(ga_oracle_register, Acc1, Auth("1"), <<"Question...">>, <<"Answer...">>, 50),

    {ok, #{tx_res := ok, oracle_query_id := QueryId1}} =
        ?call(ga_oracle_query, Acc1, Auth("2"), <<"How are you?">>),

    {ok, #{tx_res := ok, oracle_query_id := QueryId2}} =
        ?call(ga_oracle_query, Acc1, Auth("3"), <<"How are you?">>),

    ?assertMatch({X, Y} when X /= Y, {QueryId1, QueryId2}),

    ok.

oracle_respond(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    {ok, #{tx_res := ok, oracle_id := _OracleId}} =
        ?call(ga_oracle_register, Acc1, Auth("1"), <<"Question...">>, <<"Answer...">>, 50),

    {ok, #{tx_res := ok, oracle_query_id := QueryId}} =
        ?call(ga_oracle_query, Acc1, Auth("2"), <<"How are you?">>),

    {ok, #{tx_res := ok}} =
        ?call(ga_oracle_response, Acc1, Auth("3"), QueryId, <<"I am fine, thanks!">>),
    ok.

oracle_extend(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    {ok, #{tx_res := ok, oracle_id := _OracleId}} =
        ?call(ga_oracle_register, Acc1, Auth("1"), <<"Question...">>, <<"Answer...">>, 50),

    {ok, #{tx_res := ok}} =
        ?call(ga_oracle_extend, Acc1, Auth("2"), 100),
    ok.

%%%===================================================================
%%% Multi wrapping GA tests
%%%===================================================================
multi_wrap_spend(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    SpendTx = aega_test_utils:spend_tx(#{sender_id    => aeser_id:create(account, Acc1),
                                         recipient_id => aeser_id:create(account, Acc2),
                                         amount       => 12345,
                                         fee          => 20000 * MinGP,
                                         nonce        => 0}),

    %% First an incorrect inner "signing"
    {_AuthData1, _InnerTx1, MetaTx1} = prep_meta(Acc1, Auth("444"), SpendTx),
    {ok, #{tx_res := error}} =
        ?call(meta, Acc1, Auth("1"), MetaTx1, #{}),

    %% Now the happy case
    {_AuthData2, _InnerTx2, MetaTx2} = prep_meta(Acc1, Auth("3"), SpendTx),
    {ok, #{tx_res := ok}} =
        ?call(meta, Acc1, Auth("2"), MetaTx2, #{}),

    ok.

multi_wrap_sc_create(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    {ok, _} = ?call(attach, Acc2, "basic_auth", "authorize", []),
    Auth1 = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    Auth2 = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc2, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, {basic, "2"}),
    {ok, #{tx_res := ok}} =
        ?call(ga_channel_create, Acc1, Auth1("1"), Acc2, Auth2("1"), OffState, #{}),

    ok.

multi_wrap_sc_close_mutual(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    {ok, _} = ?call(attach, Acc2, "basic_auth", "authorize", []),
    Auth1 = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    Auth2 = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc2, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, {basic, "2"}),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth1("1"), Acc2, Auth2("1"), OffState, #{}),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_close_mutual, Acc1, Auth1("2"), CId, Acc2, Auth2("2"), OffState, #{}),

    ok.

multi_wrap_sc_solo_snapshot(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    {ok, _} = ?call(attach, Acc2, "basic_auth", "authorize", []),
    Auth1 = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,
    Auth2 = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc2, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, {basic, "2"}),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth1("1"), Acc2, Auth2("1"), OffState, #{}),

    BadOffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, {basic, "1"}),
    {ok, #{tx_res := error, tx_value := <<"signature_verification_failed_contract_error">>}} =
        ?call(ga_channel_snapshot_solo, Acc1, Auth1("2"), CId, BadOffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_snapshot_solo, Acc1, Auth1("3"), CId, OffState),

    ok.

%%%===================================================================
%%% Channel GA tests
%%%===================================================================
channel_create(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    ok.

channel_deposit(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    PreBalance  = ?call(account_balance, Acc1),
    DefaultFee = 50000 * aec_test_utils:min_gas_price(),
    {ok, #{tx_res := ok, auth_cost := AuthCost}} =
        ?call(ga_channel_deposit, Acc1, Auth("2"), CId, Acc2, Amnt),
    PostBalance  = ?call(account_balance, Acc1),
    ?assertMatch({X, Y} when X == Y - AuthCost - Amnt - DefaultFee,
                 {PostBalance, PreBalance}),

    ok.

channel_withdraw(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    PreBalance  = ?call(account_balance, Acc1),
    DefaultFee = 50000 * aec_test_utils:min_gas_price(),
    {ok, #{tx_res := ok, auth_cost := AuthCost}} =
        ?call(ga_channel_withdraw, Acc1, Auth("2"), CId, Acc2, Amnt div 10),
    PostBalance  = ?call(account_balance, Acc1),
    ?assertMatch({X, Y} when X == Y - AuthCost + Amnt div 10 - DefaultFee,
                 {PostBalance, PreBalance}),

    ok.

channel_snapshot_solo(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_snapshot_solo, Acc1, Auth("2"), CId, OffState),

    ok.

channel_close_mutual(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_close_mutual, Acc1, Auth("2"), CId, Acc2, OffState),

    ok.

channel_close_solo(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_close_solo, Acc1, Auth("2"), CId, Acc2, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_settle, Acc1, Auth("3"), CId, Acc2, OffState, #{height => 100}),

    ok.

channel_close_solo_snapshot(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_snapshot_solo, Acc1, Auth("2"), CId, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_close_solo, Acc1, Auth("3"), CId, Acc2, OffState, #{height => 4, payload => <<>>}),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_settle, Acc1, Auth("4"), CId, Acc2, OffState, #{height => 100}),

    ok.

channel_close_solo_w_update(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    OffState1 = aega_test_utils:transfer(Acc1, Acc2, 20000, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_close_solo, Acc1, Auth("2"), CId, Acc2, OffState1),


    {ok, #{tx_res := ok}} =
        ?call(ga_channel_settle, Acc1, Auth("3"), CId, Acc2, OffState1, #{height => 100}),

    ok.


channel_slash(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := CId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_close_solo, Acc1, Auth("2"), CId, Acc2, OffState, #{height => 8}),

    OffState1 = aega_test_utils:transfer(Acc1, Acc2, 20000, OffState),

    {ok, #{tx_res := ok}} =
        ?call(ga_channel_slash, Acc1, Auth("3"), CId, Acc2, OffState1, #{height => 10}),

    ok.

channel_force_progress(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    Acc2 = ?call(new_account, 1000000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    Auth = fun(N) -> #{ prep_fun => fun(Tx) -> ?call(basic_auth, Acc1, N, Tx) end } end,

    Amnt = 1000000 * aec_test_utils:min_gas_price(),
    OffState = aega_test_utils:new_state(Acc1, Amnt, {basic, "2"}, Acc2, Amnt, plain),
    {ok, #{tx_res := ok, channel_id := ChId}} =
        ?call(ga_channel_create, Acc1, Auth("1"), Acc2, OffState),

    {CtId, OffState1} = aega_test_utils:add_contract(Acc1, ?call(dry_run, contract_create, {"identity", []}), OffState),

    {ok, #{tx_res := ok, round := 4, call_res := ok}} =
        ?call(ga_channel_force_progress, Acc1, Auth("2"), ChId, OffState1, CtId, "identity", "main", ["42"]),

    ok.

%%%===================================================================
%%% Negative tests
%%%===================================================================
wrap_unrelated_tx(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    AuthOpts = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },

    State   = state(),
    FromId  = aeser_id:create(account, Acc2),
    ToId    = aeser_id:create(account, Acc1),
    SpendTx = aega_test_utils:spend_tx(#{sender_id => FromId, recipient_id => ToId,
                                         amount => 2000, fee => 20000 * MinGP,
                                         nonce => aect_test_utils:next_nonce(Acc2, State) }),
    PrivKey = aect_test_utils:priv_key(Acc2, State),

    SignedTx = aec_test_utils:sign_tx(SpendTx, PrivKey),


    {failed, non_relevant_signature} =
        ?call(meta, Acc1, AuthOpts, SignedTx, #{fail => true}),

    ok.

cripple_auth(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 1000000000 * MinGP),
    Acc2 = ?call(new_account, 1000000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),
    {ok, #{ct := Acc2Ct}} = ?call(attach, Acc2, "simple_auth", "do_auth_test", ["0"]),

    %% Create a remote contract and add some tokens to the Acc2's auth contract
    Auth1 = fun(N) -> #{ prep_fun => fun(_) -> simple_auth(["123", N]) end } end,
    {ok, #{tx_res := ok, init_res := ok, ct_pubkey := Ct}} =
        ?call(ga_create, Acc1, Auth1("1"), "identity", []),

    {ok, #{tx_res := ok}} = ?call(ga_spend, Acc1, Auth1("2"), Acc2Ct, 10000000, 20000 * MinGP),

    %% Now try the crazy stuff
    CtLit = binary_to_list(aeser_api_encoder:encode(contract_pubkey, Ct)),
    Acc1Lit = binary_to_list(aeser_api_encoder:encode(account_pubkey, Acc1)),
    Auth2 = fun(A) -> #{ prep_fun => fun(_) -> aega_test_utils:make_calldata("simple_auth", "do_auth_test", [A]) end } end,
    {failed, authentication_failed} =
        ?call(ga_spend, Acc2, Auth2("Spend(" ++ Acc1Lit ++ ", 1000)"), Acc1, 500, 20000 * MinGP, #{fail => true}),
    {failed, authentication_failed} =
        ?call(ga_spend, Acc2, Auth2("OracleReg"), Acc1, 500, 20000 * MinGP, #{fail => true}),
    {failed, authentication_failed} =
        ?call(ga_spend, Acc2, Auth2("RemoteCall(" ++ CtLit ++ ")"), Acc1, 500, 20000 * MinGP, #{fail => true}),

    ok.



%%%===================================================================
%%% More elaborate operations
%%%===================================================================
sign_and_apply_tx(Fail, Initiator, Tx, Opts, S) ->
    Height   = maps:get(height, Opts, 1),
    PrivKey  = aect_test_utils:priv_key(Initiator, S),
    case sign_and_apply_transaction(Tx, PrivKey, S, Height) of
        {ok, TmpS} when not Fail -> TmpS;
        {ok,_TmpS} when Fail -> error({error, succeeded});
        {error, R,_TmpS} when not Fail -> error(R);
        {error, R, TmpS} when Fail -> throw({fail, R, TmpS})
    end.

spend(From, To, Amount, Fee, State) ->
    spend(From, To, Amount, Fee, #{}, State).

spend(From, To, Amount, Fee, Opts, State) ->
    FromId  = aeser_id:create(account, From),
    ToId    = aeser_id:create(account, To),
    SpendTx = aega_test_utils:spend_tx(#{sender_id => FromId, recipient_id => ToId,
                                         amount => Amount, fee => Fee,
                                         nonce => aect_test_utils:next_nonce(From, State) }),
    S1 = sign_and_apply_tx(false, From, SpendTx, Opts, State),
    {ok, S1}.

attach(Owner, Contract, AuthFun, Args, S) ->
    attach(Owner, Contract, AuthFun, Args, #{}, S).

attach(Owner, Contract, AuthFun, Args, Opts, S) ->
    case get_contract(Contract) of
        {ok, #{src := Src, bytecode := C, map := #{type_info := TI}}} ->
            Fail  = maps:get(fail, Opts, false),
            Nonce = aect_test_utils:next_nonce(Owner, S),
            Calldata = aega_test_utils:make_calldata(Src, "init", Args),
            {ok, AuthFunHash} = aeb_abi:type_hash_from_function_name(list_to_binary(AuthFun), TI),
            Options1 = maps:merge(#{nonce => Nonce, code => C,
                                    auth_fun => AuthFunHash, call_data => Calldata},
                                  maps:without([height, return_return_value, return_gas_used, fail], Opts)),
            AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),
            S1       = sign_and_apply_tx(Fail, Owner, AttachTx, Opts, S),

            ConKey   = aect_contracts:compute_contract_pubkey(Owner, Nonce),
            CallKey  = aect_call:id(Owner, Nonce, ConKey),
            CallTree = aect_test_utils:calls(S1),
            Call     = aect_call_state_tree:get_call(ConKey, CallKey, CallTree),

            {{ok, #{res => aect_call:return_type(Call), ct => ConKey}}, S1};
        _ ->
            error(bad_contract)
    end.

ga_spend(From, AuthOpts, To, Amount, Fee, S) ->
    ga_spend(From, AuthOpts, To, Amount, Fee, #{}, S).

ga_spend(From, AuthOpts, To, Amount, Fee, Opts, S) ->
    SpendTx = aega_test_utils:spend_tx(#{sender_id    => aeser_id:create(account, From),
                                         recipient_id => aeser_id:create(account, To),
                                         amount       => Amount,
                                         fee          => Fee,
                                         nonce        => 0}),
    meta(From, AuthOpts, SpendTx, Opts, S).

ga_create(Owner, AuthOpts, ContractName, InitArgs, S) ->
    ga_create(Owner, AuthOpts, ContractName, InitArgs, #{}, S).

ga_create(Owner, AuthOpts, ContractName, InitArgs, Opts, S) ->
    {ok, #{src := Src, bytecode := Code}} = get_contract(ContractName),
    CallData = aega_test_utils:make_calldata(Src, "init", InitArgs),
    Options1 = maps:merge(#{nonce => 0, code => Code, call_data => CallData},
                          maps:without([height], Opts)),
    CreateTx = create_tx(Owner, Options1, S),

    meta(Owner, AuthOpts, CreateTx, Opts, S).

ga_call(Caller, AuthOpts, ContractPK, ContractName, Fun, Args, S) ->
    ga_call(Caller, AuthOpts, ContractPK, ContractName, Fun, Args, #{}, S).

ga_call(Caller, AuthOpts, Contract, ContractName, Fun, Args, Opts, S) ->
    CallData = aega_test_utils:make_calldata(ContractName, Fun, Args),
    Options1 = maps:merge(#{nonce => 0, call_data => CallData},
                          maps:without([height], Opts)),
    CallTx   = call_tx(Caller, Contract, Options1, S),

    meta(Caller, AuthOpts, CallTx, Opts, S).

ga_attach(Owner, AuthOpts, Contract, AuthFun, InitArgs, S) ->
    ga_attach(Owner, AuthOpts, Contract, AuthFun, InitArgs, #{}, S).

ga_attach(Owner, AuthOpts, Contract, AuthFun, InitArgs, Opts, S) ->
    {ok, #{src := Src, bytecode := Code, map := #{type_info := TI}}} = get_contract(Contract),
    Calldata = aega_test_utils:make_calldata(Src, "init", InitArgs),
    {ok, AuthFunHash} = aeb_abi:type_hash_from_function_name(list_to_binary(AuthFun), TI),
    Options1 = maps:merge(#{nonce => 0, code => Code, auth_fun => AuthFunHash,
                            call_data => Calldata}, maps:without([height], Opts)),
    AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),
    meta(Owner, AuthOpts, AttachTx, Opts, S).

ga_oracle_register(Caller, AuthOpts, QSpec, ASpec, TTL, S) ->
    ga_oracle_register(Caller, AuthOpts, QSpec, ASpec, TTL, #{}, S).

ga_oracle_register(Caller, AuthOpts, QSpec, ASpec, TTL, Opts, S) ->
    Opts1 = maps:merge(#{query_format => QSpec, response_format => ASpec, nonce => 0,
                         oracles_ttl => {delta, TTL}}, maps:without([height], Opts)),
    RegTx = aeo_test_utils:register_tx(Caller, Opts1, S),

    meta(Caller, AuthOpts, RegTx, Opts, S).

ga_oracle_query(Caller, AuthOpts, Q, S) ->
    ga_oracle_query(Caller, AuthOpts, Q, #{}, S).

ga_oracle_query(Caller, AuthOpts, Q, Opts, S) ->
    Opts1 = maps:merge(#{query => Q, nonce => 0}, maps:without([height], Opts)),
    QueryTx = aeo_test_utils:query_tx(Caller, aeser_id:create(oracle, Caller), Opts1, S),

    meta(Caller, AuthOpts, QueryTx, Opts, S).

ga_oracle_response(Caller, AuthOpts, QId, Answer, S) ->
    ga_oracle_response(Caller, AuthOpts, QId, Answer, #{}, S).

ga_oracle_response(Caller, AuthOpts, QId, Answer, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0}, maps:without([height], Opts)),
    ResponseTx = aeo_test_utils:response_tx(Caller, QId, Answer, Opts1, S),

    meta(Caller, AuthOpts, ResponseTx, Opts, S).

ga_oracle_extend(Caller, AuthOpts, TTL, S) ->
    ga_oracle_extend(Caller, AuthOpts, TTL, #{}, S).

ga_oracle_extend(Caller, AuthOpts, TTL, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0, oracle_ttl => {delta, TTL}}, maps:without([height], Opts)),
    ExtendTx = aeo_test_utils:extend_tx(Caller, Opts1, S),

    meta(Caller, AuthOpts, ExtendTx, Opts, S).

ga_channel_create(Initiator, AuthOpts, Responder, OffState, S) ->
    ga_channel_create(Initiator, AuthOpts, Responder, plain, OffState, #{}, S).

ga_channel_create(Initiator, AuthOpts1, Responder, AuthOpts2, OffState, Opts, S) ->
    IAmnt = aega_test_utils:balance(Initiator, OffState),
    RAmnt = aega_test_utils:balance(Responder, OffState),
    SHash = aega_test_utils:state_hash(OffState),
    Opts1 = maps:merge(#{nonce => 0, initiator_amount => IAmnt, state_hash => SHash,
                         responder_amount => RAmnt, lock_period => 5}, maps:without([height], Opts)),

    {ok, SCCreateTx} = aesc_create_tx:new(aesc_test_utils:create_tx_spec(Initiator, Responder, Opts1, S)),

    meta(Initiator, AuthOpts1, sign_tx(Responder, AuthOpts2, SCCreateTx, S), Opts, S).

ga_channel_deposit(Depositor, AuthOpts, CId, Acc2, Amnt, S) ->
    ga_channel_deposit(Depositor, AuthOpts, CId, Acc2, Amnt, #{}, S).

ga_channel_deposit(Depositor, AuthOpts, CId, Acc2, Amnt, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0, amount => Amnt}, maps:without([height], Opts)),

    {ok, SCDepositTx} = aesc_deposit_tx:new(
                          aesc_test_utils:deposit_tx_spec(CId, Depositor, Opts1, S)),

    meta(Depositor, AuthOpts, basic_sign_tx(Acc2, SCDepositTx, S), Opts, S).

ga_channel_withdraw(Withdrawer, AuthOpts, CId, Acc2, Amnt, S) ->
    ga_channel_withdraw(Withdrawer, AuthOpts, CId, Acc2, Amnt, #{}, S).

ga_channel_withdraw(Withdrawer, AuthOpts, CId, Acc2, Amnt, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0, amount => Amnt}, maps:without([height], Opts)),

    {ok, SCWithdrawTx} = aesc_withdraw_tx:new(
                          aesc_test_utils:withdraw_tx_spec(CId, Withdrawer, Opts1, S)),

    meta(Withdrawer, AuthOpts, basic_sign_tx(Acc2, SCWithdrawTx, S), Opts, S).

ga_channel_snapshot_solo(Snapshoter, AuthOpts, CId, OffState, S) ->
    ga_channel_snapshot_solo(Snapshoter, AuthOpts, CId, OffState, #{}, S).

ga_channel_snapshot_solo(Snapshoter, AuthOpts, CId, OffState, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0}, maps:without([height], Opts)),

    Payload = aega_test_utils:payload(CId, OffState, S),

    {ok, SCSnapshotTx} = aesc_snapshot_solo_tx:new(
                          aesc_test_utils:snapshot_solo_tx_spec(CId, Snapshoter, Payload, Opts1, S)),

    meta(Snapshoter, AuthOpts, SCSnapshotTx, Opts, S).

ga_channel_close_mutual(Acc1, AuthOpts, CId, Acc2, OffState, S) ->
    ga_channel_close_mutual(Acc1, AuthOpts, CId, Acc2, plain, OffState, #{}, S).

ga_channel_close_mutual(Acc1, AuthOpts1, CId, Acc2, AuthOpts2, OffState, Opts, S) ->
    IAmnt = aega_test_utils:balance(Acc1, OffState) - 25000 * aec_test_utils:min_gas_price(),
    RAmnt = aega_test_utils:balance(Acc2, OffState) - 25000 * aec_test_utils:min_gas_price(),
    Opts1 = maps:merge(#{nonce => 0, initiator_amount_final => IAmnt,
                         responder_amount_final => RAmnt, initiator_account => Acc1 },
                       maps:without([height], Opts)),

    {ok, SCMutualCloseTx} = aesc_close_mutual_tx:new(
                          aesc_test_utils:close_mutual_tx_spec(CId, Opts1, S)),

    meta(Acc1, AuthOpts1, sign_tx(Acc2, AuthOpts2, SCMutualCloseTx, S), Opts, S).

ga_channel_close_solo(Acc1, AuthOpts, CId, Acc2, OffState, S) ->
    ga_channel_close_solo(Acc1, AuthOpts, CId, Acc2, OffState, #{}, S).

ga_channel_close_solo(Acc1, AuthOpts, CId, Acc2, OffState, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0}, maps:without([height, payload], Opts)),

    Payload = maps:get(payload, Opts, aega_test_utils:payload(CId, OffState, S)),
    PoI     = aega_test_utils:poi([{account, Acc1}, {account, Acc2}], OffState),

    {ok, SCSoloCloseTx} = aesc_close_solo_tx:new(
                          aesc_test_utils:close_solo_tx_spec(CId, Acc1, Payload, PoI, Opts1, S)),

    meta(Acc1, AuthOpts, SCSoloCloseTx, Opts, S).

ga_channel_settle(Acc1, AuthOpts, CId, Acc2, OffState, Opts, S) ->
    IAmnt = aega_test_utils:balance(Acc1, OffState),% - 25000 * aec_test_utils:min_gas_price(),
    RAmnt = aega_test_utils:balance(Acc2, OffState),% - 25000 * aec_test_utils:min_gas_price(),
    Opts1 = maps:merge(#{nonce => 0, initiator_amount => IAmnt,
                         responder_amount => RAmnt}, maps:without([height], Opts)),

    {ok, SCSettleTx} = aesc_settle_tx:new(
                          aesc_test_utils:settle_tx_spec(CId, Acc1, Opts1, S)),

    meta(Acc1, AuthOpts, SCSettleTx, Opts, S).

ga_channel_slash(Acc1, AuthOpts, CId, Acc2, OffState, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0}, maps:without([height], Opts)),

    Payload = aega_test_utils:payload(CId, OffState, S),
    PoI     = aega_test_utils:poi([{account, Acc1}, {account, Acc2}], OffState),

    {ok, SCSlashTx} = aesc_slash_tx:new(
                          aesc_test_utils:slash_tx_spec(CId, Acc1, Payload, PoI, Opts1, S)),

    meta(Acc1, AuthOpts, SCSlashTx, Opts, S).

ga_channel_force_progress(Acc1, AuthOpts, CId, OffState, CtId, Contract, Fun, Args, S) ->
    ga_channel_force_progress(Acc1, AuthOpts, CId, OffState, CtId, Contract, Fun, Args, #{}, S).

ga_channel_force_progress(Acc1, AuthOpts, CId, OffState, CtId, Contract, Fun, Args, Opts, S) ->
    Opts1 = maps:merge(#{nonce => 0}, maps:without([height], Opts)),

    Payload = aega_test_utils:payload(CId, OffState, S),
    CallData = aega_test_utils:make_calldata(Contract, Fun, Args),
    Update  = aesc_offchain_update:op_call_contract(aeser_id:create(account, Acc1), aeser_id:create(contract, CtId),
                                                    1, 0, CallData, [], 1 * aec_test_utils:min_gas_price(), 1000),
    OffTrees  = aega_test_utils:offchain_trees(OffState),
    Round = maps:get(round, OffState) + 2,
    {DryRes, _} = dry_run(contract_call, Acc1, {CtId, Contract, Fun, Args}, S#{trees := OffTrees}),
    OffState1 = aega_test_utils:add_call(CtId, Round, DryRes, OffState),
    StateHash = aega_test_utils:state_hash(OffState1),
    {ok, SCForceProgressTx} = aesc_force_progress_tx:new(
                          aesc_test_utils:force_progress_tx_spec(CId, Acc1, Payload, Update, StateHash, Round, OffTrees, Opts1, S)),

    meta(Acc1, AuthOpts, SCForceProgressTx, Opts, S).



meta(Owner, AuthOpts, InnerTx0, Opts, S) ->
    {AuthData, InnerTx, MetaTx} = prep_meta(Owner, AuthOpts, InnerTx0),
    do_meta(Owner, AuthData, InnerTx, MetaTx, Opts, S).

prep_meta(Owner, AuthOpts, InnerTx0) ->
    {InnerTx, InnerSTx} =
        try aetx_sign:tx(InnerTx0) of
            Tx_ -> {Tx_, InnerTx0}
        catch _:_ ->
            {InnerTx0, aetx_sign:new(InnerTx0, [])}
        end,
    TxBin    = aec_governance:add_network_id(aetx:serialize_to_binary(InnerTx)),
    AuthData = make_authdata(AuthOpts, aec_hash:hash(tx, TxBin)),
    Options1 = maps:merge(#{auth_data => AuthData, tx => InnerSTx}, AuthOpts),
    MetaTx   = aega_test_utils:ga_meta_tx(Owner, Options1),
    {AuthData, InnerTx, MetaTx}.

do_meta(Owner, AuthData, InnerTx, MetaTx, Opts, S) ->
    Fail     = maps:get(fail, Opts, false),
    Height   = maps:get(height, Opts, 1),
    SMetaTx  = aetx_sign:new(MetaTx, []),
    S1       = case apply_transaction(SMetaTx, S, Height) of
                   {ok, TmpS} when not Fail       -> TmpS;
                   {ok,_TmpS} when Fail           -> error({error, succeeded});
                   {error, R,_TmpS} when not Fail -> error(R);
                   {error, R, TmpS} when Fail     -> throw({fail, R, TmpS})
               end,

    %% Getting here means authentication passed
    AuthId        = aega_meta_tx:auth_id(Owner, AuthData),
    {AuthCtId, _} = account_contract(Owner, S),
    {_, AuthCtPK} = aeser_id:specialize(AuthCtId),
    CallKey       = aect_call:ga_id(AuthId, AuthCtPK),
    CallTree      = aect_test_utils:calls(S1),
    Call          = aect_call_state_tree:get_call(Owner, CallKey, CallTree),

    GasUsed = aect_call:gas_used(Call),
    AuthCost = aetx:fee(MetaTx) + aetx:gas_price(MetaTx) * GasUsed,
    Res00 = #{ auth_gas => GasUsed, auth_cost => AuthCost,
               tx_res => aect_call:return_type(Call), tx_value => aect_call:return_value(Call) },

    {Res0, InnerTx1} = peel_onion(Res00, InnerTx, S1),

    DeepFee  = aetx:deep_fee(MetaTx, aect_test_utils:trees(S1)),
    case aect_call:return_type(Call) of
        ok ->
            %% The total amount of fees should include the fees for
            %% the inner tx (transitively) as well
            InnerDeepFee = aetx:deep_fee(InnerTx, aect_test_utils:trees(S1)),
            ?assert(InnerDeepFee > 0),
            ?assertEqual(DeepFee, aetx:fee(MetaTx) + InnerDeepFee);
        error ->
            %% Only the fee of the meta transaction should be deducted
            ?assertEqual(DeepFee, aetx:fee(MetaTx))
    end,

    Res =
        case aetx:specialize_type(InnerTx1) of
            {spend_tx, _SpendTx} ->
                {ok, Res0#{ total_cost => AuthCost + aetx:fee(InnerTx) }};
            {contract_create_tx, _CCTx} ->
                NewContract = aect_contracts:compute_contract_pubkey(Owner, AuthId),
                InitCallId  = aect_call:ga_id(AuthId, NewContract),
                InitCall    = aect_call_state_tree:get_call(NewContract, InitCallId, CallTree),
                {ok, Res0#{ ct_pubkey => NewContract
                          , init_res  => aect_call:return_type(InitCall) }};
            {contract_call_tx, CCTx} ->
                Contract    = aect_call_tx:contract_pubkey(CCTx),
                InnerCallId = aect_call:ga_id(AuthId, Contract),
                InnerCall   = aect_call_state_tree:get_call(Contract, InnerCallId, CallTree),
                {ok, Res0#{ call_res => aect_call:return_type(InnerCall),
                            call_val => aect_call:return_value(InnerCall),
                            call_gas => aect_call:gas_used(InnerCall) }};
            {oracle_register_tx, ORegTx} ->
                {ok, Res0#{oracle_id => aeo_register_tx:account_pubkey(ORegTx)}};
            {oracle_query_tx, OQueryTx} ->
                OPK = aeo_query_tx:oracle_pubkey(OQueryTx),
                {ok, Res0#{oracle_query_id => aec_hash:hash(pubkey, <<AuthId/binary, OPK/binary>>)}};
            {channel_create_tx, CCTx} ->
                ChannelId = aesc_channels:pubkey(aesc_create_tx:initiator_pubkey(CCTx),
                                                 AuthId,
                                                 aesc_create_tx:responder_pubkey(CCTx)),
                {ok, Res0#{channel_id => ChannelId}};
            {channel_force_progress_tx, CFPTx} ->
                ChId                = aesc_force_progress_tx:channel_pubkey(CFPTx),
                ChannelTrees        = aec_trees:channels(aect_test_utils:trees(S1)),
                {value, Channel}    = aesc_state_tree:lookup(ChId, ChannelTrees),
                ContractPlaceholder = aesc_utils:tx_hash_to_contract_pubkey(aetx_sign:hash(SMetaTx)),
                InnerCallId         = aect_call:ga_id(AuthId, ContractPlaceholder),
                InnerCall           = aect_call_state_tree:get_call(ContractPlaceholder, InnerCallId, CallTree),
                {ok, Res0#{round => aesc_channels:round(Channel),
                           call_res => aect_call:return_type(InnerCall)}};
            {Tx, _} when Tx == oracle_response_tx; Tx == oracle_extend_tx;
                         Tx == channel_deposit_tx; Tx == channel_withdraw_tx;
                         Tx == channel_snapshot_solo_tx; Tx == channel_close_mutual_tx;
                         Tx == channel_close_solo_tx; Tx == channel_settle_tx;
                         Tx == channel_slash_tx; Tx == ga_attach_tx; Tx == ga_meta_tx ->
                {ok, Res0}
        end,
    {Res, S1}.

peel_onion(Res = #{ tx_res := TxRes }, InnerTx, S) ->
    case aetx:specialize_type(InnerTx) of
        {ga_meta_tx, GAMetaTx} when TxRes == ok ->
            Owner         = aega_meta_tx:ga_pubkey(GAMetaTx),
            AuthId        = aega_meta_tx:auth_id(Owner, aega_meta_tx:auth_data(GAMetaTx)),
            {AuthCtId, _} = account_contract(Owner, S),
            {_, AuthCtPK} = aeser_id:specialize(AuthCtId),
            CallKey       = aect_call:ga_id(AuthId, AuthCtPK),
            CallTree      = aect_test_utils:calls(S),
            Call          = aect_call_state_tree:get_call(Owner, CallKey, CallTree),
            GasUsed       = aect_call:gas_used(Call),
            AuthCost      = aetx:fee(InnerTx) + aetx:gas_price(InnerTx) * GasUsed,
            Res1 = Res#{ auth_gas  := GasUsed + maps:get(auth_gas, Res),
                         auth_cost := AuthCost + maps:get(auth_cost, Res) },
            peel_onion(Res1, aetx_sign:tx(aega_meta_tx:tx(GAMetaTx)), S);
        _ ->
            {Res, InnerTx}
    end.

dry_run(Op, Args, S) ->
    {Acc, S1} = new_account(10000000 * aec_test_utils:min_gas_price(), S),
    dry_run(Op, Acc, Args, S1).

dry_run(Op, Acc, Args, S) ->
    Nonce = aect_test_utils:next_nonce(Acc, S),
    dry_run(Op, Acc, Nonce, Args, S).

dry_run(contract_call, Acc, Nonce, {ContractPK, Contract, Fun, Args}, S) ->
    CallData = aega_test_utils:make_calldata(Contract, Fun, Args),
    CallTx   = call_tx(Acc, ContractPK, #{call_data => CallData, nonce => Nonce}, S),
    PrivKey  = aect_test_utils:priv_key(Acc, S),
    {ok, S1} = sign_and_apply_transaction(CallTx, PrivKey, S, 1),
    CallKey  = aect_call:id(Acc, Nonce, ContractPK),
    CallTree = aect_test_utils:calls(S1),
    Call     = aect_call_state_tree:get_call(ContractPK, CallKey, CallTree),
    {{ok, Call, aect_test_utils:trees(S1)}, S};
dry_run(contract_create, Acc, Nonce, {Contract, InitArgs}, S) ->
    InitData = aega_test_utils:make_calldata(Contract, "init", InitArgs),
    {ok, #{bytecode := Code}} = get_contract(Contract),
    CreateTx = create_tx(Acc, #{call_data => InitData, nonce => Nonce, code => Code}, S),
    PrivKey  = aect_test_utils:priv_key(Acc, S),
    {ok, S1} = sign_and_apply_transaction(CreateTx, PrivKey, S, 1),
    {{ok, aect_contracts:compute_contract_pubkey(Acc, Nonce), aect_test_utils:trees(S1)}, S}.


%%%===================================================================
%%% Transactions
%%%===================================================================
create_tx(Owner, Spec0, State) ->
    Spec = maps:merge(create_tx_default(), Spec0),
    aect_test_utils:create_tx(Owner, Spec, State).

create_tx_default() ->
    #{ abi_version => aect_test_utils:latest_sophia_abi_version()
     , vm_version  => vm_version()
     , fee         => 100000 * aec_test_utils:min_gas_price()
     , deposit     => 10
     , amount      => 200
     , gas         => 10000 }.

call_tx(Caller, Contract, Spec0, State) ->
    Spec = maps:merge(call_tx_default(), Spec0),
    aect_test_utils:call_tx(Caller, Contract, Spec, State).

call_tx_default() ->
    #{ nonce       => 0
     , abi_version => aect_test_utils:latest_sophia_abi_version()
     , fee         => 500000 * aec_test_utils:min_gas_price()
     , amount      => 0
     , gas         => 10000 }.

%%%===================================================================
%%% Test framework/machinery
%%%===================================================================

sign_and_apply_transaction(Tx, PrivKey, S, Height) ->
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    apply_transaction(SignedTx, S, Height).

sign_tx(Pubkey, plain, Tx, S) ->
    PrivKey  = aect_test_utils:priv_key(Pubkey, S),
    aec_test_utils:sign_tx(Tx, PrivKey);
sign_tx(Pubkey, AuthOpts, Tx, _S) ->
    TxBin    = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    AuthData = make_authdata(AuthOpts, aec_hash:hash(tx, TxBin)),
    Options1 = #{auth_data => AuthData, tx => aetx_sign:new(Tx, [])},
    MetaTx   = aega_test_utils:ga_meta_tx(Pubkey, Options1),
    aetx_sign:new(MetaTx, []).

basic_sign_tx(Pubkey, Tx, S) ->
    sign_tx(Pubkey, plain, Tx, S).

apply_transaction(Tx, S1, Height) ->
    Trees    = aect_test_utils:trees(S1),
    Env0     = aetx_env:tx_env(Height),
    Env      = aetx_env:set_beneficiary(Env0, ?BENEFICIARY_PUBKEY),
    case aec_block_micro_candidate:apply_block_txs_strict([Tx], Trees, Env) of
        {ok, [Tx], Trees1, _} ->
            S2 = aect_test_utils:set_trees(Trees1, S1),
            {ok, S2};
        {error, R} ->
            {error, R, S1}
    end.

call(Name, Fun, Xs) ->
    Fmt = string:join(lists:duplicate(length(Xs), "~p"), ", "),
    Xs1 = [ case X of
                <<Pre:32, _:28/unit:8>> -> <<Pre:32>>;
                _ -> X
            end || X <- Xs ],
    io:format("~p(" ++ Fmt ++ ") ->\n", [Name | Xs1]),
    R = call(Fun, Xs),
    io:format("Response:  ~p\n", [R]),
    R.

call(Fun, Xs) when is_function(Fun, 1 + length(Xs)) ->
    S = state(),
    {R, S1} = try apply(Fun, Xs ++ [S])
              catch
                _:{fail, Rx, Sx} -> {{failed, Rx}, Sx};
                _:{fail, Error} -> error(Error);
                _:Reason -> {{'EXIT', Reason, erlang:get_stacktrace()}, S}
              end,
    state(S1),
    R.

state()  -> get(the_state).
state(S) -> put(the_state, S).

new_account(Balance, S) ->
    aect_test_utils:setup_new_account(Balance, S).

account_balance(PubKey, S) ->
    Account = aect_test_utils:get_account(PubKey, S),
    {aec_accounts:balance(Account), S}.

account_contract(PK, S) ->
    Account = aect_test_utils:get_account(PK, S),
    {aec_accounts:ga_contract(Account), S}.

%% perform_pre_transformations(Height, S) ->
%%     Trees = aec_trees:perform_pre_transformations(aect_test_utils:trees(S), Height),
%%     {ok, aect_test_utils:set_trees(Trees, S)}.

%% get_contract_state(Contract) ->
%%     S = state(),
%%     {{value, C}, _} = lookup_contract_by_id(Contract, S),
%%     aect_contracts_store:contents(aect_contracts:state(C)).

%% insert_contract(Account, Code, S) ->
%%     Contract  = make_contract(Account, Code, S),
%%     Contracts = aect_state_tree:insert_contract(Contract, aect_test_utils:contracts(S)),
%%     {Contract, aect_test_utils:set_contracts(Contracts, S)}.

%% insert_call(Sender, Contract, Fun, S) ->
%%     ContractPubkey = aect_contracts:pubkey(Contract),
%%     Call           = make_call(Sender, ContractPubkey, Fun, S),
%%     CallTree       = aect_call_state_tree:insert_call(Call, aect_test_utils:calls(S)),
%%     {Call, aect_test_utils:set_calls(CallTree, S)}.

%% get_contract(Contract0, S) ->
%%     ContractPubkey = aect_contracts:pubkey(Contract0),
%%     Contracts      = aect_test_utils:contracts(S),
%%     Contract       = aect_state_tree:get_contract(ContractPubkey, Contracts),
%%     {Contract, S}.

%% lookup_contract_by_id(ContractKey, S) ->
%%     Contracts = aect_test_utils:contracts(S),
%%     X         = aect_state_tree:lookup_contract(ContractKey, Contracts),
%%     {X, S}.

%% get_call(Contract0, Call0, S) ->
%%     CallId         = aect_call:id(Call0),
%%     ContractPubkey = aect_contracts:pubkey(Contract0),
%%     CallTree       = aect_test_utils:calls(S),
%%     Call           = aect_call_state_tree:get_call(ContractPubkey, CallId, CallTree),
%%     {Call, S}.

%% state_tree(_Cfg) ->
%%     state(aect_test_utils:new_state()),
%%     Acc1  = ?call(new_account, 100),
%%     Ct1   = ?call(insert_contract, Acc1, <<"Code for C1">>),
%%     Ct1   = ?call(get_contract, Ct1),
%%     Acc2  = ?call(new_account, 50),
%%     Acc3  = ?call(new_account, 30),
%%     Ct2   = ?call(insert_contract, Acc2, <<"Code for C2">>),
%%     Ct2   = ?call(get_contract, Ct2),
%%     Ct1   = ?call(get_contract, Ct1),
%%     Call1 = ?call(insert_call, Acc3, Ct1, <<"Ct1.foo">>),
%%     Call2 = ?call(insert_call, Acc2, Ct1, <<"Ct1.bar">>),
%%     Call1 = ?call(get_call, Ct1, Call1),
%%     Call2 = ?call(get_call, Ct1, Call2),
%%     Ct1   = ?call(get_contract, Ct1),
%%     <<"Code for C1">> = aect_contracts:code(Ct1),
%%     ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

vm_version() ->
    case get('$vm_version') of
        undefined -> aect_test_utils:latest_sophia_vm_version();
        X         -> X
    end.

%% protocol_version() ->
%%     case get('$protocol_version') of
%%         undefined -> aect_test_utils:latest_protocol_version();
%%         X         -> X
%%     end.

sophia_version() ->
    case get('$sophia_version') of
        undefined -> ?SOPHIA_FORTUNA_AEVM;
        X         -> X
    end.

make_authdata(#{ prep_fun := F }, TxHash) ->
    F(TxHash).

get_contract(Name) ->
    aega_test_utils:get_contract(sophia_version(), Name).

decode_call_result(Name0, Fun, Type, Val) ->
    Name = filename:join("contracts", Name0),
    {ok, BinSrc} = aect_test_utils:read_contract(Name),
    {ok, AST} = aeso_compiler:to_sophia_value(binary_to_list(BinSrc), Fun, Type, Val),
    prettypr:format(aeso_pretty:expr(AST)).

simple_auth(Args) ->
    aega_test_utils:make_calldata("simple_auth", "authorize", Args).

basic_auth(GA, Nonce, TxHash, S) ->
%%     {GACt, _}  = account_contract(GA, S),
%%     {contract, ContractPK} = aeser_id:specialize(GACt),
%%     {ok, Call} = dry_run(ContractPK, "basic_auth", "to_sign", [aega_test_utils:to_hex_lit(32, TxHash), Nonce], S),
%%     ok   = aect_call:return_type(Call),
%%     Val  = aect_call:return_value(Call),
%%     Hash = decode_call_result("basic_auth", "to_sign", ok, Val),

    GAPrivKey  = aect_test_utils:priv_key(GA, S),
    %% Sign = enacl:sign_detached(hash_lit_to_bin(Hash), GAPrivKey),

    Sign = basic_auth(list_to_integer(Nonce), TxHash, GAPrivKey),

    {aega_test_utils:make_calldata("basic_auth", "authorize", [Nonce, aega_test_utils:to_hex_lit(64, Sign)]), S}.

basic_auth(Nonce, TxHash, Privkey) ->
    Val = <<32:256, TxHash/binary, Nonce:256>>,
    enacl:sign_detached(aec_hash:hash(tx, Val), Privkey).

bitcoin_auth(_GA, Nonce, TxHash, S) ->
    Val = <<32:256, TxHash/binary, (list_to_integer(Nonce)):256>>,
    Sig = crypto:sign(ecdsa, sha256, {digest, aec_hash:hash(tx, Val)}, [?SECP256K1_PRIV, secp256k1]),
    {aega_test_utils:make_calldata("bitcoin_auth", "authorize", [Nonce, der_sig_decode(Sig)]), S}.

der_sig_decode(<<16#30, _Len0:8, 16#02, Len1:8, Rest/binary>>) ->
    <<R:Len1/binary, 16#02, Len2:8, S/binary>> = Rest,
    aega_test_utils:to_hex_lit(64, <<(der_part_trunc(Len1, R)):32/binary,
                                     (der_part_trunc(Len2, S)):32/binary>>).

der_part_trunc(33, <<0, Rest/binary>>)   -> Rest;
der_part_trunc(Len, Part) when Len =< 32 -> <<0:((32-Len)*8), Part/binary>>.
