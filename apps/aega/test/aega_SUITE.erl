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
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).
-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).

-define(CHAIN_RELATIVE_TTL_MEMORY_ENCODING(X), {variant, 0, [X]}).
-define(CHAIN_ABSOLUTE_TTL_MEMORY_ENCODING(X), {variant, 1, [X]}).

-define(AESOPHIA_1, 1).
-define(AESOPHIA_2, 2).
-define(LATEST_AESOPHIA, ?AESOPHIA_2).

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all}].

groups() ->
    [ {all, [], [ {group, simple}
                , {group, basic}
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
                  ]}
    ].

init_per_group(all, Cfg) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, generalized_accounts_not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN ->
            [{sophia_version, ?AESOPHIA_2}, {vm_version, ?VM_AEVM_SOPHIA_3},
             {protocol, fortuna} | Cfg]
    end;
%% init_per_group(vm_interaction, Cfg) ->
%%     Height = 10,
%%     Fun = fun(H) when H <  Height -> ?ROMA_PROTOCOL_VSN;
%%              (H) when H >= Height -> ?MINERVA_PROTOCOL_VSN
%%           end,
%%     meck:expect(aec_hard_forks, protocol_effective_at_height, Fun),
%%     [{sophia_version, ?AESOPHIA_2}, {vm_version, ?VM_AEVM_SOPHIA_2},
%%      {fork_height, Height},
%%      {protocol, minerva} | Cfg];
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

-define(call(Fun, X),                call(Fun, fun Fun/2, [X])).
-define(call(Fun, X, Y),             call(Fun, fun Fun/3, [X, Y])).
-define(call(Fun, X, Y, Z),          call(Fun, fun Fun/4, [X, Y, Z])).
-define(call(Fun, X, Y, Z, U),       call(Fun, fun Fun/5, [X, Y, Z, U])).
-define(call(Fun, X, Y, Z, U, V),    call(Fun, fun Fun/6, [X, Y, Z, U, V])).
-define(call(Fun, X, Y, Z, U, V, W), call(Fun, fun Fun/7, [X, Y, Z, U, V, W])).

%%%===================================================================
%%% Simple GA tests
%%%===================================================================

simple_attach(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 10000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),
    ok.

simple_double_attach_fail(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 10000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    {failed, not_a_basic_account} =
        ?call(attach, Acc1, "simple_auth", "authorize", ["0"], #{fail => true}),

    ok.

simple_spend_to(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 10000000 * MinGP),
    Acc2 = ?call(new_account, 10000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "simple_auth", "authorize", ["123"]),

    PreBalance  = ?call(account_balance, Acc1),
    ok          = ?call(spend, Acc2, Acc1, 500,  20000 * MinGP),
    PostBalance = ?call(account_balance, Acc1),
    ?assertMatch({X, Y} when X + 500 == Y, {PreBalance, PostBalance}),

    ok.

simple_spend_from(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 10000000 * MinGP),
    Acc2 = ?call(new_account, 10000000 * MinGP),
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
    Acc1 = ?call(new_account, 10000000 * MinGP),
    Acc2 = ?call(new_account, 10000000 * MinGP),
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
    {ok, #{tx_res := error}} =
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
    {ok, #{tx_res := error}} = ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000),
    PostBalance = ?call(account_balance, Acc2),
    ?assertEqual(PreBalance, PostBalance),

    ok.

%%%===================================================================
%%% Basic GA tests
%%%===================================================================
basic_attach(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 10000000 * aec_test_utils:min_gas_price()),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),
    ok.

basic_spend_from(_Cfg) ->
    state(aect_test_utils:new_state()),
    MinGP = aec_test_utils:min_gas_price(),
    Acc1 = ?call(new_account, 10000000 * MinGP),
    Acc2 = ?call(new_account, 10000000 * MinGP),
    {ok, _} = ?call(attach, Acc1, "basic_auth", "authorize", []),

    AuthOpts    = #{ prep_fun => fun(TxHash) -> ?call(basic_auth, Acc1, "1", TxHash) end },
    PreBalance  = ?call(account_balance, Acc2),
    {ok, _}     = ?call(ga_spend, Acc1, AuthOpts, Acc2, 500, 20000 * MinGP),
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

%%%===================================================================
%%% More elaborate operations
%%%===================================================================

spend(From, To, Amount, Fee, State) ->
    spend(From, To, Amount, Fee, #{}, State).

spend(From, To, Amount, Fee, Opts, State) ->
    FromId  = aeser_id:create(account, From),
    ToId    = aeser_id:create(account, To),
    SpendTx = aega_test_utils:spend_tx(#{sender_id => FromId, recipient_id => ToId,
                                         amount => Amount, fee => Fee,
                                         nonce => aect_test_utils:next_nonce(From, State) }),
    Height  = maps:get(height, Opts, 1),
    PrivKey = aect_test_utils:priv_key(From, State),
    case sign_and_apply_transaction(SpendTx, PrivKey, State, Height) of
        {ok, TmpS}       -> {ok, TmpS};
        {error, R,_TmpS} -> error(R)
    end.

attach(Owner, Contract, AuthFun, Args, S) ->
    attach(Owner, Contract, AuthFun, Args, #{}, S).

attach(Owner, Contract, AuthFun, Args, Opts, S) ->
    case get_contract(Contract) of
        {ok, #{src := Src, bytecode := C, map := #{type_info := TI}}} ->
            Fail  = maps:get(fail, Opts, false),
            Nonce = aect_test_utils:next_nonce(Owner, S),
            Calldata = aega_test_utils:make_calldata(Src, "init", Args),
            {ok, AuthFunHash} = aeso_abi:type_hash_from_function_name(list_to_binary(AuthFun), TI),
            Options1 = maps:merge(#{nonce => Nonce, code => C,
                                    auth_fun => AuthFunHash, call_data => Calldata},
                                  maps:without([height, return_return_value, return_gas_used, fail], Opts)),
            AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),
            Height   = maps:get(height, Opts, 1),
            PrivKey  = aect_test_utils:priv_key(Owner, S),
            S1       = case sign_and_apply_transaction(AttachTx, PrivKey, S, Height) of
                           {ok, TmpS} when not Fail -> TmpS;
                           {ok,_TmpS} when Fail -> error({error, succeeded});
                           {error, R,_TmpS} when not Fail -> error(R);
                           {error, R, TmpS} when Fail -> throw({fail, R, TmpS})
                       end,
            ConKey   = aect_contracts:compute_contract_pubkey(Owner, Nonce),
            CallKey  = aect_call:id(Owner, Nonce, ConKey),
            CallTree = aect_test_utils:calls(S1),
            Call     = aect_call_state_tree:get_call(ConKey, CallKey, CallTree),

            {{ok, aect_call:return_type(Call)}, S1};
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
    {ok, AuthFunHash} = aeso_abi:type_hash_from_function_name(list_to_binary(AuthFun), TI),
    Options1 = maps:merge(#{nonce => 0, code => Code, auth_fun => AuthFunHash,
                            call_data => Calldata}, maps:without([height], Opts)),
    AttachTx = aega_test_utils:ga_attach_tx(Owner, Options1),
    meta(Owner, AuthOpts, AttachTx, Opts, S).



meta(Owner, AuthOpts, InnerTx, Opts, S) ->
    Fail     = maps:get(fail, Opts, false),
    TxBin    = aec_governance:add_network_id(aetx:serialize_to_binary(InnerTx)),
    AuthData = make_authdata(AuthOpts, aec_hash:hash(tx, TxBin)),
    Options1 = maps:merge(#{auth_data => AuthData, tx => InnerTx}, AuthOpts),
    MetaTx   = aega_test_utils:ga_meta_tx(Owner, Options1),
    SMetaTx  = aetx_sign:new(MetaTx, []),
    Height   = maps:get(height, Opts, 1),
    S1       = case apply_transaction(SMetaTx, S, Height) of
                   {ok, TmpS} when not Fail       -> TmpS;
                   {ok,_TmpS} when Fail           -> error({error, succeeded});
                   {error, R,_TmpS} when not Fail -> error(R);
                   {error, R, TmpS} when Fail     -> throw({fail, R, TmpS})
               end,

    %% Getting here means authentication passed
    CallKey  = aec_hash:hash(pubkey, <<Owner/binary, AuthData/binary>>),
    CallTree = aect_test_utils:calls(S1),
    Call     = aect_call_state_tree:get_call(CallKey, CallTree),

    GasUsed = aect_call:gas_used(Call),
    AuthCost = aetx:fee(MetaTx) + aetx:gas_price(MetaTx) * GasUsed,
    Res0 = #{ auth_gas => GasUsed, auth_cost => AuthCost,
              tx_res => aect_call:return_type(Call) },

    Res =
        case aetx:specialize_type(InnerTx) of
            {spend_tx, _SpendTx} ->
                {ok, Res0#{ total_cost => AuthCost + aetx:fee(InnerTx) }};
            {contract_create_tx, _CCTx} ->
                ContractKey = aect_contracts:compute_contract_pubkey(Owner, CallKey),
                InitCall    = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
                {ok, Res0#{ ct_pubkey => ContractKey
                          , init_res  => aect_call:return_type(InitCall) }};
            {contract_call_tx, CCTx} ->
                ContractKey = aect_call_tx:contract_pubkey(CCTx),
                InnerCall   = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
                {ok, Res0#{ call_res => aect_call:return_type(InnerCall),
                            call_val => aect_call:return_value(InnerCall),
                            call_gas => aect_call:gas_used(InnerCall) }};
            {ga_attach_tx, GTx} ->
                {ok, Res0}
        end,
    {Res, S1}.

dry_run(ContractPK, Contract, Fun, Args, S) ->
    {DummyAcc, S1} = new_account(10000000 * aec_test_utils:min_gas_price(), S),
    DryData        = aega_test_utils:make_calldata(Contract, Fun, Args),
    DryNonce       = aect_test_utils:next_nonce(DummyAcc, S1),
    CallTx         = call_tx(DummyAcc, ContractPK, #{call_data => DryData, nonce => DryNonce}, S1),
    PrivKey        = aect_test_utils:priv_key(DummyAcc, S1),
    {ok, S2}       = sign_and_apply_transaction(CallTx, PrivKey, S1, 1),
    CallKey  = aect_call:id(DummyAcc, DryNonce, ContractPK),
    CallTree = aect_test_utils:calls(S2),
    Call     = aect_call_state_tree:get_call(ContractPK, CallKey, CallTree),
    {ok, Call}.

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

sign_and_apply_transaction(Tx, PrivKey, S1, Height) ->
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    apply_transaction(SignedTx, S1, Height).

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

get_contract_state(Contract) ->
    S = state(),
    {{value, C}, _} = lookup_contract_by_id(Contract, S),
    aect_contracts_store:contents(aect_contracts:state(C)).

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

lookup_contract_by_id(ContractKey, S) ->
    Contracts = aect_test_utils:contracts(S),
    X         = aect_state_tree:lookup_contract(ContractKey, Contracts),
    {X, S}.

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
        undefined -> ?LATEST_AESOPHIA;
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

to_hex_lit(Len, Bin) ->
    [_, _ | Xs] = binary_to_list(aeu_hex:hexstring_encode(Bin)),
    "#" ++
        if length(Xs) < Len * 2 ->
            lists:duplicate(Len * 2 - length(Xs), $0) ++ Xs;
           true ->
            Xs
        end.

hash_lit_to_bin("#" ++ Hex) ->
    if length(Hex) rem 2 == 1 ->
        aeu_hex:hexstring_decode(list_to_binary("0x0" ++ Hex));
       true ->
        aeu_hex:hexstring_decode(list_to_binary("0x" ++ Hex))
    end.

simple_auth(Args) ->
    aega_test_utils:make_calldata("simple_auth", "authorize", Args).

basic_auth(GA, Nonce, TxHash, S) ->
    {GACt, _}  = account_contract(GA, S),
    {contract, ContractPK} = aeser_id:specialize(GACt),
    {ok, Call} = dry_run(ContractPK, "basic_auth", "to_sign", [to_hex_lit(32, TxHash), Nonce], S),
    ok   = aect_call:return_type(Call),
    Val  = aect_call:return_value(Call),
    Hash = decode_call_result("basic_auth", "to_sign", ok, Val),

    GAPrivKey  = aect_test_utils:priv_key(GA, S),
    Sign = enacl:sign_detached(hash_lit_to_bin(Hash), GAPrivKey),

    {aega_test_utils:make_calldata("basic_auth", "authorize", [Nonce, to_hex_lit(64, Sign)]), S}.

