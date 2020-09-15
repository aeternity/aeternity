-module(aehc_contract_tests).


-include("../../aecontract/src/aect_sophia.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(cid(__x__), {'@ct', __x__}).
-define(hsh(__x__), {'#', __x__}).
-define(sig(__x__), {'$sg', __x__}).
-define(oid(__x__), {'@ok', __x__}).
-define(qid(__x__), {'@oq', __x__}).


%% -export([groups/0]).

-export([ sample_test_/0
        , scheme_test_/0
        ]).

%% groups() ->
%%     [ {staking, [], sample_test}
%%     ].

sample_test_() ->
    fun sample/0.

init_test_int() ->
    application:ensure_all_started(gproc),
    aec_test_utils:mock_genesis_and_forks(),
    ok = lager:start(),
    aec_keys:start_link(),
    {ok, _} = aec_chain_sim:start(),
    ok.

finish_test_int() ->
    aec_chain_sim:stop().


sample() ->
    init_test_int(),

    Payload = <<>>,
    {ok, Pub} = aec_keys:pubkey(),
    {ok, PrivKey} = aec_keys:sign_privkey(),
    SenderId = aeser_id:create(account, Pub),
    {ok, Tx} = aec_spend_tx:new(#{ sender_id => SenderId, recipient_id => SenderId, amount => 1,
                                   fee => 5, nonce => 1, payload => Payload, ttl => 0 }),
    BinaryTx = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    SignedTx = aetx_sign:new(Tx, [enacl:sign_detached(BinaryTx, PrivKey)]),
    TxHash = aetx_sign:hash(SignedTx),
    %% The next format is prepared accordingly to simualtor internal representation;
    %% Res = aec_chain_sim:push(#{ tx_hash => TxHash, signed_tx  => SignedTx }),

    ok.

new_account() ->
    enacl:sign_keypair().

make_calldata_from_code(Fun, Args) when is_atom(Fun) ->
    make_calldata_from_code(atom_to_binary(Fun, latin1), Args);
make_calldata_from_code(Fun, Args) when is_binary(Fun) ->
    Args1 = format_fate_args(if is_tuple(Args) -> Args;
                                true -> {Args}
                             end),
    FunctionId = make_fate_function_id(Fun),
    aeb_fate_encoding:serialize(aefate_test_utils:encode({FunctionId, Args1})).

make_fate_function_id(FunctionName) when is_binary(FunctionName) ->
    aeb_fate_code:symbol_identifier(FunctionName).

format_fate_args(?cid(B)) ->
    {contract, B};
format_fate_args(?hsh(B)) ->
    {bytes, B};
format_fate_args(?sig(B)) ->
    {bytes, B};
format_fate_args(?oid(B)) ->
    {oracle, B};
format_fate_args(?qid(B)) ->
    {oracle_query, B};
format_fate_args(<<_:256>> = B) ->
    {address, B}; %% Assume it is an address
format_fate_args({bytes, B}) ->
    {bytes, B};
format_fate_args([H|T]) ->
    [format_fate_args(H) | format_fate_args(T)];
format_fate_args(T) when is_tuple(T) ->
    list_to_tuple(format_fate_args(tuple_to_list(T)));
format_fate_args(M) when is_map(M) ->
    maps:from_list(format_fate_args(maps:to_list(M)));
format_fate_args(X) ->
    X.


init_staking_contract(D, R, W) ->
    {ok, PK} = aec_chain_sim:new_account(10),
    {ok, Code} = aeso_compiler:file("apps/aehyperchains/test/contracts/SimpleElection.aes", [{backend, fate}]),
    Serialized = aect_sophia:serialize(Code, ?SOPHIA_CONTRACT_VSN_3),
    CallData = make_calldata_from_code(init, [{D, R, W}]),
    Nonce = aec_chain_sim:next_nonce(PK),
    {ok, Tx} = aect_create_tx:new(
                 #{ fee        => 1000000 * aec_test_utils:min_gas_price()
                 , owner_id    => aeser_id:create(account, PK)
                 , nonce       => Nonce
                 , vm_version  => ?VM_FATE_SOPHIA_1
                 , abi_version => ?ABI_FATE_SOPHIA_1
                 , deposit     => 2137
                 , amount      => 0
                 , gas         => 1000000000
                 , gas_price   => 1 * aec_test_utils:min_gas_price()
                 , ttl         => 0
                 , code        => Serialized
                 , call_data   => CallData
                 }),
    aec_chain_sim:add_keyblock(),
    aec_chain_sim:sign_and_push(PK, Tx),
    aec_chain_sim:add_microblock().

scheme_test_() ->
    fun scheme/0.

scheme() ->
    init_staking_contract(1, 1, 2),
    %% Acc = new_account(100000),
    %% deposit_stake(Acc, 100),
    %% generation_pass(),
    %% request_withdraw(Acc, 50),
    %% generation_pass(),
    %% W = withdraw_stake(Acc),
    %% ?assert_equal(W, 10).
    ok.

