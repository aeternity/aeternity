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
-define(ALOT, 1000000000000000000000000000000000000000000000000000000000000000).


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

make_calldata_from_code(Fun, Args) when is_atom(Fun) ->
    make_calldata_from_code(atom_to_binary(Fun, latin1), Args);
make_calldata_from_code(Fun, Args) when is_binary(Fun) ->
    Args1 = format_fate_args(if is_tuple(Args) -> Args;
                                is_list(Args) -> list_to_tuple(Args);
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

new_account(Balance) ->
    {ok, Acct} = aec_chain_sim:new_account(Balance),
    Acct.

restricted_account() ->
    case aec_chain_sim:dict_get(restricted_account, undefined) of
        undefined ->
            Restricted = new_account(?ALOT),
            aec_chain_sim:dict_set(restricted_account, Restricted),
            Restricted;
        Restricted -> Restricted
    end.

staking_contract() ->
    case aec_chain_sim:dict_get(staking_contract, undefined) of
        undefined ->
            error("Staking contract is undefined");
        Con -> Con
    end.

create_staking_contract(#{ deposit_delay := D
                        ,  stake_retraction_delay := R
                        ,  withdraw_delay := W
                        }) ->
    Restricted = restricted_account(),
    {ok, Code} = aeso_compiler:file("apps/aehyperchains/test/contracts/SimpleElection.aes", [{backend, fate}]),
    Serialized = aect_sophia:serialize(Code, ?SOPHIA_CONTRACT_VSN_3),
    CallData = make_calldata_from_code(init, [{D, R, W}]),
    Nonce = aec_chain_sim:next_nonce(Restricted),
    {ok, Tx} = aect_create_tx:new(
                 #{ fee         => 1000000 * aec_test_utils:min_gas_price()
                 ,  owner_id    => aeser_id:create(account, Restricted)
                 ,  nonce       => Nonce
                 ,  vm_version  => ?VM_FATE_SOPHIA_1
                 ,  abi_version => ?ABI_FATE_SOPHIA_1
                 ,  deposit     => 2137
                 ,  amount      => 0
                 ,  gas         => 1000000000
                 ,  gas_price   => 1 * aec_test_utils:min_gas_price()
                 ,  ttl         => 0
                 ,  code        => Serialized
                 ,  call_data   => CallData
                 }),
    aec_chain_sim:sign_and_push(Restricted, Tx),
    Contract = aect_contracts:compute_contract_pubkey(Restricted, Nonce),
    aec_chain_sim:dict_set(staking_contract, Contract).


call_staking_contract(Fun, Args) ->
    call_staking_contract(restricted_account(), Fun, Args).
call_staking_contract(Acct, Fun, Args) ->
    call_staking_contract(Acct, 0, Fun, Args).
call_staking_contract(Acct, Value, Fun, Args) ->
    CallData = make_calldata_from_code(Fun, Args),
    Nonce = aec_chain_sim:next_nonce(Acct),
    {ok, Tx} = aect_call_tx:new(
               #{ caller_id   => aeser_id:create(account, Acct)
               ,  nonce       => Nonce
               ,  contract_id => aeser_id:create(contract, staking_contract())
               ,  abi_version => ?ABI_FATE_SOPHIA_1
               ,  fee         => 1000000 * aec_test_utils:min_gas_price()
               ,  amount      => Value
               ,  gas         => 1000000000
               ,  gas_price   => 1 * aec_test_utils:min_gas_price()
               ,  call_data   => CallData
               }),
    aec_chain_sim:sign_and_push(Acct, Tx).

random() ->
    4. %% FIXME chosen by a dice toss, perfectly random


deposit_stake(Acct, Amount) ->
    call_staking_contract(Acct, Amount, deposit_stake, []).

request_withdraw(Acct, Amount) ->
    call_staking_contract(Acct, request_withdraw, [Amount]).

withdraw(Acct) ->
    call_staking_contract(Acct, withdraw, []).

get_computed_leader() ->
    call_staking_contract(get_computed_leader, []).

get_leader(Delegates, Rand) ->
    call_staking_contract(get_leader, [Delegates, Rand]). %% FIXME

punish(BadGuy) ->
    call_staking_contract(punish, [BadGuy]).


add_microblock() ->
    aec_chain_sim:add_microblock().

add_keyblock(Delegates) ->
    aec_chain_sim:add_keyblock(),
    Leader = get_leader(Delegates, random()),
    aec_chain_sim:add_microblock(),
    Leader.

scheme_test_() ->
    fun scheme/0.

scheme() ->
    create_staking_contract(#{ deposit_delay => 1
                            ,  stake_retraction_delay => 1
                            ,  withdraw_delay => 2
                            }),
    aec_chain_sim:add_microblock(),
    aec_chain_sim:add_keyblock(),
    add_microblock(),
    add_keyblock([]),
    Acct = new_account(?ALOT),
    deposit_stake(Acct, 100000),
    add_microblock(),
    request_withdraw(Acct, 100000),
    add_microblock(),
    X1 = aec_chain_sim:get_balance(Acct),
    withdraw(Acct),
    add_microblock(),
    X2 = aec_chain_sim:get_balance(Acct),
    %% ?assertEqual(X1, X2 + 100000),
    ok.

