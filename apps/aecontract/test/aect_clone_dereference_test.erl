-module(aect_clone_dereference_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/aecontract.hrl").

dereference_test() ->
    T0 = aect_state_tree:empty(),

    Code0 = <<"0000000 - THIS IS NOT ACTUALLY PROPER BYTE CODE">>,
    Code1 = <<"1111111 - SOME OTHER NOT PROPER BYTE CODE">>,

    C0 = new_contract(#{code => Code0}),
    C1 = new_contract(#{code => Code1, nonce => 43}),
    C2 = clone_contract(C0, 44),

    T1 = aect_state_tree:insert_contract(C0, T0),
    T2 = aect_state_tree:insert_contract(C1, T1),
    T3 = aect_state_tree:insert_contract(C2, T2),
    ?assertMatch({C0, Code0}, aect_state_tree:get_contract_with_code(aect_contracts:pubkey(C0), T3)),
    ?assertMatch({C1, Code1}, aect_state_tree:get_contract_with_code(aect_contracts:pubkey(C1), T3)),
    ?assertMatch({C2, Code0}, aect_state_tree:get_contract_with_code(aect_contracts:pubkey(C2), T3)).

new_contract() ->
    new_contract(#{}).

new_contract(Override) ->
    Map = #{ owner_id    => aeser_id:create(account, <<4711:32/unit:8>>)
           , nonce       => 42
           , code        => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version  => ?VM_FATE_SOPHIA_2
           , abi_version => ?ABI_FATE_SOPHIA_1
           , fee         => 10
           , ttl         => 100
           , deposit     => 100
           , amount      => 50
           , gas         => 100
           , gas_price   => 5
           , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).

clone_contract(Contract, Nonce) ->
    aect_contracts:new_clone(
        <<"_______________k1_______________">>,
        Nonce,
        aect_contracts:ct_version(Contract),
        aect_contracts:pubkey(Contract),
        0
    ).
