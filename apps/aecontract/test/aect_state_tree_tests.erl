-module(aect_state_tree_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TESTED_MODULE, aect_state_tree).

trunc_test() ->
    T0 = ?TESTED_MODULE:empty(),

    Owner1 = aec_id:create(account, <<"_______________k1_______________">>),
    Owner2 = aec_id:create(account, <<"_______________k2_______________">>),

    StEmpty = #{},
    St1 = #{<<"1">> => <<"a">>},
    St2 = #{<<"2">> => <<"b">>},

    C0 = new_contract(),
    C1 = new_contract(#{owner => Owner1}),
    C2 = new_contract(#{owner => Owner2}),

    T1  = ?TESTED_MODULE:insert_contract(C0, T0),
    T2  = ?TESTED_MODULE:insert_contract(C1, T1),
    T30 = ?TESTED_MODULE:insert_contract(C2, T2),

    % state changes do not leave artifacts: empty state
    T31 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St1, C1), T30),
    T300 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(StEmpty, C1), T31),
    assert_root_hashes(T30, T300),

    % state changes do not leave artifacts: non empty state
    T320 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St2, C1), T31),
    T321 = ?TESTED_MODULE:enter_contract(aect_contracts:set_state(St2, C1), T30),
    assert_root_hashes(T320, T321).

new_contract() ->
    new_contract(#{}).

new_contract(Override) ->
    Map = #{ owner      => aec_id:create(account, <<4711:32/unit:8>>)
           , nonce      => 42
           , code       => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version => 2
           , fee        => 10
           , ttl        => 100
           , deposit    => 100
           , amount     => 50
           , gas        => 100
           , gas_price  => 5
           , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).

assert_root_hashes(Tree1, Tree2) ->
    {ok, T1Hash} = ?TESTED_MODULE:root_hash(Tree1),
    {ok, T2Hash} = ?TESTED_MODULE:root_hash(Tree2),
    ?assertEqual(T1Hash, T2Hash).
