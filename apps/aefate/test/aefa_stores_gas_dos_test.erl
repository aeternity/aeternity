%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Adversarial/DoS regression tests for the v7 "Salus" FATE store gas
%%%    repricing (aefa_stores.erl): charge-before-work on an under-provisioned
%%%    register read, proportional gc-subtree-read charging, and
%%%    deterministic fuel-exhaustion signaling on both reuse-fixpoint paths.
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_stores_gas_dos_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

-define(OWNER_PUBKEY,    <<16#B2:256>>).
-define(CONTRACT_PUBKEY, <<16#C2:256>>).
-define(CALLER_PUBKEY,   <<16#CB:256>>).
-define(MAP_STORE_POS, 3).
-define(SEED_GAS, 1000000000).

%%%===================================================================
%%% (a) charge-before-work: adversarial insufficient gas
%%%===================================================================

%% Under-provisioned read on invalid FATE bytes: out_of_gas, never a crash.
adversarial_insufficient_gas_never_deserializes_test() ->
    Stores0 = seed_garbage_register(<<16#DD:256>>, 5000),
    %% Enough for the floor alone, nowhere near floor + 5000*rate.
    SmallGas = aec_governance:store_read_base_gas() + 5,
    ?assertEqual({error, out_of_gas},
                 aefa_stores:find_value(<<16#DD:256>>, 1, Stores0, SmallGas)).

%% Control: same garbage bytes DO crash deserialize when actually reached.
control_sufficient_gas_reaches_deserialize_and_crashes_on_garbage_test() ->
    Pubkey = <<16#DE:256>>,
    Stores0 = seed_garbage_register(Pubkey, 5000),
    BigGas = 1000000000,
    Result = try {ok, aefa_stores:find_value(Pubkey, 1, Stores0, BigGas)}
             catch C:E -> {crash, C, E}
             end,
    ?assertMatch({crash, _, _}, Result).

seed_garbage_register(Pubkey, Size) ->
    Garbage = binary:copy(<<255>>, Size),
    %% Written directly via the raw store API to plant invalid FATE bytes
    %% (put_value/4 would insist on a real, correctly-serialized fate_val()).
    RawStore0 = aefa_stores:initial_contract_store(),
    Key = <<0, (binary:encode_unsigned(1))/binary>>,
    RawStore1 = aect_contracts_store:put(Key, Garbage, RawStore0),
    aefa_stores:put_contract_store(Pubkey, RawStore1, aefa_stores:new()).

%%%===================================================================
%%% (b) real cache-miss subtree read (GC path), gas proportional to bytes
%%%===================================================================

%% gc_refcounts/4's subtree-read charge is proportional to marginal bytes.
sec_gas_2_gc_subtree_read_charged_proportional_to_bytes_test() ->
    %% All sizes clear the production ?STORE_MAP_THRESHOLD (100 bytes) so
    %% the value is genuinely allocated as a store map, not inlined.
    Sizes = [200, 1000, 10000],
    Measurements = [ {Size, gc_round_trip_gas(Size)} || Size <- Sizes ],
    [{S0, G0}, {S1, G1}, {S2, G2}] = Measurements,
    V0 = value_bytes(S0),
    V1 = value_bytes(S1),
    V2 = value_bytes(S2),
    Rate = aec_governance:store_read_byte_gas(),
    %% The other gas costs in the round-trip are constant regardless of the
    %% map value's size, so they cancel out of a marginal comparison.
    ?assertEqual((V1 - V0) * Rate, G1 - G0),
    ?assertEqual((V2 - V1) * Rate, G2 - G1),
    ?assert(G2 > G1 andalso G1 > G0).

value_bytes(Size) ->
    byte_size(aeb_fate_encoding:serialize(aeb_fate_data:make_string(binary:copy(<<$a>>, Size)))).

%% Round 1: persist a one-entry store map with a Size-byte string value.
%% Round 2: drop the only reference to it, forcing a real GC subtree read
%% via gc_refcounts/4. Returns the total gas consumed by round 2's finalize/3.
gc_round_trip_gas(Size) ->
    Value = aeb_fate_data:make_string(binary:copy(<<$a>>, Size)),
    Protocol = ?SALUS_PROTOCOL_VSN,
    ChainApi0 = fresh_chain_api(Protocol),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    MapVal = aeb_fate_data:make_map(#{ 1 => Value }),
    Stores2 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, MapVal, Stores1),
    {ok, ChainApi1, _} = aefa_stores:finalize(ChainApi0, ?SEED_GAS, Stores2),

    %% Sanity: confirm the map was genuinely promoted to a store map.
    Stores2Check = aefa_stores:put_contract_store(?CONTRACT_PUBKEY,
                       element(1, aefa_chain_api:contract_store(?CONTRACT_PUBKEY, ChainApi1)),
                       aefa_stores:new()),
    {ok, RegVal, _, _, _} = aefa_stores:find_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, Stores2Check, 1000000000),
    ?FATE_STORE_MAP(_, _) = RegVal,

    {OnChainStore, ChainApi2} = aefa_chain_api:contract_store(?CONTRACT_PUBKEY, ChainApi1),
    Stores3 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY, OnChainStore, aefa_stores:new()),
    %% Overwrite with a constant-size value, unrelated to the map's size.
    Stores4 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, 0, Stores3),
    {ok, _ChainApi3, GasLeft2} = aefa_stores:finalize(ChainApi2, ?SEED_GAS, Stores4),
    ?SEED_GAS - GasLeft2.

fresh_chain_api(Protocol) ->
    Trees = trees_with_one_contract(),
    TxEnv = aetx_env:tx_env(_Height = 1, Protocol),
    aefa_chain_api:new(#{ gas_price => 1
                         , fee        => 0
                         , origin     => ?CALLER_PUBKEY
                         , trees      => Trees
                         , tx_env     => TxEnv
                         }).

trees_with_one_contract() ->
    CTVersion = #{vm => ?VM_FATE_SOPHIA_2, abi => ?ABI_FATE_SOPHIA_1},
    Contract0 = aect_contracts:new(?OWNER_PUBKEY, _Nonce = 1, CTVersion,
                                    _Code = <<"unused-in-this-test">>, _Deposit = 0),
    Contract1 = aect_contracts:set_pubkey(?CONTRACT_PUBKEY, Contract0),
    Account = aec_accounts:new(?CONTRACT_PUBKEY, 0),
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = aec_trees:set_contracts(Trees0,
                aect_state_tree:insert_contract(Contract1, aec_trees:contracts(Trees0))),
    aec_trees:set_accounts(Trees1,
                aec_accounts_trees:enter(Account, aec_trees:accounts(Trees1))).

%%%===================================================================
%%% (c) both fuel sites signal deterministically
%%%===================================================================

%% optimistic_reuse_fixpoint/6 returns bare `out_of_fuel`, never a coerced
%% converged-looking result.
fuel_optimistic_path_signals_out_of_fuel_not_silent_coercion_test() ->
    Store = aect_contracts_store:new(),
    ?assertEqual(out_of_fuel,
                 aefa_stores:optimistic_reuse_fixpoint(#{}, #{}, #{}, Store, 0, 1000000)).

%% full_reuse_fixpoint/6 throws out_of_gas deterministically on exhaustion.
fuel_full_path_throws_deterministic_out_of_gas_test() ->
    Store = aect_contracts_store:new(),
    ?assertThrow(out_of_gas,
                 aefa_stores:full_reuse_fixpoint(#{}, #{}, #{}, Store, 0, {#{}, 1000000})).

%% Shared fuel-countdown mechanism: Fuel = 0 fires before RefCountFun runs.
fuel_reuse_fixpoint_loop_mechanism_test() ->
    Store = aect_contracts_store:new(),
    FakeRefCountFun = fun(_Meta, _Reuse, _Maps, _Store, Acc) -> {#{}, Acc + 1} end,
    ?assertEqual({out_of_fuel, #{}, 0},
                 aefa_stores:reuse_fixpoint_loop(FakeRefCountFun, #{}, #{}, #{}, Store, 0, 0)).

%% Ordinary (non-exhausted) fallthrough to full_reuse_fixpoint still works.
compute_reuse_fixpoint_falls_through_to_full_path_when_optimistic_would_not_reuse_all_test() ->
    Store = aect_contracts_store:new(),
    Result = aefa_stores:compute_reuse_fixpoint(#{}, #{}, Store, 1000000),
    ?assertMatch({_Unused, _Reuse, _Metadata, _GasLeft}, Result).
