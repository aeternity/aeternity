%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Before/after gas measurement for the v7 "Arcus" store repricing:
%%%    compares actual gas charged for a store-register read and a
%%%    store-map read at increasing value sizes, under Ceres (frozen, flat)
%%%    vs Arcus (live, floor + per-byte, charge-before-work).
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_store_repricing_proof_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

-define(OWNER_PUBKEY,    <<16#B1:256>>).
-define(CONTRACT_PUBKEY, <<16#C1:256>>).
-define(CALLER_PUBKEY,   <<16#CA:256>>).
-define(STORE_POS, 1).
-define(MAP_STORE_POS, 2).
-define(START_GAS, 100000000).
-define(SEED_GAS,  1000000000).

%% Entry/value sizes to sweep.
-define(SIZES, [0, 100, 1000, 10000]).

%% Sizes for the store-map scenario: all clear the production
%% ?STORE_MAP_THRESHOLD (100 bytes) so the value is a real ?FATE_STORE_MAP,
%% not inlined in the register.
-define(MAP_SIZES, [200, 1000, 10000]).

%%%===================================================================
%%% Store-register lookup gas, Ceres (flat 2000) vs Arcus (floor + per-byte)
%%%===================================================================

%% Store-register read: Ceres flat charge vs Arcus floor+per-byte.
store_read_repricing_test() ->
    Rows = [ measure_row(Size) || Size <- ?SIZES ],
    %% Ceres charge is the pre-existing flat 2000, size-independent.
    [ ?assertEqual(2000, GasCeres) || {_Size, GasCeres, _GasArcus, _Bytes} <- Rows ],
    %% Arcus: floor + byte-proportional.
    [ ?assertEqual(aec_governance:store_read_base_gas() + Bytes * aec_governance:store_read_byte_gas(), GasArcus)
      || {_Size, _GasCeres, GasArcus, Bytes} <- Rows ],
    GasArcusSeq = [ GasArcus || {_, _, GasArcus, _} <- Rows ],
    ?assert(is_strictly_increasing(GasArcusSeq)),
    %% Floor applies even at size 0 (no free reads).
    {0, _, GasArcusAtZero, BytesAtZero} = hd(Rows),
    ?assertEqual(aec_governance:store_read_base_gas() + BytesAtZero * aec_governance:store_read_byte_gas(),
                 GasArcusAtZero),
    {_, CeresAtMax, ArcusAtMax, _} = lists:last(Rows),
    ?assert(ArcusAtMax > CeresAtMax * 10).

%%%===================================================================
%%% Store-map lookup gas, Ceres (flat 5000) vs Arcus (floor + per-byte)
%%%===================================================================

%% Store-map read: Ceres flat charge vs Arcus floor+per-byte.
store_map_read_repricing_test() ->
    Rows = [ measure_map_row(Size) || Size <- ?MAP_SIZES ],
    %% Ceres charge is the pre-existing flat 5000, size-independent.
    [ ?assertEqual(5000, GasCeres) || {_Size, GasCeres, _GasArcus, _Bytes} <- Rows ],
    %% Arcus adds floor + byte-proportional on top of the flat 5000.
    [ ?assertEqual(5000 + aec_governance:store_read_base_gas()
                        + Bytes * aec_governance:store_read_byte_gas(),
                    GasArcus)
      || {_Size, _GasCeres, GasArcus, Bytes} <- Rows ],
    GasArcusSeq = [ GasArcus || {_, _, GasArcus, _} <- Rows ],
    ?assert(is_strictly_increasing(GasArcusSeq)),
    {_, CeresAtMax, ArcusAtMax, _} = lists:last(Rows),
    ?assert(ArcusAtMax > CeresAtMax * 2).

is_strictly_increasing([_]) -> true;
is_strictly_increasing([A, B | Rest]) -> A < B andalso is_strictly_increasing([B | Rest]).

%%%===================================================================
%%% Scenario (1): store-register lookup
%%%===================================================================

%% {Size, GasCeres, GasArcus, RawSerializedBytes}
measure_row(Size) ->
    Value = aeb_fate_data:make_string(binary:copy(<<$a>>, Size)),
    Bytes = byte_size(aeb_fate_encoding:serialize(Value)),
    GasCeres = measure_lookup_gas(?CERES_PROTOCOL_VSN, Value),
    GasArcus = measure_lookup_gas(?ARCUS_PROTOCOL_VSN, Value),
    {Size, GasCeres, GasArcus, Bytes}.

%% Round-trips Value through a real finalize/3 so the read is a genuine
%% cache-miss, then returns the gas charged for one register read.
measure_lookup_gas(Protocol, Value) ->
    API1 = seed_on_chain_store(Protocol, Value),
    ES0 = fresh_reader_engine_state(Protocol, API1),
    GasBefore = aefa_engine_state:gas(ES0),
    {_Val, ES1} = aefa_fate:lookup_var({var, -?STORE_POS}, ES0),
    GasAfter = aefa_engine_state:gas(ES1),
    GasBefore - GasAfter.

seed_on_chain_store(Protocol, Value) ->
    ChainApi = fresh_chain_api(Protocol),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    Stores2 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?STORE_POS, Value, Stores1),
    Aefa_stores = aefa_engine_state:aefa_stores_for_protocol(Protocol),
    {ok, API1, _GasLeft} = Aefa_stores:finalize(ChainApi, ?SEED_GAS, Stores2),
    API1.

%%%===================================================================
%%% Scenario (2): store-map lookup (the incident shape, C2)
%%%===================================================================

%% {Size, GasCeres, GasArcus, RawSerializedValueBytes}
measure_map_row(Size) ->
    Value = aeb_fate_data:make_string(binary:copy(<<$a>>, Size)),
    Bytes = byte_size(aeb_fate_encoding:serialize(Value)),
    GasCeres = measure_map_lookup_gas(?CERES_PROTOCOL_VSN, Value),
    GasArcus = measure_map_lookup_gas(?ARCUS_PROTOCOL_VSN, Value),
    {Size, GasCeres, GasArcus, Bytes}.

%% Seeds a one-entry store map, then performs one real MAP_LOOKUP op and
%% returns the gas charged.
measure_map_lookup_gas(Protocol, Value) ->
    API1 = seed_on_chain_store_with_map(Protocol, Value),
    ES0 = fresh_reader_engine_state(Protocol, API1),
    {MapRegVal, ES1} = aefa_fate:lookup_var({var, -?MAP_STORE_POS}, ES0),
    ?FATE_STORE_MAP(_Cache, _Id) = MapRegVal,  %% sanity: really a store map, not inlined
    GasBefore = aefa_engine_state:gas(ES1),
    %% map_lookup/4 writes its result into Arg0; read it back to confirm a hit.
    ES2 = aefa_fate_op:map_lookup({var, 0}, {immediate, MapRegVal}, {immediate, map_key()}, ES1),
    {val, Value} = maps:get({var, 0}, aefa_engine_state:memory(ES2)),
    GasAfter = aefa_engine_state:gas(ES2),
    GasBefore - GasAfter.

seed_on_chain_store_with_map(Protocol, Value) ->
    ChainApi = fresh_chain_api(Protocol),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    MapVal = aeb_fate_data:make_map(#{ map_key() => Value }),
    Stores2 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, MapVal, Stores1),
    Aefa_stores = aefa_engine_state:aefa_stores_for_protocol(Protocol),
    {ok, API1, _GasLeft} = Aefa_stores:finalize(ChainApi, ?SEED_GAS, Stores2),
    API1.

map_key() -> aeb_fate_data:make_string(<<"k">>).

%%%===================================================================
%%% Shared helpers
%%%===================================================================

fresh_reader_engine_state(Protocol, API1) ->
    ES = aefa_engine_state:new(?START_GAS, _Value = 0,
                                #{caller => ?CALLER_PUBKEY},
                                aefa_stores:new(), API1, #{}, ?VM_FATE_SOPHIA_2),
    ES1 = aefa_engine_state:set_current_contract(?CONTRACT_PUBKEY, ES),
    %% Sanity: tx_env must resolve to the intended Protocol.
    Protocol = aefa_engine_state:consensus_version(ES1),
    ES1.

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
%%% Dry-run DoS ceiling + preview correctness (always-on Arcus metering)
%%%
%%% The public dry-run endpoint caps a call at ?DEFAULT_GAS_LIMIT gas
%%% (?DRY_RUN_GAS_CEILING below). With store reads metered at the Arcus cost
%%% (floor + per-byte, charge-before-work), a large-store read burns that
%%% budget and aborts out_of_gas BEFORE deserializing -- whereas the pre-Arcus
%%% flat charge (2000) waved the same read through for a token cost. Dry-run
%%% forces this Arcus metering by default (see aec_dry_run and its tests).
%%%
%%% Gas is charged before work, so raw garbage bytes exercise the *charge*
%%% deterministically without needing a valid serialization/finalize cycle.
%%%===================================================================

%% aehttp_dispatch_ext's ?DEFAULT_GAS_LIMIT.
-define(DRY_RUN_GAS_CEILING, 6000000).

%% DoS-hardening: a ~700 KB store read costs more than the whole dry-run
%% budget at Arcus, so it aborts out_of_gas instead of deserializing.
dry_run_big_store_read_hits_gas_ceiling_test() ->
    Size = 700000,
    Stores = seed_garbage_register(<<16#D3:256>>, Size),
    ?assertEqual({error, out_of_gas},
                 aefa_stores:find_value(<<16#D3:256>>, 1, Stores, ?DRY_RUN_GAS_CEILING)).

%% Preview correctness: the same read that the flat Ceres 2000 waved through is
%% refused at 2000 under Arcus, and passes the gas gate once given ample gas
%% (garbage bytes then crash in deserialize, which is how we know it got there).
dry_run_preview_reprices_store_read_higher_test() ->
    Size = 10000,
    Stores = seed_garbage_register(<<16#D4:256>>, Size),
    ?assertEqual({error, out_of_gas},
                 aefa_stores:find_value(<<16#D4:256>>, 1, Stores, 2000)),
    Reached = try {ok, aefa_stores:find_value(<<16#D4:256>>, 1, Stores, 1000000)}
              catch C:E -> {crash, C, E} end,
    ?assertMatch({crash, _, _}, Reached).

%% Plants Size raw bytes at register 1 via the low-level store API (charge-
%% before-work means find_value/4 charges by byte length before deserializing,
%% so the bytes need not be valid FATE).
seed_garbage_register(Pubkey, Size) ->
    Garbage = binary:copy(<<255>>, Size),
    RawStore0 = aefa_stores:initial_contract_store(),
    Key = <<0, (binary:encode_unsigned(1))/binary>>,
    RawStore1 = aect_contracts_store:put(Key, Garbage, RawStore0),
    aefa_stores:put_contract_store(Pubkey, RawStore1, aefa_stores:new()).
