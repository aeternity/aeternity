%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Regression tests for the v7 "Arcus" FATE store read gas accounting
%%%    (aefa_stores.erl): charge-before-work on register reads,
%%%    proportional gc-subtree-read charging, and deterministic
%%%    fuel-exhaustion signaling on both reuse-fixpoint paths.
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_stores_read_gas_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

-define(OWNER_PUBKEY,    <<16#B2:256>>).
-define(CONTRACT_PUBKEY, <<16#C2:256>>).
-define(CALLER_PUBKEY,   <<16#CB:256>>).
-define(MAP_STORE_POS, 3).
-define(ALIAS_STORE_POS, 4).
-define(SEED_GAS, 1000000000).

%%%===================================================================
%%% (a) charge-before-work: insufficient gas
%%%===================================================================

%% Under-provisioned read on invalid FATE bytes: out_of_gas, never a crash.
insufficient_gas_never_deserializes_test() ->
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
gc_subtree_read_charged_proportional_to_bytes_test() ->
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
    Protocol = ?ARCUS_PROTOCOL_VSN,
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

%% Re-enter through the chain api's own trees, so a later finalize/3 reads an
%% MPT-backed store with an empty write cache -- the shape a real call gets from
%% aect_state_tree:add_store/3. Reading straight back out of ChainApi1 returns the
%% primop cache instead, and the bounded walk is then never entered.
committed(ChainApi) ->
    aefa_chain_api:new(#{ gas_price => 1
                        , fee        => 0
                        , origin     => ?CALLER_PUBKEY
                        , trees      => aefa_chain_api:final_trees(ChainApi)
                        , tx_env     => aetx_env:tx_env(_Height = 1, ?ARCUS_PROTOCOL_VSN)
                        }).

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

%% optimistic_reuse_fixpoint/6 returns `{out_of_fuel, GasLeft}`, carrying the
%% gas already spent by completed iterations forward to the full-path
%% fallback -- never a bare atom that would let that gas be re-spent for free.
fuel_optimistic_path_signals_out_of_fuel_not_silent_coercion_test() ->
    Store = aect_contracts_store:new(),
    ?assertEqual({out_of_fuel, 1000000},
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

%%%===================================================================
%%% (d) store_map_lookup/store_map_member miss paths: charged too, not free
%%%===================================================================

%% A miss still costs the read floor -- only the hit path adds byte-proportional
%% cost on top, but "found nothing" is not "did no work".
store_map_lookup_miss_charges_floor_test() ->
    {MapId, Store} = seed_store_map_with_one_entry(),
    GasBefore = 1000000,
    {miss, _Store1, GasAfter} =
        aefa_stores:store_map_lookup(?CONTRACT_PUBKEY, MapId, absent_key(), Store, GasBefore),
    ?assertEqual(aec_governance:store_read_base_gas(), GasBefore - GasAfter).

%% Charge-before-work applies to misses too: too little gas for the floor
%% alone is out_of_gas, not a "cheap" miss result.
store_map_lookup_miss_out_of_gas_test() ->
    {MapId, Store} = seed_store_map_with_one_entry(),
    TooLittle = aec_governance:store_read_base_gas() - 1,
    ?assertEqual({error, out_of_gas},
                 aefa_stores:store_map_lookup(?CONTRACT_PUBKEY, MapId, absent_key(), Store, TooLittle)).

store_map_member_miss_charges_floor_test() ->
    {MapId, Store} = seed_store_map_with_one_entry(),
    GasBefore = 1000000,
    {false, _Store1, GasAfter} =
        aefa_stores:store_map_member(?CONTRACT_PUBKEY, MapId, absent_key(), Store, GasBefore),
    ?assertEqual(aec_governance:store_read_base_gas(), GasBefore - GasAfter).

store_map_member_miss_out_of_gas_test() ->
    {MapId, Store} = seed_store_map_with_one_entry(),
    TooLittle = aec_governance:store_read_base_gas() - 1,
    ?assertEqual({error, out_of_gas},
                 aefa_stores:store_map_member(?CONTRACT_PUBKEY, MapId, absent_key(), Store, TooLittle)).

absent_key() -> aeb_fate_data:make_string(<<"missing">>).

%%%===================================================================
%%% (e) in-place update_map: its per-key store reads are charged too
%%%===================================================================

%% Charged single-key reads of an entry that update_map/4 overwrites. Each
%% is a real aect_contracts_store:get/2 on map_raw_key(RawId, Key):
%%   compute_reuse_only_refcounts/5   the optimistic reuse fixpoint
%%   compute_copy_refcounts/5         the same subtraction, re-run on the
%%                                    reuse branch once the fixpoint converged
%%   size_delta/4                     update_map/4's membership check
%% update_map/4's nested-refcount fold reads at a different key, so its
%% charge is a flat floor rather than a byte-proportional term and cancels
%% out of the marginal comparison below.
-define(UPDATE_MAP_BYTE_CHARGED_READS, 3).

%% Overwriting one entry of a store map in place performs the reads above.
%% Vary only the byte size of the value being overwritten: every other cost
%% in the round-trip -- register write, new value written, metadata, the
%% flat read floors -- is identical across the runs, so it cancels and the
%% marginal gas is exactly the charged reads times the per-byte rate.
update_map_overwrite_read_charged_proportional_to_bytes_test() ->
    Sizes = [200, 1000, 10000],
    [{S0, G0}, {S1, G1}, {S2, G2}] =
        [ {Size, inplace_overwrite_gas(Size)} || Size <- Sizes ],
    V0 = value_bytes(S0),
    V1 = value_bytes(S1),
    V2 = value_bytes(S2),
    Rate = aec_governance:store_read_byte_gas(),
    ?assertEqual(?UPDATE_MAP_BYTE_CHARGED_READS * Rate * (V1 - V0), G1 - G0),
    ?assertEqual(?UPDATE_MAP_BYTE_CHARGED_READS * Rate * (V2 - V1), G2 - G1).

%% Read floors charged per key inserted into an existing store map. The key
%% is new, so each of the three sites above charges its floor rather than a
%% byte-proportional cost, and update_map/4's nested-refcount fold charges a
%% fourth floor here where the byte-proportional test sees none.
-define(UPDATE_MAP_INSERT_FLOOR_READS, 4).

%% The finalize-time fixpoint sites' floors used to be free: find_in_store/3
%% deliberately leaves a miss uncharged and lets the caller decide, and the
%% two fixpoint callers took `error -> {Acc, GasA}`. Inserting fresh keys
%% into a store map updated in place is the shape that charges all four
%% floors. Vary only the number of inserted keys, holding each key's and
%% value's serialized size constant and both resulting map sizes below 64 so
%% the metadata term serializes to the same length either way.
update_map_insert_charges_read_floor_test() ->
    Base     = aec_governance:store_read_base_gas(),
    ByteGas  = aec_governance:store_byte_gas(),
    {N1, N2} = {10, 20},
    G1 = inplace_insert_gas(N1),
    G2 = inplace_insert_gas(N2),
    PerKey = ByteGas * (byte_size(insert_key_bin(0)) + byte_size(insert_val_bin()))
           + ?UPDATE_MAP_INSERT_FLOOR_READS * Base,
    ?assertEqual((N2 - N1) * PerKey, G2 - G1).

%% A read is charged before the work it guards, so an in-place update that
%% cannot pay for its per-key reads is out_of_gas rather than a cheap write.
update_map_insert_out_of_gas_test() ->
    {MapId, Stores, ChainApi} = seed_store_map(#{overwrite_key() => big_value(200)}),
    Update = ?FATE_STORE_MAP(insert_cache(1), MapId),
    Stores1 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, Update, Stores),
    TooLittle = aec_governance:store_read_base_gas() - 1,
    ?assertEqual({error, out_of_gas},
                 aefa_stores:finalize(ChainApi, TooLittle, Stores1)).

%%% -- (e) helpers ---------------------------------------------------------

overwrite_key() -> aeb_fate_data:make_string(<<"k">>).

big_value(Size) -> aeb_fate_data:make_string(binary:copy(<<$a>>, Size)).

%% Fixed-width so every inserted entry writes the same number of bytes.
insert_key(I) ->
    aeb_fate_data:make_string(list_to_binary(io_lib:format("i~4..0b", [I]))).

insert_key_bin(I) -> aeb_fate_encoding:serialize(insert_key(I)).

insert_val() -> aeb_fate_data:make_string(<<"v">>).

insert_val_bin() -> aeb_fate_encoding:serialize(insert_val()).

insert_cache(N) ->
    maps:from_list([ {insert_key(I), insert_val()} || I <- lists:seq(1, N) ]).

%% Gas consumed by one finalize/3 that overwrites the single seeded entry of
%% an existing store map in place. The seeded value's size is the only thing
%% that varies; the value written over it is constant.
inplace_overwrite_gas(Size) ->
    {MapId, Stores, ChainApi} = seed_store_map(#{overwrite_key() => big_value(Size)}),
    Update = ?FATE_STORE_MAP(#{overwrite_key() => insert_val()}, MapId),
    inplace_update_gas(MapId, Update, Stores, ChainApi).

%% Gas consumed by one finalize/3 that inserts N fresh keys into an existing
%% store map in place. The seeded entry clears ?STORE_MAP_THRESHOLD so the
%% value is genuinely a store map; it is left untouched by the update.
inplace_insert_gas(N) ->
    {MapId, Stores, ChainApi} = seed_store_map(#{overwrite_key() => big_value(200)}),
    inplace_update_gas(MapId, ?FATE_STORE_MAP(insert_cache(N), MapId), Stores, ChainApi).

inplace_update_gas(_MapId, Update, Stores, ChainApi) ->
    Stores1 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, Update, Stores),
    {ok, _ChainApi1, GasLeft} = aefa_stores:finalize(ChainApi, ?SEED_GAS, Stores1),
    ?SEED_GAS - GasLeft.

%% Writes Entries as a real on-chain store map (a full finalize/3 round-trip,
%% so it is genuinely allocated rather than left inline) and hands back the
%% map id plus a store and chain api ready for a second, in-place update.
seed_store_map(Entries) ->
    ChainApi0 = fresh_chain_api(?ARCUS_PROTOCOL_VSN),
    Stores0 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY,
                                             aefa_stores:initial_contract_store(),
                                             aefa_stores:new()),
    Stores1 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS,
                                    aeb_fate_data:make_map(Entries), Stores0),
    {ok, ChainApi1, _} = aefa_stores:finalize(ChainApi0, ?SEED_GAS, Stores1),
    {OnChainStore, ChainApi2} =
        aefa_chain_api:contract_store(?CONTRACT_PUBKEY, committed(ChainApi1)),
    Stores2 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY, OnChainStore, aefa_stores:new()),
    {ok, RegVal, Stores3, _, _} =
        aefa_stores:find_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, Stores2, ?SEED_GAS),
    ?FATE_STORE_MAP(_Cache, MapId) = RegVal,
    {MapId, Stores3, ChainApi2}.

%% Seeds a real on-chain store map (via a real finalize/3 round-trip, so the
%% value is genuinely allocated as a store map rather than left inline) and
%% returns {MapId, Store} ready for a fresh store_map_lookup/member call.
seed_store_map_with_one_entry() ->
    Protocol = ?ARCUS_PROTOCOL_VSN,
    ChainApi0 = fresh_chain_api(Protocol),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    Value = aeb_fate_data:make_string(binary:copy(<<$a>>, 200)),
    MapVal = aeb_fate_data:make_map(#{ aeb_fate_data:make_string(<<"k">>) => Value }),
    Stores2 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, MapVal, Stores1),
    {ok, ChainApi1, _} = aefa_stores:finalize(ChainApi0, ?SEED_GAS, Stores2),
    {OnChainStore, _} = aefa_chain_api:contract_store(?CONTRACT_PUBKEY, ChainApi1),
    Store = aefa_stores:put_contract_store(?CONTRACT_PUBKEY, OnChainStore, aefa_stores:new()),
    {ok, RegVal, _, _, _} = aefa_stores:find_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, Store, 1000000000),
    ?FATE_STORE_MAP(_Cache, MapId) = RegVal,
    {MapId, Store}.

%%%===================================================================
%%% (f) finalize-time subtree reads: bounded by the gas, not by the map
%%%===================================================================

%% Equivalence guard: bounding the walk must not move the abort boundary.
gc_abort_boundary_is_exactly_the_successful_cost_test() ->
    {ChainApi, Stores} = staged_gc_drop(200),
    {ok, _, GasLeft} = aefa_stores:finalize(ChainApi, ?SEED_GAS, Stores),
    Cost = ?SEED_GAS - GasLeft,
    ?assertMatch({ok, _, 0}, aefa_stores:finalize(ChainApi, Cost, Stores)),
    ?assertEqual({error, out_of_gas}, aefa_stores:finalize(ChainApi, Cost - 1, Stores)).

%% Same guard on the copy path, which crosses the other two sites.
copy_abort_boundary_is_exactly_the_successful_cost_test() ->
    {ChainApi, Stores} = staged_map_copy(200),
    {ok, _, GasLeft} = aefa_stores:finalize(ChainApi, ?SEED_GAS, Stores),
    Cost = ?SEED_GAS - GasLeft,
    ?assertMatch({ok, _, 0}, aefa_stores:finalize(ChainApi, Cost, Stores)),
    ?assertEqual({error, out_of_gas}, aefa_stores:finalize(ChainApi, Cost - 1, Stores)).

%% The shared helper against what it replaces: read whole, then charge.
subtree_within_gas_matches_read_then_charge_test() ->
    N = 200,
    RawStore = persisted_raw_store(N),
    Prefix = map_subtree_prefix(RawStore),
    Subtree = aect_contracts_store:subtree(Prefix, RawStore),
    ?assertEqual(N, maps:size(Subtree)),
    Cost = aec_governance:store_read_base_gas()
         + subtree_bytes(Subtree) * aec_governance:store_read_byte_gas(),
    ?assertEqual({Subtree, 0}, aefa_stores:subtree_within_gas(Prefix, RawStore, Cost)),
    ?assertEqual({Subtree, 7}, aefa_stores:subtree_within_gas(Prefix, RawStore, Cost + 7)),
    ?assertThrow(out_of_gas, aefa_stores:subtree_within_gas(Prefix, RawStore, Cost - 1)),
    ?assertThrow(out_of_gas, aefa_stores:subtree_within_gas(Prefix, RawStore, 0)).

%% The GcCache-miss fallback is unreachable in production, so drive it directly:
%% a hit charges nothing, a miss charges exactly read_gas_cost(bytes), and both
%% leave the same store. Meta is passed through untouched, so #{} is enough.
gc_map_cache_miss_is_charged_like_any_other_subtree_read_test() ->
    N = 200,
    {MapId, _Stores, ChainApi} = seed_store_map(n_entry_map(N)),
    {RawStore, _} = aefa_chain_api:contract_store(?CONTRACT_PUBKEY, ChainApi),
    Subtree = aect_contracts_store:subtree(map_subtree_prefix(RawStore), RawStore),
    ?assertEqual(N, maps:size(Subtree)),
    Cost = aec_governance:store_read_base_gas()
         + subtree_bytes(Subtree) * aec_governance:store_read_byte_gas(),
    S = {#{}, RawStore},
    {_, 0, HitStore,  HitGas}  = aefa_stores:gc_map(MapId, S, #{MapId => Subtree}, ?SEED_GAS),
    {_, 0, MissStore, MissGas} = aefa_stores:gc_map(MapId, S, #{}, ?SEED_GAS),
    ?assertEqual(?SEED_GAS, HitGas),
    ?assertEqual(?SEED_GAS - Cost, MissGas),
    ?assertEqual(HitStore, MissStore),
    ?assertMatch({_, 0, _, 0}, aefa_stores:gc_map(MapId, S, #{}, Cost)),
    ?assertThrow(out_of_gas, aefa_stores:gc_map(MapId, S, #{}, Cost - 1)).

%% The regression: before the fix a 40x bigger map cost ~40x the work to refuse.
gc_refused_read_work_is_bounded_by_gas_not_map_size_test() ->
    %% The read floor plus 100 bytes -- a couple of entries.
    Gas = aec_governance:store_read_base_gas() + 1000,
    Small = refused_gc_reductions(100, Gas),
    Large = refused_gc_reductions(4000, Gas),
    ?assert(Large < Small * 3).

copy_refused_read_work_is_bounded_by_gas_not_map_size_test() ->
    Gas = aec_governance:store_read_base_gas() + 1000,
    Small = refused_copy_reductions(100, Gas),
    Large = refused_copy_reductions(4000, Gas),
    ?assert(Large < Small * 3).

%%% -- (f) helpers ---------------------------------------------------------

%% Reductions burned by a finalize that is refused, excluding all setup.
refused_gc_reductions(N, Gas) ->
    {ChainApi, Stores} = staged_gc_drop(N),
    measure_refused(ChainApi, Stores, Gas).

refused_copy_reductions(N, Gas) ->
    {ChainApi, Stores} = staged_map_copy(N),
    measure_refused(ChainApi, Stores, Gas).

measure_refused(ChainApi, Stores, Gas) ->
    {reductions, R0} = erlang:process_info(self(), reductions),
    Result = aefa_stores:finalize(ChainApi, Gas, Stores),
    {reductions, R1} = erlang:process_info(self(), reductions),
    ?assertEqual({error, out_of_gas}, Result),
    R1 - R0.

%% Big enough serialized to clear ?STORE_MAP_THRESHOLD and be a real store map.
n_entry_map(N) ->
    Value = aeb_fate_data:make_string(binary:copy(<<$a>>, 20)),
    maps:from_list([ {I, Value} || I <- lists:seq(1, N) ]).

%% Round 2 staged, not run: the map's only reference is overwritten, so
%% finalize/3 must read its whole subtree through gc_refcounts/4 to collect it.
staged_gc_drop(N) ->
    {_MapId, Stores, ChainApi} = seed_store_map(n_entry_map(N)),
    {ChainApi, aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, 0, Stores)}.

%% As above, but aliased into a second register first: two live references
%% mean the update cannot reuse in place and becomes a copy_map/5.
staged_map_copy(N) ->
    {MapId, Stores0, ChainApi} = seed_store_map(n_entry_map(N)),
    Alias = ?FATE_STORE_MAP(#{}, MapId),
    Stores1 = aefa_stores:put_value(?CONTRACT_PUBKEY, ?ALIAS_STORE_POS, Alias, Stores0),
    Updated = ?FATE_STORE_MAP(#{ aeb_fate_data:make_string(<<"new">>) =>
                                     aeb_fate_data:make_string(binary:copy(<<$n>>, 200)) },
                              MapId),
    {ChainApi, aefa_stores:put_value(?CONTRACT_PUBKEY, ?MAP_STORE_POS, Updated, Stores1)}.

persisted_raw_store(N) ->
    {_MapId, _Stores, ChainApi} = seed_store_map(n_entry_map(N)),
    {RawStore, _} = aefa_chain_api:contract_store(?CONTRACT_PUBKEY, ChainApi),
    RawStore.

%% aefa_stores' map_data_key/1 without reaching into the module: the marker
%% node is the shortest key under the prefix, and every entry key extends it.
map_subtree_prefix(RawStore) ->
    Keys = [ K || K <- maps:keys(aect_contracts_store:contents(RawStore)),
                  binary:part(K, 0, 1) =:= <<1>> ],
    hd(lists:sort(fun(A, B) -> byte_size(A) =< byte_size(B) end, Keys)).

subtree_bytes(Subtree) ->
    maps:fold(fun(K, V, Acc) -> Acc + byte_size(K) + byte_size(V) end, 0, Subtree).
