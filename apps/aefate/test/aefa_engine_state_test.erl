%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Pins the aefa_engine_state:aefa_stores_for_protocol/1 dispatch
%%%    boundary, and regression-tests that pre-Iris finalize/1 does not
%%%    `undef` (aefa_stores_lima has no terms_to_finalize/1; finalize/1
%%%    must call the live aefa_stores module directly for that step).
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_engine_state_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

%%%===================================================================
%%% 1. Dispatch-boundary test
%%%===================================================================

%% Pins the protocol -> store-module dispatch boundary.
aefa_stores_for_protocol_boundary_test_() ->
    Cases =
        [ {?ROMA_PROTOCOL_VSN,    aefa_stores_lima}
        , {?MINERVA_PROTOCOL_VSN, aefa_stores_lima}
        , {?FORTUNA_PROTOCOL_VSN, aefa_stores_lima}
        , {?LIMA_PROTOCOL_VSN,    aefa_stores_lima}   %% last pre-Iris protocol
        , {?IRIS_PROTOCOL_VSN,    aefa_stores_ceres}  %% exact boundary: lima -> ceres
        , {?CERES_PROTOCOL_VSN,   aefa_stores_ceres}  %% last protocol on the frozen logic
        , {?ARCUS_PROTOCOL_VSN,   aefa_stores}        %% exact boundary: ceres -> live
        , {?SALUS_PROTOCOL_VSN,   aefa_stores}        %% later protocols stay live
        ],
    [ {case_name(Protocol, Expected),
       fun() -> ?assertEqual(Expected, aefa_engine_state:aefa_stores_for_protocol(Protocol)) end}
      || {Protocol, Expected} <- Cases ].

%% The Arcus clause is a `>=` guard, not `==`: future protocols must keep
%% resolving to the live module without a code change.
aefa_stores_for_protocol_forward_compat_test() ->
    ?assertEqual(aefa_stores, aefa_engine_state:aefa_stores_for_protocol(?ARCUS_PROTOCOL_VSN + 1)),
    ?assertEqual(aefa_stores, aefa_engine_state:aefa_stores_for_protocol(100)).
case_name(Protocol, Expected) ->
    lists:flatten(io_lib:format("protocol ~p dispatches to ~p", [Protocol, Expected])).

%%%===================================================================
%%% 2. Regression: pre-Iris (Lima) finalize must not `undef`
%%%===================================================================

-define(SR1_OWNER_PUBKEY,    <<16#B0:256>>).
-define(SR1_CONTRACT_PUBKEY, <<16#C0:256>>).
-define(SR1_CALLER_PUBKEY,   <<16#CA:256>>).
-define(SR1_STORE_POS, 1).

%% Must not raise {undef, [{aefa_stores_lima, terms_to_finalize, ...}]}.
pre_iris_finalize_does_not_crash_test() ->
    ES = lima_engine_state_with_dirty_store(),
    Result = aefa_engine_state:finalize(ES),
    ?assertMatch({ok, _}, Result).

%% finalize/1 also writes the dirty value through unchanged.
pre_iris_finalize_produces_correct_result_test() ->
    ES = lima_engine_state_with_dirty_store(),
    {ok, ES1} = aefa_engine_state:finalize(ES),
    ChainApi1 = aefa_engine_state:chain_api(ES1),
    {OnChainStore, _ChainApi2} = aefa_chain_api:contract_store(?SR1_CONTRACT_PUBKEY, ChainApi1),
    ReadBack = aefa_stores:put_contract_store(?SR1_CONTRACT_PUBKEY, OnChainStore, aefa_stores:new()),
    %% Reads through the live aefa_stores module directly (bypassing
    %% protocol dispatch), whose find_value/3 returns a 4-tuple.
    ?assertMatch({ok, _, _, _}, aefa_stores:find_value(?SR1_CONTRACT_PUBKEY, ?SR1_STORE_POS, ReadBack)),
    {ok, ReadBackVal, _, _Bytes} = aefa_stores:find_value(?SR1_CONTRACT_PUBKEY, ?SR1_STORE_POS, ReadBack),
    ?assertEqual(test_value(), ReadBackVal).

%%%===================================================================
%%% 3. Regression: pre-Iris (Lima) store-map ops must not `undef`/misbehave
%%%
%%% The two tests above only ever write one integer register, so they
%%% would only catch aefa_stores_lima lacking a function entirely (an
%%% `undef`), never a wrong *result* from its store-map path. These
%%% exercise lookup/member/size/to_list against a real store map, plus
%%% a finalize/1 round-trip that also runs Lima's map GC.
%%%===================================================================

-define(SR2_MAP_STORE_POS, 2).

lima_map_lookup_hits_test() ->
    ES = lima_engine_state_with_store_map(),
    {MapRegVal, ES1} = aefa_fate:lookup_var({var, -?SR2_MAP_STORE_POS}, ES),
    ES2 = aefa_fate_op:map_lookup({var, 0}, {immediate, MapRegVal}, {immediate, map_key()}, ES1),
    ?assertEqual({val, map_value()}, maps:get({var, 0}, aefa_engine_state:memory(ES2))).

lima_map_member_test() ->
    ES = lima_engine_state_with_store_map(),
    {MapRegVal, ES1} = aefa_fate:lookup_var({var, -?SR2_MAP_STORE_POS}, ES),
    ES2 = aefa_fate_op:map_member({var, 0}, {immediate, MapRegVal}, {immediate, map_key()}, ES1),
    ?assertEqual({val, ?FATE_TRUE}, maps:get({var, 0}, aefa_engine_state:memory(ES2))),
    ES3 = aefa_fate_op:map_member({var, 0}, {immediate, MapRegVal}, {immediate, absent_key()}, ES1),
    ?assertEqual({val, ?FATE_FALSE}, maps:get({var, 0}, aefa_engine_state:memory(ES3))).

lima_map_size_test() ->
    ES = lima_engine_state_with_store_map(),
    {MapRegVal, ES1} = aefa_fate:lookup_var({var, -?SR2_MAP_STORE_POS}, ES),
    ES2 = aefa_fate_op:map_size_({var, 0}, {immediate, MapRegVal}, ES1),
    ?assertEqual({val, aeb_fate_data:make_integer(1)}, maps:get({var, 0}, aefa_engine_state:memory(ES2))).

lima_map_to_list_test() ->
    ES = lima_engine_state_with_store_map(),
    {MapRegVal, ES1} = aefa_fate:lookup_var({var, -?SR2_MAP_STORE_POS}, ES),
    ES2 = aefa_fate_op:map_to_list({var, 0}, {immediate, MapRegVal}, ES1),
    {val, ListVal} = maps:get({var, 0}, aefa_engine_state:memory(ES2)),
    %% Each pair comes back as a FATE tuple, not a plain Erlang 2-tuple.
    ?assertEqual([{tuple, {map_key(), map_value()}}], ?FATE_LIST_VALUE(ListVal)).

%% A store map with no reads/writes this call must still survive finalize/1
%% (Lima's GC path) and round-trip unchanged on-chain.
lima_finalize_with_untouched_map_test() ->
    ES = lima_engine_state_with_store_map(),
    {ok, ES1} = aefa_engine_state:finalize(ES),
    ChainApi1 = aefa_engine_state:chain_api(ES1),
    {OnChainStore, _} = aefa_chain_api:contract_store(?SR1_CONTRACT_PUBKEY, ChainApi1),
    ReadBack = aefa_stores:put_contract_store(?SR1_CONTRACT_PUBKEY, OnChainStore, aefa_stores:new()),
    {ok, MapRegVal, _, _Bytes} = aefa_stores:find_value(?SR1_CONTRACT_PUBKEY, ?SR2_MAP_STORE_POS, ReadBack),
    ?FATE_STORE_MAP(_Cache, MapId) = MapRegVal,
    {StoreList, _} = aefa_stores:store_map_to_list(?SR1_CONTRACT_PUBKEY, MapId, ReadBack),
    ?assertEqual([{map_key(), map_value()}], StoreList).

map_key() -> aeb_fate_data:make_string(<<"k">>).
absent_key() -> aeb_fate_data:make_string(<<"missing">>).
%% Large enough to clear the store-map inlining threshold, so finalize/1
%% promotes it to a real on-chain ?FATE_STORE_MAP instead of leaving it inline.
map_value() -> aeb_fate_data:make_string(binary:copy(<<$a>>, 200)).

%% Same fixture as lima_engine_state_with_dirty_store/0, plus one store map
%% (dispatched through aefa_stores_lima, same as the plain register above).
lima_engine_state_with_store_map() ->
    aefa_fate_op:load_pre_iris_map_ordering(),
    Trees = trees_with_one_contract(),
    TxEnv = aetx_env:tx_env(_Height = 1, ?LIMA_PROTOCOL_VSN),
    ChainApi = aefa_chain_api:new(#{ gas_price => 1
                                   , fee        => 0
                                   , origin     => ?SR1_CALLER_PUBKEY
                                   , trees      => Trees
                                   , tx_env     => TxEnv
                                   }),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?SR1_CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    MapVal = aeb_fate_data:make_map(#{ map_key() => map_value() }),
    Stores2 = aefa_stores:put_value(?SR1_CONTRACT_PUBKEY, ?SR2_MAP_STORE_POS, MapVal, Stores1),
    ES = aefa_engine_state:new(_Gas = 1000000, _Value = 0,
                               #{caller => ?SR1_CALLER_PUBKEY},
                               Stores2, ChainApi, #{}, ?VM_FATE_SOPHIA_2),
    ES1 = aefa_engine_state:set_current_contract(?SR1_CONTRACT_PUBKEY, ES),
    %% Round-trip through finalize/1 once so the map register above is
    %% allocated into a real on-chain ?FATE_STORE_MAP, not an inline map.
    {ok, ES2} = aefa_engine_state:finalize(ES1),
    ChainApi1 = aefa_engine_state:chain_api(ES2),
    ES3 = aefa_engine_state:new(_Gas2 = 1000000, _Value2 = 0,
                                #{caller => ?SR1_CALLER_PUBKEY},
                                aefa_stores:new(), ChainApi1, #{}, ?VM_FATE_SOPHIA_2),
    aefa_engine_state:set_current_contract(?SR1_CONTRACT_PUBKEY, ES3).

%% -- helpers ---------------------------------------------------------

%% Engine state for a pre-Iris (Lima, protocol 4) tx_env, one contract
%% registered in the trees, with one dirty store register.
lima_engine_state_with_dirty_store() ->
    Trees = trees_with_one_contract(),
    TxEnv = aetx_env:tx_env(_Height = 1, ?LIMA_PROTOCOL_VSN),
    ChainApi = aefa_chain_api:new(#{ gas_price => 1
                                   , fee        => 0
                                   , origin     => ?SR1_CALLER_PUBKEY
                                   , trees      => Trees
                                   , tx_env     => TxEnv
                                   }),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?SR1_CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    Stores2 = aefa_stores:put_value(?SR1_CONTRACT_PUBKEY, ?SR1_STORE_POS,
                                     test_value(), Stores1),
    aefa_engine_state:new(_Gas = 1000000, _Value = 0,
                           #{caller => ?SR1_CALLER_PUBKEY},
                           Stores2, ChainApi, #{}, ?VM_FATE_SOPHIA_2).

test_value() -> aeb_fate_data:make_integer(424242).

trees_with_one_contract() ->
    CTVersion = #{vm => ?VM_FATE_SOPHIA_2, abi => ?ABI_FATE_SOPHIA_1},
    Contract0 = aect_contracts:new(?SR1_OWNER_PUBKEY, _Nonce = 1, CTVersion,
                                    _Code = <<"unused-in-this-test">>, _Deposit = 0),
    Contract1 = aect_contracts:set_pubkey(?SR1_CONTRACT_PUBKEY, Contract0),
    Account = aec_accounts:new(?SR1_CONTRACT_PUBKEY, 0),
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = aec_trees:set_contracts(Trees0,
                aect_state_tree:insert_contract(Contract1, aec_trees:contracts(Trees0))),
    aec_trees:set_accounts(Trees1,
                aec_accounts_trees:enter(Account, aec_trees:accounts(Trees1))).
