%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_store_tests).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    EmptyStore = aect_contracts_store:new(),
    EmptyMap   = #{},
    #{ storage := EmptyMap }  = aevm_eeevm_store:init(EmptyStore, #{}),
    A = 1 bsl 255,
    V = 2,
    K = <<A:256>>,
    Val = <<V:256>>,
    NonemptyStore = aect_contracts_store:put(K, Val, EmptyStore),
    NonemptyMap   = #{ A => V },
    #{ storage := NonemptyMap }  = aevm_eeevm_store:init(NonemptyStore, #{}).

to_binary_test() ->
    A = 1 bsl 255,
    V = 2,
    K = <<A:256>>,
    Val = <<V:256>>,
    Store  = aect_contracts_store:put(K, Val, aect_contracts_store:new()),
    Store1 = aect_contracts_store:put(K, <<2>>, aect_contracts_store:new()),
    Store1 = aevm_eeevm_store:to_binary(
                aevm_eeevm_store:init(Store, #{})).


store_test() ->
    Store  = aect_contracts_store:new(),
    State0 = aevm_eeevm_store:init(Store, #{}),
    State1 = aevm_eeevm_store:store(32, 17, State0),
    #{ <<32>> := <<17>> } = aect_contracts_store:contents(aevm_eeevm_store:to_binary(State1)).

load_test() ->
    Store  = aect_contracts_store:put(<<32>>, <<17>>, aect_contracts_store:new()),
    State0 = aevm_eeevm_store:init(Store, #{}),
    17 = aevm_eeevm_store:load(32, State0).
