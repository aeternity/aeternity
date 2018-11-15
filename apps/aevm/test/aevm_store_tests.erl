%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Basic tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_store_tests).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    #{ storage := #{} }  = aevm_eeevm_store:init(#{}, #{ storage => undef}),
    A = 1 bsl 255,
    V = 2,
    K = <<A:256>>,
    Val = <<V:256>>,
    #{ storage := #{A := V} }  = aevm_eeevm_store:init(#{K => Val}, #{storage => #{}}).

to_binary_test() ->
    A = 1 bsl 255,
    V = 2,
    K = <<A:256>>,
    Val = <<V:256>>,
    #{K := <<V>>} = aevm_eeevm_store:to_binary(
                    aevm_eeevm_store:init(#{K => Val}, #{storage => #{}})).


store_test() ->
    State0 = aevm_eeevm_store:init(#{}, #{ storage => undef}),
    State1 = aevm_eeevm_store:store(32, 17, State0),
    #{ <<32>> := <<17>> } = aevm_eeevm_store:to_binary(State1).

load_test() ->
    State0 = aevm_eeevm_store:init(#{ <<32>> => <<17>>}, #{}),
    17 = aevm_eeevm_store:load(32, State0).
