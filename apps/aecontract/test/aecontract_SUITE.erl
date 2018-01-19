%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Contracts
%%% @end
%%%-------------------------------------------------------------------
-module(aecontract_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        ]).

%% test case exports
-export([ call_contract/1
        , call_contract_negative/1
        , create_contract/1
        , create_contract_negative/1
        ]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aecontract/include/contract_txs.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}
    ].

groups() ->
    [ {all_tests, [sequence], [ {group, transactions}
                              ]}
    , {transactions, [sequence], [ create_contract
                                 , create_contract_negative
                                 , call_contract
                                 , call_contract_negative
                                 ]}
    ].

%%%===================================================================
%%% Register contract
%%%===================================================================

create_contract_negative(_Cfg) ->
    %% PLACEHOLDER
    ok.

create_contract(_Cfg) ->
    %% PLACEHOLDER
    ok.

%%%===================================================================
%%% Query contract
%%%===================================================================

call_contract_negative(_Cfg) ->
    %% PLACEHOLDER
    ok.

call_contract(_Cfg) ->
    %% PLACEHOLDER
    ok.

