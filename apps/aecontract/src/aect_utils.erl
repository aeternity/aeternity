%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Utility functions for AE Contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_utils).

-export([insert_call_in_trees/2,
         insert_contract_in_trees/2
        ]).

insert_call_in_trees(Call, Trees) ->
    CallsTree0 = aec_trees:calls(Trees),
    CallsTree1 = aect_call_state_tree:insert_call(Call, CallsTree0),
    aec_trees:set_calls(Trees, CallsTree1).

insert_contract_in_trees(Contract, Trees) ->
    CTrees = aec_trees:contracts(Trees),
    CTrees1 = aect_state_tree:insert_contract(Contract, CTrees),
    aec_trees:set_contracts(Trees, CTrees1).
