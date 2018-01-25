%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_state_tree).

%% API
-export([ empty/0
        , insert_contract/2
        , root_hash/1]).

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(contract_tree, {}).

-opaque tree() :: #contract_tree{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    %% PLACEHOLDER
    #contract_tree{}.

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
insert_contract(_Contract, Tree) ->
    %% PLACEHOLDER
    Tree.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{}) ->
    {error, empty}.

