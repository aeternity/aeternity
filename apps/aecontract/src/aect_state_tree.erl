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

-type contract_tree() :: aeu_mtrees:tree(aect_contracts:id(), aect_contracts:serialized()).
-type call_tree()     :: aeu_mtrees:tree(aect_call:id(), aect_call:serialized()).
-type call_trees()    :: gb_trees:tree(aect_contracts:id(), call_tree()).

-record(contract_tree, {
    contracts = aeu_mtrees:empty() :: contract_tree(),
    calls     = gb_trees:empty()   :: call_trees()
        %% Keep calls in separate trees to avoid having to serialize/deserialize the
        %% entire call history every time we're adding a call.
    }).

-opaque tree() :: #contract_tree{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #contract_tree{}.

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
insert_contract(_Contract, Tree) ->
    %% PLACEHOLDER
    Tree.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{contracts = CtTree}) ->
    %% The contract tree references the hashes of the call trees so we only
    %% need the hash of the contract tree here.
    aeu_mtrees:root_hash(CtTree).

