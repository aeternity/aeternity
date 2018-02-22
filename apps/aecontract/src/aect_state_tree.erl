%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_state_tree).

%% API
-export([ commit_to_db/1
        , empty/0
        , empty_with_backend/0
        , get_call/3
        , get_contract/2
        , insert_call/2
        , insert_contract/2
        , lookup_contract/2
        , root_hash/1]).

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type contract_tree() :: aeu_mtrees:mtree().

-record(contract_tree, {
          contracts = aeu_mtrees:empty() :: contract_tree()
    }).

-opaque tree() :: #contract_tree{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #contract_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    CtTree = aeu_mtrees:empty_with_backend(aec_db_backends:contracts_backend()),
    #contract_tree{contracts = CtTree}.

%% -- Contracts --

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
insert_contract(Contract, Tree = #contract_tree{ contracts = CtTree }) ->
    Id         = aect_contracts:id(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:insert(Id, Serialized, CtTree),
    Tree#contract_tree{ contracts = CtTree1}.

-spec get_contract(aect_contracts:id(), tree()) -> aect_contracts:contract().
get_contract(Id, #contract_tree{ contracts = CtTree }) ->
    aect_contracts:deserialize(aeu_mtrees:get(Id, CtTree)).

-spec lookup_contract(aect_contracts:id(), tree()) -> {value, aect_contracts:contract()} | none.
lookup_contract(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#contract_tree.contracts) of
        {value, Val} -> {value, aect_contracts:deserialize(Val)};
        none         -> none
    end.

%% -- Calls --

-spec insert_call(aect_call:call(), tree()) -> tree().
insert_call(Call, Tree = #contract_tree{ contracts = CtTree}) ->
    %% Construct the Id to store in the tree.
    CtId       = aect_call:contract_address(Call),
    CallId     = aect_call:id(Call),
    CallTreeId = <<CtId/binary, CallId/binary>>,

    %% Insert the new call into the history
    Serialized = aect_call:serialize(Call),
    CtTree1    = aeu_mtrees:insert(CallTreeId, Serialized, CtTree),

    %% Update the contract tree
    Tree#contract_tree{ contracts = CtTree1}.

-spec get_call(aect_contracts:id(), aect_call:id(), tree()) -> aect_call:call().
get_call(CtId, CallId, #contract_tree{ contracts = CtTree }) ->
    CallTreeId = <<CtId/binary, CallId/binary>>,
    aect_call:deserialize(aeu_mtrees:get(CallTreeId, CtTree)).

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#contract_tree{contracts = CtTree} = Tree) ->
    Tree#contract_tree{contracts = aeu_mtrees:commit_to_db(CtTree)}.
