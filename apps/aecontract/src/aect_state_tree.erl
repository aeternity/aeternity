%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_state_tree).

%% API
-export([ empty/0
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

%% -- Contracts --

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
insert_contract(Contract, Tree = #contract_tree{ contracts = CtTree, calls = CallTrees }) ->
    Id         = aect_contracts:id(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:insert(Id, Serialized, CtTree),
    CallTrees1 = gb_trees:insert(Id, aeu_mtrees:empty(), CallTrees),
    Tree#contract_tree{ contracts = CtTree1, calls = CallTrees1 }.

-spec get_contract(aect_contracts:id(), tree()) -> aect_contracts:contract().
get_contract(Id, #contract_tree{ contracts = CtTree }) ->
    aect_contracts:deserialize(aeu_mtrees:get(Id, CtTree)).

-spec lookup_contract(aect_contracts:id(), tree()) -> {value, aect_contracts:contract()} | none.
lookup_contract(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#contract_tree.contracts) of
        {value, Val} -> {value, aeo_contracts:deserialize(Val)};
        none         -> none
    end.

%% -- Calls --

-spec insert_call(aect_call:call(), tree()) -> tree().
insert_call(Call, Tree = #contract_tree{ contracts = CtTree, calls = CallTrees }) ->
    %% Get the contract being called, and its call history
    CtId       = aect_call:contract_address(Call),
    Contract   = get_contract(CtId, Tree),
    CallTree   = gb_trees:get(CtId, CallTrees),

    %% Insert the new call into the history
    CallId     = aect_call:id(Call),
    CallTree1  = aeu_mtrees:insert(CallId, aect_call:serialize(Call), CallTree),

    %% Update the call tree hash in the contract
    {ok, Hash} = aeu_mtrees:root_hash(CallTree1),
    Contract1  = aect_contracts:set_calls_hash(Hash, Contract),

    %% Update the contract tree and the calls tree
    CtTree1    = aeu_mtrees:enter(CtId, aect_contracts:serialize(Contract1), CtTree),
    CallTrees1 = gb_trees:enter(CtId, CallTree1, CallTrees),

    Tree#contract_tree{ contracts = CtTree1, calls = CallTrees1 }.

-spec get_call(aect_contracts:id(), aect_call:id(), tree()) -> aect_call:call().
get_call(CtId, CallId, #contract_tree{ calls = CallTrees }) ->
    CallTree = gb_trees:get(CtId, CallTrees),
    aect_call:deserialize(aeu_mtrees:get(CallId, CallTree)).

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{contracts = CtTree}) ->
    %% The contract tree references the hashes of the call trees so we only
    %% need the hash of the contract tree here.
    aeu_mtrees:root_hash(CtTree).

