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
        , get_contract/2
        , insert_contract/2
        , enter_contract/2
        , lookup_contract/2
        , new_with_backend/1
        , root_hash/1]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        , lookup_poi/2
        ]).

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

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:contracts_backend()),
    #contract_tree{contracts = CtTree}.

%% -- Contracts --

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
insert_contract(Contract, Tree = #contract_tree{ contracts = CtTree }) ->
    Id         = aect_contracts:id(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:insert(Id, Serialized, CtTree),
    CtTree2    = insert_store(Contract, CtTree1),
    Tree#contract_tree{ contracts = CtTree2 }.

insert_store(Contract, CtTree) ->
    Id = aect_contracts:store_id(Contract),
    Store = aect_contracts:state(Contract),
    insert_store_nodes(Id, Store, CtTree).

insert_store_nodes(Prefix, Store, CtTree) ->
    Insert = fun (Key, Value, Tree) ->
                     Id = <<Prefix/binary, Key/binary>>,
                     aeu_mtrees:insert(Id, Value, Tree)
             end,
     maps:fold(Insert, CtTree, Store).


%% @doc Update an existing contract.
-spec enter_contract(aect_contracts:contract(), tree()) -> tree().
enter_contract(Contract, Tree = #contract_tree{ contracts = CtTree }) ->
    Id         = aect_contracts:id(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:enter(Id, Serialized, CtTree),
    OldContract = get_contract(Id, Tree),
    OldStore = aect_contracts:state(OldContract),
    CtTree2    = enter_store(Contract, OldStore, CtTree1),
    Tree#contract_tree{ contracts = CtTree2 }.

enter_store(Contract, OldStore, CtTree) ->
    Id = aect_contracts:store_id(Contract),
    Store = aect_contracts:state(Contract),
    MergedStore = maps:merge(Store, OldStore),
    %% Merged store contains all keys, and old Values.
    enter_store_nodes(Id, MergedStore, Store, OldStore, CtTree).

enter_store_nodes(Prefix, MergedStore, Store, OldStore, CtTree) ->
    %% Iterate over all (merged) keys.
    Insert = fun (Key,_MergedVal, Tree) ->
                     Id = <<Prefix/binary, Key/binary>>,
                     %% Check if key exist in new store
                     %% If not overwrite with empty tree.
                     case {maps:get(Key,    Store, <<>>),
                           maps:get(Key, OldStore, <<>>)} of
                         {Same, Same} -> Tree;
                         {Value,   _}    -> aeu_mtrees:enter(Id, Value, Tree)
                     end
             end,
     maps:fold(Insert, CtTree, MergedStore).

-spec get_contract(aect_contracts:id(), tree()) -> aect_contracts:contract().
get_contract(Id, #contract_tree{ contracts = CtTree }) ->
    Contract = aect_contracts:deserialize(Id, aeu_mtrees:get(Id, CtTree)),
    add_store(Contract, CtTree).

add_store(Contract, CtTree) ->
    Id = aect_contracts:store_id(Contract),
    Iterator = aeu_mtrees:iterator_from(Id, CtTree),
    Next = aeu_mtrees:iterator_next(Iterator),
    Size = byte_size(Id),
    Store = find_store_keys(Id, Next, Size, #{}),
    aect_contracts:set_state(Store, Contract).

find_store_keys(_, '$end_of_table', _, Store) ->
    Store;
find_store_keys(Id, {PrefixedKey, Val, Iter}, PrefixSize, Store) ->
    case PrefixedKey of
        <<Id:PrefixSize/binary, Key/binary>> ->
            Store1 = Store#{ Key => Val},
            Next = aeu_mtrees:iterator_next(Iter),
            find_store_keys(Id, Next, PrefixSize, Store1);
        _ ->
            Store
    end.


-spec lookup_contract(aect_contracts:id(), tree()) -> {value, aect_contracts:contract()} | none.
lookup_contract(Id, Tree) ->
    CtTree = Tree#contract_tree.contracts,
    case aeu_mtrees:lookup(Id, CtTree) of
        {value, Val} -> {value, add_store(aect_contracts:deserialize(Id, Val), CtTree)};
        none         -> none
    end.

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

-spec add_poi(aect_contracts:id(), aect_state_tree:tree(), aec_poi:poi()) ->
                     {'ok', binary(), aec_poi:poi()}
                   | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Id, #contract_tree{contracts = CtTree}, Poi) ->
    aec_poi:add_poi(Id, CtTree, Poi).

-spec verify_poi(aect_contracts:id(), binary(), aec_poi:poi()) ->
                        'ok' | {'error', term()}.
verify_poi(Id, SerializedContract, Poi) ->
    aec_poi:verify(Id, SerializedContract, Poi).

-spec lookup_poi(aect_contracts:id(), aec_poi:poi()) ->
                        {'ok', aect_contracts:contract()} | {'error', not_found}.
lookup_poi(Id, Poi) ->
    case aec_poi:lookup(Id, Poi) of
        {ok, Val} -> {ok, aect_contracts:deserialize(Id, Val)};
        Err -> Err
    end.

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#contract_tree{contracts = CtTree} = Tree) ->
    Tree#contract_tree{contracts = aeu_mtrees:commit_to_db(CtTree)}.
