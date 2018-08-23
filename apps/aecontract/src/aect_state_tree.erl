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
    Pubkey     = aect_contracts:pubkey(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:insert(Pubkey, Serialized, CtTree),
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
    Pubkey      = aect_contracts:pubkey(Contract),
    Serialized  = aect_contracts:serialize(Contract),
    CtTree1     = aeu_mtrees:enter(Pubkey, Serialized, CtTree),
    OldContract = get_contract(Pubkey, Tree),
    OldStore    = aect_contracts:state(OldContract),
    CtTree2     = enter_store(Contract, OldStore, CtTree1),
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

-spec get_contract(aect_contracts:pubkey(), tree()) -> aect_contracts:contract().
get_contract(Pubkey, #contract_tree{ contracts = CtTree }) ->
    Contract = aect_contracts:deserialize(Pubkey, aeu_mtrees:get(Pubkey, CtTree)),
    add_store(Contract, CtTree).

add_store(Contract, CtTree) ->
    Id = aect_contracts:store_id(Contract),
    Iterator = aeu_mtrees:iterator_from(Id, CtTree, [{with_prefix, Id}]),
    Next = aeu_mtrees:iterator_next(Iterator),
    Size = byte_size(Id),
    Store = find_store_keys(Id, Next, Size, #{}),
    aect_contracts:set_state(Store, Contract).

find_store_keys(_, '$end_of_table', _, Store) ->
    Store;
find_store_keys(Id, {PrefixedKey, Val, Iter}, PrefixSize, Store) ->
    <<Id:PrefixSize/binary, Key/binary>> = PrefixedKey,
    Store1 = Store#{ Key => Val},
    Next = aeu_mtrees:iterator_next(Iter),
    find_store_keys(Id, Next, PrefixSize, Store1).


-spec lookup_contract(aect_contracts:pubkey(), tree()) -> {value, aect_contracts:contract()} | none.
lookup_contract(Pubkey, Tree) ->
    CtTree = Tree#contract_tree.contracts,
    case aeu_mtrees:lookup(Pubkey, CtTree) of
        {value, Val} -> {value, add_store(aect_contracts:deserialize(Pubkey, Val), CtTree)};
        none         -> none
    end.

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

-spec add_poi(aect_contracts:pubkey(), aect_state_tree:tree(), aec_poi:poi()) ->
                     {'ok', aec_poi:poi()}
                   | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, #contract_tree{contracts = CtTree}, Poi) ->
    case aec_poi:add_poi(Pubkey, CtTree, Poi) of
        {ok, ContractPoi} ->
            add_store_to_poi(
              aect_contracts:compute_contract_store_id(Pubkey),
              CtTree,
              ContractPoi);
        {error, _} = Error -> Error
    end.

add_store_to_poi(Id, CtTree, Poi) ->
    Iterator = aeu_mtrees:iterator_from(Id, CtTree, [{with_prefix, Id}]),
    Next = aeu_mtrees:iterator_next(Iterator),
    Size = byte_size(Id),
    case add_store_keys_poi(Id, Next, Size, Poi, CtTree) of
        {error, _} = E -> E;
        StorePoi ->
            {ok, StorePoi}
    end.

add_store_keys_poi(_, '$end_of_table', _, Poi, _) ->
    Poi;
add_store_keys_poi(Id, {PrefixedKey, _Val, Iter}, PrefixSize, Poi, CtTree) ->
    KeyId = <<Id:PrefixSize/binary, _Key/binary>> = PrefixedKey,
    case aec_poi:add_poi(KeyId, CtTree, Poi) of
        {ok, Poi1} ->
            Next = aeu_mtrees:iterator_next(Iter),
            add_store_keys_poi(Id, Next, PrefixSize, Poi1, CtTree);
        {error, _} = E -> E
    end.



-spec verify_poi(aect_contracts:pubkey(), aect_contracts:contract(), aec_poi:poi()) ->
                        'ok' | {'error', term()}.
verify_poi(Pubkey, Contract, Poi) ->
    %% Hardcode expectation on specified contract object key being
    %% equal to key in internal representation of contract.  The key
    %% is not part of the contract serialization so this shall never
    %% happen.
    Pubkey = aect_contracts:pubkey(Contract),
    case aec_poi:verify(Pubkey, aect_contracts:serialize(Contract), Poi) of
        {error, _} = E -> E; %% More fine grained error reason than lookup.
        ok ->
            verify_store_poi(aect_contracts:store_id(Contract),
                             aect_contracts:state(Contract),
                             aect_contracts:is_legal_state_fun(Contract),
                             Poi)
    end.

verify_store_poi(Id, Store, IsLegalStoreFun, Poi) ->
    %% Check separately absence of (invalid) empty key because Merkle
    %% tree iterator-from-key, used in PoI lookup, does not return
    %% initial key if present.
    case aec_poi:lookup(Id, Poi) of
        {ok, _StoreValWithEmptyKey} -> {error, bad_proof};
        {error, not_found} ->
            case lookup_store_poi(Id, Poi) of
                {ok, Store} ->
                    case IsLegalStoreFun(Store) of
                        true -> ok;
                        false -> {error, bad_proof}
                    end;
                {ok, _} -> {error, bad_proof};
                {error, _} = E -> E
            end
    end.

-spec lookup_poi(aect_contracts:pubkey(), aec_poi:poi()) ->
                        {'ok', aect_contracts:contract()} | {'error', not_found}.
lookup_poi(Pubkey, Poi) ->
    case aec_poi:lookup(Pubkey, Poi) of
        {ok, Val} ->
            Contract = aect_contracts:deserialize(Pubkey, Val),
            case lookup_store_poi(aect_contracts:store_id(Contract), Poi) of
                {error, _} = E -> E;
                {ok, Store} -> {ok, aect_contracts:set_state(Store, Contract)}
            end;
        Err -> Err
    end.

lookup_store_poi(Id, Poi) ->
    case aec_poi:iterator_from(Id, Poi, [{with_prefix, Id}]) of
        {ok, Iterator} ->
            Next = aec_poi:iterator_next(Iterator),
            Size = byte_size(Id),
            lookup_store_from_poi(Id, Next, Size, #{}, Poi);
        {error, _} = E -> E
    end.

lookup_store_from_poi(_, {error, bad_proof} = E, _, _, _) ->
    E;
lookup_store_from_poi(_, '$end_of_table', _, Store,_Poi) ->
    {ok, Store};
lookup_store_from_poi(Id, {PrefixedKey, Val, Iter}, PrefixSize, Store, Poi) ->
    <<Id:PrefixSize/binary, Key/binary>> = PrefixedKey,
    Store1 = Store#{ Key => Val},
    case aec_poi:iterator_next(Iter) of
        {error, _} = E -> E;
        Next ->
            lookup_store_from_poi(Id, Next, PrefixSize, Store1, Poi)
    end.

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#contract_tree{contracts = CtTree} = Tree) ->
    Tree#contract_tree{contracts = aeu_mtrees:commit_to_db(CtTree)}.
