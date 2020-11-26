%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of contracts
%%% @end
%%%-------------------------------------------------------------------

-module(aect_state_tree).

%% API
-export([ commit_to_db/1
        , copy_contract_store/3
        , read_contract_store/2
        , empty/0
        , empty_with_backend/0
        , get_contract/2
        , get_contract/3
        , insert_contract/2
        , enter_contract/2
        , is_contract/2
        , lookup_contract/2
        , lookup_contract/3
        , new_with_backend/1
        , new_with_dirty_backend/1
        , proxy_tree/1
        , get/2
        , gc_cache/1
        , root_hash/1]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        , lookup_poi/2
        ]).

-export([ to_list/1
        ]).

-export([ get_mtree/1
        , set_mtree/2
        ]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        ]).

-export([ record_fields/1
        , pp_term/1 ]).

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type contract_tree() :: aeu_mtrees:mtree().

-record(contract_tree, {
          contracts = aeu_mtrees:empty() :: contract_tree()
    }).

-opaque tree() :: #contract_tree{}.

-define(VSN, 1).

-record(proxy, {mod, arg}).

-define(PROXY(F, Mod, P),
        F(#proxy{mod = Mod} = P) -> Mod:do(?MODULE, F, P)).
                
-define(PROXY(F, Arg1, Mod, P),
        F(Arg1, #proxy{mod = Mod} = P) -> Mod:do(?MODULE, F, Arg1, P)).

-define(PROXY(F, Arg1, Arg2, Mod, P),
        F(Arg1, Arg2, #proxy{mod = Mod} = P) -> Mod:do(?MODULE, F, Arg1, Arg2, P)).


%% ==================================================================
%% Tracing support
record_fields(contract_tree) -> record_info(fields, contract_tree);
record_fields(_            ) -> no.

pp_term(Term) ->
    aeu_mp_trees:tree_pp_term(Term, '$contracts', fun aect_contracts:deserialize/2).

%% ==================================================================


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

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:dirty_contracts_backend()),
    #contract_tree{contracts = CtTree}.
-spec proxy_tree(contract_tree()) -> tree().
proxy_tree(Tree) ->
    #contract_tree{contracts = Tree}.

-spec get(aec_keys:pubkey(), tree()) -> binary().
?PROXY(get, Key, Mod, Arg);
get(Key, #contract_tree{contracts = CtTree}) ->
    aeu_mtrees:get(Key, CtTree).

-spec gc_cache(tree()) -> tree().
?PROXY(gc_cache, Mod, Arg);
gc_cache(#contract_tree{contracts = CtTree} = Tree) ->
    Tree#contract_tree{contracts =  aeu_mtrees:gc_cache(CtTree)}.

%% -- Contracts --

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
?PROXY(insert_contract, Contract, Mod, Arg);
insert_contract(Contract, Tree = #contract_tree{ contracts = CtTree }) ->
    Pubkey     = aect_contracts:pubkey(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:insert(Pubkey, Serialized, CtTree),
    CtTree2    = insert_store(Contract, CtTree1),
    Tree#contract_tree{ contracts = CtTree2 }.

-spec copy_contract_store(aect_contracts:contract(),
                          aect_contracts:pubkey(), tree()) -> tree().
?PROXY(copy_contract_store, Contract, NewId, Mod, P);
copy_contract_store(Contract, NewId, Tree = #contract_tree{ contracts = CtTree }) ->
    Id    = aect_contracts:compute_contract_store_id(NewId),
    Store = aect_contracts:state(Contract),
    %% Write a value at the store id to make it possible to get it as a
    %% subtree.
    CtTree1 = aeu_mtrees:insert(Id, <<0>>, CtTree),
    CtTree2 = insert_store_nodes(Id, aect_contracts_store:contents(Store), CtTree1),
    Tree#contract_tree{ contracts = CtTree2 }.

-spec read_contract_store(aect_contracts:pubkey(), tree()) ->
        {ok, aect_contracts_store:store()} | {error, term()}.
read_contract_store(StoreKey, #contract_tree{ contracts = CtTree }) ->
    StoreId = aect_contracts:compute_contract_store_id(StoreKey),
    case aeu_mtrees:read_only_subtree(StoreId, CtTree) of
        {ok, Subtree} ->
            {ok, aect_contracts_store:new(Subtree)};
        Err = {error, _} ->
            Err
    end.

insert_store(Contract, CtTree) ->
    Id = aect_contracts:store_id(Contract),
    Store = aect_contracts:state(Contract),
    %% Write a value at the store id to make it possible to get it as a
    %% subtree.
    CtTree1 = aeu_mtrees:insert(Id, <<0>>, CtTree),
    insert_store_nodes(Id, aect_contracts_store:write_cache(Store), CtTree1).

insert_store_nodes(Prefix, Writes, CtTree) ->
    Insert = fun(<<>>, _, Tree) -> Tree;    %% Ignore the empty key
                (Key, Value, Tree) ->
                    Id = <<Prefix/binary, Key/binary>>,
                    aeu_mtrees:insert(Id, Value, Tree)
             end,
    maps:fold(Insert, CtTree, Writes).


%% @doc Update an existing contract.
-spec enter_contract(aect_contracts:contract(), tree()) -> tree().
enter_contract(Contract, Tree = #contract_tree{ contracts = CtTree }) ->
    Pubkey     = aect_contracts:pubkey(Contract),
    Serialized = aect_contracts:serialize(Contract),
    CtTree1    = aeu_mtrees:enter(Pubkey, Serialized, CtTree),
    CtTree2    = enter_store(Contract, CtTree1),
    Tree#contract_tree{ contracts = CtTree2 }.

enter_store(Contract, CtTree) ->
    Id = aect_contracts:store_id(Contract),
    Store = aect_contracts:state(Contract),
    enter_store_nodes(Id, aect_contracts_store:write_cache(Store), CtTree).

enter_store_nodes(Prefix, Writes, CtTree) ->
    Insert = fun(<<>>, _, Tree) -> Tree; %% Ignore the empty key
                (Key, Value, Tree) ->
                     Id = <<Prefix/binary, Key/binary>>,
                     aeu_mtrees:enter(Id, Value, Tree)
             end,
    maps:fold(Insert, CtTree, Writes).

-spec get_contract(aect_contracts:pubkey(), tree()) -> aect_contracts:contract().
get_contract(PubKey, Tree) ->
    get_contract(PubKey, Tree, []).

-spec get_contract(aect_contracts:pubkey(), tree(), [no_store | full_store_cache]) ->
                      aect_contracts:contract().
get_contract(Pubkey, #contract_tree{ contracts = CtTree }, Options) ->
    Contract = aect_contracts:deserialize(Pubkey, aeu_mtrees:get(Pubkey, CtTree)),
    case proplists:get_value(no_store, Options, false) of
        false -> add_store(Contract, CtTree, Options);
        true  -> Contract
    end.

add_store(Contract, CtTree, Options) ->
    StoreId = aect_contracts:store_id(Contract),
    {ok, Subtree} = aeu_mtrees:read_only_subtree(StoreId, CtTree),
    Store = aect_contracts_store:new(Subtree),
    Store1 = case proplists:get_value(full_store_cache, Options, false) of
                 false ->
                     Store;
                 true ->
                     %% Make sure all values are in the cache
                     Map = aect_contracts_store:contents(Store),
                     aect_contracts_store:put_map(Map, Store)
             end,
    aect_contracts:set_state(Store1, Contract).

-spec is_contract(aect_contracts:pubkey(), tree()) -> boolean().
is_contract(Pubkey, #contract_tree{ contracts = CtTree }) ->
    case aeu_mtrees:lookup(Pubkey, CtTree) of
        {value, _} -> true;
        none       -> false
    end.

-spec lookup_contract(aect_contracts:pubkey(), tree()) -> {value, aect_contracts:contract()} | none.
lookup_contract(Pubkey, Tree) ->
    lookup_contract(Pubkey, Tree, []).

-spec lookup_contract(aect_contracts:pubkey(), tree(), [no_store | full_store_cache]) ->
        {value, aect_contracts:contract()} | none.
lookup_contract(Pubkey, Tree, Options) ->
    CtTree = Tree#contract_tree.contracts,
    case aeu_mtrees:lookup(Pubkey, CtTree) of
        {value, Val} ->
            Contract = aect_contracts:deserialize(Pubkey, Val),
            case proplists:get_value(no_store, Options, false) of
                false -> {value, add_store(Contract, CtTree, Options)};
                true  -> {value, Contract}
            end;
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
    try lookup_store_poi(Id, Poi) of
        {ok, Store1} ->
            StoreMap  = aect_contracts_store:contents(Store),
            StoreMap1 = aect_contracts_store:contents(Store1),
            %% Put the StoreMap in the write cache to make IsLegal check it
            Store2 = aect_contracts_store:put_map(StoreMap, aect_contracts_store:new()),
            case StoreMap =:= StoreMap1 andalso IsLegalStoreFun(Store2) of
                true  -> ok;
                false -> {error, bad_proof}
            end;
        {error, _} = E -> E
    catch _:_ ->
        %% Poi can be malicious(?) so catch errors
        {error, bad_proof}
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

-spec to_list(tree()) -> [{term(), term()}].
to_list(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:to_list(CtTree).

-spec get_mtree(tree()) -> aeu_mtrees:mtree().
get_mtree(#contract_tree{contracts = CtTree}) ->
    CtTree.

-spec set_mtree(aeu_mtrees:mtree(), tree()) -> tree().
set_mtree(CtTree, #contract_tree{} = T) ->
    T#contract_tree{contracts = CtTree}.

lookup_store_poi(Id, Poi) ->
    case aec_poi:read_only_subtree(Id, Poi) of
        {ok, Subtree} -> {ok, aect_contracts_store:new(Subtree)};
        {error, _} = Err -> Err
    end.

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#contract_tree{contracts = CtTree} = Tree) ->
    Tree#contract_tree{contracts = aeu_mtrees:commit_to_db(CtTree)}.

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(#contract_tree{contracts = CtTree}) ->
    Bin = aeu_mtrees:serialize(CtTree),
    aeser_chain_objects:serialize(
        contracts_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{contracts, Bin}]).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{contracts, ContractsBin}] =
        aeser_chain_objects:deserialize(contracts_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    #contract_tree{contracts = aeu_mtrees:deserialize(ContractsBin)}.

serialization_template(?VSN) ->
    [{contracts, binary}].
