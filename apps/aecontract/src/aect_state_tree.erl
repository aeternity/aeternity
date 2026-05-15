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
        , get_contract_with_code/2
        , get_contract_with_code/3
        , delete_contract/2
        , insert_contract/2
        , enter_contract/2
        , is_contract/2
        , lookup_contract/2
        , lookup_contract/3
        , lookup_contract_with_code/2
        , lookup_contract_with_code/3
        , new_with_backend/1
        , new_with_dirty_backend/1
        , gc_cache/1
        , root_hash/1
        , db/1
        , flush_store_batch/1
        , flush_contract_batch/2
        , flush_contract_meta_batch/1
        , flush_contract_meta_for/2
        ]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        , lookup_poi/2
        ]).

-export([ to_list/1
        ]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        , from_db_format/1
        ]).

-export([record_fields/1]).

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type contract_tree() :: aeu_mtrees:mtree().

-type store_batch() ::
    #{aect_contracts:pubkey() => {StoreId :: binary(),
                                  Writes  :: #{binary() => binary()},
                                  Reads   :: #{binary() => binary()}}}.

%% Per-microblock deferred contract metadata writes.
%% 'new' = inserted this microblock (flush uses aeu_mtrees:insert).
%% 'update' = already existed in the MPT when first seen (flush uses aeu_mtrees:enter).
-type meta_entry() :: {new | update, aect_contracts:contract()}.
-type contract_meta_batch() :: #{aect_contracts:pubkey() => meta_entry()}.

-record(contract_tree, {
          contracts           = aeu_mtrees:empty() :: contract_tree(),
          store_batch         = #{}               :: store_batch(),
          contract_meta_batch = #{}               :: contract_meta_batch()
    }).

-opaque tree() :: #contract_tree{}.

-define(VSN, 1).

%% ==================================================================
%% Tracing support
record_fields(contract_tree) -> record_info(fields, contract_tree);
record_fields(_            ) -> no.
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

-spec from_db_format(tree()) -> tree().
from_db_format(Tree = #contract_tree{contracts = CtTree}) ->
    Tree#contract_tree{contracts = aeu_mtrees:from_db_format(CtTree)}.

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:contracts_backend()),
    #contract_tree{contracts = CtTree}.

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:dirty_contracts_backend()),
    #contract_tree{contracts = CtTree}.

-spec gc_cache(tree()) -> tree().
gc_cache(#contract_tree{contracts = CtTree} = Tree) ->
    Tree#contract_tree{contracts =  aeu_mtrees:gc_cache(CtTree)}.

%% -- Contracts --

-spec insert_contract(aect_contracts:contract(), tree()) -> tree().
insert_contract(Contract, Tree = #contract_tree{contract_meta_batch = MB}) ->
    Pubkey = aect_contracts:pubkey(Contract),
    %% Batch the metadata write. Duplicate pubkeys within one microblock are
    %% structurally impossible: pubkey = hash(owner, nonce), and nonce uniqueness
    %% is enforced before tx application.  The 'new' tag ensures aeu_mtrees:insert
    %% (not enter) is used at flush time, preserving the error-on-duplicate invariant.
    MB1   = MB#{Pubkey => {new, Contract}},
    Tree1 = Tree#contract_tree{contract_meta_batch = MB1},
    insert_store(Contract, Tree1).

-spec delete_contract(aect_contracts:pubkey(), tree()) -> tree().
delete_contract(PK, Tree = #contract_tree{contracts           = CtTree,
                                          store_batch         = SB,
                                          contract_meta_batch = MB}) ->
    CtTree1 = aeu_mtrees:delete(PK, CtTree),
    Tree#contract_tree{contracts           = CtTree1,
                       store_batch         = maps:remove(PK, SB),
                       contract_meta_batch = maps:remove(PK, MB)}.

-spec copy_contract_store(aect_contracts:contract(),
                          aect_contracts:pubkey(), tree()) -> tree().
copy_contract_store(Contract, NewId, Tree) ->
    %% Flush source contract's batch so contents/1 sees all pending writes.
    SrcPubkey = aect_contracts:pubkey(Contract),
    Tree1 = flush_contract_batch(SrcPubkey, Tree),
    #contract_tree{contracts = CtTree} = Tree1,
    Id    = aect_contracts:compute_contract_store_id(NewId),
    Store = aect_contracts:state(Contract),
    %% Write a value at the store id to make it possible to get it as a subtree.
    CtTree1 = aeu_mtrees:insert(Id, <<0>>, CtTree),
    CtTree2 = insert_store_nodes(Id, aect_contracts_store:contents(Store), CtTree1),
    Tree1#contract_tree{ contracts = CtTree2 }.

-spec read_contract_store(aect_contracts:pubkey(), tree()) ->
        {ok, aect_contracts_store:store()} | {error, term()}.
read_contract_store(StoreKey, Tree) ->
    Tree1 = flush_contract_batch(StoreKey, Tree),
    #contract_tree{contracts = CtTree} = Tree1,
    StoreId = aect_contracts:compute_contract_store_id(StoreKey),
    case aeu_mtrees:read_only_subtree(StoreId, CtTree) of
        {ok, Subtree} ->
            {ok, aect_contracts_store:new(Subtree)};
        Err = {error, _} ->
            Err
    end.

insert_store(Contract, #contract_tree{store_batch = Batch} = Tree) ->
    Id     = aect_contracts:store_id(Contract),
    Store  = aect_contracts:state(Contract),
    Writes = aect_contracts_store:write_cache(Store),
    Reads  = aect_contracts_store:read_cache(Store),
    Batch1 = batch_accumulate(aect_contracts:pubkey(Contract), Id, Writes, Reads, Batch),
    Tree#contract_tree{store_batch = Batch1}.

insert_store_nodes(Prefix, Writes, CtTree) ->
    Insert = fun(<<>>, _, Tree) -> Tree;    %% Ignore the empty key
                (Key, Value, Tree) ->
                    Id = <<Prefix/binary, Key/binary>>,
                    aeu_mtrees:insert(Id, Value, Tree)
             end,
    maps:fold(Insert, CtTree, Writes).


%% @doc Update an existing contract.
-spec enter_contract(aect_contracts:contract(), tree()) -> tree().
enter_contract(Contract, Tree = #contract_tree{contract_meta_batch = MB}) ->
    Pubkey = aect_contracts:pubkey(Contract),
    %% If this pubkey was inserted in the current microblock, keep the 'new' tag so
    %% the flush still uses aeu_mtrees:insert (which errors on duplicate, detecting bugs).
    %% Otherwise tag as 'update' to use aeu_mtrees:enter at flush time.
    Tag = case maps:find(Pubkey, MB) of
        {ok, {new, _}} -> new;
        _              -> update
    end,
    MB1   = MB#{Pubkey => {Tag, Contract}},
    Tree1 = Tree#contract_tree{contract_meta_batch = MB1},
    enter_store(Contract, Tree1).

enter_store(Contract, #contract_tree{store_batch = Batch} = Tree) ->
    Id     = aect_contracts:store_id(Contract),
    Store  = aect_contracts:state(Contract),
    Writes = aect_contracts_store:write_cache(Store),
    Reads  = aect_contracts_store:read_cache(Store),
    Batch1 = batch_accumulate(aect_contracts:pubkey(Contract), Id, Writes, Reads, Batch),
    Tree#contract_tree{store_batch = Batch1}.

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
get_contract(Pubkey, Tree, Options) ->
    case lookup_contract(Pubkey, Tree, Options) of
        none -> error({not_present, Pubkey});
        {value, Contract} -> Contract
    end.

-spec get_contract_with_code(aect_contracts:pubkey(), tree()) -> {aect_contracts:contract(), binary()}.
get_contract_with_code(PubKey, Tree) ->
    get_contract_with_code(PubKey, Tree, []).

-spec get_contract_with_code(aect_contracts:pubkey(), tree(), [no_store | full_store_cache]) -> {aect_contracts:contract(), binary()}.
get_contract_with_code(Pubkey, Tree, Options) ->
    case lookup_contract_with_code(Pubkey, Tree, Options) of
        none -> error({not_present, Pubkey});
        {value, Contract, Code} -> {Contract, Code}
    end.

add_store(Contract, #contract_tree{contracts = CtTree, store_batch = Batch}, Options) ->
    Pubkey  = aect_contracts:pubkey(Contract),
    StoreId = aect_contracts:store_id(Contract),
    Store0 = case aeu_mtrees:read_only_subtree(StoreId, CtTree) of
        {ok, Subtree} ->
            aect_contracts_store:new(Subtree);
        {error, no_such_subtree} ->
            %% Sentinel is written only at flush time (when writes are non-empty),
            %% so any contract with an empty store — new or existing — returns this.
            aect_contracts_store:new()
    end,
    %% Pre-populate read_cache: stable MPT reads from prior txs first, then writes on top
    %% (writes override reads for the same key — a written key must read as the written value).
    Store1 = case maps:find(Pubkey, Batch) of
        {ok, {_StoreId, Writes, Reads}} ->
            Store_r = aect_contracts_store:put_map_to_read_cache(Reads, Store0),
            aect_contracts_store:put_map_to_read_cache(Writes, Store_r);
        error ->
            Store0
    end,
    Store2 = case proplists:get_value(full_store_cache, Options, false) of
        false -> Store1;
        true  ->
            Map = aect_contracts_store:contents(Store1),
            aect_contracts_store:put_map(Map, Store1)
    end,
    aect_contracts:set_state(Store2, Contract).

-spec is_contract(aect_contracts:pubkey(), tree()) -> boolean().
is_contract(Pubkey, #contract_tree{contracts = CtTree, contract_meta_batch = MB}) ->
    maps:is_key(Pubkey, MB) orelse
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
    MB = Tree#contract_tree.contract_meta_batch,
    case maps:find(Pubkey, MB) of
        {ok, {_Tag, Contract}} ->
            %% Batch hit: O(1) map lookup, no MPT read.
            case proplists:get_value(no_store, Options, false) of
                true  -> {value, Contract};
                false -> {value, add_store(Contract, Tree, Options)}
            end;
        error ->
            %% Check block-level cache before the O(log N) MPT traversal.
            case aec_block_contract_cache:get(Pubkey) of
                {value, Contract} ->
                    case proplists:get_value(no_store, Options, false) of
                        true  -> {value, Contract};
                        false -> {value, add_store(Contract, Tree, Options)}
                    end;
                none ->
                    %% First access for this contract this keyblock epoch — go to MPT.
                    CtTree = Tree#contract_tree.contracts,
                    case aeu_mtrees:lookup(Pubkey, CtTree) of
                        {value, Val} ->
                            Contract = aect_contracts:deserialize(Pubkey, Val),
                            ok = aec_block_contract_cache:put(Pubkey, Contract),
                            case proplists:get_value(no_store, Options, false) of
                                false -> {value, add_store(Contract, Tree, Options)};
                                true  -> {value, Contract}
                            end;
                        none -> none
                    end
            end
    end.

-spec lookup_contract_with_code(aect_contracts:pubkey(), tree()) -> {value, aect_contracts:contract(), binary()} | none.
lookup_contract_with_code(PubKey, Tree) ->
    lookup_contract_with_code(PubKey, Tree, []).

-spec lookup_contract_with_code(aect_contracts:pubkey(), tree(), [no_store | full_store_cache]) -> {value, aect_contracts:contract(), binary()} | none.
lookup_contract_with_code(Pubkey, Tree, Options) ->
    case lookup_contract(Pubkey, Tree, Options) of
        none -> none;
        {value, Contract} ->
            Code =
            case aect_contracts:code(Contract) of
                {code, C} -> C;
                {ref, Ref} ->
                    RefContractPK = aeser_id:specialize(Ref, contract),
                    {value, RefContract} = lookup_contract(RefContractPK, Tree, Options),
                    {code, C} = aect_contracts:code(RefContract),
                    C
            end,
            {value, Contract, Code}
    end.

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

-spec db(tree()) -> {ok, aeu_mp_trees:db()}.
db(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:db(CtTree).

-spec add_poi(aect_contracts:pubkey(), aect_state_tree:tree(), aec_poi:poi()) ->
                     {'ok', aec_poi:poi()}
                   | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, Tree, Poi) ->
    %% Flush both the store batch and the metadata batch for this contract
    %% before building the proof, so the MPT reflects the latest state.
    Tree1 = flush_contract_batch(Pubkey, Tree),
    Tree2 = flush_contract_meta_for(Pubkey, Tree1),
    #contract_tree{contracts = CtTree} = Tree2,
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

lookup_store_poi(Id, Poi) ->
    case aec_poi:read_only_subtree(Id, Poi) of
        {ok, Subtree} -> {ok, aect_contracts_store:new(Subtree)};
        {error, _} = Err -> Err
    end.

%% -- Microblock store batch --

%% Merge write_cache and read_cache for a contract into the microblock-level batch.
%% Both empty → no batch entry needed (fast path).
-spec batch_accumulate(aect_contracts:pubkey(), binary(),
                       #{binary() => binary()}, #{binary() => binary()},
                       store_batch()) -> store_batch().
batch_accumulate(_Pubkey, _StoreId, Writes, Reads, Batch)
  when map_size(Writes) =:= 0, map_size(Reads) =:= 0 ->
    Batch;
batch_accumulate(Pubkey, StoreId, Writes, Reads, Batch) ->
    {ExW, ExR} = case maps:find(Pubkey, Batch) of
        {ok, {_Id, W, R}} -> {W, R};
        error             -> {#{}, #{}}
    end,
    %% Current tx's writes override prior writes (last writer wins within a microblock).
    NewWrites = maps:merge(ExW, Writes),
    %% Accumulate stable MPT reads; overwrite any stale read entry with current writes
    %% so a subsequent cache lookup always returns the written value for that key.
    NewReads  = maps:merge(maps:merge(ExR, Reads), Writes),
    Batch#{Pubkey => {StoreId, NewWrites, NewReads}}.

%% Flush one contract's batch entry to the MPT, removing it from the batch.
%% Reads are ephemeral — only Writes are written to the MPT.
-spec flush_contract_batch(aect_contracts:pubkey(), tree()) -> tree().
flush_contract_batch(Pubkey, #contract_tree{contracts = CtTree, store_batch = Batch} = Tree) ->
    case maps:take(Pubkey, Batch) of
        {{StoreId, Writes, _Reads}, Batch1} when map_size(Writes) > 0 ->
            %% Sentinel <<0>> at StoreId is required by read_only_subtree to locate the prefix.
            CtTree1 = aeu_mtrees:enter(StoreId, <<0>>, CtTree),
            CtTree2 = enter_store_nodes(StoreId, Writes, CtTree1),
            Tree#contract_tree{contracts = CtTree2, store_batch = Batch1};
        {{_StoreId, _Writes, _Reads}, Batch1} ->
            Tree#contract_tree{store_batch = Batch1};
        error ->
            Tree
    end.

%% Flush all pending store writes to the MPT. Called once at microblock end.
%% Reads accumulated in the batch are ephemeral — they are not written to the MPT.
-spec flush_store_batch(tree()) -> tree().
flush_store_batch(#contract_tree{store_batch = Batch} = Tree) when map_size(Batch) =:= 0 ->
    Tree;
flush_store_batch(#contract_tree{contracts = CtTree, store_batch = Batch} = Tree) ->
    CtTree1 = maps:fold(
        fun(_Pubkey, {_StoreId, Writes, _Reads}, Acc) when map_size(Writes) =:= 0 -> Acc;
           (_Pubkey, {StoreId,  Writes, _Reads}, Acc) ->
               %% Sentinel <<0>> at StoreId is required by read_only_subtree to locate the prefix.
               Acc1 = aeu_mtrees:enter(StoreId, <<0>>, Acc),
               enter_store_nodes(StoreId, Writes, Acc1)
        end, CtTree, Batch),
    Tree#contract_tree{contracts = CtTree1, store_batch = #{}}.

%% Flush all pending contract metadata writes to the MPT. Called once at microblock end
%% (via aec_trees:flush_contract_store_batch). O(1) fast-path when batch is empty.
-spec flush_contract_meta_batch(tree()) -> tree().
flush_contract_meta_batch(#contract_tree{contract_meta_batch = MB} = Tree)
  when map_size(MB) =:= 0 ->
    Tree;
flush_contract_meta_batch(#contract_tree{contracts           = CtTree,
                                         contract_meta_batch = MB} = Tree) ->
    CtTree1 = maps:fold(fun(Pubkey, {new, Contract}, Acc) ->
                                aeu_mtrees:insert(Pubkey, aect_contracts:serialize(Contract), Acc);
                           (Pubkey, {update, Contract}, Acc) ->
                                aeu_mtrees:enter(Pubkey, aect_contracts:serialize(Contract), Acc)
                        end, CtTree, MB),
    Tree#contract_tree{contracts = CtTree1, contract_meta_batch = #{}}.

%% Flush one contract's metadata batch entry to the MPT. Used before PoI construction
%% for a single contract (analogous to flush_contract_batch/2 for the store batch).
-spec flush_contract_meta_for(aect_contracts:pubkey(), tree()) -> tree().
flush_contract_meta_for(Pubkey, #contract_tree{contracts           = CtTree,
                                               contract_meta_batch = MB} = Tree) ->
    case maps:take(Pubkey, MB) of
        {{new, Contract}, MB1} ->
            Serialized = aect_contracts:serialize(Contract),
            CtTree1    = aeu_mtrees:insert(Pubkey, Serialized, CtTree),
            Tree#contract_tree{contracts = CtTree1, contract_meta_batch = MB1};
        {{update, Contract}, MB1} ->
            Serialized = aect_contracts:serialize(Contract),
            CtTree1    = aeu_mtrees:enter(Pubkey, Serialized, CtTree),
            Tree#contract_tree{contracts = CtTree1, contract_meta_batch = MB1};
        error ->
            Tree
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
