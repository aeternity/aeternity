%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of contracts.
%%%
%%% Per-microblock dirty cache: contract metadata writes accumulate in
%%% `contract_meta_batch' and store-key writes/reads in `store_batch'.
%%% Both are flushed once at microblock end via
%%% `aec_trees:flush_state_batches/1'; `lookup_contract/3' and
%%% `is_contract/2' consult the batch before the MPT so cross-tx reads
%%% within a microblock stay O(1).
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

%% Per-microblock deferred contract metadata writes/deletes.
%% 'new' = inserted this microblock (flush uses aeu_mtrees:insert).
%% 'update' = already existed in the MPT when first seen (flush uses aeu_mtrees:enter).
%% 'tombstone' = deleted this microblock: lookup_contract/is_contract
%%   report it absent without touching the MPT; flush deletes the
%%   metadata key only (store subtree left as before — see
%%   delete_contract/2).
-type meta_entry() :: {new | update, aect_contracts:contract()} | tombstone.
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
gc_cache(Tree) ->
    #contract_tree{contracts = CtTree} = FT = flush_all_batches(Tree),
    FT#contract_tree{contracts = aeu_mtrees:gc_cache(CtTree)}.

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

%% Defer the delete: tombstone the metadata batch and drop pending
%% store/meta writes for PK. Flush deletes the metadata key only,
%% leaving the store subtree as the previous immediate delete did
%% (contract pubkey reuse is structurally impossible: pubkey =
%% hash(owner, nonce), nonce unique).
-spec delete_contract(aect_contracts:pubkey(), tree()) -> tree().
delete_contract(PK, Tree = #contract_tree{store_batch         = SB,
                                          contract_meta_batch = MB}) ->
    Tree#contract_tree{store_batch         = maps:remove(PK, SB),
                       contract_meta_batch = MB#{PK => tombstone}}.

-spec copy_contract_store(aect_contracts:contract(),
                          aect_contracts:pubkey(), tree()) -> tree().
copy_contract_store(Contract, NewId, Tree) ->
    %% Flush both batches for the source contract so `contents/1' sees all
    %% pending writes and the source store-id sentinel (written by
    %% `flush_contract_meta_for/2' for new contracts) is present.
    SrcPubkey = aect_contracts:pubkey(Contract),
    Tree1 = flush_contract_batch(SrcPubkey, Tree),
    Tree2 = flush_contract_meta_for(SrcPubkey, Tree1),
    #contract_tree{contracts = CtTree} = Tree2,
    Id    = aect_contracts:compute_contract_store_id(NewId),
    Store = aect_contracts:state(Contract),
    %% Write a value at the store id to make it possible to get it as a subtree.
    CtTree1 = aeu_mtrees:insert(Id, <<0>>, CtTree),
    CtTree2 = insert_store_nodes(Id, aect_contracts_store:contents(Store), CtTree1),
    Tree2#contract_tree{ contracts = CtTree2 }.

-spec read_contract_store(aect_contracts:pubkey(), tree()) ->
        {ok, aect_contracts_store:store()} | {error, term()}.
read_contract_store(StoreKey, Tree) ->
    %% Flush both batches for this contract so the MPT has both its
    %% pending writes and the store-id sentinel required by
    %% `aeu_mtrees:read_only_subtree/2'.
    Tree1 = flush_contract_batch(StoreKey, Tree),
    Tree2 = flush_contract_meta_for(StoreKey, Tree1),
    #contract_tree{contracts = CtTree} = Tree2,
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
                (_, <<>>, Tree) -> Tree;    %% Skip <<>> values: a pending
                                            %% empty/deleted entry must not be
                                            %% copied into the fresh subtree
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
    case maps:find(Pubkey, MB) of
        {ok, tombstone} -> false;     %% deleted this microblock
        {ok, _}         -> true;
        error           ->
            case aeu_mtrees:lookup(Pubkey, CtTree) of
                {value, _} -> true;
                none       -> false
            end
    end.

-spec lookup_contract(aect_contracts:pubkey(), tree()) -> {value, aect_contracts:contract()} | none.
lookup_contract(Pubkey, Tree) ->
    lookup_contract(Pubkey, Tree, []).

-spec lookup_contract(aect_contracts:pubkey(), tree(), [no_store | full_store_cache]) ->
        {value, aect_contracts:contract()} | none.
lookup_contract(Pubkey, Tree, Options) ->
    MB = Tree#contract_tree.contract_meta_batch,
    case maps:find(Pubkey, MB) of
        {ok, tombstone} ->
            %% Deleted this microblock: absent, do NOT consult the MPT.
            none;
        {ok, {_Tag, Contract}} ->
            %% Batch hit: O(1) map lookup, no MPT read.
            case proplists:get_value(no_store, Options, false) of
                true  -> {value, Contract};
                false -> {value, add_store(Contract, Tree, Options)}
            end;
        error ->
            CtTree = Tree#contract_tree.contracts,
            case aeu_mtrees:lookup(Pubkey, CtTree) of
                {value, Val} ->
                    Contract = aect_contracts:deserialize(Pubkey, Val),
                    case proplists:get_value(no_store, Options, false) of
                        false -> {value, add_store(Contract, Tree, Options)};
                        true  -> {value, Contract}
                    end;
                none -> none
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
root_hash(Tree) ->
    #contract_tree{contracts = CtTree} = flush_all_batches(Tree),
    aeu_mtrees:root_hash(CtTree).

-spec db(tree()) -> {ok, aeu_mp_trees:db()}.
db(#contract_tree{contracts = CtTree}) ->
    aeu_mtrees:db(CtTree).

-spec add_poi(aect_contracts:pubkey(), aect_state_tree:tree(), aec_poi:poi()) ->
                     {'ok', aec_poi:poi()}
                   | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, Tree, Poi) ->
    %% Flush the entire store and metadata batches before building the proof,
    %% so the resulting PoI root hash matches `root_hash(Tree)' (which itself
    %% flushes both batches).  A single-pubkey flush is insufficient: the PoI
    %% inherits the MPT root, and that root depends on every batched entry.
    #contract_tree{contracts = CtTree} = flush_all_batches(Tree),
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
to_list(Tree) ->
    #contract_tree{contracts = CtTree} = flush_all_batches(Tree),
    aeu_mtrees:to_list(CtTree).

lookup_store_poi(Id, Poi) ->
    case aec_poi:read_only_subtree(Id, Poi) of
        {ok, Subtree} -> {ok, aect_contracts_store:new(Subtree)};
        {error, _} = Err -> Err
    end.

%% -- Microblock store batch --

%% Flush both the store batch and the contract metadata batch.  Used by
%% read paths that need a canonical MPT view (`root_hash/1', `to_list/1',
%% `add_poi/3', ...) and by `commit_to_db/1' as a defensive sweep.
-spec flush_all_batches(tree()) -> tree().
flush_all_batches(Tree) ->
    flush_contract_meta_batch(flush_store_batch(Tree)).

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

%% Apply one store batch entry to the MPT: empty Writes leave the tree as
%% is, else write the store-id sentinel <<0>> then the store nodes.
-spec apply_store_entry(aect_contracts:pubkey(),
                        {binary(), #{binary() => binary()}, #{binary() => binary()}},
                        contract_tree()) -> contract_tree().
apply_store_entry(_Pubkey, {_StoreId, Writes, _Reads}, CtTree) when map_size(Writes) =:= 0 ->
    CtTree;
apply_store_entry(_Pubkey, {StoreId, Writes, _Reads}, CtTree) ->
    CtTree1 = aeu_mtrees:enter(StoreId, <<0>>, CtTree),
    enter_store_nodes(StoreId, Writes, CtTree1).

%% Flush one contract's batch entry to the MPT, removing it from the batch.
%% Reads are ephemeral — only Writes are written to the MPT.
-spec flush_contract_batch(aect_contracts:pubkey(), tree()) -> tree().
flush_contract_batch(Pubkey, #contract_tree{contracts = CtTree, store_batch = Batch} = Tree) ->
    case maps:take(Pubkey, Batch) of
        {Entry, Batch1} ->
            CtTree1 = apply_store_entry(Pubkey, Entry, CtTree),
            Tree#contract_tree{contracts = CtTree1, store_batch = Batch1};
        error ->
            Tree
    end.

%% Flush all pending store writes to the MPT. Called once at microblock end.
%% Reads accumulated in the batch are ephemeral — they are not written to the MPT.
-spec flush_store_batch(tree()) -> tree().
flush_store_batch(#contract_tree{store_batch = Batch} = Tree) when map_size(Batch) =:= 0 ->
    Tree;
flush_store_batch(#contract_tree{contracts = CtTree, store_batch = Batch} = Tree) ->
    CtTree1 = maps:fold(fun apply_store_entry/3, CtTree, Batch),
    Tree#contract_tree{contracts = CtTree1, store_batch = #{}}.

%% Apply one metadata batch entry to the MPT. `new' also writes the
%% store-id sentinel <<0>> eagerly (matching insert_store/2).
-spec apply_meta_entry(aect_contracts:pubkey(), meta_entry(), contract_tree()) ->
        contract_tree().
apply_meta_entry(Pubkey, tombstone, CtTree) ->
    %% Metadata key only — orphaned store subtree left as the immediate delete did.
    aeu_mtrees:delete(Pubkey, CtTree);
apply_meta_entry(Pubkey, {new, Contract}, CtTree) ->
    CtTree1 = aeu_mtrees:insert(Pubkey, aect_contracts:serialize(Contract), CtTree),
    aeu_mtrees:enter(aect_contracts:store_id(Contract), <<0>>, CtTree1);
apply_meta_entry(Pubkey, {update, Contract}, CtTree) ->
    aeu_mtrees:enter(Pubkey, aect_contracts:serialize(Contract), CtTree).

%% Flush all pending contract metadata writes to the MPT. Called once at
%% microblock end (via aec_trees:flush_state_batches); O(1) when empty.
-spec flush_contract_meta_batch(tree()) -> tree().
flush_contract_meta_batch(#contract_tree{contract_meta_batch = MB} = Tree)
  when map_size(MB) =:= 0 ->
    Tree;
flush_contract_meta_batch(#contract_tree{contracts           = CtTree,
                                         contract_meta_batch = MB} = Tree) ->
    CtTree1 = maps:fold(fun apply_meta_entry/3, CtTree, MB),
    Tree#contract_tree{contracts = CtTree1, contract_meta_batch = #{}}.

%% Flush one contract's metadata batch entry to the MPT. Used before PoI
%% construction for a single contract (analogous to flush_contract_batch/2 for
%% the store batch). For `new' entries the store-id sentinel is also written.
-spec flush_contract_meta_for(aect_contracts:pubkey(), tree()) -> tree().
flush_contract_meta_for(Pubkey, #contract_tree{contracts           = CtTree,
                                               contract_meta_batch = MB} = Tree) ->
    case maps:take(Pubkey, MB) of
        {Entry, MB1} ->
            CtTree1 = apply_meta_entry(Pubkey, Entry, CtTree),
            Tree#contract_tree{contracts = CtTree1, contract_meta_batch = MB1};
        error ->
            Tree
    end.

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    #contract_tree{contracts = CtTree} = FT = flush_all_batches(Tree),
    FT#contract_tree{contracts = aeu_mtrees:commit_to_db(CtTree)}.

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(Tree) ->
    #contract_tree{contracts = CtTree} = flush_all_batches(Tree),
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
