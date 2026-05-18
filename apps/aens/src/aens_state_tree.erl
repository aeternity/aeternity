%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for keeping the state of Naming System
%%% @end
%%%=============================================================================

-module(aens_state_tree).

%% API
-export([commit_to_db/1,
         cache_root_hash/1,
         delete_commitment/2,
         delete_name_auction/2,
         delete_name/2,
         empty/0,
         empty_with_backend/0,
         enter_commitment/2,
         enter_name_auction/2,
         enter_name/2,
         get_name/2,
         prune/4,
         lookup_commitment/2,
         lookup_name_auction/2,
         lookup_name/2,
         new_with_backend/2,
         new_with_dirty_backend/2,
         root_hash/1,
         flush_name_batch/1,
         ns_db/1,
         cache_db/1,
         auction_iterator/1,
         auction_iterator_next/1]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        , from_db_format/1
        ]).

-export([record_fields/1]).

%% Export for test
-ifdef(TEST).
-export([ name_list/1
        , commitment_list/1
        ]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type mkey() :: aens_hash:commitment_hash() | aens_hash:name_hash() | aens_hash:auction_hash().
-type mvalue() :: aens_commitments:serialized() | aens_names:serialized() | aens_auctions:serialized().
-type nstree() :: aeu_mtrees:mtree(mkey(), mvalue()).
-type commitment() :: aens_commitments:commitment().
-type auction() :: aens_auctions:auction().
-type name() :: aens_names:name().
-type cache() :: aeu_mtrees:mtree(cache_key(), cache_value()).
-type cache_key() :: binary(). %% Sext encoded
-type cache_value() :: binary(). %% ?DUMMY_VAL
-type block_height() :: non_neg_integer().

%% Per-microblock deferred NS writes/deletes, keyed by mtree key. A
%% value entry replays through the unchanged writer (do_enter_*) at
%% flush, keeping the secondary TTL cache maintained; a `tombstone'
%% marks a delete (lookup_* report it absent; flush issues the real
%% delete). The cache is not part of the consensus root, so collapsing
%% repeated same-key writes does not change it.
-type nbatch_entry() :: {aens_commitments | aens_auctions | aens_names,
                         insert | enter, term()} | tombstone.
-type nbatch() :: #{binary() => nbatch_entry()}.

-record(ns_tree, { mtree  = aeu_mtrees:empty() :: nstree()
                 , cache  = aeu_mtrees:empty() :: cache()
                 , nbatch = #{}               :: nbatch()
                 }).

-opaque tree() :: #ns_tree{}.

-export_type([tree/0]).

-define(VSN, 1).

%% ==================================================================
%% Tracing support
record_fields(ns_tree) -> record_info(fields, ns_tree);
record_fields(_      ) -> no.
%% ==================================================================

%%%===================================================================
%%% API
%%%===================================================================

%% Defer the delete as a tombstone (overwriting any pending write for
%% the same key — last writer wins).  Honoured by lookup_* until flush,
%% when it becomes the real aeu_mtrees:delete.  Final key set, and thus
%% the consensus root, is identical to the immediate delete.
-spec delete_commitment(binary(), tree()) -> tree().
delete_commitment(Id, #ns_tree{nbatch = B} = Tree) ->
    Tree#ns_tree{nbatch = B#{Id => tombstone}}.

-spec delete_name_auction(binary(), tree()) -> tree().
delete_name_auction(Id, #ns_tree{nbatch = B} = Tree) ->
    Tree#ns_tree{nbatch = B#{Id => tombstone}}.

-spec delete_name(binary(), tree()) -> tree().
delete_name(Id, #ns_tree{nbatch = B} = Tree) ->
    Tree#ns_tree{nbatch = B#{Id => tombstone}}.

-spec empty() -> tree().
empty() ->
    #ns_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    MTree = aeu_mtrees:empty_with_backend(aec_db_backends:ns_backend()),
    Cache = aeu_mtrees:empty_with_backend(aec_db_backends:ns_cache_backend()),
    #ns_tree{mtree = MTree, cache = Cache}.

-spec from_db_format(tree()) -> tree().
from_db_format(Tree = #ns_tree{mtree = MTree, cache = Cache}) ->
    Tree#ns_tree{mtree = aeu_mtrees:from_db_format(MTree),
                 cache = aeu_mtrees:from_db_format(Cache)}.

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty',
                       aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(RootHash, CacheRootHash) ->
    MTree = aeu_mtrees:new_with_backend(RootHash, aec_db_backends:ns_backend()),
    Cache = aeu_mtrees:new_with_backend(CacheRootHash, aec_db_backends:ns_cache_backend()),
    #ns_tree{mtree = MTree, cache = Cache}.

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty',
                             aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(RootHash, CacheRootHash) ->
    MTree = aeu_mtrees:new_with_backend(RootHash, aec_db_backends:dirty_ns_backend()),
    Cache = aeu_mtrees:new_with_backend(CacheRootHash, aec_db_backends:dirty_ns_cache_backend()),
    #ns_tree{mtree = MTree, cache = Cache}.

-spec prune(block_height(), aec_hard_forks:protocol_vsn(), aec_trees:trees(), aetx_env:env()) -> {aec_trees:trees(), aetx_env:env()}.
prune(NextBlockHeight, Protocol, Trees, TxEnv) ->
    %% Flush first: int_prune walks the mtree+cache directly (defensive
    %% — at generation boundaries the batch is already empty).
    NTree0 = flush_name_batch(aec_trees:ns(Trees)),
    {NTree, ExpiredActions} = int_prune(NextBlockHeight - 1, NTree0),
    Trees1 = aec_trees:set_ns(Trees, NTree),
    run_elapsed(ExpiredActions, Trees1, Protocol, NextBlockHeight, TxEnv).

-spec auction_iterator(tree()) -> aeu_mtrees:iterator().
auction_iterator(Tree) ->
    #ns_tree{mtree = MTree} = flush_name_batch(Tree),
    aeu_mtrees:iterator(MTree).

-spec auction_iterator_next(aeu_mtrees:iterator()) ->
                                   {mkey(), mvalue(), aeu_mtrees:iterator()} | '$end_of_table'.
auction_iterator_next(Iter) ->
    case aeu_mtrees:iterator_next(Iter) of
        {Key, Value, NextIter} ->
            case byte_size(Key) > 32 of
                true ->
                    {Key, Value, NextIter};
                false ->
                    auction_iterator_next(NextIter)
            end;
        '$end_of_table' ->
            '$end_of_table'
    end.


run_elapsed([], Trees, _, _, TxEnv) ->
    {Trees, TxEnv};
run_elapsed([{aens_names, Id, Serialized}|Expired], Trees, Protocol, Height, TxEnv) ->
    Name = aens_names:deserialize(Id, Serialized),
    {ok, Trees1} = run_elapsed_name(Name, Trees, Height),
    run_elapsed(Expired, Trees1, Protocol, Height, TxEnv);
run_elapsed([{aens_auctions, Id, Serialized}|Expired], Trees, Protocol, Height, TxEnv) ->
    Auction = aens_auctions:deserialize(Id, Serialized),
    {ok, Trees1, TxEnv1} = run_elapsed_name_auction(Auction, Trees, Protocol, Height, TxEnv),
    run_elapsed(Expired, Trees1, Protocol, Height, TxEnv1);
run_elapsed([{aens_commitments, Id, Serialized}|Expired], Trees, Protocol, Height, TxEnv) ->
    Commitment = aens_commitments:deserialize(Id, Serialized),
    {ok, Trees1} = run_elapsed_commitment(Commitment, Trees),
    run_elapsed(Expired, Trees1, Protocol, Height, TxEnv).

%% Public enter_*: defer into the per-microblock batch (no mtree/cache
%% write here).  Last-writer-wins per key; the flush replays the final
%% object through do_enter_*/2 — the unchanged mtree+cache writer.
-spec enter_commitment(commitment(), tree()) -> tree().
enter_commitment(Commitment, #ns_tree{nbatch = B} = Tree) ->
    Hash = aens_commitments:hash(Commitment),
    Tree#ns_tree{nbatch = B#{Hash => {aens_commitments, insert, Commitment}}}.

-spec enter_name_auction(auction(), tree()) -> tree().
enter_name_auction(Auction, #ns_tree{nbatch = B} = Tree) ->
    Hash = aens_auctions:hash(Auction),
    Tree#ns_tree{nbatch = B#{Hash => {aens_auctions, enter, Auction}}}.

-spec enter_name(name(), tree()) -> tree().
enter_name(Name, #ns_tree{nbatch = B} = Tree) ->
    Hash = aens_names:hash(Name),
    Tree#ns_tree{nbatch = B#{Hash => {aens_names, enter, Name}}}.

%% The original (now flush-time) writers: mtree + secondary TTL cache.
do_enter_commitment(Commitment, Tree) ->
    CommitmentHash = aens_commitments:hash(Commitment),
    Serialized = aens_commitments:serialize(Commitment),
    TTL = aens_commitments:ttl(Commitment),
    %% TODO: consider two trees (names vs pre-claims/commitments)
    Cache1 = cache_push(TTL, CommitmentHash, aens_commitments, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:insert(CommitmentHash, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

do_enter_name_auction(Auction, Tree) ->
    AuctionHash = aens_auctions:hash(Auction),
    Serialized = aens_auctions:serialize(Auction),
    TTL = aens_auctions:ttl(Auction),
    Cache1 = cache_push(TTL, AuctionHash, aens_auctions, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:enter(AuctionHash, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

do_enter_name(Name, Tree) ->
    NameHash = aens_names:hash(Name),
    Serialized = aens_names:serialize(Name),
    TTL = aens_names:ttl(Name),
    Cache1 = cache_push(TTL, NameHash, aens_names, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:enter(NameHash, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

%% Flush all pending NS writes/deletes.  Called at microblock end via
%% aec_trees:flush_state_batches/1 and defensively at every whole-tree
%% reader/iterator/prune entry point.  O(1) fast-path when empty.
-spec flush_name_batch(tree()) -> tree().
flush_name_batch(#ns_tree{nbatch = B} = Tree) when map_size(B) =:= 0 ->
    Tree;
flush_name_batch(#ns_tree{nbatch = B} = Tree) ->
    Tree1 = Tree#ns_tree{nbatch = #{}},
    maps:fold(
      fun(Id, tombstone, Acc) ->
              Acc#ns_tree{mtree = aeu_mtrees:delete(Id, Acc#ns_tree.mtree)};
         (_Id, {aens_commitments, _How, C}, Acc) -> do_enter_commitment(C, Acc);
         (_Id, {aens_auctions,    _How, A}, Acc) -> do_enter_name_auction(A, Acc);
         (_Id, {aens_names,       _How, N}, Acc) -> do_enter_name(N, Acc)
      end, Tree1, B).

batch_lookup(Id, Deserialize, MTree, B) ->
    case maps:find(Id, B) of
        {ok, tombstone}        -> none;
        {ok, {_Mod, _How, Obj}} -> {value, Obj};
        error ->
            case aeu_mtrees:lookup(Id, MTree) of
                {value, Val} -> {value, Deserialize(Id, Val)};
                none         -> none
            end
    end.

-spec get_name(binary(), tree()) -> name().
get_name(Id, #ns_tree{mtree = MTree, nbatch = B}) ->
    case maps:find(Id, B) of
        {ok, {aens_names, _How, N}} -> N;
        _ -> aens_names:deserialize(Id, aeu_mtrees:get(Id, MTree))
    end.

-spec lookup_commitment(binary(), tree()) -> {value, commitment()} | none.
lookup_commitment(Id, #ns_tree{mtree = MTree, nbatch = B}) ->
    batch_lookup(Id, fun aens_commitments:deserialize/2, MTree, B).

-spec lookup_name_auction(binary(), tree()) -> {value, auction()} | none.
lookup_name_auction(Id, #ns_tree{mtree = MTree, nbatch = B}) ->
    batch_lookup(Id, fun aens_auctions:deserialize/2, MTree, B).

-spec lookup_name(binary(), tree()) -> {value, name()} | none.
lookup_name(Id, #ns_tree{mtree = MTree, nbatch = B}) ->
    batch_lookup(Id, fun aens_names:deserialize/2, MTree, B).

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    #ns_tree{mtree = MTree} = flush_name_batch(Tree),
    aeu_mtrees:root_hash(MTree).

%% WARNING: backing MPT db of the *materialised* mtree only — does not
%% reflect entries pending in `nbatch'.  Backend identity only.
-spec ns_db(tree()) -> {'ok', aeu_mp_trees:db()}.
ns_db(#ns_tree{mtree = MTree}) ->
    aeu_mtrees:db(MTree).

-spec cache_db(tree()) -> {'ok', aeu_mp_trees:db()}.
cache_db(#ns_tree{cache = CTree}) ->
    aeu_mtrees:db(CTree).

-spec cache_root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
cache_root_hash(Tree) ->
    #ns_tree{cache = CTree} = flush_name_batch(Tree),
    aeu_mtrees:root_hash(CTree).

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    #ns_tree{mtree = MTree, cache = Cache} = FT = flush_name_batch(Tree),
    FT#ns_tree{mtree = aeu_mtrees:commit_to_db(MTree),
               cache = aeu_mtrees:commit_to_db(Cache)
              }.

-ifdef(TEST).
-spec commitment_list(tree()) -> list(commitment()).
commitment_list(Tree0) ->
    #ns_tree{mtree = Tree} = flush_name_batch(Tree0),
    IsCommitment = fun(Id, MaybeC) ->
                       try [aens_commitments:deserialize(Id, MaybeC)]
                       catch _:_ -> [] end
                   end,
    [ C || {Id, Val} <- aeu_mtrees:to_list(Tree),
           C <- IsCommitment(Id, Val) ].

-spec name_list(tree()) -> list(name()).
name_list(Tree0) ->
    #ns_tree{mtree = Tree} = flush_name_batch(Tree0),
    IsName = fun(Id, MaybeC) ->
                 try [aens_names:deserialize(Id, MaybeC)]
                 catch _:_ -> [] end
             end,
    [ C || {Id, Val} <- aeu_mtrees:to_list(Tree),
           C <- IsName(Id, Val) ].
-endif.


-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(Tree) ->
    #ns_tree{mtree = MTree} = flush_name_batch(Tree),
    MTBin = aeu_mtrees:serialize(MTree),
    aeser_chain_objects:serialize(
        nameservice_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{mtree, MTBin}]).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{mtree, MTBin}] =
        aeser_chain_objects:deserialize(nameservice_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    MTree = aeu_mtrees:deserialize(MTBin),
    Cache = create_cache_from_mtree(MTree, aeu_mtrees:empty()),
    #ns_tree{mtree = MTree,
             cache = Cache}.

serialization_template(?VSN) ->
    [{mtree, binary}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

int_prune(NextBlockHeight, #ns_tree{cache = Cache, mtree = MTree} = Tree) ->
    {Cache1, Mtree1, ExpiredActions} = int_prune(cache_safe_peek(Cache), NextBlockHeight, Cache, MTree, []),
    {Tree#ns_tree{ cache = Cache1, mtree = Mtree1}, ExpiredActions}.

int_prune(none, _NextBlockHeight, Cache, MTree, ExpiredAcc) ->
    {Cache, MTree, lists:reverse(ExpiredAcc)};
int_prune({Height1,_Id,_Mod}, NextBlockHeight, Cache, MTree, ExpiredAcc) when NextBlockHeight < Height1 ->
    {Cache, MTree, lists:reverse(ExpiredAcc)};
int_prune({HeightLower, Id, Mod}, NextBlockHeight, Cache, MTree, ExpiredAcc) ->
    {{HeightLower, Id, Mod}, Cache1} = cache_pop(Cache),
    case aeu_mtrees:lookup(Id, MTree) of
        {value, ExpiredAction} ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, [{Mod, Id, ExpiredAction}|ExpiredAcc]);
        none ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, ExpiredAcc)
    end.

%% TODO update this figure, we now may start an auction
%% INFO: run_elapsed_name/3 and run_elapsed_commitment/2 implements 'expire' driven transitions:
%%
%%
%%                   expire
%%       unclaimed <----------------------- revoked
%%           | ^                               ^^
%%           | |                               || expire
%%           | | expire                        ||
%% pre-claim | |                               ||  _
%%           | |                        revoke || | | transfer
%%           v |                               || | v
%%      pre-claimed -------> auction ---->  claimed
%%                   claim           expire   | ^
%%                                            | |
%%                                             -
%%                                           update
%%



run_elapsed_name(Name, Trees, NextBlockHeight) ->
    NamesTree0 = aec_trees:ns(Trees),
    ExpirationBlockHeight = aens_names:ttl(Name),
    Status = aens_names:status(Name),
    case ExpirationBlockHeight =:= (NextBlockHeight - 1) of
        false ->
            %% INFO: Do nothing.
            %%       Name was updated and we triggered old cache event.
            {ok, Trees};
        true when Status =:= claimed ->
            NameHash = aens_names:hash(Name),
            Name0    = aens_state_tree:get_name(NameHash, NamesTree0),
            TTL      = aec_governance:name_protection_period(),
            Name1    = aens_names:revoke(Name0, TTL, ExpirationBlockHeight),
            NamesTree1 = aens_state_tree:enter_name(Name1, NamesTree0),
            {ok, aec_trees:set_ns(Trees, NamesTree1)};
        true when Status =:= revoked ->
            NameHash = aens_names:hash(Name),
            NamesTree1 = aens_state_tree:delete_name(NameHash, NamesTree0),
            {ok, aec_trees:set_ns(Trees, NamesTree1)}
    end.

run_elapsed_name_auction(Auction, Trees, Protocol, NextBlockHeight, TxEnv) ->
    NamesTree0 = aec_trees:ns(Trees),
    ExpirationBlockHeight = aens_auctions:ttl(Auction),
    case ExpirationBlockHeight =:= (NextBlockHeight - 1) of
        false ->
            %% INFO: Do nothing.
            %%       Name was updated and we triggered old cache event.
            {ok, Trees, TxEnv};
        true ->
            AuctionHash = aens_auctions:hash(Auction),
            NameHash   = aens_hash:from_auction_hash(AuctionHash),
            DeltaTTL   = aec_governance:name_claim_max_expiration(Protocol),
            AccountPubkey = aens_auctions:bidder_pubkey(Auction),
            Name       = aens_names:new(NameHash, AccountPubkey, ExpirationBlockHeight + DeltaTTL),
            NamesTree1 = aens_state_tree:delete_name_auction(AuctionHash, NamesTree0),
            NamesTree2 = aens_state_tree:enter_name(Name, NamesTree1),
            %% lock the final bid
            NameFee = aens_auctions:name_fee(Auction),
            ATree   = aec_trees:accounts(Trees),
            {ATree1, TxEnv1}  = lock_name_claim_fee(NameFee, ATree, TxEnv),
            {ok, aec_trees:set_accounts(
                   aec_trees:set_ns(Trees, NamesTree2), ATree1), TxEnv1}
    end.

run_elapsed_commitment(Commitment, Trees) ->
    %% INFO: We delete in both cases when name is claimed or not claimed
    %%       when it expires
    NamesTree0 = aec_trees:ns(Trees),
    CommitmentHash = aens_commitments:hash(Commitment),
    NamesTree1 = aens_state_tree:delete_commitment(CommitmentHash, NamesTree0),
    {ok,  aec_trees:set_ns(Trees, NamesTree1)}.

%%%===================================================================
%%% TTL Cache
%%%===================================================================
-define(DUMMY_VAL, <<0>>).

cache_push(TTL, Hash, Mod, C) ->
    SExt = sext:encode({TTL, Hash, Mod}),
    aeu_mtrees:enter(SExt, ?DUMMY_VAL, C).

cache_safe_peek(C) ->
    case aeu_mtrees:iterator_next(aeu_mtrees:iterator(C)) of
        '$end_of_table' -> none;
        {Next, ?DUMMY_VAL, _Iter} -> sext:decode(Next)
    end.

cache_pop(C) ->
    case aeu_mtrees:iterator_next(aeu_mtrees:iterator(C)) of
        '$end_of_table' -> none;
        {Next,?DUMMY_VAL,_Iter} ->
            {sext:decode(Next), aeu_mtrees:delete(Next, C)}
    end.

create_cache_from_mtree(MTree, EmptyCache) ->
    create_cache_from_mtree_(aeu_mtrees:iterator_next(
                               aeu_mtrees:iterator(MTree)), EmptyCache).

create_cache_from_mtree_('$end_of_table', Cache) -> Cache;
create_cache_from_mtree_({Key, Val, Iter}, Cache0) ->
    {Module, Obj} = deserialize_name_or_commitment(Key, Val),
    TTL = Module:ttl(Obj),
    Cache = cache_push(TTL, Key, Module, Cache0),
    create_cache_from_mtree_(aeu_mtrees:iterator_next(Iter), Cache).

deserialize_name_or_commitment(Hash, Bin) ->
    {Type, Vsn, RawFields} =
        aeser_chain_objects:deserialize_type_and_vsn(Bin),
    Name = aens_names:serialization_type(),
    Commitment = aens_commitments:serialization_type(),
    Module =
        case Type of
            Name       -> aens_names;
            Commitment -> aens_commitments
        end,
    Template = Module:serialization_template(Vsn),
    Fields = aeserialization:decode_fields(Template, RawFields),
    Obj = Module:deserialize_from_fields(Vsn, Hash, Fields),
    {Module, Obj}.

lock_name_claim_fee(NameFee, ATree, TxEnv) ->
    LockPubkey = aec_governance:locked_coins_holder_account(),
    TxEnv1 = aetx_env:tx_event(delta, {LockPubkey, NameFee}, <<"Chain.lock">>, TxEnv),
    case aec_accounts_trees:lookup(LockPubkey, ATree) of
        {value, Account} ->
            {ok, Account1} = aec_accounts:earn(Account, NameFee),
            {aec_accounts_trees:enter(Account1, ATree), TxEnv1};
        none ->
            {aec_accounts_trees:enter(
              aec_accounts:new(LockPubkey, NameFee), ATree), TxEnv1}
    end.
