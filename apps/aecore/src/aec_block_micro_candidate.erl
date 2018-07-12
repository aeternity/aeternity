%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_micro_candidate).

-export([ apply_block_txs/4
        , apply_block_txs_strict/4
        , create/1
        , create_with_state/4
        , update/3
        ]).

-export_type([block_info/0]).

-include("blocks.hrl").

-opaque block_info() :: #{trees := aec_trees:trees(),
                          txs_tree := aec_txs_trees:txs_tree(),
                          adj_chain := [aec_headers:header()]}.

%% -- API functions ----------------------------------------------------------

-spec create(aec_blocks:block() | aec_blocks:block_header_hash()) ->
        {ok, aec_blocks:block(), block_info()} | {error, term()}.
create(BlockHash) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} -> create(Block);
        _ -> {error, block_not_found}
    end;
create(Block) ->
    case aec_blocks:is_key_block(Block) of
        true -> int_create(Block, Block);
        false ->
            KeyBlockHash = aec_blocks:key_hash(Block),
            case aec_chain:get_block(KeyBlockHash) of
                {ok, KeyBlock} -> int_create(Block, KeyBlock);
                _ -> {error, block_not_found}
            end
    end.

-spec create_with_state(aec_blocks:block(), aec_blocks:block(),
    list(aetx_sign:signed_tx()), aec_trees:trees()) ->
    {aec_blocks:block(), aec_trees:trees()}.
create_with_state(Block, KeyBlock, Txs, Trees) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    {ok, KeyBlockHash} = aec_blocks:hash_internal_representation(KeyBlock),
    {ok, NewBlock, #{ trees := NewTrees}} =
        int_create_block(BlockHash, Block, KeyBlockHash, KeyBlock, Trees, Txs),
    {NewBlock, NewTrees}.

-spec apply_block_txs(list(aetx_sign:signed_tx()), aec_trees:trees(),
                      aec_blocks:height(), non_neg_integer()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees()}.
apply_block_txs(Txs, Trees, Height, Version) ->
    int_apply_block_txs(Txs, Trees, Height, Version, false).

-spec apply_block_txs_strict(list(aetx_sign:signed_tx()),
                             aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees()} | {error, term()}.
apply_block_txs_strict(Txs, Trees, Height, Version) ->
    case int_apply_block_txs(Txs, Trees, Height, Version, true) of
        Err = {error, _}      -> Err;
        {ok, Txs1, Trees1} -> {ok, Txs1, Trees1}
    end.

%% TODO NG: handle update after new keyblock in higher layer to get depth of microfork
-spec update(aec_blocks:block(), nonempty_list(aetx_sign:signed_tx()),
             block_info()) ->
                    {ok, aec_blocks:block(), block_info()} | {error, no_change}.
update(Block, Txs, BlockInfo) ->
    NTxs = length(aec_blocks:txs(Block)),
    MaxTxs = aec_governance:max_txs_in_block(),
    case NTxs < MaxTxs of
        false ->
            {error, block_is_full};
        true  ->
            SortedTxs = sort_txs(Txs),
            case int_update(MaxTxs - NTxs, Block, SortedTxs, BlockInfo) of
                {ok, _NewBlock, _NewBlockInfo} = Updated ->
                    Updated;
                {error, no_change} ->
                    {error, no_update_to_block_candidate}
            end
    end.

%% -- Internal functions -----------------------------------------------------

int_create(Block, KeyBlock) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    {ok, KeyBlockHash} = aec_blocks:hash_internal_representation(KeyBlock),
    case aec_chain:get_block_state(BlockHash) of
        {ok, Trees} ->
            int_create(BlockHash, Block, KeyBlockHash, KeyBlock, Trees);
        error ->
            {error, block_state_not_found}
    end.

int_create(BlockHash, Block, KeyBlockHash, KeyBlock, Trees) ->
    MaxN = aec_governance:max_txs_in_block(),
    {ok, Txs} = aec_tx_pool:get_candidate(MaxN, BlockHash),
    int_create_block(BlockHash, Block, KeyBlockHash, KeyBlock, Trees, Txs).

int_create_block(PrevBlockHash, PrevBlock, KeyBlockHash, KeyBlock, Trees, Txs) ->
    PrevBlockHeight = aec_blocks:height(PrevBlock),

    %% Assert correctness of last block protocol version, as minimum
    %% sanity check on previous block and state (mainly for potential
    %% stale state persisted in DB and for development testing).
    ExpectedPrevBlockVersion =
        aec_hard_forks:protocol_effective_at_height(PrevBlockHeight),
    {ExpectedPrevBlockVersion, _} = {aec_blocks:version(PrevBlock),
                                     {expected, ExpectedPrevBlockVersion}},

    Height = aec_blocks:height(KeyBlock),
    Version = aec_hard_forks:protocol_effective_at_height(Height),

    {ok, Txs1, Trees2} =
        int_apply_block_txs(Txs, Trees, Height, Version, false),

    TxsTree = aec_txs_trees:from_txs(Txs1),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsTree)),

    NewBlock = aec_blocks:new_micro(Height, PrevBlockHash, KeyBlockHash,
                                    aec_trees:hash(Trees2), TxsRootHash, Txs1,
                                    aeu_time:now_in_msecs(), Version),

    BlockInfo = #{ trees => Trees2, txs_tree => TxsTree },
    {ok, NewBlock, BlockInfo}.

%% Non-strict
int_apply_block_txs(Txs, Trees, Height, Version, false) ->
    {ok, Txs1, _InvalidTxs, Trees1} =
        aec_trees:apply_txs_on_state_trees(Txs, Trees, Height, Version),
    {ok, Txs1, Trees1};
%% strict
int_apply_block_txs(Txs, Trees, Height, Version, true) ->
    case aec_trees:apply_txs_on_state_trees_strict(Txs, Trees, Height, Version) of
        {ok, Txs1, [], Trees1} ->
            {ok, Txs1, Trees1};
        Err = {error, _} ->
            Err
    end.

int_update(MaxNTxs, Block, Txs, BlockInfo) ->
    case add_txs_to_trees(MaxNTxs, maps:get(trees, BlockInfo), Txs,
                          aec_blocks:height(Block), aec_blocks:version(Block)) of
        {[], _} ->
            {error, no_change};
        {Txs1, Trees1} ->
            Txs0 = aec_blocks:txs(Block),
            TxsTree1 = aec_txs_trees:add_txs(Txs1, length(Txs0), maps:get(txs_tree, BlockInfo)),
            {ok, TxsRootHash} = aec_txs_trees:root_hash(TxsTree1),
            NewBlock = Block#block{ txs_hash = TxsRootHash
                                  , root_hash = aec_trees:hash(Trees1)
                                  , txs =  Txs0 ++ Txs1
                                  , time = aeu_time:now_in_msecs() },
            NewBlockInfo = BlockInfo#{ trees => Trees1
                                     , txs_tree => TxsTree1 },
            {ok, NewBlock, NewBlockInfo}
    end.

add_txs_to_trees(MaxN, Trees, Txs, Height, Version) ->
    add_txs_to_trees(MaxN, Trees, Txs, [], Height, Version).

add_txs_to_trees(0, Trees, _Txs, Acc, _Height, _Version) ->
    {lists:reverse(Acc), Trees};
add_txs_to_trees(_N, Trees, [], Acc, _Height, _Version) ->
    {lists:reverse(Acc), Trees};
add_txs_to_trees(N, Trees, [Tx | Txs], Acc, Height, Version) ->
    case aec_trees:apply_txs_on_state_trees([Tx], Trees, Height, Version) of
        {ok, [], _, _} ->
            add_txs_to_trees(N, Trees, Txs, Acc, Height, Version);
        {ok, [Tx], _, Trees1} ->
            add_txs_to_trees(N+1, Trees1, Txs, [Tx | Acc], Height, Version)
    end.

%% Respect nonces order
sort_txs(Txs) ->
    Cmp =
        fun(STx1, STx2) ->
            Tx1 = aetx_sign:tx(STx1),
            Tx2 = aetx_sign:tx(STx2),
            {O1, N1} = {aetx:origin(Tx1), aetx:nonce(Tx1)},
            {O2, N2} = {aetx:origin(Tx2), aetx:nonce(Tx2)},
            {O1, N1} =< {O2, N2}
        end,
    lists:sort(Cmp, Txs).
