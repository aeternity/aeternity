%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_candidate).

-export([ apply_block_txs/5
        , apply_block_txs_strict/5
        , calculate_fee/1
        , create/1
        , create_with_state/4
        ]).

-export([adjust_target/2]).

%% -- API functions ----------------------------------------------------------
-spec create(aec_blocks:block() | aec_blocks:block_header_hash()) ->
        {ok, aec_blocks:block(), term()} | {error, term()}.
create(BlockHash) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            int_create(BlockHash, Block);
        error ->
            {error, block_not_found}
    end;
create(Block) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    int_create(BlockHash, Block).

-spec apply_block_txs(list(aetx_sign:signed_tx()), aec_keys:pubkey(), aec_trees:trees(),
                      aec_blocks:height(), non_neg_integer()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees()}.
apply_block_txs(Txs, Miner, Trees, Height, Version) ->
    {ok, Txs1, Trees1, _} = int_apply_block_txs(Txs, Miner, Trees, Height, Version, false),
    {ok, Txs1, Trees1}.

-spec apply_block_txs_strict(list(aetx_sign:signed_tx()), aec_keys:pubkey(),
                             aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees()} | {error, term()}.
apply_block_txs_strict(Txs, Miner, Trees, Height, Version) ->
    case int_apply_block_txs(Txs, Miner, Trees, Height, Version, true) of
        Err = {error, _}      -> Err;
        {ok, Txs1, Trees1, _} -> {ok, Txs1, Trees1}
    end.

-spec create_with_state(aec_blocks:block(), aec_keys:pubkey(),
                        list(aetx_sign:signed_tx()), aec_trees:trees()) ->
        {aec_blocks:block(), aec_trees:trees()}.
create_with_state(Block, Miner, Txs, Trees) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    {ok, NewBlock, #{ trees := NewTrees}} =
        int_create_block(BlockHash, Block, Trees, Miner, Txs),
    {NewBlock, NewTrees}.

-spec adjust_target(aec_blocks:block(), list(aec_headers:header())) ->
       {ok, aec_blocks:block()} | {error, term()}.
adjust_target(Block, AdjHeaders) ->
    Header = aec_blocks:to_header(Block),
    DeltaHeight = aec_governance:blocks_to_check_difficulty_count(),
    case aec_headers:height(Header) =< DeltaHeight of
        true ->
            %% For the first DeltaHeight blocks, use pre-defined target
            {ok, Block};
        false when DeltaHeight == length(AdjHeaders) ->
            CalculatedTarget = aec_target:recalculate(Header, AdjHeaders),
            Block1 = aec_blocks:set_target(Block, CalculatedTarget),
            {ok, Block1};
        false -> %% Wrong number of headers in AdjHeaders...
            {error, {wrong_headers_for_target_adjustment, DeltaHeight, length(AdjHeaders)}}
    end.

-spec calculate_fee(list(aetx_sign:signed_tx())) -> non_neg_integer().
calculate_fee(SignedTxs) ->
    lists:foldl(
        fun(SignedTx, TotalFee) ->
            Fee = aetx:fee(aetx_sign:tx(SignedTx)),
            TotalFee + Fee
        end, 0, SignedTxs).

%% -- Internal functions -----------------------------------------------------
-spec calculate_total_fee(list(aetx_sign:signed_tx())) -> non_neg_integer().
calculate_total_fee(SignedTxs) ->
    TxsFee = calculate_fee(SignedTxs),
    aec_governance:block_mine_reward() + TxsFee.

int_create(BlockHash, Block) ->
    case aec_chain:get_block_state(BlockHash) of
        {ok, Trees} ->
            int_create(BlockHash, Block, Trees);
        error ->
            {error, block_state_not_found}
    end.

int_create(BlockHash, Block, Trees) ->
    N = aec_governance:blocks_to_check_difficulty_count(),
    case aec_blocks:height(Block) < N of
        true  ->
            int_create(BlockHash, Block, Trees, []);
        false ->
            case aec_chain:get_n_headers_backwards_from_hash(BlockHash, N) of
                {ok, Headers} ->
                    int_create(BlockHash, Block, Trees, Headers);
                error ->
                    {error, headers_for_target_adjustment_not_found}
            end
    end.

int_create(BlockHash, Block, Trees, AdjChain) ->
    MaxN = aec_governance:max_txs_in_block(),
    {ok, Txs} = aec_tx_pool:get_candidate(MaxN, BlockHash),
    case aec_keys:pubkey() of
        {ok, Miner} ->
            int_create(BlockHash, Block, Trees, Miner, Txs, AdjChain);
        {error, _} = Error ->
            Error
    end.

int_create(PrevBlockHash, PrevBlock, Trees, Miner, Txs, AdjChain) ->
    {ok, Block, BlockInfo} = int_create_block(PrevBlockHash, PrevBlock, Trees, Miner, Txs),
    case adjust_target(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock, BlockInfo#{ adj_chain => AdjChain }};
        {error, _}     -> {error, failed_to_adjust_target}
    end.

int_create_block(PrevBlockHash, PrevBlock, Trees, Miner, Txs) ->
    PrevBlockHeight = aec_blocks:height(PrevBlock),

    %% Assert correctness of last block protocol version, as minimum
    %% sanity check on previous block and state (mainly for potential
    %% stale state persisted in DB and for development testing).
    ExpectedPrevBlockVersion =
        aec_hard_forks:protocol_effective_at_height(PrevBlockHeight),
    {ExpectedPrevBlockVersion, _} = {aec_blocks:version(PrevBlock),
                                     {expected, ExpectedPrevBlockVersion}},

    Height = PrevBlockHeight + 1,
    Version = aec_hard_forks:protocol_effective_at_height(Height),

    {ok, Txs1, Trees2, TotFee} =
        int_apply_block_txs(Txs, Miner, Trees, Height, Version, false),

    TxsTree = aec_txs_trees:from_txs(Txs1),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsTree)),

    NewBlock = aec_blocks:new(Height, PrevBlockHash, aec_trees:hash(Trees2),
                              TxsRootHash, Txs1, aec_blocks:target(PrevBlock),
                              0, aeu_time:now_in_msecs(), Version, Miner),

    BlockInfo = #{ trees => Trees2, tot_fee => TotFee, txs_tree => TxsTree },
    {ok, NewBlock, BlockInfo}.

%% Non-strict
int_apply_block_txs(Txs, Miner, Trees, Height, Version, false) ->
    Trees0 = aec_trees:perform_pre_transformations(Trees, Height),
    {ok, Txs1, Trees1} =
        aec_trees:apply_txs_on_state_trees(Txs, Trees0, Height, Version),
    TotFee = calculate_total_fee(Txs1),
    Trees2 = aec_trees:grant_fee_to_miner(Miner, Trees1, TotFee),
    {ok, Txs1, Trees2, TotFee};
%% strict
int_apply_block_txs(Txs, Miner, Trees, Height, Version, true) ->
    Trees0 = aec_trees:perform_pre_transformations(Trees, Height),
    case aec_trees:apply_txs_on_state_trees_strict(Txs, Trees0, Height, Version) of
        {ok, Txs1, Trees1} ->
            TotFee = calculate_total_fee(Txs1),
            Trees2 = aec_trees:grant_fee_to_miner(Miner, Trees1, TotFee),
            {ok, Txs1, Trees2, TotFee};
        Err = {error, _} ->
            Err
    end.

