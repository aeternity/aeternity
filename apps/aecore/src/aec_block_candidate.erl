%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_candidate).

-export([ apply_block_txs/5
        , apply_block_txs_strict/5
        , create/1
        , create_with_state/4
        , update/3
        ]).

-ifdef(TEST).
-export([adjust_target/2]).
-endif.

-export_type([block_info/0]).

-include("blocks.hrl").

-type fees_info() :: #{txs := non_neg_integer(),
                       gas := non_neg_integer()}.
-opaque block_info() :: #{trees := aec_trees:trees(),
                          fees := fees_info(),
                          txs_tree := aec_txs_trees:txs_tree(),
                          adj_chain := [aec_headers:header()]}.

%% -- API functions ----------------------------------------------------------
-spec create(aec_blocks:block() | aec_blocks:block_header_hash()) ->
        {ok, aec_blocks:block(), block_info()} | {error, term()}.
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

-spec update(aec_blocks:block(), nonempty_list(aetx_sign:signed_tx()),
             block_info()) ->
                    {ok, aec_blocks:block(), block_info()} | {error, no_change}.
update(Block, Txs, BlockInfo = #{ adj_chain := AdjChain }) ->
    NTxs = length(aec_blocks:txs(Block)),
    MaxTxs = aec_governance:max_txs_in_block(),
    case NTxs < MaxTxs of
        false ->
            {error, block_is_full};
        true  ->
            SortedTxs = sort_txs(Txs),
            case int_update(MaxTxs - NTxs, Block, SortedTxs, BlockInfo) of
                {ok, NewBlock, NewBlockInfo} ->
                    case adjust_target(NewBlock, AdjChain) of
                        {ok, AdjBlock} ->
                            {ok, AdjBlock, NewBlockInfo};
                        {error, Reason} ->
                            {error, {failed_to_adjust_target, Reason}}
                    end;
                {error, no_change} ->
                    {error, no_update_to_block_candidate}
            end
    end.

%% -- Internal functions -----------------------------------------------------
-spec calculate_txs_fee(list(aetx_sign:signed_tx())) -> non_neg_integer().
calculate_txs_fee(SignedTxs) ->
    lists:foldl(
        fun(SignedTx, TotalFee) ->
            Fee = aetx:fee(aetx_sign:tx(SignedTx)),
            TotalFee + Fee
        end, 0, SignedTxs).

-spec calculate_gas_fee(aect_call_state_tree:tree()) -> non_neg_integer().
calculate_gas_fee(Calls) ->
    F = fun(_, SerCall, GasFeeIn) ->
                Call = aect_call:deserialize(SerCall),
                GasFee = aect_call:gas_used(Call) * aect_call:gas_price(Call),
                GasFee + GasFeeIn
        end,
    aeu_mtrees:fold(F, 0, aect_call_state_tree:iterator(Calls)).

-spec calculate_total_fee(fees_info()) -> non_neg_integer().
calculate_total_fee(#{txs := TxsFee, gas := GasFee}) ->
    aec_governance:block_mine_reward() + TxsFee + GasFee.

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
        {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
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

    {ok, Txs1, Trees2, Fees} =
        int_apply_block_txs(Txs, Miner, Trees, Height, Version, false),

    TxsTree = aec_txs_trees:from_txs(Txs1),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsTree)),

    NewBlock = aec_blocks:new(Height, PrevBlockHash, aec_trees:hash(Trees2),
                              TxsRootHash, Txs1, aec_blocks:target(PrevBlock),
                              0, aeu_time:now_in_msecs(), Version, Miner),

    BlockInfo = #{ trees => Trees2, fees => Fees, txs_tree => TxsTree },
    {ok, NewBlock, BlockInfo}.

%% Non-strict
int_apply_block_txs(Txs, Miner, Trees, Height, Version, false) ->
    Trees0 = aec_trees:perform_pre_transformations(Trees, Height),
    {ok, Txs1, Trees1} =
        aec_trees:apply_txs_on_state_trees(Txs, Trees0, Height, Version),
    {Fees, Trees2} = calculate_and_grant_fees(Txs1, Miner, Trees1),
    {ok, Txs1, Trees2, Fees};
%% strict
int_apply_block_txs(Txs, Miner, Trees, Height, Version, true) ->
    Trees0 = aec_trees:perform_pre_transformations(Trees, Height),
    case aec_trees:apply_txs_on_state_trees_strict(Txs, Trees0, Height, Version) of
        {ok, Txs1, Trees1} ->
            {Fees, Trees2} = calculate_and_grant_fees(Txs1, Miner, Trees1),
            {ok, Txs1, Trees2, Fees};
        Err = {error, _} ->
            Err
    end.

-spec calculate_and_grant_fees(list(aetx_sign:signed_tx()), aec_keys:pubkey(),
                               aec_trees:trees()) ->
                                      {fees_info(), aec_trees:trees()}.
calculate_and_grant_fees(Txs, Miner, Trees) ->
    FeesInfo = #{txs => calculate_txs_fee(Txs),
                 gas => calculate_gas_fee(aec_trees:calls(Trees))},
    NewTrees = aec_trees:grant_fee_to_miner(Miner, Trees,
                                            calculate_total_fee(FeesInfo)),
    {FeesInfo, NewTrees}.

int_update(MaxNTxs, Block, Txs, BlockInfo) ->
    case add_txs_to_trees(MaxNTxs, maps:get(trees, BlockInfo), Txs,
                          aec_blocks:height(Block), aec_blocks:version(Block)) of
        {[], _} ->
            {error, no_change};
        {Txs1, Trees1} ->
            Txs0 = aec_blocks:txs(Block),
            Txs1Fee = calculate_txs_fee(Txs1),
            %% Re-calculate whole new gas fee, as computing gas fee
            %% only for added txs is complex.
            NewGasFee = calculate_gas_fee(aec_trees:calls(Trees1)),
            FeesInfo = maps:get(fees, BlockInfo),
            AddGasFee = case NewGasFee - maps:get(gas, FeesInfo) of X when X >= 0 -> X end,
            AddFee = Txs1Fee + AddGasFee,
            Trees2 = aec_trees:grant_fee_to_miner(aec_blocks:miner(Block), Trees1, AddFee),
            TxsTree1 = aec_txs_trees:add_txs(Txs1, length(Txs0), maps:get(txs_tree, BlockInfo)),
            {ok, TxsRootHash} = aec_txs_trees:root_hash(TxsTree1),
            NewBlock = Block#block{ txs_hash = TxsRootHash
                                  , root_hash = aec_trees:hash(Trees2)
                                  , txs =  Txs0 ++ Txs1
                                  , time = aeu_time:now_in_msecs() },
            NewFeesInfo = FeesInfo#{txs := maps:get(txs, FeesInfo) + Txs1Fee,
                                    gas := NewGasFee},
            NewBlockInfo = BlockInfo#{ trees => Trees2, fees => NewFeesInfo,
                                       txs_tree => TxsTree1 },
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
        {ok, [], _} ->
            add_txs_to_trees(N, Trees, Txs, Acc, Height, Version);
        {ok, [Tx], Trees1} ->
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
