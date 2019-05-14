%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_micro_candidate).

-export([ apply_block_txs/3
        , apply_block_txs_strict/3
        , create/1
        , create_with_state/4
        , update/3
        ]).

-export([ min_t_after_keyblock/0]).

-export_type([block_info/0]).

-include("blocks.hrl").

-opaque block_info() :: #{trees := aec_trees:trees(),
                          txs_tree := aec_txs_trees:txs_tree(),
                          tx_env := aetx_env:env()
                         }.

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
            case aec_chain:get_block(aec_blocks:prev_key_hash(Block)) of
                {ok, KeyBlock} -> int_create(Block, KeyBlock);
                _ -> {error, block_not_found}
            end
    end.

-spec create_with_state(aec_blocks:block(), aec_blocks:block(),
    list(aetx_sign:signed_tx()), aec_trees:trees()) ->
    {aec_blocks:block(), aec_trees:trees()}.
create_with_state(Block, KeyBlock, Txs, Trees) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    {ok, NewBlock, #{ trees := NewTrees}} =
        int_create_block(BlockHash, Block, KeyBlock, Trees, Txs),
    {NewBlock, NewTrees}.

-spec apply_block_txs(list(aetx_sign:signed_tx()), aec_trees:trees(),
                      aetx_env:env()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees(), aetx_env:events()}.
apply_block_txs(Txs, Trees, Env) ->
    int_apply_block_txs(Txs, Trees, Env, false).

-spec apply_block_txs_strict(list(aetx_sign:signed_tx()), aec_trees:trees(),
                             aetx_env:env()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees(), aetx_env:events()} | {error, term()}.
apply_block_txs_strict(Txs, Trees, Env) ->
    int_apply_block_txs(Txs, Trees, Env, true).

%% TODO NG: handle update after new keyblock in higher layer to get depth of microfork
-spec update(aec_blocks:block(), nonempty_list(aetx_sign:signed_tx()),
             block_info()) ->
                    {ok, aec_blocks:block(), block_info()} | {error, no_update_to_block_candidate | block_is_full}.
update(Block, Txs, BlockInfo) ->
    MaxGas = aec_governance:block_gas_limit(),
    BlockGas = aec_blocks:gas(Block),
    case BlockGas < MaxGas of
        true ->
            SortedTxs = sort_txs(Txs),
            case int_update(MaxGas - BlockGas, Block, SortedTxs, BlockInfo) of
                {ok, _NewBlock, _NewBlockInfo} = Updated ->
                    Updated;
                {error, no_change} ->
                    {error, no_update_to_block_candidate}
            end;
        false ->
            {error, block_is_full}
    end.

%% -- Internal functions -----------------------------------------------------

int_create(Block, KeyBlock) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    case aec_chain:get_block_state(BlockHash) of
        {ok, Trees} ->
            int_create(BlockHash, Block, KeyBlock, Trees);
        error ->
            {error, block_state_not_found}
    end.

int_create(BlockHash, Block, KeyBlock, Trees) ->
    MaxGas = aec_governance:block_gas_limit(),
    {ok, Txs} = aec_tx_pool:get_candidate(MaxGas, BlockHash),
    int_create_block(BlockHash, Block, KeyBlock, Trees, Txs).

int_create_block(PrevBlockHash, PrevBlock, KeyBlock, Trees, Txs) ->
    PrevBlockHeight = aec_blocks:height(PrevBlock),

    %% Assert correctness of last block protocol version, as minimum
    %% sanity check on previous block and state (mainly for potential
    %% stale state persisted in DB and for development testing).
    ExpectedPrevBlockVersion =
        aec_hard_forks:protocol_effective_at_height(PrevBlockHeight),
    {ExpectedPrevBlockVersion, _} = {aec_blocks:version(PrevBlock),
                                     {expected, ExpectedPrevBlockVersion}},

    PrevKeyHash = case aec_blocks:type(PrevBlock) of
                      micro -> aec_blocks:prev_key_hash(PrevBlock);
                      key   -> PrevBlockHash
                  end,
    Height = aec_blocks:height(KeyBlock),
    Version = aec_hard_forks:protocol_effective_at_height(Height),

    Time = determine_new_time(PrevBlock),
    KeyHeader = aec_blocks:to_header(KeyBlock),
    Env = aetx_env:tx_env_from_key_header(KeyHeader, PrevKeyHash,
                                          Time, PrevBlockHash),
    {ok, Txs1, Trees2, Events} = int_apply_block_txs(Txs, Trees, Env, false),

    TxsTree = aec_txs_trees:from_txs(Txs1),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsTree)),

    PoF = get_pof(KeyBlock, PrevBlockHash, PrevBlock),

    NewBlock = aec_blocks:new_micro(Height, PrevBlockHash, PrevKeyHash,
                                    aec_trees:hash(Trees2), TxsRootHash, Txs1,
                                    Time, PoF, Version),
    Env1 = aetx_env:set_events(Env, Events),
    BlockInfo = #{ trees => Trees2, txs_tree => TxsTree, tx_env => Env1},
    {ok, NewBlock, BlockInfo}.

determine_new_time(PrevBlock) ->
    LastTime = aec_blocks:time_in_msecs(PrevBlock),
    case aec_blocks:type(PrevBlock) of
        key ->
            %% Just make sure we are progressing time.
            max(aeu_time:now_in_msecs(), LastTime + min_t_after_keyblock());
        micro ->
            %% Make sure to respect the micro block cycle.
            MicroBlockCycle = aec_governance:micro_block_cycle(),
            max(aeu_time:now_in_msecs(), MicroBlockCycle + LastTime)
    end.

min_t_after_keyblock() ->
    1.

get_pof(KeyBlock, PrevBlockHash, PrevBlock) ->
    %% NOTE: If the restriction of reporting a miner in the next
    %% generation is lifted, we need to do something more elaborate.
    MaybeFraudHash = aec_blocks:prev_key_hash(KeyBlock),
    case aec_db:find_discovered_pof(MaybeFraudHash) of
        none -> no_fraud;
        {value, PoF} ->
            %% Make sure we didn't report it already.
            case aec_blocks:type(PrevBlock) of
                key ->
                    PoF;
                micro ->
                    case aec_db:find_block_fraud_status(PrevBlockHash) of
                        {value, true}  -> no_fraud;
                        {value, false} -> PoF
                    end
            end
    end.


%% Non-strict
int_apply_block_txs(Txs, Trees, Env, false) ->
    {ok, Txs1, _InvalidTxs, Trees1, Events} =
        aec_trees:apply_txs_on_state_trees(Txs, Trees, Env),
    {ok, Txs1, Trees1, Events};
%% strict
int_apply_block_txs(Txs, Trees, Env, true) ->
    case aec_trees:apply_txs_on_state_trees_strict(Txs, Trees, Env) of
        {ok, Txs1, [], Trees1, Events} ->
            {ok, Txs1, Trees1, Events};
        Err = {error, _} ->
            Err
    end.

int_update(MaxGas, Block, Txs, BlockInfo) ->
    Env = maps:get(tx_env, BlockInfo),
    case add_txs_to_trees(MaxGas, maps:get(trees, BlockInfo), Txs, Env) of
        {[], _, _} ->
            {error, no_change};
        {Txs1, Trees1, Env1} ->
            Txs0 = aec_blocks:txs(Block),
            TxsTree1 = aec_txs_trees:add_txs(Txs1, length(Txs0), maps:get(txs_tree, BlockInfo)),
            {ok, TxsRootHash} = aec_txs_trees:root_hash(TxsTree1),
            NewBlock = aec_blocks:update_micro_candidate(
                         Block, TxsRootHash, aec_trees:hash(Trees1),
                         Txs0 ++ Txs1),
            NewBlockInfo = BlockInfo#{ trees => Trees1
                                     , txs_tree => TxsTree1
                                     , tx_env => Env1 },
            {ok, NewBlock, NewBlockInfo}
    end.

add_txs_to_trees(MaxGas, Trees, Txs, Env) ->
    add_txs_to_trees(MaxGas, Trees, Txs, [], Env).

add_txs_to_trees(_MaxGas, Trees, [], Acc, Env) ->
    {lists:reverse(Acc), Trees, Env};
add_txs_to_trees(MaxGas, Trees, [Tx | Txs], Acc, Env) ->
    TxGas = aetx:gas_limit(aetx_sign:tx(Tx), aetx_env:height(Env)),
    case TxGas =< MaxGas of
        true ->
            case aec_trees:apply_txs_on_state_trees([Tx], Trees, Env) of
                {ok, [], _, _, _} ->
                    add_txs_to_trees(MaxGas, Trees, Txs, Acc, Env);
                {ok, [Tx], _, Trees1, Events} ->
                    Env1 = aetx_env:set_events(Env, Events),
                    add_txs_to_trees(MaxGas - TxGas, Trees1, Txs, [Tx | Acc], Env1)
            end;
        false ->
            {lists:reverse(Acc), Trees, Env}
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
