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
-include_lib("aecontract/include/hard_forks.hrl").

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
%% This function is only used in tests
create_with_state(Block, KeyBlock, Txs, Trees) ->
    MBEnv = create_micro_block_env(Block, KeyBlock),
    TxEnv = create_tx_env(MBEnv),
    {ok, NewBlock, #{ trees := NewTrees}} =
        int_create_block(MBEnv, TxEnv, Txs, Trees),
    {NewBlock, NewTrees}.

-spec apply_block_txs(list(aetx_sign:signed_tx()), aec_trees:trees(),
                      aetx_env:env()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees(), aetx_env:events()}.
apply_block_txs(Txs, Trees, Env) ->
    {ok, Txs1, _InvalidTxs, Trees1, Events} = int_apply_block_txs(Txs, Trees, Env, false),
    {ok, Txs1, Trees1, Events}.


-spec apply_block_txs_strict(list(aetx_sign:signed_tx()), aec_trees:trees(),
                             aetx_env:env()) ->
        {ok, list(aetx_sign:signed_tx()), aec_trees:trees(), aetx_env:events()} | {error, term()}.
apply_block_txs_strict(Txs, Trees, Env) ->
    int_apply_block_txs(Txs, Trees, Env, true).

%% TODO NG: handle update after new keyblock in higher layer to get depth of microfork
-spec update(aec_blocks:block(), nonempty_list(aetx_sign:signed_tx()),
             block_info()) ->
                    {ok, aec_blocks:block(), block_info()} | {error, no_update_to_block_candidate | block_is_full}.
update(Block, Txs, BlockInfo = #{trees := Trees}) ->
    MaxGas = aec_governance:block_gas_limit(),
    OldTxs = aec_blocks:txs(Block),
    BlockGas = used_gas(Block, OldTxs, Trees),
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

int_create(PrevBlock, KeyBlock) ->
    MBEnv = #{prev_hash := PrevBlockHash} = create_micro_block_env(PrevBlock, KeyBlock),
    case aec_chain:get_block_state(PrevBlockHash) of
        {ok, Trees} ->
            TxEnv = create_tx_env(MBEnv),
            MaxGas = aec_governance:block_gas_limit(),
            int_pack_block(MaxGas, [], [], MBEnv, TxEnv, Trees, []);
        error ->
            {error, block_state_not_found}
    end.

create_micro_block_env(PrevBlock, PrevBlock) ->
    {ok, PrevBlockHash} = aec_blocks:hash_internal_representation(PrevBlock),
    KeyBlockHeader = aec_blocks:to_header(PrevBlock),
    #{prev_block => PrevBlock, prev_hash => PrevBlockHash,
      key_block => PrevBlock, key_hash => PrevBlockHash, key_header => KeyBlockHeader};
create_micro_block_env(PrevBlock, KeyBlock) ->
    {ok, PrevBlockHash} = aec_blocks:hash_internal_representation(PrevBlock),
    {ok, KeyBlockHash} = aec_blocks:hash_internal_representation(KeyBlock),
    KeyBlockHeader = aec_blocks:to_header(KeyBlock),
    #{prev_block => PrevBlock, prev_hash => PrevBlockHash,
      key_block => KeyBlock, key_hash => KeyBlockHash, key_header => KeyBlockHeader}.

create_tx_env(#{prev_block := PrevBlock, prev_hash := PrevBlockHash,
                key_hash := KeyBlockHash, key_header := KeyHeader}) ->
    Time = determine_new_time(PrevBlock),
    aetx_env:tx_env_from_key_header(KeyHeader, KeyBlockHash, Time, PrevBlockHash).

int_pack_block(GasAvailable, TxHashes, Txs, MBEnv, TxEnv, Trees, Events) ->
    #{prev_hash := PrevBlockHash, key_block := KeyBlock} = MBEnv,
    case aec_tx_pool:get_candidate(GasAvailable, TxHashes, PrevBlockHash) of
        {ok, []} ->
            int_create_block(MBEnv, TxEnv, Txs, Trees, Events);
        {ok, Txs0} ->
            {ok, Txs1, FailedTxs1, Trees1, Events1} = int_apply_block_txs(Txs0, Trees, TxEnv, false),
            report_failed_txs(FailedTxs1),
            TxHashes1 = [ aetx_sign:hash(Tx) || Tx <- Txs0 ],
            int_pack_block(GasAvailable - used_gas(KeyBlock, Txs1, Trees1), TxHashes ++ TxHashes1,
                           Txs ++ Txs1, MBEnv, TxEnv, Trees1, Events ++ Events1)
    end.

int_create_block(MBEnv, TxEnv, Txs, Trees) ->
    {ok, Txs1, InvalidTxs, Trees1, Events1} = int_apply_block_txs(Txs, Trees, TxEnv, false),
    report_failed_txs(InvalidTxs),
    int_create_block(MBEnv, TxEnv, Txs1, Trees1, Events1).

int_create_block(MBEnv, TxEnv, Txs, Trees, Events) ->
    #{key_block := KeyBlock, key_hash := KeyBlockHash,
      prev_block := PrevBlock, prev_hash := PrevBlockHash} = MBEnv,
    Time = aetx_env:time_in_msecs(TxEnv),
    Protocol = aec_blocks:version(KeyBlock),

    %% Heigth is relative to last key-block.
    ConsensusModule = aec_blocks:consensus_module(KeyBlock),
    Height = ConsensusModule:micro_block_height_relative_previous_block(key, aec_blocks:height(KeyBlock)),

    TxsTree = aec_txs_trees:from_txs(Txs),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsTree)),

    PoF = get_pof(KeyBlock, PrevBlockHash, PrevBlock),

    NewBlock = aec_blocks:new_micro(Height, PrevBlockHash, KeyBlockHash,
                                    aec_trees:hash(Trees), TxsRootHash, Txs,
                                    Time, PoF, Protocol),
    TxEnv1 = aetx_env:set_events(TxEnv, Events),
    BlockInfo = #{ trees => Trees, txs_tree => TxsTree, tx_env => TxEnv1},
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
    {ok, _Txs1, _InvalidTxs, _Trees1, _Events} =
        aec_trees:apply_txs_on_state_trees(Txs, Trees, Env);
%% strict
int_apply_block_txs(Txs, Trees, Env, true) ->
    BlockGasLimit = aec_governance:block_gas_limit(),
    Opts = [strict, tx_events, {gas_limit, BlockGasLimit}],
    case aec_trees:apply_txs_on_state_trees(Txs, Trees, Env, Opts) of
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
    Protocol = aetx_env:consensus_version(Env),
    Height = aetx_env:height(Env),
    TxGasLimit = aetx:gas_limit(aetx_sign:tx(Tx), Height, Protocol),
    case TxGasLimit =< MaxGas of
        true ->
            case aec_trees:apply_txs_on_state_trees([Tx], Trees, Env) of
                {ok, [], _, _, _} ->
                    add_txs_to_trees(MaxGas, Trees, Txs, Acc, Env);
                {ok, [Tx], _, Trees1, Events} ->
                    Env1 = aetx_env:set_events(Env, Events),
                    TxGas =
                        case Protocol =< ?IRIS_PROTOCOL_VSN of
                            true  -> TxGasLimit;
                            false -> aetx:used_gas(aetx_sign:tx(Tx), Height, Protocol, Trees1)
                        end,
                    add_txs_to_trees(MaxGas - TxGas, Trees1, Txs, [Tx | Acc], Env1)
            end;
        false ->
            {lists:reverse(Acc), Trees, Env}
    end.

report_failed_txs([])  -> ok;
report_failed_txs(Txs) -> aec_tx_pool:failed_txs(Txs).

used_gas(Block, Txs, Trees) ->
    Version = aec_blocks:version(Block),
    Height = aec_blocks:height(Block),
    GasFun =
        case Version =< ?IRIS_PROTOCOL_VSN of
            true  -> fun(Tx) -> aetx:gas_limit(aetx_sign:tx(Tx), Height, Version) end;
            false -> fun(Tx) -> aetx:used_gas(aetx_sign:tx(Tx), Height, Version, Trees) end
        end,
    lists:foldl(fun(Tx, Acc) -> GasFun(Tx) + Acc end, 0, Txs).

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
