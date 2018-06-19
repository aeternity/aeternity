%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_key_candidate).

-export([ create/2
        , create_with_state/4
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
-spec create(aec_blocks:block() | aec_blocks:block_header_hash(), fees_info()) ->
        {ok, aec_blocks:block(), block_info()} | {error, term()}.
create(BlockHash, FeesInfo) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            int_create(BlockHash, Block, FeesInfo);
        error ->
            {error, block_not_found}
    end;
create(Block, FeesInfo) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    int_create(BlockHash, Block, FeesInfo).

-spec create_with_state(aec_blocks:block(), aec_keys:pubkey(),
                        aec_trees:trees(), fees_info()) ->
        {aec_blocks:block(), aec_trees:trees()}.
create_with_state(Block, Miner, Trees, FeesInfo) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    {ok, NewBlock, #{ trees := NewTrees}} =
        int_create_block(BlockHash, Block, FeesInfo, Trees, Miner),
    {NewBlock, NewTrees}.

-spec adjust_target(aec_blocks:block(), list(aec_headers:header())) ->
       {ok, aec_blocks:block()} | {error, term()}.
adjust_target(Block, AdjHeaders) ->
    Header = aec_blocks:to_header(Block),
    DeltaHeight = aec_governance:key_blocks_to_check_difficulty_count(),
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

%% -- Internal functions -----------------------------------------------------
int_create(BlockHash, Block, FeesInfo) ->
    case aec_chain:get_block_state(BlockHash) of
        {ok, Trees} ->
            int_create(BlockHash, Block, FeesInfo, Trees);
        error ->
            {error, block_state_not_found}
    end.

int_create(BlockHash, Block, FeesInfo, Trees) ->
    N = aec_governance:key_blocks_to_check_difficulty_count(),
    case aec_blocks:height(Block) < N of
        true  ->
            int_create(BlockHash, Block, FeesInfo, Trees, []);
        false ->
            case aec_chain:get_n_headers_backwards_from_hash(BlockHash, N) of
                {ok, Headers} ->
                    int_create(BlockHash, Block, FeesInfo, Trees, Headers);
                error ->
                    {error, headers_for_target_adjustment_not_found}
            end
    end.

int_create(BlockHash, Block, FeesInfo, Trees, AdjChain) ->
    case aec_keys:pubkey() of
        {ok, Miner} ->
            int_create(BlockHash, Block, FeesInfo, Trees, Miner, AdjChain);
        {error, _} = Error ->
            Error
    end.

int_create(PrevBlockHash, PrevBlock, FeesInfo, Trees, Miner, AdjChain) ->
    {ok, Block, BlockInfo} = int_create_block(PrevBlockHash, PrevBlock, FeesInfo, Trees, Miner),
    case adjust_target(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock, BlockInfo#{ adj_chain => AdjChain }};
        {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
    end.

int_create_block(PrevBlockHash, PrevBlock, FeesInfo, Trees, Miner) ->
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

    Trees2 = grant_fees(FeesInfo, Miner, Trees),

    NewBlock = aec_blocks:new_key(Height, PrevBlockHash, aec_trees:hash(Trees2),
                                  aec_blocks:target(PrevBlock),
                                  0, aeu_time:now_in_msecs(), Version, Miner),

    BlockInfo = #{ trees => Trees2, fees => FeesInfo},
    {ok, NewBlock, BlockInfo}.


-spec calculate_total_fee(fees_info()) -> non_neg_integer().
calculate_total_fee(#{txs := TxsFee, gas := GasFee}) ->
    aec_governance:block_mine_reward() + TxsFee + GasFee.

-spec grant_fees(list(aetx_sign:signed_tx()), aec_keys:pubkey(),
                 aec_trees:trees()) ->
                 {fees_info(), aec_trees:trees()}.
grant_fees(FeesInfo, Miner, Trees) ->
    _NewTrees = aec_trees:grant_fee_to_miner(Miner, Trees,
                                            calculate_total_fee(FeesInfo)).
