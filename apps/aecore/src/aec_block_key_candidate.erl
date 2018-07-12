%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block creation.
%%% @end
%%%=============================================================================
-module(aec_block_key_candidate).

-export([ create/2
        ]).

-ifdef(TEST).
-export([adjust_target/2]).
-endif.

-include("blocks.hrl").

%% -- API functions ----------------------------------------------------------
-spec create(aec_blocks:block() | aec_blocks:block_header_hash(),
             aec_keys:pubkey()) ->
                    {ok, aec_blocks:block()} | {error, term()}.
create(BlockHash, Beneficiary) when is_binary(BlockHash) ->
    case aec_chain:get_block(BlockHash) of
        {ok, Block} ->
            int_create(BlockHash, Block, Beneficiary);
        error ->
            {error, block_not_found}
    end;
create(Block, Beneficiary) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    int_create(BlockHash, Block, Beneficiary).

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

int_create(BlockHash, Block, Beneficiary) ->
    N = aec_governance:key_blocks_to_check_difficulty_count(),
    case aec_blocks:height(Block) < N of
        true  ->
            int_create(BlockHash, Block, Beneficiary, []);
        false ->
            case aec_chain:get_n_generation_headers_backwards_from_hash(BlockHash, N) of
                {ok, Headers} ->
                    int_create(BlockHash, Block, Beneficiary, Headers);
                error ->
                    {error, headers_for_target_adjustment_not_found}
            end
    end.

int_create(BlockHash, Block, Beneficiary, AdjChain) ->
    case aec_keys:pubkey() of
        {ok, Miner} ->
            int_create(BlockHash, Block, Miner, Beneficiary, AdjChain);
        {error, _} = Error ->
            Error
    end.

int_create(PrevBlockHash, PrevBlock, Miner, Beneficiary, AdjChain) ->
    {ok, Trees} = aec_chain_state:calculate_state_for_new_keyblock(PrevBlockHash, Miner, Beneficiary),
    Block = int_create_block(PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees),
    case adjust_target(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock};
        {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
    end.

int_create_block(PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees) ->
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

    aec_blocks:new_key(Height, PrevBlockHash, aec_trees:hash(Trees),
                       aec_blocks:target(PrevBlock),
                       0, aeu_time:now_in_msecs(), Version, Miner, Beneficiary).
