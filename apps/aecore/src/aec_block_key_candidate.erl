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

-include_lib("aeminer/include/aeminer.hrl").
-include("blocks.hrl").

%% -- API functions ----------------------------------------------------------
-spec create(aec_blocks:block(), aec_keys:pubkey()) ->
                    {ok, aec_blocks:block()} | {error, term()}.
create(PrevBlock, Beneficiary) ->
    PrevHeight = aec_blocks:height(PrevBlock),
    {ok, PrevBlockHash} = aec_blocks:hash_internal_representation(PrevBlock),
    case aec_hard_forks:protocol_effective_at_height(PrevHeight + 1) of
        {ok, Protocol} ->
            int_create(PrevBlockHash, PrevBlock, Beneficiary, Protocol);
        {error, _Rsn} = Err ->
            Err
    end.

-spec adjust_target(aec_blocks:block(), list(aec_headers:header())) ->
                           {ok, aec_blocks:block()} | {error, term()}.
adjust_target(Block, AdjHeaders) ->
    Header = aec_blocks:to_header(Block),
    DeltaHeight = aec_governance:key_blocks_to_check_difficulty_count() + 1,
    case aec_headers:height(Header) =< DeltaHeight of
        true ->
            %% For the first DeltaHeight blocks, use pre-defined target
            Target = aec_block_genesis:target(),
            {ok, aec_blocks:set_target(Block, Target)};
        false when DeltaHeight == length(AdjHeaders) ->
            CalculatedTarget = aec_target:recalculate(AdjHeaders),
            Block1 = aec_blocks:set_target(Block, CalculatedTarget),
            {ok, Block1};
        false -> %% Wrong number of headers in AdjHeaders...
            {error, {wrong_headers_for_target_adjustment, DeltaHeight, length(AdjHeaders)}}
    end.

%% -- Internal functions -----------------------------------------------------

int_create(PrevBlockHash, PrevBlock, Beneficiary, Protocol) ->
    N = aec_governance:key_blocks_to_check_difficulty_count() + 1,
    case aec_blocks:height(PrevBlock) < N of
        true  ->
            int_create(PrevBlockHash, PrevBlock, Beneficiary, [], Protocol);
        false ->
            case aec_chain:get_n_generation_headers_backwards_from_hash(PrevBlockHash, N) of
                {ok, Headers} ->
                    int_create(PrevBlockHash, PrevBlock, Beneficiary, Headers, Protocol);
                error ->
                    {error, headers_for_target_adjustment_not_found}
            end
    end.

int_create(PrevBlockHash, PrevBlock, Beneficiary, AdjChain, Protocol) ->
    case aec_keys:candidate_pubkey() of
        {ok, Miner} ->
            int_create(PrevBlockHash, PrevBlock, Miner, Beneficiary, AdjChain, Protocol);
        {error, _} = Error ->
            Error
    end.

int_create(PrevBlockHash, PrevBlock, Miner, Beneficiary, AdjChain, Protocol) ->
    {ok, Trees} = aec_chain_state:calculate_state_for_new_keyblock(PrevBlockHash, Miner, Beneficiary),
    Block = int_create_block(PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol),
    case adjust_target(Block, AdjChain) of
        {ok, AdjBlock}  -> {ok, AdjBlock};
        {error, Reason} -> {error, {failed_to_adjust_target, Reason}}
    end.

int_create_block(PrevBlockHash, PrevBlock, Miner, Beneficiary, Trees, Protocol) ->
    PrevBlockHeight = aec_blocks:height(PrevBlock),
    PrevKeyHash =
        case aec_blocks:type(PrevBlock) of
            micro -> aec_blocks:prev_key_hash(PrevBlock);
            key   -> PrevBlockHash
        end,
    aec_blocks:new_key(PrevBlockHeight + 1, PrevBlockHash, PrevKeyHash,
                       aec_trees:hash(Trees), ?HIGHEST_TARGET_SCI,
                       0, aeu_time:now_in_msecs(), Protocol, Miner, Beneficiary).
