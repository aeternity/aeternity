%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_mining).

%% API
-export([create_micro_block_candidate/3, create_key_block_candidate/4,
         need_to_regenerate/1,
         mine/3,
         get_miner_account_balance/0]). %% For tests.

-ifdef(TEST).
-export([adjust_target/2]).
-endif.

-include("common.hrl").

%% API

-spec create_micro_block_candidate(aec_blocks:block(), aec_blocks:block(), aec_trees:trees()) ->
                                    {ok, aec_blocks:block(), aec_pow:nonce()} |
                                    {error, term()}.
create_micro_block_candidate(TopBlock, CurrentKeyBlock, TopBlockTrees) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(TopBlock),
    create_micro_block_candidate(get_txs_to_mine_in_pool(Hash), TopBlock, CurrentKeyBlock, TopBlockTrees).

-spec create_key_block_candidate(aec_blocks:block(), aec_blocks:block(), aec_trees:trees(),
    list(aec_headers:header())) -> {ok, aec_blocks:block(), aec_pow:nonce()} | {error, term()}.
create_key_block_candidate(TopBlock, CurrentKeyBlock, TopBlockTrees, AdjHeaders) ->
    case aec_keys:pubkey() of
        {error, _} = Error ->
            Error;
        {ok, Miner} ->
            Block = aec_blocks:new_key(TopBlock, CurrentKeyBlock, Miner, TopBlockTrees),
            case adjust_target(Block, AdjHeaders) of
                {ok, AdjBlock} ->
                    {ok, AdjBlock, aec_pow:pick_nonce()};
                {error, _} = Error ->
                    Error
            end
    end.

-spec need_to_regenerate(aec_blocks:block()) -> boolean().
need_to_regenerate(_Block) ->
    %Txs = aec_blocks:txs(Block),
    %PrevHash = aec_blocks:prev_hash(Block),
    %%% TODO: This should be an access function in tx pool
    %MaxTxsInBlockCount = aec_governance:max_txs_in_block(),
    %CurrentTxsBlockCount = length(Txs),
    %(MaxTxsInBlockCount =/= CurrentTxsBlockCount)
    %    andalso (lists:sort(get_txs_to_mine_in_pool(PrevHash)) =/= lists:sort(Txs)).
    %% TODO: not the best option, we need to add new fees to earn more - possibly its handled in start_mining in conductor tho
    %% TODO: it is probably always worth to add new blocks - maybe we can be off by 1-small_N (3?)
    false.

-spec mine(binary(), aec_pow:sci_int(), aec_pow:nonce()) ->  aec_pow:pow_result().
mine(HeaderBin, Target, Nonce) ->
    aec_pow_cuckoo:generate(HeaderBin, Target, Nonce).

-spec get_miner_account_balance() -> {ok, non_neg_integer()} |
                                     {error, account_not_found}.
get_miner_account_balance() ->
    {ok, Pubkey} = aec_keys:pubkey(),
    case aec_chain:get_account(Pubkey) of
        {value, A} ->
            {ok, aec_accounts:balance(A)};
        none ->
            {error, account_not_found}
    end.

%% Internal functions

-spec get_txs_to_mine_in_pool(binary()) -> list(aetx_sign:signed_tx()).
get_txs_to_mine_in_pool(TopHash) ->
    MaxN = aec_governance:max_txs_in_block(),
    {ok, Txs} = aec_tx_pool:get_candidate(MaxN, TopHash),
    Txs.


-spec create_micro_block_candidate(
        list(aetx_sign:signed_tx()),
        aec_blocks:block(),
        aec_blocks:block(),
        aec_trees:trees())
        -> {ok, aec_blocks:block(), aec_pow:nonce()} | {error, term()}.
create_micro_block_candidate(Txs, TopBlock, CurrentKeyBlock, TopBlockTrees) ->
    aec_blocks:new_micro(TopBlock, CurrentKeyBlock, Txs, TopBlockTrees).


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
