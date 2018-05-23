%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_mining).

%% API
-export([create_block_candidate/3,
         need_to_regenerate/1,
         mine/3,
         get_miner_account_balance/0]). %% For tests.

-ifdef(TEST).
-export([adjust_target/2]).
-endif.

-include("common.hrl").

%% API

-spec create_block_candidate(aec_blocks:block(), aec_trees:trees(),
                             list(aec_headers:header())) ->
                                    {ok, aec_blocks:block(), aec_pow:nonce()} |
                                    {error, term()}.
create_block_candidate(TopBlock, TopBlockTrees, AdjHeaders) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(TopBlock),
    create_block_candidate(get_txs_to_mine_in_pool(Hash),
                           TopBlock, TopBlockTrees,
                           AdjHeaders).

-spec need_to_regenerate(aec_blocks:block()) -> boolean().
need_to_regenerate(Block) ->
    Txs = aec_blocks:txs(Block),
    PrevHash = aec_blocks:prev_hash(Block),
    %% TODO: This should be an access function in tx pool
    MaxTxsInBlockCount = aec_governance:max_txs_in_block(),
    CurrentTxsBlockCount = length(Txs),
    (MaxTxsInBlockCount =/= CurrentTxsBlockCount)
        andalso (lists:sort(get_txs_to_mine_in_pool(PrevHash)) =/= lists:sort(Txs)).

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

-spec create_block_candidate(
        list(aetx_sign:signed_tx()),
        aec_blocks:block(), aec_trees:trees(),
        list(aec_headers:header())) -> {ok, aec_blocks:block(), aec_pow:nonce()} |
                           {error, term()}.
create_block_candidate(Txs, TopBlock, TopBlockTrees, AdjHeaders) ->
    case aec_keys:pubkey() of
        {error, _} = Error ->
            Error;
        {ok, Miner} ->
            Block = aec_blocks:new(TopBlock, Miner, Txs, TopBlockTrees),
            case adjust_target(Block, AdjHeaders) of
                {ok, AdjBlock} ->
                    {ok, AdjBlock, aec_pow:pick_nonce()};
                {error, _} = Error ->
                    Error
            end
    end.

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
