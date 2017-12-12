-module(aec_mining).

%% API
-export([create_block_candidate/2,
         need_to_regenerate/1,
         mine/2]).

-ifdef(TEST).
-export([adjust_target/2]).
-endif.

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").


%% API

-spec create_block_candidate(block(), list(header())) ->
                        {ok, block(), aec_pow:nonce()} | {error, term()}.
create_block_candidate(TopBlock, AdjHeaders) ->
    create_block_candidate(get_txs_to_mine_in_pool(), TopBlock, AdjHeaders).

-spec need_to_regenerate(block()) -> boolean().
need_to_regenerate(#block{txs = [_Coinbase|Txs]}) ->
    %% TODO: This should be an access function in tx pool
    MaxTxsInBlockCount = aec_governance:max_txs_in_block(),
    CurrentTxsBlockCount = length(Txs) + 1,
    (MaxTxsInBlockCount =/= CurrentTxsBlockCount)
        andalso (lists:sort(get_txs_to_mine_in_pool()) =/= lists:sort(Txs)).

-spec mine(block(), aec_pow:nonce()) -> {ok, block()} | {error, term()}.
mine(Block, Nonce) ->
    Target = aec_blocks:target(Block),
    BlockBin = aec_headers:serialize_for_hash(aec_blocks:to_header(Block)),
    Mod = aec_pow:pow_module(),
    case Mod:generate(BlockBin, Target, Nonce) of
        {ok, {Nonce, Evd}} ->
            {ok, aec_blocks:set_pow(Block, Nonce, Evd)};
        {error, no_solution} = Error ->
            Error;
        {error, {runtime, _}} = Error ->
            Error
    end.


%% Internal functions

-spec get_txs_to_mine_in_pool() -> list(aec_tx_sign:signed_tx()).
get_txs_to_mine_in_pool() ->
    {ok, Txs} = aec_tx_pool:peek(aec_governance:max_txs_in_block() - 1),
    Txs.

-spec create_block_candidate(list(aec_tx_sign:signed_tx()), block(), list(header())) ->
                  {ok, block(), aec_pow:nonce(), integer()} | {error, term()}.
create_block_candidate(TxsToMineInPool, TopBlock, AdjHeaders) ->
    case create_signed_coinbase_tx() of
        {error, _} = Error ->
            Error;
        {ok, SignedCoinbaseTx} ->
            Txs = [SignedCoinbaseTx | TxsToMineInPool],
            Trees = aec_blocks:trees(TopBlock),
            Block = aec_blocks:new(TopBlock, Txs, Trees),
            case aec_blocks:cointains_coinbase_tx(Block) of
                true ->
                    adjust_target(Block, AdjHeaders);
                false ->
                    {error, coinbase_tx_rejected}
            end
    end.

-spec create_signed_coinbase_tx() -> {ok, aec_tx_sign:signed_tx()} | {error, term()}.
create_signed_coinbase_tx() ->
    case create_coinbase_tx() of
        {ok, CoinbaseTx} ->
            case aec_keys:sign(CoinbaseTx) of
                {ok, SignedCoinbaseTx} ->
                    {ok, SignedCoinbaseTx};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec create_coinbase_tx() -> {ok, coinbase_tx()} | {error, term()}.
create_coinbase_tx() ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            aec_coinbase_tx:new(#{account => Pubkey});
        {error, _} = Error ->
            Error
    end.

-spec adjust_target(block(), list(header())) ->
            {ok, block(), aec_pow:nonce()} | {error, term()}.
adjust_target(Block, AdjHeaders) ->
    Header = aec_blocks:to_header(Block),
    DeltaHeight = aec_governance:blocks_to_check_difficulty_count(),
    case aec_headers:height(Header) =< DeltaHeight of
        true ->
            %% For the first DeltaHeight blocks, use pre-defined target
            {ok, Block, aec_pow:pick_nonce()};
        false when DeltaHeight == length(AdjHeaders) ->
            CalculatedTarget = aec_target:recalculate(Header, AdjHeaders),
            Block1 = aec_blocks:set_target(Block, CalculatedTarget),
            {ok, Block1, aec_pow:pick_nonce()};
        false -> %% Wrong number of headers in AdjHeaders...
            {error, wrong_headers_for_target_adjustment}
    end.
