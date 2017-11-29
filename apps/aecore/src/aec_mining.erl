-module(aec_mining).

%% API
-export([create_block_candidate/0,
         apply_new_txs/1,
         mine/2]).


-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").


%% API

-spec create_block_candidate() -> {ok, block(), aec_pow:nonce()} | {error, term()}.
create_block_candidate() ->
    create_block_candidate(get_txs_to_mine_in_pool()).

-spec apply_new_txs(block()) -> {ok, block()} | {ok, block(), aec_pow:nonce()} | {error, term()}.
apply_new_txs(#block{txs = Txs} = Block) ->
    MaxTxsInBlockCount = aec_governance:max_txs_in_block(),
    CurrentTxsBlockCount = length(Txs),
    case {MaxTxsInBlockCount, CurrentTxsBlockCount} of
        {Count, Count} ->
            {ok, Block};
        {_, _} ->
            case get_txs_to_mine_in_pool() of
                [] ->
                    {ok, Block};
                [_|_] = NewTxs ->
                    create_block_candidate(NewTxs)
            end
    end.

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

-spec get_txs_to_mine_in_pool() -> list(signed_tx()).
get_txs_to_mine_in_pool() ->
    {ok, Txs} = aec_tx_pool:peek(aec_governance:max_txs_in_block() - 1),
    Txs.

-spec create_block_candidate(list(signed_tx())) -> {ok, block(), aec_pow:nonce()} | {error, term()}.
create_block_candidate(TxsToMineInPool) ->
    case create_signed_coinbase_tx() of
        {error, _} = Error ->
            Error;
        {ok, SignedCoinbaseTx} ->
            Txs = [SignedCoinbaseTx | TxsToMineInPool],
            {ok, LastBlock} = aec_chain:top(),
            Trees = aec_blocks:trees(LastBlock),
            Block0 = aec_blocks:new(LastBlock, Txs, Trees),
            case aec_blocks:cointains_coinbase_tx(Block0) of
                true ->
                    Block = adjust_target(Block0),
                    RandomNonce = aec_pow:pick_nonce(),
                    {ok, Block, RandomNonce};
                false ->
                    {error, coinbase_tx_rejected}
            end
    end.

-spec create_signed_coinbase_tx() -> {ok, signed_tx()} | {error, term()}.
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

-spec adjust_target(block()) -> block().
adjust_target(Block) ->
    Header = aec_blocks:to_header(Block),
    case aec_target:determine_delta_header_height(Header) of
        {ok, InitialHeaderHeight} ->
            %% For mining we can grab the header at height=DeltaHeight from
            %% the current main chain, hence aec_chain:get_header_by_height/1 call.
            {ok, InitialHeader} = aec_chain:get_header_by_height(InitialHeaderHeight),
            CalculatedTarget = aec_target:recalculate(Header, InitialHeader),
            aec_blocks:set_target(Block, CalculatedTarget);
        {error, chain_too_short_to_recalculate_target} ->
            Block
    end.
