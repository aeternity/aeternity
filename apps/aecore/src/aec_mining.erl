-module(aec_mining).

%% API
-export([mine/0,
         mine/1]).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(DEFAULT_MINE_ATTEMPTS_COUNT, 10).

%% API

-spec mine() -> {ok, block()} | {error, term()}.
mine() ->
    mine(?DEFAULT_MINE_ATTEMPTS_COUNT).

-spec mine(non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Attempts) ->
    {ok, LastBlock} = aec_blocks:top(),
    Trees = aec_blocks:trees(LastBlock),
    Txs = get_txs_to_mine(Trees),
    case aec_blocks:new(LastBlock, Txs, Trees) of
        {ok, Block} ->
            case mine(Block, Attempts) of
                {ok, _Block} = Ok ->
                    Ok;
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.


%% Internal functions

-spec mine(block(), non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Block, Attempts) ->
    Difficulty = aec_blocks:difficulty(Block),
    case aec_pow_sha256:generate(Block, Difficulty, Attempts) of
        {ok, Nonce} ->
            {ok, aec_blocks:set_nonce(Block, Nonce)};
        {error, generation_count_exhausted} = Error ->
            Error
    end.

-spec get_txs_to_mine(trees()) -> list(signed_tx()).
get_txs_to_mine(Trees) ->
    {ok, Txs0} = aec_tx_pool:all(),
    {ok, CoinbaseTx} = create_coinbase_tx(Trees),
    {ok, SignedCoinbaseTx} = aec_keys:sign(CoinbaseTx),
    [SignedCoinbaseTx | Txs0].

-spec create_coinbase_tx(trees()) -> {ok, coinbase_tx()}.
create_coinbase_tx(Trees) ->
    {ok, Pubkey} = aec_keys:pubkey(),
    {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => Pubkey}, Trees),
    {ok, CoinbaseTx}.
