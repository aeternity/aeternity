-module(aehttp_logic).

-export([ get_top/0
        , get_top_height/0
        , get_top_hash/0
        , get_header_by_hash/1
        , get_key_header_by_height/1
        , get_key_block_by_hash/1
        , get_key_block_by_height/1
        , get_micro_block_by_hash/1
        , get_block_by_hash/1
        , get_block_latest/0
        , get_block_pending/0
        , get_block_genesis/0
        , get_genesis_hash/0
        , get_top_blocks_time_summary/1
        , get_top_blocks_gas_price_summary/0
        ]).

-export([ get_account/1
        , get_account_balance/1
        ]).

-export([ version/0
        , revision/0
        ]).

-export([ connected_peers/1
        , blocked_peers/0
        ]).

-spec get_top() -> {ok, aec_blocks:block()}.
get_top() ->
    Block = aec_chain:top_block(),
    {ok, Block}.

-spec get_top_height() -> {ok, integer()}.
get_top_height() ->
    TopHeader = aec_chain:top_header(),
    Height = aec_headers:height(TopHeader),
    {ok, Height}.

-spec get_top_hash() -> {ok, binary()}.
get_top_hash() ->
    TopHeader = aec_chain:top_header(),
    {ok, _Hash} = aec_headers:hash_header(TopHeader).

-spec get_header_by_hash(binary()) -> {ok, aec_headers:header()} | {error, header_not_found}.
get_header_by_hash(Hash) ->
    case aec_chain:get_header(Hash) of
        {ok, _Header} = OK -> OK;
        error ->
            {error, header_not_found}
    end.

-spec get_key_header_by_height(integer()) -> {ok, aec_headers:header()} | {error, chain_too_short}.
get_key_header_by_height(Height) ->
    case aec_chain:get_key_header_by_height(Height) of
        {ok, _Header} = OK -> OK;
        {error, chain_too_short} = Err ->
            Err
    end.

-spec get_key_block_by_height(integer()) -> {ok, aec_blocks:block()} |
                                        {error, block_not_found | chain_too_short}.
get_key_block_by_height(Height) ->
    case aec_chain:get_key_block_by_height(Height) of
        {ok, Block} ->
            {ok, Block};
        {error, Msg} = Err when Msg =:= block_not_found orelse
                                Msg =:= chain_too_short ->
            Err
    end.

get_block_genesis() ->
    GenBlock = aec_chain:genesis_block(),
    {ok, GenBlock}.

get_block_latest() ->
    TopBlock = aec_chain:top_block(),
    {ok, TopBlock}.

get_block_pending() ->
    aec_conductor:get_key_block_candidate().

-spec get_block_by_hash(binary()) -> {ok, aec_blocks:block()} |
                                     {error, block_not_found}.
get_block_by_hash(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            {ok, Block};
        error ->
            {error, block_not_found}
    end.

-spec get_key_block_by_hash(binary()) -> {ok, aec_blocks:block()} |
                                         {error, block_not_found}.
get_key_block_by_hash(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            case aec_blocks:is_key_block(Block) of
                true -> {ok, Block};
                false -> {error, block_not_found}
            end;
        error ->
            {error, block_not_found}
    end.

-spec get_micro_block_by_hash(binary()) -> {ok, aec_blocks:block()} |
                                           {error, block_not_found}.
get_micro_block_by_hash(Hash) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            case aec_blocks:is_key_block(Block) of
                false -> {ok, Block};
                true -> {error, block_not_found}
            end;
        error ->
            {error, block_not_found}
    end.

-spec get_account(binary()) -> {ok, map()} | {error, account_not_found}.
get_account(Pubkey) ->
    case aec_chain:get_account(Pubkey) of
        {value, Account} -> {ok, Account};
        none -> {error, account_not_found}
    end.

-spec get_account_balance(binary()) -> {ok, integer()}
                                     | {error, account_not_found}.
get_account_balance(Pubkey) when is_binary(Pubkey) ->
    case aec_chain:get_account(Pubkey) of
        {value, A} ->
            {ok, aec_accounts:balance(A)};
        none ->
            {error, account_not_found}
    end.

version() -> {ok, aeu_info:get_version()}.

revision() -> {ok, aeu_info:get_revision()}.

get_genesis_hash() -> {ok, aec_chain:genesis_hash()}.

get_top_blocks_time_summary(Count) ->
    TimeSummary0 = aec_chain:get_top_N_blocks_time_summary(Count),
    TimeSummary =
        lists:foldl(
          fun({Height, Ts, Delta, Difficulty}, Acc) ->
                  [#{height => Height,
                    time => Ts,
                    difficulty => Difficulty,
                    time_delta_to_parent => Delta} | Acc];
            ({Height, Ts, Difficulty}, Acc) ->
                  [#{height => Height,
                    time => Ts,
                    difficulty => Difficulty} | Acc]
          end, [], TimeSummary0),
    {ok, lists:reverse(TimeSummary)}.

connected_peers(Tag) -> aec_peers:connected_peers(Tag).

blocked_peers() -> aec_peers:blocked_peers().

get_top_blocks_gas_price_summary() ->
    TopKeyHash = aec_chain:top_key_block_hash(),
    case get_gens_min_gas_price(TopKeyHash, 480) of
        Err = {error, _} ->
            Err;
        MinGasPrices ->
            MinPriceN = fun(N) ->
                            case lists:min(lists:sublist(MinGasPrices, N)) of
                                undefined -> 0;
                                X         -> X
                            end
                        end,

            {ok, [ [N, MinPriceN(N)] || N <- [1, 5, 20, 120, 480] ]}
    end.

-define(GEN_MIN_PRICE_CACHE, '$aec_generation_min_gas_price').
-define(MIN_GAS_SLACK, 200_000).

get_gens_min_gas_price(_, 0) -> [];
get_gens_min_gas_price(KeyHash, N) ->
    case get_gen_min_gas_price(KeyHash) of
        {ok, MinGasPrice, PrevKeyHash} ->
            [MinGasPrice | get_gens_min_gas_price(PrevKeyHash, N - 1)];
        Err = {error, _} ->
            Err
    end.

get_gen_min_gas_price(KeyHash) ->
    aeu_ets_cache:get(?GEN_MIN_PRICE_CACHE, KeyHash,
                      fun() -> get_gen_min_gas_price_(KeyHash) end).

get_gen_min_gas_price_(KeyHash) ->
    case aec_chain:get_block(KeyHash) of
        {ok, Block} ->
            get_gen_min_gas_price(aec_blocks:prev_hash(Block), aec_blocks:version(Block), undefined);
        error ->
            {error, block_not_found}
    end.

get_gen_min_gas_price(Hash, Protocol, MinGasPrice) ->
    {ok, Block} = aec_chain:get_block(Hash),
    case aec_blocks:type(Block) of
        key ->
            {ok, MinGasPrice, Hash};
        micro ->
            Txs = lists:map(fun aetx_sign:tx/1, aec_blocks:txs(Block)),
            Height = aec_blocks:height(Block),
            NewMinGasPrice = lists:foldl(
                fun(MGP, Tx) ->
                    min(MGP, aetx:min_gas_price(Tx, Height, Protocol))
                end, MinGasPrice, Txs),
            get_gen_min_gas_price(aec_blocks:prev_hash(Block), Protocol, NewMinGasPrice)
    end.
