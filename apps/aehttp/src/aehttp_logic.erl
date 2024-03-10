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
        , get_top_blocks_gas_price_summary/1
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

-include_lib("aecontract/include/hard_forks.hrl").

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

get_top_blocks_gas_price_summary(Minutes) ->
    Now = erlang:system_time(millisecond),
    Offsets = [ {N, Now - N * 60 * 1_000} || N <- Minutes ],
    TopHash = aec_chain:top_block_hash(),
    get_min_gas_price_since(Offsets, TopHash, {undefined, 0, 0}, []).

min_gas_price(undefined) -> 0;
min_gas_price(N) -> N.

get_min_gas_price_since([], _Hash, _AccStats, Data) ->
  {ok, lists:reverse(Data)};
get_min_gas_price_since([{Ms, CutOff} | CutOffs] = COs, Hash, AccStats, Data) ->
    case aec_chain:get_header(Hash) of
        {ok, Header} ->
            case aec_headers:time_in_msecs(Header) < CutOff of
                true ->
                    get_min_gas_price_since(CutOffs, Hash, AccStats, [stats_to_data(Ms, AccStats) | Data]);
                false ->
                    case aec_headers:type(Header) of
                        key ->
                            get_min_gas_price_since(COs, aec_headers:prev_hash(Header), AccStats, Data);
                        micro ->
                        {ok, {_, Stats}} = get_mb_stats(Hash, aec_headers:version(Header)),
                        get_min_gas_price_since(COs, aec_headers:prev_hash(Header), add_stats(Stats, AccStats), Data)
                    end
            end;
        error ->
            {error, block_not_found}
    end.

stats_to_data(Minutes, {MinGasPrice, 0, 0}) ->
    stats_to_data(Minutes, {MinGasPrice, 0, 1});

stats_to_data(Minutes, {MinGasPrice, UsedGas, TotGas}) ->
    {Minutes, min_gas_price(MinGasPrice), round((100 * UsedGas) / TotGas)}.

add_stats({MinGasPrice, UsedGas, TotGas}, {AccMinGasPrice, AccUsedGas, AccTotGas}) ->
    {min(MinGasPrice, AccMinGasPrice), AccUsedGas + UsedGas, AccTotGas + TotGas}.

get_mb_stats(Hash, Protocol) ->
    case persistent_term:get({aehttp_app, kache}, undefined) of
        undefined ->
            get_mb_stats_(Hash, Protocol);
        Cache ->
            kache:get_fill(Cache, Hash, fun(_) -> {ok, get_mb_stats_(Hash, Protocol)} end)
    end.

get_mb_stats_(Hash, Protocol) ->
    case {aec_chain:get_block(Hash), aec_chain:get_block_state(Hash)} of
        {{ok, Block}, {ok, Trees}} ->
            Txs = lists:map(fun aetx_sign:tx/1, aec_blocks:txs(Block)),
            Height = aec_blocks:height(Block),
            {MinGasPrice, UsedGas} =
                lists:foldl(
                    fun(Tx, {MGP, UG}) ->
                        {min(MGP, aetx:min_gas_price(Tx, Height, Protocol)),
                         UG + get_tx_gas(Tx, Height, Protocol, Trees)}
                    end, {undefined, 0}, Txs),

            {aec_blocks:time_in_msecs(Block), {MinGasPrice, UsedGas, aec_governance:block_gas_limit()}};
        _Error ->
            {error, block_not_found}
    end.

get_tx_gas(Tx, Height, Protocol, _Trees) when Protocol < ?CERES_PROTOCOL_VSN ->
    aetx:gas_limit(Tx, Height, Protocol);
get_tx_gas(Tx, Height, Protocol, Trees) ->
    aetx:used_gas(Tx, Height, Protocol, Trees).
