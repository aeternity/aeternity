%% Implements expires/3 similar to cowboy_rest expires/2 callback
%% customized with additional OperationId (first) argument for easy switching
%%
%% Get the top block (either key or micro block)
%% Cannot be cached for more than MICRO_BLOCK_CYCLE (3s)
%%
%% Get a micro block
%% Can be cached aggressively, however micro-blocks of last X key blocks can be reorganized by forks,
%% thus their cache time should not exceed EXPECTED_BLOCK_MINE_RATE_MINUTES (3 min.)
%% Expires = ExpectedNextBlockTime() -> TopBlockTime + InterBlockTime
%%
%% Also micro blocks can be reorganized because of micro-forks,
%% so micro-blocks of current generation cannot be cached for more than MICRO_BLOCK_CYCLE (3s)
%% Expires = ExpectedNextMicroBlockTime() -> TopMicroBlockTime + InterMicroBlockTime
%%
%% Get a key block
%% Can be cached for long period of time, however last X key blocks can be reorganized by forks,
%% thus their cache time should not exceed EXPECTED_BLOCK_MINE_RATE_MINUTES (3 min.)
%% Expires = ExpectedNextBlockTime() -> TopBlockTime + InterBlockTime
%%
%% Get a generation by hash
%% Same as micro block endpoints cache requirements - older generations can be cached aggressively.
%%
%% Transaction can be cached based on age of it's generation

-module(aehttp_cache_expires).

-export([expires/3]).

-define(DEFAULT_AGED_BLOCKS_TIME, 24*60*60). % Time to consider a block as aged (1d)
-define(DEFAULT_AGED_BLOCKS_CACHE_TIME, 24*60*60). % Cache aged key blocks for a day by default

-type cowboy_expires_res() :: {calendar:datetime() | undefined, cowboy_req:req(), any()}.
-type cache_type() :: key | micro.
-type hash_param() :: key_block_hash | micro_block_hash.

%%%===================================================================
%%% External API

-spec expires(atom(), cowboy_req:req(), any()) -> cowboy_expires_res().
expires('GetTopBlock', Req, State) ->
    CacheTime = expected_next_block_time(micro),
    Exp = unixtime_to_datetime(CacheTime),
    {Exp, Req, State};

expires('GetCurrentKeyBlock', Req, State) ->
    Header = top_key_block_header(),
    Exp = expires_datetime(Header, key),
    {Exp, Req, State};

expires('GetKeyBlockByHash', Req, State) ->
    handle_block_hash(key_block_hash, key, Req, State);

expires('GetKeyBlockByHeight', Req, State) ->
    handle_block_height(key, Req, State);

expires('GetMicroBlockHeaderByHash', Req, State) ->
    handle_block_hash(micro_block_hash, micro, Req, State);

expires('GetMicroBlockTransactionsByHash', Req, State) ->
    handle_block_hash(micro_block_hash, micro, Req, State);

expires('GetMicroBlockTransactionByHashAndIndex', Req, State) ->
    handle_block_hash(micro_block_hash, micro, Req, State);

expires('GetMicroBlockTransactionsCountByHash', Req, State) ->
    handle_block_hash(micro_block_hash, micro, Req, State);

expires('GetGenerationByHash', Req, State) ->
     handle_block_hash(key_block_hash, micro, Req, State);

expires('GetGenerationByHeight', Req, State) ->
    handle_block_height(micro, Req, State);

expires('GetTransactionByHash', Req, State) ->
    handle_transaction_hash(Req, State);

expires('GetTransactionInfoByHash', Req, State) ->
    handle_transaction_hash(Req, State);

expires(_OperationId, Req, State) ->
    {undefined, Req, State}.

%%%===================================================================
%%% Private functions

-spec aged_blocks_time() -> non_neg_integer().
aged_blocks_time() ->
    aeu_env:user_config_or_env([<<"http">>, <<"cache">>, <<"aged_blocks_time">>],
                               aehttp, [cache, aged_blocks_time], ?DEFAULT_AGED_BLOCKS_TIME).

-spec aged_blocks_cache_time() -> non_neg_integer().
aged_blocks_cache_time() ->
    aeu_env:user_config_or_env([<<"http">>, <<"cache">>, <<"aged_blocks_cache_time">>],
                               aehttp, [cache, aged_blocks_cache_time], ?DEFAULT_AGED_BLOCKS_CACHE_TIME).

-spec handle_block_hash(hash_param(), cache_type(), cowboy_req:req(), any()) -> cowboy_expires_res().
handle_block_hash(Param, CacheType, Req, State) ->
    Hash0 = cowboy_req:binding(hash, Req),
    case aeser_api_encoder:safe_decode(Param, Hash0) of
        {ok, Hash} ->
            Exp = expires_block_hash(Hash, CacheType),
            {Exp, Req, State};
        _ -> {undefined, Req, State}
    end.

-spec handle_block_height(cache_type(), cowboy_req:req(), any()) -> cowboy_expires_res().
handle_block_height(CacheType, Req, State) ->
    Height0 = cowboy_req:binding(height, Req),
    case to_int(Height0) of
        {ok, Height} ->
            Exp = expires_block_height(Height, CacheType),
            {Exp, Req, State};
        _ ->
            {undefined, Req, State}
    end.

-spec handle_transaction_hash(cowboy_req:req(), any()) -> cowboy_expires_res().
handle_transaction_hash(Req, State) ->
    Hash0 = cowboy_req:binding(hash, Req),
    case aeser_api_encoder:safe_decode(tx_hash, Hash0) of
        {ok, Hash} ->
            case aec_chain:find_tx_with_location(Hash) of
                {BlockHash, _Tx} when is_binary(BlockHash) ->
                    Exp = expires_block_hash(BlockHash, micro),
                    {Exp, Req, State};
                _ ->
                    {undefined, Req, State}
            end;
        _ ->
            {undefined, Req, State}
    end.

-spec expires_block_height(non_neg_integer(), cache_type()) -> calendar:datetime() | undefined.
expires_block_height(Height, CacheType) ->
    case aehttp_logic:get_key_header_by_height(Height) of
        {ok, Header} -> expires_datetime(Header, CacheType);
        _ -> undefined
    end.

-spec expires_block_hash(binary(), cache_type()) -> calendar:datetime() | undefined.
expires_block_hash(Hash, CacheType) ->
    case aehttp_logic:get_header_by_hash(Hash) of
        {ok, Header} -> expires_datetime(Header, CacheType);
        _ -> undefined
    end.

-spec expires_datetime(aec_headers:header(), cache_type()) -> calendar:datetime().
expires_datetime(Header, CacheType) ->
    unixtime_to_datetime(cache_time(Header, CacheType)).

-spec cache_time(aec_headers:header(), key | micro ) -> non_neg_integer().
cache_time(Header, key) ->
    case is_aged(Header) of
        true  ->
            lager:info("Aged block."),
            aeu_time:now_in_secs() + aged_blocks_cache_time();
        false -> expected_next_block_time(key)
    end;

cache_time(Header, micro) ->
    case is_top_generation(Header) of
        true ->
            lager:info("Part of top generation."),
            expected_next_block_time(micro);
        false -> cache_time(Header, key)
    end.

-spec is_aged(aec_headers:header()) -> boolean().
is_aged(Header) ->
    Now = aeu_time:now_in_secs(),
    BlockTimestamp = aec_headers:time_in_secs(Header),
    Diff = Now - BlockTimestamp,
    Diff > aged_blocks_time().

-spec is_top_generation(aec_headers:header()) -> boolean().
is_top_generation(Header) ->
    TopKeyHeader = top_key_block_header(),
    {ok, TopKeyHash} = aec_headers:hash_header(TopKeyHeader),
    KeyHash = case aec_headers:type(Header) of
        micro ->
            aec_headers:prev_key_hash(Header);
        key ->
            {ok, Hash} = aec_headers:hash_header(Header),
            Hash
    end,
    lager:info("Compare: ~p =:= ~p", [TopKeyHash, KeyHash]),
    TopKeyHash =:= KeyHash.

-spec top_key_block_header() -> aec_headers:header().
top_key_block_header() ->
    {ok, Block} = aec_chain:top_key_block(),
    aec_blocks:to_key_header(Block).

-spec expected_next_block_time(key | micro) -> non_neg_integer().
expected_next_block_time(CacheType) ->
    TopHeader = aec_chain:top_header(),
    BlockTimestamp = aec_headers:time_in_secs(TopHeader),
    Rate = aeu_time:msecs_to_secs(block_rate(CacheType)),
    BlockTimestamp + Rate.

-spec block_rate(key|micro) -> non_neg_integer().
block_rate(key) ->
    aec_governance:expected_block_mine_rate();
block_rate(micro) ->
    aec_governance:micro_block_cycle().

-spec to_int(binary()) -> {ok, integer()} | error.
to_int(Data) ->
    try {ok, binary_to_integer(Data)}
    catch
        error:badarg -> error
    end.

-spec unixtime_to_datetime(non_neg_integer()) -> calendar:datetime().
unixtime_to_datetime(Unixtime) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds  = BaseDate + Unixtime,
    calendar:gregorian_seconds_to_datetime(Seconds).
