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

%%%===================================================================
%%% External API

-spec expires(atom(), cowboy_req:req(), any()) -> cowboy_expires_res().
expires('GetTopBlock', Req, State) ->
    CacheTime = expected_next_micro_block_time(),
    Exp = unixtime_to_datetime(CacheTime),
    {Exp, Req, State};

expires('GetCurrentKeyBlock', Req, State) ->
    case aec_chain:top_key_block() of
        {ok, Block} ->
            Header = aec_blocks:to_header(Block),
            Exp = expires_block_header(Header),
            {Exp, Req, State};
        _ ->
            {undefined, Req, State}
    end;

expires('GetKeyBlockByHash', Req, State) ->
    handle_key_block_hash(Req, State);

expires('GetKeyBlockByHeight', Req, State) ->
    handle_key_block_height(Req, State);

expires('GetMicroBlockHeaderByHash', Req, State) ->
    handle_micro_block_hash(Req, State);

expires('GetMicroBlockTransactionsByHash', Req, State) ->
    handle_micro_block_hash(Req, State);

expires('GetMicroBlockTransactionByHashAndIndex', Req, State) ->
    handle_micro_block_hash(Req, State);

expires('GetMicroBlockTransactionsCountByHash', Req, State) ->
    handle_micro_block_hash(Req, State);

expires('GetGenerationByHash', Req, State) ->
    handle_key_block_hash(Req, State);

expires('GetGenerationByHeight', Req, State) ->
    handle_key_block_height(Req, State);

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

-spec handle_key_block_hash(cowboy_req:req(), any()) -> cowboy_expires_res().
handle_key_block_hash(Req, State) ->
    Hash0 = cowboy_req:binding(hash, Req),
    case aeser_api_encoder:safe_decode(key_block_hash, Hash0) of
        {ok, Hash} ->
            Exp = expires_block_hash(Hash),
            {Exp, Req, State};
        _ -> {undefined, Req, State}
    end.

-spec handle_key_block_height(cowboy_req:req(), any()) -> cowboy_expires_res().
handle_key_block_height(Req, State) ->
    Height0 = cowboy_req:binding(height, Req),
    case to_int(Height0) of
        {ok, Height} ->
            Exp = expires_block_height(Height),
            {Exp, Req, State};
        _ ->
            {undefined, Req, State}
    end.

-spec handle_micro_block_hash(cowboy_req:req(), any()) -> cowboy_expires_res().
handle_micro_block_hash(Req, State) ->
    Hash0 = cowboy_req:binding(hash, Req),
    case aeser_api_encoder:safe_decode(micro_block_hash, Hash0) of
        {ok, Hash} ->
            Exp = expires_block_hash(Hash),
            {Exp, Req, State};
        _ -> {undefined, Req, State}
    end.

-spec handle_transaction_hash(cowboy_req:req(), any()) -> cowboy_expires_res().
handle_transaction_hash(Req, State) ->
    Hash0 = cowboy_req:binding(hash, Req),
    case aeser_api_encoder:safe_decode(tx_hash, Hash0) of
        {ok, Hash} ->
            case aec_chain:find_tx_with_location(Hash) of
                {BlockHash, _Tx} when is_binary(BlockHash) ->
                    Exp = expires_block_hash(BlockHash),
                    {Exp, Req, State};
                _ ->
                    {undefined, Req, State}
            end;
        _ ->
            {undefined, Req, State}
    end.

-spec expires_block_height(non_neg_integer()) -> {ok, aec_headers:header()} | undefined.
expires_block_height(Height) ->
    case aehttp_logic:get_key_header_by_height(Height) of
        {ok, Header} -> expires_block_header(Header);
        _ -> undefined
    end.

-spec expires_block_hash(binary()) -> {ok, aec_headers:header()} | undefined.
expires_block_hash(Hash) ->
    case aehttp_logic:get_header_by_hash(Hash) of
        {ok, Header} -> expires_block_header(Header);
        _ -> undefined
    end.

-spec expires_block_header(aec_headers:header()) -> non_neg_integer() | error.
expires_block_header(Header) ->
    case block_header_cache_time(Header) of
        CacheTime when is_integer(CacheTime) ->
            unixtime_to_datetime(CacheTime);
        _ -> error
    end.

-spec block_header_cache_time(aec_headers:header()) -> non_neg_integer() | error.
block_header_cache_time(Header) ->
    Now = aeu_time:now_in_secs(),
    BlockTimestamp = aec_headers:time_in_secs(Header),
    Diff = Now - BlockTimestamp,
    case Diff > aged_blocks_time() of
        true ->
            Now + aged_blocks_cache_time();
        false ->
            expected_next_key_block_time()
    end.

-spec expected_next_key_block_time() -> non_neg_integer() | error.
expected_next_key_block_time() ->
    case top_key_block_header() of
        {ok, Header} ->
            BlockTimestamp = aec_headers:time_in_secs(Header),
            Rate = aeu_time:msecs_to_secs(aec_governance:expected_block_mine_rate()),
            BlockTimestamp + Rate;
        _ -> error
    end.

-spec top_key_block_header() -> {ok, aec_headers:header()} | error.
top_key_block_header() ->
    case aec_chain:top_key_block() of
        {ok, Block} ->
            {ok, aec_blocks:to_key_header(Block)};
        _ ->
            error
    end.

-spec expected_next_micro_block_time() -> non_neg_integer().
expected_next_micro_block_time() ->
    TopHeader = aec_chain:top_header(),
    BlockTimestamp = aec_headers:time_in_secs(TopHeader),
    Rate = aeu_time:msecs_to_secs(aec_governance:micro_block_cycle()),
    BlockTimestamp + Rate.

to_int(Data) ->
    try {ok, binary_to_integer(Data)}
    catch
        error:badarg ->
            error
    end.

-spec unixtime_to_datetime(non_neg_integer()) -> calendar:datetime().
unixtime_to_datetime(Unixtime) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds  = BaseDate + Unixtime,
    calendar:gregorian_seconds_to_datetime(Seconds).
