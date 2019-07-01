%% Implements generate/3 similar to cowboy_rest generate_etag/2 callback
%% customized with additional OperationId (first) argument for easy switching

-module(aehttp_cache_etag).

-export([generate/3]).

-type cowboy_etag_res() :: {undefined | {strong, binary()}, cowboy_req:req(), any()}.

%%%===================================================================
%%% External API

-spec generate(atom(), cowboy_req:req(), any()) -> cowboy_etag_res().
generate('GetTopBlock', Req, State) ->
    case aec_chain:top_block() of
        undefined ->
            {undefined, Req, State};
        Block ->
            ETag = etag_block(Block),
            {{strong, ETag}, Req, State}
    end;

generate('GetCurrentKeyBlock', Req, State) ->
    case aec_chain:top_key_block() of
        {ok, Block} ->
            ETag = etag_block(Block),
            {{strong, ETag}, Req, State};
        _ ->
            {undefined, Req, State}
    end;

generate('GetKeyBlockByHash', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetKeyBlockByHeight', Req, State) ->
    handle_block_height(Req, State);

generate('GetMicroBlockHeaderByHash', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetMicroBlockTransactionsByHash', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetMicroBlockTransactionByHashAndIndex', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetMicroBlockTransactionsCountByHash', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetGenerationByHash', Req, State) ->
    handle_generation_hash(Req, State);

generate('GetGenerationByHeight', Req, State) ->
    handle_generation_height(Req, State);

generate('GetTransactionByHash', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetTransactionInfoByHash', Req, State) ->
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate(_OperationId, Req, State) ->
    {undefined, Req, State}.

%%%===================================================================
%%% Private functions

-spec handle_block_height(cowboy_req:req(), any()) -> cowboy_etag_res().
handle_block_height(Req, State) ->
    Height0 = cowboy_req:binding(height, Req),
    case to_int(Height0) of
        {ok, Height} ->
            case etag_block_height(Height) of
                {ok, ETag} ->
                    {{strong, ETag}, Req, State};
                undefined ->
                    {undefined, Req, State}
            end;
        _ ->
            {undefined, Req, State}
    end.

-spec handle_generation_hash(cowboy_req:req(), any()) -> cowboy_etag_res().
handle_generation_hash(Req, State) ->
    Hash0 = cowboy_req:binding(hash, Req),
    case aeser_api_encoder:safe_decode(key_block_hash, Hash0) of
        {ok, Hash} ->
            case etag_generation_hash(Hash) of
                {ok, ETag} ->
                    {{strong, ETag}, Req, State};
                _ ->
                    {undefined, Req, State}
            end;
        _ ->
            {undefined, Req, State}
    end.

-spec handle_generation_height(cowboy_req:req(), any()) -> cowboy_etag_res().
handle_generation_height(Req, State) ->
    Height0 = cowboy_req:binding(height, Req),
    case to_int(Height0) of
        {ok, Height} ->
            case etag_generation_height(Height) of
                {ok, ETag} ->
                    {{strong, ETag}, Req, State};
                _ ->
                    {undefined, Req, State}
            end;
        _ ->
            {undefined, Req, State}
    end.

-spec etag_block_height(integer()) -> {ok, binary()} | undefined.
etag_block_height(Height) ->
    case aehttp_logic:get_key_header_by_height(Height) of
        {ok, Header} ->
            ETag = etag_block_header(Header),
            {ok, ETag};
        _ -> undefined
    end.

-spec etag_block(aec_blocks:block()) -> binary().
etag_block(Block) ->
    Header = aec_blocks:to_header(Block),
    etag_block_header(Header).

-spec etag_block_header(aec_headers:header()) -> binary().
etag_block_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Hex = aeu_hex:bin_to_hex(Hash),
    list_to_binary(Hex).

-spec etag_generation_height(non_neg_integer()) -> {ok, binary()} | undefined.
etag_generation_height(Height) ->
    case aec_chain:get_generation_by_height(Height, forward) of
        {ok, #{key_block := KeyBlock, micro_blocks := MBs}} ->
            ETag = etag_generation(KeyBlock, MBs),
            {ok, ETag};
        _ ->
            undefined
    end.

-spec etag_generation_hash(binary()) -> {ok, binary()} | undefined.
etag_generation_hash(Hash) ->
    case aec_chain:get_generation_by_hash(Hash, forward) of
        {ok, #{key_block := KeyBlock, micro_blocks := MBs}} ->
            ETag = etag_generation(KeyBlock, MBs),
            {ok, ETag};
        _ ->
            undefined
    end.

-spec etag_generation(aec_blocks:key_block(), [aec_blocks:micro_block()]) -> binary().
etag_generation(KeyBlock, []) ->
    etag_block(KeyBlock);

etag_generation(_KeyBlock, MBs) ->
    Block = lists:last(MBs),
    etag_block(Block).

-spec to_int(binary()) -> {ok, integer()} | error.
to_int(Data) ->
    try {ok, binary_to_integer(Data)}
    catch
        error:badarg ->
            error
    end.
