%% Implements generate/3 similar to cowboy_rest generate_etag/2 callback
%% customized with additional OperationId (first) argument for easy switching

-module(aehttp_cache_etag).

-export([generate/3]).

%%%===================================================================
%%% External API

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
    Hash = cowboy_req:binding(hash, Req),
    {{strong, Hash}, Req, State};

generate('GetGenerationByHeight', Req, State) ->
    handle_block_height(Req, State);

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

handle_block_height(Req, State) ->
    Height0 = cowboy_req:binding(height, Req),
    case to_int(Height0) of
        {ok, Height} ->
            ETag = etag_block_height(Height),
            {{strong, ETag}, Req, State};
        _ ->
            {undefined, Req, State}
    end.

etag_block_height(Height) ->
    case aehttp_logic:get_key_header_by_height(Height) of
        {ok, Header} -> etag_block_header(Header);
        _ -> undefined
    end.

etag_block(Block) ->
    Header = aec_blocks:to_header(Block),
    etag_block_header(Header).

etag_block_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Hex = aeu_hex:bin_to_hex(Hash),
    list_to_binary(Hex).

to_int(Data) ->
    try {ok, binary_to_integer(Data)}
    catch
        error:badarg ->
            error
    end.
