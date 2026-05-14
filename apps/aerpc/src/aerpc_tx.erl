%%%-------------------------------------------------------------------
%%% @doc Transaction adapter for the AE JSON-RPC layer.
%%%
%%% Resolves an AE signed-tx to the JSON shape that ae_getTransactionBy*
%%% methods return. v1 emits the AE-native field set (via
%%% `aetx_sign:serialize_for_client/4') rather than re-mapping every
%%% AE tx type onto the eth tx shape -- a faithful translation requires
%%% a tx-type dispatch table that is non-trivial for non-spend / non-
%%% contract types (oracle, name, channel, paying-for, GA, ...). When
%%% that translation lands it slots in here without touching callers.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_tx).

-export([
          by_hash/1
        , by_block_hash_index/2
        , by_block_height_index/2
        ]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec by_hash(binary()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_hash(HashIn) when is_binary(HashIn) ->
    case decode_tx_hash(HashIn) of
        {ok, TxHash} ->
            case aec_chain:find_tx_with_location(TxHash) of
                none ->
                    {ok, null};
                {mempool, SignedTx} ->
                    {ok, serialize_pending(SignedTx)};
                {BlockHash, SignedTx} when is_binary(BlockHash) ->
                    {ok, serialize_mined(SignedTx, BlockHash, TxHash)}
            end;
        {error, _, _} = Err ->
            Err
    end;
by_hash(_) ->
    {error, -32602, <<"Invalid params">>}.

-spec by_block_hash_index(binary(), non_neg_integer()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_block_hash_index(HashIn, Index) when is_binary(HashIn), is_integer(Index) ->
    case aerpc_block:decode_block_hash(HashIn) of
        {ok, BlockHash} ->
            nth_tx(BlockHash, Index);
        {error, _, _} = Err ->
            Err
    end;
by_block_hash_index(_, _) ->
    {error, -32602, <<"Invalid params">>}.

-spec by_block_height_index(binary(), non_neg_integer()) ->
    {ok, map() | null} | {error, integer(), binary()}.
by_block_height_index(TagOrHex, Index)
  when is_binary(TagOrHex), is_integer(Index) ->
    case aerpc_block:resolve_tag(TagOrHex) of
        {ok, Height} ->
            case aec_chain:get_key_block_by_height(Height) of
                {ok, KeyBlock} ->
                    {ok, BlockHash} =
                        aec_blocks:hash_internal_representation(KeyBlock),
                    nth_tx(BlockHash, Index);
                {error, _Reason} ->
                    {ok, null}
            end;
        {error, _, _} = Err ->
            Err
    end;
by_block_height_index(_, _) ->
    {error, -32602, <<"Invalid params">>}.

%% ===================================================================
%% Internal
%% ===================================================================

nth_tx(BlockHash, Index) ->
    case aec_chain:get_generation_by_hash(BlockHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            Flat = lists:flatten([aec_blocks:txs(MB) || MB <- MBs]),
            case nth_safe(Index + 1, Flat) of
                {ok, SignedTx} ->
                    TxHash = aetx_sign:hash(SignedTx),
                    {ok, serialize_mined(SignedTx, BlockHash, TxHash)};
                none ->
                    {ok, null}
            end;
        error ->
            {ok, null}
    end.

nth_safe(N, _) when N =< 0 -> none;
nth_safe(_, [])            -> none;
nth_safe(1, [H | _])       -> {ok, H};
nth_safe(N, [_ | T])       -> nth_safe(N - 1, T).

serialize_pending(SignedTx) ->
    try aetx_sign:serialize_for_client_pending(SignedTx)
    catch _:_ ->
        #{<<"hash">> => aerpc_encoding:format_tx_hash(aetx_sign:hash(SignedTx))}
    end.

serialize_mined(SignedTx, BlockHash, TxHash) ->
    case aec_chain:get_header(BlockHash) of
        {ok, Header} ->
            try aetx_sign:serialize_for_client(Header, SignedTx)
            catch _:_ ->
                #{<<"hash">> => aerpc_encoding:format_tx_hash(TxHash)}
            end;
        error ->
            #{<<"hash">> => aerpc_encoding:format_tx_hash(TxHash)}
    end.

decode_tx_hash(<<"th_", _/binary>> = Encoded) ->
    case aeapi:decode_tx_hash(Encoded) of
        {ok, Bin} -> {ok, Bin};
        _Error    -> {error, -32602, <<"Invalid params">>}
    end;
decode_tx_hash(<<"0x", _/binary>> = Hex) ->
    try
        Bin = aerpc_encoding:from_hex_data(Hex),
        case byte_size(Bin) of
            32 -> {ok, Bin};
            _  -> {error, -32602, <<"Invalid params">>}
        end
    catch _:_ -> {error, -32602, <<"Invalid params">>}
    end;
decode_tx_hash(_) ->
    {error, -32602, <<"Invalid params">>}.
