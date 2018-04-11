%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     HTTP request support.
%%%     This module contains a function for each HTTP endpoint with
%%%     as arguments
%%%     1) the host (e.g. http://localhost:3013) in binary format
%%%        to allow utf8 characters,
%%%     2) the parameters.
%%%
%%%     This module is preparing for an HTTP request according to the
%%%     specification in swagger.yaml. That is, it checks types and
%%%     business logic of the input and of the response.
%%%
%%%     The actual HTTP request is performed via aeu_http_client:request.
%%%     The latter is also validating request input and response according
%%%     to the swagger schema definitions, but is unaware of the actual
%%%     business logic.
%%%     For example,
%%%     the ping request provides a "share" parameter stating how many
%%%     peers it maximally want to receive in the response.
%%%     It is hard to express this relation in the JSON schema(s), but
%%%     easy to verify here that the list of returned pings has a length
%%%     not exceeding Share.
%%%
%%%     Note that we perform both the JSON encoding and the actual
%%%     request in the separate aeu_http_client module. This allows
%%%     mocking on the request layer during testing.
%%% @end
%%% Created: 2017
%%%
%%%-------------------------------------------------------------------

-module(aeu_requests).

%% API
-export([top/1,
         get_header_by_hash/2,
         get_header_by_height/2,
         get_block_by_height/2,
         get_block/2,
         transactions/1,
         send_tx/2,
         send_block/2,
         new_spend_tx/2,
         pp_uri/1
        ]).

-import(aeu_debug, [pp/1]).

-type http_uri_uri() :: string() | binary(). %% From https://github.com/erlang/otp/blob/9fc5b13/lib/inets/src/http_lib/http_uri.erl#L72
-type http_uri_host() :: string() | binary(). %% From https://github.com/erlang/otp/blob/9fc5b13/lib/inets/src/http_lib/http_uri.erl#L75

-type response(Type) :: {ok, Type} | {error, string()}.

-spec top(http_uri_uri()) -> response(aec_headers:header()).
top(Uri) ->
    Response = process_request(Uri, 'GetTop', []),
    case Response of
        {ok, 200, Data} ->
            {ok, _Header} = aehttp_api_parser:decode(header, Data);
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.


-spec get_header_by_hash(http_uri_uri(), binary()) -> response(aec_headers:header()).
get_header_by_hash(Uri, Hash) ->
    EncHash = aec_base58c:encode(block_hash, Hash),
    Response = process_request(Uri, 'GetHeaderByHash', [{"hash", EncHash}]),
    case Response of
        {ok, 200, Data} ->
            {ok, _Header} = aehttp_api_parser:decode(header, Data);
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.

%% Add API for header later... now use block
-spec get_header_by_height(http_uri_uri(), non_neg_integer()) -> response(aec_headers:header()).
get_header_by_height(Uri, Height) when is_integer(Height) ->
    Response = process_request(Uri, 'GetHeaderByHeight', [{"height", integer_to_list(Height)}]),
    case Response of
        {ok, 200, Data} ->
            {ok, _Header} = aehttp_api_parser:decode(header, Data);
        {ok, 400, Reason} ->
            lager:debug("Header not found ~p", [Reason]),
            {error, chain_too_short};
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.

-spec get_block_by_height(http_uri_uri(), non_neg_integer()) -> response(aec_headers:header()).
get_block_by_height(Uri, Height) when is_integer(Height) ->
    Response = process_request(Uri, 'GetBlockByHeight', [{"height", integer_to_list(Height)}]),
    case Response of
        {ok, 200, Data} ->
            {ok, _Block} = aehttp_api_parser:decode(block, Data);
        {ok, 404, #{reason := <<"Chain too short">>}} ->
            {error, chain_too_short};
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.



-spec get_block(http_uri_uri(), binary()) -> response(aec_blocks:block()).
get_block(Uri, Hash) ->
    EncHash = aec_base58c:encode(block_hash, Hash),
    Response = process_request(Uri,'GetBlockByHashDeprecated', [{"hash", EncHash}]),
    case Response of
        {ok, 200, Data} ->
            {ok, _Block} = aehttp_api_parser:decode(block, Data);
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.

-spec transactions(http_uri_uri()) -> response([aetx_sign:signed_tx()]).
transactions(Uri) ->
    Response = process_request(Uri, 'GetTxs', []),
    lager:debug("transactions Response = ~p", [pp(Response)]),
    case tx_response(Response) of
        bad_result ->
           lager:warning("Wrong response type: ~p", [Response]),
           {error, wrong_response_type};
        Txs when is_list(Txs) ->
           try {ok, lists:map(
                      fun(#{<<"tx">> := T}) ->
                              {transaction, Dec} =
                                  aec_base58c:decode(T),
                            aetx_sign:deserialize_from_binary(Dec)
                      end, Txs)}
           catch
               error:Reason ->
                   lager:error("Error decoding transactions: ~p", [Reason]),
                   {error, Reason}
           end
    end.

tx_response({ok, 200, #{'Transactions' := Txs}}) -> Txs;
tx_response({ok, 200, [#{<<"tx">> := _}|_] = Txs}) -> Txs;
tx_response({ok, 200, []}) -> [];
tx_response(_Other) -> bad_result.


-spec send_block(http_uri_uri(), aec_blocks:block()) -> response(ok).
send_block(Uri, Block) ->
    BlockSerialized =
        aehttp_api_parser:encode(block, Block),
    lager:debug("send_block; serialized: ~p", [pp(BlockSerialized)]),
    Response = process_request(Uri, 'PostBlock', BlockSerialized),
    case Response of
        {ok, 200, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.

-spec send_tx(http_uri_uri(), aetx_sign:signed_tx()) -> response(ok).
send_tx(Uri, SignedTx) ->
    TxSerialized = aec_base58c:encode(
                     transaction, aetx_sign:serialize_to_binary(SignedTx)),
    Response = process_request(Uri, 'PostTx', #{tx => TxSerialized}),
    case Response of
        {ok, 200, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, unexpected_response}
    end.

%% NOTE that this is part of the internal API, thus the standard peer
%% uris will not work
new_spend_tx(IntPeer, #{recipient_pubkey := Kr,
                        amount := Am,
                        fee := Fee} = Req0)
  when is_binary(Kr), is_integer(Am), is_integer(Fee) ->
    Req = maps:put(
            recipient_pubkey, aec_base58c:encode(account_pubkey, Kr), Req0),
    Response = process_request(IntPeer, 'PostSpendTx', Req),
    case Response of
        {ok, 200, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [IntPeer, Response]),
            {error, unexpected_response}
    end.

process_request(Uri, OperationId, Params) ->
    aeu_http_client:request(Uri, OperationId, Params).

%% No trailing /, since BaseUri starts with /
-spec pp_uri({http_uri:scheme(), http_uri_host(), inet:port_number()}) ->
                    binary().
pp_uri({Scheme, Host, Port}) when is_list(Host) ->
    pp_uri({Scheme, unicode:characters_to_binary(Host, utf8), Port});
pp_uri({Scheme, Host, Port}) ->
    Pre = unicode:characters_to_binary(atom_to_list(Scheme) ++ "://", utf8),
    Post = unicode:characters_to_binary(":" ++ integer_to_list(Port), utf8),
    << Pre/binary, Host/binary, Post/binary >>.

