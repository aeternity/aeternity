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
-export([ping/2,
         top/1,
         get_block/2,
         transactions/1,
         send_tx/2,
         send_block/2,
         new_spend_tx/2,
         pp_uri/1
        ]).

-import(aeu_debug, [pp/1]).

-type http_uri_uri() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L57
-type http_uri_host() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L64
-type http_uri_port() :: pos_integer(). %% https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L66

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(http_uri_uri(), map()) -> {ok, map(), list(http_uri_uri())} | {error, any()}.
ping(Uri, LocalPingObj) ->
    #{<<"share">> := Share,
      <<"genesis_hash">> := GHash,
      <<"best_hash">> := TopHash,
      <<"source_id">> := SourceId
     } = LocalPingObj,
    Peers = aec_peers:get_random(Share, [Uri]),
    lager:debug("ping(~p); Peers = ~p", [Uri, Peers]),
    PingObj = LocalPingObj#{<<"peers">> => Peers,
                            <<"genesis_hash">> => aec_base58c:encode(block_hash, GHash),
                            <<"best_hash">>  => aec_base58c:encode(block_hash, TopHash),
                            <<"source_id">> => aec_base58c:encode(node_id, SourceId)
                           },
    Response = process_request(Uri, 'Ping', PingObj),
    case Response of
        {ok, _, #{<<"reason">> := Reason}} ->
            lager:debug("Got an error return: Reason = ~p", [Reason]),
            aec_events:publish(chain_sync,
                               {sync_aborted, #{uri => Uri,
                                                reason => Reason}}),
            {error, case Reason of
                      <<"Different genesis", _/binary>> ->
                          protocol_violation;
                      <<"Not allowed", _/binary>> ->
                          protocol_violation;
                      _ -> Reason
                    end};
        {ok, 200, #{ <<"genesis_hash">> := EncRemoteGHash,
                     <<"best_hash">> := EncRemoteTopHash,
                     <<"source_id">> := EncSourceId} = Map} ->
            case {aec_base58c:safe_decode(block_hash, EncRemoteGHash),
                  aec_base58c:safe_decode(block_hash, EncRemoteTopHash),
                  aec_base58c:safe_decode(node_id, EncSourceId)} of
              {{ok, RemoteGHash}, {ok, RemoteTopHash}, {ok, SourceId}} ->
                  RemoteObj = Map#{<<"genesis_hash">> => RemoteGHash,
                                   <<"best_hash">>  => RemoteTopHash,
                                   <<"source_id">>  => SourceId},
                  decoded_ping_response(Uri, LocalPingObj, RemoteObj, false);
              _ ->
                %% Something is wrong, block the peer later on
                lager:debug("Erroneous ping response (~p): ~p", [Uri, Map]),
                {error, protocol_violation}
            end;
        {ok, 200, #{ <<"genesis_hash">> := EncRemoteGHash,
                     <<"best_hash">> := EncRemoteTopHash} = Map} ->
            %% Response from a deprecated node without `source_id`.
            case {aec_base58c:safe_decode(block_hash, EncRemoteGHash),
                  aec_base58c:safe_decode(block_hash, EncRemoteTopHash)} of
              {{ok, RemoteGHash}, {ok, RemoteTopHash}} ->
                  RemoteObj = Map#{<<"genesis_hash">> => RemoteGHash,
                                   <<"best_hash">>  => RemoteTopHash},
                  decoded_ping_response(Uri, LocalPingObj, RemoteObj, true);
              _ ->
                %% Something is wrong, block the peer later on
                lager:debug("Erroneous ping response (~p): ~p", [Uri, Map]),
                {error, protocol_violation}
            end;
        {error, _Reason} = Error ->
            Error;
        _ ->
            %% Should have been turned to {error, _} by swagger validation
            lager:debug("unexpected response (~p): ~p", [Uri, Response]),
            {error, protocol_violation}
    end.

decoded_ping_response(Uri, LocalObj, RemoteObj, _IsDeprecated) ->
	#{<<"share">> := Share} = LocalObj,
    case maps:get(<<"peers">>, RemoteObj, []) of
        RemotePeers when is_list(RemotePeers), length(RemotePeers) =< Share ->
            lager:debug("ping response (~p): ~p", [Uri, pp(RemoteObj)]),
            {ok, RemoteObj, RemotePeers};
        _ ->
            lager:debug("ping response with too many peers ~p", [Uri]),
            %% Do not print the object, that in itself opens a DoS attack
            {error, protocol_violation}
    end.

-spec top(http_uri_uri()) -> response(aec_headers:header()).
top(Uri) ->
    Response = process_request(Uri, 'GetTop', []),
    case Response of
        {ok, 200, Data} ->
            {ok, Header} = aec_headers:deserialize_from_map(Data),
            {ok, Header};
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
    Response = process_request(Uri,'GetBlockByHash', [{"hash", EncHash}]),
    case Response of
        {ok, 200, Data} ->
            {ok, Block} = aec_blocks:deserialize_from_map(Data),
            {ok, Block};
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
    BlockSerialized = aec_blocks:serialize_to_map(Block),
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
-spec pp_uri({http_uri:scheme(), http_uri_host(), http_uri_port()}) -> binary().
pp_uri({Scheme, Host, Port}) when is_list(Host) ->
    pp_uri({Scheme, unicode:characters_to_binary(Host, utf8), Port});
pp_uri({Scheme, Host, Port}) ->
    Pre = unicode:characters_to_binary(atom_to_list(Scheme) ++ "://", utf8),
    Post = unicode:characters_to_binary(":" ++ integer_to_list(Port), utf8),
    << Pre/binary, Host/binary, Post/binary >>.

