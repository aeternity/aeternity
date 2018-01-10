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

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(http_uri:uri(), map()) -> response(map()).
ping(Uri, LocalPingObj) ->
    #{<<"share">> := Share, 
      <<"genesis_hash">> := GHash,
      <<"best_hash">> := TopHash
     } = LocalPingObj,
    Peers = aec_peers:get_random(Share, [Uri]),
    lager:debug("ping(~p); Peers = ~p", [Uri, Peers]),
    PingObj = LocalPingObj#{<<"peers">> => Peers,
                            <<"genesis_hash">> => aec_base58c:encode(block_hash, GHash),
                            <<"best_hash">>  => aec_base58c:encode(block_hash, TopHash)
                           },
    Response = process_request(Uri, post, "ping", PingObj),
    case Response of
        {ok, #{<<"reason">> := Reason}} ->
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
        {ok, #{ <<"genesis_hash">> := EncRemoteGHash,
                <<"best_hash">> := EncRemoteTopHash} = Map} ->
            case {aec_base58c:safe_decode(block_hash, EncRemoteGHash),
                  aec_base58c:safe_decode(block_hash, EncRemoteTopHash)} of
              {{ok, RemoteGHash}, {ok, RemoteTopHash}} ->
                RemoteObj = Map#{<<"genesis_hash">> => RemoteGHash,
                                 <<"best_hash">>  => RemoteTopHash},
                RemotePeers = maps:get(<<"peers">>, Map, []),
                lager:debug("ping response (~p): ~p", [Uri, pp(RemoteObj)]),
                case aec_sync:compare_ping_objects(Uri, LocalPingObj, RemoteObj) of
                  ok    -> {ok, RemoteObj, RemotePeers};
                  {error, _} = Error -> Error
                end;
              _ ->
                %% Something is wrong, block the peer later on
                {error, protocol_violation}
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec top(http_uri:uri()) -> response(aec_headers:header()).
top(Uri) ->
    Response = process_request(Uri, get, "top", []),
    case Response of
        {ok, Data} ->
            {ok, Header} = aec_headers:deserialize_from_map(Data),
            {ok, Header};
        {error, _Reason} = Error ->
            Error
    end.


-spec get_block(http_uri:uri(), binary()) -> response(aec_blocks:block()).
get_block(Uri, Hash) ->
    EncHash = aec_base58c:encode(block_hash, Hash),
    Response = process_request(Uri, get, "block-by-hash", [{"hash", EncHash}]),
    case Response of
        {ok, Data} ->
            {ok, Block} = aec_blocks:deserialize_from_map(Data),
            {ok, Block};
        {error, _Reason} = Error ->
            Error
    end.

-spec transactions(http_uri:uri()) -> response([aec_tx:signed_tx()]).
transactions(Uri) ->
    Response = process_request(Uri, get, "transactions", []),
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
                            aec_tx_sign:deserialize_from_binary(Dec)
                      end, Txs)}
           catch
               error:Reason ->
                   lager:error("Error decoding transactions: ~p", [Reason]),
                   {error, Reason}
           end
    end.

tx_response({ok, #{'Transactions' := Txs}}) -> Txs;
tx_response({ok, [#{<<"tx">> := _}|_] = Txs}) -> Txs;
tx_response({ok, []}) -> [];
tx_response(_Other) -> bad_result.


-spec send_block(http_uri:uri(), aec_blocks:block()) -> response(ok).
send_block(Uri, Block) ->
    BlockSerialized = aec_blocks:serialize_to_map(Block),
    lager:debug("send_block; serialized: ~p", [pp(BlockSerialized)]),
    Response = process_request(Uri, post, "block", BlockSerialized),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

-spec send_tx(http_uri:uri(), aec_tx:signed_tx()) -> response(ok).
send_tx(Uri, SignedTx) ->
    TxSerialized = aec_base58c:encode(
                     transaction, aec_tx_sign:serialize_to_binary(SignedTx)),
    Response = process_request(Uri, post, "tx", #{tx => TxSerialized}),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

%% NOTE that this is part of the internal API, thus the standard peer
%% uris will not work
new_spend_tx(IntPeer, #{recipient_pubkey := Kr,
                        amount := Am,
                        fee := Fee} = Req0)
  when is_binary(Kr), is_integer(Am), is_integer(Fee) ->
    Req = maps:put(
            recipient_pubkey, aec_base58c:encode(account_pubkey, Kr), Req0),
    Response = process_request(IntPeer, post, "spend-tx", Req),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

process_request(Uri, Method, Endpoint, Params) ->
    BaseUri = iolist_to_binary([Uri, <<"v1/">>]),
    aeu_http_client:request(BaseUri, Method, Endpoint, Params).

-spec pp_uri({http_uri:schema(), http_uri:host(), http_uri:port()}) -> string().  %% TODO: | unicode:unicode_binary().
pp_uri({Schema, Host, Port}) when is_binary(Host) ->
    pp_uri({Schema, binary_to_list(Host), Port});
pp_uri({Schema, Host, Port}) ->
    atom_to_list(Schema) ++ "://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/".

