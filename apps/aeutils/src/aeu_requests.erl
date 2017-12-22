-module(aeu_requests).

%% API
-export([ping/1,
         top/1,
         block/2,
         transactions/1,
         send_tx/2,
         send_block/2,
         new_spend_tx/2,
         pp_uri/1
        ]).

-export([parse_uri/1]).
-import(aeu_debug, [pp/1]).

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(aec_peers:peer()) -> response(map()).
ping(Peer) ->
    Req = "ping",
    Uri = aec_peers:uri(Peer),
    #{<<"share">> := Share} = PingObj0 = aec_sync:local_ping_object(),
    Peers = [iolist_to_binary(aec_peers:uri(P))
             || P <- aec_peers:get_random(Share, [Peer])],
    lager:debug("ping(~p); Peers = ~p", [Uri, Peers]),
    PingObj = PingObj0#{<<"peers">> => Peers},
    Response = process_request(Peer, post, Req, PingObj),
    case Response of
        {ok, #{<<"reason">> := Reason}} ->
            lager:debug("Got an error return: Reason = ~p", [Reason]),
            case Reason of
                <<"Different genesis", _/binary>> ->
                    aec_peers:block_peer(Uri);
                <<"Not allowed", _/binary>> ->
                    aec_peers:block_peer(Uri);
                _ -> ok
            end,
            aec_events:publish(chain_sync,
                               {sync_aborted, #{uri => Uri,
                                                reason => Reason}}),
            {error, Reason};
        {ok, Map} ->
            lager:debug("ping response (~p): ~p", [Uri, pp(Map)]),
            case aec_sync:compare_ping_objects(PingObj, Map) of
                ok ->  
                    check_returned_source(Map, list_to_binary(aec_peers:uri(Peer))),
                    {ok, Map};
                Error ->
                  Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec top(aec_peers:peer()) -> response(aec_headers:header()).
top(Peer) ->
    Response = process_request(Peer, get, "top", []),
    case Response of
        {ok, Data} ->
            {ok, Header} = aec_headers:deserialize_from_map(Data),
            {ok, Header};
        {error, _Reason} = Error ->
            Error
    end.


-spec block(aec_peers:peer(), binary()) -> response(aec_blocks:block()).
block(Peer, Hash) ->
    EncHash = base64:encode(Hash),
    Response = process_request(Peer, get, "block-by-hash", [{"hash", EncHash}]),
    case Response of
        {ok, Data} ->
            {ok, Block} = aec_blocks:deserialize_from_map(Data),
            {ok, Block};
        {error, _Reason} = Error ->
            Error
    end.

-spec transactions(aec_peers:peer()) -> response([aec_tx:signed_tx()]).
transactions(Peer) ->
    Response = process_request(Peer, get, "transactions", []),
    lager:debug("transactions Response = ~p", [pp(Response)]),
    try Txs = tx_response(Response),
         try {ok, lists:map(
                    fun(#{<<"tx">> := T}) ->
                            aec_tx_sign:deserialize_from_binary(
                              base64:decode(T))
                    end, Txs)}
         catch
             error:Reason ->
                 lager:error("Error decoding transactions: ~p", [Reason]),
                 {error, Reason}
         end
    catch
        error:_Reason1 ->
            lager:warning("Wrong response type: ~p", [Response]),
            {error, wrong_response_type}
    end.

tx_response({ok, #{'Transactions' := Txs}}) -> Txs;
tx_response({ok, [#{<<"tx">> := _}|_] = Txs}) -> Txs;
tx_response({ok, []}) -> [];
tx_response(Other) -> error({bad_result, Other}).


-spec send_block(aec_peers:peer(), aec_blocks:block()) -> response(ok).
send_block(Peer, Block) ->
    BlockSerialized = aec_blocks:serialize_to_map(Block),
    lager:debug("send_block; serialized: ~p", [pp(BlockSerialized)]),
    Response = process_request(Peer, post, "block", BlockSerialized),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

-spec send_tx(aec_peers:peer(), aec_tx:signed_tx()) -> response(ok).
send_tx(Peer, SignedTx) ->
    TxSerialized = base64:encode(aec_tx_sign:serialize_to_binary(SignedTx)),
    Response = process_request(Peer, post, "tx", #{tx => TxSerialized}),
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
    Req = maps:put(recipient_pubkey, base64:encode(Kr), Req0),
    Response = process_request(IntPeer, post, "spend-tx", Req),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

-spec parse_uri(http_uri:uri()) -> {http_uri:scheme(), http_uri:host(), http_uri:port()} | error.
parse_uri(Uri) ->
    case http_uri:parse(Uri) of
        {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query, _Fragment}} ->
            {Scheme, Host, Port};
        {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query}} ->
            {Scheme, Host, Port};
        {error, _Reason} ->
            lager:debug("cannot parse Uri (~p): ~p", [Uri, _Reason]),
            error
    end.


process_request(Peer, Method, Endpoint, Params) ->
    BaseUri = aec_peers:uri(Peer) ++ "v1/",  %% TODO make this work for unicode
    aeu_http_client:request(BaseUri, Method, Endpoint, Params).

check_returned_source(#{<<"source">> := Source}, Peer) ->
    if Peer =/= Source ->
            %% Consider Peer an alias of Source
            %% (which should already be registered with aec_peers)
            lager:debug("Source (~p) and Peer (~p) differ; adding alias",
                        [Source, Peer]),
            aec_peers:register_source(Source, Peer);
       true ->
            ok
    end.


-spec pp_uri({http_uri:schema(), http_uri:host(), http_uri:port()}) -> string().  %% TODO: | unicode:unicode_binary().
pp_uri({Schema, Host, Port}) when is_binary(Host) ->
  pp_uri({Schema, binary_to_list(Host), Port});
pp_uri({Schema, Host, Port}) ->
    atom_to_list(Schema) ++ "://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/".

