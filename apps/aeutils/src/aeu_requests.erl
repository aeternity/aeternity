-module(aeu_requests).

%% API
-export([ping/1,
         top/1,
         block/2,
         transactions/1,
         send_tx/2,
         send_block/2,
         new_spend_tx/2
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
            aec_sync:compare_ping_objects(PingObj, Map),
            check_returned_source(Map, list_to_binary(aec_peers:uri(Peer))),
            {ok, Map};
        {error, _Reason} = Error ->
            Error
    end.

-spec top(aec_peers:peer()) -> response(aec_headers:header()).
top(Peer) ->
    Response = process_request(Peer, get, "top"),
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
    Uri = aec_peers:uri(Peer),
    Response = process_request(Uri, get, "transactions"),
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
            lager:error("Wrong response type: ~p", [Response]),
            {error, wrong_response_type}
    end.

tx_response({ok, #{'Transactions' := Txs}}) -> Txs;
tx_response({ok, [#{<<"tx">> := _}|_] = Txs}) -> Txs;
tx_response({ok, []}) -> [];
tx_response(Other) -> error({bad_result, Other}).


-spec send_block(aec_peers:peer(), aec_blocks:block()) -> response(ok).
send_block(Peer, Block) ->
    Uri = aec_peers:uri(Peer),
    BlockSerialized = aec_blocks:serialize_to_map(Block),
    lager:debug("send_block; serialized: ~p", [pp(BlockSerialized)]),
    Response = process_request(Uri, post, "block", BlockSerialized),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

-spec send_tx(aec_peers:peer(), aec_tx:signed_tx()) -> response(ok).
send_tx(Peer, SignedTx) ->
    Uri = aec_peers:uri(Peer),
    TxSerialized = base64:encode(aec_tx_sign:serialize_to_binary(SignedTx)),
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
    Uri = aec_peers:uri(IntPeer),
    Req = maps:put(recipient_pubkey, base64:encode(Kr), Req0),
    Response = process_request(Uri, post, "spend-tx", Req),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.

-spec parse_uri(http_uri:uri()) -> {string(), string(), integer()} | error.
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


-spec process_request(aec_peers:peer(), get, string()) ->
			     response(B) when
      B :: aec_blocks:block_serialized_for_network() | map() | [map()].
process_request(Peer, Method, Request) ->
    process_request(Peer, Method, Request, []).

process_request(Peer, Method, Request, Params) ->
    Header = [],
    HTTPOptions = [],
    Options = [],
    process_request(Peer, Method, Request, Params, Header, HTTPOptions, Options).

process_request(Peer, get, Request, Params, Header, HTTPOptions, Options) ->
    URL = binary_to_list(
            iolist_to_binary(
              [aec_peers:uri(Peer), "v1/", Request, encode_get_params(Params)])),
    lager:debug("GET URL = ~p", [URL]),
    R = httpc:request(get, {URL, Header}, HTTPOptions, Options),
    process_http_return(R);
process_request(Peer, post, Request, Params, Header, HTTPOptions, Options) ->
    URL = aec_peers:uri(Peer) ++ "v1/" ++ Request,
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           lager:debug("JSON-encoding Params: ~p",
                                       [pp(Params)]),
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Request)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    R = httpc:request(post, {URL, Header, Type, Body}, HTTPOptions, Options),
    process_http_return(R).

process_http_return(R) ->
    case R of
        {ok, {{_,_ReturnCode, _State}, _Head, Body}} ->
            try
                %% lager:debug("Body to parse: ~s", [Body]),
                Result = jsx:decode(iolist_to_binary(Body), [return_maps]),
                lager:debug("Decoded response: ~p", [pp(Result)]),
                {ok, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = _Error ->
            lager:debug("process_http_return: ~p", [_Error]),
            {error, "A problem occured"}  %TODO investigate responses and make errors meaningfull
    end.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

%% str(A) when is_atom(A) ->
%%     atom_to_binary(A, latin1);
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(V) ->
    http_uri:encode(V).

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
