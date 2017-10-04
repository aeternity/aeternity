-module(aeu_requests).

%% API
-export([ping/1,
         top/1,
         block/2,
         send_block/2
        ]).

-compile({parse_transform, lager_transform}).
%% All requests take 

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(aec_peers:peer()) -> response(map()).
ping(Peer) ->
    Req = "ping",
    #{<<"share">> := Share} = PingObj0 = aec_sync:local_ping_object(),
    Peers = [iolist_to_binary(aec_peers:uri(P))
             || P <- aec_peers:get_random(Share, [Peer])],
    PingObj = PingObj0#{<<"peers">> => Peers},
    Response = process_request(Peer, post, Req, PingObj),
    case Response of
        {ok, Map} ->
            lager:debug("ping response: ~p", [Map]),
            aec_sync:compare_ping_objects(PingObj, Map),
            check_returned_source(Map, Peer),
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

-spec send_block(aec_peers:peer(), aec_blocks:block()) -> response(ok).
send_block(Peer, Block) ->
    Uri = aec_peers:uri(Peer),
    BlockSerialized = aec_blocks:serialize_to_map(Block),
    Response = process_request(Uri, post, "block", BlockSerialized),
    case Response of
        {ok, _Map} ->
            {ok, ok};
        {error, _Reason} = Error ->
            Error
    end.


%% Internal functions

-spec process_request(aec_peers:peer(), get, string()) ->
			     response(B) when
      B :: aec_blocks:block_serialized_for_network() | map().
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
                           lager:debug("JSON-encoding Params: ~p", [Params]),
                           {"application/json", jsx:encode(Params)};
                       _ ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Request)}
                   end,
    lager:debug("Type = ~p~nBody = ~p", [Type, Body]),
    R = httpc:request(post, {URL, Header, Type, Body}, HTTPOptions, Options),
    process_http_return(R).

process_http_return(R) ->
    case R of
        {ok, {{_,_ReturnCode, _State}, _Head, Body}} ->
            try
                Result = jsx:decode(iolist_to_binary(Body), [return_maps]),
                lager:debug("Decoded response: ~p", [Result]),
                {ok, Result}
            catch
                error:E ->
                    {error, {parse_error, E}}
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
            aec_peers:register_alias(Source, Peer);
       true ->
            ok
    end.
