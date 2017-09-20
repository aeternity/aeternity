-module(aeu_requests).

%% API
-export([ping/1,
         top/1,
         block/2%,
         %send_block/2
        ]).

-compile({parse_transform, lager_transform}).
%% All requests take 

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(aec_peers:peer()) -> response(map()).
ping(Peer) ->
    Req = "ping?source=" ++ source_uri() ++ share_param(),
    Response = process_request(Peer, get, Req),
    case Response of
        {ok, Map} ->
            lager:debug("ping response: ~p", [Map]),
            {ok, Map};
        {error, _Reason} = Error ->
            Error
    end.

-spec top(aec_peers:peer()) -> response(aec_headers:header()).
top(Peer) ->
    Response = process_request(Peer, get, "top"),
    case Response of
        {ok, Data} ->
            {ok, Header} = aec_headers:deserialize_from_network(Data),
            {ok, Header};
        {error, _Reason} = Error ->
            Error
    end.

-spec block(aec_peers:peer(), binary()) -> response(aec_blocks:block()).
block(Peer, Hash) ->
    HexHash = aeu_hex:bin_to_hex(Hash),
    Response = process_request(Peer, get, "block?BlockHash="++HexHash),
    case Response of
        {ok, Data} ->
            Block = aec_blocks:deserialize_from_network(Data),
            {ok, Block};
        {error, _Reason} = Error ->
            Error
    end.


%-spec send_block(aec_peers:peer(), aec_blocks:block()) -> response(ok).
%send_block(Peer, Block) ->
%    BlockSerialized = aec_blocks:serialize_for_network(Block),
%    Response = process_request(Peer, post, "block", BlockSerialized),
%    case Response of
%        {ok, _Map} ->
%            {ok, ok};
%        {error, _Reason} = Error ->
%            Error
%    end.


%% Internal functions

-spec process_request(aec_peers:peer(), get, string()) ->
			     response(B) when
      B :: aec_blocks:block_serialized_for_network() | map().
process_request(Peer, get, Request) ->
    URL = aec_peers:uri(Peer) ++ "v1/" ++ Request,
    Header = [],
    HTTPOptions = [],
    Options = [],
    R = httpc:request(get, {URL, Header}, HTTPOptions, Options),
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
        {error, _Reason} ->
            {error, "A problem occured"}  %TODO investigate responses and make errors meaningfull
    end.

source_uri() ->
    Uri = aec_peers:get_local_peer_uri(),
    http_uri:encode(Uri).

share_param() ->
    "&share=30".
