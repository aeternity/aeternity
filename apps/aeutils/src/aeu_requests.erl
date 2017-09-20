-module(aeu_requests).

%% API
-export([ping/1,
         top/1%,
         %block/2,
         %send_block/2
        ]).

%% All requests take 

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(aec_peers:peer()) -> response(pong).
ping(Peer) ->
    Response = process_request(Peer, get, "ping"),
    case Response of
        {ok, _Map} ->
            {ok, pong};
        {error, _Reason} = Error ->
            Error
    end.

-spec top(aec_peers:peer()) -> response(aec_headers:header()).
top(Peer) ->
    Response = process_request(Peer, get, "top"),
    case Response of 
        {ok, Map} ->
            {ok, Header} = aec_headers:deserialize_from_map(Map),
            {ok, Header};
        {error, _Reason} = Error ->
            Error
    end.

%-spec block(aec_peers:peer(), binary()) -> response(aec_blocks:block()).
%block(Peer, Hash) ->
%    HexHash = [ hd(erlang:integer_to_list(Nibble, 16)) || << Nibble:4 >> <= Hash],
%    Response = process_request(Peer, get, "block?BlockHash="+HexHash),
%    case Response of 
%        {ok, Map} ->
%            Block = aec_blocks:deserialize_from_json(Map),
%            {ok, Block};
%        {error, _Reason} = Error ->
%            Error
%    end.


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

-spec process_request(aec_peers:peer(), get, string()) -> response(map()).
process_request(Peer, get, Request) ->
    URL = aec_peers:uri(Peer) ++ "v1/" ++ Request,
    Header = [],
    HTTPOptions = [],
    Options = [],
    R = httpc:request(get, {URL, Header}, HTTPOptions, Options),
    case R of
        {ok, {{_,_ReturnCode, _State}, _Head, Body}} ->
            Result = jsx:decode(list_to_binary(Body), [return_maps]),
            {ok, Result};
        {error, _Reason} ->
            {error, "A problem occured"}  %TODO investigate responses and make errors meaningfull
    end.


