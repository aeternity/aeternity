-module(aeu_requests).

%% API
-export([ping/1]).

%% All requests take 

-type response(Type) :: {ok, Type} | {error, string()}.

-spec ping(aec_peers:peer()) -> response(pong).
ping(Peer) ->
    Response = process_request(Peer, get, "ping", #{}),
    case Response of
        {ok, _Map} ->
            {ok, pong};
        {error, _Reason} = Error ->
            Error
    end.

%% Internal functions

-spec process_request(aec_peers:peer(), atom(), string(), map()) -> response(map()).
process_request(Peer, Method, Request, Map) ->
    URL = aec_peers:uri(Peer) ++ "/" ++ Request,
    Header = [],
    Type = "application/json",
    Body = jsx:encode(Map),
    HTTPOptions = [],
    Options = [],
    R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
    case R of
        {ok, {{_,_ReturnCode, _State}, _Head, Body}} ->
            Result = jsx:decode(Body, [return_maps]),
            {ok, Result};
        {error, _Reason} ->
            {error, "A problem occured"}  %TODO investigate responses and make errors meaningfull
    end.


