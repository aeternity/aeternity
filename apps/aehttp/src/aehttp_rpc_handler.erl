%%%-------------------------------------------------------------------
%%% @doc Cowboy handler for the JSON-RPC endpoint.
%%% Reads the POST body, hands the decoded JSON term to
%%% `aerpc:dispatch/1', and writes the encoded reply.
%%% @end
%%%-------------------------------------------------------------------
-module(aehttp_rpc_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-define(CT_JSON, <<"application/json">>).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_post(Req0, State);
        _Other ->
            Reply = cowboy_req:reply(405,
                #{<<"allow">> => <<"POST">>}, <<>>, Req0),
            {ok, Reply, State}
    end.

handle_post(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    ReplyTerm =
        try jsx:decode(Body, [return_maps]) of
            Decoded when is_map(Decoded); is_list(Decoded) ->
                aerpc:dispatch(Decoded);
            _Other ->
                aerpc_jsonrpc:error(null, -32700, <<"Parse error">>)
        catch
            error:badarg ->
                aerpc_jsonrpc:error(null, -32700, <<"Parse error">>);
            _:_ ->
                aerpc_jsonrpc:error(null, -32700, <<"Parse error">>)
        end,
    Encoded = jsx:encode(ReplyTerm),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => ?CT_JSON},
        Encoded,
        Req1),
    {ok, Req2, State}.
