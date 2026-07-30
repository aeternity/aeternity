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

%% Soft cap on the number of entries a single JSON-RPC batch may
%% contain. Beyond this we reject early so a hostile / misconfigured
%% client can't OOM the node. Configurable via the `aerpc' env block.
-define(DEFAULT_MAX_BATCH, 1024).

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
    ReplyTerm = dispatch_or_error(Body),
    Encoded = jsx:encode(ReplyTerm),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => ?CT_JSON},
        Encoded,
        Req1),
    {ok, Req2, State}.

dispatch_or_error(Body) ->
    try jsx:decode(Body, [return_maps]) of
        Decoded when is_list(Decoded) ->
            case length(Decoded) > max_batch() of
                true ->
                    {error, Code, Msg} =
                        aerpc_errors:batch_too_large(max_batch()),
                    aerpc_jsonrpc:error(null, Code, Msg);
                false ->
                    aerpc:dispatch(Decoded)
            end;
        Decoded when is_map(Decoded) ->
            aerpc:dispatch(Decoded);
        _Other ->
            aerpc_jsonrpc:error(null, -32700, <<"Parse error">>)
    catch
        error:badarg ->
            aerpc_jsonrpc:error(null, -32700, <<"Parse error">>);
        _:_ ->
            aerpc_jsonrpc:error(null, -32700, <<"Parse error">>)
    end.

max_batch() ->
    application:get_env(aerpc, max_batch_size, ?DEFAULT_MAX_BATCH).
