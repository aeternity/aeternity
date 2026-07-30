%%%-------------------------------------------------------------------
%%% @doc Public entry point for the AE JSON-RPC dispatcher.
%%% Accepts a decoded JSON term (a single request map, or a list of
%%% request maps for a batch) and returns the JSON term that the
%%% transport layer should re-encode and send back.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc).

-export([dispatch/1]).

-define(JSONRPC_VSN, <<"2.0">>).

-spec dispatch(map() | [map()] | term()) -> map() | [map()].
dispatch([]) ->
    aerpc_jsonrpc:error(null, -32600, <<"Invalid Request">>);
dispatch(Batch) when is_list(Batch) ->
    [dispatch(Req) || Req <- Batch];
dispatch(#{<<"jsonrpc">> := ?JSONRPC_VSN, <<"method">> := Method} = Req)
  when is_binary(Method) ->
    Id     = maps:get(<<"id">>, Req, null),
    Params = maps:get(<<"params">>, Req, []),
    case aerpc_methods:dispatch_method(Method, Params) of
        {ok, Result} ->
            aerpc_jsonrpc:result(Id, Result);
        {error, Code, Msg} ->
            aerpc_jsonrpc:error(Id, Code, Msg);
        {error, Code, Msg, Data} ->
            aerpc_jsonrpc:error(Id, Code, Msg, Data)
    end;
dispatch(_BadEnvelope) ->
    aerpc_jsonrpc:error(null, -32600, <<"Invalid Request">>).
