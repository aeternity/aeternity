%%%-------------------------------------------------------------------
%%% @doc Public entry point for the eth-compatible JSON-RPC dispatcher.
%%% Accepts a decoded JSON term (a single request map, or a list of
%%% request maps for a batch) and returns the JSON term that the
%%% transport layer should re-encode and send back.
%%%
%%% Both transports enter here, so the batch cap applies to both.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc).

-export([dispatch/1, max_batch_size/0, enable/0, log_index_enabled/0]).

%% @doc Bring up the parts of this app that only an enabled endpoint
%% needs. Called by `aehttp_app' when it mounts the routes, so the cost
%% follows the operator switch rather than mere installation of the app.
%% Idempotent.
%%
%% The address index and the filter registry follow the endpoint switch;
%% the log index takes a second opt-in (`http > rpc > log_index') because
%% it is the only one that does per-key-block chain work and holds a real
%% amount of memory. Without it `eth_getLogs' still answers, from the
%% inline walker.
-spec enable() -> ok | {error, term()}.
enable() ->
    Results = [aerpc_sup:ensure_addr_index(),
               aerpc_sup:ensure_filter_registry()
               | maybe_log_indexer()],
    case [E || {error, _} = E <- Results] of
        []          -> ok;
        [First | _] -> First
    end.

maybe_log_indexer() ->
    case log_index_enabled() of
        true  -> [aerpc_sup:ensure_log_indexer()];
        false -> []
    end.

-spec log_index_enabled() -> boolean().
log_index_enabled() ->
    application:get_env(aerpc, log_index, false) =:= true.

-define(JSONRPC_VSN, <<"2.0">>).

%% Soft cap on the number of entries a single JSON-RPC batch may carry,
%% so a hostile or misconfigured client cannot make one request expand
%% into unbounded work. Operator-settable as `http > rpc >
%% max_batch_size'.
-define(DEFAULT_MAX_BATCH, 1024).

-spec max_batch_size() -> pos_integer().
max_batch_size() ->
    case application:get_env(aerpc, max_batch_size, ?DEFAULT_MAX_BATCH) of
        N when is_integer(N), N > 0 -> N;
        _Other                      -> ?DEFAULT_MAX_BATCH
    end.

%% The cap lives here rather than in the HTTP handler because it is a
%% property of the RPC layer, not of one transport: it used to sit in
%% `aehttp_rpc_handler' alone, which left the WebSocket transport with
%% no cap at all.
-spec dispatch(map() | [map()] | term()) -> map() | [map()].
dispatch([]) ->
    aerpc_jsonrpc:error(null, -32600, <<"Invalid Request">>);
dispatch(Batch) when is_list(Batch) ->
    Max = max_batch_size(),
    case length(Batch) > Max of
        true ->
            {error, Code, Msg} = aerpc_errors:batch_too_large(Max),
            aerpc_jsonrpc:error(null, Code, Msg);
        false ->
            [dispatch_one(Req) || Req <- Batch]
    end;
dispatch(Req) ->
    dispatch_one(Req).

dispatch_one(Batch) when is_list(Batch) ->
    %% Nested batches are not a thing in JSON-RPC 2.0.
    aerpc_jsonrpc:error(null, -32600, <<"Invalid Request">>);
dispatch_one(#{<<"jsonrpc">> := ?JSONRPC_VSN, <<"method">> := Method} = Req)
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
dispatch_one(_BadEnvelope) ->
    aerpc_jsonrpc:error(null, -32600, <<"Invalid Request">>).
