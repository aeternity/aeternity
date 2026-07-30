%%%-------------------------------------------------------------------
%%% @doc Cowboy routes for the JSON-RPC endpoint backed by `aerpc'.
%%% Prepended to the external listener's path list so it is matched
%%% before the OpenAPI/REST routes.
%%% @end
%%%-------------------------------------------------------------------
-module(aehttp_rpc_router).

-export([routes/0]).

-spec routes() -> [{string(), module(), term()}].
routes() ->
    [ {"/v3/rpc",    aehttp_rpc_handler,    []}
    , {"/v3/rpc/ws", aehttp_rpc_ws_handler, []}
    ].
