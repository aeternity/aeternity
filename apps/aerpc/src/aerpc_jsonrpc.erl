%%%-------------------------------------------------------------------
%%% @doc JSON-RPC 2.0 envelope helpers.
%%% Produces map terms suitable for jsx:encode/1.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_jsonrpc).

-export([
          result/2
        , error/3
        , error_codes/0
        ]).

-define(JSONRPC_VSN, <<"2.0">>).

-spec result(term(), term()) -> map().
result(Id, Result) ->
    #{<<"jsonrpc">> => ?JSONRPC_VSN,
      <<"id">>      => Id,
      <<"result">>  => Result}.

-spec error(term(), integer(), binary()) -> map().
error(Id, Code, Message) when is_integer(Code), is_binary(Message) ->
    #{<<"jsonrpc">> => ?JSONRPC_VSN,
      <<"id">>      => Id,
      <<"error">>   => #{<<"code">>    => Code,
                         <<"message">> => Message}}.

%% Standard JSON-RPC 2.0 error codes; -32000..-32099 is the reserved
%% range for implementation-defined server errors.
-spec error_codes() -> [{atom(), integer(), binary()}].
error_codes() ->
    [ {parse_error,      -32700, <<"Parse error">>}
    , {invalid_request,  -32600, <<"Invalid Request">>}
    , {method_not_found, -32601, <<"Method not found">>}
    , {invalid_params,   -32602, <<"Invalid params">>}
    , {internal_error,   -32603, <<"Internal error">>}
    ].
