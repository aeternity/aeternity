%%%-------------------------------------------------------------------
%%% @doc Method dispatcher. One clause per supported `ae_*' method;
%%% the catch-all clause returns the JSON-RPC method-not-found error.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_methods).

-export([dispatch_method/2]).

-spec dispatch_method(binary(), term()) ->
    {ok, term()} | {error, integer(), binary()}.
dispatch_method(<<"ae_clientVersion">>, _Params) ->
    Vsn = aeapi:node_version(),
    Rev = aeapi:node_revision(),
    {ok, <<"aeternity/", Vsn/binary, "/", Rev/binary>>};

dispatch_method(<<"ae_blockNumber">>, _Params) ->
    case aec_chain:top_header() of
        undefined ->
            {error, -32603, <<"Chain not initialized">>};
        Header ->
            Height = aec_headers:height(Header),
            {ok, aerpc_encoding:to_quantity(Height)}
    end;

dispatch_method(<<"ae_accounts">>, _Params) ->
    %% Node never holds user wallet keys.
    {ok, []};

dispatch_method(_Method, _Params) ->
    {error, -32601, <<"Method not found">>}.
