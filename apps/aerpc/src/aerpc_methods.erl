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

dispatch_method(<<"ae_netListening">>, _Params) ->
    %% If the node is able to answer this RPC it is, by definition, listening.
    {ok, true};

dispatch_method(<<"ae_getStorageAt">>, _Params) ->
    %% FATE contracts use a typed key-value store that does not map onto
    %% EVM-style 32-byte slot indices. v1 returns a zero word; a real
    %% mapping is deferred until a slot-derivation convention is agreed.
    {ok, <<"0x", (binary:copy(<<"0">>, 64))/binary>>};

dispatch_method(_Method, _Params) ->
    {error, -32601, <<"Method not found">>}.
