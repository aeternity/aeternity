%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Chain state version marker attached to HTTP API responses.
%%%
%%%    A client talking to a pool of nodes behind a load balancer cannot tell a
%%%    response computed from an up-to-date chain from one computed by a lagging
%%%    or still syncing node. Every API response therefore carries the height of
%%%    the node's chain top, which lets a client discard answers from a node
%%%    that is behind a state it has already observed.
%%%
%%%    The marker is read *before* the request handler reads any chain data, so
%%%    the body always reflects a chain state at or after the advertised height,
%%%    never before it. Clients may treat the value as a lower bound.
%%%
%%%    Resolution is one generation: micro blocks applied within the current
%%%    generation do not bump it. That is the deliberate trade-off from GH-4186
%%%    - a marker that changes rarely stays cache-friendly for reverse proxies
%%%    in front of the node, and computing a micro block position would cost a
%%%    height index lookup plus a walk back to the key block on every request.
%%% @end
%%%=============================================================================
-module(aehttp_state_version).

-export([ header_name/0
        , set_resp_header/1
        , top_height/0
        ]).

%% Not RFC 6648 clean, but the name agreed on in GH-4186 and the one SDKs look
%% for. Cowboy expects response header names lowercased.
-define(HEIGHT_HEADER, <<"x-ae-height">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec header_name() -> binary().
header_name() ->
    ?HEIGHT_HEADER.

%% @doc Attach the state version marker to the response headers of Req.
%%
%% The marker is left out when the chain top cannot be read - during startup
%% the chain tables are not readable yet, and no marker is a better answer than
%% a wrong one. Clients are expected to treat a missing header as "unknown".
-spec set_resp_header(cowboy_req:req()) -> cowboy_req:req().
set_resp_header(Req) ->
    case top_height() of
        undefined ->
            Req;
        Height ->
            cowboy_req:set_resp_header(?HEIGHT_HEADER,
                                       integer_to_binary(Height), Req)
    end.

%% @doc Height of the node's chain top, or undefined if it is not readable.
%%
%% Deliberately a dirty read - it runs once per request and must not start a
%% transaction. Micro block headers carry the height of their generation, so
%% this is the current generation height regardless of the top block type.
-spec top_height() -> undefined | aec_blocks:height().
top_height() ->
    try aec_chain:dirty_top_header() of
        undefined -> undefined;
        Header    -> aec_headers:height(Header)
    catch
        Class:Reason ->
            %% Expected during the startup window before chain tables are
            %% readable; logged at debug so an unrelated regression in
            %% aec_chain is still discoverable instead of silently reading
            %% as "unknown" on every request.
            lager:debug("Chain top not readable for X-Ae-Height: ~p:~p", [Class, Reason]),
            undefined
    end.
