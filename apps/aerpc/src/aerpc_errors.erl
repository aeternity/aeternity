%%%-------------------------------------------------------------------
%%% @doc Centralised AE-specific JSON-RPC error builders.
%%%
%%% JSON-RPC 2.0 reserves `-32000..-32099' for implementation-defined
%%% server errors. We allocate within that band so eth-style indexers
%%% can pattern-match codes for retry decisions (e.g. "range too wide"
%%% means chunk smaller and try again).
%%%
%%% The code allocations:
%%%   -32003  execution reverted              (contract call)
%%%   -32004  operation not supported         (FATE / write methods)
%%%   -32005  range too wide                  (eth_getLogs)
%%%   -32006  batch too large                 (transport-level)
%%%   -32007  address index not ready         (20-byte address lookup)
%%%   -32009  too many filters                (filter registry cap)
%%%   -32010  filter registry unavailable     (endpoint enabled, no registry)
%%%   -39001  block hash not on canonical     (EIP-1898 requireCanonical)
%%%
%%% One exception to the "allocate our own" rule: an unknown filter id is
%%% `-32000 filter not found', byte-for-byte what geth returns, because
%%% that is the string eth tooling recognises for a filter that has
%%% expired and needs re-creating.
%%%
%%% Keep this table in lock-step with the doc under
%%% /tasks/eth-like-rpc-layer/rpc-endpoint/ponder-compat-gaps.md.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_errors).

-export([range_too_wide/2,
         batch_too_large/1,
         address_index_not_ready/0,
         filter_not_found/0,
         too_many_filters/1,
         filter_registry_unavailable/0]).

%% @doc -32007. A 20-byte address could not be resolved AND the reverse
%% index has not finished building, so we cannot tell "no such account"
%% from "not walked to yet". Eth would answer zero for the former; doing
%% that for the latter is a wrong balance that looks like a right one,
%% so the request fails instead. Retryable: the client should back off
%% and re-issue once the node reports the index complete.
-spec address_index_not_ready() -> {error, integer(), binary()}.
address_index_not_ready() ->
    {error, -32007,
     <<"Address index is still building; retry once it is complete">>}.

%% @doc -32005, formatted with the actual size + the configured max so
%% the fork can chunk on a machine-readable cue.
-spec range_too_wide(non_neg_integer(), non_neg_integer()) ->
    {error, integer(), binary()}.
range_too_wide(Requested, Max) ->
    Msg = iolist_to_binary(
            io_lib:format(
              "Range too wide (~p generations; max ~p). "
              "Retry with toBlock - fromBlock < ~p.",
              [Requested, Max, Max])),
    {error, -32005, Msg}.

%% @doc -32006, with the configured batch cap. Clients are expected to
%% split and retry.
-spec batch_too_large(non_neg_integer()) ->
    {error, integer(), binary()}.
batch_too_large(Max) ->
    Msg = iolist_to_binary(
            io_lib:format("Batch too large (max ~p requests). "
                          "Split the batch.", [Max])),
    {error, -32006, Msg}.

%% @doc geth's exact code and message for an id the registry does not
%% hold -- either never allocated, already uninstalled, or expired by the
%% idle TTL. Clients treat this as "re-create the filter", which is the
%% right reaction to all three.
-spec filter_not_found() -> {error, integer(), binary()}.
filter_not_found() ->
    {error, -32000, <<"filter not found">>}.

%% @doc -32009. The registry is at its configured cap. Naming the cap
%% lets an operator see whether to raise it or a client to stop leaking
%% filters it never uninstalls.
-spec too_many_filters(non_neg_integer()) -> {error, integer(), binary()}.
too_many_filters(Max) ->
    Msg = iolist_to_binary(
            io_lib:format("Too many filters (max ~p). Uninstall unused "
                          "filters or raise http > rpc > max_filters.",
                          [Max])),
    {error, -32009, Msg}.

%% @doc -32010. The endpoint is serving but the filter registry is not
%% running, which is a node configuration state rather than a bad
%% request -- so it is reported as such instead of surfacing a crash.
-spec filter_registry_unavailable() -> {error, integer(), binary()}.
filter_registry_unavailable() ->
    {error, -32010, <<"Filter registry is not running on this node">>}.
