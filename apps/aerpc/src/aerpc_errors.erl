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
%%%   -32005  range too wide                  (ae_getLogs)
%%%   -32006  batch too large                 (transport-level)
%%%   -39001  block hash not on canonical     (EIP-1898 requireCanonical)
%%%
%%% Keep this table in lock-step with the doc under
%%% /tasks/eth-like-rpc-layer/rpc-endpoint/ponder-compat-gaps.md.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_errors).

-export([range_too_wide/2,
         batch_too_large/1,
         filter_registry_pending/0]).

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

%% @doc Helper for the filter-family stubs.
-spec filter_registry_pending() -> {error, integer(), binary()}.
filter_registry_pending() ->
    {error, -32004, <<"Filter registry not yet implemented (v1.5)">>}.
