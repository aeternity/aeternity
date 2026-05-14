%%%-------------------------------------------------------------------
%%% @doc 2048-bit logs-bloom helper for transaction receipts.
%%%
%%% v1: always emits the all-zero bloom (no logs are surfaced yet in
%%% receipts, so the bloom for an empty log set is correct).
%%% A real implementation will follow once log translation from
%%% `aect_call.log' to the eth log shape lands; this module is the
%%% single place that will need to change to honour real bits.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_bloom).

-export([empty/0, of_logs/1]).

-define(EMPTY,
        <<"0x", (binary:copy(<<"0">>, 512))/binary>>).

-spec empty() -> binary().
empty() ->
    ?EMPTY.

%% @doc Bloom for a list of log objects. v1 always emits the empty
%% bloom; a real implementation will set bits for each address and
%% topic per yellow paper appendix Appendix-B section J.
-spec of_logs([map()]) -> binary().
of_logs(_Logs) ->
    ?EMPTY.
