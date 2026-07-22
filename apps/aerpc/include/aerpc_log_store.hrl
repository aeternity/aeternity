%%%-------------------------------------------------------------------
%%% @doc Shared record definition for the log index. Included by both
%%% `aerpc_log_store' (which writes entries) and `aerpc_logs' (which
%%% shapes them into eth log objects on the read path).
%%% @end
%%%-------------------------------------------------------------------
-ifndef(AERPC_LOG_STORE_HRL).
-define(AERPC_LOG_STORE_HRL, true).

-record(log_entry, {
    address          :: binary(),
    height           :: non_neg_integer(),
    tx_idx           :: non_neg_integer(),
    log_idx          :: non_neg_integer(),
    topics           :: [binary()],
    data             :: binary(),
    block_hash       :: binary(),
    micro_block_hash :: binary(),
    tx_hash          :: binary()
}).

-endif.
