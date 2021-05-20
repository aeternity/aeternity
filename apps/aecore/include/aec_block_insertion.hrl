%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Data structures used during block insertion - they should be used inside consesus modules
%%% and the consensus agnostic aec_chain_state module
%%%

%% Header along with it's hash
-record(chain_node, {
    header :: aec_headers:header()
    , hash :: binary()
    , type :: key | micro
}).

%% Metadata about the given fork
%% ForkId is local with regards to the node and for recently gossiped blocks
%% nodes might disagree about the exact value - fork_id is eventually consistent
%% across the network
-record(fork_info, {
    fork_id
    , difficulty
    , fees
    , fraud
}).

%% Cache entry for recently inserted blocks
-record(recent_blocks, {
    key :: binary()
    %% Window of recent statistics of the chain
    %% The head of this list is always the node corresponding to the key
    %% Invariant: key =:= node_hash(hd(recents))
    %% The remaining entries are {hash, term()}
    %% The first element of the tuple is used for maintaining the cache
    %% The second element is defined by the currently active consensus engine
    %% Please keep in mind that the cache is never filled by 2 engines at the same time
    %% When a consensus change occurs the cache is regenerated
    , recents :: [aec_headers:header() | term()]
    %% Current length of the header window
    , len :: non_neg_integer()
}).

%% Insertion context - cached data used during block insertion
%% The data present in the context is sufficient for fully validating
%% the block headers outside a DB transaction.
-record(insertion_ctx, {
    %% Window of last N keyheaders newest first
    window_len = 0 :: non_neg_integer(),
    %% Recent key headers -> ALWAYS stripped
    recent_key_headers = [] :: [term()],
    prev_node = #chain_node{},
    prev_key_node = #chain_node{}
}).
