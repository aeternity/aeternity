%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc End of epoch voting mechanism for Hyper chains
%%% @end
%%% -------------------------------------------------------------------

-module(aec_eoe_vote).

%% Export API functions
-export([start_link/2, negotiate/7, get_finalize_transaction/1, add_parent_block/2]).


%% API to start the state machine
-spec start_link(#{binary() => binary()}, non_neg_integer())  -> {ok, pid()} | {error, atom()}.
start_link(Stakers, BlockTime) ->
    aec_eoe_fork_vote:start_link(Stakers, BlockTime).

%% Negotiate a fork, called with preferred fork and epoch length delta
-spec negotiate(non_neg_integer(), non_neg_integer(), binary(), aec_keys:pubkey(), [{binary(), non_neg_integer()}], binary(), non_neg_integer()) -> ok.
negotiate(Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength) ->
    aec_eoe_fork_vote:negotiate(Epoch, Height, Hash, Leader, Validators, Seed, CurrentLength).

-spec add_parent_block(non_neg_integer(), aec_parent_chain_block:block()) -> ok.
add_parent_block(Epoch, ParentBlock) ->
    aec_eoe_fork_vote:add_parent_block(Epoch, ParentBlock).

-spec get_finalize_transaction(aec_trees:trees()) -> {ok, aetx_sign:signed_tx()} | {error, not_ready} | {error, term()}.
get_finalize_transaction(Trees) ->
    aec_eoe_fork_vote:get_finalize_transaction(Trees).

