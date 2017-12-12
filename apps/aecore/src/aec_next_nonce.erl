-module(aec_next_nonce).

-export([pick_for_account/2]).

-include("common.hrl").
-include("blocks.hrl").

%% It assumes that in order to pick a nonce for a transaction
%% account has to be present in state tree.
%% It implies that user cannot pick a nonce to create a transaction (e.g. spend tx)
%% to put it into the mempool until either
%% - some funds are transferred to user's account
%% or
%% - user mined a block, which was already added to the chain.
-spec pick_for_account(pubkey(), block()) -> {ok, non_neg_integer()} | {error, account_not_found}.
pick_for_account(AccountPubkey, TopBlock) ->
    case get_state_tree_nonce(AccountPubkey, TopBlock) of
        {ok, StateTreeNonce} ->
            MempoolNonce = get_mempool_nonce(AccountPubkey),
            NextNonce = pick_higher_nonce(StateTreeNonce, MempoolNonce) + 1,
            {ok, NextNonce};
        {error, account_not_found} = Error ->
            Error
    end.


%% Internals

-spec get_state_tree_nonce(pubkey(), block()) -> {ok, non_neg_integer()} | {error, account_not_found}.
get_state_tree_nonce(AccountPubkey, TopBlock) ->
    Trees = aec_blocks:trees(TopBlock),
    AccountsTrees = aec_trees:accounts(Trees),
    case aec_accounts:get(AccountPubkey, AccountsTrees) of
        {ok, Account} ->
            {ok, aec_accounts:nonce(Account)};
        {error, notfound} ->
            {error, account_not_found}
    end.

-spec get_mempool_nonce(pubkey()) -> integer().
get_mempool_nonce(AccountPubkey) ->
    case aec_tx_pool:get_max_nonce(AccountPubkey) of
        {ok, Nonce} ->
            Nonce;
        undefined ->
            -1
    end.

-spec pick_higher_nonce(non_neg_integer(), integer()) -> non_neg_integer().
pick_higher_nonce(Nonce1, Nonce2) when Nonce1 >= Nonce2 ->
    Nonce1;
pick_higher_nonce(_Nonce1, Nonce2) ->
    Nonce2.
